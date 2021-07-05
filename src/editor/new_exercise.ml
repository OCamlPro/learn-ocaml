(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * The main authors of the editor part is the pfitaxel team see 
 * https://github.com/pfitaxel/learn-ocaml-editor for more information
 * 
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Dom_html
open Js_utils
open Learnocaml_common
open Editor_lib
open Learnocaml_config
open Learnocaml_data
open Learnocaml_data.Editor
open Learnocaml_data.Exercise.Meta

module StringMap = Map.Make (String)
(*
(* Internationalization *)
let () = Translate.set_lang ()
let () =
  let translations = [
    "txt_new_exo", [%i"New exercise"];
    "txt_id", [%i"Unique identifier:<br>"];
    "txt_title", [%i"Title (unique too):<br>"];
    "txt_descr", [%i"Description of the exercise:<br>"];
    "txt_diff", [%i"Difficulty level:<br>"];
    "cancel", [%i"Cancel"];
    "save", [%i"Save"];
  ] in
  Translate.set_string_translations translations
 *)
 

let getString element= Js.to_string element##.value
let getStringOpt = function
  | None -> None
  | Some input -> Some (Js.to_string input##.value)
     
let getFloat element= float_of_string (Js.to_string element##.value)

let get = function
    None -> failwith "Element not found"
  | Some s -> s 

let string_with_spaces list=
  let s =
    List.fold_left
      (fun acc elt ->acc^" "^elt)
      ""
      list
  in
  if s="" then
    ""
  else
    String.sub s 1 (String.length s - 1 )

let resultOptionToBool = function
  | None -> false
  | Some _ -> true
  

let isIdCorrect s =
  resultOptionToBool (Regexp.string_match (Regexp.regexp "^[a-z0-9_-]+$") s 0)

let isTitleCorrect s =
  (resultOptionToBool (Regexp.string_match (Regexp.regexp "^[^ \t]") s 0)) &&
    (resultOptionToBool (Regexp.string_match (Regexp.regexp ".*[^ \t]$") s 0))

  
module H = Tyxml_js.Html
open Lwt.Infix

(*getting html elements*)   
let previous_id = match (arg "id") with
  | exception Not_found -> ""
  | s -> s
let save_element = getElementById "save"
let identifier_input =
  get (getElementById_coerce "identifier" CoerceTo.input)
let title_input =
  get (getElementById_coerce "title" CoerceTo.input)
let authors_input =
  get (getElementById_coerce "authors" CoerceTo.input)
let required_input =
  get (getElementById_coerce "required" CoerceTo.input) 
let trained_input =
  get (getElementById_coerce "focus" CoerceTo.input) 
let description_textarea =
  get (getElementById_coerce "description" CoerceTo.textarea)
let difficulty_select = 
  get (getElementById_coerce "difficulty" CoerceTo.select)
   
let backward_input =get (getElementById_coerce "backward" CoerceTo.input)                      
let forward_input = get (getElementById_coerce "forward" CoerceTo.input)
let previous_state =
  match get_editor_state previous_id with
  | exception Not_found -> None    
  | state->Some state

(*filling the form eventualy *)
let _ = match previous_state with
  | None -> ()
  | Some state -> identifier_input##.value := Js.string previous_id;
                  
                  title_input##.value := Js.string state.metadata.title;
                  
                  let s = (List.fold_left
                             (fun acc (a,b) ->acc^a^", "^b^"; ")
                             ""
                             state.metadata.author)
                  in
                  authors_input##.value :=
                    if s="" then Js.string ""
                    else
                      Js.string (String.sub s 0 (String.length s - 2));
                                   
                  required_input##.value := Js.string
                                              (string_with_spaces
                                                 state.metadata.requirements);
                  trained_input##.value := Js.string
                                             (string_with_spaces
                                                state.metadata.focus);
                  setInnerHtml
                    description_textarea
                    (match state.metadata.short_description with
                       None -> ""
                      |Some s -> s);
                  
                  difficulty_select##.value := Js.string (string_of_float state.metadata.stars);
                  
                  backward_input##.value := Js.string
                                              (string_with_spaces
                                                 state.metadata.backward);
                  
                  forward_input##.value := Js.string
                                             (string_with_spaces
                                                state.metadata.forward)
                  

(* handling the save of metadata *)                  
let store metadata =
  let state =
    match get_editor_state previous_id with
    | exception Not_found -> new_state metadata
    | e ->
       {exercise=e.exercise ;metadata}
  in
  update_index state

let id_error = getElementById "id_error"
let title_error = getElementById "title_error"

let _ = 
  save_element##.onclick := handler (fun _ ->
  let id = getString identifier_input
  and title = getString title_input
  and short_description = Some (getString description_textarea)
  and stars = getFloat difficulty_select
  and string_parser string = Regexp.split
                   (Regexp.regexp " ")
                    string
  in
 
    let requirements= string_parser (Js.to_string required_input##.value)
    and  focus= string_parser (Js.to_string trained_input##.value)
    and  backward = string_parser (Js.to_string backward_input##.value)
    and forward = string_parser (Js.to_string forward_input##.value)
    and authors=
      if String.trim (Js.to_string authors_input##.value) = "" then
        []
      else
      Regexp.split
        (Regexp.regexp ";")
        (Js.to_string authors_input##.value)
      |> List.map @@
           fun s ->
           match List.map String.trim @@ Regexp.split
                   (Regexp.regexp ",")
                   s
                    with
                      a::b::[]->(a,b)
                    | _ ->
                       Dom_html.window##alert (Js.string "Incorrect value for the authors field");
                       failwith "bad syntax"
      
  in
  let metadata={requirements;focus;backward;forward;
                kind= Exercise;title; id=Some id;
                author=authors;short_description;stars}
  and id_correct = isIdCorrect id 
  and id_unique = idUnique id || (previous_id = id)
  and title_correct = isTitleCorrect title
  and previous_title = match previous_state with
      None -> None
    | Some state ->
       Some state.metadata.title
  in
  let title_unique = titleUnique title || previous_title = (Some title) in
  (if not id_correct then
    setInnerHtml id_error [%i"Incorrect identifier: an identifier \
                              can't be empty, \
                              and only lower case letters, numerals, dashes \
                              and underscores are allowed"]
  else if not id_unique then
    setInnerHtml id_error [%i"This identifier is already used, \
                              please choose another one"]
  else
    setInnerHtml id_error "");
  (if not title_correct then
    setInnerHtml title_error [%i"Incorrect title: a title can't be empty, \
                                 or begin or end with a space or a tab"]
  else if not title_unique then
     setInnerHtml title_error
       [%i"This title is already used, please choose another one"]
  else
    setInnerHtml title_error "");
  if id_correct && title_correct && id_unique && title_unique then
    begin
      store metadata;
      Dom_html.window##.location##assign
        (Js.string (api_server ^ "/editor.html#id=" ^ id));
 
    end;
  Js._true);
