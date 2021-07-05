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

open Js_utils
open Lwt
open Learnocaml_data
open Learnocaml_common
open Editor
open Editor_lib   
open Tyxml_js.Html5


let fetch_editor_index ()=
  let index=
    Learnocaml_local_storage.(retrieve editor_index ) in

  let json =
    Json_repr_browser.Json_encoding.construct
       (SMap.enc editor_state_enc) index in
    try Lwt.return (Json_repr_browser.Json_encoding.destruct
                      (SMap.enc editor_state_enc) json) with exn ->
    let msg =
      Format.asprintf "bad structure for %s@.%a"
        "index"
        (fun ppf -> Json_encoding.print_error ppf) exn in
    Lwt.fail (Server_caller.Cannot_fetch msg);;


let delete_button_handler exercise_id =
  (fun _ ->
    let message =
      pcdata [%i"Are you sure you want  to delete this exercise?\n"]
    in
    Learnocaml_common.confirm
      ~title:"Confirmation"
      ~ok_label:"Yes"
      [message]
      (fun () ->
        remove_exo exercise_id;
        Dom_html.window##.location##reload);
    true) ;;

let import_bar =
  a ~a:[ a_onclick (fun _ -> Editor_io.upload ();true);
         a_class [ "exercise"] ]
    [ div ~a:[ a_class [ "descr" ] ] [
          h1 [ pcdata [%i"Import"] ];
          p [pcdata [%i"Import from a zip file"]]]]
  
let export_all_bar =
  a ~a:[ a_onclick (fun _ -> Editor_io.download_all ();true);
         a_class [ "exercise"] ]
    [ div ~a:[ a_class [ "descr" ] ] [
          h1 [ pcdata [%i"Export all"] ];
          p [pcdata [%i"Export all exercises to a zip file"]]]]
  
let new_exercise_bar =
  a ~a:[ a_href  "new_exercise.html";
         a_class [ "exercise" ] ] [
      div ~a:[ a_class [ "descr" ] ] [
          h1 [ pcdata [%i"New exercise"] ];
          p [pcdata [%i"Create \
                        a new exercise"]]]]  

  
let editor_tab  _ _ () =


    Lwt_js.sleep 0.5 >>= fun () ->
    let _ =match Learnocaml_local_storage.(retrieve editor_index) with
    | exception Not_found ->
       Learnocaml_local_storage.(store editor_index) SMap.empty
    | _ -> ()
    in
    let content_div = find_component "learnocaml-main-content" in
    fetch_editor_index () >>=
    fun index ->
    let format_exercise_list contents =
      let open Tyxml_js.Html5 in
      let open Exercise.Meta in 
      SMap.fold
        (fun exercise_id editor_sate acc ->
          div ~a:[a_id "toolbar"; a_class ["button"]] [
              (let button = button ~a:[a_id exercise_id]
                              [img ~src:"icons/icon_cleanup_dark.svg"
                                 ~alt:"" () ; pcdata "" ] in
               Manip.Ev.onclick button
                 (delete_button_handler exercise_id); button);
              (let download_button = button ~a:[a_id exercise_id]
                              [img ~src:"icons/icon_download_dark.svg"
                                 ~alt:"" () ; pcdata "" ] in
               Manip.Ev.onclick download_button
                 (fun _ ->  Editor_io.download exercise_id; true) ;download_button
         )] ::
        a ~a:[ a_href ("editor.html#id="^exercise_id) ;
               a_class [ "exercise" ] ]
          [div ~a:[ a_class [ "descr" ] ] [
               h1 [ pcdata editor_sate.metadata.title ] ;
               p [ match editor_sate.metadata.short_description with
                   | None -> pcdata [%i"No description available."]
                       | Some text -> pcdata text ] ;
             ] ;
           div ~a:[ a_class [ "time-left" ] ] [pcdata ("id: " ^ editor_sate.exercise.id ) ];

           div ~a:[a_class["stats"]] [
               div ~a:[ a_class [ "stars" ] ] [
                   let num = 5 * int_of_float (editor_sate.metadata.stars*. 2.) in
                   let num = max (min num 40) 0 in
                   let alt =
                     Format.asprintf "difficulty: %d / 40" num in
                   let src =
                     Format.asprintf "icons/stars_%02d.svg" num in
                   img ~alt ~src ()
                 ] ;
               div ~a:[ a_class [ "length" ] ] [
                   match editor_sate.metadata.kind with
                   | Project -> pcdata "editor project"
                   | Problem -> pcdata "editor problem"
                   | Exercise ->
                      pcdata "editor exercise" ] ;
                 ];
          ] ::
          acc) index contents
    in
    let c= List.rev
             (format_exercise_list 
                [ new_exercise_bar;
                  export_all_bar;
                   import_bar] )
    in
    let list_div =
      Tyxml_js.Html5.(div ~a:
                        [ Tyxml_js.Html5.a_id "learnocaml-main-exercise-list" ])
        c in
    Manip.appendChild content_div list_div ;
    hide_loading ~id:"learnocaml-main-loading" () ;
    Lwt.return list_div;;
