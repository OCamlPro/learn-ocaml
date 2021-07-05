
(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Learnocaml_data

type 'a storage_key =
  { key : string option ;
    dependent_keys : string -> bool ;
    store : 'a -> unit ;
    retrieve : unit -> 'a ;
    delete : unit -> unit ;
    mutable listeners : ('a option -> unit) option ref list }

type any_key = Key : _ storage_key -> any_key

let keys_with_listeners = ref []

let notify = function
  | None -> ()
  | Some key ->
      keys_with_listeners :=
        List.fold_left
          (fun acc (Key storage_key) ->
             if storage_key.dependent_keys key then
               let listeners =
                 List.fold_left
                   (fun acc listener ->
                      match !listener with
                      | None -> acc
                      | Some cb ->
                          let acc, value = match acc with
                            | (acc, None) ->
                                let value =
                                  try Some (storage_key.retrieve ())
                                  with Not_found -> None in
                                acc, value
                            | (acc, Some value) -> acc, value in
                          cb value ;
                          (listener :: acc, Some value))
                   ([], None) storage_key.listeners
                 |> fst in
               storage_key.listeners <- listeners ;
               if listeners = [] then
                 acc
               else (Key storage_key) :: acc
             else (Key storage_key) :: acc)
          [] !keys_with_listeners

let init () =
  let storage_event_typ
    : Dom_html.storageEvent Js.t Dom_html.Event.typ
    = Dom_html.Event.make "storage" in
  Dom_html.addEventListener
    Dom_html.window storage_event_typ
    (Dom_html.handler (fun evt ->
         Js.Opt.case (evt##.key)
           (fun () -> Js._false)
           (fun name ->
              let name = Js.to_string name in
              notify (Some name) ;
              Js._true)))
    Js._true |> ignore

let store { store ; key ; _ } v =
  store v ;
  notify key

let retrieve { retrieve ; _ } =
  retrieve ()

let delete { delete ; key ; _ } =
  delete () ;
  notify key

let listener key =
  let listener = ref None in
  if key.listeners = [] then begin
    keys_with_listeners := Key key :: !keys_with_listeners ;
  end ;
  key.listeners <- listener :: key.listeners ;
  listener

let mangle parts =
  String.concat ":" ("learnocaml" :: parts)

let demangle str =
  match Stringext.split ~on:':' str with
  | "learnocaml" :: rest -> rest
  | _ -> [ (* ignore *) ]

let store_single name enc value =
  Js.Optdef.case
    (Dom_html.window##.localStorage)
    (fun () -> failwith "local storage support required")
    (fun localStorage ->
       let json = Json_repr_browser.Json_encoding.construct enc value in
       localStorage##(setItem (Js.string name) (Js._JSON##(stringify json))))

let retrieve_single ?default name enc () =
  Js.Optdef.case
    (Dom_html.window##.localStorage)
    (fun () -> failwith "local storage support required")
    (fun localStorage ->
       Js.Opt.case
         (localStorage##(getItem (Js.string name)))
         (fun () ->
            match default with
            | Some default -> default
            | None -> raise Not_found)
         (fun v ->
            let open Json_repr_browser.Json_encoding in
            try
              destruct enc (Js._JSON##(parse v))
            with exn ->
              raise (Json_encoding.Cannot_destruct
                       ([ `Field "localStorage" ; `Field name ], exn))))

let delete_single name _enc () =
  Js.Optdef.case
    (Dom_html.window##.localStorage)
    (fun () -> failwith "local storage support required")
    (fun localStorage ->
       localStorage##(removeItem (Js.string name)))

let clear () =
  Js.Optdef.iter
    (Dom_html.window##.localStorage)
    (fun localStorage -> localStorage##clear)

let server_id =
  let key = mangle [ "server_id" ]
  and enc = Json_encoding.(obj1 (req "server_id" int)) in
  let store = store_single key enc
  and retrieve = retrieve_single key enc
  and delete = delete_single key enc in
  { key = Some key ; dependent_keys = (=) key ;
    store ; retrieve ; delete ; listeners = [] }

let sync_token =
  let key = mangle [ "sync-token" ] in
  let enc = Json_encoding.(obj1 (req "token" string)) in
  let store value = store_single key enc (Token.to_string value)
  and retrieve () = retrieve_single key enc () |> Token.parse
  and delete () = delete_single key enc () in
  { key = Some key ; dependent_keys = (=) key ;
    store ; retrieve ; delete ; listeners = [] }

let nickname =
  let key = mangle [ "nickname" ] in
  let enc = Json_encoding.(obj1 (req "nickname" string)) in
  let store value = store_single key enc value
  and retrieve () =
    try retrieve_single key enc () with Not_found -> ""
  and delete () = delete_single key enc () in
  { key = Some key ; dependent_keys = (=) key ;
    store ; retrieve ; delete ; listeners = [] }
  
let cached_exercise name =
  let key = mangle [ "cached-exercise" ; name ] in
  let enc = Learnocaml_exercise.enc in
  let store value = store_single key enc value
  and retrieve () = retrieve_single key enc ()
  and delete () = delete_single key enc () in
  { key = Some key ; dependent_keys = (=) key ;
    store ; retrieve ; delete ; listeners = []  }

let listed list_key item_prefix ?default enc =
  let list =
    let key = mangle list_key in
    let enc = Json_encoding.(list string) in
    let store value = store_single key enc value
    and retrieve () = retrieve_single ~default: [] key enc ()
    and delete () = delete_single key enc () in
    { key = Some key ; dependent_keys = (=) key ;
      store ; retrieve ; delete ; listeners = []  } in
  let item name =
    let key = mangle (item_prefix @ [ name ]) in
    let store value =
      store_single key enc value ;
      let all = retrieve list in
      if not (List.mem name all) then
        store list (name :: all)
    and retrieve () =
      retrieve_single ?default key enc ()
    and delete () =
      delete_single key enc () ;
      let all = retrieve list in
      if List.mem name all then
        store list (List.filter ((<>) name) all)  in
    { key = Some key ; dependent_keys = (=) key ;
      store ; retrieve ; delete ; listeners = []  } in
  let assoc =
    let retrieve () =
      try
        List.fold_left
          (fun acc name -> SMap.add name (retrieve (item name)) acc)
          SMap.empty (retrieve list)
      with Not_found -> SMap.empty
    and delete () =
      let all = retrieve list in
      List.iter (fun name -> delete (item name)) all ;
      delete list in
    let store index =
      delete () ;
      let all =
        SMap.fold
          (fun name state acc ->
             store (item name) state ;
             name :: acc)
          index [] in
      store list all in
    let dependent_keys name =
      let name = demangle name in
      name = list_key ||
      let rec is_prefix p l = match (p, l) with
        | [], [ _ ] -> true
        | [], _ | _, [] -> false
        | pw :: p, lw :: l ->
            pw = lw && is_prefix p l in
      is_prefix item_prefix name in
    { key = None ; dependent_keys ;
      store ; retrieve ; delete ; listeners = []  } in
  list, item, assoc

let exercise_list,
    exercise_state,
    all_exercise_states =
  listed
    [ "exercise-state-list" ]
    [ "exercise-state" ]
    Answer.enc

let toplevel_history_list,
    toplevel_history,
    all_toplevel_histories =
  listed
    [ "toplevel-history-list" ]
    [ "toplevel-history" ]
    ~default: Learnocaml_toplevel_history.empty_snapshot
    Learnocaml_toplevel_history.snapshot_enc

let exercise_toplevel_history_list,
    exercise_toplevel_history,
    all_exercise_toplevel_histories =
  listed
    [ "exercise-toplevel-history-list" ]
    [ "exercise-toplevel-history" ]
    ~default: Learnocaml_toplevel_history.empty_snapshot
    Learnocaml_toplevel_history.snapshot_enc

(* Editor storage *) 
let editor_index=
  let key = mangle [ "editor-index" ] in
  let enc = SMap.enc Editor.editor_state_enc in
  let store value = store_single key enc value
  and retrieve () =
    try retrieve_single key enc () with Not_found -> SMap.empty
  and delete () = delete_single key enc () in
  { key = Some key ; dependent_keys = (=) key ;
    store ; retrieve ; delete ; listeners = [] }
  
let editor_templates =
  let key = mangle [ "editor-templates" ] in
  let enc = Json_encoding.list Editor.editor_template_enc in
  let store value = store_single key enc value
  and retrieve () =
    try retrieve_single key enc () with Not_found -> []
  and delete () = delete_single key enc () in
  { key = Some key ; dependent_keys = (=) key ;
    store ; retrieve ; delete ; listeners = [] }
    
              
