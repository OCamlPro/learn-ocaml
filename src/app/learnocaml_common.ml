(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Js_utils
open Lwt.Infix
open Learnocaml_data
open Learnocaml_config

module H = Tyxml_js.Html

let find_div_or_append_to_body id =
  match Manip.by_id id with
  | Some div -> div
  | None ->
      let div = H.(div ~a:[ a_id id ]) [] in
      Manip.(appendChild Elt.body) div;
      div

let find_component id =
  match Js_utils.Manip.by_id id with
  | Some div -> div
  | None -> failwith ("Cannot find id " ^ id)

let fake_download ~name ~contents =
  (* TODO: add some primitives to jsoo and clean this up  *)
  let blob : (Js.js_string Js.t Js.js_array Js.t -> File.blob Js.t) Js.constr =
    Js.Unsafe.global ##. _Blob in
  let blob = new%js blob (Js.array [| contents |]) in
  let url =
    Js.Unsafe.meth_call (Js.Unsafe.global##._URL) "createObjectURL" [| Js.Unsafe.inject blob |] in
  let link = Dom_html.createA Dom_html.document in
  link##.href := url ;
  Js.Unsafe.set link (Js.string "download") (Js.string name) ;
  ignore (Dom_html.document##.body##(appendChild ((link :> Dom.node Js.t)))) ;
  ignore (Js.Unsafe.meth_call link "click" [||]) ;
  ignore (Dom_html.document##.body##(removeChild ((link :> Dom.node Js.t))))

let fake_upload () =
  let input_files_load =
    Dom_html.createInput ~_type: (Js.string "file") Dom_html.document in
  let result_t, result_wakener = Lwt.wait () in
  let fail () =
    Lwt.wakeup_exn result_wakener
      (Failure "file loading not implemented for this browser") ;
    Js._true in
  input_files_load##.onchange := Dom.handler (fun ev ->
      Js.Opt.case (ev##.target) fail @@ fun target ->
      Js.Opt.case (Dom_html.CoerceTo.input target) fail @@ fun input ->
      Js.Optdef.case (input##.files) fail @@ fun files ->
      Js.Opt.case (files##(item (0))) fail @@ fun file ->
      let name = Js.to_string file##.name in
      let fileReader = new%js File.fileReader in
      fileReader##.onload := Dom.handler (fun ev ->
          Js.Opt.case (ev##.target) fail @@ fun target ->
          Js.Opt.case (File.CoerceTo.string (target##.result)) fail @@ fun result ->
          Lwt.wakeup result_wakener (name, result) ;
          Js._true) ;
      fileReader##(readAsText file) ;
      Js._true) ;
  ignore (Js.Unsafe.meth_call input_files_load "click" [||]) ;
  result_t

let fatal ?(title=[%i"INTERNAL ERROR"]) message =
  let titletext = title in
  let id = "ocp-fatal-layer" in
  let div = match Manip.by_id id with
    | Some div -> div
    | None ->
        let div =
          H.div ~a:[ H.a_id id ;
                     H.a_class ["learnocaml-dialog-overlay"]
                   ]
            []
        in
        Manip.(appendChild Elt.body) div;
        div in
  Manip.replaceChildren div [
    H.div [
      H.h3 [ H.txt titletext ];
      H.div [ H.p [ H.txt (String.trim message) ] ];
    ]
  ]

let dialog_layer_id = "ocp-dialog-layer"

let box_button txt f =
  H.button ~a: [
    H.a_onclick (fun _ ->
        f ();
        match Manip.by_id dialog_layer_id with
        | Some div -> Manip.removeChild Manip.Elt.body div; false
        | None -> (); false)
  ] [ H.txt txt ]

let close_button txt =
  box_button txt @@ fun () -> ()

let ext_alert ~title ?(buttons = [close_button [%i"OK"]]) message =
  let div = match Manip.by_id dialog_layer_id with
    | Some div -> div
    | None ->
        let div =
          H.div ~a:[ H.a_id dialog_layer_id ;
                     H.a_class ["learnocaml-dialog-overlay"] ]
            []
        in
        Manip.(appendChild Elt.body) div;
        div in
  Manip.replaceChildren div [
      H.div [
          H.h3 [ H.pcdata title ];
          H.div message;
          H.div ~a:[ H.a_class ["buttons"] ] buttons;
        ]
    ]

let lwt_alert ~title ~buttons message =
  let waiter, wakener = Lwt.task () in
  let buttons =
    List.map (fun (txt, f) ->
        box_button txt (fun () ->
            Lwt.async @@ fun () ->
            f () >|= Lwt.wakeup_later wakener))
      buttons
  in
  ext_alert ~title message ~buttons;
  waiter

let alert ?(title=[%i"ERROR"]) ?buttons message =
  ext_alert ~title ?buttons [ H.p [H.txt (String.trim message)] ]

let confirm ~title ?(ok_label=[%i"OK"]) ?(cancel_label=[%i"Cancel"]) contents f =
  ext_alert ~title contents ~buttons:[
    box_button ok_label f;
    close_button cancel_label;
  ]

let ask_string ~title ?(ok_label=[%i"OK"]) contents =
  let input_field =
    H.input ~a:[
        H.a_input_type `Text;
      ] ()
  in
  let result_t, up = Lwt.wait () in
  ext_alert ~title (contents @ [input_field]) ~buttons:[
      box_button ok_label (fun () -> Lwt.wakeup up @@ Manip.value input_field)
    ];
  result_t

let default_exn_printer = function
  | Failure msg -> msg
  | e -> Printexc.to_string e

let catch_with_alert ?(printer=default_exn_printer) f =
  Lwt.catch f @@ fun exn -> alert (printer exn); Lwt.return_unit

let hide_loading ?(id = "ocp-loading-layer") () =
  let elt = find_div_or_append_to_body id in
  Manip.(removeClass elt "initial") ;
  Manip.(removeClass elt "loading") ;
  Manip.(addClass elt "loaded")

let show_loading ?(id = "ocp-loading-layer") contents f =
  let show () =
    let elt = find_div_or_append_to_body id in
    Manip.(addClass elt "loading-layer") ;
    Manip.(removeClass elt "loaded") ;
    Manip.(addClass elt "loading") ;
    let chamo_src =
      api_server ^ "/icons/tryocaml_loading_" ^ string_of_int (Random.int 9 + 1) ^ ".gif" in
    Manip.replaceChildren elt
      H.[
        div ~a: [ a_id "chamo" ] [ img ~alt: "loading" ~src: chamo_src () ] ;
        div ~a: [ a_class [ "messages" ] ] contents
      ]
  in
  let hide () =
    let elt = find_div_or_append_to_body id in
    Manip.(removeClass elt "initial") ;
    Manip.(removeClass elt "loading") ;
    Manip.(addClass elt "loaded")
  in
  Lwt.finalize
    (fun () -> show (); f ())
    (fun () -> hide (); Lwt.return_unit)

let set_assoc name value =
  let rec set acc = function
    | [] -> List.rev ((name, value) :: acc)
    | (n, _) :: args when n = name ->
        List.rev_append ((name, value) :: acc) args
    | arg :: args -> set (arg :: acc) args in
  set []

let delete_assoc name =
  List.filter (fun (n, _) -> n <> name)

let arg, set_arg, delete_arg =
  let args = ref (Js_utils.parse_fragment ()) in
  let delete_arg name =
    args := delete_assoc name !args ;
    Js_utils.set_fragment !args in
  let set_arg name value =
    args := set_assoc name value !args ;
    Js_utils.set_fragment !args  in
  let arg name =
    List.assoc name !args in
  arg, set_arg, delete_arg

type button_group =
  (< disabled : bool Js.t Js.prop > Js.t * bool ref) list ref
  * Lwt_mutex.t
  * int ref

let button_group () : button_group =
  (ref [], Lwt_mutex.create (), ref 0)

type button_state =
  bool ref
  * (button_group * < disabled : bool Js.t Js.prop > Js.t) option ref

let button_state () : button_state =
  (ref false, ref None)

let disable_button_group (buttons, _, cpt) =
  incr cpt ;
  if !cpt = 1 then
    List.iter
      (fun (button, _) ->
         button##.disabled := Js.bool true)
      !buttons

let enable_button_group (buttons, _, cpt) =
  decr cpt ;
  if !cpt = 0 then
    List.iter
      (fun (button, state) ->
         if not !state then
           button##.disabled := Js.bool false)
      !buttons

let disable_button (disabled, self) =
  match !self with
  | None ->
      disabled := true
  | Some (_, button) ->
      disabled := true ;
      button##.disabled := Js.bool true

let enable_button (disabled, self) =
  match !self with
  | None ->
      disabled := false
  | Some ((_, _, cpt), button) ->
      disabled := false ;
      if !cpt = 0 then
        button##.disabled := Js.bool false

let button_group_disabled (_, _, cpt) =
  !cpt > 0

let disabling_button_group group cb =
  disable_button_group group ;
  Lwt_js.yield () >>= fun () ->
  Lwt.catch cb
    (function
      | Lwt.Canceled -> Lwt.return ()
      | exn -> Lwt.fail exn) >>= fun res ->
  enable_button_group group ;
  Lwt_js.yield () >>= fun () ->
  Lwt.return res

let disable_with_button_group component (buttons, _, _) =
  buttons :=
    ((component :> < disabled : bool Js.t Js.prop > Js.t), ref false)
    :: !buttons

let button ~container ~theme ?group ?state ~icon lbl cb =
  let (others, mutex, cnt) as group =
    match group with
    | None -> button_group ()
    | Some group -> group in
  let button =
    H.(button [
        img ~alt:"" ~src:(api_server ^ "/icons/icon_" ^ icon ^ "_" ^ theme ^ ".svg") () ;
        txt " " ;
        span ~a:[ a_class [ "label" ] ] [ txt lbl ]
      ]) in
  Manip.Ev.onclick button
    (fun _ ->
       begin Lwt.async @@ fun () ->
         Lwt_mutex.with_lock mutex @@ fun () ->
         disabling_button_group group cb
       end ;
       true) ;
  let dom_button =
    (Tyxml_js.To_dom.of_button button
     :> < disabled : bool Js.t Js.prop > Js.t) in
  let self_disabled =
    match state with
    | None -> ref false
    | Some (disabled, self) ->
        self := Some (group, dom_button) ;
        disabled in
  others := (dom_button, self_disabled) :: !others ;
  if !self_disabled || !cnt > 0 then
    dom_button##.disabled := Js.bool true ;
  Manip.appendChild container button

let dropdown ~id ~title items =
    let toggle _ =
      let menu = find_component id in
      let disp =
        match Manip.Css.display menu with
        | "block" -> "none"
        | _ ->
            Lwt_js_events.async (fun () ->
                Lwt_js_events.click window >|= fun _ ->
                Manip.SetCss.display menu "none"
              );
            "block"
      in
      Manip.SetCss.display menu disp;
      false
    in
    H.div ~a: [H.a_class ["dropdown_btn"]] [
      H.button ~a: [H.a_onclick toggle]
        (title @ [H.txt " \xe2\x96\xb4" (* U+25B4 *)]);
      H.div ~a: [H.a_id id; H.a_class ["dropdown_content"]] items
    ]

let gettimeofday () =
  (new%js Js.date_now)##getTime /. 1000.

let render_rich_text ?on_runnable_clicked text =
  let open Learnocaml_data.Tutorial in
  let rec render acc text =
    match text with
    | [] -> List.rev acc
    | Text text :: rest ->
        render
          (H.txt text :: acc)
          rest
    | Code { code ; runnable } :: rest ->
        let elt = H.code [ H.txt code ] in
        (match runnable, on_runnable_clicked with
         | true, Some cb ->
             Manip.addClass elt "runnable" ;
             Manip.Ev.onclick elt (fun _ -> cb code ; true)
         | _ -> ()) ;
        render (elt :: acc) rest ;
    | Emph text :: rest ->
        render
          (H.em (render [] text) :: acc)
          rest
    | Image _ :: _ -> assert false
    | Math code :: rest ->
        render
          (H.txt ("`" ^ code ^ "`") :: acc)
          rest in
  (render [] text
   :> [< Html_types.phrasing > `Code `Em `PCDATA ] H.elt list)

let extract_text_from_rich_text text =
  let open Learnocaml_data.Tutorial in
  let rec render acc text =
    match text with
    | [] -> String.concat " " (List.rev acc)
    | Text text :: rest ->
        render (text :: acc) rest
    | Code { code ; _ } :: rest ->
        render (("[" ^ code ^ "]") :: acc) rest
    | Emph text :: rest ->
        render (("*" ^ render [] text ^ "*") :: acc) rest
    | Image { alt ; _ } :: rest ->
        render (("(" ^ alt ^ ")") :: acc) rest
    | Math code :: rest ->
        render (("$" ^ code ^ "$") :: acc) rest in
  render [] text

let set_state_from_save_file ?token save =
  let open Learnocaml_data.Save in
  let open Learnocaml_local_storage in
  (match token with None -> () | Some t -> store sync_token t);
  store nickname save.nickname;
  store all_exercise_states
    (SMap.merge (fun _ ans edi ->
         match ans, edi with
         | Some ans, Some (mtime, solution) ->
             Some {ans with Answer.solution; mtime}
         | None, Some (mtime, solution) ->
             Some Answer.{grade = None; report = None; solution; mtime}
         | ans, _ -> ans)
        save.all_exercise_states save.all_exercise_editors);
  store all_toplevel_histories save.all_toplevel_histories;
  store all_exercise_toplevel_histories save.all_exercise_toplevel_histories

let rec retrieve ?ignore req =
  Server_caller.request req >>= function
  | Ok x -> Lwt.return x
  | Error e ->
      lwt_alert ~title:[%i"REQUEST ERROR"] [
        H.p [H.txt [%i"Could not retrieve data from server"]];
        H.code [H.txt (Server_caller.string_of_error e)];
      ] ~buttons:(
        ([%i"Retry"], (fun () -> retrieve req)) ::
        (match ignore with
         | None -> []
         | Some v -> [[%i"Ignore"], fun () -> Lwt.return v]) @
        [[%i"Cancel"], (fun () -> Lwt.fail Lwt.Canceled)]
      )

let get_state_as_save_file ?(include_reports = false) () =
  let open Learnocaml_data.Save in
  let open Learnocaml_local_storage in
  let answers = retrieve all_exercise_states in
  {
    nickname = retrieve nickname;
    all_exercise_editors =
      if include_reports then SMap.empty
      else SMap.map (fun a -> a.Answer.mtime, a.Answer.solution) answers;
    all_exercise_states =
      if include_reports then answers
      else SMap.empty;
    all_toplevel_histories = retrieve all_toplevel_histories;
    all_exercise_toplevel_histories = retrieve all_exercise_toplevel_histories;
  }

let rec sync_save token save_file =
  Server_caller.request (Learnocaml_api.Update_save (token, save_file))
  >>= function
  | Ok save -> set_state_from_save_file ~token save; Lwt.return save
  | Error (`Not_found _) ->
      Server_caller.request_exn
        (Learnocaml_api.Create_token ("", Some token, None)) >>= fun _token ->
      assert (_token = token);
      Server_caller.request_exn
        (Learnocaml_api.Update_save (token, save_file)) >>= fun save ->
      set_state_from_save_file ~token save;
      Lwt.return save
  | Error e ->
      lwt_alert ~title:[%i"SYNC FAILED"] [
        H.p [H.txt [%i"Could not synchronise save with the server"]];
        H.code [H.txt (Server_caller.string_of_error e)];
      ] ~buttons:[
        [%i"Retry"], (fun () -> sync_save token save_file);
        [%i"Ignore"], (fun () -> Lwt.return save_file);
      ]

let sync token = sync_save token (get_state_as_save_file ())

let sync_exercise token ?answer ?editor id =
  let handle_serverless () =
    (* save the text at least locally (but not the report & grade, that could
       be misleading) *)
    let txt = match editor, answer with
      | Some t, _ -> Some t
      | _, Some a -> Some a.Answer.solution
      | _ -> None
    in
    match txt with
    | Some txt ->
       let key = Learnocaml_local_storage.exercise_state id in
       let a0 = Learnocaml_local_storage.retrieve key in
       Learnocaml_local_storage.store key
         {a0 with Answer.
          solution = txt;
          mtime = gettimeofday () }
    | None -> ()
  in
  let nickname = Learnocaml_local_storage.(retrieve nickname) in
  let toplevel_history =
    SMap.find_opt id Learnocaml_local_storage.(retrieve all_toplevel_histories)
  in
  let txt = match editor with None -> None | Some e -> Some (max_float, e) in
  let opt_to_map = function
    | Some i -> SMap.singleton id i
    | None -> SMap.empty
  in
  let save_file = Save.{
    nickname;
    all_exercise_editors = opt_to_map txt;
    all_exercise_states = opt_to_map answer;
    all_toplevel_histories = SMap.empty;
    all_exercise_toplevel_histories = opt_to_map toplevel_history;
  } in
  match token with
  | Some token ->
     Lwt.catch (fun () -> sync_save token save_file)
       (fun e ->
         handle_serverless ();
         raise e)
  | None -> set_state_from_save_file save_file;
            handle_serverless ();
            Lwt.return save_file

let string_of_seconds seconds =
  let days = seconds / 24 / 60 / 60 in
  let hours = seconds / 60 / 60 mod 24 in
  let minutes = seconds / 60 mod 60 in
  let seconds = seconds mod 60 in
  if days >= 1 then Printf.sprintf [%if"%dd %02dh"] days hours else
  if hours >= 1 then Printf.sprintf [%if"%02d:%02d"] hours minutes else
    Printf.sprintf [%if"0:%02d:%02d"] minutes seconds

let countdown ?(ontimeout = fun () -> ()) container t =
  let deadline = gettimeofday () +. t in
  let update_interval seconds =
    if seconds >= 24 * 60 * 60 then 1000. *. 60. *. 60.
    else if seconds >= 60 * 60 then 1000. *. 60.
    else 1000.
  in
  let update remaining =
    Manip.setInnerText container (string_of_seconds remaining)
  in
  let rec callback () =
    let remaining = int_of_float (deadline -. gettimeofday ()) in
    if remaining <= 0 then
      (update 0;
       ontimeout ())
    else
      (update remaining;
       ignore (window##setTimeout
                 (Js.wrap_callback callback)
                 (update_interval remaining)))
  in
  callback ()

let flog fmt = Printf.ksprintf (fun s -> Firebug.console##log(Js.string s)) fmt

let stars_div stars =
  H.div ~a:[ H.a_class [ "stars" ] ] [
    let num = 5 * int_of_float (stars *. 2.) in
    let num = max (min num 40) 0 in
    let alt = Format.asprintf [%if"difficulty: %d / 40"] num in
    let src = Format.asprintf "%s/icons/stars_%02d.svg" api_server num in
    H.img ~alt ~src ()
  ]

let exercise_text ex_meta exo =
  let mathjax_url =
    api_server ^ "/js/mathjax/MathJax.js?delayStartupUntil=configured"
  in
  let mathjax_config =
    "MathJax.Hub.Config({\n\
    \  jax: [\"input/AsciiMath\", \"output/HTML-CSS\"],\n\
    \  extensions: [],\n\
    \  showMathMenu: false,\n\
    \  showMathMenuMSIE: false,\n\
    \  \"HTML-CSS\": {\n\
    \    imageFont: null\n\
    \  }
          });"
    (* the following would allow comma instead of dot for the decimal separator,
       but should depend on the language the exercise is in, not the language of the
       app
       "AsciiMath: {\n\
       \  decimal: \"" ^[%i"."]^ "\"\n\
        },\n"
    *)
  in
  (* Looking for the description in the correct language. *)
  let descr =
    let lang = "" in
    try
      List.assoc lang (Learnocaml_exercise.(access File.descr exo))
    with
      Not_found ->
        try List.assoc "" (Learnocaml_exercise.(access File.descr exo))
        with Not_found -> [%i "No description available for this exercise." ]
  in
  Format.asprintf
    "<!DOCTYPE html>\
     <html><head>\
     <title>%s - exercise text</title>\
     <meta charset='UTF-8'>\
     <link rel='stylesheet' href='%s/css/learnocaml_standalone_description.css'>\
     <script type='text/x-mathjax-config'>%s</script>
            <script type='text/javascript' src='%s'></script>\
     </head>\
     <body>\
     %s\
     </body>\
     <script type='text/javascript'>MathJax.Hub.Configured()</script>\
     </html>"
    ex_meta.Exercise.Meta.title
    api_server
    mathjax_config
    mathjax_url
    descr

let string_of_exercise_kind = function
  | Exercise.Meta.Project -> [%i"project"]
  | Exercise.Meta.Problem -> [%i"problem"]
  | Exercise.Meta.Exercise -> [%i"exercise"]

let grade_color = function
  | None -> "#808080"
  | Some score ->
      Printf.sprintf "hsl(%d, 100%%, 67%%)"
        (int_of_float (float_of_int score /. 100. *. 138.))

let get_assignments tokens exos_status =
  let module ES = Exercise.Status in
  let module ATM = Map.Make(struct
      type t = (float * float) * Token.Set.t * bool
      let compare (d1, ts1, dft1) (d2, ts2, dft2) =
        match compare d1 d2 with
        | 0 -> (match Token.Set.compare ts1 ts2 with
            | 0 -> compare dft1 dft2
            | n -> n)
        | n -> n
    end)
  in
  let atm_add atm key id =
    match ATM.find_opt key atm with
    | None -> ATM.add key (SSet.singleton id) atm
    | Some set -> ATM.add key (SSet.add id set) atm
  in
  let atm =
    SMap.fold (fun id st atm ->
        let assg = st.ES.assignments in
        let default = ES.default_assignment assg in
        let stl = ES.by_status tokens assg in
        let atm = match default with
          | ES.Assigned {start; stop} ->
              let explicit_tokens =
                Token.Map.fold (fun tok _ -> Token.Set.add tok)
                  assg.ES.token_map Token.Set.empty
              in
              let implicit_tokens =
                Token.Set.diff tokens explicit_tokens
              in
              atm_add atm ((start, stop), implicit_tokens, true) id
          | _ -> atm
        in
        List.fold_left (fun atm (status, tokens) ->
            match status with
            | ES.Open | ES.Closed -> atm
            | ES.Assigned {start; stop} ->
                let key = (start, stop), tokens, (status = default) in
                match ATM.find_opt key atm with
                | None ->
                    ATM.add key (SSet.singleton id) atm
                | Some ids ->
                    ATM.add key (SSet.add id ids) atm)
          atm
          stl)
      exos_status
      ATM.empty
  in
  ATM.fold (fun (assg, tokens, dft) exos l ->
      (assg, tokens, dft, exos) :: l)
    atm []
  |> List.rev

let string_of_date ?(time=false) t =
  let date = new%js Js.date_fromTimeValue (t *. 1000.) in
  if time then
    Printf.sprintf "%04d-%02d-%02d %02d:%02d"
      date##getFullYear (date##getMonth + 1) date##getDate
      date##getHours date##getMinutes
  else
    Printf.sprintf "%04d-%02d-%02d"
      date##getFullYear (date##getMonth + 1) date##getDate

let date ?(time=false) t =
  let date = new%js Js.date_fromTimeValue (t *. 1000.) in
  H.time ~a:[ H.a_datetime (Js.to_string date##toISOString) ] [
    H.txt
      (Js.to_string (if time then date##toLocaleString
                     else date##toLocaleDateString))
  ]

let tag_span tag =
  let color =
    Printf.sprintf "#%06x" ((Hashtbl.hash tag lor 0x808080) land 0xffffff)
  in
  H.span ~a:[H.a_class ["tag"];
             H.a_style ("background-color: "^color)]
    [H.txt tag]

let get_worker_code name =
  let worker_url = ref None in
  fun () -> match !worker_url with
    | None ->
        retrieve (Learnocaml_api.Static ["js"; name]) >|= fun js ->
        let url = js_code_url js in worker_url := Some url; url
    | Some url -> Lwt.return url

let mouseover_toggle_signal elt sigvalue setter =
  let rec hdl _ =
    Manip.Ev.onmouseout elt (fun _ ->
        setter None;
        Manip.Ev.onmouseover elt hdl;
        true
      );
    setter (Some sigvalue);
    true
  in
  Manip.Ev.onmouseover elt hdl

let ace_display tab =
  let ace = lazy (
    let answer =
      Ocaml_mode.create_ocaml_editor
        (Tyxml_js.To_dom.of_div tab)
    in
    let ace = Ocaml_mode.get_editor answer in
    Ace.set_font_size ace 16;
    Ace.set_readonly ace true;
    ace
  ) in
  (fun ans ->
     Ace.set_contents (Lazy.force ace) ~reset_undo:true ans),
  (fun () ->
    Ace.set_contents (Lazy.force ace) ~reset_undo:true "")

let toplevel_launch ?display_welcome ?after_init ?(on_disable=fun () -> ()) ?(on_enable=fun () -> ())
      container history on_show toplevel_buttons_group id =
  let timeout_prompt =
    Learnocaml_toplevel.make_timeout_popup ~on_show () in
  let flood_prompt =
    Learnocaml_toplevel.make_flood_popup ~on_show () in
  let history =
    let storage_key = history id in
    let on_update self =
      Learnocaml_local_storage.store storage_key
        (Learnocaml_toplevel_history.snapshot self) in
    let snapshot =
      Learnocaml_local_storage.retrieve storage_key in
    Learnocaml_toplevel_history.create
      ~gettimeofday
      ~on_update
      ~max_size: 99
      ~snapshot () in
  get_worker_code "learnocaml-toplevel-worker.js" () >>= fun worker_js_file ->
  Learnocaml_toplevel.create ~worker_js_file
    ?display_welcome ?after_init ~timeout_prompt ~flood_prompt
    ~on_disable_input: (fun _ -> on_disable (); disable_button_group toplevel_buttons_group)
    ~on_enable_input: (fun _ -> on_enable (); enable_button_group toplevel_buttons_group)
    ~container
    ~history ()

let init_toplevel_pane toplevel_launch top toplevel_buttons_group toplevel_button =
  begin toplevel_button
      ~icon: "cleanup" [%i"Clear"] @@ fun () ->
    Learnocaml_toplevel.clear top ;
    Lwt.return ()
  end ;
  begin toplevel_button
      ~icon: "reload" [%i"Reset"] @@ fun () ->
    toplevel_launch >>= fun top ->
    disabling_button_group toplevel_buttons_group (fun () -> Learnocaml_toplevel.reset top)
  end ;
  begin toplevel_button
      ~icon: "run" [%i"Eval phrase"] @@ fun () ->
    Learnocaml_toplevel.execute top ;
    Lwt.return ()
  end

let set_inner_list lst =
  let aux (id, text) =
    match Js_utils.Manip.by_id id with
    | None -> ()
    | Some component ->
       Manip.setInnerHtml component text in
  List.iter aux lst

let set_string_translations_exercises () =
  let translations = [
    "txt_preparing", [%i"Preparing the environment"];
    "learnocaml-exo-button-editor", [%i"Editor"];
    "learnocaml-exo-button-toplevel", [%i"Toplevel"];
    "learnocaml-exo-button-report", [%i"Report"];
    "learnocaml-exo-button-text", [%i"Exercise"];
    "learnocaml-exo-button-meta", [%i"Details"];
    "learnocaml-exo-editor-pane", [%i"Editor"];
    "txt_grade_report", [%i"Click the Grade button to get your report"];
  ] in set_inner_list translations

let set_string_translations_view () =
  let translations = [
    "txt_loading", [%i"Loading student data"];
    "learnocaml-exo-button-stats", [%i"Stats"];
    "learnocaml-exo-button-list", [%i"Exercises"];
    "learnocaml-exo-button-report", [%i"Report"];
    "learnocaml-exo-button-text", [%i"Subject"];
    "learnocaml-exo-button-editor", [%i"Answer"];
  ] in set_inner_list translations

let local_save ace id =
  let key = Learnocaml_local_storage.exercise_state id in
  let ans =
    try Learnocaml_local_storage.retrieve key with Not_found ->
      Answer.{solution = ""; mtime = 0.; report = None; grade = None}
  in
  Learnocaml_local_storage.store key
    { ans with Answer.solution = Ace.get_contents ace;
               mtime = gettimeofday () }

let run_async_with_log f =
  Lwt.async_exception_hook := begin fun e ->
    Firebug.console##log (Js.string
                            (Printexc.to_string e ^
                               if Printexc.backtrace_status () then
                                 Printexc.get_backtrace ()
                               else ""));
    match e with
    | Failure message -> fatal message
    | Server_caller.Cannot_fetch message -> fatal message
    | exn -> fatal (Printexc.to_string exn)
  end ;
  (match Js_utils.get_lang() with Some l -> Ocplib_i18n.set_lang l | None -> ());
  Lwt.async f

let mk_tab_handlers default_tab other_tabs =
  let names = default_tab::other_tabs in
  let current = ref default_tab in
  let select_tab name =
    set_arg "tab" name ;
    Manip.removeClass
      (find_component ("learnocaml-exo-button-" ^ !current))
      "front-tab" ;
    Manip.removeClass
      (find_component ("learnocaml-exo-tab-" ^ !current))
      "front-tab" ;
    Manip.enable
      (find_component ("learnocaml-exo-button-" ^ !current)) ;
    Manip.addClass
      (find_component ("learnocaml-exo-button-" ^ name))
      "front-tab" ;
    Manip.addClass
      (find_component ("learnocaml-exo-tab-" ^ name))
      "front-tab" ;
    Manip.disable
      (find_component ("learnocaml-exo-button-" ^ name)) ;
    current := name in
  let init_tabs () =
    current :=
      begin
        try
          let requested = arg "tab" in
          if List.mem requested names then requested else default_tab
        with Not_found -> default_tab
      end ;
    List.iter
      (fun name ->
        Manip.removeClass
          (find_component ("learnocaml-exo-button-" ^ name))
          "front-tab" ;
        Manip.removeClass
          (find_component ("learnocaml-exo-tab-" ^ name))
          "front-tab" ;
        Manip.Ev.onclick
          (find_component ("learnocaml-exo-button-" ^ name))
          (fun _ -> select_tab name ; true))
      names ;
    select_tab !current in
  init_tabs, select_tab

module type Editor_info = sig
  val ace : Ocaml_mode.editor Ace.editor
  val buttons_container : 'a Tyxml_js.Html5.elt
end

module Editor_button (E : Editor_info) = struct

  let editor_button = button ~container:E.buttons_container ~theme:"light"

  let cleanup template =
  editor_button
    ~icon: "cleanup" [%i"Reset"] @@ fun () ->
    confirm ~title:[%i"START FROM SCRATCH"]
      [H.txt [%i"This will discard all your edits. Are you sure?"]]
      (fun () ->
         Ace.set_contents E.ace template);
    Lwt.return ()

  let download id =
    editor_button
      ~icon: "download" [%i"Download"] @@ fun () ->
      let name = id ^ ".ml" in
      let contents = Js.string (Ace.get_contents E.ace) in
      fake_download ~name ~contents ;
      Lwt.return ()

  let eval top select_tab =
    editor_button
      ~icon: "run" [%i"Eval code"] @@ fun () ->
      Learnocaml_toplevel.execute_phrase top (Ace.get_contents E.ace) >>= fun _ ->
      select_tab "toplevel";
      Lwt.return_unit

  let sync token id =
    editor_button
      ~icon: "sync" [%i"Sync"] @@ fun () ->
      token >>= fun token ->
      sync_exercise token id ~editor:(Ace.get_contents E.ace) >|= fun _save -> ()
end

let setup_editor solution =
  let editor_pane = find_component "learnocaml-exo-editor-pane" in
  let editor = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_pane) in
  let ace = Ocaml_mode.get_editor editor in
  Ace.set_contents ace ~reset_undo:true solution;
  Ace.set_font_size ace 18;
  editor, ace

let typecheck top ace editor set_class =
  Learnocaml_toplevel.check top (Ace.get_contents ace) >>= fun res ->
  let error, warnings =
    match res with
    | Toploop_results.Ok ((), warnings) -> None, warnings
    | Toploop_results.Error (err, warnings) -> Some err, warnings in
  let transl_loc { Toploop_results.loc_start ; loc_end } =
    { Ocaml_mode.loc_start ; loc_end } in
  let error = match error with
    | None -> None
    | Some { Toploop_results.locs ; msg ; if_highlight } ->
       Some { Ocaml_mode.locs = List.map transl_loc locs ;
              msg = (if if_highlight <> "" then if_highlight else msg) } in
  let warnings =
    List.map
      (fun { Toploop_results.locs ; msg ; if_highlight } ->
        { Ocaml_mode.loc = transl_loc (List.hd locs) ;
          msg = (if if_highlight <> "" then if_highlight else msg) })
      warnings in
  Ocaml_mode.report_error ~set_class editor error warnings >|= fun () ->
  Ace.focus ace

let set_nickname_div () =
  let nickname_div = find_component "learnocaml-nickname" in
  match Learnocaml_local_storage.(retrieve nickname) with
  | nickname -> Manip.setInnerText nickname_div nickname
  | exception Not_found -> ()

let setup_prelude_pane ace prelude =
  if prelude = "" then () else
  let editor_pane = find_component "learnocaml-exo-editor-pane" in
  let prelude_pane = find_component "learnocaml-exo-prelude" in
  let open Tyxml_js.Html5 in
  let state =
    ref (match arg "prelude" with
         | exception Not_found -> true
         | "shown" -> true
         | "hidden" -> false
         | _ -> failwith "Bad format for argument prelude.") in
  let prelude_btn = button [] in
  let prelude_title = h1 [ txt [%i"OCaml prelude"] ;
                           prelude_btn ] in
  let prelude_container =
    pre ~a: [ a_class [ "toplevel-code" ] ]
      (Learnocaml_toplevel_output.format_ocaml_code prelude) in
  let update () =
    if !state then begin
        Manip.replaceChildren prelude_btn [ txt ("↳ "^[%i"Hide"]) ] ;
        Manip.SetCss.display prelude_container "" ;
        Manip.SetCss.top editor_pane "193px" ; (* 150 + 43 *)
        Manip.SetCss.bottom editor_pane "40px" ;
        Ace.resize ace true;
        set_arg "prelude" "shown"
      end else begin
        Manip.replaceChildren prelude_btn [ txt ("↰ "^[%i"Show"]) ] ;
        Manip.SetCss.display prelude_container "none" ;
        Manip.SetCss.top editor_pane "43px" ;
        Manip.SetCss.bottom editor_pane "40px" ;
        Ace.resize ace true;
        set_arg "prelude" "hidden"
      end in
  update () ;
  Manip.Ev.onclick prelude_btn
    (fun _ -> state := not !state ; update () ; true) ;
  Manip.appendChildren prelude_pane
    [ prelude_title ; prelude_container ]
    
let get_token ?(has_server = true) () =
  if not has_server then
    Lwt.return None
  else
    try
      Some Learnocaml_local_storage.(retrieve sync_token) |>
        Lwt.return
    with Not_found ->
      retrieve (Learnocaml_api.Nonce ())
      >>= fun nonce ->
      ask_string ~title:"Secret"
        [H.txt [%i"Enter the secret"]]
      >>= fun secret ->
      retrieve
        (Learnocaml_api.Create_token (Sha.sha512 (nonce ^ Sha.sha512 secret), None, None))
      >|= fun token ->
      Learnocaml_local_storage.(store sync_token) token;
      Some token
      
module Display_exercise =
  functor (
    Q: sig
      val exercise_link: ?cl:string list ->
                         string -> 'a Tyxml_js.Html.elt list -> [> 'a Html_types.a ] Tyxml_js.Html.elt
    end) ->
  struct
    open Q
    let display_descr ex_meta =
      let open Tyxml_js.Html5 in
      let open Learnocaml_data.Exercise in
      match ex_meta.Meta.short_description with
      | None -> div ~a:[ a_class [ "descr" ] ] []
      | Some descr ->
         div ~a:[ a_class [ "descr" ] ] [
             h2 ~a:[ a_class [ "learnocaml-exo-meta-category" ] ]
               [ txt ex_meta.Meta.title ] ;
             p [ txt descr ]
           ]

    let display_stars ex_meta =
      let open Tyxml_js.Html5 in
      let open Learnocaml_data.Exercise in
      let stars =
        let num = 5 * int_of_float (ex_meta.Meta.stars *. 2.) in
        let num = max (min num 40) 0 in
        let alt = Format.asprintf [%if"difficulty: %d / 40"] num in
        let src = Format.asprintf "%s/icons/stars_%02d.svg" api_server num in
        img ~alt ~src ()
      in
      div ~a:[ a_class [ "stars" ] ] [
          p [
              txt [%i "Difficulty:"] ;
              txt " "; (* Put no whitespace in translation strings
                             (the colon is mandatory, though, given
                             the conventions are different in English
                             and French, for example). *)
              stars
            ]
        ]

    let display_kind ex_meta =
      let open Tyxml_js.Html5 in
      let open Learnocaml_data.Exercise in
      let kind_repr = string_of_exercise_kind ex_meta.Meta.kind in
      div ~a:[ a_class [ "length" ] ] [
          p [ txt (Format.sprintf [%if "Kind: %s"] kind_repr) ]
        ]

    let display_exercise_meta id meta content_id =
      let content = find_component content_id in
      let descr =
        exercise_link ~cl:[ "exercise" ] id [
            display_descr meta ;
            H.div ~a:[  ] [
                display_stars meta ;
                display_kind meta ;
              ]
          ]
      in
      Manip.replaceChildren content [ descr ];
      Lwt.return ()

    let display_list ?(sep=Tyxml_js.Html5.txt ", ") l =
      let open Tyxml_js.Html5 in
      let rec gen acc = function
        | [] -> [ txt "" ]
        | a :: [] -> a :: acc
        | a :: ((_ :: _) as rem) ->
           gen (sep :: (a  :: acc)) rem
      in
      gen [] l |> List.rev

    let get_skill_index token =
      let index = lazy (
                      retrieve (Learnocaml_api.Exercise_index (Some token))
    >|= fun (index, _) ->
                      Exercise.Index.fold_exercises (fun (req, focus) id meta ->
                          let add sk id map =
                            SMap.add sk
                              (SSet.add id (try SMap.find sk map with Not_found -> SSet.empty))
                              map
                          in
                          List.fold_left (fun acc sk -> add sk id acc) req
                            meta.Exercise.Meta.requirements,
                          List.fold_left (fun acc sk -> add sk id acc) focus
                            meta.Exercise.Meta.focus
                        ) (SMap.empty, SMap.empty) index
                    ) in
      fun skill ->
      Lazy.force index >|= fun (req, focus) ->
      try match skill with
          | `Requirements s -> SSet.elements (SMap.find s req)
          | `Focus s -> SSet.elements (SMap.find s focus)
      with Not_found -> []

    let display_skill_meta _skill exs content_id =
      let content = find_component content_id in
      Manip.replaceChildren content
        (display_list @@
           List.map (fun ex_id ->
               exercise_link ex_id [Tyxml_js.Html5.txt ex_id]) exs);
      Lwt.return ()

    let display_link onclick content_id value =
      let open Tyxml_js.Html5 in
      let cid = Format.asprintf "%s-%s" content_id value in
      let expand_id = Format.asprintf "%s-expand" cid in
      let displayed = ref false in
      let onclick _ =
        let exp = find_component expand_id in
        if !displayed then
          Manip.removeChildren (find_component cid)
        else
          ignore (onclick cid);
        Manip.removeChildren exp;
        Manip.appendChild exp (txt (if !displayed then "[-]" else "[+]"));
        displayed := not !displayed;
        true
      in
      div [ p ~a:[ a_class [ "learnocaml-exo-expandable-link" ] ;
                   a_onclick onclick ] [
                span ~a:[ a_id expand_id ;
                          a_class ["expand-sign"] ] [ txt "[+]" ] ;
                txt value ] ;
            div ~a:[ a_id cid ;
                     a_class [ "learnocaml-exo-meta-category" ] ]
              []
        ]

    let display_skill_link index content_id s =
      let skill = match s with `Focus s | `Requirements s -> s in
      display_link (display_skill_meta s index) content_id skill

    let display_exercise_link content_id meta e =
      display_link (display_exercise_meta e meta) content_id e

    let display_authors caption_text authors =
      let open Tyxml_js.Html5 in
      let author (name, mail) =
        span [ txt name ;
               txt " <" ;
               a ~a:[ a_href ("mailto:" ^ mail) ] [ txt mail ] ;
               txt ">" ;
          ] in
      span [ txt caption_text; txt " " ]
      :: (display_list @@ List.map author authors)

    let add_map_set sk id map =
      SMap.add sk
        (SSet.add id (try SMap.find sk map with Not_found -> SSet.empty))
        map

    let extract_maps_exo_index index =
      Exercise.Index.fold_exercises
        (fun (req, focus) id meta ->
          (List.fold_left (fun acc sk -> add_map_set sk id acc) req
             meta.Exercise.Meta.requirements,
           List.fold_left (fun acc sk -> add_map_set sk id acc) focus
             meta.Exercise.Meta.focus))
        (SMap.empty, SMap.empty) index

    let opt_display_skills caption map label fskill = function
      | [] -> None
      | skills ->
         Some (caption,
               display_list ~sep:(H.txt "") @@
                 List.map (fun s ->
                     display_skill_link
                       (try SSet.elements (SMap.find s map) with Not_found -> [])
                       label (fskill s))
                   skills)

    let opt_display_adjacent_exos index label exos caption =
      List.fold_left (fun acc id ->
          match Exercise.Index.find_opt index id with
          |  Some meta ->
              display_exercise_link label meta id :: acc
          | None -> acc)
        []
        (List.rev exos)
      |> function
        | [] -> None
        | l -> Some (caption, display_list ~sep:(H.txt "") l)

    let display_meta token ex_meta id =
      let open Learnocaml_data.Exercise in
      let ident = Format.asprintf "%s %s" [%i "Identifier:" ] id in
      let authors =
        match ex_meta.Meta.author with
        | [] -> None
        | [author] -> Some (display_authors [%i "Author:"] [author])
        | authors -> Some (display_authors [%i "Authors:"] authors) in
      retrieve (Learnocaml_api.Exercise_index token)
      >|= fun (index, _) ->
      let req_map, focus_map = extract_maps_exo_index index in
      let focus =
        opt_display_skills [%i "Skills trained:"] focus_map
          "learnocaml-exo-focus-meta" (fun s -> `Focus s)
          ex_meta.Meta.focus in
      let requirements =
        opt_display_skills [%i "Skills required:"] req_map
          "learnocaml-exo-requirements-meta" (fun s -> `Requirements s)
          ex_meta.Meta.requirements in
      let backward =
        opt_display_adjacent_exos index "learnocaml-exo-backward-meta"
          ex_meta.Meta.backward [%i "Previous exercises:"] in
      let forward =
        opt_display_adjacent_exos index "learnocaml-exo-forward-meta"
          ex_meta.Meta.forward [%i "Next exercises:"] in
      let tab = find_div_or_append_to_body "learnocaml-exo-tab-meta" in
      Manip.replaceChildren tab @@
        Tyxml_js.Html5.([
              h1 ~a:[ a_class [ "learnocaml-exo-meta-title" ] ]
                [ txt [%i "Metadata" ] ] ;
              div ~a:[ a_id "learnocaml-exo-content-meta" ] @@
                [ display_descr ex_meta ;
                  display_stars ex_meta ;
                  display_kind ex_meta ;
                  p [ txt ident ] ;
                  (match authors with Some a -> p a | None -> div [])
                ] @ List.map
                      (function
                       | Some (title, values) ->
                          div (h2 ~a:[ a_class
                                         [ "learnocaml-exo-meta-category-title" ] ]
                                 [ txt title ] :: values)
                       | None -> div [])
                      [ focus ; requirements ; backward ; forward ]
        ])
  end

module Grade_exercise = struct

let get_grade =
  let get_worker = get_worker_code "learnocaml-grader-worker.js" in
  fun ?callback ?timeout exercise ->
    get_worker () >>= fun worker_js_file ->
    Grading_jsoo.get_grade ~worker_js_file ?callback ?timeout exercise

let display_report exo report =
  let score, _failed = Report.result report in
  let report_button = find_component "learnocaml-exo-button-report" in
  Manip.removeClass report_button "success" ;
  Manip.removeClass report_button "failure" ;
  Manip.removeClass report_button "partial" ;
  let grade =
    let max = Learnocaml_exercise.(access File.max_score exo) in
    if max = 0 then 999 else score * 100 / max
  in
  if grade >= 100 then begin
    Manip.addClass report_button "success" ;
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata [%i"Report"] ]
  end else if grade = 0 then begin
    Manip.addClass report_button "failure" ;
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata [%i"Report"] ]
  end else begin
    Manip.addClass report_button "partial" ;
    let pct = Format.asprintf "%2d%%" grade in
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata [%i"Report"] ;
                       span ~a: [ a_class [ "score" ] ] [ pcdata pct ]]
  end ;
  let report_container = find_component "learnocaml-exo-tab-report" in
  Manip.setInnerHtml report_container
    (Format.asprintf "%a" Report.(output_html ~bare: true) report) ;
  grade

end
