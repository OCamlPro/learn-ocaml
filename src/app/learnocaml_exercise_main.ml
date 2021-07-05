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
open Learnocaml_common
open Learnocaml_data
open Learnocaml_config
open Grade_exercise

module H = Tyxml_js.Html

let init_tabs, select_tab =
  mk_tab_handlers "text"  [ "toplevel" ; "report" ; "editor"; "meta" ]

let check_if_need_refresh has_server =
  if has_server then
    let local_server_id = Learnocaml_local_storage.(retrieve server_id) in
    retrieve @@ Learnocaml_api.Version ()
    >|= (fun (_, server_id) ->
    if local_server_id <> server_id then
      let title = [%i "WARNING: You have an older grader version than the server"]
      and ok_label = [%i "Refresh the page"]
      and refresh () = Dom_html.window##.location##reload
      and cancel_label = [%i "I will do it myself!"]
      and message = [%i "The server has been updated, please refresh the page to make sure you are using the latest version of Learn-OCaml server (none of your work will be lost)."] in
      let contents = [ H.p [H.txt (String.trim message) ] ] in
      confirm ~title ~ok_label ~cancel_label contents refresh)
  else
    Lwt.return_unit

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
      Tyxml_js.Html5.[ txt [%i"Report"] ]
  end else if grade = 0 then begin
    Manip.addClass report_button "failure" ;
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ txt [%i"Report"] ]
  end else begin
    Manip.addClass report_button "partial" ;
    let pct = Format.asprintf "%2d%%" grade in
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ txt [%i"Report"] ;
                       span ~a: [ a_class [ "score" ] ] [ txt pct ]]
  end ;
  let report_container = find_component "learnocaml-exo-tab-report" in
  Manip.setInnerHtml report_container
    (Format.asprintf "%a" Report.(output_html ~bare: true) report) ;
  grade

module Exercise_link =
  struct
    let exercise_link ?(cl = []) id content =
      let open Tyxml_js.Html5 in
      a ~a:[ a_href ("/exercises/"^id^"/") ;
             a_class cl ;
        ]
        content
  end

module Display = Display_exercise(Exercise_link)
open Display

let is_readonly = ref false

let make_readonly () =
  is_readonly := true;
  alert ~title:[%i"TIME'S UP"]
    [%i"The deadline for this exercise has expired. Any changes you make \
        from now on will remain local only."]

(* experiment button of editor.html redirects to the html associated to this ml
   to know if we are in this page because of that we decide
   to put a '.' before the id
   therefore idEditor looks for a '.' before the id *)

let idEditor s =  not ((Regexp.string_match (Regexp.regexp "^[.]+") s 0) = None)

let () =
  run_async_with_log @@ fun () ->
  set_string_translations_exercises ();
  Learnocaml_local_storage.init ();
  Server_caller.request (Learnocaml_api.Version ()) >>=
    (function
     | Ok (_, server_id) -> Learnocaml_local_storage.(store server_id) server_id; Lwt.return_true
     | Error _ -> Lwt.return_false) >>= fun has_server ->
  let token = get_token ~has_server ()
  in
  (* ---- launch everything --------------------------------------------- *)
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  let toplevel_toolbar = find_component "learnocaml-exo-toplevel-toolbar" in
  let editor_toolbar = find_component "learnocaml-exo-editor-toolbar" in
  let toplevel_button =
    button ~container: toplevel_toolbar ~theme: "dark" ~group:toplevel_buttons_group ?state:None in
  let id = match Url.Current.path with
    | "" :: "exercises" :: p | "exercises" :: p ->
        String.concat "/" (List.map Url.urldecode (List.filter ((<>) "") p))
    | _ -> arg "id"
  in
  Dom_html.document##.title :=
    Js.string (id ^ " - " ^ "Learn OCaml" ^" v."^ Learnocaml_api.version);

  (* if we came from a true exercise we search in the server.
   In the other case we get the exercise information from the Local storage *)
  let exercise_fetch = match idEditor id with
    | false -> token >>= fun token ->
               retrieve (Learnocaml_api.Exercise (token, id))

    | true -> let proper_id = String.sub id 1 ((String.length id)-1) in
              Lwt.return ((Editor_lib.get_editor_state proper_id).Editor.metadata,
                          (Editor_lib.exo_creator proper_id ),
                          None)
  in

  let after_init top =
    exercise_fetch >>= fun (_meta, exo, _deadline) ->
    begin match Learnocaml_exercise.(decipher File.prelude exo) with
      | "" -> Lwt.return true
      | prelude ->
          Learnocaml_toplevel.load ~print_outcome:true top
            ~message: [%i"loading the prelude..."]
            prelude
    end >>= fun r1 ->
    Learnocaml_toplevel.load ~print_outcome:false top
      (Learnocaml_exercise.(decipher File.prepare exo)) >>= fun r2 ->
    if not r1 || not r2 then failwith [%i"error in prelude"] ;
    Learnocaml_toplevel.set_checking_environment top >>= fun () ->
    Lwt.return () in
  let toplevel_launch =
    toplevel_launch ~after_init (find_component "learnocaml-exo-toplevel-pane")
      Learnocaml_local_storage.exercise_toplevel_history
      (fun () -> select_tab "toplevel") toplevel_buttons_group id
  in
  init_tabs () ;
  set_nickname_div ();
  toplevel_launch >>= fun top ->
  exercise_fetch >>= fun (ex_meta, exo, deadline) ->
  (match deadline with
   | None -> ()
   | Some 0. -> make_readonly ()
   | Some t ->
       match Manip.by_id "learnocaml-countdown" with
       | Some elt -> countdown elt t ~ontimeout:make_readonly
       | None -> ());
  let solution =
    match Learnocaml_local_storage.(retrieve (exercise_state id)) with
    | { Answer.report = Some report ; solution ; _ } ->
        let _ : int = display_report exo report in
        solution
    | { Answer.report = None ; solution ; _ } ->
        solution
    | exception Not_found -> Learnocaml_exercise.(access File.template exo) in
  (* ---- details pane -------------------------------------------------- *)
  let load_meta () =
    Lwt.async (fun () ->
        token >>= fun token ->
        display_meta token ex_meta id)
  in
  if arg "tab" = "meta" then load_meta () else
    Manip.Ev.onclick (find_component "learnocaml-exo-button-meta") (fun _ ->
        load_meta ();
        select_tab "meta";
        true);
  (* ---- toplevel pane ------------------------------------------------- *)
  init_toplevel_pane toplevel_launch top toplevel_buttons_group toplevel_button ;
  (* ---- text pane ----------------------------------------------------- *)
  let text_container = find_component "learnocaml-exo-tab-text" in
  let text_iframe = Dom_html.createIframe Dom_html.document in
  Manip.replaceChildren text_container
    Tyxml_js.Html5.[ h1 [ txt ex_meta.Exercise.Meta.title ] ;
                     Tyxml_js.Of_dom.of_iFrame text_iframe ] ;
  (* ---- editor pane --------------------------------------------------- *)
  let editor, ace = setup_editor solution in
  let module EB = Editor_button (struct let ace = ace let buttons_container = editor_toolbar end) in
  EB.cleanup (Learnocaml_exercise.(access File.template exo));
  EB.sync token id;
  EB.download id;
  EB.eval top select_tab;
  let typecheck = typecheck top ace editor in
(*------------- prelude -----------------*)
  setup_prelude_pane ace Learnocaml_exercise.(decipher File.prelude exo);
  Js.Opt.case
    (text_iframe##.contentDocument)
    (fun () -> failwith "cannot edit iframe document")
    (fun d ->
       d##open_;
       d##write (Js.string (exercise_text ex_meta exo));
       d##close) ;
  (* ---- main toolbar -------------------------------------------------- *)
  let exo_toolbar = find_component "learnocaml-exo-toolbar" in
  let toolbar_button = button ~container: exo_toolbar ~theme: "light" in
  let () =
    if idEditor id then
      begin
        let id = String.sub id 1 ((String.length id)-1) in
        begin toolbar_button
       ~icon: "upload" [%i"Edit"] @@ fun ()->
       Dom_html.window##.location##assign
       (Js.string ("editor.html#id=" ^ id ^ "&action=open"));
       Lwt.return_unit
        end;
      end
  in
  begin toolbar_button
      ~icon: "list" [%i"Exercises"] @@ fun () ->
    Dom_html.window##.location##assign
      (Js.string (api_server ^ "/index.html#activity=exercises")) ;
    Lwt.return ()
  end ;
  let messages = Tyxml_js.Html5.ul [] in
  let callback text =
    Manip.appendChild messages Tyxml_js.Html5.(li [ txt text ]) in
  let worker =
    ref (get_grade ~callback exo)
  in
  
  begin toolbar_button
      ~icon: "typecheck" [%i"Compile"] @@ fun () ->
    typecheck true
  end;
  begin toolbar_button
          ~icon: "reload" [%i"Grade!"] @@ fun () ->
    check_if_need_refresh has_server >>= fun () ->
    let aborted, abort_message =
      let t, u = Lwt.task () in
      let btn = Tyxml_js.Html5.(button [ txt [%i"abort"] ]) in
      Manip.Ev.onclick btn (fun _ -> Lwt.wakeup u () ; true) ;
      let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ txt [%i"Grading is taking a lot of time, "] ;
                            btn ;
                            txt " ?" ]) in
      Manip.SetCss.opacity div (Some "0") ;
      t, div in
    Manip.replaceChildren messages
      Tyxml_js.Html5.[ li [ txt [%i"Launching the grader"] ] ] ;
    let submit_report = not !is_readonly in (* Don't count the grading time *)
    show_loading ~id:"learnocaml-exo-loading" [ messages ; abort_message ]
    @@ fun () ->
    Lwt_js.sleep 1. >>= fun () ->
    let solution = Ace.get_contents ace in
    Learnocaml_toplevel.check top solution >>= fun res ->
    match res with
    | Toploop_results.Ok ((), _) ->
        let grading =
          Lwt.finalize
            (fun () ->
               !worker >>= fun w ->
               w solution >>= fun (report, _, _, _) ->
               Lwt.return report)
            (fun () ->
               worker := get_grade ~callback exo;
               Lwt.return_unit)
        in
        let abortion =
          Lwt_js.sleep 5. >>= fun () ->
          Manip.SetCss.opacity abort_message (Some "1") ;
          aborted >>= fun () ->
          Lwt.return Learnocaml_report.[ Message ([ Text [%i"Grading aborted by user."] ], Failure) ] in
        Lwt.pick [ grading ; abortion ] >>= fun report ->
        let grade = display_report exo report in
        let editor, answer =
          if submit_report then
            None,
            Some { Answer.grade = Some grade ;
                   solution ;
                   report = Some report ;
                   mtime = max_float } (* To ensure server time will be used *)
          else
            Some solution, None
        in
        token >>= fun token ->
        sync_exercise token id ?answer ?editor >>= fun _save ->
        select_tab "report" ;
        Lwt_js.yield () >>= fun () ->
        Ace.focus ace ;
        Lwt.return ()
    | Toploop_results.Error _ ->
        let msg =
          Learnocaml_report.[ Text [%i"Error in your code."] ; Break ;
                   Text [%i"Cannot start the grader if your code does not typecheck."] ] in
        let report = Learnocaml_report.[ Message (msg, Failure) ] in
        let grade = display_report exo report in
        Learnocaml_local_storage.(store (exercise_state id))
          { Answer.grade = Some grade ; solution ; report = Some report ;
            mtime = gettimeofday () } ;
        select_tab "report" ;
        Lwt_js.yield () >>= fun () ->
        Ace.focus ace ;
        typecheck true
  end ;
  Window.onunload (fun _ev -> local_save ace id; true);
  (* ---- return -------------------------------------------------------- *)
  toplevel_launch >>= fun _ ->
  typecheck false >>= fun () ->
  hide_loading ~id:"learnocaml-exo-loading" () ;
  Lwt.return ()
