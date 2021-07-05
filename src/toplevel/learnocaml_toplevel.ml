(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_utils
open Tyxml_js

let (>>=) = Lwt.(>>=)
(* let (>|=) = Lwt.(>|=)
 * 
 * let map_option f = function
 *   | None -> None
 *   | Some o -> Some (f o)
 * let iter_option f o = match o with | None -> () | Some o -> f o
 * 
 * type 'a result = Success of 'a | Timeout of float *)

type t = {
  timeout_delay: float;
  mutable timeout_prompt: t -> unit Lwt.t;
  mutable current_timeout_prompt: unit Lwt.t;
  flood_limit: int;
  mutable flood_prompt: t -> Html_types.nmtoken -> (unit -> int) -> bool Lwt.t;
  mutable current_flood_prompt: unit Lwt.t;
  flood_reset: t -> unit;
  worker: Learnocaml_toplevel_worker_caller.t;
  container: [ `Div ] Html5.elt;
  oldify: bool;
  mutable status: [ `Reset of (unit Lwt.t * unit Lwt.u) | `Execute of unit Lwt.t | `Idle ] ;
  mutable on_enable_input: t -> unit;
  mutable on_disable_input: t -> unit;
  mutable disabled : int;
  output: Learnocaml_toplevel_output.output;
  input: Learnocaml_toplevel_input.input;
}

(* let set_timeout_prompt t f = t.timeout_prompt <- f
 * let set_flood_prompt t f = t.flood_prompt <- f
 * let set_on_enable_input t f = t.on_enable_input <- f
 * let set_on_disable_input t f = t.on_disable_input <- f *)

(* let trim s =
 *   let ws c = c = ' ' || c = '\t' || c = '\n' in
 *   let len = String.length s in
 *   let start = ref 0 in
 *   let stop = ref (len - 1) in
 *   while !start < len && (ws s.[!start])
 *   do incr start done;
 *   while !stop > !start && (ws s.[!stop])
 *   do decr stop done;
 *   String.sub s !start (!stop - !start + 1) *)

let disable_input top =
  top.disabled <- top.disabled + 1 ;
  if top.disabled = 1 then begin
    top.on_disable_input top ;
    Learnocaml_toplevel_input.disable top.input
  end

let enable_input top =
  top.disabled <- top.disabled - 1 ;
  if top.disabled = 0 then begin
    top.on_enable_input top ;
    Learnocaml_toplevel_input.enable top.input
  end

let scroll { output; _ } =
  Learnocaml_toplevel_output.scroll output

let clear { output; _ } =
  Learnocaml_toplevel_output.clear output ;
  Learnocaml_toplevel_output.output_stdout output
    [%i"The toplevel has been cleared.\n"]

(* let never_ending =
 *   let t = fst (Lwt.wait ()) in
 *   fun _ -> t *)

let wait_for_prompts top =
  Lwt.join
    [ Lwt.catch
        (fun () -> top.current_timeout_prompt)
        Lwt.(function Canceled -> return () | exn -> fail exn) ;
      Lwt.catch
        (fun () -> top.current_flood_prompt)
        Lwt.(function Canceled -> return () | exn -> fail exn) ]

let start_timeout top _name timeout =
  Lwt.cancel top.current_timeout_prompt ;
  match timeout with
  | Some timeout -> timeout top
  | None ->
      Lwt_js.sleep top.timeout_delay >>= fun () ->
      wait_for_prompts top >>= fun () ->
      top.current_timeout_prompt <- top.timeout_prompt top ;
      top.current_timeout_prompt

let input_focus top f =
  f () >>= fun r -> Learnocaml_toplevel_input.focus top.input; Lwt.return r

let reset_with_timeout top ?timeout () =
  input_focus top @@ fun () ->
  match top.status with
  | `Reset (t, _) -> t
  | `Idle ->
      let t, u = Lwt.wait () in
      Lwt.cancel top.current_timeout_prompt ;
      Lwt.cancel top.current_flood_prompt ;
      wait_for_prompts top >>= fun () ->
      top.status <- `Reset (t, u) ;
      let timeout () = start_timeout top "reset" timeout in
      disable_input top;
      Learnocaml_toplevel_worker_caller.reset ~timeout top.worker () >>= fun () ->
      t
  | `Execute task ->
      let t, u = Lwt.wait () in
      Lwt.cancel top.current_timeout_prompt ;
      Lwt.cancel top.current_flood_prompt ;
      wait_for_prompts top >>= fun () ->
      top.status <- `Reset (t, u) ;
      let timeout () = start_timeout top "reset" timeout in
      disable_input top;
      Learnocaml_toplevel_worker_caller.reset ~timeout top.worker () >>= fun () ->
      t >>= fun () ->
      Lwt.cancel task ;
      Lwt.return ()

let reset top =
  let timeout _ = Lwt_js.sleep 2. in
  reset_with_timeout top ~timeout ()

let protect_execution top exec =
  input_focus top @@ fun () ->
  wait_for_prompts top >>= fun () ->
  match top.status with
  | `Reset _ | `Execute _ ->
      Lwt.fail_invalid_arg "Learnocaml_toplevel.protect_execution"
  | `Idle ->
      let t, u = Lwt.task () in
      top.status <- `Execute t ;
      disable_input top;
      top.flood_reset top ;
      let thread = t >>= fun () ->
        Lwt.catch
          (fun () ->
             exec () >>= fun res ->
             match top.status with
             | `Execute t' ->
                 assert (t == t') ;
                 top.status <- `Idle;
                 wait_for_prompts top >>= fun () ->
                 enable_input top;
                 Lwt.return res
             | `Idle | `Reset _ ->
                 (* The task successfully ended between a reset order
                    and its ack, we fake its cancellation. *)
                 Lwt.fail Lwt.Canceled)
          (function
            | Lwt.Canceled ->
                enable_input top;
                begin match top.status with
                  | `Reset (t, _) -> t
                  | _ -> Lwt.return ()
                end >>= fun () -> Lwt.fail Lwt.Canceled
            | exn ->
                enable_input top;
                Lwt.fail exn) in
      Lwt.wakeup u () ;
      thread

let execute_phrase top ?timeout content =
  input_focus top @@ fun () ->
  let phrase = Learnocaml_toplevel_output.phrase () in
  let pp_code = Learnocaml_toplevel_output.output_code ~phrase top.output in
  let pp_answer = Learnocaml_toplevel_output.output_answer ~phrase top.output in
  let t =
    Learnocaml_toplevel_worker_caller.execute
      top.worker ~pp_code ~pp_answer ~print_outcome:true content in
  Lwt.pick [
    (Lwt.protected t >>= fun _ -> Lwt.return_unit) ;
    (start_timeout top "execute" timeout >>= fun () ->
     let timeout _ = Lwt.return () in
     reset_with_timeout top ~timeout ());
  ] >>= fun () ->
  t >>= fun result ->
  let warnings, result = match result with
    | Toploop_results.Ok (result, warnings) -> warnings, result
    | Toploop_results.Error (error, warnings) ->
        Learnocaml_toplevel_output.output_error ~phrase top.output error ;
        warnings, false in
  List.iter
    (Learnocaml_toplevel_output.output_warning ~phrase top.output)
    warnings ;
  Lwt.return result

let execute top =
  Learnocaml_toplevel_input.execute top.input

let execute_test top =
  Learnocaml_toplevel_output.get_blocks top.output

let go_backward top =
  Learnocaml_toplevel_input.go_backward top.input

let go_forward top =
  Learnocaml_toplevel_input.go_forward top.input

let check ?grading top code =
  protect_execution top @@ fun () ->
  Learnocaml_toplevel_worker_caller.check ?grading top.worker code

let set_checking_environment top =
  protect_execution top @@ fun () ->
  Learnocaml_toplevel_worker_caller.set_checking_environment top.worker >>= fun _ ->
  Lwt.return ()

let execute_phrase top ?timeout content =
  protect_execution top @@ fun () ->
  execute_phrase top ?timeout content

let load top ?(print_outcome = true) ?timeout ?message content =
  let phrase = Learnocaml_toplevel_output.phrase () in
  protect_execution top @@ fun () ->
  begin match message with
    | None -> ()
    | Some message ->
        Learnocaml_toplevel_output.output_code ~phrase top.output
          ("(* " ^ message ^ "*)")
  end ;
  let pp_answer =
    if print_outcome then
      Learnocaml_toplevel_output.output_answer ~phrase top.output
    else
      ignore in
  let t =
    Learnocaml_toplevel_worker_caller.use_string
      top.worker ~pp_answer ~print_outcome content in
  Lwt.pick [
    (Lwt.protected t >>= fun _ -> Lwt.return_unit) ;
    (start_timeout top "load" timeout >>= fun () ->
     reset top);
  ] >>= fun () ->
  t >>= fun result ->
  let warnings, result = match result with
    | Toploop_results.Ok (result, warnings) -> warnings, result
    | Toploop_results.Error (error, warnings) ->
        Learnocaml_toplevel_output.output_error top.output error ;
        warnings, false in
  List.iter
    (Learnocaml_toplevel_output.output_warning top.output)
    warnings ;
  Lwt.return result

let make_timeout_popup
    ?(countdown = 10)
    ?(refill_step = 10)
    ?(on_show = (fun () -> ()))
    () { container; _ } =
  let open Tyxml_js.Html5 in
  let t0 = Sys.time () in
  let countdown = ref countdown in
  let btn_continue =
    let label = Format.asprintf [%if"%d seconds!"] refill_step in
    button [ txt label ] in
  let btn_stop =
    button [ txt [%i"Kill it!"] ] in
  Manip.Ev.onclick btn_continue
    (fun _ -> countdown := !countdown + refill_step ; true) ;
  Manip.Ev.onclick btn_stop
    (fun _ -> countdown := 0 ; true) ;
  let clock_span = span [] in
  let countdown_span = span [] in
  let dialog =
    div ~a: [ a_class [ "dialog-container" ] ]
      [ div ~a: [ a_class [ "dialog" ] ]
          [ h1 [ txt [%i"Infinite loop?"] ] ;
            div ~a: [ a_class [ "message" ] ]
              [ txt [%i"The toplevel has not been responding for "] ;
                clock_span ;
                txt [%i" seconds."] ;
                br () ;
                txt [%i"It will be killed in "] ;
                countdown_span ;
                txt [%i" seconds."] ] ;
            div ~a: [ a_class [ "buttons" ] ]
              [ btn_continue ; btn_stop ] ] ] in
  Lwt.catch
    (fun () ->
       Manip.appendChild container dialog ;
       on_show () ;
       let rec loop () =
         let elapsed = int_of_float (Sys.time () -. t0) in
         Manip.replaceChildren clock_span
           [ txt (string_of_int elapsed) ] ;
         Manip.replaceChildren countdown_span
           [ txt (string_of_int (!countdown - elapsed)) ] ;
         if elapsed >= !countdown then begin
           Manip.removeChild container dialog ;
           Lwt.return ()
         end else
           Lwt_js.sleep 0.2 >>= loop in
       loop ())
    (fun exn ->
       (try Manip.removeChild container dialog with _ -> ()) ;
       Lwt.fail exn)

let make_flood_popup
    ?(on_show = (fun () -> ()))
    () { container; _ } name amount =
  let open Tyxml_js.Html5 in
  let answer = ref None in
  let btn_continue =
    button [ txt [%i"Show anyway!"] ] in
  let btn_stop =
    button [ txt [%i"Hide output!"] ] in
  Manip.Ev.onclick btn_continue
    (fun _ -> answer := Some false ; true) ;
  Manip.Ev.onclick btn_stop
    (fun _ -> answer := Some true ; true) ;
  let qty_span = span [] in
  let dialog =
    div ~a: [ a_class [ "dialog-container" ] ]
      [ div ~a: [ a_class [ "dialog" ] ]
          [ h1 [ txt [%i"Flooded output!"] ] ;
            div ~a: [ a_class [ "message" ] ]
              [ txt (Printf.sprintf
                          [%if"Your code is flooding the %s channel."] name) ;
                br ();
                txt [%i"It has already printed "] ;
                qty_span ;
                txt [%i" bytes."] ] ;
            div ~a: [ a_class [ "buttons" ] ]
              [ btn_continue ; btn_stop ] ] ] in
  Manip.appendChild container dialog ;
  on_show () ;
  let rec loop () =
    Manip.replaceChildren qty_span
      [ txt (string_of_int (amount ())) ] ;
    match !answer with
    | Some ans ->
        Manip.removeChild container dialog ;
        Lwt.return ans
    | None ->
        Lwt_js.sleep 0.2 >>= loop in
  Lwt.catch
    loop
    (fun exn ->
       Manip.removeChild container dialog ;
       Lwt.fail exn)

let wrap_flusher_to_prevent_flood top name hook real =
  let flooded = ref 0 in
  hook := fun s ->
    real s ;
    let total = !flooded + String.length s in
    if total >= top.flood_limit then begin
      let buf = Buffer.create top.flood_limit in
      hook := (fun s -> try flooded := !flooded + String.length s ; Buffer.add_string buf s with _ -> ()) ;
      flooded := total ;
      Lwt.async @@ fun () ->
      Lwt.catch
        (fun () ->
           wait_for_prompts top >>= fun () ->
           top.current_flood_prompt <-
             (top.flood_prompt top name (fun () -> !flooded) >>= function
               | true ->
                   real (Printf.sprintf [%if"\nInterrupted output channel %s.\n"] name) ;
                   hook := ignore ;
                   Lwt.return ()
               | false ->
                   real (Buffer.contents buf) ;
                   hook := real ;
                   Lwt.return ()) ;
           top.current_flood_prompt)
        (fun _exn ->
           hook := ignore ;
           Lwt.return ())
    end else begin
      flooded := total
    end

let welcome_phrase () =
  [%i"Printf.printf \"Welcome to OCaml %s\\n%!\" (Sys.ocaml_version);\n\
      print_endline \" - type your OCaml phrase in the box below and press [Enter]\";\n\
      print_endline \" - use [Shift-Enter] to break lines without triggering execution\";\n\
      print_endline \" - use [Ctrl-\\xe2\\x86\\x91] once to reuse the previous entry\";\n\
      print_endline \" - use [Ctrl-\\xe2\\x86\\x91] / [Ctrl-\\xe2\\x86\\x93] \
      to navigate through history\" ;;"]
  (* U+2191 upwards arrow, U+2193 downwards arrow*)

let create
    ?worker_js_file
    ?(timeout_delay = 5.)
    ~timeout_prompt
    ?(flood_limit = 8000)
    ~flood_prompt
    ?after_init
    ?(input_sizing =
      { Learnocaml_toplevel_input.line_height = 18 ;
        min_lines = 1 ; max_lines = 6 })
    ?on_resize
    ?(on_disable_input = fun _ -> ())
    ?(on_enable_input = fun _ -> ())
    ?history
    ?(oldify = true)
    ?(display_welcome = true)
    ~container () =
  (match get_lang() with Some l -> Ocplib_i18n.set_lang l | None -> ());
  let output_div = Html5.div [] in
  let input_div = Html5.div [] in
  Manip.appendChild container output_div;
  Manip.appendChild container input_div;
  let output =
    Learnocaml_toplevel_output.setup
      ?on_resize
      ~container:output_div
      () in
  let execute_hook = ref (fun _code -> assert false) in
  let input =
    Learnocaml_toplevel_input.setup
      ~sizing: input_sizing
      ~execute: (fun code -> !execute_hook code)
      ~container:input_div
      ?on_resize
      ?history
      () in
  let pp_stdout_hook = ref ignore in
  let pp_stdout s = !pp_stdout_hook s in
  let pp_stderr_hook = ref ignore in
  let pp_stderr s = !pp_stderr_hook s in
  let flood_reset top =
    let phrase = Learnocaml_toplevel_output.phrase () in
    Lwt.cancel top.current_flood_prompt ;
    wrap_flusher_to_prevent_flood top
      "stdout" pp_stdout_hook
      (Learnocaml_toplevel_output.output_stdout ~phrase output) ;
    wrap_flusher_to_prevent_flood top
      "stderr" pp_stderr_hook
      (Learnocaml_toplevel_output.output_stderr ~phrase output) in
  Learnocaml_toplevel_worker_caller.create
    ?js_file:worker_js_file
    ~pp_stdout ~pp_stderr () >>= fun worker ->
  let top = {
    timeout_prompt;
    current_timeout_prompt = Lwt.return ();
    timeout_delay;
    flood_prompt;
    current_flood_prompt = Lwt.return ();
    flood_limit;
    flood_reset;
    worker;
    container;
    oldify;
    status = `Reset (Lwt.wait ());
    on_enable_input;
    on_disable_input;
    disabled = 1;
    input;
    output;
  } in
  flood_reset top ;
  execute_hook :=
    (fun code -> Lwt.async @@ fun () ->
      Lwt.catch
        (fun () -> execute_phrase top code)
        (function
          | Lwt.Canceled -> Lwt.return true
          | exn -> Lwt.fail exn )) ;
  let first_time = ref true in
  let after_init top =
    if !first_time || not oldify then
      Learnocaml_toplevel_output.clear output
    else
      Learnocaml_toplevel_output.oldify output;
    enable_input top ;
    top.flood_reset top;
    begin match top.status with
      | `Reset (_, u) -> Lwt.wakeup u ()
      | `Idle | `Execute _ -> assert false
    end ;
    top.status <- `Idle;
    begin if display_welcome && (!first_time || not oldify) then
        Learnocaml_toplevel_worker_caller.execute
          ~pp_answer: (fun _ -> ())
          ~print_outcome: false
          worker (welcome_phrase ()) >>= fun _ ->
        Lwt.return ()
      else Lwt.return ()
    end >>= fun _ ->
    if not !first_time then
      let phrase = Learnocaml_toplevel_output.phrase () in
      Learnocaml_toplevel_output.output_stdout output ~phrase
        [%i"The toplevel has been reset.\n"]
    else
      first_time := false ;
    Learnocaml_toplevel_worker_caller.register_callback worker "print_html"
      (Learnocaml_toplevel_output.output_html output) >>= fun _ ->
    match after_init with
    | None -> Lwt.return_unit
    | Some f -> f top in
  after_init top >>= fun () ->
  Learnocaml_toplevel_worker_caller.set_after_init top.worker (fun _ -> after_init top);
  Lwt.return top

let print_string { output; _ } = Learnocaml_toplevel_output.output_stdout output

let prerr_string { output; _ } = Learnocaml_toplevel_output.output_stderr output

let print_html { output; _ } = Learnocaml_toplevel_output.output_html output
