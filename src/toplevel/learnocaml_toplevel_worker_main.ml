(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Learnocaml_toplevel_worker_messages

let debug = ref false
let grading = ref false

let (>>=) = Lwt.bind

let is_success = function
  | Toploop_ext.Ok _ -> true
  | Toploop_ext.Error _ -> false

type 'a return =
  | ReturnSuccess of 'a * Toploop_ext.warning list
  | ReturnError of Toploop_ext.error * Toploop_ext.warning list

let return_success v w = Lwt.return (ReturnSuccess (v, w))
let return_unit_success = return_success () []
let return_error e w = Lwt.return (ReturnError (e, w))

let unwrap_result =
  let open Toploop_ext in
  function
  | Ok (b, w) -> return_success b w
  | Error (err, w) -> return_error err w

(** File descriptors *)

module IntMap = Map.Make(struct
    type t = int
    let compare (x:int) (y:int) = Pervasives.compare x y
  end)

(* Limit the frequency of sent messages to one per ms, using an active
   loop (yuck) because, well, there is no other concurrency primitive
   and we do not want to fill a memory buffer but really "pause" the
   program.

   The problem arises with debug off and developer tools off only.
   In this case, with a program that does a lot of writes (print or
   callbacks), the messages queue fills up super quickly and kills the
   browser / tab.

   A possible improvement would be to bufferize the messages channel
   per channel, and emit the buffer of each channel every ms if it has
   changed. But it could cause bad asynchronicity in case the worker
   does a big computation just after a bufferized write. And it would
   still need some kind of active waiting to limit throughput. All in
   all this spinwait is not that ugly. *)
let last = ref 0.
let rec wait () =
  let now = Sys.time () (* let's hope this yields a bit *) in
  if now -. !last > 0.001 then
    last := now
  else wait ()

let post_message (m: toploop_msg) =
  wait () ;
  Worker.post_message (Json.output m)

let (wrap_fd, close_fd, clear_fds) =
  let fds = ref IntMap.empty in
  let wrap_fd fd =
    try IntMap.find fd !fds
    with Not_found ->
      let buf = Buffer.create 503 in
      let flush () =
        let s = Buffer.contents buf in
        if s <> "" then begin
          Buffer.reset buf;
          if !debug then Js_utils.debug "Worker: <- Write %d %S" fd s;
          post_message (Write (fd, s))
        end in
      let ppf = Format.make_formatter (Buffer.add_substring buf) flush in
      fds := IntMap.add fd ppf !fds;
      ppf in
  let close_fd fd =
    if IntMap.mem fd !fds then (Format.pp_print_flush (IntMap.find fd !fds) ());
    fds := IntMap.remove fd !fds in
  let clear_fds () =
    fds :=
      IntMap.fold
        (fun id ppf fds ->
           Format.pp_print_flush ppf ();
           if id = 0 || id = 1 then
             IntMap.add id ppf fds
           else
             fds)
        !fds
        IntMap.empty in
  (wrap_fd, close_fd, clear_fds)

let stdout_ppf = wrap_fd 0
let stderr_ppf = wrap_fd 1

let () =
  Sys_js.set_channel_flusher stdout
    (fun s ->
       Format.pp_print_string stdout_ppf s;
       Format.pp_print_flush stdout_ppf ());
  Sys_js.set_channel_flusher stderr
    (fun s ->
       Format.pp_print_string stderr_ppf s;
       Format.pp_print_flush stderr_ppf ())

let make_answer_ppf fd_answer =
  let orig_print_string, orig_flush =
    Format.pp_get_formatter_output_functions (wrap_fd fd_answer) () in
  let check_first_call =
    let first_call = ref true in
    fun () ->
      if !first_call then begin
        flush stdout ;
        flush stderr ;
        Format.(pp_print_flush std_formatter ()) ;
        Format.(pp_print_flush err_formatter ()) ;
        first_call := false ;
      end in
  Format.make_formatter
    (fun str -> check_first_call () ; orig_print_string str)
    (fun () -> check_first_call () ; orig_flush ())

(** Code compilation and execution *)

(* TODO protect execution with a mutex! *)

(** Message dispatcher *)

let map_option f o = match o with | None -> None | Some o -> Some (f o)
let iter_option f o = match o with | None -> () | Some o -> f o

let checking_environment = ref !Toploop.toplevel_env

let setup_ppx = lazy (Ast_mapper.register "ppx_metaquot" Ppx_metaquot.expander)

let handler : type a. a host_msg -> a return Lwt.t = function
  | Set_checking_environment ->
      checking_environment := !Toploop.toplevel_env ;
      return_unit_success
  | Init ->
      return_unit_success
  | Reset ->
      if !debug then Js_utils.debug "Worker: -> Reset";
      clear_fds ();
      Toploop.initialize_toplevel_env ();
      if !debug then Js_utils.debug "Worker: <- Reset";
      return_unit_success
  | Execute (fd_code, print_outcome, fd_answer, code) ->
      let ppf_code = map_option wrap_fd fd_code in
      let ppf_answer = make_answer_ppf fd_answer in
      if !debug then Js_utils.debug "Worker: -> Execute (%S)" code;
      let result = Toploop_ext.execute ?ppf_code ~print_outcome ~ppf_answer code in
      if !debug then Js_utils.debug "Worker: <- Execute (%B)" (is_success result);
      iter_option close_fd fd_code;
      close_fd fd_answer;
      unwrap_result result
  | Use_string (filename, print_outcome, fd_answer, code) ->
      let ppf_answer = make_answer_ppf fd_answer in
      if !debug then
        Js_utils.debug "Worker: -> Use_string (%S)" code;
      let result = Toploop_ext.use_string ?filename ~print_outcome ~ppf_answer code in
      if !debug then
        Js_utils.debug "Worker: <- Use_string (%B)" (is_success result);
      close_fd fd_answer;
      unwrap_result result
  | Use_mod_string (fd_answer, print_outcome, modname, sig_code, impl_code) ->
      let ppf_answer = make_answer_ppf fd_answer in
      if !debug then
        Js_utils.debug
          "Worker: -> Use_mod_string %s (%S)" modname impl_code;
      let result = Toploop_ext.use_mod_string
          ~ppf_answer ~print_outcome ~modname ?sig_code impl_code in
      if !debug then
        Js_utils.debug
          "Worker: <- Use_mod_string %s (%B)" modname (is_success result);
      close_fd fd_answer;
      unwrap_result result
  | Set_debug b ->
      debug := b;
      return_unit_success
  | Register_callback (name, fd) ->
      let callback text =
        post_message (Write (fd, text)) ; () in
      let ty =
        let ast =
          let arg =
            Ast_helper.(Typ.constr (Location.mknoloc (Longident.Lident "string")) []) in
          let ret =
            Ast_helper.(Typ.constr (Location.mknoloc (Longident.Lident "unit")) []) in
          { Parsetree.ptyp_desc = Parsetree.Ptyp_arrow (Asttypes.Nolabel, arg, ret) ;
            ptyp_loc = Location.none ;
            ptyp_attributes = [] } in
        Typetexp.transl_type_scheme !Toploop.toplevel_env ast in
      Toploop.toplevel_env :=
        Env.add_value
          (Ident.create name)
          { Types.
            val_type = ty.Typedtree.ctyp_type;
            val_kind = Types.Val_reg;
            val_attributes = [];
            val_loc = Location.none }
          !Toploop.toplevel_env ;
      Toploop.setvalue name (Obj.repr callback) ;
      return_unit_success
  | Check (code, grading_cmi_and_ppx_meta) ->
     let saved = !Toploop.toplevel_env in
     Toploop.toplevel_env := !checking_environment ;
     if grading_cmi_and_ppx_meta then (grading := true; Lazy.force setup_ppx) ;
     let result = Toploop_ext.check code in
     Toploop.toplevel_env := saved ;
     unwrap_result result
     
let ty_of_host_msg : type t. t host_msg -> t msg_ty = function
  | Init -> Unit
  | Reset -> Unit
  | Execute _ -> Bool
  | Use_string _ -> Bool
  | Use_mod_string _ -> Bool
  | Set_debug _ -> Unit
  | Check _ -> Unit
  | Set_checking_environment -> Unit
  | Register_callback _ -> Unit

let () =
  let handler (type t) data =
    let (id, data) : (int * t host_msg) = Json.unsafe_input data  in
    let ty = ty_of_host_msg data in
    handler data >>= function
    | ReturnSuccess (v, w) ->
        post_message (ReturnSuccess (id, ty, v, w));
        Lwt.return_unit
    | ReturnError (res, w) ->
        if !debug then
          Js_utils.debug "Worker: <- ReturnError %d" id;
        post_message (ReturnError (id, res, w));
        Lwt.return_unit
  in
  let path = "/worker_cmis" in
  (* we don't use
     [OCamlRes.Res.merge Embedded_cmis.root Embedded_grading_cmis.root]
     because we don't want to unconditionally load [Embedded_grading_cmis] *)
  let root1 = Embedded_cmis.root in
  let root2 = Embedded_grading_cmis.root in
  Sys_js.mount ~path
    (fun ~prefix:_ ~path ->
       match OCamlRes.Res.find (OCamlRes.Path.of_string path) root1 with
       | cmi ->
           Js.Unsafe.set cmi (Js.string "t") 9 ; (* XXX hack *)
           Some cmi
       | exception Not_found ->
          if !grading then
            match OCamlRes.Res.find (OCamlRes.Path.of_string path) root2 with
            | cmi ->
               Js.Unsafe.set cmi (Js.string "t") 9 ; (* XXX hack *)
               Some cmi
            | exception Not_found -> None
          else None) ;
  Config.load_path := [ path ] ;
  Toploop_jsoo.initialize ();
  Hashtbl.add Toploop.directive_table
    "debug_worker"
    (Toploop.Directive_bool (fun b -> debug := b));
  Worker.set_onmessage (fun s -> Lwt.async (fun () -> handler s))
