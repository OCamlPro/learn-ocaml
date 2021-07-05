(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml

let debug = ref false

let (>>=) = Lwt.bind
let (>>?) o f =
  let open Toploop_results in
  o >>= function
  | Error (err, w) -> Lwt.return (Error (err, w))
  | Ok (x, w) ->
      f x >>= function
      | Error (err, w') -> Lwt.return (Error (err, w @ w'))
      | Ok (x, w') -> Lwt.return (Ok (x, w @ w'))

let return_success e = Lwt.return (Toploop_results.Ok (e, []))
let return_unit_success = return_success ()

let wrap pp =
  let buf = Buffer.create 503 in
  let flush () =
    let s = Buffer.contents buf in
    if s <> "" then begin
      Buffer.reset buf;
      pp s
    end in
  Format.make_formatter (Buffer.add_substring buf) flush

module IntMap = Map.Make(struct
    type t = int
    let compare (x:int) (y:int) = Pervasives.compare x y
  end)
let map_option f o = match o with | None -> None | Some o -> Some (f o)
let iter_option f o = match o with | None -> () | Some o -> f o


open Worker
open Learnocaml_toplevel_worker_messages

type u =
    U : 'a msg_ty * 'a Toploop_results.toplevel_result Lwt.u
        * 'a Toploop_results.toplevel_result Lwt.t -> u

type t = {
  js_file: string;
  mutable worker: (Js.js_string Js.t, Js.js_string Js.t) worker Js.t;
  mutable wakeners: u IntMap.t;
  mutable counter: int;
  mutable fds: (string -> unit) IntMap.t;
  mutable fd_counter: int;
  mutable reset_worker: t -> unit Lwt.t;
  mutable after_init: t -> unit Lwt.t;
  pp_stdout: string -> unit;
  pp_stderr: string -> unit;
}


exception Not_equal
let check_equal
  : type t1 t2.
    t1 msg_ty ->
    t2 msg_ty -> (t1, t2) eq = fun ty1 ty2 ->
  match ty1, ty2 with
  | Unit, Unit -> Eq
  | Bool, Bool -> Eq
  | Int, Int -> Eq
  | String, String -> Eq
  | Unit, _ -> raise Not_equal
  | Bool, _ -> raise Not_equal
  | Int, _ -> raise Not_equal
  | String, _ -> raise Not_equal

let onmessage worker (ev : _ Worker.messageEvent Js.t) =
  match Json.unsafe_input ev##.data with
  | Write (fd, s) ->  begin
      if !debug then Js_utils.debug "Host: Write %d %S" fd s;
      try
        IntMap.find fd worker.fds s ;
        Js._false
      with
      | Not_found ->
          Firebug.console##(warn
            (Js.string (Printf.sprintf "Missing channels (%d)" fd)));
          Js._false
    end
  | ReturnSuccess (id, ty_v, v, w) -> begin
      if !debug then Js_utils.debug "Host: ReturnOk %d" id;
      try
        let U (ty_u, u, _) = IntMap.find id worker.wakeners in
        let Eq = check_equal ty_u ty_v in
        worker.wakeners <- IntMap.remove id worker.wakeners;
        Lwt.wakeup u (Toploop_results.Ok (v, w));
        Js._false
      with
      | Not_found ->
          Firebug.console##(warn
            (Js.string (Printf.sprintf "Missing wakeners (%d)" id)));
          Js._false
      | Not_equal ->
          Firebug.console##(warn
            (Js.string (Printf.sprintf "Unexpected wakeners (%d)" id)));
          Js._false
    end
  | ReturnError (id, e, w) -> begin
      if !debug then Js_utils.debug "Host: Error %d" worker.counter;
      try
        let U (_, u, _) = IntMap.find id worker.wakeners in
        worker.wakeners <- IntMap.remove id worker.wakeners;
        Lwt.wakeup u (Toploop_results.Error (e, w));
        Js._false
      with Not_found ->
        Firebug.console##(warn
          (Js.string (Printf.sprintf "Missing wakeners (%d)" id)));
        Js._false
    end

let terminate worker =
  (worker.worker)##terminate ;
  IntMap.iter
    (fun id (U (_, _, t)) ->
       worker.wakeners <- IntMap.remove id worker.wakeners;
       Lwt.cancel t)
    worker.wakeners


let never_ending = (* and not cancellable. *)
  fst (Lwt.wait ())

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

(** Threads created with [post] will always be wake-uped by
    [onmessage] by calling [Lwt.wakeup]. They should never end with
    an exception, unless canceled. When canceled, the worker is
    killed and a new one is spawned. *)
let rec post : type a. t -> a host_msg -> a Toploop_results.toplevel_result Lwt.t =
  fun worker msg ->
    let msg_id = worker.counter in
    let msg_ty = ty_of_host_msg msg in
    if !debug then Js_utils.debug "Host: queuing %d" msg_id;
    let (t, u) = Lwt.task () in
    Lwt.on_cancel t
      (fun () -> Lwt.async (fun () -> worker.reset_worker worker));
    worker.wakeners <- IntMap.add msg_id (U (msg_ty, u, t)) worker.wakeners;
    worker.counter <- msg_id + 1;
    worker.worker##(postMessage (Json.output (msg_id, msg)));
    t

and do_reset_worker () =
  let running = ref true in
  fun worker ->
    if !running then begin
      if !debug then Js_utils.debug "Host: do_reset_worker";
      running := false;
      terminate worker;
      IntMap.iter
        (* GRGR: Peut-on 'cancel' directement le Lwt.u ? *)
        (fun _ (U (_, _, t)) -> Lwt.cancel t)
        worker.wakeners;
      worker.worker <- Worker.create (worker.js_file);
      worker.fds <-
        IntMap.empty |>
        IntMap.add 0 (IntMap.find 0 worker.fds) |>
        IntMap.add 1 (IntMap.find 1 worker.fds);
      worker.fd_counter <- 2;
      worker.wakeners <- IntMap.empty;
      worker.counter <- 0;
      worker.reset_worker <- do_reset_worker ();
      (Obj.magic worker.worker)##.onmessage :=
        Js.wrap_callback (onmessage worker);
      post worker @@ Init >>= fun _ ->
      worker.after_init worker >>= fun _ ->
      Lwt.return_unit
    end else
      Lwt.return_unit

let create
    ?(js_file = "/js/learnocaml-toplevel-worker.js")
    ?(after_init = fun _ -> Lwt.return_unit)
    ?(pp_stdout = (fun text -> Firebug.console##(log (Js.string text))))
    ?(pp_stderr = (fun text -> Firebug.console##(log (Js.string text))))
    () =
  let worker = Worker.create js_file in
  let fds =
    IntMap.empty |>
    IntMap.add 0 pp_stdout |>
    IntMap.add 1 pp_stderr in
  let worker =
    { worker; js_file;
      wakeners = IntMap.empty; counter = 0; fds; fd_counter = 2;
      reset_worker = do_reset_worker ();
      after_init; pp_stdout; pp_stderr;
    } in
  (Obj.magic worker.worker)##.onmessage := Js.wrap_callback (onmessage worker);
  post worker @@ Init >>= fun _ ->
  worker.after_init worker >>= fun () ->
  Lwt.return worker

let create_fd worker pp =
  worker.fds <- IntMap.add worker.fd_counter pp worker.fds;
  let fd = worker.fd_counter in
  worker.fd_counter <- fd + 1;
  fd

let close_fd worker fd =
  worker.fds <- IntMap.remove fd worker.fds

let reset worker ?(timeout = fun () -> never_ending) () =
  if !debug then Js_utils.debug "Host: reset";
  let timeout = timeout () in
  Lwt.choose [
    ( post worker Reset >>= fun res -> Lwt.return (`Reset res) );
    ( timeout >>= fun () -> Lwt.return `Timeout );
  ] >>= function
  | `Reset Toploop_results.Ok ((), _) ->
      Lwt.cancel timeout;
      worker.after_init worker
  | `Reset Toploop_results.Error (err, _) ->
      Lwt.cancel timeout;
      worker.pp_stderr err.Toploop_results.msg;
      worker.reset_worker worker
  | `Timeout ->
      (* Not canceling the Reset thread, but manually resetting. *)
      worker.reset_worker worker

let check ?(grading = false) worker code =
  post worker @@ Check (code, grading)

let set_checking_environment worker =
  post worker @@ Set_checking_environment

let execute worker ?pp_code ~pp_answer ~print_outcome code =
  let pp_code = map_option (create_fd worker) pp_code in
  let pp_answer = create_fd worker pp_answer in
  post worker @@
  Execute (pp_code, print_outcome, pp_answer, code) >>= fun result ->
  iter_option (close_fd worker) pp_code;
  close_fd worker pp_answer;
  Lwt.return result

let use_string worker ?filename ~pp_answer ~print_outcome code =
  let pp_answer = create_fd worker pp_answer in
  post worker @@
  Use_string (filename, print_outcome, pp_answer, code) >>= fun result ->
  close_fd worker pp_answer;
  Lwt.return result

let use_mod_string worker
    ~pp_answer ~print_outcome ~modname ?sig_code impl_code =
  let pp_answer = create_fd worker pp_answer in
  post worker @@
  Use_mod_string (pp_answer, print_outcome, modname, sig_code, impl_code)
  >>= fun result ->
  close_fd worker pp_answer;
  Lwt.return result

let set_after_init w after_init = w.after_init <- after_init

let register_callback worker name callback =
  let fd = create_fd worker callback in
  post worker (Register_callback (name, fd)) >>? fun () ->
  return_unit_success
