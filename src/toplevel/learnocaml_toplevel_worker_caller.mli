(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** An unified interface for OCaml toplevels running in a Web Worker
    or not. This module signature is very simalar to the signature of
    {!module:Learnocaml_toplevel_toploop}, except:
      {ul
      {- all blocking functions will kill the underlying Web Worker when
        cancelled; a new worker will be spawned. }
      {- it uses function of type [string -> unit] as outputs instead
        of [Format.formatter].}} *)

open Toploop_results

(** An abstract type representing a toplevel instance. *)
type t


(** Create a toplevel instance.

    @param after_init a function that will be called whenever the
           toplevel is initialized or reset.

    @param pp_stdout a function to be called when the toplevel standard
           output is flushed (default: flush to the browser's console).

    @param pp_stderr a function to be called when the toplevel standard
           error output is flushed (default: flush to the browser's
           console).

    @param js_file the web worker [.js] file.
           (default: ["/js/learnocaml-toplevel-worker.js"]). *)
val create:
  ?js_file: string ->
  ?after_init:(t -> unit Lwt.t) ->
  ?pp_stdout:(string -> unit) ->
  ?pp_stderr:(string -> unit) ->
  unit -> t Lwt.t


(** Parse and typecheck a given source code

    @return [Success ()] in case of success and [Error err]
            where [err] contains the error message otherwise.

*)
val check: t -> string -> unit toplevel_result Lwt.t


(** Execute a given source code. The evaluation stops after the first
    toplevel phrase (as terminated by ";;") that fails to compile or
    for which the evaluation raises an uncaught exception.

    @param pp_code a function that will received the parsed code
           before its execution. The calls to this function might be
           interleaved with call to "pp_answer" when a line finishes
           by ";;".

    @param pp_answer a function that will received compiler
           outputs.

    @param print_outcome should the toplevel print the computed
           values and their types ?

    @return [Error err] when parsing or typechecking failed, where
            [err] contains the error message. It returns [Success true]
            when the code evaluation finished without uncaught
            exception, and [Success false] otherwise.

*)
val execute:
  t ->
  ?pp_code:(string -> unit) ->
  pp_answer:(string -> unit) ->
  print_outcome:bool ->
  string -> bool toplevel_result Lwt.t

(** Freezes the environment for future calls to {!check}. *)
val set_checking_environment:
  t -> unit toplevel_result Lwt.t

(** Execute a given source code. The code is parsed and
    typechecked all at once before to start the evaluation.

    @param pp_answer see {!val:execute}.

    @param print_outcome see {!val:execute}.

    @return as {!val:execute}.

*)
val use_string:
  t ->
  ?filename: string ->
  pp_answer:(string -> unit) ->
  print_outcome:bool ->
  string -> bool toplevel_result Lwt.t


(** Wrap a given source code into a module and bind it with a given name.

    @param pp_answer see {!val:execute}.

    @param print_outcome see {!val:execute}.

    @param modname the module name, it must start with a capital
           character.

    @param sig_code source code for the module signature.

    @return as {!val:execute}.

*)
val use_mod_string:
  t ->
  pp_answer:(string -> unit) ->
  print_outcome:bool ->
  modname:string ->
  ?sig_code:string ->
  string -> bool toplevel_result Lwt.t

(** Insert a callback in the toplevel environment. *)
val register_callback : t -> string -> (string -> unit) -> unit toplevel_result Lwt.t

(** Reset the current toplevel environment to the initial
    environment. *)
val reset: t -> ?timeout:(unit -> unit Lwt.t) -> unit -> unit Lwt.t


(** Terminate the toplevel, i.e. destroy the Web Worker. It does
    nothing if the toplevel as been created with [async=false]. *)
val terminate: t -> unit

val set_after_init: t -> (t -> unit Lwt.t) -> unit

(**/**)

val debug: bool ref
val wrap: (string -> unit) -> Format.formatter
