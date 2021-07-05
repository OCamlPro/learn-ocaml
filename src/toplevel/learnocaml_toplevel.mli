(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** An OCaml toplevel whose input and output will be in a given HTML [div]. *)

open Tyxml_js

(** An abstract type representing a toplevel instance. *)
type t

(** Create a toplevel instance in a given container [div].

   @param container
     The [div] that will contain the toplevel input and output.
   @param timeout_delay
     Time before the [timeout_prompt] function is launched.
   @param timeout_prompt
     A function called when an operation has taken more than
     [timeout_delay] to execute. The resulting thread triggers the
     cancellation of this operation when it terminate, and is conversely
     canceled if the operation terminates before.
     It is the default value for the optional parameter of the
     {!val:execute}, {!val:load} and {!val:reset} functions.
     You can use {!make_timeout_popup} to build such a callback.
   @param flood_limit
     Number of bytes after which the [after_flood] function is called.
     The default limit is to 8000 bytes, 0 means unlimited.
   @param flood
     A function called when a (single) execution is printing
     too much on an output channel. From this point, the output
     is buffered until the resulting thread terminates.
     If the function returns [false], the buffered output is released.
     Otherwise, it is dropped, along with any other output until
     the end of the current evaluation.
     The thread may be canceled if the toplevel is reset.
     You can use {!make_flood_popup} to build such a callback.
   @param after_init
     A function that is called whenever the toplevel is initialized or reset.
   @param on_resize
     A callback called when the input or output of the toplevel change,
     so that the page layout may be updated if needed.
   @param on_disable_input
    A callback called when the input is disabled (when an operation starts).
   @param on_enable_input
    A callback called when the input is enabled (when an operation ends).
   @param oldify
     When [~oldify:true] is used (by default), and when the toplevel is reset,
     the previous outputs are kept and marked as old
     (see {!Learnocaml_toplevel_output.oldify}). Otherwise, the output console is cleaned.
   @param input_sizing
     See (!Learnocaml_toplevel_input.sizing}.
   @param history
     The history storage to use. If none, a new volatile one is created.
   @param display_welcome
     Tells if the welcome message with some help and the version of OCaml
     is to be displayed or not. *)
val create:
  ?worker_js_file:string ->
  ?timeout_delay: float ->
  timeout_prompt:(t -> unit Lwt.t) ->
  ?flood_limit: int ->
  flood_prompt: (t -> string -> (unit -> int) -> bool Lwt.t) ->
  ?after_init:(t -> unit Lwt.t) ->
  ?input_sizing: Learnocaml_toplevel_input.sizing ->
  ?on_resize:(unit -> unit) ->
  ?on_disable_input:(t -> unit) ->
  ?on_enable_input:(t -> unit) ->
  ?history:Learnocaml_toplevel_history.history ->
  ?oldify:bool ->
  ?display_welcome: bool ->
  container:[`Div] Html5.elt ->
  unit -> t Lwt.t

(** Creates a thread that displays a popup over the toplevel container
    with a countdown and a button to augment it manually, and
    terminates after the countdown. *)
val make_timeout_popup:
  ?countdown: int ->
  ?refill_step: int ->
  ?on_show: (unit -> unit) ->
  unit ->
  t -> unit Lwt.t

(** Create a thread that displays a popup over the toplevel
    container displaying the flood amount in real time, and
    asking if the display should be hidden or not. *)
val make_flood_popup:
  ?on_show: (unit -> unit) ->
  unit ->
  (t -> string -> (unit -> int) -> bool Lwt.t)

(** Execute a given piece of code.

    @param timeout
      See {!create}.
    @returns
      Returns [Success true] whenever the code was correctly
      typechecked and its evaluation did not raise an exception nor
      timeouted and [false] otherwise. *)
val execute_phrase: t ->
  ?timeout:(t -> unit Lwt.t) ->
  string -> bool Lwt.t

(** Execute a given piece of code without displaying it.

    @param timeout
      See {!create}.
    @param print_outcome
      Tells if answers of the toplevel are to be displayed.
    @param message
      Displays [(* message *)] where the code should have been echoed.
    @returns
       Returns [Success true] whenever the code was correctly
       typechecked and its evaluation did not raise an exception nor
       timeouted and [false] otherwise. *)
val load:
  t ->
  ?print_outcome:bool ->
  ?timeout:(t -> unit Lwt.t) ->
  ?message: string ->
  string -> bool Lwt.t

(** Parse and typecheck a given source code.

    @param grading
      Load [Embedded_grading_cmis] and ppx-metaquot in the checker toploop. *)
val check: ?grading:bool -> t -> string -> unit Toploop_results.toplevel_result Lwt.t

(** Freezes the environment for future calls to {!check}. *)
val set_checking_environment: t -> unit Lwt.t

(** Empty the toplevel container content. *)
val clear: t -> unit

(** Reset the toplevel environment. *)
val reset: t -> unit Lwt.t

(** Print a message in the toplevel standard output. This is equivalent
    to calling [Pervasives.print_string] in the toplevel session.
    Calls {!Learnocaml_toplevel_output.output_stdout}. *)
val print_string: t -> string -> unit

(** Print a message in the toplevel standard error output. This is
    equivalent to calling [Pervasives.prerr_string] in the toplevel
    session. Calls {!Learnocaml_toplevel_output.output_stderr}. *)
val prerr_string: t -> string -> unit

(** Print a block of HTML in the toplevel output.
    Calls {!Learnocaml_toplevel_output.output_html}. *)
val print_html: t -> string -> unit

(** scroll the view to show the last phrase.
    Calls {!Learnocaml_toplevel_output.scroll. *)
val scroll: t -> unit

(** Execute the content of the input [textarea].
    This is equivalent to pressing [Enter] when the toplevel is focused. *)
val execute: t -> unit
                    
val execute_test: t -> string
  
(** Go backward in the input's history.
    This is equivalent to pressing [Up] when the toplevel is focused. *)
val go_backward: t -> unit

(** Go forward in the input's history.
    This is equivalent to pressing [Down] when the toplevel is focused. *)
val go_forward: t -> unit
