(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Toplevel output console. *)

(** A toplevel output console handle. *)
type output

(** Use a given div as an output console.
    Gives it the class [toplevel-output].

    @param on_resize
      A callback, called every time the contents of the console is updated.
    @param limit
      The maximum number of blocks displayed. *)
val setup :
  ?limit: int ->
  ?on_resize:(unit -> unit) ->
  container: [ `Div ] Tyxml_js.Html5.elt ->
  unit -> output

(** Empty the console. *)
val clear : output -> unit

(** Make the last element of the console visible on the screen.
    This works if the div itself is scrollable (CSS attribute [overflow-y]).
    If not, the [on_resize] parameter of {!setup} can be used to scroll
    a parent scrollable area.
    The console is automatically scrolled after each output.*)
val scroll : output -> unit

(** Make the current elements of the console greyed and unselectable. *)
val oldify : output -> unit

(** Represents a sequence of unseparated output blocks. *)
type phrase

(** Forges a new phrase identifier.

    Phrases are subsequences of outputs in the console, separated by
    [hr] elements.

    A new phrase is inserted at the end of the console whenever it is
    passed for the first time to one of the following output
    functions. When an output function is given the identifier of a
    phrase that already exists in the console, the output is appended
    to its existing contents, which may be in the middle of the
    console if new phrases have been added since.

    If no phrase is given to an output function, a new phrase is
    inserted, consisting only of this output. Successive outputs with
    no phrase specified are considered in the same phrase.

    The {!output_stdout} and {!output_stderr} have a different
    behaviour, in order to maintain the order of outputs. If a phrase
    is specified, it is considered only if it is the last one in the
    console. Otherwise, it is appended to the console, as if no phrase
    was specified. *)
val phrase : unit -> phrase

(** Output verbatim text to the console. Successive outputs are
    grouped, mixing {!output_stdout} and {!output_stderr}.  The output
    block is a direct child of the console, a [pre] element with class
    [toplevel-output]. The text is wrapped in a [span] element with
    class [stdout]. *)
val output_stdout : ?phrase: phrase -> output -> string -> unit

(** Output verbatim error text. See {!output_stdout}. The text is
    wrapped in a [span] element with class [stdout]. *)
val output_stderr : ?phrase: phrase -> output -> string -> unit

(** Output HTML in a [div] element with class [toplevel-html-block]. *)
val output_html : ?phrase: phrase -> output -> string -> unit

(** Output ocaml code in a [pre] element with class [toplevel-code].
    Code tokens are wrapped in [span] elements with classes as
    documented in {!Ocaml_mode.token_type}. An intermediate level of
    [span] elements with classes [toplevel-hilighted-error] and
    [toplevel-hilighted-warning] are used for errors and warnings. A
    [span] with class [ref] is used for location labels. *)
val output_code : ?phrase: phrase -> output -> string -> unit

(** Output an ocaml toplevel answer in a [pre] element with class
    [toplevel-answer]. *)
val output_answer : ?phrase: phrase -> output -> string -> unit

(** Output an error in a [pre] element with class [toplevel-error].
    A [span] with class [ref] is used for location labels. *)
val output_error : ?phrase: phrase -> output -> Toploop_results.error -> unit

(** Output a warning in a [pre] element with class [toplevel-warning].
    A [span] with class [ref] is used for location labels. *)
val output_warning : ?phrase: phrase -> output -> Toploop_results.warning -> unit

(** Format OCaml code in the style of {!output_code}. *)
val format_ocaml_code : string -> [> `Span | `PCDATA ] Tyxml_js.Html5.elt list

val get_blocks : output -> string
