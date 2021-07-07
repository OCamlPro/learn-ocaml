(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml

type editor

type loc = Ace.loc = {
  loc_start: int * int;
  loc_end: int * int;
}

type error = {
  locs: loc list;
  msg: string;
}

type warning = {
  loc: loc;
  msg: string;
}

val create_ocaml_editor: Dom_html.divElement Js.t -> ((string -> unit) -> unit) -> editor
val get_editor: editor -> editor Ace.editor

val report_error: editor -> ?set_class: bool -> error option -> warning list -> unit Lwt.t
val report_current_error: editor -> ?set_class: bool -> unit -> unit Lwt.t

val get_current_error: editor -> error option
val get_current_warnings: editor -> warning list

val token_type : Approx_tokens.token -> string
