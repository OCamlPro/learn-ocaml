(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml

(** Editor *)

type 'a editor

type loc = {
  loc_start: int * int;
  loc_end: int * int;
}

val create_editor: Dom_html.divElement Js.t -> 'a editor

val set_mode: 'a editor -> string -> unit
val on: 'b editor -> string -> (Dom_html.event Js.t -> unit) -> unit
val insert: 'a editor -> Ace_types.position Js.t -> string -> unit


val read_range: Ace_types.range Js.t -> (int * int) * (int * int)
val create_range:
   Ace_types.position Js.t -> Ace_types.position Js.t -> Ace_types.range Js.t
val create_position: int -> int -> Ace_types.position Js.t
val read_position: Ace_types.position Js.t -> int * int
val greater_position:
  Ace_types.position Js.t -> Ace_types.position Js.t -> bool
val get_cursor_position: 'a editor -> Ace_types.position Js.t  

val get_contents: ?range:Ace_types.range Js.t -> 'a editor -> string
val get_line: 'a editor -> int -> string
val set_contents: ?reset_undo:bool -> 'a editor -> string -> unit

val get_selection_range: 'a editor -> Ace_types.range Js.t
val get_selection: 'a editor -> string

type mark_type = Error | Warning | Message

val set_mark:
  'a editor -> ?loc:loc -> ?type_:mark_type -> string -> unit
val clear_marks: 'a editor -> unit
val record_event_handler: 'a editor -> string -> (unit -> unit) -> unit
val set_background_color: 'a editor -> string -> unit
val add_class: 'a editor -> string -> unit
val remove_class: 'a editor -> string -> unit

val focus: 'a editor -> unit
val resize: 'a editor -> bool -> unit

val require: string -> unit

val show_keybindings: 'a editor -> unit
val add_keybinding:
  'a editor ->
  ?ro:bool ->
  ?scrollIntoView:string ->
  ?multiSelectAction:string ->
  string -> string -> ('a editor -> unit) -> unit

val set_font_size: 'a editor -> int -> unit
val set_tab_size: 'a editor -> int -> unit
val set_readonly: 'a editor -> bool -> unit
val get_state: 'a editor -> int -> < .. > Js.t

val get_last: 'a editor -> Ace_types.position Js.t

type doc

val document: 'a editor -> doc
val replace: doc -> Ace_types.range Js.t -> string -> unit
val delete: doc -> Ace_types.range Js.t -> unit
val remove: 'a editor -> string -> unit

val get_custom_data: 'a editor -> 'a
val set_custom_data: 'a editor -> 'a -> unit

(** Mode *)

type token
val token: type_:string -> string -> token

type 'state helpers = {
  initial_state: unit -> 'state;
  get_next_line_indent: 'state -> line:string -> tab:string -> string;
  get_line_tokens: string -> 'state -> int -> doc -> ('state * token list);
  check_outdent: ('state -> string -> string -> bool) option;
  auto_outdent: ('state -> Ace_types.document Js.t -> int -> unit) option;
}

val define_mode: string -> 'state helpers -> unit
