(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2015 OCamlPro: Grégoire Henry, Çağdaş Bozman.
 * Copyright (C) 2012 Vincent Balat, Benedikt Becker (for the 'Manip' module)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open Js_of_ocaml

val alert: string -> unit
val confirm: string -> bool

val log: ('a, Format.formatter, unit, unit) format4 -> 'a
val debug: ('a, Format.formatter, unit, unit) format4 -> 'a
val warn: ('a, Format.formatter, unit, unit) format4 -> 'a
val error: ('a, Format.formatter, unit, unit) format4 -> 'a

val js_log: 'a -> unit
val js_debug: 'a -> unit
val js_warn: 'a -> unit
val js_error: 'a -> unit

val reload: unit -> unit

(** Gets the language configured in the browser *)
val get_lang: unit -> string option

module Manip : sig

  (* à la Eliom_content.Manip ... *)

  open Tyxml_js.Html5

  val window: 'a elt -> Dom_html.window Js.t

  val setInnerHtml: 'a elt -> string -> unit
  val setInnerText: 'a elt -> string -> unit
  val clone: ?deep:bool -> 'a elt -> 'a elt

  val appendChild: ?before:'a elt -> 'b elt ->  'c elt -> unit
  val appendToHead: ?before:'a elt -> 'c elt -> unit
  val appendToBody: ?before:'a elt -> 'c elt -> unit
  val appendChildren: ?before:'a elt -> 'b elt ->  'c elt list -> unit
  val appendChildFirst: 'b elt ->  'c elt -> unit
  val insertChildAfter: 'a elt -> 'b elt ->  'c elt -> unit
  val insertChildrenAfter: 'a elt -> 'b elt ->  'c elt list -> unit
  val nth: 'a elt -> int -> 'b elt option
  val childLength: 'a elt -> int
  val removeChild: 'a elt -> 'b elt -> unit
  val replaceChild: 'a elt -> 'b elt -> 'c elt -> unit
  val replaceChildren: 'a elt -> 'b elt list -> unit
  val removeChildren: 'a elt -> unit
  val removeSelf: 'a elt -> unit
  val replaceSelf: 'a elt -> 'a elt -> unit

  val children: 'a elt -> 'b elt list
  val by_id: string -> 'b elt option
  val by_classname: string -> 'b elt list

  val disable: 'a elt -> unit
  val enable: 'a elt -> unit

  val value: 'a elt -> string

  val hasClass: 'a elt -> string -> bool
  val addClass: 'a elt -> string -> unit
  val removeClass: 'a elt -> string -> unit

  val scrollIntoView: ?bottom:bool -> 'a elt -> unit

  (* Returns [true] if the class has been set, [false] if it was unset *)
  val toggleClass: 'a elt -> string -> bool

  val focus: 'a elt -> unit
  val blur: 'a elt -> unit

  module Elt : sig
    val body : [`Body] elt
    val active : unit -> 'a elt
  end

  module Ev : sig
    type ('a, 'b) ev = 'a elt -> ('b Js.t -> bool) -> unit
    type ('a,'b) ev_unit = 'a elt -> ('b Js.t -> unit) -> unit
    val onkeyup: ('a,Dom_html.keyboardEvent) ev
    val onkeydown: ('a,Dom_html.keyboardEvent) ev
    val onmouseup: ('a,Dom_html.mouseEvent) ev
    val onmousedown: ('a,Dom_html.mouseEvent) ev
    val onmouseout: ('a,Dom_html.mouseEvent) ev
    val onmouseover: ('a,Dom_html.mouseEvent) ev
    val onclick: ('a,Dom_html.mouseEvent) ev
    val ondblclick: ('a,Dom_html.mouseEvent) ev
    val onload: ('a,Dom_html.event) ev
    val onerror: ('a,Dom_html.event) ev
    val onabort: ('a,Dom_html.event) ev
    val onfocus: ('a,Dom_html.event) ev
    val onblur: ('a,Dom_html.event) ev
    val onfocus_textarea: ('a,Dom_html.event) ev
    val onblur_textarea: ('a,Dom_html.event) ev
    val onscroll: ('a,Dom_html.event) ev
    val onreturn: ('a,Dom_html.keyboardEvent) ev_unit
    val onchange: ('a,Dom_html.event) ev
    val onchange_select: ('a,Dom_html.event) ev
    val oninput: ('a,Dom_html.event) ev
  end

  module Attr : sig
    val clientWidth: 'a elt -> int
    val clientHeight: 'a elt -> int
    val offsetWidth: 'a elt -> int
    val offsetHeight: 'a elt -> int
    val clientLeft: 'a elt -> int
    val clientTop: 'a elt -> int
  end

  (** Read the CSS properties of DOM elements. *)
  module Css : sig
    val background: 'a elt -> string
    val backgroundAttachment: 'a elt -> string
    val backgroundColor: 'a elt -> string
    val backgroundImage: 'a elt -> string
    val backgroundPosition: 'a elt -> string
    val backgroundRepeat: 'a elt -> string
    val border: 'a elt -> string
    val borderBottom: 'a elt -> string
    val borderBottomColor: 'a elt -> string
    val borderBottomStyle: 'a elt -> string
    val borderBottomWidth: 'a elt -> string
    val borderBottomWidthPx: 'a elt -> int
    val borderCollapse: 'a elt -> string
    val borderColor: 'a elt -> string
    val borderLeft: 'a elt -> string
    val borderLeftColor: 'a elt -> string
    val borderLeftStyle: 'a elt -> string
    val borderLeftWidth: 'a elt -> string
    val borderLeftWidthPx: 'a elt -> int
    val borderRight: 'a elt -> string
    val borderRightColor: 'a elt -> string
    val borderRightStyle: 'a elt -> string
    val borderRightWidth: 'a elt -> string
    val borderRightWidthPx: 'a elt -> int
    val borderSpacing: 'a elt -> string
    val borderStyle: 'a elt -> string
    val borderTop: 'a elt -> string
    val borderTopColor: 'a elt -> string
    val borderTopStyle: 'a elt -> string
    val borderTopWidth: 'a elt -> string
    val borderTopWidthPx: 'a elt -> int
    val borderWidth: 'a elt -> string
    val borderWidthPx: 'a elt -> int
    val bottom: 'a elt -> string
    val captionSide: 'a elt -> string
    val clear: 'a elt -> string
    val clip: 'a elt -> string
    val color: 'a elt -> string
    val content: 'a elt -> string
    val counterIncrement: 'a elt -> string
    val counterReset: 'a elt -> string
    val cssFloat: 'a elt -> string
    val cssText: 'a elt -> string
    val cursor: 'a elt -> string
    val direction: 'a elt -> string
    val display: 'a elt -> string
    val emptyCells: 'a elt -> string
    val font: 'a elt -> string
    val fontFamily: 'a elt -> string
    val fontSize: 'a elt -> string
    val fontStyle: 'a elt -> string
    val fontVariant: 'a elt -> string
    val fontWeight: 'a elt -> string
    val height: 'a elt -> string
    val heightPx: 'a elt -> int
    val left: 'a elt -> string
    val leftPx: 'a elt -> int
    val letterSpacing: 'a elt -> string
    val lineHeight: 'a elt -> string
    val listStyle: 'a elt -> string
    val listStyleImage: 'a elt -> string
    val listStylePosition: 'a elt -> string
    val listStyleType: 'a elt -> string
    val margin: 'a elt -> string
    val marginBottom: 'a elt -> string
    val marginBottomPx: 'a elt -> int
    val marginLeft: 'a elt -> string
    val marginLeftPx: 'a elt -> int
    val marginRight: 'a elt -> string
    val marginRightPx: 'a elt -> int
    val marginTop: 'a elt -> string
    val marginTopPx: 'a elt -> int
    val maxHeight: 'a elt -> string
    val maxHeightPx: 'a elt -> int
    val maxWidth: 'a elt -> string
    val maxWidthPx: 'a elt -> int
    val minHeight: 'a elt -> string
    val minHeightPx: 'a elt -> int
    val minWidth: 'a elt -> string
    val minWidthPx: 'a elt -> int
    val opacity: 'a elt -> string option
    val outline: 'a elt -> string
    val outlineColor: 'a elt -> string
    val outlineOffset: 'a elt -> string
    val outlineStyle: 'a elt -> string
    val outlineWidth: 'a elt -> string
    val overflow: 'a elt -> string
    val overflowX: 'a elt -> string
    val overflowY: 'a elt -> string
    val padding: 'a elt -> string
    val paddingBottom: 'a elt -> string
    val paddingBottomPx: 'a elt -> int
    val paddingLeft: 'a elt -> string
    val paddingLeftPx: 'a elt -> int
    val paddingRight: 'a elt -> string
    val paddingRightPx: 'a elt -> int
    val paddingTop: 'a elt -> string
    val paddingTopPx: 'a elt -> int
    val pageBreakAfter: 'a elt -> string
    val pageBreakBefore: 'a elt -> string
    val position: 'a elt -> string
    val right: 'a elt -> string
    val rightPx: 'a elt -> int
    val tableLayout: 'a elt -> string
    val textAlign: 'a elt -> string
    val textDecoration: 'a elt -> string
    val textIndent: 'a elt -> string
    val textTransform: 'a elt -> string
    val top: 'a elt -> string
    val topPx: 'a elt -> int
    val verticalAlign: 'a elt -> string
    val visibility: 'a elt -> string
    val whiteSpace: 'a elt -> string
    val width: 'a elt -> string
    val widthPx: 'a elt -> int
    val wordSpacing: 'a elt -> string
    val zIndex: 'a elt -> string
  end

  (** Modify the CSS properties of DOM elements. *)
  module SetCss : sig
    val background: 'a elt -> string -> unit
    val backgroundAttachment: 'a elt -> string -> unit
    val backgroundColor: 'a elt -> string -> unit
    val backgroundImage: 'a elt -> string -> unit
    val backgroundPosition: 'a elt -> string -> unit
    val backgroundRepeat: 'a elt -> string -> unit
    val border: 'a elt -> string -> unit
    val borderBottom: 'a elt -> string -> unit
    val borderBottomColor: 'a elt -> string -> unit
    val borderBottomStyle: 'a elt -> string -> unit
    val borderBottomWidth: 'a elt -> string -> unit
    val borderBottomWidthPx: 'a elt -> int -> unit
    val borderCollapse: 'a elt -> string -> unit
    val borderColor: 'a elt -> string -> unit
    val borderLeft: 'a elt -> string -> unit
    val borderLeftColor: 'a elt -> string -> unit
    val borderLeftStyle: 'a elt -> string -> unit
    val borderLeftWidth: 'a elt -> string -> unit
    val borderLeftWidthPx: 'a elt -> int -> unit
    val borderRight: 'a elt -> string -> unit
    val borderRightColor: 'a elt -> string -> unit
    val borderRightStyle: 'a elt -> string -> unit
    val borderRightWidth: 'a elt -> string -> unit
    val borderRightWidthPx: 'a elt -> int -> unit
    val borderSpacing: 'a elt -> string -> unit
    val borderStyle: 'a elt -> string -> unit
    val borderTop: 'a elt -> string -> unit
    val borderTopColor: 'a elt -> string -> unit
    val borderTopStyle: 'a elt -> string -> unit
    val borderTopWidth: 'a elt -> string -> unit
    val borderTopWidthPx: 'a elt -> int -> unit
    val borderWidth: 'a elt -> string -> unit
    val bottom: 'a elt -> string -> unit
    val bottomPx: 'a elt -> int -> unit
    val captionSide: 'a elt -> string -> unit
    val clear: 'a elt -> string -> unit
    val clip: 'a elt -> string -> unit
    val color: 'a elt -> string -> unit
    val content: 'a elt -> string -> unit
    val counterIncrement: 'a elt -> string -> unit
    val counterReset: 'a elt -> string -> unit
    val cssFloat: 'a elt -> string -> unit
    val cssText: 'a elt -> string -> unit
    val cursor: 'a elt -> string -> unit
    val direction: 'a elt -> string -> unit
    val display: 'a elt -> string -> unit
    val emptyCells: 'a elt -> string -> unit
    val font: 'a elt -> string -> unit
    val fontFamily: 'a elt -> string -> unit
    val fontSize: 'a elt -> string -> unit
    val fontStyle: 'a elt -> string -> unit
    val fontVariant: 'a elt -> string -> unit
    val fontWeight: 'a elt -> string -> unit
    val height: 'a elt -> string -> unit
    val heightPx: 'a elt -> int -> unit
    val left: 'a elt -> string -> unit
    val leftPx: 'a elt -> int -> unit
    val letterSpacing: 'a elt -> string -> unit
    val lineHeight: 'a elt -> string -> unit
    val listStyle: 'a elt -> string -> unit
    val listStyleImage: 'a elt -> string -> unit
    val listStylePosition: 'a elt -> string -> unit
    val listStyleType: 'a elt -> string -> unit
    val margin: 'a elt -> string -> unit
    val marginBottom: 'a elt -> string -> unit
    val marginBottomPx: 'a elt -> int -> unit
    val marginLeft: 'a elt -> string -> unit
    val marginLeftPx: 'a elt -> int -> unit
    val marginRight: 'a elt -> string -> unit
    val marginRightPx: 'a elt -> int -> unit
    val marginTop: 'a elt -> string -> unit
    val marginTopPx: 'a elt -> int -> unit
    val maxHeight: 'a elt -> string -> unit
    val maxHeightPx: 'a elt -> int -> unit
    val maxWidth: 'a elt -> string -> unit
    val maxWidthPx: 'a elt -> int -> unit
    val minHeight: 'a elt -> string -> unit
    val minHeightPx: 'a elt -> int -> unit
    val minWidth: 'a elt -> string -> unit
    val minWidthPx: 'a elt -> int -> unit
    val opacity: 'a elt -> string option -> unit
    val outline: 'a elt -> string -> unit
    val outlineColor: 'a elt -> string -> unit
    val outlineOffset: 'a elt -> string -> unit
    val outlineStyle: 'a elt -> string -> unit
    val outlineWidth: 'a elt -> string -> unit
    val overflow: 'a elt -> string -> unit
    val overflowX: 'a elt -> string -> unit
    val overflowY: 'a elt -> string -> unit
    val padding: 'a elt -> string -> unit
    val paddingBottom: 'a elt -> string -> unit
    val paddingBottomPx: 'a elt -> int -> unit
    val paddingLeft: 'a elt -> string -> unit
    val paddingLeftPx: 'a elt -> int -> unit
    val paddingRight: 'a elt -> string -> unit
    val paddingRightPx: 'a elt -> int -> unit
    val paddingTop: 'a elt -> string -> unit
    val paddingTopPx: 'a elt -> int -> unit
    val pageBreakAfter: 'a elt -> string -> unit
    val pageBreakBefore: 'a elt -> string -> unit
    val position: 'a elt -> string -> unit
    val right: 'a elt -> string -> unit
    val rightPx: 'a elt -> int -> unit
    val tableLayout: 'a elt -> string -> unit
    val textAlign: 'a elt -> string -> unit
    val textDecoration: 'a elt -> string -> unit
    val textIndent: 'a elt -> string -> unit
    val textTransform: 'a elt -> string -> unit
    val top: 'a elt -> string -> unit
    val topPx: 'a elt -> int -> unit
    val verticalAlign: 'a elt -> string -> unit
    val visibility: 'a elt -> string -> unit
    val whiteSpace: 'a elt -> string -> unit
    val width: 'a elt -> string -> unit
    val widthPx: 'a elt -> int -> unit
    val wordSpacing: 'a elt -> string -> unit
    val zIndex: 'a elt -> string -> unit
  end

end

val window : Dom_html.window Js.t
val window_open: ?features:string -> string -> string -> Dom_html.window Js.t Js.opt

module Window : sig
  val close: Dom_html.window Js.t -> unit
  val head: Dom_html.window Js.t -> [`Head] Tyxml_js.Html5.elt
  val body: Dom_html.window Js.t -> [`Body] Tyxml_js.Html5.elt
  val onresize:
    ?win:Dom_html.window Js.t ->
    (Dom_html.event Js.t -> bool) -> unit
  val onunload:
    ?win:Dom_html.window Js.t ->
    (Dom_html.event Js.t -> bool) -> unit
  val onbeforeunload:
    ?win:Dom_html.window Js.t ->
    (Dom_html.event Js.t -> bool * bool) -> unit
  val onhashchange:
    ?win:Dom_html.window Js.t ->
    (Dom_html.hashChangeEvent Js.t -> bool) -> unit
  val prompt: ?win:Dom_html.window Js.t -> ?value:string -> string -> string
end

val hide : 'a Tyxml_js.Html5.elt -> unit
val show : 'a Tyxml_js.Html5.elt -> unit

module Document : sig
  val uri: unit -> string
end

val parse_fragment: unit -> (string * string) list
val set_fragment: (string * string) list -> unit

module MakeLocal(V: sig type t val name: string end) : sig
  val get: unit -> V.t option
  val set: V.t -> unit
end

(** Returns an URL that can be passed to [Worker.create] from a string
    containing the code *)
val js_code_url: string -> string

(* (\** Wrapper for [Worker.create] that uses JS code as a string instead of a URL*\)
 * val worker_with_code: string -> ('a, 'b) Worker.worker Js.t
 * 
 * (\** Similar to [Worker.create], but retrieves the js file manually *\)
 * val worker: string -> ('a, 'b) Worker.worker Js.t Lwt.t *)
