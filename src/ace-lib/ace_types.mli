(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml

class type token = object
  method value : Js.js_string Js.t Js.prop
  method _type : Js.js_string Js.t Js.prop
end

(** Editor *)

class type position = object
  method row : int Js.prop
  method column : int Js.prop
end

class type range = object
  method start : position Js.t Js.prop
  method end_ : position Js.t Js.prop
end

class type binding = object
  method win : Js.js_string Js.t Js.prop
  method mac : Js.js_string Js.t Js.prop
end

class type annotation = object
  method row : int Js.prop
  method column : int Js.prop
  method text : Js.js_string Js.t Js.prop
  method type_ : Js.js_string Js.t Js.prop
end

class type document = object
  method getLine : int -> Js.js_string Js.t Js.meth
  method getLines : int -> int -> Js.string_array Js.t Js.meth
  method getTextRange : range Js.t -> Js.js_string Js.t Js.meth
  method getValue : Js.js_string Js.t Js.meth
  method replace : range Js.t -> Js.js_string Js.t -> unit Js.meth
  method setValue : Js.js_string Js.t -> unit Js.meth
  method getLength : int Js.meth
end

class type undoManager = object
  method undo : bool Js.t -> range Js.t Js.meth
  method redo : bool Js.t -> unit Js.meth
  method reset : unit Js.meth
end

class type editSession = object
  method getDocument : document Js.t Js.meth
  method getTabSize : int Js.meth
  method setTabSize : int -> unit Js.meth
  method getTokenAt : int -> int -> token Js.t Js.meth
  method replace : range Js.t -> Js.js_string Js.t -> unit Js.meth
  method setMode : Js.js_string Js.t -> unit Js.meth
  method on : Js.js_string Js.t ->
              ((Dom_html.event Js.t , unit) Js.meth_callback)->
              unit Js.meth
  method insert : position Js.t -> Js.js_string Js.t -> unit Js.meth      
  method setAnnotations : annotation Js.t Js.js_array Js.t -> unit Js.meth
  method getAnnotations : annotation Js.t Js.js_array Js.t Js.meth
  method clearAnnotations : unit Js.meth
  method addMarker :
    range Js.t -> Js.js_string Js.t -> Js.js_string Js.t -> bool Js.t ->
    int Js.meth
  method getMarkers :
    bool Js.t -> int Js.js_array Js.t Js.meth
  method removeMarker : int -> unit Js.meth
  method getState : 'a. int -> (< .. > as 'a) Js.t Js.meth
  method getUndoManager : undoManager Js.t Js.meth
end

class type selection = object
  method selectLine : unit Js.meth
  method selectTo : int -> int -> unit Js.meth
  method setSelectionRange : range Js.t -> bool Js.t -> unit Js.meth
end

class type ['a] editor = object
  method on : Js.js_string Js.t -> (unit -> unit) -> unit Js.meth
  method commands : 'a commandManager Js.t Js.prop
  method destroy : unit Js.meth
  method getCursorPosition : position Js.t Js.meth
  method getSelection : selection Js.t Js.meth
  method getSelectionRange : range Js.t Js.meth
  method getSession : editSession Js.t Js.meth
  method getValue : Js.js_string Js.t Js.meth
  method moveCursorTo : int -> int -> unit Js.meth
  method remove : Js.js_string Js.t -> unit Js.meth
  method remove_range : range Js.t -> unit Js.meth
  method removeLines : unit Js.meth
  method resize : bool Js.t -> unit Js.meth
  method selectAll : unit Js.meth
  method setReadOnly : bool Js.t -> unit Js.meth
  method setSession : editSession Js.t -> unit Js.meth
  method setTheme : Js.js_string Js.t -> unit Js.meth
  method setValue : Js.js_string Js.t -> unit Js.meth
  method setOption : Js.js_string Js.t -> 'b Js.t -> unit Js.meth
  method toggleCommentLines : unit Js.meth
  method focus : unit Js.meth
  method setFontSize : int -> unit Js.meth
  method customData : 'a Js.prop
end

and ['a] command = object
  method name : Js.js_string Js.t Js.prop
  method bindKey : binding Js.t Js.prop
  method exec : ('a editor Js.t -> unit -> unit) Js.callback Js.prop
  method readOnly : bool Js.t Js.prop
  method multiSelectAction : Js.js_string Js.t Js.prop
  method scrollIntoView : Js.js_string Js.t Js.prop
end

and ['a] commandManager = object
  method addCommand : 'a command Js.t -> unit Js.meth
end

class type ace = object
  method edit: 'a. Dom_html.element Js.t -> 'a editor Js.t Js.meth
end

class type keybinding_menu = object
  method showKeyboardShortcuts: unit Js.meth
end

(** Mode *)

class type ['a] line_tokens = object
  method state : 'a Js.prop
  method tokens : token Js.t Js.js_array Js.t Js.prop
end

class type ['state] ace_mode_helpers = object
  method initialState :
    (unit -> 'state) Js.callback Js.writeonly_prop
  method getNextLineIndent :
    ('state -> Js.js_string Js.t -> Js.js_string Js.t -> Js.js_string Js.t)
      Js.callback Js.writeonly_prop
  method getLineTokens :
    (Js.js_string Js.t -> 'state -> int -> document Js.t ->
     'state line_tokens Js.t) Js.callback Js.writeonly_prop
  method checkOutdent :
    ('state -> Js.js_string Js.t -> Js.js_string Js.t -> bool Js.t)
      Js.callback Js.writeonly_prop
  method autoOutdent :
    ('state -> document Js.t -> int -> unit) Js.callback Js.writeonly_prop
end

class type ace_mode = object
  method define :
    Js.js_string Js.t -> 'state ace_mode_helpers -> unit Js.meth
end
