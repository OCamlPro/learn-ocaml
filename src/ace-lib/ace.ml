(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Ace_types

let iter_option f = function
  | None -> ()
  | Some x -> f x

(** Editor *)

type 'a editor = {
  editor_div: Dom_html.divElement Js.t;
  editor: ('a editor * 'a option) Ace_types.editor Js.t;
  mutable marks: int list;
  mutable keybinding_menu: bool;
}

let ace : Ace_types.ace Js.t = Js.Unsafe.variable "ace"
let edit el = ace##(edit el)

let create_position r c =
  let pos : position Js.t = Js.Unsafe.obj [||] in
  pos##.row := r;
  pos##.column := c;
  pos
let greater_position p1 p2 =
  p1##.row > p2##.row ||
  (p1##.row = p2##.row && p1##.column > p2##.column)


let create_range s e =
  let range : range Js.t = Js.Unsafe.obj [||] in
  range##.start := s;
  range##.end_ := e;
  range

let read_position pos = (pos##.row, pos##.column)

let get_cursor_position {editor} = editor##(getCursorPosition)                       
                      
let read_range range =
  ((range##.start##.row, range##.start##.column),
   (range##.end_##.row, range##.end_##.column))

let get_contents ?range {editor} =
  let document = (editor##getSession)##getDocument in
  match range with
  | None ->
      Js.to_string @@ document##getValue
  | Some r ->
      Js.to_string @@ document##(getTextRange r)

let set_contents ?(reset_undo=false) {editor} code =
  let session = editor##getSession in
  session##getDocument##setValue (Js.string code);
  if reset_undo then session##getUndoManager##reset

let get_selection_range {editor} = editor##getSelectionRange

let get_selection {editor} =
  let document = (editor##getSession)##getDocument in
  let range = editor##getSelectionRange in
  Js.to_string @@ document##(getTextRange range)

let get_line {editor} line =
  let document = (editor##getSession)##getDocument in
  Js.to_string @@ document##(getLine line)

let create_editor editor_div =
  let editor = edit editor_div in
  Js.Unsafe.set editor "$blockScrolling" (Js.Unsafe.variable "Infinity");
  let data =
    { editor; editor_div; marks = []; keybinding_menu = false; } in
  editor##.customData := (data, None);
  editor##setOption (Js.string "displayIndentGuides") (Js.bool false);
  data

let get_custom_data { editor } =
  match snd editor##.customData with
  | None -> raise Not_found
  | Some x -> x

let set_custom_data { editor } data =
  let ed = fst editor##.customData in
  editor##.customData := (ed, Some data)

let set_mode {editor} name =
  editor##getSession##(setMode (Js.string name))

let on {editor} event callback =
  editor##getSession##(on (Js.string event) (Js.Unsafe.meth_callback callback))

let insert {editor} position content =
  editor##getSession##(insert position (Js.string content))

type mark_type = Error | Warning | Message

let string_of_make_type: mark_type -> string = function
  | Error -> "error"
  | Warning -> "warning"
  | Message -> "message"

let require s = (Js.Unsafe.variable "ace")##(require (Js.string s))

type range = Ace_types.range Js.t
let range_cstr = (require  "ace/range")##._Range
let range sr sc er ec : range =
  Js.Unsafe.new_obj range_cstr
    [| Js.Unsafe.inject sr ; Js.Unsafe.inject sc ;
       Js.Unsafe.inject er ; Js.Unsafe.inject ec |]

type loc = {
  loc_start: int * int;
  loc_end: int * int;
}

let set_mark editor ?loc ?(type_ = Message) msg =
  let session = (editor.editor)##getSession in
  let type_ = string_of_make_type type_ in
  let sr, sc, range =
    match loc with
    | None -> 0, 0, None
    | Some { loc_start = (sr, sc) ; loc_end = (er, ec) } ->
        let sr = sr - 1 in
        let er = er - 1 in
      sr, sc, Some (range sr sc er ec) in
  let annot : annotation Js.t = Js.Unsafe.obj [||] in
  annot##.row := sr;
  annot##.column := sc;
  annot##.text := Js.string msg;
  annot##.type_ := Js.string type_;
  let annotations =
    Array.concat [[| annot |]; Js.to_array (session##getAnnotations)] in
  session##(setAnnotations (Js.array @@ annotations));
  match range with
  | None -> ()
  | Some range ->
    editor.marks <-
      session##(addMarker range (Js.string type_) (Js.string "text") (Js._false)) ::
      editor.marks

let set_background_color editor color =
  editor.editor_div##.style##.backgroundColor := Js.string color

let add_class { editor_div } name =
  editor_div##.classList##(add (Js.string name))
let remove_class { editor_div } name =
  editor_div##.classList##(remove (Js.string name))

let clear_marks editor =
  let session = (editor.editor)##getSession in
  List.iter (fun i -> session##(removeMarker i)) editor.marks;
  session##clearAnnotations;
  editor.marks <- []

let record_event_handler editor event handler =
  editor.editor##(on (Js.string event) handler)

let focus { editor } = editor##focus
let resize { editor } force = editor##(resize (Js.bool force))

let get_keybinding_menu e =
  if e.keybinding_menu then
    Some (Obj.magic e.editor : keybinding_menu Js.t)
  else
    let ext = require "ace/ext/keybinding_menu" in
    Js.Optdef.case
      ext
      (fun () -> None)
      (fun _ext ->
         e.keybinding_menu <- true;
         Some (Obj.magic e.editor : keybinding_menu Js.t))

let show_keybindings e =
  match get_keybinding_menu e with
  | None ->
      Firebug.console##(log
        (Js.string "You should load: 'ext-keybinding_menu.js'"))
  | Some o ->
      o##showKeyboardShortcuts

let add_keybinding { editor }
    ?ro ?scrollIntoView ?multiSelectAction
    name key exec =
  let command : _ command Js.t = Js.Unsafe.obj [||] in
  let binding : binding Js.t = Js.Unsafe.obj [||] in
  command##.name := Js.string name;
  command##.exec := Js.wrap_callback (fun ed _args -> exec (fst ed##.customData));
  iter_option (fun ro -> command##.readOnly := Js.bool ro) ro;
  iter_option
    (fun s -> command##.scrollIntoView := Js.string s)
    scrollIntoView;
  iter_option
    (fun s -> command##.multiSelectAction := Js.string s)
    multiSelectAction;
  binding##.win := Js.string key;
  binding##.mac := Js.string key;
  command##.bindKey := binding;
  editor##.commands##(addCommand command)

(** Mode *)

type token = Ace_types.token Js.t
let token ~type_ value =
  let obj : Ace_types.token Js.t = Js.Unsafe.obj [||] in
  obj##.value := Js.string value;
  obj##._type := Js.string type_;
  obj

type doc = Ace_types.document Js.t

type 'state helpers = {
  initial_state: unit -> 'state;
  get_next_line_indent: 'state -> line:string -> tab:string -> string;
  get_line_tokens: string -> 'state -> int -> doc -> ('state * token list);
  check_outdent: ('state -> string -> string -> bool) option;
  auto_outdent: ('state -> document Js.t -> int -> unit) option;
}

let create_js_line_tokens (st, tokens) =
  let obj : _ Ace_types.line_tokens Js.t = Js.Unsafe.obj [||] in
  obj##.state := st;
  obj##.tokens := Js.array (Array.of_list tokens);
  obj

let define_mode name helpers =
  let js_helpers : _ ace_mode_helpers Js.t = Js.Unsafe.obj [||] in
  js_helpers##.initialState := Js.wrap_callback helpers.initial_state;
  js_helpers##.getNextLineIndent :=
    (Js.wrap_callback @@ fun st line tab ->
     Js.string @@
     helpers.get_next_line_indent
       st ~line:(Js.to_string line) ~tab:(Js.to_string tab));
  js_helpers##.getLineTokens :=
    (Js.wrap_callback @@ fun line st row doc ->
     create_js_line_tokens @@
     helpers.get_line_tokens (Js.to_string line) st row doc);
  begin match helpers.check_outdent with
    | None -> ()
    | Some check_outdent ->
        js_helpers##.checkOutdent :=
          (Js.wrap_callback @@ fun st line input ->
           Js.bool @@
           check_outdent st (Js.to_string line) (Js.to_string input))
  end;
  begin match helpers.auto_outdent with
    | None -> ()
    | Some auto_outdent ->
        js_helpers##.autoOutdent := Js.wrap_callback auto_outdent
  end;
  Js.Unsafe.fun_call
    (Js.Unsafe.variable "define_ocaml_mode")
    [| Js.Unsafe.inject (Js.string ("ace/mode/" ^ name)) ;
       Js.Unsafe.inject js_helpers |]

let set_font_size {editor} sz =
  editor##(setFontSize sz)
let set_tab_size {editor} sz =
  editor##getSession##(setTabSize sz)

let set_readonly {editor} t =
  editor##setReadOnly (Js.bool t)

let get_state { editor } row =
  editor##getSession##(getState row)

let get_last { editor } =
  let doc = (editor##getSession)##getDocument in
  let lines = doc##getLength in
  let last = doc##(getLine (lines - 1)) in
  create_position (lines - 1) last##.length

let document { editor } =
  (editor##getSession)##getDocument

let replace doc range text =
  doc##(replace range (Js.string text))

let delete doc range =
  doc##(replace range (Js.string ""))

let remove { editor } dir =
  editor##(remove (Js.string dir))
