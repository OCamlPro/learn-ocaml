(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_utils
open Js_of_ocaml
open Lwt.Infix

let debug_indent = ref 0
(* <= 0: nothing *)
(* 1: fun call *)
(* 2: ocp-indent stacks *)

let token_type =
  let open Approx_tokens in
  function

  | COMMENT_OPEN_EOL
  | COMMENT_OPEN_CLOSE
  | COMMENT_OPEN
  | COMMENT_VERB_OPEN
  | COMMENT_CODE_OPEN
  | COMMENT_CONTENT
  | COMMENT_CLOSE
  | COMMENT_VERB_CLOSE
  | COMMENT_CODE_CLOSE -> "comment"

  | AND
  | AS
  | ASSERT
  | BEGIN
  | CLASS
  | CONSTRAINT
  | DO
  | DONE
  | DOWNTO
  | ELSE
  | END
  | EXCEPTION
  | EXTERNAL
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | IF
  | IN
  | INCLUDE
  | INHERIT
  | INITIALIZER
  | LAZY
  | LET
  | MATCH
  | METHOD
  | MODULE
  | MUTABLE
  | NEW
  | OBJECT
  | OF
  | OPEN
  | OR
  | PRIVATE
  | REC
  | SIG
  | STRUCT
  | THEN
  | TO
  | TRY
  | TYPE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH

  | INFIXOP3 "mod"
  | INFIXOP3 "land"
  | INFIXOP3 "lor"
  | INFIXOP3 "lxor"
  | INFIXOP4 "lsl"
  | INFIXOP4 "lsr"
  | INFIXOP4 "asr" -> "keyword"

  | FLOAT _
  | INT _
  | INT32 _
  | INT64 _
  | NATIVEINT _ -> "constant"

  | INFIXOP0 _
  | INFIXOP1 _
  | INFIXOP2 _
  | INFIXOP3 _
  | INFIXOP4 _
  | PREFIXOP _ -> "function"

  | LABEL _
  | OPTLABEL _ -> "type" (* Hack *)

  | AMPERAMPER
  | AMPERSAND
  | BACKQUOTE
  | BANG
  | BAR
  | BARBAR
  | BARRBRACKET
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
  | DOT
  | DOTDOT
  | EQUAL
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETAT
  | LBRACKETATAT
  | LBRACKETATATAT
  | LBRACKETBAR
  | LBRACKETGREATER
  | LBRACKETLESS
  | LBRACKETPERCENT
  | LBRACKETPERCENTPERCENT
  | LESS
  | LESSMINUS
  | LPAREN
  | MINUS
  | MINUSDOT
  | MINUSGREATER
  | PLUS
  | PLUSDOT
  | QUESTION
  | QUESTIONQUESTION
  | QUOTE
  | RBRACE
  | RBRACKET
  | RPAREN
  | SEMI
  | SEMISEMI
  | SHARP
  | STAR
  | TILDE
  | UNDERSCORE -> "operator"

  | LINE_DIRECTIVE _ -> "meta"

  | FALSE
  | TRUE
  | LIDENT _
  | UIDENT _
  | TYPEVAR -> "variable"

  | EOL
  | SPACES -> "text"
  | ILLEGAL_CHAR _ -> "error"

  | EOF -> assert false

  | CHAR _

  | STRING_OPEN
  | STRING_CONTENT
  | ESCAPED_EOL
  | STRING_CLOSE

  | PPX_QUOTATION_OPEN
  | PPX_QUOTATION_CONTENT
  | PPX_QUOTATION_CLOSE -> "string"

  | P4_QUOTATION_OPEN
  | P4_QUOTATION_CONTENT
  | P4_QUOTATION_CLOSE -> "meta"

type state = {
  block: IndentBlock.t;
  lex_ctxt: Nstream.snapshot;
}

let initial_state = {
  block = IndentBlock.empty ;
  lex_ctxt =
    { Approx_lexer.initial_state with Approx_lexer.eof_closing = false },
    Nstream.Region.zero;
}

let in_comment ctxt =
  let open Approx_lexer in
  let rec loop = function
    | [] | Code :: _ -> false
    | Comment :: _ -> true
    | _ :: stack -> loop stack in
  loop ctxt.stack
  (* List.mem Comment ctxt.stack *)

let wrap_token state token =
  let open Nstream in
  let value = token.between ^ token.substr in
  let type_ =
    if in_comment (fst state.lex_ctxt) then
      "comment"
    else
      token_type token.token in
  Ace.token ~type_ value

type config = {
  indent: IndentConfig.t;
  forced: bool;
}

let config =
  ref {
    indent = IndentConfig.({default with i_match_clause = 4});
    forced = true;
  }

let get_next_line_indent state ~line ~tab =
  let indent = IndentBlock.guess_indent state.block in
  if !debug_indent > 0 then
    debug "get_next_line_indent %S %S -> %d" line tab indent;
  String.make indent ' '

let comment_open between = Ace.token ~type_:"comment" (between ^ "(*")

let phrases : (Ace.doc * Ace_types.position Js.t list ref) list ref = ref []

let get_phrases doc =
  try List.assq doc !phrases
  with Not_found ->
    let ph = ref [] in
    phrases := (doc, ph) :: !phrases;
    ph

let remove_phrases doc row =
  let rec remove row = function
    | [] -> []
    | pos' :: _ as phrases when Ace.greater_position row pos' -> phrases
    | _ :: phrases -> remove row phrases in
  let phrases = get_phrases doc in
  phrases := remove (Ace.create_position row 0) !phrases

let mark_phrase doc pos =
  let phrases = get_phrases doc in
  phrases := pos :: !phrases;
  if !debug_indent > 0 then begin
    debug "Phrases:";
    List.iter js_debug (List.rev !phrases)
  end

let all_spaces s =
  let rec loop s l i = i >= l || s.[i] = ' ' && loop s l (i+1) in
  loop s (String.length s) 0

let get_line_tokens line st row doc =
  if !debug_indent > 0 then debug "get_line_tokens %d %S" row line;
  let stream = Nstream.of_string ~st:st.lex_ctxt (line ^ "\n") in
  remove_phrases doc row;
  let rec iter ?(first = false) st offset stream tokens =
    let open Approx_tokens in
    let open Nstream in
    match Nstream.next_full stream with
    | None | Some ({token = EOF ; _}, _, _) -> st, List.rev tokens
    | Some (tok, lex_ctxt, stream) ->
        let block = IndentBlock.update !config.indent st.block stream tok; in
        let tok, block, offset =
          if not first || all_spaces line || IndentBlock.is_in_comment block then
            tok, block, offset
          else if not !config.forced then
            (* Update ocp-indent context with current indentation. *)
            tok, IndentBlock.reverse block, 0
          else if IndentBlock.indent block = String.length tok.between then
            tok, block, 0
          else
            (* Update line to the 'forced' indentation. *)
            let old_indent = String.length tok.between in
            let indent = IndentBlock.indent block in
            if !debug_indent > 0 then
              debug "Reindent: new indent %d (old: %d)" indent old_indent;
            let spaces = String.make indent ' ' in
            Ace.replace
              doc
              (Ace.create_range
                 (Ace.create_position row 0)
                 (Ace.create_position row old_indent))
              spaces;
            { tok with between = spaces }, block, (indent - old_indent) in
        let col = Nstream.(Region.start_column tok.region) in
        if IndentBlock.is_at_top block then
          mark_phrase doc (Ace.create_position row (col + offset));
        if !debug_indent > 1 && tok.token <> EOL && tok.token <> ESCAPED_EOL then
          IndentBlock.dump block;
        let st = { block; lex_ctxt; } in
        match tok.token with
        | EOL | ESCAPED_EOL ->
            (* FIXME some spaces ??? *)
            (st, List.rev tokens)
        | COMMENT_OPEN_EOL ->
            (st, List.rev (comment_open tok.between :: tokens))
        | _ ->
            iter st offset stream (wrap_token st tok :: tokens)
  in
  iter ~first:true st 0 stream []

let () =
  let open Ace in
  let initial_state () = initial_state in
  define_mode "ocaml.ocp" {
    initial_state;
    get_next_line_indent;
    get_line_tokens;
    check_outdent = None;
    auto_outdent = None;
  }

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

type editor = {
  ace: editor Ace.editor;
  mutable current_error: error option;
  mutable current_warnings: warning list;
}

let get_editor { ace; _ } = ace
let get_current_error { current_error; _ } = current_error
let get_current_warnings { current_warnings; _ } = current_warnings

let reset_error editor =
  editor.current_error <- None;
  editor.current_warnings <- [];
  Ace.clear_marks editor.ace;
  Ace.remove_class editor.ace "ocaml-check-error";
  Ace.remove_class editor.ace "ocaml-check-warn";
  Ace.remove_class editor.ace "ocaml-check-success" ;
  Lwt_js.sleep 0.1

let report_error editor ?(set_class = true) err warnings =
  reset_error editor >>= fun () ->
  Lwt_js.yield () >|= fun () ->
  let add_warning editor { loc; msg } =
    Ace.set_mark editor ~loc ~type_:Ace.Warning msg in
  editor.current_error <- err;
  editor.current_warnings <- warnings;
  match err, warnings with
  | None, [] ->
      if set_class then
        Ace.add_class editor.ace "ocaml-check-success";
  | None, warnings ->
      if set_class then
        Ace.add_class editor.ace "ocaml-check-warn";
      List.iter (add_warning editor.ace) warnings
  | Some { locs; msg }, warnings ->
      if set_class then
        Ace.add_class editor.ace "ocaml-check-error";
      List.iter (add_warning editor.ace) warnings;
      match locs with
      | [] ->
          Ace.set_mark editor.ace ~type_:Ace.Error msg
      | locs ->
          List.iter
            (fun loc -> Ace.set_mark editor.ace ~loc ~type_:Ace.Error msg)
            locs

let report_current_error editor ?set_class () =
  report_error editor ?set_class editor.current_error editor.current_warnings

let get_state editor row =
  let s = Ace.get_state editor row in
  if Js.to_string (Js.typeof s) = "string" then
    initial_state
  else
    (Obj.magic s : state)

let get_old_indent line =
  let rec loop line len i =
    if i < len && line.[i] = ' ' then loop line len (i+1) else i in
  loop line (String.length line) 0

let get_indent state line =
  debug "Indent!";
  IndentBlock.dump state.block;
  match Nstream.(next (of_string ~st:state.lex_ctxt line)) with
  | None | Some ({ Nstream.token = Approx_tokens.EOF; _ } , _) ->
      IndentBlock.guess_indent state.block
  | Some _ when IndentBlock.is_in_comment state.block ->
      IndentBlock.guess_indent state.block
  | Some (token, stream) ->
      if !debug_indent > 1 then IndentBlock.dump state.block;
      let block =
        IndentBlock.update !config.indent state.block stream token in
      if !debug_indent > 1 then IndentBlock.dump block;
      IndentBlock.indent block

let do_indent ace_editor =
  let ((row, _col), _) =
    (* TODO when multiple line are selected... *)
    Ace.read_range (Ace.get_selection_range ace_editor) in
  let state = get_state ace_editor (row - 1) in
  if IndentBlock.is_in_comment state.block || not !config.forced then begin
    if !debug_indent > 0 then
      debug "Tab-indent: line %d (%a)"
        row Approx_lexer.print_context (fst state.lex_ctxt);
    let line = Ace.get_line ace_editor row in
    let old_indent = get_old_indent line in
    let indent = get_indent state line in
    if !debug_indent > 0 then
      debug "Tab-indent: new indent %d (old: %d)" indent old_indent;
    if old_indent <> indent && indent >= 0 then
      Ace.replace
        (Ace.document ace_editor)
        (Ace.create_range
           (Ace.create_position row 0)
           (Ace.create_position row old_indent))
        (String.make indent ' ')
  end

let rec all_spaces line i max =
  i >= max ||
  ((line.[i] = ' ' || line.[i] = '\n') && all_spaces line (succ i) max)

let rec all_trailing_spaces line i =
  if i <= 0 || line.[i] <> ' ' then
    i
  else
    all_trailing_spaces line (i-1)

let remove_trailing_spaces line =
  let last = String.length line - 1 in
  let last_non_space = all_trailing_spaces line last in
  if last != last_non_space then
    String.sub line 0 (last_non_space + 1)
  else
    line

let may_reset_indent ace_editor =
  let (_, (row, _col)) =
    Ace.read_range (Ace.get_selection_range ace_editor) in
  let line = Ace.get_line ace_editor row in
  if all_spaces line 0 (String.length line) then begin
    let state = get_state ace_editor (row-1) in
    if not (IndentBlock.is_in_comment state.block) then begin
      let indent = IndentBlock.guess_indent state.block in
      let old_indent = get_old_indent line in
      Ace.replace
        (Ace.document ace_editor)
        (Ace.create_range
           (Ace.create_position row 0)
           (Ace.create_position row old_indent))
        (String.make indent ' ')
    end
  end

let do_delete ace_editor =
  if !config.forced then begin
    let ((row, col), (row2, _col2)) =
      Ace.read_range (Ace.get_selection_range ace_editor) in
    if !debug_indent > 0 then
      debug "Delete: line %d col %d -> line %d col %d" row col row2 _col2;
    let selected = Ace.get_selection ace_editor in
    let line = Ace.get_line ace_editor row in
    let state = get_state ace_editor (row2-1) in
    if IndentBlock.is_in_comment state.block then
      Ace.remove ace_editor "left"
    else if not (all_spaces line 0 (min col (String.length line)) &&
                 all_spaces selected 0 (String.length selected)) then begin
      Ace.remove ace_editor "left";
      may_reset_indent ace_editor
    end else if row > 0 then begin
      let raw_prev_line = Ace.get_line ace_editor (row-1) in
      let prev_line = remove_trailing_spaces raw_prev_line in
      let old_indent = get_old_indent (Ace.get_line ace_editor row2) in
      if String.length prev_line = 0 then
        Ace.delete
          (Ace.document ace_editor)
          (Ace.create_range
             (Ace.create_position (row-1) (String.length raw_prev_line))
             (Ace.create_position row2 old_indent))
      else
        Ace.replace
          (Ace.document ace_editor)
          (Ace.create_range
             (Ace.create_position (row-1) (String.length prev_line))
             (Ace.create_position row2 old_indent))
          " "
    end
  end else begin
    Ace.remove ace_editor "left"
  end

let create_ocaml_editor div check_valid_state =
  let ace = Ace.create_editor div check_valid_state in
  Ace.set_mode ace "ace/mode/ocaml.ocp";
  Ace.set_tab_size ace !config.indent.IndentConfig.i_base;
  let editor = { ace; current_error = None; current_warnings = [] } in
  Ace.set_custom_data editor.ace editor;
  Ace.record_event_handler editor.ace "change"
    (fun () -> Lwt.async (fun () -> reset_error editor));
  Ace.add_keybinding editor.ace "backspace" "Shift-Backspace|Backspace"
    do_delete;
  Ace.add_keybinding editor.ace "indent" "Tab" do_indent;
  editor

(* GRGR TODO 'checkOutdent' on non forced mode. *)
