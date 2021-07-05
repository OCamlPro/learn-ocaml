(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml

type block =
  | Html of string * [ `Div ] Tyxml_js.Html5.elt
  | Std of (string * [ `Out | `Err ]) list ref * [ `Pre ] Tyxml_js.Html5.elt
  | Code of string * pretty list ref * [ `Pre ] Tyxml_js.Html5.elt * Nstream.snapshot option
  | Answer of string * pretty list ref * [ `Pre ] Tyxml_js.Html5.elt * Nstream.snapshot option
  | Error of Toploop_results.error * [ `Pre ] Tyxml_js.Html5.elt
  | Warning of int * Toploop_results.warning * [ `Pre ] Tyxml_js.Html5.elt
  | Phrase of phrase * block list ref

and pretty =
  | String of string
  | Ref of int
  | Class of string * pretty list

and phrase =
  { mutable warnings : int }

let phrase () =
  { warnings = 0 }

type output =
  { limit : int ;
    container : [ `Div ] Tyxml_js.Html5.elt ;
    mutable blocks : block list ;
    on_resize : unit -> unit }

let setup
    ?(limit = max_int)
    ?(on_resize = (fun () -> ()))
    ~container () =
  Js_utils.Manip.addClass container "toplevel-output" ;
  { container ; limit ; blocks = [] ; on_resize }

let enforce_limit { limit ; container } =
  let container = Tyxml_js.To_dom.of_div container in
  while container##.childNodes##.length > limit do
    Js.Opt.case
      (container##.firstChild)
      (fun () -> ())
      (fun child -> ignore (container##(removeChild child)))
  done

let scroll { container ; on_resize } =
  let container = Tyxml_js.To_dom.of_div container in
  Lwt.async @@ fun () ->
  Lwt.bind (Lwt_js.yield ()) @@ fun () ->
  container##.scrollTop := container##.scrollHeight - container##.clientHeight ;
  on_resize () ;
  Lwt.return_unit

let rec pretty_html
  : 'a. pretty list -> ([> `Span | `PCDATA ] as 'a) Tyxml_js.Html5.elt list
  = fun pretty ->
  let open Tyxml_js.Html5 in
  List.map
    (function
      | String text ->
          txt text
      | Ref n ->
          span ~a: [ a_class [ "ref" ] ] [ txt (string_of_int n) ]
      | Class (cls, [ Class (cls', [ Class (cls'', toks) ]) ]) ->
          span ~a: [ a_class [ cls ; cls' ; cls'' ] ] (pretty_html toks)
      | Class (cls, [ Class (cls', toks) ]) ->
          span ~a: [ a_class [ cls ; cls' ] ] (pretty_html toks)
      | Class (cls, toks) ->
          span ~a: [ a_class [ cls ] ] (pretty_html toks))
    pretty

let initial_state =
  { Approx_lexer.initial_state with Approx_lexer.eof_closing = false },
  Nstream.Region.zero

let prettify_ocaml ?(snapshot = initial_state) code =
  let stream = Nstream.of_string ~st: snapshot code in
  let rec format snapshot stream acc =
    let open Approx_tokens in
    let open Nstream in
    match Nstream.next_full stream with
    | None -> List.rev acc, snapshot
    | Some ({token = EOF}, _snapshot, _) -> List.rev acc, snapshot
    | Some (tok, snapshot, stream) ->
        let this =
          let kind = Ocaml_mode.token_type tok.token in
          Class (kind, [ String tok.substr ]) ::
          if tok.between = "" then [] else [ String tok.between ] in
        format (Some snapshot) stream (this @ acc) in
  format (Some snapshot) stream []

let rec last_elt = function
  | [] -> raise Not_found
  | Html (_, div) :: _ -> (div :> [ `Div | `Pre ] Tyxml_js.Html5.elt)
  | Std (_, pre) :: _
  | Code (_, _, pre, _) :: _
  | Answer (_, _, pre, _) :: _
  | Error (_, pre) :: _
  | Warning (_, _, pre) :: _ -> (pre :> [ `Div | `Pre ] Tyxml_js.Html5.elt)
  | Phrase (_, { contents }) :: rest ->
      try last_elt contents with Not_found -> last_elt rest

let find_phrase output u =
  List.fold_left
    (fun acc block -> match acc, block with
       | None, Phrase (u', l) when u == u' -> Some l
       | _ -> acc)
    None output.blocks

let insert output ?phrase block elt =
  let hr = Tyxml_js.Html5.hr () in
  match phrase with
  | None ->
      output.blocks <- block :: output.blocks ;
      Js_utils.Manip.appendChild output.container hr ;
      Js_utils.Manip.appendChild output.container elt ;
      scroll output
  | Some u ->
      match find_phrase output u with
      | Some l ->
          Js_utils.Manip.insertChildAfter output.container (last_elt !l) elt ;
          l := block :: !l ;
          scroll output
      | None ->
          output.blocks <- Phrase (u, ref [ block ]) :: output.blocks ;
          Js_utils.Manip.appendChild output.container hr ;
          Js_utils.Manip.appendChild output.container elt ;
          scroll output

let output_std ?phrase output (str, chan) =
  enforce_limit output ;
  let buf, pre =
    match output.blocks with
    | Phrase (u, l) :: _ when (Some u) = phrase ->
        let rec find = function
          | Std (buf, pre) :: _ -> buf, pre
          | _ :: rest -> find rest
          | [] ->
              let buf, pre =
                ref [],
                Tyxml_js.Html5.(pre ~a: [ a_class [ "toplevel-output" ] ]) [] in
              Js_utils.Manip.insertChildAfter output.container (last_elt !l) pre ;
              l := Std (buf, pre) :: !l ;
              Js_utils.Manip.appendChild output.container pre ;
              buf, pre in
        find !l
    | Std (buf, pre) :: _ -> buf, pre
    | _ ->
        let hr = Tyxml_js.Html5.hr () in
        let buf, pre =
          ref [],
          Tyxml_js.Html5.(pre ~a: [ a_class [ "toplevel-output" ] ]) [] in
        output.blocks <- Std (buf, pre) :: output.blocks ;
        Js_utils.Manip.appendChild output.container hr ;
        Js_utils.Manip.appendChild output.container pre ;
        buf, pre in
  let cls = match chan with `Err -> "stderr" | `Out -> "stdout" in
  Js_utils.Manip.appendChild pre
    (Tyxml_js.Html5.(span ~a: [ a_class [ cls ] ] [ txt str ])) ;
  buf := (str, chan) :: !buf ;
  scroll output

let output_stdout ?phrase output str =
  output_std ?phrase output (str, `Out)

let output_stderr ?phrase output str =
  output_std ?phrase output (str, `Err)

let output_html ?phrase output html =
  enforce_limit output ;
  let div = Tyxml_js.Html5.(div ~a: [ a_class [ "toplevel-html-block" ] ]) [] in
  Js_utils.Manip.setInnerHtml div html ;
  Js_utils.Manip.appendChild output.container div ;
  insert output ?phrase (Html (html, div)) div

let output_code ?phrase output code =
  let snapshot =
    let blocks = match phrase with
      | None -> output.blocks
      | Some u ->
          match find_phrase output u with
          | None -> []
          | Some l -> !l in
    match blocks with
    | Code (_, _, _, snapshot) :: _ -> snapshot
    | [] | _ -> None in
  enforce_limit output ;
  let pretty, snapshot = prettify_ocaml ?snapshot code in
  let pre =
    Tyxml_js.Html5.(pre ~a: [ a_class [ "toplevel-code" ] ])
      (pretty_html pretty) in
  insert output ?phrase (Code (code, ref pretty, pre, snapshot)) pre

let output_answer ?phrase output answer =
  let snapshot =
    let blocks = match phrase with
      | None -> output.blocks
      | Some u ->
          match find_phrase output u with
          | None -> []
          | Some l -> !l in
    match blocks with
    | Answer (_, _, _, snapshot) :: _ -> snapshot
    | [] | _ -> None in
  enforce_limit output ;
  let pretty, snapshot = prettify_ocaml ?snapshot answer in
  let pre =
    Tyxml_js.Html5.(pre ~a: [ a_class [ "toplevel-answer" ] ])
      (pretty_html pretty) in
  insert output ?phrase (Answer (answer, ref pretty, pre, snapshot)) pre

open Toploop_results

let inside (l, c) { loc_start = (sl, sc) ; loc_end = (el, ec) } =
  ((l > sl) || (l = sl && c >= sc))
  && ((l < el) || (l = el && c < ec))

let last (l, c) { loc_end = (el, ec) } =
  l = el && c = ec - 1

let hilight_pretty cls pretty locs lbl =
  let hilight_one pretty loc =
    let rec hilight_one pretty pos acc = match pretty with
      | [] -> List.rev acc, pos
      | Class (cls, toks) :: rest ->
          let toks, pos = hilight_one toks pos [] in
          hilight_one rest pos (Class (cls, toks) :: acc)
      | String "" :: rest ->
          hilight_one rest pos acc
      | Ref n :: rest ->
          hilight_one rest pos (Ref n :: acc)
      | String s :: rest ->
          let next (l, c) = function '\n' -> (l + 1, 0) | _ -> (l, c + 1) in
          let rec loop was_inside was_last p i acc pos =
            let tok p i =
              if was_inside then
                (if was_last then lbl else [])
                @ [ Class (cls, [ String (String.sub s p (i - p)) ]) ]
              else
                [ String (String.sub s p (i - p)) ] in
            if i = String.length s then
              tok p i @ acc, pos
            else if was_inside <> (inside pos loc) then
              let acc = if p < i then tok p i @ acc else acc in
              loop (not was_inside) was_last i i acc pos
            else
              loop was_inside (last pos loc) p (i + 1) acc (next pos (String.get s i)) in
          let toks, pos = loop false false 0 0 [] pos in
          hilight_one rest pos (toks @ acc) in
    fst (hilight_one pretty (1, 0) []) in
  List.fold_left hilight_one pretty locs

let advance_loc code loc =
  let rec loop i sl sc el ec =
    if i >= String.length code then
      { loc_start = (sl, sc) ; loc_end = (el, ec) }
    else
      let next l c =
        (* should work even with ignored '\n's *)
        if l > 1 then
          (if String.get code i = '\n' then l - 1 else l), c
        else
          1, max 0 (c - 1) in
      let sl, sc = next sl sc in
      let el, ec = next el ec in
      loop (i + 1) sl sc el ec in
  let { loc_start = (sl, sc) ; loc_end = (el, ec) } = loc in
  loop 0 sl sc el ec

let hilight cls output u locs lbl =
  match find_phrase output u with
  | None -> assert false
  | Some l ->
      let rec loop locs = function
        | Code (code, pretty, pre, _) :: rest ->
            pretty := hilight_pretty cls !pretty locs lbl ;
            Js_utils.Manip.replaceChildren pre (pretty_html !pretty) ;
            let locs = List.map (advance_loc code) locs in
            loop locs rest
        | _ :: rest -> loop locs rest
        | [] -> () in
      loop locs (List.rev !l)

let output_error ?phrase output error =
  let { Toploop_results.locs ; msg ; if_highlight } = error in
  let content =
    [ Tyxml_js.Html5.txt
        (match phrase with None -> msg | Some _ -> if_highlight) ] in
  let pre =
    Tyxml_js.Html5.(pre ~a: [ a_class [ "toplevel-error" ] ]) content in
  begin match phrase, locs with
    | None, _ | _, [] -> ()
    | Some u, _ -> hilight "toplevel-hilighted-error" output u locs []
  end ;
  insert output ?phrase (Error (error, pre)) pre

let output_warning ?phrase output warning =
  let { Toploop_results.locs ; msg ; if_highlight } = warning in
  match phrase, locs with
  | None, _ | _, [] ->
      let pre =
        Tyxml_js.Html5.(pre ~a: [ a_class [ "toplevel-warning" ] ]
                          [ txt msg ]) in
      insert output ?phrase (Warning (0, warning, pre)) pre
  | Some phrase, _ ->
      phrase.warnings <- phrase.warnings + 1 ;
      hilight "toplevel-hilighted-warning" output phrase locs
        [ Ref phrase.warnings ] ;
      let pre =
        Tyxml_js.Html5.(pre ~a: [ a_class [ "toplevel-warning" ] ]
                          [ span ~a: [ a_class [ "ref" ] ]
                              [ txt (string_of_int phrase.warnings) ] ;
                            txt ":" ;
                            txt if_highlight ]) in
      insert output ~phrase (Warning (phrase.warnings, warning, pre)) pre

let clear output =
  Js_utils.Manip.removeChildren output.container ;
  output.blocks <- []

let oldify output =
  List.iter
    (fun elt -> Js_utils.Manip.addClass elt "old")
    (Js_utils.Manip.children output.container) ;
  output.blocks <- []

let format_ocaml_code code =
  pretty_html (fst (prettify_ocaml code))

let get_blocks output =
  let rec process = function
    | [] -> ""
    | (Answer (s,_,_,_)) :: suite -> process suite ^ s
    | (Phrase (_,l)) :: suite -> process suite ^ (process (!l)) (* OK? *)
    | b :: suite->(process suite) in
  process output.blocks
