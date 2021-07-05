(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * The main authors of the editor part is the pfitaxel team see 
 * https://github.com/pfitaxel/learn-ocaml-editor for more information
 * 
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Editor_lib

let string_of_char ch = String.make 1 ch

let rec to_string_aux char_list =match char_list with
  | []-> ""
  | c::l -> (string_of_char c) ^ ( to_string_aux l)

let to_funty str = "[%funty: " ^ str ^ "]"

(* The tester arg could take into account exceptions/sorted lists/etc. *)
let question_typed ?num question =
  let open Learnocaml_data.Editor in
  let opt_string param = function
    | "" -> ""
    | v -> Format.sprintf " ~%s:(%s)" param v
  and sampler_args = function
    | "" -> ""
    | f -> Format.sprintf "fun () -> last ((%s) ())" f
  in
  let suffix = match num with None -> "" | Some n -> "_" ^ string_of_int n in
  match question with
  | TestAgainstSpec a ->
     (* FIXME *)
     "(* Question #" ^ " about " ^ a.name ^ " was not translated\n"
     ^ "(TestAgainstSpec not currently supported by the learn-ocaml runtime) *)"
  | TestSuite a ->
     let name, prot, tester, suite =
       a.name, to_funty a.ty, opt_string "test" a.tester, a.suite in
     (* Naming convention: [q_name], [q_name_1; q_name_2] (occurrence 1/3) *)
     Format.sprintf "let q_%s%s =@.  \
                     let prot = %s in@.  \
                     test_function%s prot@.  \
                     (lookup_student (ty_of_prot prot) %s)@.  \
                     %s;;@."
       name suffix prot tester name suite
  | TestAgainstSol a ->
     let name = a.name
     and prot = to_funty a.ty
     and gen = a.gen
     and sampler = opt_string "sampler" (sampler_args a.sampler)
     and tester = opt_string "test" a.tester
     and suite = a.suite
     in
     (* Naming convention: [q_name], [q_name_1; q_name_2] (occurrence 2/3) *)
     Format.sprintf "let q_%s%s =@.  \
                     let prot = %s in@.  \
                     test_function_against_solution ~gen:(%d)%s%s prot@.  \
                     \"%s\"@.  \
                     %s;;@."
     name suffix prot gen sampler tester name suite

(*****************)
(* compile stuff *)
(*****************)

let test_prel = "open Test_lib\nopen Learnocaml_report;;\n"

let quality_function = {|
let avoid_thentrue = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "* Do not write the following code patterns:";
                                   Code "[if ... then true else ...;
 if ... then false else ...;
 if ... then ... else true;
 if ... then ... else false]"; Text "
Preferably use Boolean operators (&&), (||), not."], Success ~-4) ]
  end

let check_thentrue e =
    Parsetree.(
      match e with
      | {pexp_desc = Pexp_ifthenelse (_, e1, (Some e2))} ->
         begin
           match e1 with
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "false")}, None)}
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "true")}, None)} ->
              avoid_thentrue e1
           | _ -> []
         end @ begin
           match e2 with
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "false")}, None)}
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "true")}, None)} ->
             avoid_thentrue e2
           | _ -> []
          end
      | _ -> [])

let avoid_list1app = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "* Do not write:";
                                   Code "[x] @ l";
                                   Text ". Preferably write:";
                                   Code "x :: l";
                                   Text "."], Success ~-4) ]
  end

let check_list1app e =
  Parsetree.(
    match e.pexp_desc with
    | Pexp_apply (app0, [(_, lst1); _]) ->
       (match app0.pexp_desc, lst1.pexp_desc with
        | Pexp_ident {Asttypes.txt = app0'},
          Pexp_construct ({Asttypes.txt = (Longident.Lident "::")}, Some lst1')
             when List.mem (Longident.flatten app0') [["List"; "append"]; ["@"]] ->
           (match lst1'.pexp_desc with
            | Pexp_tuple [_; nil0] ->
               (match nil0.pexp_desc with
                | Pexp_construct ({Asttypes.txt = (Longident.Lident "[]")}, None) ->
                   avoid_list1app e
                | _ -> [])
            | _ -> [])
        | _ -> [])
    | _ -> [])

let avoid_eqphy = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "*  Do not use physical equality";
                                   Code "(==)";
                                   Text ". Preferably use structural equality";
                                   Code "(=)";
                                   Text "."], Success ~-1) ]
  end

let avoid_neqphy = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "* Do not use physical inequality";
                                   Code "(!=)";
                                   Text ". Preferably use structural inequality";
                                   Code "(<>)";
                                   Text "."], Success ~-1) ]
  end

let check_eqphy e =
  Parsetree.(
    match e.pexp_desc with
    | Pexp_ident {Asttypes.txt = Longident.Lident "=="} -> avoid_eqphy e
    | _ -> [])

let check_neqphy e =
  Parsetree.(
    match e.pexp_desc with
    | Pexp_ident {Asttypes.txt = Longident.Lident "!="} -> avoid_neqphy e
    | _ -> [])
|}

let imperative_function = {|let ast_imperative_check ast =
  let chk_expr e =
    Parsetree.(
      match e with
      | {pexp_desc = Pexp_sequence _} -> forbid_syntax ";" e
      | {pexp_desc = Pexp_while _} -> forbid_syntax "while" e
      | {pexp_desc = Pexp_for _} -> forbid_syntax "for" e
      | {pexp_desc = Pexp_array _} -> forbid_syntax "array" e
      | _ -> [] ) in
  let imperative_report =
    ast_check_structure
      ~on_expression:chk_expr
      ast |> List.sort_uniq compare in
  if snd (Learnocaml_report.result imperative_report) then
    imperative_report
  else
    []|}


let ast_fonction quality imperative  =
  let fonction = if quality then
                   quality_function
                 else
                   "" in
  let fonction = if imperative then
                   fonction ^ imperative_function
                 else
                   fonction ^ "" in
  let fonction = fonction ^ "\n\nlet ast_quality ast =" in
  let fonction =
    if imperative then
      fonction ^ {|
                  let imperative_report =
                  let tempReport = ast_imperative_check ast in
                  if tempReport = [] then []
                  else (Message ([ Text "Imperative features have been detected:" ],
                  Success ~-4)) :: tempReport
                  |}
    else
      fonction ^ {|
                  let imperative_report = []
                  |} in
  let fonction =
    if quality then
      fonction ^ {|
                  and report =
                  let tempReport = ast_check_structure
                  ~on_expression:(check_thentrue @@@ check_list1app @@@
                  check_eqphy @@@ check_neqphy)
                  ast |> List.sort_uniq compare in
                  if tempReport = [] then []
                  else (Message ([Text "Unwanted code patterns have been detected:"],
                  Failure)) :: tempReport
                  |}
    else fonction ^ " and report = []" in
  let fonction = fonction ^ {|
                             in if imperative_report = [] && report = []
                             then [ Message ([ Text "OK (no prohibited construction detected)"], Important) ]
                             else imperative_report @ report;;

                             |} in
  fonction

let ast_code quality imperative =
let fonction =
    if quality || imperative then
      {|Section ([ Text "Code quality:" ], ast_quality code_ast);
       |}
    else
      "" in
  fonction

(* Naming convention: [q_name], [q_name_1; q_name_2] (occurrence 3/3) *)
(* [cat_question "foo" [42] = "q_foo"]
   [cat_question "foo" [42; 42; 42] = "q_foo_1 @ q_foo_2 @ q_foo_3"] *)
let cat_question name list_qst =
  match list_qst with
  | [] -> invalid_arg "cat_question"
  | [_] -> "q_" ^ name
  | _q :: ((_ :: _) as l) ->
     List.fold_left (fun (i, acc) _e ->
         (i + 1, acc ^" @ q_"^ name ^ "_" ^ string_of_int i))
       (2, "q_" ^ name ^ "_1") l
     |> snd

(* TODO/FIXME: Add some "let () = set_progress ..." lines *)
let compile indexed_list =
  let tests = test_prel ^ (ast_fonction true true) in
  let tests = List.fold_left (fun acc (_name, list_qst) ->
                  acc ^
                  if List.length list_qst > 1 then
                    List.fold_left (fun (i, acc) qst ->
                        (i + 1, acc ^ question_typed ~num:i qst ^" \n"))
                      (1, "") list_qst
                    |> snd
                  else List.fold_left (fun acc qst ->
                           acc ^ question_typed qst ^" \n")
                         "" list_qst)
                tests indexed_list in
  let tests = tests ^ init ^ "[\n" ^ ast_code true true in
  let tests =
    List.fold_left (fun acc (name, list_qst) ->
        acc ^ section name (cat_question name list_qst))
      tests indexed_list in
  tests ^ " ]"

(*
module Generate_1 = struct
  open Tyxml_js.Html5
  open Learnocaml_common
  open Learnocaml_data
  open Lwt.Infix
     
  let ask_name top =
    ask_string ~title:"Name" [ pcdata "Enter the function name"] >>=
      (fun name ->
        let questions = extract_functions (get_answer top) in
        match  List.assoc_opt name questions  with
        | None -> Learnocaml_common.alert "No question with this name found"; Lwt.return None
        | Some s -> Lwt.return (Some (name,s))
      )
    
  let generate_against_solution tuple_opt =
    match tuple_opt with
    | None -> ""
    | Some (name, ty) ->  let question =
                            Editor.TestAgainstSol
                              {name = name; ty;
                               gen=10; suite= "[1 @:!! 4 ; 3 @:!! 3 ]";
                               tester = ""; sampler = ""}
                          in
                          question_typed question

  let generate_test_suite tuple_opt =
    match tuple_opt with
    | None -> ""
    | Some (name, ty) ->  let question =
                            Editor.TestSuite
                              {name = name; ty;
                               gen=10; suite= "[1 @:!! 4 ; 3 @:!! 3 ]";
                               tester = ""; sampler = ""}
                          in
                          question_typed question
                           
                                 
end
            
 *)
