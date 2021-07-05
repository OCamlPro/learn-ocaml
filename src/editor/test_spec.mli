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

(** [question_typed] is used by [compile]
    @return the compiled string of a single question *)
val question_typed : ?num:int -> Learnocaml_data.Editor.test_qst_untyped -> string

(** [compile] is used by the "Generate" feature of the test.ml tab
    @return the compiled string of all given questions *)
val compile : (string * Learnocaml_data.Editor.test_qst_untyped list) list -> string
