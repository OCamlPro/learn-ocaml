(* This file is part of Learn-OCaml
 *
 * Copyright (C) 2020 Alban Gruin.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details.  *)

module Js = Js_of_ocaml.Js

class type learnocaml_config = object
  method enableTryocaml: bool Js.optdef_prop
  method enableLessons: bool Js.optdef_prop
  method enableExercises: bool Js.optdef_prop
  method enableToplevel: bool Js.optdef_prop
  method enablePlayground: bool Js.optdef_prop
  method txtLoginWelcome: Js.js_string Js.t Js.optdef_prop
  method txtNickname: Js.js_string Js.t Js.optdef_prop
  method baseUrl: Js.js_string Js.t Js.optdef_prop
end

let config : learnocaml_config Js.t = Js.Unsafe.js_expr "learnocaml_config"
let api_server = Js.(to_string (Optdef.get config##.baseUrl (fun () -> string "")))
