(* This file is part of Learn-OCaml
 *
 * Copyright (C) 2020 Alban Gruin.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details.  *)

(** This is not transpiled to learnocaml-static.js, but is an interface
   to the values stored in this file.  It is "statically linked" with
   learnocaml-common.ml. *)

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

val config : learnocaml_config Js.t
val api_server : string
