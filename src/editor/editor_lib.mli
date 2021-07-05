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

open Learnocaml_data
open Editor
module H = Tyxml_js.Html

val update_index : Editor.editor_state -> unit

(** Getters of an editor exercise
 * @param the id *)
val get_editor_state : string -> Editor.editor_state
val get_titre : string -> string
val get_description : string -> string
val get_diff : string -> float
val get_solution : string -> string
val get_question : string -> string
val get_template : string -> string
val get_testml : string -> string
val get_prelude : string -> string
val get_prepare : string -> string

val with_test_lib_prepare :string->string

(** Remove an exercise from the local storage *)
val remove_exo : Map.Make(String).key -> unit

(** @return a bool depending on whether the id is already used or not *)
val idUnique : string -> bool
(** @return a bool depending on whether the title is already used or not *)
val titleUnique : string -> bool

val new_state : Exercise.Meta.t -> editor_state
(** arguments Dom element , string *)
val setInnerHtml : < innerHTML : < set : Js.js_string Js.t -> unit; .. >
                   Js_of_ocaml.Js.gen_prop; .. > Js_of_ocaml.Js.t -> string -> unit

(** Fragment of a test.ml code
  * @see definition *)
val init : string

(** Create the code of a section
  * @param name_of_the_function associated_report *)
val section : string -> string -> string

(* TODO: Remove commented code
(** @param content_of_the_toplevel [[]]
  * @return a list
  * The first value is the type of the first val, etc. *)
val get_all_val : char list -> char list list -> char list list

(** Remove atomic values from a list of types
  * @return a list of type of function (var function_name : type = <fun>)
  * @param content_of_the_toplevel [[]] result_list  (second parameter must be []) *)
val get_only_fct : char list -> char list -> char list

(** Associate each function with its type
  * @ return a list of couple (function_name, function_type)
  * @ param content_of_the_toplevel result_list (second param must be []) *)
val get_questions : char list list -> (string * string) list -> (string * string) list
 *)

(*
(** Create a list of triples (key, alea, "monorphic type"):
    polymorph_detector [("f", "'a -> 'b"); ("p", "int -> int")] =
    [("f", 5, "int -> bool"); ("f", 5, "bool -> char"); ("p", 10, "int -> int")] *)
val polymorph_detector : ('a * string) list -> ('a * int * string) list
 *)

(** [genTemplate top ?(on_err=fun()->()) sol]:
    evaluate the solution [sol] using the toplevel [top],
    generate then return a template string.
    Run also [on_err] if there is a typecheck error. *)
val genTemplate :
  Learnocaml_toplevel.t -> ?on_err:(unit -> unit) -> string -> string Lwt.t

(** [typecheck set_class ace editor top prelprep ?(mock=false) ?onpasterr code]:
    check if [code] (taken from buffer [ace, editor], with [prelprep]
    prepended and with test_lib mock code if [mock=true]) compiles,
    using toplevel [top]. Raise a CSS class if [set_class=true] (among
    "ocaml-check-success", "ocaml-check-warn", "ocaml-check-error").
    Run [onpasterr] if some error line of code occurs with [loc < 0].
*)
val typecheck :
  bool -> 'a Ace.editor -> Ocaml_mode.editor -> Learnocaml_toplevel.t ->
  string -> ?mock:bool -> ?onpasterr:(unit -> unit Lwt.t) -> string -> unit Lwt.t
(* Note: the type of ?onpasterr could be simplified, using more monadic style *)

(** Create an exercise with the data of the local storage
  * @param editor_exercise_id *)
val exo_creator : string -> Learnocaml_exercise.t

(** @return the output of toplevel buffer *) 
val get_answer : Learnocaml_toplevel.t -> string
val typecheck_dialog_box : string-> 'a Toploop_results.toplevel_result -> unit Lwt.t


(** Extract the function definitions from a toplevel output
    @return the list of their (name, type) *)
val extract_functions : string -> (string * string) list

(** Generate monomorphic test specifications
    @return a list of ("function_name", list_of_monomorphic_test_cases) *)
val monomorph_generator : (string * string) list -> (string * Editor.test_qst_untyped list) list

val show_load : Html_types.text Tyxml_js.Html.wrap ->
[< Html_types.div_content_fun ] Tyxml_js.Html.elt Tyxml_js.Html.list_wrap ->
unit

module Editor_io : sig
  val download : Learnocaml_data.SMap.key -> unit
  val upload : unit -> unit
  val download_all : unit -> unit
end

module Templates : sig
  val give_templates : unit -> Learnocaml_data.Editor.editor_template list
  val give_first_templates :
    unit -> Learnocaml_data.Editor.editor_template list
  val against_solution_template : Learnocaml_data.Editor.editor_template
  val test_suite_template : Learnocaml_data.Editor.editor_template
  val save : Learnocaml_data.Editor.editor_template list -> unit
  val init : unit -> unit
  val to_string : Learnocaml_data.Editor.editor_template list -> string
  val from_string : string -> Learnocaml_data.Editor.editor_template list
  val template_to_a_elt : 'a Ace.editor -> Learnocaml_data.Editor.editor_template ->
                          [> [> Html_types.pcdata ] Html_types.a ] H.elt
end

module Editor_components : sig

  val dropup :
    icon:string ->
    theme:string ->
    string H.wrap ->
    [< Html_types.div_content_fun ] H.elt H.list_wrap ->
    [> Html_types.div ] H.elt

  val editor_overlay : unit -> [> Html_types.div ] H.elt

  val ace_editor_container :
    save:(unit -> 'a) ->
    size:string * string ->
    editor:[< Html_types.div_content_fun > `Div `H3 ] H.elt ->
    box_title:string H.wrap ->
    box_header:string H.wrap -> [> Html_types.div ] H.elt

  val all_templates_container :
    size:string * string ->
    elements:[< `A of Html_types.flow5_without_interactive & 'a ] H.elt
             H.list_wrap ->
    box_title:string H.wrap ->
    box_header:[< Html_types.div_content_fun ] H.elt ->
    [> Html_types.div ] H.elt
end
