(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Learnocaml_data

val find_div_or_append_to_body : string -> [> Html_types.div ] Tyxml_js.Html.elt

val find_component : string -> 'a Tyxml_js.Html.elt

val gettimeofday : unit -> float

val fake_download : name: string -> contents: Js.js_string Js.t -> unit

val fake_upload : unit -> (string * Js.js_string Js.t ) Lwt.t

val fatal : ?title: string -> string -> unit

val alert : ?title: string -> ?buttons: Html_types.div_content Tyxml_js.Html.elt list -> string -> unit

val ext_alert :
  title: string ->
  ?buttons: Html_types.div_content_fun Tyxml_js.Html.elt list ->
  [< Html_types.div_content ] Tyxml_js.Html.elt list ->
  unit

val lwt_alert :
  title: string ->
  buttons: (string * (unit -> 'a Lwt.t)) list ->
  [< Html_types.div_content ] Tyxml_js.Html.elt list ->
  'a Lwt.t

val confirm :
  title: string ->
  ?ok_label: string -> ?cancel_label: string ->
  [< Html_types.div_content ] Tyxml_js.Html.elt list ->
  (unit -> unit) -> unit

val ask_string :
  title: string ->
  ?ok_label: string ->
  [< Html_types.div_content > `Input] Tyxml_js.Html.elt list ->
  string Lwt.t

val catch_with_alert : ?printer: (exn -> string) -> (unit -> unit Lwt.t) -> unit Lwt.t

val hide_loading : ?id: string -> unit -> unit

val show_loading :
  ?id: string -> [< Html_types.div_content_fun ] Tyxml_js.Html.elt list ->
  (unit -> 'a Lwt.t) -> 'a Lwt.t

val set_assoc : string -> 'a -> (string * 'a) list -> (string * 'a) list

val delete_assoc : string -> (string * 'a) list -> (string * 'a) list

val arg : string -> string

val set_arg : string -> string -> unit

val delete_arg : string -> unit

type button_group

val button_group : unit -> button_group

type button_state

val button_state : unit -> button_state

val disable_button_group : button_group -> unit

val enable_button_group : button_group -> unit

val button_group_disabled : button_group -> bool

val disable_button : button_state -> unit

val enable_button : button_state -> unit

val disabling_button_group : button_group -> (unit -> unit Lwt.t) -> unit Lwt.t

val disable_with_button_group :
  < disabled : bool Js.t Js.prop ; .. > Js.t ->
  button_group -> unit

val button :
  container: 'a Tyxml_js.Html.elt ->
  theme: string ->
  ?group: button_group ->
  ?state: button_state ->
  icon:string ->
  string -> (unit -> unit Lwt.t) ->
  unit

val dropdown :
  id: string ->
  title: [< Html_types.button_content_fun > `PCDATA] Tyxml_js.Html.elt list ->
  [< Html_types.div_content_fun ] Tyxml_js.Html.elt list ->
  [> Html_types.div ] Tyxml_js.Html.elt

val render_rich_text :
  ?on_runnable_clicked: (string -> unit) ->
  Learnocaml_data.Tutorial.text ->
  [< Html_types.phrasing > `Code `Em `PCDATA ] Tyxml_js.Html.elt list

val extract_text_from_rich_text : Learnocaml_data.Tutorial.text -> string

(** Sets the local storage from the data in a save file *)
val set_state_from_save_file :
  ?token:Token.t -> Save.t -> unit

(** Gets a save file containing the locally stored data *)
val get_state_as_save_file : ?include_reports:bool -> unit -> Save.t

(** Sync the local save state with the server state, and returns the merged save
    file. The save will be created on the server if it doesn't exist.

    This syncs student {b,content}, but never the reports which are only synched
    on "Grade" *)
val sync: Token.t -> Save.t Lwt.t

(** The same, but limiting the submission to the given exercise, using the given
    answer if any, and the given editor text, if any. *)
val sync_exercise:
  Token.t option -> ?answer:Learnocaml_data.Answer.t -> ?editor:string ->
  Learnocaml_data.Exercise.id ->
  Save.t Lwt.t

val countdown:
  ?ontimeout: (unit -> unit) -> 'a Tyxml_js.Html5.elt -> float -> unit

val string_of_seconds: int -> string

val flog: ('a, unit, string, unit) format4 -> 'a

val stars_div: float -> [> Html_types.div ] Tyxml_js.Html5.elt

(** Returns an HTML string expected to be put in an iframe *)
val exercise_text:
  Exercise.Meta.t -> Exercise.t -> string

val string_of_exercise_kind: Exercise.Meta.kind -> string

val get_assignments:
  Token.Set.t -> Exercise.Status.t SMap.t ->
  ((float * float) * Token.Set.t * bool * SSet.t) list

(** Returns a CSS color from a grade
    (red for 0, green for 100, grey for None) *)
val grade_color: int option -> string

val string_of_date: ?time:bool -> float -> string

val date: ?time:bool -> float -> [> Html_types.time ] Tyxml_js.Html5.elt

val tag_span: string -> [> Html_types.span ] Tyxml_js.Html5.elt

(** A protected call to Server_caller.request *)
val retrieve: ?ignore:'a -> 'a Learnocaml_api.request -> 'a Lwt.t

val get_worker_code: string -> (unit -> string Lwt.t)

val set_string_translations_exercises : unit -> unit
val set_string_translations_view : unit -> unit

val local_save : 'a Ace.editor -> string -> unit

val toplevel_launch :
  ?display_welcome:bool ->
  ?after_init:(Learnocaml_toplevel.t -> unit Lwt.t) ->
  ?on_disable:(unit -> unit) ->
  ?on_enable:(unit -> unit) ->
  [ `Div ] Tyxml_js.Html5.elt ->
  (string ->
   Learnocaml_toplevel_history.snapshot
     Learnocaml_local_storage.storage_key) ->
  (unit -> unit) ->
  button_group -> string -> Learnocaml_toplevel.t Lwt.t

val mouseover_toggle_signal : 'a Tyxml_js.Html5.elt -> 'b -> ('b option -> unit) -> unit

val ace_display :
  [< Html_types.div ] Tyxml_js.To_dom.elt -> (string -> unit) * (unit -> unit)

val init_toplevel_pane :
  Learnocaml_toplevel.t Lwt.t ->
  Learnocaml_toplevel.t ->
  button_group ->
  (icon:string ->
   string -> (unit -> unit Lwt.t) ->
   unit) ->
  unit

val run_async_with_log : (unit -> 'a Lwt.t) -> unit

val mk_tab_handlers : string -> string list -> (unit -> unit) * (string -> unit)

module type Editor_info = sig
  val ace : Ocaml_mode.editor Ace.editor
  val buttons_container : 'a Tyxml_js.Html5.elt
end

module Editor_button (E : Editor_info) : sig
  val cleanup : string -> unit
  val download : string -> unit
  val eval : Learnocaml_toplevel.t -> (string -> 'a) -> unit
  val sync : Token.t option Lwt.t -> Learnocaml_data.SMap.key -> unit
end

val setup_editor : string -> Ocaml_mode.editor * Ocaml_mode.editor Ace.editor

val typecheck :
  Learnocaml_toplevel.t ->
  'a Ace.editor -> Ocaml_mode.editor -> bool -> unit Lwt.t

val set_nickname_div : unit -> unit

val setup_prelude_pane : 'a Ace.editor -> string -> unit

val get_token : ?has_server:bool -> unit -> Learnocaml_data.student Learnocaml_data.token option Lwt.t

module Display_exercise :functor
  (Q : sig
         val exercise_link :
           ?cl:string list ->
           string ->
           'a Tyxml_js.Html.elt list ->
           [> 'a Html_types.a ] Tyxml_js.Html.elt
       end) ->
  sig
    val display_descr :
      Learnocaml_data.Exercise.Meta.t ->
      [> Html_types.div ] Tyxml_js.Html5.elt
    val display_stars :
      Learnocaml_data.Exercise.Meta.t ->
      [> Html_types.div ] Tyxml_js.Html5.elt
    val display_kind :
      Learnocaml_data.Exercise.Meta.t ->
      [> Html_types.div ] Tyxml_js.Html5.elt
    val display_exercise_meta :
      string -> Learnocaml_data.Exercise.Meta.t -> string -> unit Lwt.t
    val display_list :
      ?sep:([> Html_types.pcdata ] as 'a) Tyxml_js.Html5.elt ->
      'a Tyxml_js.Html5.elt list -> 'a Tyxml_js.Html5.elt list
    val get_skill_index :
      'a Learnocaml_data.token ->
      [< `Focus of Learnocaml_data.SMap.key
       | `Requirements of Learnocaml_data.SMap.key ] ->
      Learnocaml_data.SSet.elt list Lwt.t
    val display_skill_meta :
      'a -> string Tyxml_js.Html5.wrap list -> string -> unit Lwt.t
    val display_link :
      (string -> 'a) ->
      string ->
      string Tyxml_js.Html5.wrap -> [> Html_types.div ] Tyxml_js.Html5.elt
    val display_skill_link :
      string Tyxml_js.Html5.wrap list ->
      string ->
      [< `Focus of string Tyxml_js.Html5.wrap
       | `Requirements of string Tyxml_js.Html5.wrap ] ->
      [> Html_types.div ] Tyxml_js.Html5.elt
    val display_exercise_link :
      string ->
      Learnocaml_data.Exercise.Meta.t ->
      string Tyxml_js.Html5.wrap -> [> Html_types.div ] Tyxml_js.Html5.elt
    val display_authors :
      string ->
      (string Tyxml_js.Html5.wrap * string Tyxml_js.Html5.wrap) list ->
      [> `PCDATA | `Span ] Tyxml_js.Html5.elt list
    val display_meta :
      'a Learnocaml_data.token option ->
      Learnocaml_data.Exercise.Meta.t -> string -> unit Lwt.t
  end

module Grade_exercise : sig
  val get_grade :
    ?callback:(string -> unit) ->
    ?timeout:float ->
    Learnocaml_exercise.t ->
    (string -> (Learnocaml_report.t * string * string * string) Lwt.t) Lwt.t
  val display_report :
    Learnocaml_exercise.t -> Learnocaml_data.Report.t -> int
end
