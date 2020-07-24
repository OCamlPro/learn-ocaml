(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Used both for file i/o and request handling *)
module Json_codec: Learnocaml_api.JSON_CODEC

module type IndexRW = sig
  type t

  val init : unit -> t
  val read : string -> (string -> 'a) -> 'a Lwt.t
  val write : t -> string -> ('a -> string) -> 'a -> unit Lwt.t
end

module IndexFile: IndexRW

module TokenIndex: sig
  (** Create or regenerate token index from sync/ and write sync/token.json.
      This step may take a long time (up to several minutes).  Automatically
      called (once and for all) by [get_tokens] or [add_token] if need be.
      The first argument denotes the sync directory path. *)
  val create_index : string -> unit Lwt.t

  (** Get the list of all tokens. *)
  val get_tokens : string -> Learnocaml_data.Token.t list Lwt.t

  (** Add a registered token in the index. *)
  val add_token : string -> Learnocaml_data.Token.t -> unit Lwt.t
end

module MoodleIndex: sig
  val create_index : string -> unit Lwt.t

  val add_user : string -> string -> Learnocaml_data.Token.t -> unit Lwt.t

  (** Get a Moodle user's token, create it if not exist *)
  val get_user_token : string -> string -> Learnocaml_data.Token.t Lwt.t

  val get_users : string -> (string * Learnocaml_data.Token.t) list Lwt.t
  val user_exists : string -> string -> bool Lwt.t
end

module OauthIndex: sig
  val create_index : string -> string Lwt.t

  val get_first_oauth : string -> (string * string list) Lwt.t
  val get_current_secret : string -> string Lwt.t

  (** Delete all secrets + nonce associated excepted the current secret with its nonces *)
  val purge : string -> unit Lwt.t
end

val check_oauth : string -> string -> (string * string) list -> (string, string) result Lwt.t

type authentication =
  | AuthToken of Learnocaml_data.Token.t
  | Passwd of (string * string)

module UserIndex: sig
  val create_index : string -> Learnocaml_data.Token.t list -> unit Lwt.t
  val authenticate : string -> authentication -> Learnocaml_data.Token.t option Lwt.t
  val exists : string -> string -> bool Lwt.t
  val add : string -> Learnocaml_data.Token.t -> (string * string) option -> unit Lwt.t
  val upgrade : string -> Learnocaml_data.Token.t -> string -> string -> unit Lwt.t
  val can_login : string -> Learnocaml_data.Token.t -> bool Lwt.t
end