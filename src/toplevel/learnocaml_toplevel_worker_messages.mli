(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Types of the messages exchanged with a toplevel in a Web Worker. *)

open Js_of_ocaml

type _ host_msg =
  | Init : unit host_msg
  | Reset : unit host_msg
  | Execute : int option * bool * int * string -> bool host_msg
  | Use_string : string option * bool * int * string -> bool host_msg
  | Use_mod_string : int * bool * string * string option * string -> bool host_msg
  | Set_debug : bool -> unit host_msg
  | Register_callback : string * int -> unit host_msg
  | Set_checking_environment : unit host_msg
  | Check : string * bool -> unit host_msg
  
type _ msg_ty =
  | Unit : unit msg_ty
  | Bool : bool msg_ty
  | Int : int msg_ty
  | String : Js.js_string Js.t msg_ty

type (_, _) eq = Eq : ('a, 'a) eq

type toploop_msg =
  | Write : int * string -> toploop_msg (* pseudo file descriptor * content *)
  | ReturnSuccess :
      int * 'a msg_ty * 'a * Toploop_results.warning list -> toploop_msg
  | ReturnError :
      int * Toploop_results.error * Toploop_results.warning list -> toploop_msg
