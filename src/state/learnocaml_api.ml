(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data

let version = Learnocaml_version.v

type _ request =
  | Static:
      string list -> string request
  | Version:
      unit -> (string * int) request
  | Nonce:
      unit -> string request
  | Create_token:
      string * student token option * string option -> student token request
  | Create_teacher_token:
      teacher token -> teacher token request
  | Fetch_save:
      'a token -> Save.t request
  | Archive_zip:
      'a token -> string request
  | Update_save:
      'a token * Save.t -> Save.t request
  | Git: 'a token * string list -> string request

  | Students_list:
      teacher token -> Student.t list request
  | Set_students_list:
      teacher token * (Student.t * Student.t) list -> unit request
  | Students_csv:
      teacher token * Exercise.id list * Token.t list -> string request

  | Exercise_index:
      'a token option -> (Exercise.Index.t * (Exercise.id * float) list) request
  | Exercise:
      'a token option * string -> (Exercise.Meta.t * Exercise.t * float option) request

  | Lesson_index:
      unit -> (string * string) list request
  | Lesson:
      string -> Lesson.t request

  | Tutorial_index:
      unit -> Tutorial.Index.t request
  | Tutorial:
      string -> Tutorial.t request

  | Playground_index:
      unit -> Playground.Index.t request
  | Playground:
      string -> Playground.t request

  | Exercise_status_index:
      teacher token -> Exercise.Status.t list request
  | Exercise_status:
      teacher token * Exercise.id -> Exercise.Status.t request
  | Set_exercise_status:
      teacher token * (Exercise.Status.t * Exercise.Status.t) list -> unit request

  | Partition:
      teacher token * Exercise.id * string * int -> Partition.t request

  | Invalid_request:
      string -> string request


type http_request = {
  meth: [ `GET | `POST of string];
  path: string list;
  args: (string * string) list;
}

module J = Json_encoding

module type JSON_CODEC = sig
  val decode: 'a J.encoding -> string -> 'a
  val encode: ?minify:bool -> 'a J.encoding -> 'a -> string
end

module Conversions (Json: JSON_CODEC) = struct

  let response_codec
    : type resp.
      resp request -> (resp -> string) * (string -> resp)
    = fun req ->
      let str = (fun x -> x), (fun x -> x) in
      let json enc = (Json.encode enc), (Json.decode enc) in
      let ( +> ) (cod, decod) (cod', decod') =
        (fun x -> cod (cod' x)),
        (fun s -> decod' (decod s))
      in
      match req with
      | Static _ -> str
      | Version _ -> json J.(obj2 (req "version" string) (req "server_id" int))
      | Nonce _ -> json J.(obj1 (req "nonce" string))
      | Create_token _ ->
          json J.(obj1 (req "token" string)) +>
          Token.(to_string, parse)
      | Create_teacher_token _ ->
          json J.(obj1 (req "token" string)) +>
          Token.(to_string, parse)
      | Fetch_save _ ->
          json Save.enc
      | Archive_zip _ ->
          str
      | Update_save _ ->
          json Save.enc
      | Git _ -> str
      | Students_list _ ->
          json (J.list Student.enc)
      | Set_students_list _ ->
          json J.unit
      | Students_csv _ ->
          str
      | Exercise_index _ ->
         json (J.tup2 Exercise.Index.enc (J.assoc J.float))
      | Exercise _ ->
          json (J.tup3 Exercise.Meta.enc Exercise.enc (J.option J.float))
      | Lesson_index _ ->
          json Lesson.Index.enc
      | Lesson _ ->
          json Lesson.enc
      | Tutorial_index _ ->
          json Tutorial.Index.enc
      | Tutorial _ ->
         json Tutorial.enc
      | Playground_index _ ->
          json Playground.Index.enc
      | Playground _ ->
          json Playground.enc

      | Exercise_status_index _ ->
          json (J.list Exercise.Status.enc)
      | Exercise_status _ ->
          json Exercise.Status.enc
      | Set_exercise_status _ ->
          json J.unit

      | Partition _ ->
          json Partition.enc

      | Invalid_request _ ->
          str

  let response_encode r = fst (response_codec r)
  let response_decode r = snd (response_codec r)


  let to_http_request
    : type resp. resp request -> http_request
    =
    let get ?token path = {
      meth = `GET;
      path;
      args = match token with None -> [] | Some t -> ["token", Token.to_string t];
    } in
    let post ~token path body = {
      meth = `POST body;
      path;
      args = ["token", Token.to_string token];
    } in
    function
    | Static path ->
        get path
    | Version () ->
        get ["version"]

    | Nonce () ->
        get ["nonce"]
    | Create_token (secret_candiate, token, nick) ->
        get ?token (["sync"; "new"; secret_candiate] @
                    (match nick with None -> [] | Some n -> [n]))
    | Create_teacher_token token ->
        assert (Token.is_teacher token);
        get ~token ["teacher"; "new"]

    | Fetch_save token ->
        get ~token ["save.json"]
    | Archive_zip token ->
        get ~token ["archive.zip"]
    | Update_save (token, save) ->
        post ~token ["sync"] (Json.encode Save.enc save)
    | Git _ ->
        assert false (* Reserved for the [git] client *)

    | Students_list token ->
        assert (Token.is_teacher token);
        get ~token ["teacher"; "students.json"]
    | Set_students_list (token, students) ->
        assert (Token.is_teacher token);
        post ~token
          ["teacher"; "students.json"]
          (Json.encode (J.list (J.tup2 Student.enc Student.enc)) students)
    | Students_csv (token, exercises, students) ->
        assert (Token.is_teacher token);
        post ~token ["teacher"; "students.csv"]
          (Json.encode
             (J.obj2
                (J.dft "exercises" (J.list J.string) [])
                (J.dft "students" (J.list Token.enc) []))
             (exercises, students))

    | Exercise_index (Some token) ->
       get ~token ["exercise-index.json"]
    | Exercise_index None ->
       get ["exercise-index.json"]

    | Exercise (Some token, id) ->
       get ~token ("exercises" :: String.split_on_char '/' (id^".json"))
    | Exercise (None, id) ->
       get ("exercises" :: String.split_on_char '/' (id^".json"))

    | Lesson_index () ->
        get ["lessons.json"]
    | Lesson id ->
       get ["lessons"; id^".json"]

    | Playground_index () ->
        get ["playgrounds.json"]
    | Playground id ->
        get ["playgrounds"; id^".json"]

    | Tutorial_index () ->
        get ["tutorials.json"]
    | Tutorial id ->
        get ["tutorials"; id^".json"]

    | Exercise_status_index token ->
        assert (Token.is_teacher token);
        get ~token ["teacher"; "exercise-status.json"]
    | Exercise_status (token, id) ->
        get ~token
          ("teacher" :: "exercise-status" :: String.split_on_char '/' id)
    | Set_exercise_status (token, status) ->
        post ~token
          ["teacher"; "exercise-status"]
          (Json.encode
             (J.list (J.tup2 Exercise.Status.enc Exercise.Status.enc))
             status)

    | Partition (token, eid, fid, prof) ->
        get ~token
          ["partition"; eid; fid; string_of_int prof]

    | Invalid_request s ->
        failwith ("Error request "^s)

end

module type REQUEST_HANDLER = sig
  type 'resp ret
  val map_ret: ('a -> 'b) -> 'a ret -> 'b ret

  val callback: Conduit.endp ->
                Learnocaml_data.Server.config -> 'resp request -> 'resp ret
end

module Server (Json: JSON_CODEC) (Rh: REQUEST_HANDLER) = struct

  module C = Conversions(Json)

  let rec last =
    function [f] -> Some f | [] -> None | _::r -> last r

  let handler conn config request =
      let k req =
        Rh.callback conn config req |> Rh.map_ret (C.response_encode req)
      in
      let token =
        match List.assoc_opt "token" request.args with
        | None -> None
        | Some stoken ->
            try Some (Token.parse stoken)
            with Failure _ -> None
      in
      match request.meth, request.path, token with
      | `GET, ([] | [""]), _ ->
          Static ["index.html"] |> k
      | `GET, ["version"], _ ->
          Version () |> k

      | `GET, ["nonce"], _ ->
          Nonce () |> k
      | `GET, ["sync"; "new"; secret_candidate], token ->
          Create_token (secret_candidate, token, None) |> k
      | `GET, ["sync"; "new"; secret_candidate; nick], token ->
          Create_token (secret_candidate, token, Some nick) |> k
      | `GET, ["teacher"; "new"], Some token when Token.is_teacher token ->
          Create_teacher_token token |> k

      | `GET, ["save.json"], Some token ->
          Fetch_save token |> k
      | `GET, ["archive.zip"], Some token ->
          Archive_zip token |> k
      | `POST body, ["sync"], Some token ->
          (match Json.decode Save.enc body with
           | save -> Update_save (token, save) |> k
           | exception e -> Invalid_request (Printexc.to_string e) |> k)
      | `GET, (stoken::"learnocaml-workspace.git"::p), None ->
          (match Token.parse stoken with
           | token -> Git (token, p) |> k
           | exception Failure e -> Invalid_request e |> k)

      | `GET, ["teacher"; "students.json"], Some token
        when Token.is_teacher token ->
          Students_list token |> k
      | `POST body, ["teacher"; "students.json"], Some token
        when Token.is_teacher token ->
          (match Json.decode (J.list (J.tup2 Student.enc Student.enc)) body with
           | students -> Set_students_list (token, students) |> k
           | exception e -> Invalid_request (Printexc.to_string e) |> k)
      | `GET, ["teacher"; "students.csv"], Some token
        when Token.is_teacher token ->
          Students_csv (token, [], []) |> k
      | `POST body, ["teacher"; "students.csv"], Some token
        when Token.is_teacher token ->
          (match Json.decode
                   (J.obj2
                      (J.dft "exercises" (J.list J.string) [])
                      (J.dft "students" (J.list Token.enc) []))
                   body
           with
           | exercises, students ->
               Students_csv (token, exercises, students) |> k
           | exception e -> Invalid_request (Printexc.to_string e) |> k)

      | `GET, ["exercise-index.json"], token ->
         Exercise_index token |> k
      | `GET, ("exercises"::path), token ->
          (match last path with
           | Some s when String.lowercase_ascii (Filename.extension s) = ".json" ->
               (match token with
                | Some token ->
                    let id = Filename.chop_suffix (String.concat "/" path) ".json" in
                    Exercise (Some token, id) |> k
                | None -> Invalid_request "Missing token" |> k)
           | Some "" ->
               Static ["exercise.html"] |> k
           | _ ->
              Static ("static"::path) |> k)
      | `GET, ("description"::_), _token ->
         (* match token with
          | None -> Invalid_request "Missing token" |> k *)
          Static ["description.html"] |> k
      | `GET, ("playground"::path), _token ->
         begin
           match last path with
           | Some s when String.lowercase_ascii (Filename.extension s) = ".json" ->
              let id = Filename.chop_suffix (String.concat "/" path) ".json" in
              Playground id |> k
           | Some "" ->
              Static ["playground.html"] |> k
           | _ ->
              Static ("static"::path) |> k
         end
      | `GET, ["lessons.json"], _ ->
          Lesson_index () |> k
      | `GET, ["lessons"; f], _ when Filename.check_suffix f ".json" ->
          Lesson (Filename.chop_suffix f ".json") |> k

      | `GET, ["tutorials.json"], _ ->
          Tutorial_index () |> k
      | `GET, ["tutorials"; f], _ when Filename.check_suffix f ".json" ->
         Tutorial (Filename.chop_suffix f ".json") |> k

      | `GET, ["playgrounds.json"], _ ->
          Playground_index () |> k
      | `GET, ["playgrounds"; f], _ when Filename.check_suffix f ".json" ->
          Playground (Filename.chop_suffix f ".json") |> k

      | `GET, ["partition"; eid; fid; prof], Some token
        when Token.is_teacher token ->
          Partition (token, eid, fid, int_of_string prof) |> k

      | `GET, ["teacher"; "exercise-status.json"], Some token
        when Token.is_teacher token ->
          Exercise_status_index token |> k
      | `GET, ("teacher" :: "exercise-status" :: id), Some token
        when Token.is_teacher token ->
          Exercise_status (token, String.concat "/" id) |> k
      | `POST body, ["teacher"; "exercise-status"], Some token
        when Token.is_teacher token ->
          (match Json.decode
                   (J.list (J.tup2 Exercise.Status.enc Exercise.Status.enc))
                   body
           with
           | status ->
               Set_exercise_status (token, status) |> k
           | exception e -> Invalid_request (Printexc.to_string e) |> k)

      | `GET,
        ( ["index.html"]
        | ["exercise.html"]
        | ["playground.html"]
        | ["student-view.html"]
        | ["description.html"]
        | ["new_exercise.html"]
        | ["editor.html"]
        | ["partition-view.html"]
        | ("js"|"fonts"|"icons"|"css"|"static") :: _ as path),
        _ ->
          Static path |> k

      | `GET, ["favicon.ico"], _ ->
          Static ["icons"; "favicon.ico"] |> k

      | meth, path, _ ->
          Invalid_request
            (Printf.sprintf "%s /%s%s"
               (match meth with `GET -> "GET" | `POST _ -> "POST")
               (String.concat "/" path)
               (match request.args with [] -> "" | l ->
                   "?" ^ String.concat "&"
                     (List.map (fun (k, v) -> k ^"="^ v) l)))
          |> k

end

module Client (Json: JSON_CODEC) = struct

  open Lwt.Infix

  module C = Conversions(Json)

  let make_request
    : type resp.
      (http_request -> (string, 'b) result Lwt.t) ->
      resp request -> (resp, 'b) result Lwt.t
    = fun send req ->
      let http_request = C.to_http_request req in
      send http_request >|= function
      | Ok str -> Ok (C.response_decode req str)
      | Error e -> Error e

end

(*
let client: type resp. resp request -> resp result = fun req ->

  let query_enc =
 function
  | Static str as req -> Server_caller.fetch (path req) |> query
*)

(* let server: meth * string list * string -> _ request = function
 *   | `GET, [] -> Static "index.json"
 *   | `GET, ["sync"; "gimme"] -> Create_token ()
 *   | `GET, ["sync"; token] -> Fetch_save token
 *   | `POST, ["sync"; token] -> *)
