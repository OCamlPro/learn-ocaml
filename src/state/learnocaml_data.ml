(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

module J = Json_encoding

module SMap = struct

  include Map.Make (String)

  let enc val_enc =
    J.conv
      bindings
      (List.fold_left
         (fun acc (n, v) -> add n v acc)
         empty)
      (J.assoc val_enc)

end

module SSet = struct

  include Set.Make (String)

  let enc = J.conv elements of_list (J.list J.string)

  let merge3 ~ancestor ~theirs ~ours =
    let (++), (--), (%%) = union, diff, inter in
    (ancestor %% theirs %% ours) ++ (theirs -- ancestor) ++ (ours -- ancestor)

end

module Answer = struct

  type t =
    { solution : string ;
      grade : int (* \in [0, 100] *) option ;
      report : Learnocaml_report.t option ;
      mtime : float }

  let enc =
    let grade_enc =
      J.conv
        (function
          | Some n when n < 0 || n > 100 -> None
          | g -> g)
        (function
          | Some s when s < 0 || s > 100 -> failwith "grade overflow"
          | g -> g)
        J.(option int) in
    J.conv
      (fun { grade ; solution ; report ; mtime } ->
         (grade, solution, report, mtime))
      (fun (grade, solution, report, mtime) ->
         { grade ; solution ; report ; mtime })
      (J.obj4
         (J.dft "grade" grade_enc None)
         (J.req "solution" J.string)
         (J.opt "report" Learnocaml_report.enc)
         (J.dft "mtime" J.float 0.))

end

module Report = Learnocaml_report

module Save = struct

  type t =
    { nickname : string ;
      all_exercise_editors : (float * string) SMap.t ;
      all_exercise_states : Answer.t SMap.t ;
      all_toplevel_histories :
        Learnocaml_toplevel_history.snapshot SMap.t ;
      all_exercise_toplevel_histories :
        Learnocaml_toplevel_history.snapshot SMap.t }

  let enc =
    J.conv
      (fun t ->
        t.nickname,
        t.all_exercise_editors,
        t.all_exercise_states,
        t.all_toplevel_histories,
        t.all_exercise_toplevel_histories)
      (fun (nickname,
            all_exercise_editors,
            all_exercise_states,
            all_toplevel_histories,
            all_exercise_toplevel_histories) ->
        { nickname ;
          all_exercise_editors ;
          all_exercise_states ;
          all_toplevel_histories ;
          all_exercise_toplevel_histories }) @@
    J.obj5
      (J.dft "nickname" J.string "")
      (J.dft "exercises-editors"
         (SMap.enc (J.tup2 J.float J.string)) SMap.empty)
      (J.dft "exercises"
         (SMap.enc Answer.enc) SMap.empty)
      (J.dft "toplevel-histories"
         (SMap.enc Learnocaml_toplevel_history.snapshot_enc) SMap.empty)
      (J.dft "exercise-toplevel-histories"
         (SMap.enc Learnocaml_toplevel_history.snapshot_enc) SMap.empty)

  let sync a b =
    let sync_snapshot snapshot_a snapshot_b =
      let open Learnocaml_toplevel_history in
      if snapshot_a.mtime > snapshot_b.mtime then
        snapshot_a
      else
        snapshot_b in
    let sync_exercise_edits (ts_a, _ as a) (ts_b, _ as b) =
      if ts_a > ts_b then a else b
    in
    let sync_exercise_state state_a state_b =
      let open Answer in
      if state_a.mtime > state_b.mtime then
        state_a
      else
        state_b in
    let sync_map sync_item index_a index_b =
      SMap.merge
        (fun _id a b -> match a, b with
           | None, None -> assert false
           | None, Some i | Some i, None -> Some i
           | Some a, Some b -> Some (sync_item a b))
        index_a index_b in
    let all_exercise_states =
      sync_map sync_exercise_state
        a.all_exercise_states
        b.all_exercise_states
    in
    let all_exercise_editors =
      sync_map sync_exercise_edits
        a.all_exercise_editors
        b.all_exercise_editors
      |> SMap.filter (fun id (ts, _) ->
          match SMap.find_opt id all_exercise_states with
          | Some {Answer.mtime; _} when mtime > ts -> false
          | _ -> true)
    in
    { nickname = if b.nickname = "" then a.nickname else b.nickname;
      all_exercise_editors;
      all_exercise_states;
      all_toplevel_histories =
        sync_map sync_snapshot
          a.all_toplevel_histories
          b.all_toplevel_histories ;
      all_exercise_toplevel_histories =
        sync_map sync_snapshot
          a.all_exercise_toplevel_histories
          b.all_exercise_toplevel_histories }

  let fix_mtimes save =
    let now = Unix.gettimeofday () in
    let fix t = min t now in
    let fix_snapshot s =
      Learnocaml_toplevel_history.{ s with mtime = fix s.mtime }
    in
    let fix_exercise_edits (ts, e) = fix ts, e in
    let fix_exercise_state s =
      Answer.{ s with mtime = fix s.mtime }
    in
    {
      save with
      all_exercise_editors =
        SMap.map fix_exercise_edits save.all_exercise_editors;
      all_exercise_states =
        SMap.map fix_exercise_state save.all_exercise_states;
      all_toplevel_histories =
        SMap.map fix_snapshot save.all_toplevel_histories;
      all_exercise_toplevel_histories =
        SMap.map fix_snapshot save.all_exercise_toplevel_histories;
    }

  let empty = {
    all_exercise_editors = SMap.empty;
    all_exercise_states = SMap.empty;
    all_toplevel_histories = SMap.empty;
    all_exercise_toplevel_histories = SMap.empty;
    nickname = "";
  }

end

module Token = struct

  type t = string list

  let teacher_token_prefix = "X"

  let to_string = String.concat "-"
  let to_path = String.concat (Filename.dir_sep)
  let teacher_tokens_path = teacher_token_prefix

  let alphabet =
    "ABCDEFGH1JKLMNOPORSTUVWXYZO1Z34SG1B9"
  let visually_equivalent_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

  let parse =
    let table = Array.make 256 None in
    String.iteri
      (fun i c -> Array.set table (Char.code c) (Some alphabet.[i]))
      visually_equivalent_alphabet ;
    let translate part =
      String.map (fun c ->
          match Array.get table (Char.code c) with
          | None -> failwith "bad token character"
          | Some c -> c)
        part in
    fun token ->
      let token = String.trim token in
      let translate_base_token token =
        if String.length token = 15 then
          if String.get token 3 <> '-'
          || String.get token 7 <> '-'
          || String.get token 11 <> '-' then
            failwith "bad token format"
          else
            List.map translate
              [ String.sub token 0 3 ;
                String.sub token 4 3 ;
                String.sub token 8 3 ;
                String.sub token 12 3 ]
        else
          failwith "bad token length"
      in
      if String.length token >= 2 &&
         String.sub token 0 2 = teacher_token_prefix ^ "-"
      then
        teacher_token_prefix ::
        translate_base_token (String.sub token 2 (String.length token - 2))
      else
        translate_base_token token

  let enc = J.conv to_string parse J.string

  let check token =
    try ignore (parse token) ; true
    with _ -> false

  let random () =
    let rand () = String.get alphabet (Random.int (String.length alphabet)) in
    let part () = String.init 3 (fun _ -> rand ()) in
    [ part () ; part () ; part () ; part () ]

  let random_teacher () = teacher_token_prefix :: random ()

  let is_teacher = function
    | x::_ when x = teacher_token_prefix -> true
    | _ -> false

  let is_student t = not (is_teacher t)

  module T = struct
    type nonrec t = t
    let compare = Pervasives.compare
  end

  module Set = Set.Make(T)
  module Map = Map.Make(T)
end

type 'a token = Token.t

type student
type teacher

module Student = struct

  type t = {
    token: student token;
    nickname: string option;
    results: (float * int option) SMap.t;
    creation_date: float;
    tags: SSet.t;
  }

  let enc =
    let open Json_encoding in
    obj5
      (req "token" string)
      (opt "nickname" string)
      (dft "results" (assoc (tup2 float (option int))) [])
      (dft "creation_date" float 0.)
      (dft "tags" (list string) [])
    |> conv
      (fun t ->
         Token.to_string t.token,
         t.nickname, SMap.bindings t.results, t.creation_date,
         SSet.elements t.tags)
      (fun (token, nickname, results, creation_date, tags) -> {
           token = Token.parse token;
           nickname;
           results =
             List.fold_left (fun m (s, r) -> SMap.add s r m)
               SMap.empty
               results;
           creation_date;
           tags = SSet.of_list tags;
         })

  let default token = {
    token;
    nickname = None;
    results = SMap.empty;
    creation_date = Unix.gettimeofday ();
    tags = SSet.empty;
  }

  let three_way_merge ~ancestor ~theirs ~ours =
    let token = ancestor.token in
    if token <> theirs.token || token <> ours.token then
      invalid_arg "three_way_merge";
    let tags = SSet.merge3
        ~ancestor:ancestor.tags
        ~theirs:theirs.tags
        ~ours:ours.tags
    in
    let nickname =
      if ours.nickname <> ancestor.nickname
      then ours.nickname
      else theirs.nickname
    in
    let results =
      SMap.merge (fun id a o ->
          if a <> o then o else
          SMap.find_opt id theirs.results)
        ancestor.results ours.results
    in
    let creation_date =
      min ancestor.creation_date (min theirs.creation_date ours.creation_date)
    in
    { token; tags; nickname; creation_date; results }

  module Index = struct

    type nonrec t = t list

    let enc = J.(list enc)

  end
end

let enc_check_version_1 enc =
  J.conv
    (fun exercise -> ("1", exercise))
    (fun (version, exercise) ->
       if version <> "1" then begin
         let msg = Format.asprintf "unknown version %s" version in
         raise (J.Cannot_destruct ([], Failure msg))
       end ;
       exercise)
    (J.merge_objs (J.obj1 (J.req "learnocaml_version" J.string)) enc)

let enc_check_version_2 enc =
  J.conv
    (fun exercise -> ("2", exercise))
    (fun (version, exercise) ->
       begin
         match version with
         | "1" | "2" -> ()
         | _ ->
             let msg = Format.asprintf "unknown version %s" version in
             raise (J.Cannot_destruct ([], Failure msg))
       end ;
       exercise)
    (J.merge_objs (J.obj1 (J.req "learnocaml_version" J.string)) enc)

module Server = struct
  type preconfig = {
    secret : string option;
  }
  let empty_preconfig = {
    secret = None;
  }

  let preconfig_enc =
    J.conv (fun (c : preconfig) -> c.secret)
           (fun secret : preconfig -> {secret}) @@
      J.obj1 (J.opt "secret" J.string)

  type config = {
    secret : string option;
    server_id : int;
  }

  let build_config (preconf : preconfig) : config =
    let secret = match preconf.secret with
      | None -> None
      | Some secret_in_clear -> Some (Sha.sha512 secret_in_clear) in
    let server_id = Random.bits () in
    {
      secret;
      server_id;
    }

  let config_enc =
    J.conv (fun (c : config) -> (c.secret,c.server_id))
           (fun (secret,server_id) : config -> {secret; server_id}) @@
      J.obj2 (J.opt "secret" J.string) (J.req "server_id" J.int)
end

module Exercise = struct

  type id = string

  type t = Learnocaml_exercise.t

  let enc = Learnocaml_exercise.encoding

  module Meta = struct

    type kind =
      | Project
      | Problem
      | Exercise

    type t = {
      kind: kind;
      title: string;
      short_description: string option;
      stars: float (* \in [0.,4.] *);
      id: id option;
      author: (string * string) list;
      focus: string list;
      requirements: string list;
      forward: id list;
      backward: id list;
    }

    let enc =
      let kind_enc =
        J.string_enum
          [ "problem", Problem ;
            "project", Project ;
            "exercise", Exercise ]
      in
      let exercise_enc_v1 =
        J.(obj10
             (req "kind" kind_enc)
             (dft "title" string "")
             (opt "short_description" string)
             (req "stars" float)
             (opt "identifier" string)
             (dft "authors" (list (tup2 string string)) [])
             (dft "focus" (list string) [])
             (dft "requirements" (list string) [])
             (dft "forward_exercises" (list string) [])
             (dft "backward_exercises" (list string) []))
      in
      let exercise_enc_v2 =
        J.(obj1
             (opt "max_score" int))
        (* deprecated & ignored *)
      in
      J.conv
        (fun t ->
           ((t.kind, t.title, t.short_description, t.stars, t.id,
             t.author, t.focus, t.requirements, t.forward, t.backward),
            None))
        (fun ((kind, title, short_description, stars, id,
               author, focus, requirements, forward, backward),
              _max_score) ->
          { kind; title; short_description; stars; id;
            author; focus; requirements; forward; backward;
          })
        (enc_check_version_2
           (J.merge_objs
              exercise_enc_v1
              exercise_enc_v2))

  end

  module Status = struct

    type skill = [`Plus | `Minus] * string

    type status =
      | Open
      | Closed
      | Assigned of {start: float; stop: float}

    type assignments = {
      token_map: status Token.Map.t;
      default: status;
    }

    type t = {
      id: id;
      skills_prereq: skill list;
      skills_focus: skill list;
      assignments: assignments;
    }

    let empty_assignments = {
      token_map = Token.Map.empty;
      default = Open;
    }

    let default_assignment a = a.default

    let set_default_assignment a default = {a with default}

    let get_status token a =
      match Token.Map.find_opt token a.token_map with
      | Some a -> a
      | None -> a.default

    let is_open_assignment token a =
      match get_status token a with
      | Assigned a ->
         let t = Unix.gettimeofday () in
         if t < a.start then `Closed
         else `Deadline (a.stop -. t)
      | Open -> `Open
      | Closed -> `Closed

    let get_skills ~base skills =
      SSet.elements @@
      List.fold_left (fun acc (op, sk) ->
          match op with
          | `Plus -> SSet.add sk acc
          | `Minus -> SSet.remove sk acc)
        (SSet.of_list base) skills

    let skills_base ~current skills =
      get_skills ~base:current (List.map (function
          | `Plus, x -> `Minus, x
          | `Minus, x -> `Plus, x)
          skills)

    let make_skills ~base current =
       let base = SSet.of_list base in
       let current = SSet.of_list current in
       SSet.fold (fun sk acc -> (`Plus, sk) :: acc)
         (SSet.diff current base) @@
       SSet.fold (fun sk acc -> (`Minus, sk) :: acc)
         (SSet.diff base current) @@
       []

    let skills_prereq meta status =
      get_skills ~base:meta.Meta.requirements status.skills_prereq

    let skills_focus meta status =
      get_skills ~base:meta.Meta.focus status.skills_focus

    module STM = Map.Make(struct
        type t = status
        let compare = compare
      end)

    let by_status_explicit base a =
      Token.Map.fold (fun tok st sm ->
          match STM.find_opt st sm with
          | None -> STM.add st (Token.Set.singleton tok) sm
          | Some toks -> STM.add st (Token.Set.add tok toks) sm)
        a.token_map
        base

    let by_status tokens a =
      let rem_tokens =
        Token.Set.filter (fun tok -> not (Token.Map.mem tok a.token_map)) tokens
      in
      let base =
        if Token.Set.is_empty rem_tokens then STM.empty
        else STM.singleton a.default rem_tokens
      in
      by_status_explicit base a |> STM.bindings

    let three_way_merge ~ancestor ~theirs ~ours =
      let id = ancestor.id in
      if id <> theirs.id || id <> ours.id then
        invalid_arg "three_way_merge";
      let skills_merge3 field =
        let aux filter =
          SSet.merge3
            ~ancestor:(filter ancestor)
            ~theirs:(filter theirs)
            ~ours:(filter ours)
          |> SSet.elements
        in
        List.map (fun sk -> `Plus, sk)
          (aux (fun a ->
               List.fold_left (fun acc -> function
                   |`Plus, sk -> SSet.add sk acc
                   | _ -> acc) SSet.empty (field a))) @
        List.map (fun sk -> `Minus, sk)
          (aux (fun a ->
               List.fold_left (fun acc -> function
                   |`Minus, sk -> SSet.add sk acc
                   | _ -> acc) SSet.empty (field a)))
      in
      let skills_prereq =
        skills_merge3 (fun a -> a.skills_prereq)
      in
      let skills_focus =
        skills_merge3 (fun a -> a.skills_focus)
      in
      let default =
        if ours.assignments.default <> ancestor.assignments.default
        then ours.assignments.default
        else theirs.assignments.default
      in
      let token_map =
        Token.Map.merge (fun tok a o ->
            let a = match a with
              | None -> ancestor.assignments.default
              | Some st -> st
            in
            let t =
              match Token.Map.find_opt tok theirs.assignments.token_map with
              | None -> theirs.assignments.default
              | Some st -> st
            in
            let o = match o with
              | None -> ours.assignments.default
              | Some st -> st
            in
            if o <> a then Some o else Some t)
          ancestor.assignments.token_map
          ours.assignments.token_map
      in
      let token_map =
        Token.Map.merge (fun _ t o -> match t, o with
            | _, (Some st as s) | (Some st as s), _ ->
                if st = default then None else s
            | None, None -> assert false)
          theirs.assignments.token_map
          token_map
      in
      { id;
        skills_prereq;
        skills_focus;
        assignments = { default; token_map } }

    let make_assignments token_map default =
      { token_map; default }

    let enc =
      let status_enc =
        J.union [
          J.case (J.constant "Open")
            (function Open -> Some () | _ -> None) (fun () -> Open);
          J.case (J.constant "Closed")
            (function Closed -> Some () | _ -> None) (fun () -> Closed);
          J.case
            (J.obj2 (J.req "start" J.float) (J.req "stop" J.float))
            (function Assigned a -> Some (a.start, a.stop) | _ -> None)
            (fun (start, stop) -> Assigned {start; stop})
        ]
      in
      let assignments_enc =
        J.conv
          (fun t ->
             t.default,
             List.map (fun (tk, st) -> Token.to_string tk, st)
               (Token.Map.bindings t.token_map))
          (fun (default, token_assoc) -> {
               default;
               token_map =
                 (List.fold_left (fun acc (tok, st) ->
                      Token.Map.add (Token.parse tok) st acc)
                     Token.Map.empty token_assoc)
             })
        @@
        J.obj2
          (J.dft "default" status_enc empty_assignments.default)
          (J.dft "token_map" (J.assoc status_enc) [])
      in
      let skill_enc =
        J.union [
          J.case (J.obj1 (J.req "plus" J.string))
            (function `Plus, sk -> Some sk | _ -> None)
            (fun sk -> `Plus, sk);
          J.case (J.obj1 (J.req "minus" J.string))
            (function `Minus, sk -> Some sk | _ -> None)
            (fun sk -> `Minus, sk);
        ]
      in
      enc_check_version_2 @@
      J.conv
        (fun t -> t.id, t.skills_prereq, t.skills_focus, t.assignments)
        (fun (id, skills_prereq, skills_focus, assignments) ->
           {id; skills_prereq; skills_focus; assignments})
      @@
      J.obj4
        (J.req "id" J.string)
        (J.dft "skills_prereq" (J.list skill_enc) [])
        (J.dft "skills_focus" (J.list skill_enc) [])
        (J.dft "assignments" assignments_enc empty_assignments)

    let default id = {
      id;
      skills_prereq = [];
      skills_focus = [];
      assignments = {
        token_map = Token.Map.empty;
        default = Open;
      }
    }

  end

  module Index = struct

    type t =
      | Exercises of (id * Meta.t option) list
      | Groups of (string * group) list
    and group =
      { title : string;
        contents : t }

    let enc =
      let exercise_enc =
        J.union [
          J.case J.string
            (function id, None -> Some id | _ -> None)
            (fun id -> id, None);
          J.case J.(tup2 string Meta.enc)
            (function id, Some meta -> Some (id, meta) | _ -> None)
            (fun (id, meta) -> id, Some meta);
        ]
      in
      let group_enc =
        J.mu "group" @@ fun group_enc ->
        J.conv
          (fun (g : group) -> g.title, g.contents)
          (fun (title, contents) -> { title; contents }) @@
        J.union
          [ J.case
              J.(obj2
                   (req "title" string)
                   (req "exercises" (list exercise_enc)))
              (function
                | (title, Exercises map) -> Some (title, map)
                | _ -> None)
              (fun (title, map) -> (title, Exercises map)) ;
            J.case
              J.(obj2
                   (req "title" string)
                   (req "groups" (assoc group_enc)))
              (function
                | (title, Groups map) -> Some (title, map)
                | _ -> None)
              (fun (title, map) -> (title, Groups map)) ]
      in
      enc_check_version_2 @@
      J.union
        [ J.case
            J.(obj1 (req "exercises" (list exercise_enc)))
            (function
              | Exercises map -> Some map
              | _ -> None)
            (fun map -> Exercises map) ;
          J.case
            J.(obj1 (req "groups" (assoc group_enc)))
            (function
              | Groups map -> Some map
              | _ -> None)
            (fun map -> Groups map) ]

    let find t id =
      let rec aux t = match t with
        | Groups ((_, g)::r) ->
            (try aux g.contents with Not_found -> aux (Groups r))
        | Groups [] -> raise Not_found
        | Exercises l -> (match List.assoc id l with
            | None -> raise Not_found
            | Some e -> e)
      in
      aux t

    let find_opt t id = try Some (find t id) with Not_found -> None

    let rec map_exercises f = function
      | Groups gs ->
          Groups
            (List.map (fun (id, (g: group)) ->
                 (id, {g with contents = map_exercises f g.contents}))
                gs)
      | Exercises l ->
          Exercises
            (List.map (function
                 | (id, Some ex) -> (id, Some (f id ex))
                 | x -> x)
                l)

    let rec mapk_exercises f t k =
      let rec mapk_list acc f l k = match l with
        | x::r -> f x (fun y -> mapk_list (y::acc) f r @@ k)
        | [] -> List.rev acc |> k
      in
      match t with
      | Groups gs ->
          mapk_list [] (fun (id, (g: group)) k ->
              mapk_exercises f g.contents
              @@ fun contents -> (id, {g with contents}) |> k)
            gs
          @@ fun gs -> Groups gs |> k
      | Exercises l ->
          mapk_list [] (fun e k -> match e with
              | (id, Some ex) ->
                  f id ex @@ fun ex -> (id, Some ex) |> k
              | x -> x |> k)
            l
          @@ fun l -> Exercises l |> k

    let rec fold_exercises f acc = function
      | Groups gs ->
          List.fold_left
            (fun acc (_, (g: group)) -> fold_exercises f acc g.contents)
            acc gs
      | Exercises l ->
          List.fold_left (fun acc -> function
              | (id, Some ex) -> f acc id ex
              | _ -> acc)
            acc l

    let rec filterk f g k =
      match g with
      | Groups gs ->
          let rec aux acc = function
            | (id, (g: group)) :: r ->
                (filterk f g.contents @@ function
                  | Exercises [] -> aux acc r
                  | contents -> aux ((id, { g with contents }) :: acc) r)
            | [] -> match acc with
              | [] -> k (Exercises [])
              | l -> k (Groups (List.rev l))
          in
          aux [] gs
      | Exercises l ->
          let rec aux acc = function
            | (id, Some ex) :: r ->
                (f id ex @@ function
                  | true -> aux ((id, Some ex) :: acc) r
                  | false -> aux acc r)
            | (_, None) :: r -> aux acc r
            | [] -> k (Exercises (List.rev acc))
          in
          aux [] l

    let filter f g = filterk (fun x y k -> f x y |> k) g (fun x -> x)

    (* let rec filter f = function
     *   | Groups (gs) ->
     *       List.fold_left (fun acc (id, (g: group)) ->
     *           match filter f g.contents with
     *           | Exercises [] -> acc
     *           | contents -> (id, { g with contents}) :: acc)
     *         [] (List.rev gs)
     *       |> (function [] -> Exercises [] | l -> Groups l)
     *   | Exercises l ->
     *       List.fold_left (fun acc (id, ex) ->
     *           match ex with
     *           | Some ex when f id ex -> (id, Some ex) :: acc
     *           | _ -> acc)
     *         [] (List.rev l)
     *       |> (function l -> Exercises l) *)

  end

  module Graph = struct

    type relation = Skill of string | Exercise of id

    type node =
      { name : id;
        mutable children : (node * relation list) list }

    let node_exercise { name; _ } = name
    let node_children { children; _ } = children

    let ex_node exs id =
      try Hashtbl.find exs id
      with Not_found ->
        let node = { name = id; children = [] } in
        Hashtbl.add exs id node;
        node

    let merge_children ch =
      let rec merge acc = function
        | [] -> acc
        | (n, ks) :: [] -> (n, ks) :: acc
        | (n, ks) :: (((n', ks') :: rem) as ch') ->
            if n.name = n'.name then merge acc ((n, ks @ ks') :: rem)
            else merge ((n, ks) :: acc) ch'
      in
      List.fast_sort (fun (n1, _) (n2, _) -> compare n1.name n2.name) ch
      |> merge []


    let compute_node ex_id ex_meta focus exercises =
      let exs =
        List.map (fun id -> ex_node exercises id, [Exercise ex_id])
          ex_meta.Meta.backward
      in
      let exs =
        List.fold_left (fun exs skill ->
            List.fold_left (fun exs id ->
                (ex_node exercises id, [Skill skill]) :: exs)
              exs (SMap.find skill focus)
          ) exs ex_meta.Meta.requirements
      in
      let exs = merge_children exs in
      let node = ex_node exercises ex_id in
      node.children <- exs;
      node

    let focus_map exercises =
      let add_ex focus (id, skill) =
        let exs =
          try SMap.find skill focus
          with Not_found -> [] in
        SMap.add skill (id :: exs) focus
      in
      Index.fold_exercises
        (fun focus id meta ->
           List.fold_left add_ex focus
           @@ List.map (fun s -> id, s) meta.Meta.focus)
        SMap.empty exercises

    let apply_filters filters exercises =
      Index.filter (fun id _ ->
          not (List.mem (Exercise id) filters)) exercises |>
      Index.map_exercises (fun _ meta ->
          let requirements =
            List.filter (fun s -> not (List.mem (Skill s) filters))
              meta.Meta.requirements in
          let focus =
            List.filter (fun s -> not (List.mem (Skill s) filters))
              meta.Meta.focus in
          let backward =
            List.filter (fun s -> not (List.mem (Exercise s) filters))
              meta.Meta.backward in
          { meta with Meta.requirements; Meta.focus; Meta.backward })

    let compute_graph ?(filters=[]) exercises =
      let exercises_nodes = Hashtbl.create 17 in
      let ex_filtered = apply_filters filters exercises in
      let focus = focus_map ex_filtered in
      let compute acc ex_id ex_meta =
        compute_node ex_id ex_meta focus exercises_nodes :: acc
      in
      Index.fold_exercises (fun acc id meta -> compute acc id meta) [] ex_filtered

    let compute_exercise_set graph =
      let seen = ref SSet.empty in
      let rec compute acc node =
        if SSet.mem node.name !seen then acc
        else begin
          seen := SSet.add node.name !seen;
          List.fold_right (fun (node, _kinds) acc ->
              compute acc node)
            node.children
            (node.name :: acc)
        end
      in
      compute [] graph

    let dump_dot fmt nodes =
      let print_kind fmt = function
        | Skill s -> Format.fprintf fmt "(S %s)" s
        | Exercise s -> Format.fprintf fmt "(E %s)" s
      in
      let print_child fmt ex child kinds =
        Format.fprintf fmt "%s -> %s [label=\"%a\"];\n"
          ex
          child.name
          (fun fmt -> List.iter (print_kind fmt)) kinds
      in
      let print_node fmt n =
        List.iter (fun (child, kinds) ->
            print_child fmt n.name child kinds)
          n.children
      in
      Format.fprintf fmt
        "digraph exercises {\n\
         %a\n\
         }"
        (fun fmt -> List.iter (print_node fmt)) nodes

  end

end

module Lesson = struct

  type id = string

  type phrase =
    | Text of string
    | Code of string

  type step = {
    step_title: string;
    step_phrases: phrase list;
  }

  type t = {
    title: string;
    steps: step list;
  }

  let enc =
    enc_check_version_2 @@
    J.conv
      (fun t -> (t.title, t.steps))
      (fun (title, steps) -> { title; steps }) @@
    J.obj2
      J.(req "title" string)
      J.(req "steps"
           (list @@
            conv
              (fun s -> (s.step_title, s.step_phrases))
              (fun (step_title, step_phrases) -> {step_title; step_phrases}) @@
            (obj2
               (req "title" string)
               (req "contents"
                  (list @@ union
                     [ case
                         (obj1 (req "html" string))
                         (function Text text -> Some text | Code _ -> None)
                         (fun text -> Text text) ;
                       case
                         (obj1 (req "code" string))
                         (function Code code -> Some code | Text _ -> None)
                         (fun code -> Code code) ])))))

  module Index = struct

    type t = (id * string) list

    let enc =
      enc_check_version_2 @@
      J.(obj1 (req "lessons" (list @@ tup2 string string)))

  end

end

module Tutorial = struct

  type id = string

  type code = {
    code: string;
    runnable: bool;
  }

  type word =
    | Text of string
    | Code of code
    | Emph of text
    | Image of { alt : string ; mime : string ; contents : bytes }
    | Math of string

  and text =
    word list

  type phrase =
    | Paragraph of text
    | Enum of phrase list list
    | Code_block of code

  type step = {
    step_title: text;
    step_contents: phrase list;
  }

  type t = {
    title: text;
    steps: step list;
  }

  let text_enc =
    J.mu "text" @@ fun content_enc ->
    let word_enc =
      J.union
        [ J.case J.string
            (function Text text -> Some text | _ -> None)
            (fun text -> Text text) ;
          J.case
            J.(obj1 (req "text" string))
            (function Text text -> Some text | _ -> None)
            (fun text -> Text text) ;
          J.case
            J.(obj1 (req "emph" content_enc))
            (function Emph content -> Some content | _ -> None)
            (fun content -> Emph content) ;
          J.case
            J.(obj2 (req "code" string) (dft "runnable" bool false))
            (function Code { code ; runnable } -> Some (code, runnable)
                    | _ -> None)
            (fun (code, runnable) -> Code { code ; runnable }) ;
          J.case
            J.(obj1 (req "math" string))
            (function Math math-> Some math | _ -> None)
            (fun math -> Math math) ;
          J.case
            J.(obj3 (req "image" bytes) (req "alt" string) (req "mime" string))
            (function
              | Image { alt ; mime ; contents = image } ->
                  Some (image, alt, mime)
              | _ -> None)
            (fun (image, alt, mime) ->
               Image { alt ; mime ; contents = image }) ] in
    J.union
    [ J.case
        word_enc
        (function [ ctns ] -> Some ctns | _ -> None) (fun ctns -> [ ctns ]) ;
      J.case
        (J.list @@ word_enc)
        (fun ctns -> Some ctns) (fun ctns -> ctns) ]

  let phrase_enc =
    J.mu "phrase" @@ fun phrase_enc ->
    J.union
      [ J.case
          J.(obj1 (req "paragraph" text_enc))
          (function Paragraph phrase -> Some phrase | _ -> None)
          (fun phrase -> Paragraph phrase) ;
        J.case
          J.(obj1 (req "enum" (list (list phrase_enc))))
          (function Enum items -> Some items | _ -> None)
          (fun items -> Enum items) ;
        J.case
          J.(obj2 (req "code" string) (dft "runnable" bool false))
          (function Code_block { code ; runnable } ->
             Some (code, runnable) | _ -> None)
          (fun (code, runnable) ->
             Code_block { code ; runnable }) ;
        J.case
          text_enc
          (function Paragraph phrase -> Some phrase | _ -> None)
          (fun phrase -> Paragraph phrase) ]

  let enc =
    enc_check_version_2 @@
    J.conv
      (fun t -> t.title, t.steps)
      (fun (title, steps) -> {title; steps}) @@
    J.obj2
      (J.req "title" text_enc)
      (J.req "steps"
         (J.list @@
          J.conv
            (fun t -> t.step_title, t.step_contents)
            (fun (step_title, step_contents) -> {step_title; step_contents}) @@
          J.(obj2
               (req "title" text_enc)
               (req "contents" (list phrase_enc)))))

  module Index = struct

    type entry = {
      name: string;
      title: text;
    }

    type series = {
      series_title: string;
      series_tutorials: entry list;
    }

    type t = (id * series) list

    let enc =
      let entry_enc =
        J.union [
          J.case
            J.(tup2 string text_enc)
            (function ({title = []; _}: entry) -> None
                    | {name; title} -> Some (name, title))
            (fun (name, title) -> {name; title});
          J.case
            J.string
            (function {name; title = []} -> Some name
                    | _ -> None)
            (fun name -> {name; title = []});
        ]
      in
      let series_enc =
        J.conv
          (fun t ->
             (t.series_title, t.series_tutorials))
          (fun (series_title, series_tutorials) ->
             {series_title; series_tutorials}) @@
        J.obj2
          J.(req "title" string)
          J.(req "tutorials" (list entry_enc)) in
      enc_check_version_1 @@
      J.(obj1 (req "series" (assoc series_enc)))

  end
end

module Partition = struct
  type t =
  {
    not_graded : Token.t list;
    bad_type   : Token.t list;
    partition_by_grade :
      (int *
         (((Token.t * string) list) Asak.Wtree.wtree list))
        list;
  }

  let token_list = J.list Token.enc

  let tree_enc leaf_enc =
    let open Asak.Wtree in
    J.mu "tree" @@ fun self ->
       J.union
          [ J.case (J.obj1 (J.req "leaf" leaf_enc))
              (function Leaf x -> Some x | Node _ -> None)
              (fun x -> Leaf x) ;
            J.case (J.obj3 (J.req "coef" J.int) (J.req "left" self) (J.req "right" self))
              (function Node (t,l,r) -> Some (t,l,r) | Leaf _ -> None)
              (fun (t,l,r) -> Node (t,l,r)) ]

  let leaf_enc =
    J.list (J.tup2 Token.enc J.string)

  let innerlist = J.list (tree_enc leaf_enc)

  let int_assoc =
    J.tup2 J.int innerlist

  let enc =
    J.conv
      (fun t ->
        (t.not_graded, t.bad_type, t.partition_by_grade))
      (fun (not_graded, bad_type, partition_by_grade) ->
        {not_graded; bad_type; partition_by_grade}) @@
      J.obj3
        J.(req "not_graded" token_list)
        J.(req "bad_type"   token_list)
        J.(req "patition_by_grade" (J.list int_assoc))
end

module Playground = struct
  type id = string

  type t =
  { id : id ;
    prelude : string ;
    template : string ;
  }

  let enc =
    J.conv
    (fun { id; prelude; template } ->
       id, prelude, template)
    (fun (id, prelude, template) ->
       { id ; prelude ; template })
    (J.obj3
       (J.req "id" J.string)
       (J.req "prelude" J.string)
       (J.req "template" J.string))

  module Meta = struct
    type t =
      {
        title: string;
        short_description: string option;
      }

    let default id = {title=id; short_description=None}

    let enc =
    J.conv
    (fun { title; short_description } ->
       title, short_description)
    (fun (title, short_description) ->
       { title; short_description })
    (J.obj2
       (J.req "title" J.string)
       (J.req "short_description" (J.option J.string)))
  end

  module Index = struct

    type t = (id * Meta.t) list

    let enc = J.list (J.tup2 J.string Meta.enc)

  end
end

module Editor = struct
  
 
  type type_question= Suite | Solution | Spec ;;
  
  
  type test_qst_untyped =
    | TestAgainstSol of
        { name: string
        ; ty: string
        ; gen: int
        ; suite: string
        ; tester: string
        ; sampler : string }
    | TestAgainstSpec of
        { name: string
        ; ty: string
        ; gen: int
        ; suite: string
        ; spec : string
        ; tester: string
        ; sampler: string }
    | TestSuite of
        { name: string;
          ty: string;
          suite: string;
          tester :string };;
  


type exercise =
  { id : string ;
    prelude : string ;
    template : string ;
    descr : string ;
    prepare : string ;
    test : string ;
    solution : string ;
    max_score : int ;
  }
  
  
  let exercise_encoding =
  let open Json_encoding in
  conv
    (fun { id; prelude; template; descr; prepare; test; solution; max_score } ->
       id, prelude, template, descr, prepare, test, solution, max_score)
    (fun (id, prelude, template, descr, prepare, test, solution, max_score) ->
       { id ; prelude ; template ; descr ; prepare ; test ; solution ; max_score })
    (obj8
       (req "id" string)
       (req "prelude" string)
       (req "template" string)
       (req "descr" string)
       (req "prepare" string)
       (req "test" string)
       (req "solution" string)
       (req "max_score" int))



  type editor_state =
    { exercise : exercise;
      metadata : Exercise.Meta.t;}

  let editor_state_enc =
    J.conv
      (fun {exercise; metadata } ->
        (exercise, metadata))
      (fun (exercise, metadata) ->
        {exercise; metadata })
      (J.obj2
         (J.req "exercise" exercise_encoding)
         (J.req "metadata" Exercise.Meta.enc))

  type editor_template =
    { name : string;
      template : string}

  let editor_template_enc =
    J.conv
      (fun {name; template } ->
        (name, template))
      (fun (name, template) ->
        {name; template})
      (J.obj2
         (J.req "name" J.string)
         (J.req "template" J.string))
        

  module IMap = struct

    include Map.Make(struct type t = int let compare (x:t) y = compare x y end)

    (** Useful for serialization *)
    let string_of_int_map iv =
      
      fold (fun i v sv -> SMap.add (string_of_int i) v sv) iv SMap.empty
      
    let int_of_string_map iv =
      SMap.fold (fun s v iv -> add (int_of_string s) v iv) iv empty
  end
              
end
