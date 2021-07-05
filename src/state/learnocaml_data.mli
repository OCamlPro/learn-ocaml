(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

module SMap: sig

  include Map.S with type key = string

  val enc: 'a Json_encoding.encoding -> 'a t Json_encoding.encoding

end

module SSet: sig

  include Set.S with type elt = string

  val enc: t Json_encoding.encoding

  (** Three-way merge. [ours] always wins if it was modified from [ancestor] *)
  val merge3: ancestor:t -> theirs:t -> ours:t -> t

end

module Report = Learnocaml_report

module Answer: sig

  type t = {
    solution: string ;
    grade: int (* \in [0, 100] *) option ;
    report: Report.t option ;
    mtime: float
  }

  val enc: t Json_encoding.encoding

end

module Save: sig

  type t = {
    nickname: string ;
    all_exercise_editors: (float * string) SMap.t;
    all_exercise_states: Answer.t SMap.t;
    all_toplevel_histories: Learnocaml_toplevel_history.snapshot SMap.t;
    all_exercise_toplevel_histories:
      Learnocaml_toplevel_history.snapshot SMap.t;
  }

  val enc: t Json_encoding.encoding

  (** Merges two save files, trusting the [mtime] fields to take the most recent
      versions of every item. All other things equal, the fields from the second
      argument are preferred. *)
  val sync: t -> t -> t

  (** Checks all [mtime] fields to get them back to now in case they are in the
      future. Needed for save files that come from clients with possibly bad
      clocks. *)
  val fix_mtimes: t -> t

  val empty: t

end

module Token: sig
  type t

  val enc: t Json_encoding.encoding

  val to_path: t -> string
  val to_string: t -> string
  val parse: string -> t
  val check: string -> bool
  val random: unit -> t
  val random_teacher: unit -> t
  val is_teacher: t -> bool
  val is_student: t -> bool

  (** The relative path containing teacher tokens *)
  val teacher_tokens_path: string

  module Set: Set.S with type elt = t

  module Map: Map.S with type key = t
end

type 'a token = Token.t

type student
type teacher

module Student: sig

  type t = {
    token: student token;
    nickname: string option;
    results: (float * int option) SMap.t;
    creation_date: float;
    tags: SSet.t;
  }

  val enc: t Json_encoding.encoding

  val default: student token -> t

  val three_way_merge: ancestor:t -> theirs:t -> ours:t -> t

  module Index: sig

    type nonrec t = t list

    val enc: t Json_encoding.encoding

  end

end

module Server : sig
  (* preconfig: the type of configuration files in the corpus repository,
     where users can pre-set some of the server settings. *)
  type preconfig = {
    secret : string option;
  }
  val empty_preconfig : preconfig

  (* config: the type of configuration of a running server, generated
     from the preconfig during the 'build' stage. *)
  type config = {
    secret : string option; (* maybe a secret *)
    server_id : int; (* random integer generated each building time *)
  }

  val build_config : preconfig -> config

  val preconfig_enc: preconfig Json_encoding.encoding
  val config_enc: config Json_encoding.encoding
end

module Exercise: sig

  type id = string

  type t = Learnocaml_exercise.t

  val enc: t Json_encoding.encoding

  module Meta: sig

    type kind =
      | Project
      | Problem
      | Exercise

    type t = {
      kind: kind;
      title: string;
      short_description: string option;
      stars: float (** \in [0.,4.] *);
      id: id option;
      author: (string * string) list;
      focus: string list;
      requirements: string list;
      forward: id list;
      backward: id list;
    }

    val enc: t Json_encoding.encoding

  end

  module Status: sig

    type skill

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

    val default: id -> t

    val default_assignment: assignments -> status

    val set_default_assignment: assignments -> status -> assignments

    val get_status:
      Token.t -> assignments -> status

    val is_open_assignment:
      Token.t -> assignments -> [> `Open | `Closed | `Deadline of float]

    val by_status:
      Token.Set.t -> assignments -> (status * Token.Set.t) list

    (** Computes the current set of skills from the base list (from Meta.t),
        using the mutable changes in the Status.skill list. E.g. {[
          get_skills ~base:meta.Meta.requirements st.skills_prereq
        ]} *)
    val get_skills: base:string list -> skill list -> string list

    (** The opposite of [get_skills]: retrieves the base from the already
        updated version and the skill list that has been applied to it. Since the
        server provides [skills] (= [get_skills meta_base status_skills]), this
        is useful to recover [meta_base]. *)
    val skills_base: current:string list -> skill list -> string list

    val skills_prereq: Meta.t -> t -> string list

    val skills_focus: Meta.t -> t -> string list

    (** Generates a skill list that can be saved, such that
        {[get_skills ~base (make_skills ~base l) = l]}.

        Remember to call [skills_base] first on the base if you got the
        skills from the meta returned by the server. *)
    val make_skills: base:string list -> string list -> skill list

    (** Merges all changes from [theirs] and [ours], based on [ancestor]. [ours]
        is privileged in case of any conflict (e.g. different affectation of the
        same student) *)
    val three_way_merge:
      ancestor:t -> theirs:t -> ours:t -> t

    val make_assignments:
      status Token.Map.t -> status -> assignments

    val enc: t Json_encoding.encoding

  end

  module Index: sig

    type t =
      | Exercises of (id * Meta.t option) list
      | Groups of (string * group) list
    and group =
      { title : string;
        contents : t }

    val enc: t Json_encoding.encoding

    val find: t -> id -> Meta.t

    val find_opt: t -> id -> Meta.t option

    val map_exercises: (id -> Meta.t -> Meta.t) -> t -> t

    val fold_exercises: ('a -> id -> Meta.t -> 'a) -> 'a -> t -> 'a

    val filter: (id -> Meta.t -> bool) -> t -> t

    (** CPS version of [map_exercises] *)
    val mapk_exercises:
      (id -> Meta.t -> (Meta.t -> 'a) -> 'a) ->
      t ->
      (t -> 'a) -> 'a

    (** CPS version of [filter] *)
    val filterk: (id -> Meta.t -> (bool -> 'a) -> 'a) -> t -> (t -> 'a) -> 'a

  end

  (** Dependency graph of exercises *)
  module Graph : sig

    (** Two exercises can be related either by a skill dependency, or backward
        relationship *)
    type relation = Skill of string | Exercise of id

    (** An exercise depends on others, by a skill or/and backward relation.
        Due to hashconsing, its representation is not directly available.
    *)
    type node

    val node_exercise : node -> id
    val node_children : node -> (node * relation list) list

    (** Computes the dependency graph of exercises, and filters out exercises
        or skills if any are given. *)
    val compute_graph : ?filters:relation list -> Index.t -> node list

    (** Computes a set of exercises that appear as dependencies of the given
        exercise. *)
    val compute_exercise_set : node -> string list

    (** Dumps the graph as a `dot` representation, into the given formatter. *)
    val dump_dot : Format.formatter -> node list -> unit

  end

end

module Lesson: sig

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

  val enc: t Json_encoding.encoding

  module Index: sig

    type t = (id * string) list

    val enc: t Json_encoding.encoding

  end

end

module Tutorial: sig

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

  val enc: t Json_encoding.encoding

  module Index: sig

    type entry = {
      name: string;
      title: text;
    }

    type series = {
      series_title: string;
      series_tutorials: entry list;
    }

    type t = (id * series) list

    val enc: t Json_encoding.encoding

  end

end

module Partition : sig
  type t =
  {
    not_graded : Token.t list;
    bad_type   : Token.t list;
    partition_by_grade :
      (int *
         (((Token.t * string) list) Asak.Wtree.wtree list))
        list;
  }

  val enc: t Json_encoding.encoding
end

module Playground : sig
  type id = string

  type t =
  { id : id ;
    prelude : string ;
    template : string ;
  }

  val enc: t Json_encoding.encoding

  module Meta : sig
    type t =
      {
        title: string;
        short_description: string option;
      }

    val default : string -> t

    val enc: t Json_encoding.encoding
  end

  module Index: sig

    type t = (id * Meta.t) list

    val enc: t Json_encoding.encoding

  end
end
                   
module Editor : sig     
     
  type type_question = Suite | Solution | Spec

  type test_qst_untyped =
    | TestAgainstSol of
        { name: string
        ; ty: string 
        ; gen: int
        ; suite: string
        ; tester: string
        ; sampler: string }
    | TestAgainstSpec of
        { name: string
        ; ty: string
        ; gen: int
        ; suite: string
        ; spec : string
        ; tester: string
        ; sampler: string }
    | TestSuite of
        { name: string
        ; ty: string
        ; suite: string
        ; tester: string } ;;
 
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

  type editor_state =
    { exercise : exercise;
      metadata : Exercise.Meta.t; }

  val editor_state_enc : editor_state Json_encoding.encoding

  type editor_template =
    { name : string;
      template : string}

  val editor_template_enc : editor_template Json_encoding.encoding
    
end
