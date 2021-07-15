(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Documentation for [test_lib] library. [Test_lib] module can be
   used to write graders for learn-ocaml.  *)
module type S = sig

  val set_result : Learnocaml_report.t -> unit

  type nonrec 'a result = ('a, exn) result

  (** {1 AST checkers}

   Various functions to explore the student's code AST and
   enforce restrictions on the student's code.  *)

  module Ast_checker : sig

    (** Since the user's code is reified, the parsed abstract syntax
       tree is available in the testing environment, as a variable
       named [code_ast], with type [Parsetree.structure]. As such, it
       can be checked using the iterators in the module [Ast_mapper]
       from [compiler-libs]. However, [Test_lib] provides some
       functions to check the Parsetree. *)

    (** {2 Checkers} *)

    (** The functional type ['a ast_checker] describes the functions
       used for AST introspection. Such a function takes as input the
       introspected objects (mainly [Parsetree structure] like
       [code_ast] or [Parsetree expression]).

       For example, adding [~on_open: (forbid "open")] will prevent the
       student from using [open] syntax by returning a [Failure]
       report at the first occurrence of the [open] keyword in the
       input [Parsetree] structure or expression.

       This function has several optional arguments to describe the
       behavior of the AST checker on various parts of the AST. Each
       argument corresponds to a specific type:

        - [~on_expression]: [Parsetree.expression] ;

        - [~on_pattern]: [Parsetree.pattern] ;

        - [~on_structure_item]: [Parsetree.structure_item] ;

        - [~on_external]: [value_description] ;

        - [~on_include]: [Parsetree.include_declaration] ;

        - [~on_open]: [Parsetree.open_description] ;

       Besides, there are more high-level checkers:

        - [~on_module_occurence]: this function is called each time a
       module is used. The string input is the name of the module.

        - [~on_variable_occurence]: function called each time a free
       variable (relative to the AST given as argument) is used.
       The string input is the name of the variable. The functions
       [restrict] and [forbid] can be used to construct this function.

        - [~on_function_call]: function called each time a function
       call is used. The first argument of this function is the
       [Parsetree.expression] of the called function. The second
       argument is the arguments represented by a tuple [(name,
       expression)].

    *)
    type 'a ast_checker =
      ?on_expression: (Parsetree.expression -> Learnocaml_report.t) ->
      ?on_pattern: (Parsetree.pattern -> Learnocaml_report.t) ->
      ?on_structure_item: (Parsetree.structure_item -> Learnocaml_report.t) ->
      ?on_external: (Parsetree.value_description -> Learnocaml_report.t) ->
      ?on_include: (Parsetree.include_declaration -> Learnocaml_report.t) ->
      ?on_open: (Parsetree.open_description -> Learnocaml_report.t) ->
      ?on_module_occurence: (string -> Learnocaml_report.t) ->
      ?on_variable_occurence: (string -> Learnocaml_report.t) ->
      ?on_function_call: (
          (Parsetree.expression
           * (string * Parsetree.expression) list) -> Learnocaml_report.t)
      -> 'a -> Learnocaml_report.t

    (** [ast_check_expr] builds an {{!ast_checker}AST checker} for
       [Parsetree] expressions. This function can be used as
       functional argument for {!find_binding}. *)
    val ast_check_expr : Parsetree.expression ast_checker

    (** [ast_check_structure] builds an {{!ast_checker}AST checker}
       for [Parsetree] structure. The returned AST checker can
       directly be used with [code_ast] which is available in the
       grading environment. *)
    val ast_check_structure : Parsetree.structure ast_checker

    (** {2 Finding top level variable in AST}*)

    (** [find_binding code_ast name cb] looks for the top level
       variable [name] in [code_ast] and its associated Parsetree
       expression [expr] ([let name = expr]). If the variable is
       found, it returns an {!Learnocaml_report.Informative} report
       concatenated with the report resulting of [cb] applied to
       [expr]. Otherwise, it returns a {!Learnocaml_report.Failure}
       report. *)
    val find_binding :
      Parsetree.structure -> string
      -> (Parsetree.expression -> Learnocaml_report.t)
      -> Learnocaml_report.t

    (** {2 Functions for optional arguments of checkers} *)
    (** The following functions are classic functions to use as
       optional arguments for {!ast_checker} like forbidding,
       rectricting or requiring some [Parsetree] structures or
       expressions etc..*)

    (** {3 Generic functions} *)

    (** [forbid k pr ls t] returns a
       {{!Learnocaml_report.Failure}Failure} the first time [t] is
       tested if [t] is in the list [ls]. The message of the
       report is {e The text1 text2 is forbidden} where [text1] is the
       result of [pr] applies to [t] and [text2] is value
       [k]. Otherwise, an empty report is returned. *)
    val forbid :
      string -> ('a -> string) -> 'a list -> ('a -> Learnocaml_report.t)

    (** [restrict k pr ls t] returns a
       {{!Learnocaml_report.Failure}Failure} the first time [t] is
       tested if [t] is {e not} in [ls]. The message of the
       report is {e The text1 text2 is not allowed} where [text1] is
       the result of [pr] applies to [t] and [text2] is value of
       [k]. Otherwise, an empty report is returned. *)
    val restrict :
      string -> ('a -> string) -> 'a list -> ('a -> Learnocaml_report.t)

    (** [require k pr _ t] returns a {{!Learnocaml_report.Success
       5}Success 5} report the first time this function is called. The
       message of the report is {e Found text1 text2} where
       [text1] is value of [k] and [text2] is the result of [pr]
       applies to [t]. Otherwise, an empty report is returned. *)
    val require :
      string -> ('a -> string) -> 'a -> ('a -> Learnocaml_report.t)

    (** {3 For expressions } *)

    (** [forbid_expr name exprs expr] returns a
       {{!Learnocaml_report.Failure}Failure} report the first time
       [expr] is tested if [expr] is in the list of forbidden
       expressions [exprs]. The message of the report is {e
       The text1 text2 is forbidden} where [text1] is [expr] and
       [text2] is value of [name]. Otherwise, an empty report is
       returned. *)
    val forbid_expr :
      string -> Parsetree.expression list
      -> (Parsetree.expression -> Learnocaml_report.t)

    (** [restrict_expr name exprs expr] returns a
       {{!Learnocaml_report.Failure}Failure} report the first time
       [expr] is tested if [expr] is {e not} in the list of allowed
       expressions [exprs]. The message of the report is {e
       The text1 text2 is not allowed} where [text1] is [expr] and
       [text2] is value of [name]. Otherwise, an empty report is
       returned.  *)
    val restrict_expr :
      string -> Parsetree.expression list
      -> (Parsetree.expression -> Learnocaml_report.t)

    (** [require_expr name _ t] returns a {{!Learnocaml_report.Success
       5}Success 5} report the first time this function is called. The
       message of the success report is {e Found text1 text2} where
       [text1] is value of [name] and [text2] is the result of [pr]
       applies to [t]. Otherwise, an empty report is returned. *)
    val require_expr :
      string -> Parsetree.expression
      -> (Parsetree.expression -> Learnocaml_report.t)

    (** {3 For syntax } *)

    (** These functions are very restricted function to either forbid
       any use of a particular syntax or require to have at least one
       use of it. Their first argument is used to build the message of
       the return report and the second argument is simply ignored.

        For example, adding [~on_include: (forbid "include") ~on_open:
       (forbid "open")] prevents the student from using [open] and
       [include] syntaxes. *)

    (** [forbid_syntax n _] returns a
       {{!Learnocaml_report.Failure}Failure} report the first time it
       is called. The message of the report is {e The {b text}
       syntax is forbidden} where [text] is the value of
       [n]. Otherwise, an empty report is returned. *)
    val forbid_syntax : string -> (_ -> Learnocaml_report.t)

    (** [require_syntax n _] returns a {{!Learnocaml_report.Success
       5}Success 5} report the first time it is called. The message of
       the report is {e The {b text} syntax has been found, as
       expected} where [text] is the value of [n]. Otherwise, an empty
       report is returned.  *)
    val require_syntax : string -> (_ -> Learnocaml_report.t)

    (** {2 AST sanity checks } *)

    (** [ast_sanity_check ~modules ast cb] *)
    val ast_sanity_check :
      ?modules: string list -> Parsetree.structure
      -> (unit -> Learnocaml_report.t)
      -> Learnocaml_report.t

  end

  (** {1 Testers and IO testers} *)

  (** Predefined functions and function builders used for the optional
     argument [~test], [~test_stdout] and [~test_stderr] of the various
     {{!Test_functions_function.test_functions_fun_sec}grading functions
     for functions}. *)

  (** Functions of type [tester] are used to compare student result
     with solution result. The first {!S.result} is the student
     output and the second one is the solution output. *)
  type 'a tester =
    'a Ty.ty -> 'a result -> 'a result -> Learnocaml_report.t

  (** Functions of type [io_tester] are used to compare student
     standard out or standard error channels with solution ones. *)
  type io_tester =
    string -> string -> Learnocaml_report.t

  (** Functions of type [io_postcond] are used to verify that student
     standard out or standard error channels satisfy a postcondition. *)
  type io_postcond =
    string -> Learnocaml_report.t

  (** The exception [Timeout limit] is raised by [run_timeout]. Thus, the
      functions [exec] and [result] can return [Error (Timeout limit)].
      The integer [limit] is the maximum time that was allowed, in seconds. *)
  exception Timeout of int

  module Tester : sig
    (** Testers are essentially used for the optional arguments
       [~test], [~test_stdout], [~test_stderr] of
       {{!Test_functions_function.test_functions_fun_sec}test
       functions for functions}. They define the functions which build
       three of four parts of the global report returned by test
       functions.

       {!S.tester} functions compare student output and solution
       output and return usually a {!Learnocaml_report.Success 1} if
       they match and a {!Learnocaml_report.Failure} if they don't.

       {!S.io_tester} functions compare the string outputs on standard
       or error channels for student and solution functions and return
       usually a {!Learnocaml_report.Success 5} if they match and a
       {!Learnocaml_report.Failure} if they don't. *)

    (** {2:tester_sec Pre-defined testers and tester builders} *)

    (** [test] is a {!S.tester} that compares its two {!S.result} inputs
       with OCaml structural equality. This is the default value of [~test]
       optional argument of grading functions for functions.*)
    val test : 'a tester

    (** [test_ignore] is a {!S.tester} that compares only the constructor of its
       {!S.result} inputs. The content is ignored. If the constructors
       match, an empty report is returned. *)
    val test_ignore : 'a tester

    (** [test_eq eq] builds a {!S.tester} with function [eq] as comparison
       function. *)
    val test_eq : ('a result -> 'a result -> bool) -> 'a tester

    (** [test_eq_ok eq] builds a {!S.tester} that compares [Ok] results with
       [eq] and [Error] results with Ocaml structural equality. *)
    val test_eq_ok : ('a -> 'a -> bool) -> 'a tester

    (** [test_eq_exn eq] builds a {!S.tester} that compares [Error] results
       with [eq] and [Ok] results with Ocaml structural equality. *)
    val test_eq_exn : (exn -> exn -> bool) -> 'a tester

    (** [test_canon canon] builds a {!S.tester} that compares its two
       {!S.result} inputs after application to [canon] function with
       Ocaml structural equality. *)
    val test_canon : ('a result -> 'a result) -> 'a tester

    (** [test_canon_ok canon] builds a {!S.tester} that compares two
       [Ok] result inputs after application to [canon] function with
       Ocaml structural equality. [Error] results are compared
       normally with Ocaml structural equality. *)
    val test_canon_ok : ('a -> 'a) -> 'a tester

    (** [test_canon_error canon] builds a {!S.tester} that compares two
       [Error] result inputs after application to [canon] function with
       Ocaml structural equality. [Ok] results are compared
       normally with Ocaml structural equality. *)
    val test_canon_error : (exn -> exn) -> 'a tester

    (** [test_translate conv test ty] builds a {!S.tester} that
       translates its inputs [va] and [vb] to ['b results] [va_trans]
       and [vb_trans] using the conversion function [conv] and returns
       the report of [test ty va_trans vb_trans].*)
    val test_translate : ('a -> 'b) -> 'b tester -> 'b Ty.ty -> 'a tester

    (** {2:io_tester_sec Pre-defined IO testers and IO tester builders} *)

    (** IO testers are essentially used for the optional arguments
        [ ~test_stdout] [~test_stderr] of grading functions. *)

    (** Important warning : when successful, predefined IO testers
       return [Success 5] reports. *)

    (** There are two common optional arguments for IO testers :

        - [~trim] : list of chars removed at beginning and end of IO
       testers input strings.

        - [~drop] : list of chars removed from IO testers input
       strings *)


    (** [io_test_ignore] is the default value of [~test_stdout] and
       [~test_stderr]. Returns an empty report whatever its inputs. *)
    val io_test_ignore : io_tester

    (** [io_test_equals] builds a {!S.io_tester} which compares its
       input strings using Ocaml structural equality. *)
    val io_test_equals :
      ?trim: char list -> ?drop: char list -> io_tester

    (** [io_test_lines ~skip_empty ~test_line] builds a {!S.io_tester}
       which compares each line (separated with ['\n']) of its two
       string inputs with [test_line]. The default value of
       [test_line] is [io_tester_equals]. If [skip_empty] is set to
       [true], the empty lines are skipped (default value is
       [false]). *)
    val io_test_lines :
      ?trim: char list -> ?drop: char list ->
      ?skip_empty: bool -> ?test_line: io_tester -> io_tester

    (** [io_test_items ~split ~skip_empty ~test_item] buids a
       {!S.io_tester} which splits its two string inputs into several
       items using [split] as separators and compares each item with
       [test_item] ([io_tester_equals] by default). If [skip_empty] is
       set to [true], the empty items are skipped (default value is
       [false]). *)
    val io_test_items :
      ?split: char list -> ?trim: char list -> ?drop: char list ->
      ?skip_empty: bool -> ?test_item: io_tester -> io_tester

  end

  (** {1 Mutation observer builders} *)

  (** Functions to help to build the optional arguments
     [~before_reference], [~before_user], [~test] used by grading
     functions for {b unary} function with a mutable input. *)
  module Mutation : sig

    (** Important warning: this part is useful only to grade unary
       function using grading functions such than
       {!S.Test_functions_function.test_function_1_against_solution}. *)

    type 'arg arg_mutation_test_callbacks =
      { before_reference : 'arg -> unit ;
        before_user : 'arg -> unit ;
        test : 'ret. ?test_result: 'ret tester -> 'ret tester }

    (** [arg_mutation_test_callbacks ~test_ref ~dup ~blit ty] returns
       a {!Mutation.arg_mutation_test_callbacks} [out] such than the
       functions [out.before_reference] and [out.before_user] can
       create two copies of the mutable input of type [ty] with
       [dup]. One copy [got] is made before executing the user code
       and saved before executing the solution. The second copy is
       also made at this time and so mutate during solution
       execution. They can then be compared with [out.test].

       [out.test] is a {!S.tester} which actually builds two reports,
       one using its optional argument [~test_result] and one that
       compares the values of the references set previously using
       [test_ref]. By default [~test_result] is equal to
       {!Tester.test_ignore}.

       [dup in] returns a copy of [in].

       [blit src dst] copies [src] into [dst].*)
    val arg_mutation_test_callbacks:
      ?test: 'a tester -> dup: ('a -> 'a)
      -> blit:('a -> 'a -> unit) -> 'a Ty.ty
      -> 'a arg_mutation_test_callbacks

    (** [array_arg_mutation_test_callbacks ~test_arr ty] builds
       [before_user], [before_reference] and [test] such than [test] can
       compare mutation of an input array through student code and
       solution.

        By default, [test_arr] is set to {!Tester.test}.*)
    val array_arg_mutation_test_callbacks:
      ?test: 'a array tester -> 'a array Ty.ty ->
      'a array arg_mutation_test_callbacks

    (** [ref_arg_mutation_test_callbacks ~test_ref ty] builds
       [before_user], [before_reference] and [test] such than [test] can
       compare mutation of an input reference through student code and
       solution.

        By default, [test_ref] is set to {!Tester.test}. *)
    val ref_arg_mutation_test_callbacks:
      ?test: 'a ref tester -> 'a ref Ty.ty ->
      'a ref arg_mutation_test_callbacks

  end

  (** {1 Samplers } *)

  (** [Sampler] provides a library of predefined samplers for
     {{!Test_functions_function}grading functions}.*)
  module Sampler : sig

    type 'a sampler = unit -> 'a

    (** {2:sampler_sec Samplers} *)

    (** [sample_int ()] returns a random integer between -5 and 5. *)
    val sample_int : int sampler

    (** [sample_float ()] returns a random float between -5. and 5. *)
    val sample_float : float sampler

    (** [sample_string ()] returns a randomly long random string. *)
    val sample_string : string sampler

    (** [sample_char ()] returns an alphabet letter randomly. *)
    val sample_char : char sampler

    (** [sample_bool ()] returns randomly [false] or [true]. *)
    val sample_bool : bool sampler

    (** {2 Sampler builders} *)

    (** [sample_list ~min_size ~max_size sample] returns a list
       sampler that generates a list of random length between
       [min_size] ([0] by default) and [max_size] ([10] by default)
       where each element are generated using [sample].

     If [~sorted:true] ([false] by default) the generated list is
       sorted (using Pervasives.compare).

     If [~dups:false] ([true] by default), all elements of generated
       list are unique.*)
    val sample_list :
      ?min_size: int -> ?max_size: int -> ?dups: bool -> ?sorted: bool
      -> 'a sampler
      -> 'a list sampler

    (** [sample_array ~min_size ~max_size sample] returns an array
       sampler that generates an array of random length between
       [min_size] ([0] by default) and [max_size] ([10] by default)
       where each element are generated using [sample].

     If [~sorted:true] ([false] by default) the generated array is
       sorted.

     If [~dups:false] ([true] by default), all elements of generated
       array are unique.*)
    val sample_array :
      ?min_size: int -> ?max_size: int -> ?dups: bool -> ?sorted: bool
      -> 'a sampler
      -> 'a array sampler

    (** [sample_pair s1 s2] returns a sampler that generates a value
        of type ['a * 'b] using [s1] and [s2] to generate values of
        type ['a] and ['b], respectively, on each call. *)
    val sample_pair : 'a sampler -> 'b sampler -> ('a * 'b) sampler

    (** [sample_alternatively s] returns a sampler that mimics
       randomly the behavior of one of [s] sampler and change at each
       call. *)
    val sample_alternatively : 'a sampler list -> 'a sampler

    (** [sample_case cases] returns a sampler that generates randomly
       one of the value of cases. *)
    val sample_cases : 'a list -> 'a sampler

    (** [sample_option sample] returns a sampler that generates an ['a
       option] value using [sample] to generate an ['a] value if
       necessary. *)
    val sample_option : 'a sampler -> 'a option sampler

    (**  {2 Utilities} *)

    val printable_fun : string -> (_ -> _ as 'f) -> 'f
  end

  (** {1 Grading functions for references and variables } *)

  (** Grading function for variables and references. *)
  module Test_functions_ref_var : sig

    (** [test_ref ty got exp] returns {!LearnOcaml_report.Success 1}
       report if reference [got] value is equal to [exp] and
       {!LearnOcaml_report.Failure} report otherwise.

        {e WARNING:} contrary to other grading functions, you cannot
       use this function to evaluate a reference defined or modified
       in student's code. In this case, you should use
       {{!Mutation}mutation functions}. This function should be used
       for a reference defined locally (in [test.ml]). *)
    val test_ref :
      'a Ty.ty -> 'a ref -> 'a -> Learnocaml_report.t

    (** [test_variable ty name r] returns {!LearnOcaml_report.Success
        1} report if variable named [name] exists and is equal to
        [r]. Otherwise returns {!LearnOcaml_report.Failure} report.*)
    val test_variable :
      'a Ty.ty -> string -> 'a -> Learnocaml_report.t

    (** [test_variable_property ty name cb] returns the report
        resulting of application of cb to variable named [name] if it
        exists.  Otherwise returns {!LearnOcaml_report.Failure} report.  *)
    val test_variable_property :
      'a Ty.ty -> string -> ('a -> Learnocaml_report.t) -> Learnocaml_report.t

    (** [test_variable ty name r] returns {!LearnOcaml_report.Success
        1} report if variable named [name] exists and is equal to
        variable with the same name defined in solution. Otherwise returns
        {!LearnOcaml_report.Failure} report.*)
    val test_variable_against_solution :
      'a Ty.ty -> string -> Learnocaml_report.t

  end

  (** {1 Grading functions for types} *)

  (** Grading function for types. *)
  module Test_functions_types : sig

    val compatible_type : expected:string -> string -> Learnocaml_report.t

    val existing_type : ?score:int -> string -> bool * Learnocaml_report.t

    val abstract_type :
      ?allow_private:bool -> ?score:int -> string -> bool * Learnocaml_report.t

    val test_student_code :
      'a Ty.ty -> ('a -> Learnocaml_report.t) -> Learnocaml_report.t

    val test_module_property :
      'a Ty.ty -> string -> ('a -> Learnocaml_report.t) -> Learnocaml_report.t
  end

  (** {1 Grading functions for functions }*)

  (** Grading function for functions. *)
  module Test_functions_function : sig

    (** {2:test_functions_fun_sec Grading functions for functions}*)

    (** Three grading functions for functions are defined for arity one
       to four functions:

  - [test_function_<args_nb> ty name tests]

  - [test_function_<args_nb>_against ty name rf tests]

  - [test_function_<args_nb>_against_solution ty name tests]

       It tests [args_nb]-arity function named [name] with
       non-polymorphic type [ty] (a polymorphic function must be
       tested on a completely determined type) through tests
       [tests]. If a function named [name] is defined in the student
       code and has the right type (or a more generic one), tests
       [tests] are checked one by one and the function returns a
       report concatening reports of each test. Otherwise a
       {!Learnocaml_report.Failure} report is returned.*)


    (** {3 Returned report}*)

    (** The grading functions for functions return a {!report} which
       actually concatenated 4 reports generated by (in this order):

      - the tester [~test]

      - the IO tester [~test_stdout]

      - The IO tester [~test_stderr]

      - the post-processing result function [after].

      Each of this report can be empty. However by default, [~test]
       always returns a non-empty report while the other three returns
       empty reports. *)


    (** {3 Unary functions}*)

    (** [test_function_1 ty name tests] tests the function named
       [name] by directly comparing obtained outputs against expected
       outputs.

     A test [(arg-1, r, out, err)] results in a
       {!LearnOcaml_report.Success 1} report if the student function
       applied to [arg-1] is equal to [r] and if standard output and
       standard error messages match [out] and [err] respectively. The
       result of a test is a {!Learnocaml_report.Failure} report otherwise.

     See {{!optional_arguments_sec} this section} for information about optional
       arguments. *)
    val test_function_1 :
      ?test: 'b tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before : ('a -> unit) ->
      ?after : ('a -> ('b * string * string)
                -> ('b * string * string)
                -> Learnocaml_report.t) ->
      ('a -> 'b) Ty.ty -> string
      -> ('a * 'b * string * string) list -> Learnocaml_report.t

    (** [test_function_1_against ty name rf tests] tests the function
       named [name] by comparing outputs obtained with the student
       function against outputs of [rf].

     A test [arg-1] results in a {!LearnOcaml_report.Success 1} report
       if the student function applied to [arg-1] gives the same
       result than the solution function [rf] applied to [arg-1]. Otherwise
       the result of a test is a {!Learnocaml_report.Failure} report.

     See {{!optional_arguments_sec} this section} for information about optional
       arguments. *)
    val test_function_1_against :
      ?gen: int ->
      ?test: 'b tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> unit) ->
      ?before_user : ('a -> unit) ->
      ?after : ('a
                -> ('b * string * string)
                -> ('b * string * string)
                -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a) ->
      ('a -> 'b) Ty.ty -> string -> ('a -> 'b) -> 'a list -> Learnocaml_report.t

    (** [test_function_1_against_solution ty name tests] tests the
       function named [name] by comparison to solution function [rf]
       which must be defined under name [name] in the corresponding
       [solution.ml] file.

     A test [arg-1] results in a {!LearnOcaml_report.Success 1} report
       if the student function applied to [arg-1] gives the same
       result than the solution function [rf] applied to
       [arg-1]. Otherwise the result of a test is a
       {!Learnocaml_report.Failure} report.

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_1_against_solution :
      ?gen: int ->
      ?test: 'b tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> unit) ->
      ?before_user : ('a -> unit) ->
      ?after : ('a
                -> ('b * string * string)
                -> ('b * string * string)
                -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a) ->
      ('a -> 'b) Ty.ty -> string -> 'a list -> Learnocaml_report.t

    (** [test_function_1_against_postcond postcond ty name tests] tests that
       the function named [name] satisfies the postcondition [postcond].

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_1_against_postcond :
      ?gen: int ->
      ?test_stdout: io_postcond ->
      ?test_stderr: io_postcond ->
      ?before_reference : ('a -> unit) ->
      ?before_user : ('a -> unit) ->
      ?after : ('a -> ('b * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a) ->
      ('a -> 'b Ty.ty -> 'b result -> Learnocaml_report.t) ->
      ('a -> 'b) Ty.ty -> string -> 'a list -> Learnocaml_report.t

    (** {3 Binary functions }*)

    (** [test_function_2 ty name tests] tests the function named
       [name] by directly comparing obtained outputs against expected
       outputs.

     A test [(arg-1, arg-2, r, out, err)] results in a
       {!LearnOcaml_report.Success 1} report if the student function
       applied to [arg-1] and [arg-2] is equal to [r] and if standard
       output and standard error messages match [out] and [err]
       respectively. The result of a test is a
       {!Learnocaml_report.Failure} report otherwise.

     See {{!optional_arguments_sec} this section} for information about optional
       arguments. *)
    val test_function_2 :
      ?test: 'c tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before : ('a -> 'b -> unit) ->
      ?after : ('a -> 'b -> ('c * string * string)
                -> ('c * string * string)
                -> Learnocaml_report.t) ->
      ('a -> 'b -> 'c) Ty.ty
      -> string
      -> ('a * 'b * 'c * string * string) list
      -> Learnocaml_report.t

    (** [test_function_2_against ty name rf tests] tests the function
       named [name] by comparing outputs obtained with the student
       function against outputs of [rf].

     A test [(arg-1, arg-2)] results in a {!LearnOcaml_report.Success
       1} report if the student function applied to [arg-1] and
       [arg-2] gives the same result than the solution function [rf]
       applied to the same arguments. Otherwise the result of a test is a
       {!Learnocaml_report.Failure} report.

     See {{!optional_arguments_sec} this section} for information about optional
       arguments. *)
    val test_function_2_against :
      ?gen: int ->
      ?test: 'c tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> unit) ->
      ?before_user : ('a -> 'b -> unit) ->
      ?after : ('a -> 'b
                -> ('c * string * string)
                -> ('c * string * string)
                -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b) ->
      ('a -> 'b -> 'c) Ty.ty -> string
      -> ('a -> 'b -> 'c)
      -> ('a * 'b) list
      -> Learnocaml_report.t

    (** [test_function_2_against_soltion ty name tests] tests the function
       named [name] by comparison to solution function [rf] which must
       be defined under name [name] in the corresponding [solution.ml]
       file.

     A test [(arg-1, arg-2)] results in a {!LearnOcaml_report.Success
       1} report if the student function applied to [arg-1] and
       [arg-2] gives the same result than the solution function [rf]
       applied to the same arguments. Otherwise the result of a test
       is a {!Learnocaml_report.Failure} report.

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_2_against_solution :
      ?gen: int ->
      ?test: 'c tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> unit) ->
      ?before_user : ('a -> 'b -> unit) ->
      ?after : ('a -> 'b
                -> ('c * string * string)
                -> ('c * string * string)
                -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b) ->
                 ('a -> 'b -> 'c) Ty.ty -> string -> ('a * 'b) list -> Learnocaml_report.t

    (** [test_function_2_against_postcond postcond ty name tests] tests that
       the function named [name] satisfies the postcondition [postcond].

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_2_against_postcond :
      ?gen: int ->
      ?test_stdout: io_postcond ->
      ?test_stderr: io_postcond ->
      ?before_reference : ('a -> 'b -> unit) ->
      ?before_user : ('a -> 'b -> unit) ->
      ?after : ('a -> 'b -> ('c * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b) ->
      ('a -> 'b -> 'c Ty.ty -> 'c result -> Learnocaml_report.t) ->
      ('a -> 'b -> 'c) Ty.ty -> string -> ('a * 'b) list -> Learnocaml_report.t

    (** {3 Three-arguments functions }*)

    (** [test_function_3 ty name tests] tests the function named
       [name] by directly comparing obtained outputs against expected
       outputs.

     A test [(arg-1, arg-2, arg-3, r, out, err)] results in a
       {!LearnOcaml_report.Success 1} report if the student function
       applied to [arg-1], [arg-2] and [arg-3] is equal to [r] and if
       standard output and standard error messages match [out] and
       [err] respectively. The result of a test is a
       {!Learnocaml_report.Failure} report otherwise.

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_3 :
      ?test: 'd tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before : ('a -> 'b -> 'c -> unit) ->
      ?after : ('a -> 'b -> 'c
                -> ('d * string * string)
                -> ('d * string * string) -> Learnocaml_report.t)
      -> ('a -> 'b -> 'c -> 'd) Ty.ty -> string
      -> ('a * 'b * 'c * 'd * string * string) list
      -> Learnocaml_report.t

    (** [test_function_3_against ty name rf tests] tests the function
       named [name] by comparing outputs obtained with the student
       function against outputs of [rf].

     A test [(arg-1, arg-2, arg-3)] results in a
       {!LearnOcaml_report.Success 1} report if the student function
       applied to [arg-1], [arg-2] and [arg-3] gives the same result
       than the solution function [rf] applied to the same
       arguments. Otherwise the result of a test is a
       {!Learnocaml_report.Failure} report.

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_3_against :
      ?gen: int ->
      ?test: 'd tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> 'c -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> unit) ->
      ?after : ('a -> 'b -> 'c
                -> ('d * string * string)
                -> ('d * string * string)
                -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c) ->
      ('a -> 'b -> 'c -> 'd) Ty.ty
      -> string
      -> ('a -> 'b -> 'c -> 'd)
      -> ('a * 'b * 'c) list
      -> Learnocaml_report.t

    (** [test_function_3_against_solution ty name tests] tests the function
       named [name] by comparison to solution function [rf] which must
       be defined under name [name] in the corresponding [solution.ml]
       file.

     A test [(arg-1, arg-2, arg-3)] results in a
       {!LearnOcaml_report.Success 1} report if the student function
       applied to [arg-1], [arg-2] and [arg-3] gives the same result
       than the solution function [rf] applied to the same
       arguments. Otherwise the result of a test is a
       {!Learnocaml_report.Failure} report.

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_3_against_solution :
      ?gen: int ->
      ?test: 'd tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> 'c -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> unit) ->
      ?after : ('a -> 'b -> 'c
                -> ('d * string * string)
                -> ('d * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c) ->
      ('a -> 'b -> 'c -> 'd) Ty.ty
      -> string -> ('a * 'b * 'c) list
      -> Learnocaml_report.t

    (** [test_function_3_against_postcond postcond ty name tests] tests that
       the function named [name] satisfies the postcondition [postcond].

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_3_against_postcond :
      ?gen: int ->
      ?test_stdout: io_postcond ->
      ?test_stderr: io_postcond ->
      ?before_reference : ('a -> 'b -> 'c -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> unit) ->
      ?after : ('a -> 'b -> 'c -> ('d * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c) ->
      ('a -> 'b -> 'c -> 'd Ty.ty -> 'd result -> Learnocaml_report.t) ->
      ('a -> 'b -> 'c -> 'd) Ty.ty -> string -> ('a * 'b * 'c) list -> Learnocaml_report.t

    (** {3 Four-arguments functions }*)

    (** [test_function_4 ty name tests] tests the function named
       [name] by directly comparing obtained outputs against expected
       outputs.

     A test [(arg-1, arg-2, arg-3, arg-4, r, out, err)] results in a
       {!LearnOcaml_report.Success 1} report if the student function
       applied to [arg-1], [arg-2], [arg-3] and [arg-4] is equal to
       [r] and if standard output and standard error messages match
       [out] and [err] respectively. The result of a test is a
       {!Learnocaml_report.Failure} report otherwise.

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_4 :
      ?test: 'e tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?after : ('a -> 'b -> 'c -> 'd
                -> ('e * string * string)
                -> ('e * string * string)
                -> Learnocaml_report.t)
      -> ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty -> string
      -> ('a * 'b * 'c * 'd * 'e * string * string) list
      -> Learnocaml_report.t

    (** [test_function_4_against ty name rf tests] tests the function
       named [name] by comparing outputs obtained with the student
       function against outputs of [rf].

     A test [(arg-1, arg-2, arg-3m arg-4)] results in a
       {!LearnOcaml_report.Success 1} report if the student function
       applied to [arg-1], [arg-2], [arg-3] and [arg-4] gives the same
       result than the solution function [rf] applied to the same
       arguments. Otherwise the result of a test is a
       {!Learnocaml_report.Failure} report.

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_4_against :
      ?gen: int ->
      ?test: 'e tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?after : ('a -> 'b -> 'c -> 'd
                -> ('e * string * string)
                -> ('e * string * string)
                -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c * 'd)
      -> ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty -> string
      -> ('a -> 'b -> 'c -> 'd -> 'e)
      -> ('a * 'b * 'c * 'd) list -> Learnocaml_report.t

    (** [test_function_4_against_solution ty name tests] tests the
       function named [name] by comparison to solution function [rf]
       which must be defined under name [name] in the corresponding
       [solution.ml] file.

     A test [(arg-1, arg-2, arg-3, arg-4)] results in a
       {!LearnOcaml_report.Success 1} report if the student function
       applied to [arg-1], [arg-2], [arg-3] and [arg-4] gives the same
       result than the solution function [rf] applied to the same
       arguments. Otherwise the result of a test is a
       {!Learnocaml_report.Failure} report.

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_4_against_solution :
      ?gen: int ->
      ?test: 'e tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?after : ('a -> 'b -> 'c -> 'd
                -> ('e * string * string)
                -> ('e * string * string)
                -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c * 'd)
      -> ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty -> string
      -> ('a * 'b * 'c * 'd) list -> Learnocaml_report.t

    (** [test_function_4_against_postcond postcond ty name tests] tests that
       the function named [name] satisfies the postcondition [postcond].

     See {{!optional_arguments_sec} this section} for information
       about optional arguments. *)
    val test_function_4_against_postcond :
      ?gen: int ->
      ?test_stdout: io_postcond ->
      ?test_stderr: io_postcond ->
      ?before_reference : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?after : ('a -> 'b -> 'c -> 'd -> ('e * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c * 'd) ->
      ('a -> 'b -> 'c -> 'd -> 'e Ty.ty -> 'e result -> Learnocaml_report.t) ->
      ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty -> string -> ('a * 'b * 'c * 'd) list -> Learnocaml_report.t

    (** {2:optional_arguments_sec Optional arguments for grading functions} *)

    (** The various grading functions use numerous common optional
       argument. Here is a list in alphabetic order of each of them.

    {3 ?⁠after}
    defines a function which is called with the current
       tested inputs, the student {!type:result} and the solution
       {!type:result} and returns a new report which is concatenated to
       reports built with [~test], [~test_sdtout] and [~test_sdterr].
       Enables for example to inspect references introduced with
       [~before], [~before_user] or [~before_reference] and build an
       appropriate report. Default value is [fun _ _ _ -> []].

    {3 ?before}
    defines a function called right before the application
       of student function to the current tested inputs. Default value
       is [fun _ -> ()]

     For [test_function_<args_nb>] only.

    {3 ?before_reference}
    defines a function called right before the
       application of solution function to the current tested
       inputs. This function is called {b before} student function
       evaluation. Default value is [fun _ -> ()].

     For [test_function_<args_nb>_against] and
       [test_function_<args_nb>_against_solution].

    {3 ?before_user}
    defines a function called right before the
       application of student function to the current tested
       inputs. This function is called {b after} solution function
       evaluation. Default value is [fun _ -> ()].

     For [test_function_<args_nb>_against] and
       [test_function_<args_nb>_against_solution].

    {3 ?gen}
    Number of automatically generated tested inputs. Inputs
       are generated using either sampler defined in the current
       environment or function defined with [~sampler] optional
       argument. By default, [gen] is [max 5 (10 - List.length
       tests)].

     For [test_function_<args_nb>_against] and
       [test_function_<args_nb>_against_solution].

     See {{!Sampler.sampler_sec}Sampler module}.

    {3 ?sampler}
    defines the function used to automatically generated
       inputs. If unset, the grading function checks if a sampler is
       defined for each input type in the current environment. Such
       sampler for a type [some-type] must be named [sample_some-type]
       and have a type [unit -> some-type] if not parametric or [(unit
       -> 'a) -> (unit -> 'b) -> ... -> unit -> some-type] otherwise.

     For [test_function_<args_nb>_against] and
       [test_function_<args_nb>_against_solution].

     See {{!Sampler.sampler_sec}Sampler module}.

    {3 ?test}
    defines the function used to compare the output of
       student function and the output of solution function. Default
       value is {!Tester.test}.

     See {{!Tester.tester_sec}predefined testers and tester builders}.

    {3 ?test_sdterr}
    defines the function used to compare the standard
       output produced by student function and the one produced by
       solution function. Default value is {!Tester.io_test_ignore}.

     See {{!Tester.io_tester_sec}predefined IO testers and IO tester
       builders}.

    {3 ?test_sdtout}
    defines the function used to compare the standard
       error produced by student function and the one produced by
       solution function. Default value is {!Tester.io_test_ignore}.

     See {{!Tester.io_tester_sec}predefined IO testers and IO tester
       builders}.  *)
  end

  (** {1 Generic grading functions} *)

  (** Grading functions for functions with more than 4 arguments. Most
     of the functions here should not be used. If you actually need
     a grading function for a function with more than 4 arguments, you
     should instead raise an issue to ask for the corresponding
     grading functions.  *)
  module Test_functions_generic : sig

    (** [run_timeout ?time v] executes [v()] under an optional time limit.
        The exceptions raised by [v] are intentionally *not* caught,
        so the caller is able to catch and get a backtrace, if desired.
        If given, [time] overrides the global timeout parameter.
    *)
    val run_timeout : ?time:int -> (unit -> 'a) -> 'a

    (** [exec v] executes [v ()] and returns [Ok (r, stdout, stderr)]
        if no exception is raised and where [r] is the result of [v
        ()], [stdout] the standard output string (possibly empty) and
        [stderr] the standard error string (possibly empty) or returns
        [Error exn] is exception [exn] is raised. In particular, a
        timeout error [Error (Timeout limit)] can be returned. *)
    val exec : (unit -> 'a) -> ('a * string * string) result

    (** [result v] executes [v ()] and returns [Ok r] where [r] is
        the result of [v ()] or [Error exn] if exception [exn] is
        raised. In particular, a timeout error [Error (Timeout limit)]
        can be returned. *)
    val result : (unit -> 'a) -> 'a result

    include (module type of Fun_ty
                            with type ('a, 'b, 'c) args = ('a, 'b, 'c) Fun_ty.args
                            and type ('a, 'b, 'c) fun_ty = ('a, 'b, 'c) Fun_ty.fun_ty)

    val ty_of_prot :
      (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty -> ('ar -> 'row) Ty.ty
    [@@ocaml.deprecated "Use ty_of_fun_ty instead."]

    (** {2 Lookup functions} *)

    type 'a lookup =
      unit
      -> [ `Found of string * Learnocaml_report.t * 'a
         | `Unbound of string * Learnocaml_report.t ]

    val lookup : 'a Ty.ty -> ?display_name: string -> string -> 'a lookup
    val lookup_student : 'a Ty.ty -> string -> 'a lookup
    val lookup_solution : 'a Ty.ty -> string -> 'a lookup
    val found : string -> 'a -> 'a lookup
    val name : 'a lookup -> string

    (** {2 Generic grading functions}*)

    (** [test_value lookup cb] *)
    val test_value :
      'a lookup -> ('a -> Learnocaml_report.t) -> Learnocaml_report.t

    (** [test_function ~test ~test_stdout ~test_stderr ~before ~after
        prot uf tests]  *)
    val test_function :
      ?test: 'ret tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before :
        (('ar -> 'row, 'ar -> 'urow, 'ret) args ->
         unit) ->
      ?after :
        (('ar -> 'row, 'ar -> 'urow, 'ret) args ->
         ('ret * string * string) ->
         ('ret * string * string) ->
         Learnocaml_report.t) ->
      (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
      ('ar -> 'row) lookup ->
      (('ar -> 'row, 'ar -> 'urow, 'ret) args * (unit -> 'ret)) list ->
      Learnocaml_report.t

    (** [test_function_against ~gen ~test ~test_stdout ~test_stderr
        ~before_reference ~before_user ~after ~sampler prot uf rf tests] *)
    val test_function_against :
      ?gen: int ->
      ?test: 'ret tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference :
        (('ar -> 'row, 'ar -> 'urow, 'ret) args -> unit) ->
      ?before_user :
        (('ar -> 'row, 'ar -> 'urow, 'ret) args -> unit) ->
      ?after :
        (('ar -> 'row, 'ar -> 'urow, 'ret) args ->
         ('ret * string * string) ->
         ('ret * string * string) ->
         Learnocaml_report.t) ->
      ?sampler:
        (unit -> ('ar -> 'row, 'ar -> 'urow, 'ret) args) ->
      (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
      ('ar -> 'row) lookup -> ('ar -> 'row) lookup ->
      ('ar -> 'row, 'ar -> 'urow, 'ret) args list ->
      Learnocaml_report.t

    (** [test_function_against_solution ~gen ~test ~test_stdout ~test_stderr
        ~before_reference ~before_user ~after ~sampler prot name tests] *)
    val test_function_against_solution :
      ?gen:int ->
      ?test: 'ret tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference:
        (('ar -> 'row, 'ar -> 'urow, 'ret) args -> unit) ->
      ?before_user:
        (('ar -> 'row, 'ar -> 'urow, 'ret) args -> unit) ->
      ?after:
        (('ar -> 'row, 'ar -> 'urow, 'ret) args ->
         'ret * string * string ->
         'ret * string * string ->
         Learnocaml_report.item list) ->
      ?sampler:
        (unit -> ('ar -> 'row, 'ar -> 'urow, 'ret) args) ->
      (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
      string ->
      ('ar -> 'row, 'ar -> 'urow, 'ret) args list ->
      Learnocaml_report.item list

    (** Helper notation to test pure functions.

        [p ==> r] is the pair [(p, fun () -> r)].

        Example: [test_function prot
                  (lookup_student (ty_of_fun_ty prot) name)
                  [1 @: 2 @: 3 @: 4 @:!! 5 ==> 15; ... ==> ...]] *)
    val (==>) : 'params -> 'ret -> 'params * (unit -> 'ret)
  end

    (** [r1 @@@ r2] is the function [x -> r1 x @ r2 x]. *)
   val (@@@) :
     ('a -> Learnocaml_report.t)
     -> ('a -> Learnocaml_report.t)
     -> ('a -> Learnocaml_report.t)

   (** [r1 @@> f] returns [r1] if [r1] is a failure report,
       otherwise returns the result of report generator
       [f ()]. *)
   val (@@>) :
     Learnocaml_report.t
     -> (unit -> Learnocaml_report.t)
     -> Learnocaml_report.t

   (** [r1 @@= f] returns [r1] if [r1] is a failure report,
       otherwise concatenates [r1] to the result of report
       generator [f ()]. *)
   val (@@=) :
     Learnocaml_report.t
     -> (unit -> Learnocaml_report.t)
     -> Learnocaml_report.t

   (**/**)
   include (module type of Ast_checker)
   include (module type of Tester)
   include (module type of Mutation)
   include (module type of Sampler)
   include (module type of Test_functions_types)
   include (module type of Test_functions_ref_var)
   include (module type of Test_functions_function)
   include (module type of Test_functions_generic)
end

module Make : functor
  (Params : sig
     val results : Learnocaml_report.t option ref
     val set_progress : string -> unit
     val timeout : int option
     module Introspection : Introspection_intf.INTROSPECTION
   end) -> S
