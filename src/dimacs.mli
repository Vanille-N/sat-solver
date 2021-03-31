(** Manage SAT problem descriptions,
  * with input and output to DIMACS file format.
  *
  * This module uses global state to maintain an implicit set of declared
  * variables and clauses. *)

(** Litterals, i.e. propositional variables or negations of variables.
  * We have no type for variables. *)
type literal

(** Negation of a literal. *)
val not : literal -> literal

(** Number of variables currently declared. *)
val nb_variables : unit -> int

(** Convert a literal to an integer: positive literals are mapped to positive
  * integers that are less or equal to [nb_variables ()]; negative literals
  * are mapped to the opposite negative integers. *)
val to_int : literal -> int

type clause = literal list

(** {2 Variable declaration} *)

(** Indices are used to create arrays of arrays of ... literals.
  * A value [S _ (S _ ... O)] with [n] uses of constructor [C]
  * is of type [(variable array^n) index] and it can be used to
  * create an array of variables of dimension [n]. The integers
  * are the sizes of the array for the given dimension. *)
type _ index = O : literal index | S : int * 'a index -> 'a array index

(** Create an n-dimensional array of variables (i.e. positive literals),
  * where the dimension n and the sizes on respective dimensions are given
  * by an index. *)
val make : 'a index -> 'a

(** {3 Notations for creating indices} *)

(** These notations notably allow to write
  * [let x = Dimacs.(make (2**i**o))] to create a two-dimensional
  * array of size 2 on dimension 1 and size [i] on dimension 2,
  * i.e. such that [x.(1).(i-1)] is a variable. *)
val ( ** ) : int -> 'a index -> 'a array index
val o : literal index

(** {2 Clause declaration} *)

(** Clauses, i.e. disjunctions of literals, are represented as lists
  * of literals. *)

(** [bigor n f] returns the clause [f 0 \/ ... \/ f (n-1)].
  * Indices passed to [f] start at [0]: they are meant to be
  * used as array indices, not as literals. *)
val bigor : int -> (int -> literal) -> clause

(** Add a clause to the problem. *)
val add_clause : literal list -> unit

(** Get the current problem as a list of clauses. *)
val get_clauses : unit -> clause list

(** {2 DIMACS input/output} *)

(** Output the problem in DIMACS format on some output channel. *)
val output : out_channel -> unit

(** Read a SAT problem in DIMACS format from some input channel.
  * This modifies the current set of variables and clauses,
  * erasing previous declarations. *)
val input : in_channel -> unit

(** {2 Models} *)

(** A model (assignment) for the current set of propositional variables. *)
type model

(** Read model from some input channel in DIMACS format.
  * Comments are not supported. *)
val read_model : in_channel -> model

(** Tell whether a literal is satisfied in some model. *)
val sat : model -> literal -> bool

(** {2 Runners} *)

(** Runners are used to launch problem encoders and provide them with
  * a common command-line interface.
  *
  * The binaries will take two arguments,
  * the first one being "problem" or "solution" (or simply "p" and "s")
  * and the second one being the problem description:
  * an integer with [run_int], or
  * a filename as a string with [run].
  *
  * In problem mode, the application declares the problem with the [problem]
  * function, then the problem is written to file "./problem.cnf".
  * In solution mode,
  * the [solution] function is ran, and it is expected to read a model
  * from file "./output.sat" after having declared the appropriate variables. *)

val run : problem:(string -> unit) -> solution:(string -> unit) -> unit

val run_int : problem:(int -> unit) -> solution:(int -> unit) -> unit
