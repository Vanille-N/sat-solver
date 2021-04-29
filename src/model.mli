(* a model *)
type m

type literal = int

(* create a new model with a fixed size *)
val make : int -> m

(* test if a literal is satisfied (unassigned is not satisfied) *)
val sat : m -> literal -> bool

(* test if a literal is assigned *)
val assigned : m -> literal -> bool

(* assign value to literal *)
val add : m -> literal -> unit

(* unassign all hypothesis made after a certain literal was set *)
val remove : m -> literal -> unit

(* pretty-print *)
val pp : Format.formatter -> m -> unit
