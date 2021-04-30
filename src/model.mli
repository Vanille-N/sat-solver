(** Efficient model manipulation *)

(** A model *)
type m

(** A single variable *)
type literal = int

(** Create a new model with a fixed number of literals *)
val make : int -> m

(** Test if a literal is satisfied (unassigned is not satisfied) *)
val sat : m -> literal -> bool

(** Test if a literal is assigned *)
val assigned : m -> literal -> bool

(** Assign value to literal *)
val add : m -> literal -> unit

(** Rollback all hypothesis made after a certain literal was set *)
val remove : m -> literal -> unit

(** Pretty-print *)
val pp : Format.formatter -> m -> unit
