(** An implementation of circular doubly-linked lists *)

(** A list with elements of type ['elt]
    and optional markers of type ['mk] *)
type ('elt, 'mk) t

(** Create a new empty list with optional marks *)
val make : unit -> ('elt, 'mk) t

(** Add a new node with its contents and put the cursor on it *)
val insert : ('elt, _) t -> 'elt -> unit

(** Take a look at the contents of the node under the cursor *)
val peek : ('elt, _) t -> 'elt option

(** Remove the node under the cursor,
    move the cursor forward *)
val take : ('elt, _) t -> 'elt option

(** Set the marker for the node under the cursor *)
val set_mark : (_, 'mk) t -> 'mk option -> unit

(** Read the marker for the node under the cursor *)
val get_mark : (_, 'mk) t -> 'mk option

(** Move the cursor forward *)
val rotate : ('elt, _) t -> unit
