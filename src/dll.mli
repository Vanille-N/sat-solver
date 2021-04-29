type ('elt, 'mk) t
val make : unit -> ('elt, 'mk) t
val insert : ('elt, _) t -> 'elt -> unit
val peek : ('elt, _) t -> 'elt option
val take : ('elt, _) t -> 'elt option
val set_mark : (_, 'mk) t -> 'mk option -> unit
val get_mark : (_, 'mk) t -> 'mk option
val rotate : ('elt, _) t -> unit
