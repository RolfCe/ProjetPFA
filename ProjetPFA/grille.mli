module type Grille =
    sig
    type t
    val value : t
    val set : int -> int -> int -> unit
    val get : int -> int -> int
    end
