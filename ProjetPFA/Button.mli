module type Button =
    sig
    val x : int
    val y : int
    val width : int
    val height : int
    val action : unit -> unit
    val drawButton : unit -> unit
    end ;;

