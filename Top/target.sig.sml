signature TARGET =
sig

    val setTarget : Platform.objtype -> unit
    val getTarget : unit -> Platform.objtype

    (*
	targetString is not the same as Platform.toString o getTarget;
	the former accounts for certain flags that lead to
	incompatible binaries.
    *)
    val targetString : unit -> string

    val native : unit -> bool
    val tal : unit -> bool

    val checkNative : unit -> unit
end
