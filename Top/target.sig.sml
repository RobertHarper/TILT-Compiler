signature TARGET =
sig

    val littleEndian : bool ref	(* Is target little endian? *)

    datatype platform = TIL_ALPHA | TIL_SPARC | TIL_TALx86

    val blastOutPlatform : Blaster.outstream -> platform -> unit
    val blastInPlatform : Blaster.instream -> platform

    val setTargetPlatform : platform -> unit	(* also sets endian-ness *)
    val getTargetPlatform : unit -> platform

    val platformName : platform -> string
    val platformFromName : string -> platform option

    val importantFlags : (string * bool ref * bool) list
    val platformString : unit -> string	(* depends on importantFlags *)

    val native : unit -> bool

    val checkNative : unit -> unit
end
