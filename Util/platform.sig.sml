(*
	Objtype is the platform we are targeting; a backend must
	exist.  Cputype is the platform we are compiling on; a backend
	need not exist as TILT can run under other compiler runtimes.
*)

signature PLATFORM =
sig
    datatype objtype = SPARC | ALPHA | TALx86

    val littleEndian : objtype -> bool
    val toString : objtype -> string
    val fromString : string -> objtype option
    val blastOutObjtype : Blaster.outstream -> objtype -> unit
    val blastInObjtype : Blaster.instream -> objtype

    datatype cputype = UNSUPPORTED | SUPPORTED of objtype

    val cputype : unit -> cputype

    val cputypeToString : cputype -> string
    val blastOutCputype : Blaster.outstream -> cputype -> unit
    val blastInCputype : Blaster.instream -> cputype

    val hostname : unit -> string
    val pid : unit -> Word32.word
    val sleep : real -> unit
end
