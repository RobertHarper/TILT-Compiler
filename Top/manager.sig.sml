signature MANAGER =
sig

    (*
	DiagLevel determines how verbose the compiler is; zero (the
	default) is quiet and higher numbers are more verbose.
    *)
    val DiagLevel : int ref	(* Print diagnostics. *)	
    val PrintStats : bool ref	(* Print stats after compilation. *)
    val ResetStats : bool ref	(* Reset stats before compilation. *)

    type label
    type targets = label list

    val unit : string -> label
    val interface : string -> label

    (*
	These run a master only.
    *)
    val make' : string * targets -> unit
    val make_exe' : string * string * targets -> unit	(* project, exe *)
    val make_lib' : string * string * targets -> unit	(* project, lib *)

    (*
	These run a master and a slave together.
    *)
    val make : string * targets -> unit
    val make_exe : string * string * targets -> unit	(* project, exe *)
    val make_lib : string * string * targets -> unit	(* project, lib *)
    val purge : string * targets -> unit
    val purgeAll : string * targets -> unit

    (*
	Slave makes the current process act as a slave.  Slaves
	launches slaves and immediately terminates.
    *)
    val slave : unit -> 'a
    val slaves : (int * string) list -> unit

end
