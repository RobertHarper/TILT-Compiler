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
	These run a master and, if boolean is true, a slave.
    *)
    val make : bool -> string list * targets -> unit
    val make_exe : bool -> string list * string * targets -> unit	(* projects, exe *)
    val make_lib : bool -> string list * string * targets -> unit	(* projects, lib *)

    val purge : string list * targets -> unit
    val purgeAll : string list * targets -> unit

    (*
	This makes the current process act as a slave.
    *)
    val slave : unit -> 'a

end
