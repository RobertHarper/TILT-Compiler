signature TOOLS =
sig

    val ToolsDiag : bool ref
    val ShowTools : bool ref	(* show tool invocations *)
    val DebugAsm : bool ref	(* Make assembler include debugging information. *)
    val Profile : bool ref	(* Link against monitor for prof(1) support *)

    val assemble : string list * string * string * string -> unit
	(* tali include directories, assembler file, object file, type-object file *)

    val link : string list * string list * string -> unit
	(* tali include directories, object files, executable name *)

    val compress : {src : string, dest : string} -> unit
    val uncompress : {src : string, dest : string} -> unit
end
