signature TOOLS =
sig

    val ToolsDiag : bool ref
    val ShowTools : bool ref	(* show tool invocations *)
    val DebugAsm : bool ref	(* Make assembler include debugging information. *)
    val Profile : bool ref	(* Link against monitor for prof(1) support *)

    val assemble : string * string -> unit	(* assembler file, object file *)
    val link : string list * string -> unit	(* object files, executable name *)

    val compress : {src : string, dest : string} -> unit
    val uncompress : {src : string, dest : string} -> unit
end
