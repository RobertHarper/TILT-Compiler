(*$import Prelude *)

(* XXX: we want compress, uncompress, dot, ps, etc all here. *)

signature TOOLS =
sig

    val showTools : bool ref			(* show tool invocations *)
    val debugAsm : bool ref			(* Make assembler include debugging information. *)
    val profile : bool ref		        (* Link against monitor for prof(1) support *)
	
    val assemble : string * string -> unit	(* assembler file, object file *)
    val link : string list * string -> unit	(* object files, executable name *)

end
