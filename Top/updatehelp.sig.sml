(*$import Prelude FILECACHE *)

signature UPDATE_HELP =
sig
    val compressAsm : bool ref		(* If keeping assembler files, compress them too? *)
	
    structure Cache : FILECACHE
    structure IlCache : FILECACHE
    structure InfoCache : FILECACHE

    type unit_paths			(* parameter *)
    type import				(* parameter *)
    type state
	
    datatype asmfiles = COMPRESSED | UNCOMPRESSED | BOTH | NEITHER
    val goalAsmFiles : unit -> asmfiles
	
    val init : unit_paths * (unit_paths * import) list -> state
    val elaborate : state -> state
    val generate : state -> state
    val prepare : state -> state
    val assemble : state -> state
    val cleanup : state -> state
end
