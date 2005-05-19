signature TOOLS =
sig

	val ToolsDiag : bool ref
	val ShowTools : bool ref	(* show tool invocations *)
	val DebugRuntime : bool ref (* Link against debugging runtime *)
	val Profile : bool ref		(* Link against monitor for prof(1) support *)

	val sparcas : {obj:string, asm:string} -> unit
	val sparcld : {exe:string, objs:string list} -> unit

	val talx86as : {verify:bool, obj:string, tobj:string, incs:string list, asm:string} -> unit
	val talx86ld : {verify:bool, exe:string, incs:string list, objs:string list} -> unit

	val compress : {src : string, dest : string} -> unit
	val uncompress : {src : string, dest : string} -> unit

	val nproc : unit -> int
end
