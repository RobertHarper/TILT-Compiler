(*
	The primary purpose of structure Update is to bring up to date
	all target files associated with an interface or unit.  Update
	assumes that all import interfaces, as well as any
	constraining interface, are up to date.  Out of date targets
	are either recompiled or, when the user sets flags that
	prevent compilation, deleted.

	Compilation needs up to three stages.  The first application
	of Compile.compile elaborates to bring the interface up to
	date.  The second application compiles as far as possible,
	returning a non-empty plan when cross-assembly is needed but
	not possible.  The third stage is the master performing any
	unfinished assembly and cleanup.

*)
signature UPDATE =
sig

    val UpdateDiag : bool ref
    val ShowPlan : bool ref
    val ShowStale : bool ref

    val UptoElaborate : bool ref
    val UptoAsm : bool ref
    val KeepAsm : bool ref
    val CompressAsm : bool ref		(* Compress kept assembler. *)

    type context = (string * Paths.iface) list
    type imports = Compiler.imports
    type ue = UnitEnvironment.ue
    type equiv = Crc.crc * Crc.crc -> bool	(* interface CRC equivalence *)
    datatype pack =
	PACKU of Paths.compunit * ExtSyn.id list
      | PACKI of Paths.iface * ExtSyn.id list
      | PACKV of ExtSyn.id * ExtSyn.exp

    type plan

    val empty_plan : plan

    val isEmpty : plan -> bool	(* compiling has no effect *)
    val isReady : plan -> bool	(* dependents can be compiled in parallel *)
    val sendToSlave : plan -> bool
    val ackInterface : plan -> bool

    val blastOutPlan : Blaster.outstream -> plan -> unit
    val blastInPlan : Blaster.instream -> plan
    val pp_plan : plan -> Formatter.format

    val plan_compi : equiv * ue * Paths.iface -> plan
    val plan_compu : equiv * ue * Paths.compunit -> plan
    val plan_srci : equiv * context * imports * Paths.iface -> plan
    val plan_compile : equiv * context * imports * Paths.compunit -> plan
    val plan_checku : equiv * context * Paths.compunit * Paths.iface -> plan
    val plan_link : equiv * Paths.compunit  list * Paths.exe -> plan
    val plan_pack : equiv * pack list * Paths.lib -> plan

    val compile : plan -> unit -> plan
    val flush : plan -> unit
    val flushAll : unit -> unit

end
