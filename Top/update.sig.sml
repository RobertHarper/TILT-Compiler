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

	The interface for plan_link and plan_pack could be simpler.
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

    type iface = Compiler.iface
    type precontext = Compiler.precontext
    type imports = Compiler.imports
    type ue = UnitEnvironment.ue
    type equiv = Crc.crc * Crc.crc -> bool	(* interface CRC equivalence *)
    type var = Name.var
    type unit_help =
	{parms : string -> var list,
	 get_id : var -> string,
	 get_unit_paths : var -> Paths.compunit option}
    type iface_help =
	{get_id : var -> string,
	 fresh : ExtSyn.id -> ExtSyn.id,
	 find : ExtSyn.id -> var,
	 get_iface_paths : var -> Paths.iface option}
    type importOnly = var -> bool

    type plan

    val empty_plan : plan

    val isEmpty : plan -> bool	(* compiling has no effect *)
    val isReady : plan -> bool	(* dependents can be compiled in parallel *)
    val sendToSlave : plan -> bool
    val ackInterface : plan -> bool

    val blastOutPlan : Blaster.outstream -> plan -> unit
    val blastInPlan : Blaster.instream -> plan

    val plan_compi : equiv * ue * Paths.iface -> plan
    val plan_compu : equiv * ue * Paths.compunit -> plan
    val plan_srci : equiv * precontext * imports * Paths.iface -> plan
    val plan_compile : equiv * precontext * imports * Paths.compunit -> plan
    val plan_checku : equiv * precontext * Paths.compunit * Paths.iface -> plan
    val plan_link : equiv * unit_help * var list * Paths.exe -> plan
    val plan_pack : equiv * unit_help * iface_help * importOnly * var list * Paths.lib -> plan

    val compile : plan -> unit -> plan
    val flush : plan -> unit
    val flushAll : unit -> unit

end
