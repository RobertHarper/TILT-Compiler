(* Interface to the compiler. *)
(*
	Gc_desc discards project declarations that are not relevant
	to the named units and interfaces.

	Get_inputs discards project declarations that are not relevant to
	elaborating the named unit or interface.

	Info computes an Info.info value for the project description.
	This is used for cut-off recompilation.

	Compile_int generates a TAL interface (tali) for pdec against
	desc, assuming that all the pinterfaces in desc are up to date and
	that pdec defines a unit with an ascribed interface.

	Compile builds pdec against desc, assuming all the pinterfaces and
	talis in desc are up to date.  Compile returns true if compilation
	is done and false if further assembly (on a native slave) is
	required.  The supplied function is called once the pinterface and
	tali files are up to date.

	Assemble creates an object file from an up to date assembler
	file; it is used to finish compilation jobs.

	Link and pack build executables and libraries.
*)
signature COMPILER =
sig

    type label = Name.label
    type desc = IntSyn.desc
    type pdec = IntSyn.pdec

    val CompilerDebug : bool ref
    val CompilerDiag : bool ref
    val CompilerVerbose : bool ref

    val UptoElaborate : bool ref
    val UptoAsm : bool ref
    val KeepAsm : bool ref
    val CompressAsm : bool ref
    val PackUnitSource : bool ref

    val gc_desc : desc * label list -> desc

    val get_inputs : desc * label -> desc * pdec

    val info : desc * pdec -> IntSyn.info

    val compile_int : desc * pdec -> unit
    val compile : desc * pdec * (unit -> unit) -> bool

    val assemble : desc * pdec -> unit

    val link : IntSyn.desc * IntSyn.F.link -> unit
    val pack : desc * string -> unit	(* library directory *)

end
