(* Interface to the compiler. *)
signature COMPILER =
sig

    type label = Name.label
    type desc = IntSyn.desc
    type pdec = IntSyn.pdec
    type inputs = IntSyn.inputs

    val CompilerDebug : bool ref
    val CompilerDiag : bool ref
    val CompilerVerbose : bool ref

    val UptoElaborate : bool ref
    val UptoAsm : bool ref
    val KeepAsm : bool ref
    val CompressAsm : bool ref
    val PackUnitSource : bool ref

    val summarize : inputs -> IntSyn.S.summary

    (*
	Gc_desc discards project declarations that are not relevant
	to the named units and interfaces.

	If |- desc ok,
	and labels subset dom(desc),
	and desc' = gc_desc(desc,labels),
	then |- desc' ok,
	and desc' subset desc,
	and support(labels,desc') = dom(desc').
    *)
    val gc_desc : desc * IntSyn.set -> desc

    (*
	If |- desc ok,
	and id in dom(desc),
	and all prerequisites of id in desc have been compiled,
	then get_inputs(desc,id) : inputs.
    *)
    val get_inputs : desc * label -> inputs

    (*
	If (desc,pdec) = inputs,
	and pdec declares a unit with an ascribed interface,
	then compile_int inputs compiles a TAL interface.
    *)
    val compile_int : inputs -> unit

    (*
	Compile(inputs,ack_interface) compiles inputs, calling
	ack_interface once the pinterface and tali files are up to
	date, and returning true if compilation is done and false if
	further assembly (on a native slave) is required.
    *)
    val compile : inputs * (unit -> unit) -> bool

    (*
	Assemble inputs finishes compilation; it should only be used
	when compile returned false and then only on a native system.
    *)
    val assemble : inputs -> unit

    (*
	Link and pack build executables and libraries.
    *)
    val link : desc * string -> unit	(* executable *)
    val pack : desc * string -> unit	(* library directory *)

end
