signature LINKIL =
sig

    type filepos = LinkParse.filepos
    type label = Il.label
    type module = Il.module
    type sc_module = Il.sc_module

    val LinkIlDiag : bool ref
    val ShowHIL : bool ref
    val ShowInterface : bool ref
    val ShowContext : bool ref
    val LinkIlDebug : bool ref
    val compiling_tiltprim : bool ref

    type pinterface
    type precontext = (label * pinterface) list
    type opened = label list

    val blastOutPinterface : Blaster.outstream -> pinterface -> unit
    val blastInPinterface  : Blaster.instream -> pinterface
    val blastInPinterfaceParm : Blaster.instream -> Name.LabelSet.set

    val parameters : pinterface -> Name.LabelSet.set

    val eq : precontext * pinterface * pinterface -> bool

    val elab_topspec : precontext * opened * filepos * Ast.topspec
	-> pinterface option
    val elab_primspec : unit -> pinterface
    val elab_topdec : precontext * label * opened * filepos * Ast.dec
	-> (module * pinterface) option
    val elab_sealed_topdec :
	precontext * label * opened * filepos * Ast.dec * pinterface
	-> module option
    val elab_primdec : label * pinterface -> module option

    val sc_module : precontext * label * pinterface -> Il.sc_module

end
