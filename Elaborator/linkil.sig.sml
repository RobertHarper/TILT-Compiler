signature LINKIL =
sig

    type filepos = LinkParse.filepos
    type label = Il.label
    type module = Il.module

    val LinkIlDiag : bool ref
    val ShowHIL : bool ref
    val ShowInterface : bool ref
    val ShowContext : bool ref
    val LinkIlDebug : bool ref

    type context
    type interface
    type pinterface

    val blastOutPinterface : Blaster.outstream -> pinterface -> unit
    val blastInPinterface  : Blaster.instream -> pinterface

    val parameters : pinterface -> Name.LabelSet.set
    val slice : context * Name.LabelSet.set -> Name.LabelSet.set

    val empty : context
    val add_unit : context * label * interface -> context

    val instantiate  : context * pinterface -> interface option

    (*
	For eq and seal, both objects must have been created or
	instantiated in a prefix of the given context.
    *)
    val eq   : context * interface * interface -> bool
    val seal : context * string * module * interface -> module option (* source *)

    val unitlabel : string -> label
    type imports = Il.labels

    val tiltprim : context * label * imports -> module * pinterface

    val elab_dec : context * label * imports * filepos * Ast.dec
	-> (module * pinterface) option

    val elab_specs : context * imports * filepos * Ast.spec list
	-> pinterface option

end
