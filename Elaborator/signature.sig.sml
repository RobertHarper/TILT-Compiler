signature SIGNATURE = 
sig
    val debug : bool ref
    val debug_full : bool ref

    structure IlContext : ILCONTEXT
	
    type var = IlContext.Il.var
    type mod = IlContext.Il.mod
    type signat = IlContext.Il.signat
    type con = IlContext.Il.con
    type kind = IlContext.Il.kind
    type sdec = IlContext.Il.sdec
    type sbnd = IlContext.Il.sbnd
    type context = IlContext.context
    type label = IlContext.Il.label
    type labels = label list

    val xsig_wheretype : context * sdec list * label list * con * kind -> sdec list
    val xsig_sharing_structure : context *  sdec list * labels list -> sdec list
    val xsig_sharing_type : context *  sdec list * labels list -> sdec list

    type polyinst = context * sdec list -> sbnd list * sdec list * con list
    val xcoerce_seal : polyinst * context * var * signat * signat -> mod * signat
    val xcoerce_transparent : polyinst * context * var * signat * signat -> mod * signat

end