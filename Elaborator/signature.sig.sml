(*$import IL *)

signature SIGNATURE = 
sig
    val debug : bool ref
    val debug_full : bool ref

    structure Il : IL

    type var = Il.var
    type mod = Il.mod
    type signat = Il.signat
    type con = Il.con
    type kind = Il.kind
    type sdec = Il.sdec
    type sbnd = Il.sbnd
    type context = Il.context
    type label = Il.label
    type labels = label list

    val xsig_where_type : context * sdec list * label list * con * kind -> sdec list
    val xsig_where_structure : context * sdec list * label list * mod * signat -> sdec list
    val xsig_sharing_type : context *  sdec list * labels list -> sdec list
    val xsig_sharing_structure : context *  sdec list * labels list -> sdec list


    type polyinst = context * sdec list -> sbnd list * sdec list * con list
    val xcoerce_seal : polyinst * context * var * signat * signat -> mod * signat
    val xcoerce_transparent : polyinst * context * var * signat * signat -> mod * signat

end