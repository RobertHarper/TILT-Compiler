(*$import Il *)

signature SIGNATURE = 
sig
    val debug : bool ref

    type path = Il.path
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
    val xsig_sharing_types : context *  sdec list * labels list -> sdec list
    val xsig_sharing_structures : context *  sdec list * labels list -> sdec list

    (* xcoerce_seal : ... * mod_actual * sig_actual * sig_target -> mod_result

          Sig_actual is the signature of mod_actual.
	  Sig_target does not contain mod_actual.
	  Mod_result contains mod_actual and have signature sig_target.

       xcoerce_transparent : ... * mod_actual * sig_actual * sig_target -> mod_result * sig_result

          Sig_actual is the signature of mod_actual.
	  Sig_target names desired components and possible component coercions.
	  Mod_result contains mod_actual.
	  Sig_result is the signature of mod_result.

        xcoerce_funtor : ... * context * path_actual * sig_actual * sig_target -> mod_result * sig_result
	  Context contains path_actual already.
          Sig_actual is the signature of module path_actual and contains path_actual.
  	  Sig_target does not contain path_actual.
	  Mod_result contains but does not bind path_actual.
    *)
    type polyinst = context * sdec list -> sbnd list * sdec list * con list
    val xcoerce_seal        : polyinst * context * mod * signat * signat -> mod
    val xcoerce_transparent : polyinst * context * mod * signat * signat -> mod * signat
    val xcoerce_functor     : polyinst * context * path * signat * signat -> mod * signat

end