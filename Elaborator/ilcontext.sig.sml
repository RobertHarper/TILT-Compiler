(*$import IL Il *)
signature ILCONTEXT =
    sig

	type context = Il.context
	type exp = Il.exp
	type con = Il.con
	type kind = Il.kind
	type mod = Il.mod
	type signat = Il.signat
	type label = Il.label
	type var = Il.var
	type dec = Il.dec
	type decs = Il.decs
	type sdec = Il.sdec
	type sdecs = Il.sdecs
	type fixity_table = Il.fixity_table
	type path = Il.path
	type context_entry = Il.context_entry
	type phrase_class = Il.phrase_class
	    
	(* ----------- context extenders ----------------------------  *)
	val empty_context : context

	val add_context_dec   : context * dec -> context
	val add_context_decs  : context * decs -> context
	val add_context_sdec  : context * sdec -> context
	val add_context_sdecs : context * sdecs -> context
	val add_context_fixity : context * fixity_table -> context
	val add_context_overexp : context * label * var * (con * exp) list -> context
	val add_context_entries : context * context_entry list -> context

	val add_context_mod  : context * label * var * signat            -> context
	val add_context_mod' : context *         var * signat            -> context
	val add_context_sig  : context * label * var * signat            -> context
	val add_context_sig' : context *         var * signat            -> context
	val add_context_exp  : context * label * var * con               -> context
	val add_context_exp' : context *         var * con               -> context
	val add_context_con  : context * label * var * kind * con option -> context
	val add_context_con' : context *         var * kind * con option -> context

	val plus_context : Il.context list -> Il.context

	(* ---- These lookup functions don't perform selfification ---- *)		
	val Context_Fixity  : context -> fixity_table
	val Context_Lookup  : context * label -> (path * phrase_class) option
	val Context_Lookup' : context * var   -> (label * phrase_class) option
	val Context_Lookup_Path : context * path -> (label * phrase_class) option
	val Context_Ordering : context -> path list

    end
