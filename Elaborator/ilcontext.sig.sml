signature ILCONTEXT =
    sig

	structure Il : IL
	type context = Il.context
	type exp = Il.exp
	type con = Il.con
	type kind = Il.kind
	type mod = Il.mod
	type signat = Il.signat
	type label = Il.label
	type var = Il.var
	type sdec = Il.sdec
	type sdecs = Il.sdecs
	type fixity_table = Il.fixity_table
	type path = Il.path
	type inline = Il.inline
	type context_entry = Il.context_entry
	    
	val context_entries : context -> context_entry list (* for printing *)
	    
	(* context extenders and extractors *)
	val empty_context : context
	val add_context_inline : context * label * var * inline -> context
	val add_context_module : context * label * var * signat -> context
	val add_context_signat : context * label * var * signat -> context
	val add_context_var : context * label * var * con -> context
	val add_context_convar : context * label * var 
	    * kind * con option -> context
	val add_context_sdecs : context * sdecs -> context
	val add_context_sdec : context * sdec -> context
	    
	val add_context_dec  : context * Il.dec -> context
	val add_context_module' : context * var * signat -> context
	val add_context_var' : context * var * con -> context
	val add_context_convar' : context * var 
	    * kind * con option -> context
	    
	val add_context_entries : context * context_entry list -> context
	val Context_Get_FixityTable : context -> fixity_table
	val Context_Get_BoundConvars : context -> var list
		
	val var_bound : context * var -> bool
	val name_bound : context * Il.tag -> bool
	    
	exception NOTFOUND of string
	
	(* Lookup routines *)
	datatype phrase = PHRASE_EXP of exp
	  | PHRASE_CON of con
	  | PHRASE_MOD of mod
	  | PHRASE_SIG of signat
	  | PHRASE_OVEREXP of unit -> exp * (context,con) Il.Tyvar.ocon
	    
	datatype class = CLASS_EXP of con
	  | CLASS_CON of kind
	  | CLASS_MOD of signat
	  | CLASS_SIG
	  | CLASS_OVEREXP
	    
	datatype phrase_class = PHRASE_CLASS_EXP  of exp * con
	  | PHRASE_CLASS_CON  of con * kind
	  | PHRASE_CLASS_MOD  of mod * signat
	  | PHRASE_CLASS_SIG  of signat
	  | PHRASE_CLASS_OVEREXP of unit -> exp * (context,con) Il.Tyvar.ocon
	    
	(* ---- none of these lookup functions perform normalization ---- *)		
	val Sdecs_Lookup' : mod * sdecs * label list -> bool * (label list * phrase_class)
	val Sdecs_Lookup  : mod * sdecs * label list -> label list * phrase_class
	val Sbnds_Lookup  : Il.sbnds * label list -> label list * phrase
	val Context_Lookup  : context * label list -> phrase_class
	val Context_Lookup' : context * var -> phrase_class
	val Context_Exn_Lookup : context * Il.tag -> con
	val modsig_lookup : context * label list -> (path * mod * signat) option
	val var2label     : context * var -> label 

    end
