(*$import IL Formatter *)
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
	type dec = Il.dec
	type decs = Il.decs
	type sdec = Il.sdec
	type sdecs = Il.sdecs
	type tag = Il.tag
	type fixity_table = Il.fixity_table
	type path = Il.path
	type inline = Il.inline
	type context_entry = Il.context_entry
	    
	(* ----------- context extenders ----------------------------  *)
	val empty_context : context

	val add_context_fixity : context * fixity_table -> context
	val add_context_dec   : context * dec -> context
	val add_context_decs  : context * decs -> context
	val add_context_sdec  : context * sdec -> context
	val add_context_sdecs : context * sdecs -> context
	val add_context_inline : context * label * var * inline -> context
	val add_context_alias : context * label * label list -> context
	val add_context_entries : context * context_entry list -> context

	val add_context_mod  : context * label * var * signat            -> context
	val add_context_mod' : context *         var * signat            -> context
	val add_context_sig  : context * label * var * signat            -> context
	val add_context_sig' : context *         var * signat            -> context
	val add_context_exp  : context * label * var * con               -> context
	val add_context_exp' : context *         var * con               -> context
	val add_context_con  : context * label * var * kind * con option -> context
	val add_context_con' : context *         var * kind * con option -> context

	(* ------------ Lookup routines ----------------- *)

	datatype phrase = 
	    PHRASE_EXP of exp
	  | PHRASE_CON of con
	  | PHRASE_MOD of mod
	  | PHRASE_SIG of signat
	  | PHRASE_OVEREXP of (con * exp) list

	datatype class = 
	    CLASS_EXP of con
	  | CLASS_CON of kind
	  | CLASS_MOD of signat
	  | CLASS_SIG
	  | CLASS_OVEREXP
	    

	type phrase_class = Il.phrase_class

        (* ----- Useful structure-related helper functions ------- *)	    
        (* ----- Sdecs_Lookup' looks inside starred structure --------- *)
	val Sdecs_Lookup  : mod * sdecs * label list -> (label list * phrase_class) option
	val Sdecs_Lookup' : mod * sdecs * label list -> (label list * phrase_class) option
	val Sbnds_Lookup  : Il.sbnds * label list -> (label list * phrase) option

	val context_to_sdecs : Il.context -> Il.sdecs
	val plus_context : ((con * (var * exp) list * (var * con) list * (var * mod) list) -> con) *
	                   ((kind * (var * exp) list * (var * con) list * (var * mod) list) -> kind) * 
			   ((signat * (var * exp) list * (var * con) list * (var * mod) list) -> signat) ->
			   Il.context list -> Il.context

	(* ---- none of these lookup functions perform normalization ---- *)		
	val fixity : context -> fixity_table
	val Context_Lookup     : context * label list -> (path * phrase_class) option
	val Context_Lookup'    : context * var        -> (label * phrase_class) option
	val Context_Exn_Lookup : context * tag        -> con option
	val Context_Varlist    : context -> var list


        (* -------- if you get desperate enough to print the context, i feel sorry for you ----- *)
	val print_context    : {pp_exp : exp -> Formatter.format,
				pp_mod : mod -> Formatter.format,
				pp_con : con -> Formatter.format,
				pp_fixity_list : fixity_table -> Formatter.format,
				pp_inline : inline -> Formatter.format,
				pp_kind   : kind   -> Formatter.format,
				pp_label  : label  -> Formatter.format,
				pp_var    : var    -> Formatter.format,
				pp_tag    : tag    -> Formatter.format,
				pp_signat : signat -> Formatter.format} *
	    context -> Formatter.format



    end
