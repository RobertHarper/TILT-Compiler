signature ILCONTEXT =
sig
    val IlcontextDiag : bool ref

    type context = Il.context
    type exp = Il.exp
    type con = Il.con
    type kind = Il.kind
    type mod = Il.mod
    type signat = Il.signat
    type label = Il.label
    type labels = Il.labels
    type var = Il.var
    type dec = Il.dec
    type decs = Il.decs
    type sdec = Il.sdec
    type sdecs = Il.sdecs
    type path = Il.path
    type ovld = Il.ovld
    type fixity = Fixity.fixity
    type context_entry = Il.context_entry
    type entries = Il.entries
    type phrase_class = Il.phrase_class
    type module = Il.module

    val installHelpers : {eq_con : context * con * con -> bool} -> unit

    (* ----------- context extenders ----------------------------  *)
    val empty_context : context

    val add_context_exp     : context * label * var * con               -> context
    val add_context_exp'    : context * var * con                       -> context
    val add_context_con     : context * label * var * kind * con option -> context
    val add_context_con'    : context * var * kind * con option         -> context
    val add_context_mod     : context * label * var * signat            -> context
    val add_context_mod'    : context * var * signat                    -> context
    val add_context_sdec    : context * sdec                            -> context
    val add_context_dec     : context * dec                             -> context
    val add_context_sdecs   : context * sdecs                           -> context
    val add_context_decs    : context * decs                            -> context
    val add_context_label   : context * label * var                     -> context (* ugly hack *)
    val add_context_sig     : context * label * var * signat            -> context
    val add_context_sig'    : context * var * signat                    -> context
    val add_context_extern  : context * label * var * label * con       -> context
    val add_context_extern' : context * var * label * con               -> context
    val add_context_fixity  : context * label * fixity                  -> context
    val add_context_overexp : context * label * ovld                    -> context
    val add_context_entry   : context * context_entry                   -> context
    val add_context_entries : context * entries                         -> context

    (* ----------- context access; see also IlStatic ----------------------------  *)

    val list_entries : context -> entries

    val Context_Fixity : context -> Fixity.fixity Name.LabelMap.map

    val Context_Lookup_Var_Raw  : context * var   -> (label * phrase_class) option
    val Context_Lookup_Var      : context * var   -> (label * phrase_class) option
    val Context_Lookup_Label    : context * label -> path option
    val Context_Lookup_Overload : context * label -> ovld option

    (* ----------- miscellaneous ----------------------------  *)

    (*
	Reachable identifies the top-level bindings in context that
	contribute to the definition of the given variables.
	Gc_context eliminates any top-level bindings in context that
	do not contribute to the definition of the given vars.
    *)
    val reachable : context * Name.VarSet.set -> Name.VarSet.set
    val gc_context : context * Name.VarSet.set -> context
end
