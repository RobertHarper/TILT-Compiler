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

    val add_context_mod_switchable : context * var * signat             -> context

    (* ----------- context access; see also IlStatic ----------------------------  *)

    val list_entries : context -> entries

    val Context_Fixity : context -> Fixity.fixity Name.LabelMap.map

    (* 
       Context_Lookup_Var_Raw returns the "natural" unselfified signature of the variable.
       Context_Lookup_Var returns the selfified, but unpeeled, signature of the variable.
       These functions are used in IlStatic, but not so much in other places.
       For functions that return results in peeled form, which is usually what you want,
       see the Context_Lookup functions defined in IlStatic.
     *)
    val Context_Lookup_Var_Raw  : context * var   -> (label * phrase_class) option
    val Context_Lookup_Var      : context * var   -> (label * phrase_class) option
    val Context_Lookup_Label    : context * label -> path option
    val Context_Lookup_Overload : context * label -> ovld option

    (* ----------- miscellaneous ----------------------------  *)

    (*
	Eliminate any top-level bindings in context that do not
	contribute to the definition of the given vars.
    *)
    val gc_context : context * Name.VarSet.set -> context

    (* The boolean parameter should be supplied as true iff we want the resulting
       signature to be non-dependent, which is usually the case. *)
    val selfify : bool -> context * mod * signat -> signat

    (* Used only by recursive module elaboration algorithm.
       update_context() ensures that the next time a variable with a
       "switchable" signature is looked up in the context, we return the
       (selfified form of the) latest version of the signature.
          -Derek
     *)
    val update_context : unit -> unit

end
