(*$import Prelude IL Il Fixity Name *)

signature ILCONTEXT =
  sig

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
    type phrase_class = Il.phrase_class
    type partial_context = Il.partial_context
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
    val add_context_label   : context * label * var                     -> context
    val add_context_sig     : context * label * var * signat            -> context
    val add_context_fixity  : context * label * fixity                  -> context
    val add_context_overexp : context * label * ovld                    -> context
    val add_context_entry   : context * context_entry                   -> context
    val add_context_entries : context * context_entry list              -> context
	
    (* ----------- context access; see also IlStatic ----------------------------  *)
	
    val list_entries : context -> context_entry list
	
    val Context_Fixity : context -> Fixity.fixity Name.LabelMap.map

    val Context_Lookup_Var_Raw  : context * var   -> (label * phrase_class) option
    val Context_Lookup_Var      : context * var   -> (label * phrase_class) option
    val Context_Lookup_Label    : context * label -> path option
    val Context_Lookup_Overload : context * label -> ovld option

    val Context_Ordering : context -> var list	(* Going away soon. *)
	
    (* ----------- separate compilation support; will go away ----------------------------  *)

    (* Subtract the second context from the first one.  *)
    val sub_context  : context * context -> partial_context  

    (* Return alpha-varied partial contexts if alpha-varying was needed, i.e. if:
       1) The unresolved variables of the partial context need to be renamed to fit the
          corresponding variables of the resolving context.
       2) The bound variables of the partial context conflict with the bound
          variables of the resolving context and thus need to be renamed.

       If any alpha-varying is required, an alpha-varied version of the partial context is
       returned along with the combined context.  Otherwise, NONE is.
     *)
    val plus_context : context * partial_context -> partial_context option * context

    (* GC the given context by using the partial context, bindings, and "kept imports" as roots *)
    val gc_context : module -> context

    (* Support for hiding top-level labels.  You can think of a label_info as just a list of
       labels.  When elaborating a module, we want to make sure that it is only allowed to refer
       to components of its direct (declared) imports, not its indirect imports (the imports of
       its direct imports).  To do so, prior to elaboration of the module, the Manager runs
       get_labels on all its indirect imports and obscures those labels in the context using 
       obscure_labels.  After elaboration, the correct labels are reinstated in the context (and
       possibly the sdecs) of the module using unobscure_labels.

       In the implementation, label_info is actually a mapping from "real" labels to unforgeable
       dummy labels.  Obscure_labels applies the mapping, unobscure_labels applies the inverse mapping.
     *)
    type label_info
    val empty_label_info : label_info
    val get_labels : partial_context * label_info -> label_info
    val obscure_labels : context * label_info -> context
    val unobscure_labels : module * label_info -> module
	
    (* Support for name mangling.  List labels in context that may be
       an import after phase splitting.  *)
    val list_labels : context -> labels

    val removeNonExport : partial_context -> partial_context
  end
