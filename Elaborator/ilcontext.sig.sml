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
    type var = Il.var
    type dec = Il.dec
    type decs = Il.decs
    type sdec = Il.sdec
    type sdecs = Il.sdecs
    type path = Il.path
    type context_entry = Il.context_entry
    type phrase_class = Il.phrase_class

    val installHelpers : {eq_con : context * con * con -> bool} -> unit
	
    (* ----------- context extenders ----------------------------  *)
    val empty_context : context
	
    val add_context_dec   : context * dec -> context
    val add_context_decs  : context * decs -> context
    val add_context_sdec  : context * sdec -> context
    val add_context_sdecs : context * sdecs -> context
    val add_context_fixity : context * label * Fixity.fixity -> context
    val add_context_overexp : context * label * Il.ovld -> context
    val add_context_entries : context * context_entry list -> context
	
    val add_context_mod  : context * label * var * signat            -> context
    val add_context_mod' : context *         var * signat            -> context
    val add_context_sig  : context * label * var * signat            -> context
    val add_context_sig' : context *         var * signat            -> context
    val add_context_exp  : context * label * var * con               -> context
    val add_context_exp' : context *         var * con               -> context
    val add_context_con  : context * label * var * kind * con option -> context
    val add_context_con' : context *         var * kind * con option -> context

    (* Subtract the second context from the first one.  *)
    val sub_context  : Il.context * Il.context -> Il.partial_context  
    (* Returns alpha-varied partial contexts if alpha-varying was needed. *)
    val plus_context : Il.context * Il.partial_context list -> Il.partial_context option list * Il.context
    (* GC the given context by using the partial context and bindings as roots *)
    val gc_context : Il.module -> Il.context

    (* Support for hiding top-level labels. *)
    type label_info
    val empty_label_info : label_info
    val get_labels : Il.partial_context * label_info -> label_info
    val obscure_labels : Il.context * label_info -> Il.context
    val unobscure_labels : Il.module * label_info -> Il.module
	
    (* ---- These lookup functions don't perform selfification ---- *)		
    val Context_Fixity       : context -> Fixity.fixity Name.LabelMap.map
    val Context_Lookup_Overload : context * label -> Il.ovld option
    val Context_Lookup_Label : context * label -> (path * phrase_class) option
    val Context_Lookup_Var   : context * var   -> (label * phrase_class) option
    val Context_Lookup_Path  : context * path  -> (label * phrase_class) option
    val Context_Ordering     : context -> path list

    (* ---- Selfification needed before inserting into contexts ----- *)
    val SelfifySig : Il.context -> Il.path * Il.signat -> Il.signat
    val UnselfifySig : Il.context ->  Il.path * Il.signat -> Il.signat
    val SelfifyDec : Il.context -> Il.dec -> Il.dec
    val SelfifySdec : Il.context -> Il.sdec -> Il.sdec
    val SelfifySdecs : Il.context -> Il.path * Il.sdecs -> Il.sdecs
    val SelfifyEntry : Il.context -> Il.context_entry -> Il.context_entry
    val reduce_signat : Il.context -> Il.signat -> Il.signat

    val removeNonExport : Il.partial_context -> Il.partial_context

    (* --- temporary testing function --- *)	
    val UnselfifyPartialContext : Il.partial_context -> Il.partial_context

  end
