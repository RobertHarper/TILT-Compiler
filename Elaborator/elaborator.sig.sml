signature ELABORATOR =
 sig

   type sbnd and context_entry and context
   type filepos = SourceMap.charpos -> string * int * int 

   (* Contexts *)
   structure Basis : BASIS where type Il.context = context
   structure IlContext : ILCONTEXT where type Il.context = context
   structure Ppil : PPIL where type Il.context = context
   val plus_context : context list -> context
   val eq_context : context * context -> bool

   (* Elaboration functions: The functions below return NONE if an error
    * has occurred during elaboration. *)
   val elab_specs : context * filepos * Ast.spec list -> context option
   val elab_dec : context * filepos * Ast.dec -> ((sbnd option * context_entry) list * context) option
   val elab_dec_constrained : context * filepos * Ast.dec * filepos * Ast.spec list -> 
                              ((sbnd option * context_entry) list * context) option
   
 end
   
