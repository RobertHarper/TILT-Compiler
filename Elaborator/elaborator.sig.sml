signature ELABORATOR =
 sig

   type sbnds and context and spec = Ast.spec and dec = Ast.dec
   type filepos = SourceMap.charpos -> string * int * int 

   (* Contexts *)
   val empty_context : context
   val init_context : context
   val plus_context : context list -> context
   val eq_context : context * context -> bool

   (* Elaboration functions: The functions below return NONE if an error
    * has occurred during elaboration. *)
   val elab_specs : context * filepos * spec list -> context option
   val elab_dec : context * filepos * dec -> (sbnds * context) option
   val elab_dec_constrained : context * filepos * dec * context -> sbnds option
   
 end
   
