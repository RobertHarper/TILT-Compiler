(*$import Prelude Il PPPRIM PPIL ILUTIL ILCONTEXT ILSTATIC BASIS ILCONTEXTEQ Ast SourceMap *)

signature LINKIL = 
  sig
      structure Ppprim : PPPRIM
      structure Ppil : PPIL
      structure IlUtil : ILUTIL
      structure IlContext : ILCONTEXT
      structure IlContextEq : ILCONTEXTEQ
      structure IlStatic : ILSTATIC


      type sbnd = Il.sbnd
      type context_entry = Il.context_entry
      type context = Il.context
      type filepos = SourceMap.charpos -> string * int * int
      type module = Il.module

      val show_hil : bool ref		(* Show HIL from elaboration. *)
      val show_hilcontext : bool ref	(* Show elaboration contexts. *)

      val initial_context : unit -> context
      val plus_context : context * Il.partial_context list -> Il.partial_context option list * context

      (* The elab??? functions take
      	   (1) the unit name for making unique labels
	   (2) the context in which to elaborate
	   (3) position info for (4), used for error messages
	   (4) the objects to elaborate
      *)
      (* Compile interfaces to generate a new context. *)
      val elab_specs           : string * context * filepos * Ast.spec list -> Il.partial_context option
      (* Compile source files possibly with interface constraint to produce some HIL code. *)
      val elab_dec             : string * context * filepos * Ast.dec -> module option
      val elab_dec_constrained : string * context * filepos * Ast.dec * filepos * Ast.spec list -> module option


  end
