(*$import IL PPPRIM PPIL ILUTIL ILCONTEXT ILSTATIC BASIS ILCONTEXTEQ Ast SourceMap *)

signature LINKIL = 
  sig
      structure Ppprim : PPPRIM
      structure Ppil : PPIL
      structure IlUtil : ILUTIL
      structure IlContext : ILCONTEXT
      structure IlContextEq : ILCONTEXTEQ
      structure IlStatic : ILSTATIC
      structure Basis : BASIS

      type sbnd = Il.sbnd
      type context_entry = Il.context_entry
      type context = Il.context
      type filepos = SourceMap.charpos -> string * int * int
      type module = Il.module

      val plus_context : context list -> context
      val elab_specs : context * filepos * Ast.spec list -> context option
      val elab_dec : context * filepos * Ast.dec -> module option
      val elab_dec_constrained : context * filepos * Ast.dec * filepos * Ast.spec list -> module option

      val compile_prelude : bool * string -> module
      val compile : string -> module option
      val compiles : string list -> module list option
      val elaborate : context * string -> (sbnd option * context_entry) list option

      val test : string -> module option


(*      val setdepth : int -> unit (* printing depth *) *)

  end
