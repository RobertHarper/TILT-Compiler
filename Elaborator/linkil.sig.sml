(*$import IL PPPRIM PPIL ILUTIL ILCONTEXT ILSTATIC BASIS ILCONTEXTEQ Ast SourceMap *)

signature LINKIL = 
  sig
      structure Il : IL
      structure Ppprim : PPPRIM where Prim = Il.Prim
      structure Ppil : PPIL where Il = Il
      structure IlUtil : ILUTIL where Il = Il
      structure IlContext : ILCONTEXT  where Il = Il
      structure IlContextEq : ILCONTEXTEQ  where Il = Il
      structure IlStatic : ILSTATIC   where Il = Il
      structure Basis : BASIS where Il = Il

      type sbnd = Il.sbnd
      type context_entry = Il.context_entry
      type context = Il.context
      type module = (context * (sbnd option * context_entry) list)
      type filepos = SourceMap.charpos -> string * int * int

      val plus_context : context list -> context
      val elab_specs : context * filepos * Ast.spec list -> context option
      val elab_dec : context * filepos * Ast.dec -> module option
      val elab_dec_constrained : context * filepos * Ast.dec * filepos * Ast.spec list -> module option

      val compile_prelude : bool * string -> module
      val compile : string -> module option
      val compiles : string list -> module list option
      val elaborate : context * string -> (sbnd option * context_entry) list option

      val test : string ->  module option


(*      val setdepth : int -> unit (* printing depth *) *)

  end
