(*$import IL NIL NILUTIL NILCONTEXT NILSTATIC PPNIL LinkIl *)

signature LINKNIL = 
sig
    structure Il : IL
    structure Nil : NIL
    structure NilUtil : NILUTIL
    structure NilContext : NILCONTEXT
    structure NilStatic : NILSTATIC
    structure PpNil : PPNIL
    sharing NilUtil.Nil = NilContext.Nil = NilStatic.Nil = PpNil.Nil = Nil
    sharing type NilContext.context = NilStatic.context

    val phasesplit : LinkIl.module -> Nil.module
    val compile_prelude : bool * string -> Nil.module
    val compile : string -> Nil.module
    val compiles : string list -> Nil.module list
    val test : string -> Nil.module
    val il_to_nil : string * LinkIl.module -> Nil.module
    val tests :  string list -> Nil.module list

    val do_opt : bool ref
    val do_one_optimize : bool ref
    val do_two_optimize : bool ref
    val do_specialize : bool ref

    val typecheck_after_cc : bool ref
    val typecheck_before_opt : bool ref
    val typecheck_after_opt : bool ref
    
    val show_renamed : bool ref
    val show_cc : bool ref
    val show_before_rtl : bool ref
end
where Il = LinkIl.Il