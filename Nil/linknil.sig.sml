(*$import IL NIL NILUTIL NILCONTEXT NILSTATIC PPNIL LinkIl TONIL TOCLOSURE *)

signature LINKNIL = 
sig
    structure Nil : NIL
    structure Tonil : TONIL
    structure NilUtil : NILUTIL
    structure NilContext : NILCONTEXT
    structure NilStatic : NILSTATIC
    structure PpNil : PPNIL
    structure ToClosure : TOCLOSURE
    sharing NilUtil.Nil = NilContext.Nil = NilStatic.Nil = PpNil.Nil = Nil = ToClosure.Nil
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
    
    val show_hil : bool ref
    val show_renamed : bool ref
    val show_one_optimize : bool ref
    val show_two_optimize : bool ref
    val show_phasesplit : bool ref
    val show_cc : bool ref
    val show_before_rtl : bool ref
end
