(*$import Il Nil NILUTIL NILCONTEXT PPNIL TONIL TOCLOSURE NORMALIZE *)

signature LINKNIL = 
sig
    structure Tonil : TONIL
    structure NilUtil : NILUTIL
    structure NilContext : NILCONTEXT
(*    structure NilStatic : NILSTATIC *)
    structure Normalize : NORMALIZE
    structure Ppnil : PPNIL
    structure ToClosure : TOCLOSURE
    sharing type NilContext.context = Normalize.context (* = NilStatic.context  *)

    val phasesplit : Il.module -> Nil.module
    val compile_prelude : bool * string -> Nil.module
    val compile : string -> Nil.module
    val compiles : string list -> Nil.module list
    val test : string -> Nil.module
    val il_to_nil : string * Il.module -> Nil.module
    val tests :  string list -> Nil.module list

    val do_opt : bool ref
    val do_one_optimize : bool ref
    val do_two_optimize : bool ref
    val do_vararg : bool ref
    val do_specialize : bool ref

    val typecheck_after_cc : bool ref
    val typecheck_before_opt : bool ref
    val typecheck_after_opt : bool ref
    
    val show_hil : bool ref
    val show_renamed : bool ref
    val show_one_optimize : bool ref
    val show_two_optimize : bool ref
    val show_vararg : bool ref
    val show_phasesplit : bool ref
    val show_cc : bool ref
    val show_before_rtl : bool ref
end
