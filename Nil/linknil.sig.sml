(*$import Il Nil NILUTIL NILCONTEXT PPNIL TONIL TOCLOSURE NORMALIZE PASS *)

signature LINKNIL = 
sig
    structure Tonil : TONIL
    structure NilUtil : NILUTIL
    structure Normalize : NORMALIZE
    structure NilContext : NILCONTEXT where type context = Normalize.context

    structure Ppnil : PPNIL
    structure ToClosure : TOCLOSURE
    structure Flatten : PASS

    val il_to_nil  : string * Il.module -> Nil.module

    val do_opt : bool ref
    val do_one_optimize : bool ref
    val do_two_optimize : bool ref
    val do_vararg : bool ref
    val do_specialize : bool ref

    val do_typecheck_after_cc : bool ref
    val do_typecheck_before_opt : bool ref
    val do_typecheck_after_opt : bool ref

    val show_html : bool ref    
    val show_renamed : bool ref
    val show_reduce : bool ref
    val show_one_optimize : bool ref
    val show_two_optimize : bool ref
    val show_vararg : bool ref
    val show_phasesplit : bool ref
    val show_cc : bool ref
    val show_before_rtl : bool ref
    
end
