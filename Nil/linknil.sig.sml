(*$import Il Nil NILUTIL NILCONTEXT PPNIL NORMALIZE TONIL VARARG *)

signature LINKNIL = 
sig
    structure Tonil : TONIL
    structure NilUtil : NILUTIL
    structure Normalize : NORMALIZE
    structure NilContext : NILCONTEXT where type context = Normalize.context

    structure Ppnil : PPNIL
(*
    structure ToClosure : TOCLOSURE
    structure Vararg : VARARG
*)

    val typecheck : bool ref

    val il_to_nil  : string * Il.module -> Nil.module
    val show_html : bool ref    
    
end
