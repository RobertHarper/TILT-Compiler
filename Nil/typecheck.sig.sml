signature TYPECHECK = 
sig

  structure Nil : NIL

  type tcontext = (Nil.var, Nil.kind) HashTable.hash_table
  type econtext = (Nil.var, Nil.con)  HashTable.hash_table

  val empty_tcontext : tcontext
  val empty_econtext : tcontext

  val is_kind : tcontext -> Nil.kind -> bool
  val eq_kind : tcontext -> Nil.kind * Nil.kind -> bool

  val kind_check : tcontext -> Nil.con -> Nil.kind
  val norm_con   : tcontext -> Nil.con -> Nil.con
  (* checks for equality up to alpha-conversion *)
  val eq_con     : (Nil.con * Nil.con) -> bool 
  val type_check : tcontext -> econtext -> Nil.exp -> Nil.con

 (* Although primitives at both levels are always fully applied,
    we provide these helper functions. *)
  val eprim_check : Nil.allprim -> Nil.con
  val cprim_check : Nil.primcon -> Nil.kind

end

