(*$import Prelude Nil TilWord32 Rtl TortlBase Name *)

signature TORTL_SUM = 
sig

  type var = Name.var
  type con =  Nil.con
  type kind = Nil.kind
  type regi = Rtl.regi
  type state = TortlBase.state
  type term = TortlBase.term

  type typearg = state * TilWord32.word * con

  val make_sum_tag_static : typearg * Nil.niltrace -> term

  (* Term arguments to xinject_sum_static will be one of the following 3 patterns: *)
  (*     1. [], in the case that the value of the injection is just a tag.         *)
  (*     2. [t], in the case of a single value-carrying arm that need not be boxed. *)
  (*     3. [tw,t], in every other case; tw is the gctag for the heap-allocated record. *)
  val xinject_sum_static    : (typearg * term list * Nil.niltrace) -> term * state
  val xinject_sum_dynamic   : (typearg * term * term * Nil.niltrace) -> term * state

  val xproject_sum_static    : (typearg * regi * Nil.niltrace) -> term * state
  val xproject_sum_dynamic   : (typearg * regi * regi * Nil.niltrace) -> term * state

end