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

  val xinject_sum_dynamic   : (typearg * term * term * Nil.niltrace) -> term * state
  val xinject_sum_static    : (typearg * term option * Nil.niltrace) -> term * state

  val xproject_sum_static    : (typearg * regi * Nil.niltrace) -> term * state
  val xproject_sum_dynamic   : (typearg * regi * regi * Nil.niltrace) -> term * state

end