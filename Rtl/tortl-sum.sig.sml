(*$import TortlBase Name *)

signature TORTL_SUM = 
sig

  type var = Name.var
  type con =  Nil.con
  type kind = Nil.kind
  type regi = Rtl.regi
  type state = TortlBase.state
  type term = TortlBase.term

  type typearg = state * TilWord32.word * con

  val xsum_dynamic   : (typearg * term * term * Nil.niltrace) -> term * con * state
  val xsum_nonrecord : (typearg * term option * Nil.niltrace) -> term * con * state
  val xsum_record    : (typearg * term list) -> term * con * state

  val xproject_sum_dynamic   : (typearg * regi * regi * Nil.niltrace) -> term * con * state
  val xproject_sum_record    : (typearg * Name.label * con list * regi * Nil.niltrace) -> term * con * state
  val xproject_sum_nonrecord : (typearg * regi * con * Nil.niltrace) -> term * con * state

end