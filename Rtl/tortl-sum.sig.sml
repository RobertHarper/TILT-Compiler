(*$import TortlBase Name *)

signature TORTL_SUM = 
sig

  type state = TortlBase.state
  type var = Name.var
  type con =  Nil.con
  type kind = Nil.kind
  type regi = Rtl.regi
  type loc_or_val = TortlBase.loc_or_val
  type var_loc = TortlBase.var_loc

  type typearg = state * TilWord32.word * con
  val xsum_dynamic   : (typearg * loc_or_val * loc_or_val) -> loc_or_val * con * state
  val xsum_nonrecord : (typearg * loc_or_val option) -> loc_or_val * con * state
  val xsum_record    : (typearg * loc_or_val list) -> loc_or_val * con * state

  val xproject_sum_dynamic   : (typearg * regi * regi) -> loc_or_val * con * state
  val xproject_sum_record    : (typearg * Name.label * con list * regi) -> loc_or_val * con * state
  val xproject_sum_nonrecord : (typearg * regi * con) -> loc_or_val * con * state

end