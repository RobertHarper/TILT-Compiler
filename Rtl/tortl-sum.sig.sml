(*$import TORTLBASE Name *)

signature TORTL_SUM = 
sig

  structure TortlBase : TORTL_BASE
  type state = TortlBase.state
  type var = Name.var
  type con =  Nil.con
  type kind = Nil.kind
  type regi = Rtl.regi
  type loc_or_val = TortlBase.loc_or_val
  type var_loc = TortlBase.var_loc

  val xsum : (state * var * con * kind option -> regi * kind * state) ->
		bool -> (state * TilWord32.word * con * loc_or_val list) -> loc_or_val * con * state
  val xproject_sum_record : (state * int * Name.label * con list * regi * con  * con option) 
					-> loc_or_val * con * state
  val xproject_sum : (state * var * con * kind option -> regi * kind * state) ->
		     (state * int * con list * regi * con  * con option) 
					-> loc_or_val * con * state

end