(*$import TORTLBASE Name *)

signature TORTL_VARARG = 
sig

  structure TortlBase : TORTL_BASE
  type state = TortlBase.state
  type var = Name.var
  type con = TortlBase.Nil.con
  type kind = TortlBase.Nil.kind
  type regi = TortlBase.Rtl.regi
  type loc_or_val = TortlBase.loc_or_val
  type var_loc = TortlBase.var_loc

  (* argc, resc, term function of type argc -> resc *)
  val xmake_vararg_support : unit -> TortlBase.Nil.bnd list
  val xmake_vararg : (state * TortlBase.Nil.exp -> regi) -> state * regi * regi * regi -> state * regi

  val xmake_onearg_support : unit -> TortlBase.Nil.bnd list
  val xmake_onearg : (state * TortlBase.Nil.exp -> regi) -> state * regi * regi * regi -> state * regi


end
