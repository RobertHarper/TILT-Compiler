(*$import TORTLBASE Name *)

signature TORTL_VARARG = 
sig

  structure TortlBase : TORTL_BASE
  type state = TortlBase.state
  type var = Name.var
  type con = Nil.con
  type exp = Nil.exp
  type bnd = Nil.bnd
  type kind = Nil.kind
  type regi = Rtl.regi
  type loc_or_val = TortlBase.loc_or_val
  type var_loc = TortlBase.var_loc

  (* argc, resc, term function of type argc -> resc *)
  val xmake_vararg_support : unit -> bnd list
  val xmake_vararg : (state * exp -> regi) -> state * regi * regi * regi -> state * regi

  val xmake_onearg_support : unit -> bnd list
  val xmake_onearg : (state * exp -> regi) -> state * regi * regi * regi -> state * regi


end
