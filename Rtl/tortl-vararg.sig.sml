(*$import TortlBase Name *)

signature TORTL_VARARG = 
sig

  type state = TortlBase.state
  type var = Name.var
  type exp = Nil.exp
  type bnd = Nil.bnd
  type regi = Rtl.regi

  (* argc, resc, term function of type argc -> resc *)
  val xmake_vararg : (state * exp -> regi) -> state * regi * regi * regi -> state * regi
  val xmake_onearg : (state * exp -> regi) -> state * regi * regi * regi -> state * regi

  val xmake_vararg_support : unit -> bnd list
  val xmake_onearg_support : unit -> bnd list

end
