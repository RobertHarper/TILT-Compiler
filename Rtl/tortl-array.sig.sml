(*$import Prelude Nil Rtl Prim TortlBase Name *)

signature TORTL_ARRAY = 
sig

  type state = TortlBase.state
  type var = Name.var
  type con =  Nil.con
  type kind = Nil.kind
  type regi = Rtl.regi
  type term = TortlBase.term
  type value = TortlBase.value

    val xlen_float   : state * Prim.floatsize -> term -> term * state
    val xlen_int     : state * Prim.intsize   -> term -> term * state
    val xlen_known   : state                  -> term -> term * state
    val xlen_dynamic : state * con * regi     -> term -> term * state

    val xsub_float   : state * Prim.floatsize -> term * term * Nil.niltrace -> term * state
    val xsub_int     : state * Prim.intsize   -> term * term * Nil.niltrace -> term * state
    val xsub_known   : state                  -> term * term * Nil.niltrace -> term * state
    val xsub_dynamic : state * con * regi     -> term * term * Nil.niltrace -> term * state

    val xupdate_float : state * Prim.floatsize -> term * term * term -> term * state
    val xupdate_int   : state * Prim.intsize   -> term * term * term -> term * state
    val xupdate_known : state                  -> term * term * term -> term * state
    val xupdate_dynamic : state * con * regi   -> term * term * term -> term * state

    val xarray_float : state * Prim.floatsize -> term * term option -> term * state
    val xarray_int   : state * Prim.intsize   -> term * term option -> term * state
    val xarray_known : state                  -> term * term option -> term * state
    val xarray_dynamic : state * con * regi   -> term * term option -> term * state

    val xvector : state * con * value list -> term * state

end
