(*$import TortlBase Name *)

signature TORTL_ARRAY = 
sig

  type state = TortlBase.state
  type var = Name.var
  type con =  Nil.con
  type kind = Nil.kind
  type regi = Rtl.regi
  type term = TortlBase.term

    val xlen_float   : state * Prim.floatsize -> term -> term * con * state
    val xlen_int     : state * Prim.intsize   -> term -> term * con * state
    val xlen_known   : state * con            -> term -> term * con * state
    val xlen_dynamic : state * con * regi     -> term -> term * con * state

    val xsub_float   : state * Prim.floatsize -> term * term -> term * con * state
    val xsub_int     : state * Prim.intsize   -> term * term -> term * con * state
    val xsub_known   : state * con            -> term * term -> term * con * state
    val xsub_dynamic : state * con * regi     -> term * term -> term * con * state

    val xupdate_float : state * Prim.floatsize -> term * term * term -> term * con * state
    val xupdate_int   : state * Prim.intsize   -> term * term * term -> term * con * state
    val xupdate_known : state * con            -> term * term * term -> term * con * state
    val xupdate_dynamic : state * con * regi   -> term * term * term -> term * con * state

    val xarray_float : state * Prim.floatsize -> term * term -> term * con * state
    val xarray_int   : state * Prim.intsize   -> term * term -> term * con * state
    val xarray_known : state * con            -> term * term -> term * con * state
    val xarray_dynamic : state * con * regi   -> term * term -> term * con * state

end