(*$import TortlBase Name *)

signature TORTL_ARRAY = 
sig

  type state = TortlBase.state
  type var = Name.var
  type con =  Nil.con
  type kind = Nil.kind
  type regi = Rtl.regi
  type loc_or_val = TortlBase.loc_or_val
  type var_loc = TortlBase.var_loc


    val xlen_float   : state * Prim.floatsize -> loc_or_val -> loc_or_val * con * state
    val xlen_int     : state * Prim.intsize   -> loc_or_val -> loc_or_val * con * state
    val xlen_known   : state * con            -> loc_or_val -> loc_or_val * con * state
    val xlen_dynamic : state * con * regi     -> loc_or_val -> loc_or_val * con * state

    val xsub_float   : state * Prim.floatsize -> loc_or_val * loc_or_val -> loc_or_val * con * state
    val xsub_int     : state * Prim.intsize   -> loc_or_val * loc_or_val -> loc_or_val * con * state
    val xsub_known   : state * con            -> loc_or_val * loc_or_val -> loc_or_val * con * state
    val xsub_dynamic : state * con * regi     -> loc_or_val * loc_or_val -> loc_or_val * con * state

    val xupdate_float : state * Prim.floatsize -> loc_or_val * loc_or_val * loc_or_val -> loc_or_val * con * state
    val xupdate_int   : state * Prim.intsize   -> loc_or_val * loc_or_val * loc_or_val -> loc_or_val * con * state
    val xupdate_known : state * con            -> loc_or_val * loc_or_val * loc_or_val -> loc_or_val * con * state
    val xupdate_dynamic : state * con * regi   -> loc_or_val * loc_or_val * loc_or_val -> loc_or_val * con * state

    val xarray_float : state * Prim.floatsize -> loc_or_val * loc_or_val -> loc_or_val * con * state
    val xarray_int   : state * Prim.intsize   -> loc_or_val * loc_or_val -> loc_or_val * con * state
    val xarray_known : state * con            -> loc_or_val * loc_or_val -> loc_or_val * con * state
    val xarray_dynamic : state * con * regi   -> loc_or_val * loc_or_val -> loc_or_val * con * state

end