structure LilPrimUtilParam 
  : PRIMUTILPARAM where type con = Lil.con
	            and type exp = Lil.primarg
	            and type context = unit = 
struct
  structure LD = LilDefs
  structure LU = LilUtil
  structure TD = TranslationDefs
  structure Dec = Deconstruct.Dec
  open Lil
  open Prim

  type exp = primarg
  type con = con
  type context = unit


    
  fun partial_arrow (cons,c2) = LD.T.arrow' cons [] c2
  fun total_arrow (cons,c2) =   LD.T.arrow' cons [] c2
    

  fun con_int is = LD.T.intt (LU.i2size is)
  fun con_uint is = LD.T.intt (LU.i2size is)
  fun con_float fs = LD.T.float()

  fun con_array c = LD.T.ptr (LD.T.array B4 c)
  fun con_intarray is = LD.T.ptr (LD.T.array (LU.i2size is) (con_int is))
  fun con_floatarray fs = LD.T.ptr (LD.T.array (LU.f2size fs) (con_float fs))
  val con_vector = con_array
  val con_intvector = con_intarray
  val con_floatvector = con_floatarray

  fun con_ref c = LD.T.ptr (LD.T.refc c)

  fun con_tag c = LD.C.pcon_app Dyntag_c [c]

  fun con_bool context = LD.T.bool()
  fun bool2exp context b = arg32 (LD.E.bool' b)

  val con_unit = LD.T.unit()
  val unit_value = arg32 (LD.E.unit'())
    
  fun exp2value (arg32(Const_32 v)) = SOME v
    | exp2value (arg64(Const_64 v)) = SOME v
    | exp2value (slice(_,Const_32 v)) = SOME v
    | exp2value _ = NONE
    
  fun value2exp v =
    (case v
       of float (F8,_) => arg64 (Const_64 v)
	| int (W32,_)  => arg32 (Const_32 v)
	| int (sz,_)   => slice (LU.i2size sz,Const_32 v)
	| uint (W32,_) => arg32 (Const_32 v)
	| uint (sz,_)  => slice (LU.i2size sz,Const_32 v)
	| _ => arg32 (Const_32 v))
    
  val con_tuple = LD.T.tupleptr'
end
