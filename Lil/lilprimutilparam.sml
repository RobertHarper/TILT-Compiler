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
    

  fun con_int is = LD.C.pcon_app (Int_c (LU.i2size is)) []
  fun con_uint is = LD.C.pcon_app (Int_c (LU.i2size is)) []
  fun con_float fs = LD.C.pcon_app (Float_c) []
  fun con_array c = LD.T.ptr(case Dec.C.float' c of SOME () => LD.T.array B8 c | _ => LD.T.array B4 c)
  fun con_ref c = (print "Warning: refs are arrays";
		   LD.T.ptr(LD.T.array B4 c))
  fun con_vector c = con_array c
  fun con_tag c = LD.C.pcon_app Dyntag_c [c]

  fun con_bool context = LD.T.bool()
  fun bool2exp context b = arg32 (LD.E.bool' b)

  val con_unit = LD.T.unit()
  val unit_value = arg32 (LD.E.unit'())
    
  fun exp2value (arg32(Const_32 v)) = SOME v
    | exp2value (arg64(Const_64 v)) = SOME v
    | exp2value _ = NONE
    
  fun value2exp v =
    (case v
       of float (F8,_) => arg64 (Const_64 v)
	| _ => arg32 (Const_32 v))
    
  val con_tuple = LD.T.tupleptr'
end
