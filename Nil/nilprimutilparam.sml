functor NilPrimUtilParam(structure NilUtil : NILUTIL) 
    : PRIMUTILPARAM =
    struct

	open NilUtil 
	open Nil
	open Util
	open Prim

	type con = con
	type exp = exp
	type ('con,'exp) value = ('con,'exp) Prim.value
	val error = fn s => error "ilprimutilparam.sml" s

	fun partial_arrow (c1,c2) = AllArrow_c(Code,Partial,[],[c1],0w0,c2)
	fun total_arrow (c1,c2) = AllArrow_c(Code,Total,[],[c1],0w0,c2)
	fun con_int is = Prim_c(Int_c is,[])
	fun con_uint is = Prim_c(Int_c is,[])
	fun con_float fs = Prim_c(Float_c fs,[])
	fun con_array c = Prim_c(Array_c,[c])
	fun con_ref c = Prim_c(Ref_c,[c])
	fun con_vector c = Prim_c(Vector_c,[c])
	fun con_tag c = Prim_c(Exntag_c,[c])
	val con_bool = bool_con
	val con_unit = unit_con
	val unit_value = unit_exp

	fun exp2value (Const_e v) = SOME v
	  | exp2value _ = NONE

	val value2exp = Const_e

	fun bool2exp false = false_exp
	  | bool2exp true = true_exp

    end
