(* Maps from expressions and constructors *)

structure OpTable :> OPTABLE =

struct
    open Lil Prim

    fun error s = Util.error "optable.sml" s


(* General functions *)

    fun cmp_maker less greater =
      fn (a,b) =>
      if less (a, b) then
	LESS
      else
	if greater (a,b) then
	  GREATER
	else EQUAL
	  
    val cmp_int:(int*int->order) = Int.compare (*cmp_maker op< op>*)
    val cmp_TilWord64 = cmp_maker TilWord64.slt TilWord64.sgt
    val cmp_uTilWord64 = cmp_maker TilWord64.ult TilWord64.ugt
    val cmp_word = Word32.compare

    fun cmp_bool (false, false) = EQUAL
      | cmp_bool (false, true) = LESS
      | cmp_bool (true, true) = EQUAL
      | cmp_bool (true, false) = GREATER

    fun cmp_option cmp (a,b) =
	case (a,b) of
	    (SOME a, SOME b) => cmp (a,b)
	  | (SOME a, NONE) => GREATER
	  | (NONE, SOME b) => LESS
	  | (NONE, NONE) => EQUAL

    (* Takes 2 orders lexicographically to produce another *)
    fun cmp_orders [] = EQUAL
      | cmp_orders (EQUAL::rest) = cmp_orders rest
      | cmp_orders (first::rest) = first

    fun cmp2 (f1,f2) ((a1,a2),(b1,b2)) =
      (case f1 (a1,b1)
	 of EQUAL => f2 (a2,b2)
	  | other => other)

    fun cmp4 (f1,f2,f3,f4) ((a1,a2,a3,a4),(b1,b2,b3,b4)) =
      (case f1 (a1,b1)
	 of EQUAL => (case f2 (a2,b2)
			of EQUAL => (case f3 (a3,b3)
				       of EQUAL => f4 (a4,b4)
					| other => other)
			 | other => other)
	  | other => other)

    fun cmp3 (f1,f2,f3) ((a1,a2,a3),(b1,b2,b3)) =
      (case f1 (a1,b1)
	 of EQUAL => (case f2 (a2,b2)
			of EQUAL => f3 (a3,b3)
			 | other => other)
	  | other => other)
    
    fun cmp_list cmp (a,b) =
      let fun f (nil,nil) = EQUAL
	    | f (nil,_ :: _) = GREATER
	    | f (_ :: _,nil) = LESS
	    | f (h::t,h'::t') =
	(case cmp(h,h')
	   of EQUAL => f(t,t')
	    | r => r)
      in f (a,b)
      end
    
    val sskip = 4
    val skip = 7*sskip
      
    fun hash_intsize sz =
      case sz of
	W8 => 0
      | W16 => 1
      | W32 => 2
      | W64 => 3
	  
    fun hash_floatsize sz =
      case sz of
	F32 => 0
      | F64 => 1
	  
      

    fun hash_tt tt =
      case tt of
	int_tt => 0
      | real_tt => 1
      | both_tt => 2
	  
    fun hash_bool b =
      case b of
	false => 0
      | true => 1
	  
    fun hash_table t =
      case t of
	IntArray sz => 0 *sskip + hash_intsize sz
      | IntVector sz => 1*sskip + hash_intsize sz
      | FloatArray sz => 2*sskip + hash_floatsize sz
      | FloatVector sz => 3*sskip + hash_floatsize sz
      | OtherArray false => 4*sskip + 0
      | OtherArray true => 4*sskip + 1
      | OtherVector false => 4*sskip + 2
      | OtherVector true => 4*sskip + 3

    fun hash_prim p =
      case p of
	soft_vtrap tt => 0 +   (hash_tt tt)
      | soft_ztrap tt => 1*skip + (hash_tt tt)
      | hard_vtrap tt => 2*skip + (hash_tt tt)
      | hard_ztrap tt => 3*skip + (hash_tt tt)
	  
      | float2int => 6*skip
      | int2float => 7*skip
      | int2uint (sz1, sz2) => 8*skip + hash_intsize sz1*sskip + hash_intsize sz2
      | uint2int (sz1, sz2) => 9*skip + hash_intsize sz1*sskip + hash_intsize sz2
      | int2int (sz1, sz2) => 62*skip + hash_intsize sz1*sskip + hash_intsize sz2
      | uint2uint (sz1, sz2) => 63*skip + hash_intsize sz1*sskip + hash_intsize sz2
      | uinta2uinta (sz1, sz2) => 60 *skip + hash_intsize sz1*sskip + hash_intsize sz2
      | uintv2uintv (sz1 , sz2) => 61 *skip + hash_intsize sz1*sskip + hash_intsize sz2
	  
      | neg_float sz => 12*skip + hash_floatsize sz
      | abs_float sz => 13*skip + hash_floatsize sz
      | plus_float sz => 14*skip + hash_floatsize sz
      | minus_float  sz => 15*skip + hash_floatsize sz
      | mul_float sz => 16*skip + hash_floatsize sz
      | div_float sz  => 17*skip + hash_floatsize sz
      | less_float sz => 18*skip + hash_floatsize sz
      | greater_float sz => 19*skip + hash_floatsize sz
      | lesseq_float sz => 20*skip + hash_floatsize sz
      | greatereq_float sz => 21*skip + hash_floatsize sz
      | eq_float sz => 22*skip + hash_floatsize sz
      | neq_float sz => 23*skip + hash_floatsize sz
	  
      | plus_int sz => 24*skip + hash_intsize sz
      | minus_int sz => 25*skip + hash_intsize sz
      | mul_int sz => 26*skip + hash_intsize sz
      | div_int sz => 27*skip + hash_intsize sz
      | mod_int sz => 28 *skip + hash_intsize sz
      | quot_int sz => 29*skip + hash_intsize sz
      | rem_int sz => 30*skip + hash_intsize sz
      | plus_uint sz => 31*skip + hash_intsize sz
      | minus_uint sz => 32*skip + hash_intsize sz
      | mul_uint sz => 33*skip + hash_intsize sz
      | div_uint sz => 34*skip + hash_intsize sz
      | mod_uint sz => 35*skip + hash_intsize sz
      | less_int sz => 36*skip + hash_intsize sz
      | greater_int sz => 37*skip + hash_intsize sz
      | lesseq_int sz => 38*skip + hash_intsize sz
      | greatereq_int sz => 39*skip + hash_intsize sz
      | less_uint sz => 40*skip + hash_intsize sz
      | greater_uint sz => 41*skip + hash_intsize sz
      | lesseq_uint sz => 42*skip + hash_intsize sz
      | greatereq_uint sz => 43*skip + hash_intsize sz
      | eq_int sz => 44*skip + hash_intsize sz
      | neq_int sz => 45*skip + hash_intsize sz
      | neg_int sz => 46*skip + hash_intsize sz
      | abs_int sz => 47*skip + hash_intsize sz
	  
      (* bit-pattern manipulation *)
      | not_int sz => 48 *skip + hash_intsize sz
      | and_int sz => 49*skip + hash_intsize sz
      | or_int sz => 50*skip + hash_intsize sz
      | xor_int sz => 64*skip + hash_intsize sz
      | lshift_int sz => 51*skip + hash_intsize sz
      | rshift_int sz => 52 *skip + hash_intsize sz
      | rshift_uint sz => 53 *skip + hash_intsize sz
	  
      (* array and vector ops - bool = true indicates writeable *)
      | array2vector table => 54 *skip + hash_table table
      | vector2array table => 76 *skip + hash_table table
      | create_table t => 55*skip + hash_table t
      | create_empty_table t => 65*skip + hash_table t
      | sub t => 56*skip + hash_table t
      | update t => 57*skip + hash_table t
      | length_table t => 58*skip + hash_table t
      | equal_table t => 59 *skip + hash_table t
      | mk_ref => 60 * skip 
      | deref => 61 * skip 
      | eq_ref => 62 * skip 
      | setref => 63 * skip 
	  
	  
    fun cmp_intvectors ((sz1, arr1), (sz2, arr2)) =
	let
	    val length1 = Array.length arr1
	    val length2 = Array.length arr2
	    fun e2n (slice(B1,Lil.Const_32(Prim.uint(Prim.W8,n)))) = n
              | e2n _ = error "cmp_vectors: e2n: ill-formed string"
	    fun loop i =
		if (i >= length1) then
		    EQUAL
		else
		    (case cmp_uTilWord64 (e2n (Array.sub(arr1,i)),
					  e2n (Array.sub(arr2,i))) of
			 EQUAL => loop (i+1)
		       | ord => ord)
	in
	    (case (sz1,sz2) of
	       (Prim.W8,Prim.W8) =>
		 (* Comparison of strings *)
		 (case cmp_int(length1, length2) of
		    EQUAL => loop 0
		  | ord => ord)
	     | _ => LESS)
	end


    fun cmp_vk ((v1,k1),(v2,k2)) = cmp_orders[Name.compare_var(v1,v2),cmp_kind(k1,k2)]

    fun cmp_vc ((v1,c1),(v2,c2)) = cmp_orders[Name.compare_var(v1,v2),cmp_con(c1,c2)]

    fun cmp_vklist (vklist1, vklist2) = cmp_list cmp_vk (vklist1, vklist2)
    fun cmp_vclist (vclist1, vclist2) = cmp_list cmp_vc (vclist1, vclist2)
    fun cmp_con_list e = cmp_list cmp_con e

    fun cmp_value (v1, v2) =
	case (v1, v2) of
	    ( int(sz1, wd1), int(sz2, wd2) ) =>
		cmp_orders [cmp_int(hash_intsize sz1, hash_intsize sz2),
			    cmp_TilWord64 (wd1, wd2)]
	  | (int a, _) => GREATER
	  | (_ , int a) => LESS

	  | (uint(sz1, wd1), uint(sz2, wd2)) =>
		cmp_orders [cmp_int(hash_intsize sz1, hash_intsize sz2),
			    cmp_uTilWord64 (wd1, wd2) ]
	  |  (uint a, _) => GREATER
	  | (_ , uint a) => LESS

	  | (float (sz1, str1), float(sz2, str2)) =>
		cmp_orders [cmp_int(hash_floatsize sz1, hash_floatsize sz2),
			    String.compare(str1, str2)]
	  | (float f, _ )  => GREATER
	  | (_, float f) => LESS


	  (* | (array a, array b) => LESS   These shouldn't ever be equal *)
	  | (array a, _ ) => GREATER
	  | (_, array a) => LESS

	  (* | (array a, array b) => LESS   These shouldn't ever be equal *)
	  | (intarray a, _ ) => GREATER
	  | (_, intarray a) => LESS

	  (* | (array a, array b) => LESS   These shouldn't ever be equal *)
	  | (floatarray a, _ ) => GREATER
	  | (_, floatarray a) => LESS

(*	  | (vector v1, (vector v2)) => cmp_vectors (v1, v2)*)
	  | (vector v, _ ) => GREATER
	  | (_, vector v) => LESS

(*	  | (vector v1, (vector v2)) => cmp_vectors (v1, v2)*)
	  | (floatvector v, _ ) => GREATER
	  | (_, floatvector v) => LESS

	  | (intvector v1, (intvector v2)) => cmp_intvectors (v1, v2)
	  | (intvector v, _ ) => GREATER
	  | (_, intvector v) => LESS

	  | (refcell r, _ ) => GREATER (* And these *)
	  | (_, refcell r) => LESS

	  | (tag (t1, con1), (tag (t2, con2))) =>
		cmp_orders[ Name.compare_tag(t1,t2),
			    cmp_con (con1, con2)]


    fun cmp_sv64 (a,b) = 
      (case (a,b)
	 of (Var_64 a,Var_64 b) => Name.compare_var (a,b)
	  | (Var_64 _ , _) => GREATER
	  | (_ , Var_64 _) => LESS
	  | (Const_64 a,Const_64 b) => cmp_value (a,b))

    fun cmp_ctag (a,b) =
      let
	fun hash_tag a = 
	  (case a
	     of Roll => 0
	      | Unroll => 1
	      | Pack => 2
	      | ForgetKnown => 3
	      | ProjKnown => 4
	      | InjUnion => 5
	      | InjForget => 6)
      in cmp_int (hash_tag a,hash_tag b)
      end

    fun cmp_coercion (a,b) = cmp2 (cmp_ctag,cmp_con_list) (a,b)

    fun cmp_sv32 (a,b) =
      (case (a,b)
	 of (Var_32 v1,Var_32 v2) => Name.compare_var (v1,v2)
	  | (Var_32 _, _ )  => GREATER
	  | (_ , Var_32 _) => LESS
	  | (Label l1,Label l2) => Name.compare_label (l1,l2)
	  | (Label _ , _) => GREATER
	  | (_ , Label _) => LESS
	  | (Coercion q1,Coercion q2) => cmp_coercion (q1,q2)
	  | (Coercion _ , _) => GREATER
	  | (_ , Coercion _) => LESS
	  | (Coerce a,Coerce b) => cmp2 (cmp_sv32,cmp_sv32) (a,b)
	  | (Coerce _ , _) => GREATER
	  | (_ , Coerce _) => LESS
	  | (Tabs a,Tabs b) => cmp2 (cmp_vk,cmp_sv32) (a,b)
	  | (Tabs _ , _) => GREATER
	  | (_ , Tabs _) => LESS
	  | (TApp a,TApp b) => cmp2 (cmp_sv32,cmp_con) (a,b)
	  | (TApp _ , _) => GREATER
	  | (_ , TApp _) => LESS
	  | (Const_32 v1,Const_32 v2) => cmp_value (v1,v2)
	  | (Const_32 _ , _) => GREATER
	  | (_ , Const_32 _) => LESS
	  | (Tag w1,Tag w2) => cmp_word (w1,w2)
	  | (Tag _ , _) => GREATER
	  | (_ , Tag _) => LESS
	  | (Unit,Unit) => EQUAL)

    fun cmp_size (a,b) =
      let
	fun hash_size a = 
	  (case a 
	     of B1 => 1
	      | B2 => 2
	      | B4 => 4
	      | B8 => 8)
      in cmp_int(hash_size a,hash_size b)
      end

    val cmp_sv32_list = cmp_list cmp_sv32
    val cmp_sv64_list = cmp_list cmp_sv64
    fun cmp_primarg (arg1,arg2) =
      (case (arg1,arg2)
	 of (arg32 a,arg32 b) => cmp_sv32 (a,b)
	  | (arg32 _, _) => GREATER
	  | (_, arg32 _) => LESS
	  | (arg64 a,arg64 b) => cmp_sv64 (a,b)
	  | (arg64 _, _) => GREATER
	  | (_,arg64 _) => LESS
	  | (slice (sz1,sv1),slice (sz2,sv2)) => cmp_orders [cmp_size (sz1,sz2),cmp_sv32 (sv1,sv2)])

    val cmp_primarg_list = cmp_list cmp_primarg

    fun cmp_prim ((p1,cs1,args1),(p2,cs2,args2)) =
      (case cmp_int (hash_prim p1, hash_prim p2)
	 of EQUAL => cmp2 (cmp_con_list,cmp_primarg_list) ((cs1,args1),(cs2,args2))
	  | other => other)
    val cmp_prim32 = cmp_prim
    fun cmp_prim64 ((p1,args1),(p2,args2)) = cmp_prim((p1,[],args1),(p2,[],args2))
    fun cmp_primembed ((sz1,p1,args1),(sz2,p2,args2)) = 
      cmp_orders[cmp_size (sz1,sz2),cmp_prim((p1,[],args1),(p2,[],args2))]

    fun cmp_op64 (a,b) = 
      (case (a,b)
	 of (Val_64 a,Val_64 b) => cmp_sv64 (a,b)
	  | (Val_64 _, _ )  => GREATER
	  | (_ , Val_64 _) => LESS
	  | (Unbox a, Unbox b)  => cmp_sv32 (a,b)
	  | (Unbox _, _ )  => GREATER
	  | (_ , Unbox _) => LESS
	  | (Prim64 a, Prim64 b)  => cmp_prim64 (a,b)
	  | (Prim64 _, _ )  => GREATER
	  | (_ , Prim64 _) => LESS
	  | (ExternAppf (f1,args1,fargs1), ExternAppf (f2,args2,fargs2))  => 
	   (case cmp_list cmp_sv32 (f1::args1,f2::args2)
	      of EQUAL => cmp_list cmp_sv64 (fargs1,fargs2)
	       | other => other))
(*	  | (ExternAppf _, _ )  => GREATER
	  | (_ , ExternAppf _) => LESS*)

    fun cmp_lilprim (a,b) =
      (case (a,b) 
	 of (Box,Box) => EQUAL
	  | (Box, _) => GREATER
	  | (_, Box) => LESS
	  | (Tuple,Tuple) => EQUAL
	  | (Tuple, _) => GREATER
	  | (_, Tuple) => LESS
	  | (Select w1,Select w2) => cmp_word (w1,w2)
	  | (Select _, _) => GREATER
	  | (_, Select _) => LESS
	  | (Dyntag,Dyntag) => EQUAL
	  | (Dyntag, _) => GREATER
	  | (_, Dyntag) => LESS
	  | (Ptreq,Ptreq) => EQUAL)


    fun cmp_lilprimop(a,b) = 
      cmp4 (cmp_lilprim,cmp_con_list,cmp_sv32_list,cmp_sv64_list) (a,b)

    fun cmp_op32 (a,b) = 
      (case (a,b) 
	 of (Val a, Val b) => cmp_sv32 (a,b)
	  | (Val _ , _) => GREATER
	  | (_,Val _) => LESS
	  | (Prim32 a,Prim32 b) => cmp_prim32 (a,b)
	  | (Prim32 _, _) => GREATER
	  | (_, Prim32 _) => LESS
	  | (PrimEmbed a,PrimEmbed b) => cmp_primembed (a,b)
	  | (PrimEmbed _, _) => GREATER
	  | (_, PrimEmbed _) => LESS
	  | (LilPrimOp32 a,LilPrimOp32 b) => cmp4 (cmp_lilprim,cmp_con_list,cmp_sv32_list,cmp_sv64_list) (a,b)
	  | (LilPrimOp32 _, _) => GREATER
	  | (_, LilPrimOp32 _) => LESS
	  | (ExternApp a,ExternApp b) => cmp3 (cmp_sv32,cmp_sv32_list,cmp_sv64_list) (a,b)
	  | (ExternApp _, _) => GREATER
	  | (_, ExternApp _) => LESS
	  | (App a,App b) => cmp3 (cmp_sv32,cmp_sv32_list,cmp_sv64_list) (a,b)
	  | (App _, _) => GREATER
	  | (_, App _) => LESS
	  | (Call a,Call b) => cmp3 (cmp_sv32,cmp_sv32_list,cmp_sv64_list) (a,b)
	  | (Call _, _) => GREATER
	  | (_, Call _) => LESS
	  | (Switch a,Switch b) => LESS (* We don't try to track expressions *)
	  | (Switch _, _) => GREATER
	  | (_, Switch _) => LESS
	  | (Raise a,Raise b) => cmp2 (cmp_con,cmp_sv32) (a,b)
	  | (Raise _, _) => GREATER
	  | (_, Raise _) => LESS
	  | (Handle (c1,_,_),Handle (c2,_,_)) => 
	   (case cmp_con (c1,c2)
	      of EQUAL => LESS
	       | other => other))  (* We don't try to track expressions *)



    structure Op32Key =
	struct
	    type ord_key = op32
	    val compare = cmp_op32
	end

    structure Op32map = BinaryMapFn(Op32Key)

    structure Op64Key =
      struct
	type ord_key = op64
	val compare = cmp_op64
      end
    structure Op64map = BinaryMapFn(Op64Key)

end



