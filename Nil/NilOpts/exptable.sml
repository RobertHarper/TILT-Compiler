(* Basically revamped from old version of Til *)





functor ExpTable (structure Nil : NIL
		      ) : EXPTABLE = 
    
struct
    structure Nil = Nil
    open Nil Nil.Prim

    exception NotValue
    exception UNIMP



(* General functions *)

    fun cmp_maker less greater =
	fn (a,b) =>
	if less (a, b) then 
	    LESS 
	else 
	    if greater (a,b) then 
		GREATER
	    else EQUAL
		
    val cmp_int:(int*int->order) = cmp_maker op< op>
    val cmp_TilWord64 = cmp_maker TilWord64.slt TilWord64.sgt
    val cmp_uTilWord64 = cmp_maker TilWord64.ult TilWord64.ugt

    fun cmp_option cmp (a,b) =
	case (a,b) of
	    (SOME a, SOME b) => cmp (a,b)
	  | (SOME a, NONE) => GREATER
	  | (NONE, SOME b) => LESS
	  | (NONE, NONE) => EQUAL

    fun cmp_array arr = LESS (* replace these or something *)   
    fun cmp_func f = LESS
    fun cmp_bnd b = LESS
    fun cmp_seq s = LESS 

(*    fun cmp_primcon p = 
	case p of 
	    ( Int_c sz1, Int_c sz2) => 
	  | (Int_c _, _) => GREATER
	  | (_, Int_c _ ) => LESS

	 | (Float_c sz1, Float_c sz2) =>
	 | (Float_c _, _) => GREATER
	 | (_, Float_c _) => LESS
    
	 | (BoxFloat_c sz1, BoxFloat_x sz2) =>
	 | (BoxFloat_c _, _) => GREATER
	 | ( _, BoxFloat_c _) => LESS

	 | (Exn_c , Exn_c ) => EQUAL
	 | ( Exn_c , _) => GREATER
	 | ( _, Exen_c) => LESS

	 | ( Array_c , Array_c ) => EQUAL
	 | (Array_c, _) => GREATER
	 | (_, Array_c) => LESS

	 | ( Vector_c, Vector_c) => EQUAL
	 | (Vector_c, _) => GREATER
	 | ( _, Vector_c) => LESS

	 | (Ref_c, Ref_c) => EQUAL
	 | ( Ref_c, _) => GREATER
	 | (_, Ref_c) => LESS

	 | (Exntag_c, Exntag_c) => EQUAL
	 | ( Exntag_c, _) => GREATER
	 | (_, Exntag_c) => LESS
 
	 | (Sum_c {t1, k1} , Sum_c {t2, k2}) => cmp_orders ( Word32.compare(t1, t2), Word32.compare(s1, s2))
	 | ( Sum_c, _) => GREATER
	 | (_, Sum_c) => LESS

	 | (Record_c labels1, Record_c labels2) => cmp_list Name.compare_label (labels1, labels2)
	 | ( Record_c, _) => GREATER
	 | (_, Record_c) => LESS

	 | (Vararg_c (o1, e1), Vararg_c (o2, e2)) =>
	       cmp_orders ( cmp_int (hash_openness (o1), hash_openness(o2)),
			   cmp_int (hash_effect e1, hash_effect e2))
	 | ( Vararg_c, _) => GREATER
	 | (_, Vararg_c) => LESS *)

    fun cmp_con (c1, c2) = LESS
(*        case (c1, c2) of
	    ( Prim_c (p1, clist1), Prim_c (p2, clist2)) => 
		cmp_compose [ cmp_primcon, cmp_con_list ]  [ (p1, p2) (clist1, clist2) ]
	  | ( Prim_c _, _ ) => GREATER 
	  | ( _ , Prim_c _ ) => LESS

	  | (Mu_c (seq1, v1) , Mu_c (seq2,v2)) => cmp_compose [ cmp_seq , Name.compare_var ] [ (seq1, seq2), (v1, v2)]
	  | (Mu_c _ , _) => GREATER
	  | (_ , Mu_c _) =>LESS

	  | ( AllArrow_c a1, AllArrow_c a2) => raise UNIMP
	  | ( AllArrow_c _, _) => GREATER
	  | ( _, AllArrow_c _ ) => LESS

	  | (Var_c v1, Var_c v2) => Name.compare_var (v1, v2)
	  | (Var_c _, _) => GREATER
	  | (_, Var_c _) => LESS

	  | ( Let_c (sort1, conbnds1, con1) , Let_c (sort2, conbnds2, con2)) => raise UNIMP
	  | (Let_c _, _) => GREATER
	  | (_, Let_c) => LESS

	  | (Crecord_c lclist1, Crecord_c lclist2) => 
		let val (labels1, cons1) = Listops.unzip lclist1
		    val (labels2, cons2) = Listops.unzip lclist2
		in 
		    cmp_compose [ cmp_list Name.compare_label, cmp_list Name.compare_var]
		    [  Listops.zip labels1 labels2, Listops.zip cons1, cons2 ] 
		end
	  | (Crecord_c _, _) => GREATER
	  | (_, Crecord_c _) => LESS
		
	  | (Proj_c (con1, label1) , Proj_c (con2, label2) ) =>
		cmp_compose [ Name.compare_label, Name.compare_var] [ (con1, con2) , (label1, label2) ]
	  | (Proj_c _ , _) => GREATER
	  | ( _, Proj_c _) => LESS

	  | (Closure_c (code1, env1), Closure_c (code2, env2) ) => cmp_con_list [ (code1, code2) , (env1, env2) ]
	  | (Closure_c _, _) =>GREATER
	  | (_, Closure_c _) => LESS

	  | (App_c (con1, clist1) , App_c (con2, clist2)) => 
		cmp_compse [ cmp_con, cmp_con_list] [ (con1, con2), (clist1, clist2)]
	  | (App_c _, _) => GREATER
	  | (_, App_c _) => LESS

	  | (Typecase_c rec1, Typecase_c rec2) => raise UNIMP
	  | (Typecase_c _, _) => GREATER
	  | (_, Typecase_c) => LESS

	  | (Annotate_c _, Annotate_c) => raise UNIMP 
*)


    fun cmp_orders (a, b) =
	(* Takes 2 orders lexicographically to produce another *)
	case a of
	    EQUAL => b
	  | _ => a


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
	  | PtrArray => 4*sskip
	  | PtrVector => 5*sskip
	  | WordArray => 6*sskip
	  | WordVector => 7*sskip


    fun hash_prim p =
	case p of
	    soft_vtrap (tt)  => 0 +   (hash_tt tt)
	  | soft_ztrap tt => 1*skip + (hash_tt tt) 
	  | hard_vtrap tt => 2*skip + (hash_tt tt)
	  | hard_ztrap tt => 3*skip + (hash_tt tt)
	    
	  | mk_ref => 4*skip
	  | deref => 5*skip

	  | float2int => 6*skip
	  | int2float => 7*skip
	  | int2uint (sz1, sz2) => 8*skip + hash_intsize sz1*sskip + hash_intsize sz2
	  | uint2int (sz1, sz2) => 9*skip + hash_intsize sz1*sskip + hash_intsize sz2
	  | uinta2uinta (sz1, sz2) => 60 *skip + hash_intsize sz1*sskip + hash_intsize sz2
	  | uintv2uintv (sz1 , sz2) => 61 *skip + hash_intsize sz1*sskip + hash_intsize sz2

	  | eq_ref => 10*skip
	  | setref => 11*skip
		
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
	  | lshift_int sz => 51*skip + hash_intsize sz
	  | rshift_int sz => 52 *skip + hash_intsize sz    
	  | rshift_uint sz => 53 *skip + hash_intsize sz   
		
	  (* array and vector ops - bool = true indicates writeable *)
	  | array2vector table => 54 *skip + hash_table table
	  | create_table t => 55*skip + hash_table t
	  | sub t => 56*skip + hash_table t
	  | update t => 57*skip + hash_table t
	  | length_table t => 58*skip + hash_table t
	  | equal_table t => 59 *skip + hash_table t

	  (* IO operations *)
	  | open_in => 66 *skip
	  | input => 67 *skip
	  | input1 => 68 *skip
	  | lookahead => 69 *skip
	  | open_out => 70 *skip
	  | close_in => 71 *skip
	  | output => 72 *skip
	  | flush_out => 73 *skip
	  | close_out => 74 *skip
	  | end_of_stream => 75 *skip

     fun cmp_value (v1, v2) =
	case (v1, v2) of 
	    ( int(sz1, wd1), int(sz2, wd2) ) =>
		cmp_orders (cmp_int(hash_intsize sz1, hash_intsize sz2), 
			    cmp_TilWord64 (wd1, wd2) )
	  | (int a, _) => GREATER
	  | (_ , int a) => LESS
		
	  | (uint(sz1, wd1), uint(sz2, wd2)) => 
		cmp_orders (cmp_int(hash_intsize sz1, hash_intsize sz2), 
			    cmp_uTilWord64 (wd1, wd2) )
	  |  (uint a, _) => GREATER
	  | (_ , uint a) => LESS

	  | (float (sz1, str1), float(sz2, str2)) =>
		cmp_orders (cmp_int(hash_floatsize sz1, hash_floatsize sz2), 
			    String.compare(str1, str2))
	  | (float f, _ )  => GREATER
	  | (_, float f) => LESS

	  (* | (array a, array b) => LESS   These shouldn't ever be equal *)
	  | (array a, _ ) => GREATER
	  | (_, array a) => LESS 

	  | (vector (con1, eArray1), (vector (con2, eArray2))) => 
		cmp_orders (cmp_con (con1, con2) , cmp_array (eArray1, eArray2))
	  | (vector v, _ ) => GREATER
	  | (_, vector v) => LESS

	  | (refcell r, _ ) => GREATER (* And these *)
	  | (_, refcell r) => LESS 

	  | (tag (t1, con1), (tag (t2, con2))) => 
		cmp_orders( Name.compare_tag(t1,t2),
			   cmp_con (con1, con2))


    fun hash_openness ope =
	case ope of 
	    Open => 0
	  | Code => 1
	  | Closure => 2
	  | ExternCode => 3

    fun hash_effect eff =
	case eff of 
	    Total => 0
	  | Partial => 1

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

    (* Takes a list of comparison ops and a list of things to compare *)
    fun cmp_compose cmps args = 
	let fun f nil (nil,nil) = EQUAL
	      | f (cmp::rest) (h::t,h'::t') =
		(case cmp(h,h')
		     of EQUAL => f rest (t,t')
		   | r => r)
	in f cmps args
	end 

    fun cmp_exp_list e = cmp_list cmp_exp e
    and cmp_con_list e = cmp_list cmp_con e
    and cmp_bnd_list e = cmp_list cmp_bnd e
    and cmp_label_list e = cmp_list Name.compare_label e


    and cmp_allprim ((NilPrimOp p1), (NilPrimOp p2))= 
	( case (p1, p2) of 
	    (record labels1, record labels2) => cmp_label_list (labels1, labels2)
	  | (record l, _) => GREATER
	  | ( _, record l) => LESS

	  | (select l1, select l2) => Name.compare_label (l1, l2)
	  | (select l, _ ) => GREATER
	  | (_, select l) => LESS

	  | (inject {tagcount = t1, sumtype= f1} , inject {tagcount = t2, sumtype = f2} ) =>
		cmp_orders ( Word32.compare(t1, t2), Word32.compare(f1, f2))
	  | (inject _, _) => GREATER
	  | ( _, inject _) => LESS

          | (inject_record {tagcount = t1, sumtype = s1}, 
	     inject_record {tagcount = t2, sumtype = s2} ) =>
	    cmp_orders ( Word32.compare(t1, t2), Word32.compare(s1, s2))
	   | (inject_record _, _) => GREATER
	   | (_, inject_record _) => LESS
	   
	   | (project_sum {tagcount=t1, sumtype= s1}, project_sum{tagcount=t2, sumtype = s2}) =>
		 cmp_orders ( Word32.compare(t1, t2), Word32.compare(s1, s2))
	   | (project_sum _, _) => GREATER
	   | (_, project_sum _) => LESS

      | (project_sum_record {tagcount=t1, sumtype=s1, field=f1},
	 project_sum_record {tagcount=t2, sumtype=s2, field=f2}) => 
	cmp_orders ( cmp_orders (Word32.compare (t1, t2), Word32.compare(s1,s2)), Name.compare_label (f1, f2))
       | (project_sum_record _ , _) => GREATER
       | (_ , project_sum_record _) => LESS

       | (box_float sz1, box_float sz2) => cmp_int (hash_floatsize sz1, hash_floatsize sz2)
       | (box_float _ , _ ) => GREATER
       | (_, box_float _) => LESS


       | (unbox_float sz1, unbox_float sz2) => cmp_int (hash_floatsize sz1, hash_floatsize sz2)
       | (unbox_float _ , _ ) => GREATER
       | (_, unbox_float _) => LESS
	     
       | (roll, roll) => EQUAL
       |( roll, _ ) => GREATER
       | (_, roll) => LESS
	     
       | (unroll, unroll) => EQUAL
       | (unroll, _ ) => GREATER
       | (_, unroll) => LESS
	     
       | (make_exntag, make_exntag) => EQUAL
       | (make_exntag, _) => GREATER
       | (_, make_exntag) => LESS

       | (inj_exn s1, inj_exn s2) => String.compare(s1, s2)
       | (inj_exn _ , _ ) => GREATER
       | (_, inj_exn _) => LESS

       | (make_vararg (o1, e1), make_vararg(o2, e2)) => 
	     cmp_orders ( cmp_int (hash_openness (o1), hash_openness(o2)),
			cmp_int (hash_effect e1, hash_effect e2))
       | (make_vararg _, _) => GREATER
       | (_, make_vararg _ ) => LESS

       | ( make_onearg  (o1, e1), make_onearg(o2, e2)) => 
	     cmp_orders ( cmp_int (hash_openness (o1), hash_openness(o2)),
			 cmp_int (hash_effect e1, hash_effect e2))
       | (make_onearg _, _) => GREATER
       | (_, make_onearg _ ) => LESS

       | (peq, peq) => EQUAL )
(*       | (peq, _) => GREATER
       | (_, peq) => LESS) *)

      | cmp_allprim (NilPrimOp _ , _ ) = GREATER
      | cmp_allprim (_ , NilPrimOp _) = LESS
      | cmp_allprim ( PrimOp p1, PrimOp p2) = 
	cmp_int (hash_prim(p1), hash_prim p2)
	
	

    (* In order for this not to get x^2 running time, the expression 
     should be in Anormal form *)
	
    and cmp_exp (a, b) = 
	case (a,b) of 
	    ( Var_e v1, Var_e v2 ) => Name.compare_var (v1, v2)
	  | (Var_e _, _ ) => GREATER
	  | (_ , Var_e _ ) => LESS
		
	  | (Const_e c1, Const_e c2) => cmp_value (c1,c2)
	  | ( Const_e _, _ ) => GREATER
	  | (_, Const_e _) => LESS
		
      | ( Let_e ( _ , bnds1 , Var_e v1),
	 Let_e ( _ , bnds2 , Var_e v2) ) => 
	( case  Name.compare_var (v1, v2) of 
	      EQUAL => cmp_bnd_list (bnds1, bnds2)
	    | r => r)
       | (Let_e _, _) => GREATER
       | (_, Let_e _ ) => LESS
	     
       | (Prim_e (np1, clist1, elist1),  (Prim_e (np2, clist2, elist2))) =>
	     ( case cmp_allprim (np1, np2) of
		   EQUAL => (case cmp_exp_list (elist1, elist2) of
				 EQUAL => cmp_con_list (clist1, clist2)
			       | r => r)
		 | r => r)
		   
       | (Prim_e _, _) => GREATER
       | (_, Prim_e _) => LESS
	     
       | ( Switch_e sw1, Switch_e sw2) => LESS (* Fill *)
       | (Switch_e _, _) => GREATER
       | (_, Switch_e _) => LESS
	     
       | (App_e (_, exp1, clist1, elist1, eflist1), App_e (_, exp2, clist2, elist2, eflist2)) =>
	     (case cmp_exp(exp1,exp2)
		  of EQUAL => ( case cmp_exp_list(elist1,elist2) of
			       EQUAL => ( case cmp_exp_list (eflist1, eflist2) of 
					 EQUAL => cmp_con_list (clist1, clist2)
				       | r => r)
			     | r => r)
		| r => r)			      
       | ( App_e _, _) => GREATER
       | (_, App_e _) => LESS
	     
       | (Raise_e (e1, c1) , Raise_e (e2, c2)) =>
	     (case cmp_exp (e1, e2) of
		  EQUAL => cmp_con (c1, c2)
		| r => r)
       | (Raise_e _, _) => GREATER
       | (_, Raise_e _) => LESS
	     
       | (Handle_e (exp1, func1), Handle_e (exp2, func2)) =>
	     (case cmp_exp (exp1, exp2) of
		  EQUAL => cmp_func (func1, func2)
		| r => r)

	     

    structure ExpKey = 
	struct 
	    type ord_key = exp
	    val compare = cmp_exp
	end

    structure Expmap = BinaryMapFn(ExpKey)
end
	    


