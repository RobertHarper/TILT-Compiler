structure Trail :> TRAIL =
  struct

    open Nil

    val error = fn s => Util.error "Trail" s

    val cmp_label = Name.compare_label
    val cmp_var   = Name.compare_var

    val cmp_int  = Int.compare
    val cmp_word = Word32.compare

    val alpha_equate_pair  = Alpha.alpha_equate_pair
    val alpha_equate_pairs = Alpha.alpha_equate_pairs
    val substitute         = Alpha.substitute

    fun cmp_list' cmp_one args =
      let
	fun loop (state,l1,l2) =
	  (case (l1,l2)
	     of ([],[]) => (state,EQUAL)
	      | (_,[])  => (state,GREATER)
	      | ([],_)  => (state,LESS)
	      | (a::aa,b::bb) =>
	       (case cmp_one(state,a,b)
		  of (state,EQUAL) => loop(state,aa,bb)
		   | other         => other))
      in loop args
      end

    fun cmp_list cmp_one (aa,bb) =
      let
	val cmp_one = fn (s,a,b) => (s,cmp_one (a,b))
	val (_,res) = cmp_list' cmp_one ((),aa,bb)
      in res
      end

    val cmp_var_list = cmp_list cmp_var

    fun cmp_pair cmp ((a1,b1),(a2,b2)) =
      (case cmp (a1,a2)
	 of EQUAL => cmp(b1,b2)
	  | other => other)

    fun cmp_option cmp (a,b) =
      (case (a,b)
	 of (SOME a, SOME b) => cmp (a,b)
	  | (SOME a, NONE) => GREATER
	  | (NONE, SOME b) => LESS
	  | (NONE, NONE) => EQUAL)

    fun cmp_orders [] = EQUAL
      | cmp_orders (EQUAL::rest) = cmp_orders rest
      | cmp_orders (first::rest) = first


    fun cmp_2int toint (a,b) = cmp_int (toint a,toint b)

    fun cmp_intsize args =
      let
	fun toint sz =
	  case sz of
	    Prim.W8 => 0
	  | Prim.W16 => 1
	  | Prim.W32 => 2
	  | Prim.W64 => 3
      in cmp_2int toint args
      end

    fun cmp_floatsize args =
      let
	fun toint sz =
	  case sz
	    of Prim.F32 => 0
	     | Prim.F64 => 1
      in cmp_2int toint args
      end

    fun cmp_openness args =
      let
	fun toint ope =
	  case ope
	    of Open    => 0
	     | Code    => 1
	     | Closure => 2
      in cmp_2int toint args
      end

    fun cmp_effect args =
      (case args
	 of (Total,Total)    => EQUAL
	  | (Total,Partial)  => LESS
	  | (Partial,Total)  => GREATER
	  | (Partial,Partial)=> EQUAL)

    fun cmp_bool (false, false) = EQUAL
      | cmp_bool (false, true)  = LESS
      | cmp_bool (true, true)   = EQUAL
      | cmp_bool (true, false)  = GREATER

    fun cmp_prim (context,pcon1,pcon2) =
      case (pcon1,pcon2)
	of (Int_c sz1, Int_c sz2)     => cmp_intsize(sz1,sz2)
	 | (Int_c _, _) => GREATER
	 | (_, Int_c _ ) => LESS

	 | (Float_c sz1, Float_c sz2) => cmp_floatsize(sz1,sz2)
	 | (Float_c _, _) => GREATER
	 | (_, Float_c _) => LESS

	 | (BoxFloat_c sz1, BoxFloat_c sz2) => cmp_floatsize(sz1,sz2)
	 | (BoxFloat_c _, _) => GREATER
	 | ( _, BoxFloat_c _) => LESS

	 | (Exn_c , Exn_c ) => EQUAL
	 | ( Exn_c , _) => GREATER
	 | ( _, Exn_c ) => LESS

	 | ( Array_c , Array_c ) => EQUAL
	 | (Array_c, _) => GREATER
	 | (_, Array_c) => LESS

	 | ( Vector_c, Vector_c) => EQUAL
	 | (Vector_c, _) => GREATER
	 | ( _, Vector_c) => LESS

	 | (Loc_c, Loc_c) => EQUAL
	 | (Loc_c, _) => GREATER
	 | (_, Loc_c) => LESS

	 | (Exntag_c, Exntag_c) => EQUAL
	 | ( Exntag_c, _) => GREATER
	 | (_, Exntag_c) => LESS

	 | (Sum_c {tagcount = t1, known = k1, totalcount = tt1} ,
	    Sum_c {tagcount = t2, known = k2, totalcount = tt2}) =>
	  cmp_orders [Word32.compare(t1, t2),
		      Word32.compare(tt1, tt2),
		      cmp_option Word32.compare (k1, k2)]
	 | ( Sum_c _ , _) => GREATER
	 | (_, Sum_c _ ) => LESS

	 | (Record_c l1, Record_c l2) =>
	  cmp_list cmp_label (l1,l2)
	 | ( Record_c _, _) => GREATER
	 | (_, Record_c _) => LESS

	 | (Vararg_c (o1, e1), Vararg_c (o2, e2)) =>
	  cmp_orders [cmp_openness (o1,o2),
		      cmp_effect (e1,e2)]
	 | (Vararg_c _, _) => GREATER
	 | (_, Vararg_c _) => LESS

	 | (GCTag_c, GCTag_c) => EQUAL

    fun cmp_kind (context,kind1,kind2) =
      (case (kind1,kind2) of
	 (Type_k, Type_k) => EQUAL
       | (Type_k,_)       => GREATER
       | (_,Type_k)       => LESS

       | (Single_k con1, Single_k con2) =>
	   cmp_con (context,con1,con2)
       |  (Single_k _,_)  => GREATER
       |  (_,Single_k _)  => LESS

       | (SingleType_k con1,SingleType_k con2) =>
	   cmp_con (context,con1,con2)
       |  (SingleType_k _,_)  => GREATER
       |  (_,SingleType_k _)  => LESS

       | (Record_k elts1_seq,Record_k elts2_seq) =>
	   let
	     val elts1 = Sequence.toList elts1_seq
	     val elts2 = Sequence.toList elts2_seq
	     fun cmp_one (context,((lbl1,var1),kind1),((lbl2,var2),kind2)) =
	       (case cmp_label(lbl1,lbl2)
		  of EQUAL =>
		    (alpha_equate_pair (context,(var1,var2)),
		     cmp_kind (context,kind1,kind2))
		   | other => (context,other))
	     val (_,res) = cmp_list' cmp_one (context,elts1,elts2)
	   in res
	   end
       | (Record_k _,_) => GREATER
       | (_,Record_k _) => LESS

       | (Arrow_k (openness1, formals1, return1),
	   (Arrow_k (openness2, formals2, return2))) =>
	   (case cmp_openness (openness1,openness2)
	      of EQUAL =>
		(case cmp_vklist (context,formals1,formals2)
		   of (context,EQUAL) => cmp_kind (context,return1,return2)
		    | (context,other) => other)
	       | other => other))

    and cmp_vklist args =
      let
	fun cmp_one (context,(var1,kind1),(var2,kind2)) =
	  (alpha_equate_pair (context,(var1,var2)),
	   cmp_kind (context,kind1,kind2))
      in cmp_list' cmp_one args
      end
    and cmp_con_list (context,l1,l2) = cmp_list (fn (a,b) => cmp_con (context,a,b)) (l1,l2)
    and cmp_con (context,con1,con2) =
      let
	val res =
	  (case (con1,con2)
	     of (*(Annotate_c (annot1,con1),con2) => cmp_con (context,con1,con2)
	      | (con1,Annotate_c (annot1,con2)) => cmp_con (context,con1,con2)
	      |*) (Prim_c (pcon1,args1),Prim_c (pcon2,args2)) =>
	       (case cmp_prim(context,pcon1,pcon2)
		  of EQUAL => cmp_con_list (context,args1,args2)
		   | other => other)
	      | (Prim_c _,_) => GREATER
	      | (_,Prim_c _) => LESS

	      | (Mu_c (flag1,defs1),Mu_c (flag2,defs2)) =>
		  (case cmp_bool(flag1,flag2)
		     of EQUAL =>
		       let
			 val def_list1 = Sequence.toList defs1
			 val def_list2 = Sequence.toList defs2
			 val (var_list1,con_list1) = ListPair.unzip def_list1
			 val (var_list2,con_list2) = ListPair.unzip def_list2
			 val context = if flag1 then alpha_equate_pairs (context,(var_list1,var_list2)) else context
		       in cmp_con_list (context,con_list1,con_list2)
		       end
		      | other => other)
	      | (Mu_c _,_) => GREATER
	      | (_,Mu_c _) => LESS

	      | (ExternArrow_c (args1,b1),ExternArrow_c (args2,b2)) =>
		(case cmp_con_list (context,args1,args2)
		   of EQUAL => cmp_con(context,b1,b2)
		    | other => other)
	      | (ExternArrow_c _,_) => GREATER
	      | (_,ExternArrow_c _) => LESS

	      | (AllArrow_c {openness = o1, effect = eff1,
			     tFormals = t1, eFormals = e1, fFormals = f1, body_type = b1},
  	         AllArrow_c {openness = o2, effect = eff2,
			     tFormals = t2, eFormals = e2, fFormals = f2, body_type = b2}) =>
		 (case (cmp_orders [cmp_openness(o1,o2),
				    cmp_word (f1,f2),
				    cmp_effect (eff1,eff2)])
		    of EQUAL =>
		      (case cmp_vklist (context,t1,t2)
			 of (context,EQUAL) =>
			   (case cmp_con_list (context, e2, e1)
			      of EQUAL => cmp_con (context,b1,b2)
			       | other => other)
			  | (_,other) => other)
		     | other => other)
	      | (AllArrow_c _,_) => GREATER
	      | (_,AllArrow_c _) => LESS

(*
	      | (Typeof_c (Var_e v1), Typeof_c (Var_e v2)) => cmp_var (v1,v2)

	      (* XXX need to fix this! *)
	      | (Typeof_c exp1, Typeof_c exp2) => EQUAL (*error "Typeof not handled correctly"*)
	      | (Typeof_c _,_) => GREATER
	      | (_,Typeof_c _) => LESS
*)

	      | (Var_c var1,Var_c var2) =>
		cmp_var(substitute(#1 context,var1),substitute(#2 context,var2))
	      | (Var_c _,_) => GREATER
	      | (_,Var_c _) => LESS

	      (*XXX ignores sort*)
	      | (Let_c (sort1, binds1,con1),Let_c (sort2, binds2,con2)) =>
		let

		  fun equate_fun context ((var1,formals1,con1),(var2,formals2,con2)) =
		    (case cmp_vklist (context,formals1,formals2)
		       of (context',EQUAL) =>
			 (alpha_equate_pair(context,(var1,var2)),
			  cmp_con (context',con1,con2))
			| (_,other) => (context,other))

		  fun cmp_one (context,bnd1,bnd2) =
		    (case (bnd1,bnd2)
		       of (Con_cb(var1,con1),Con_cb(var2,con2)) =>
			 (alpha_equate_pair(context,(var1,var2)),cmp_con (context,con1,con2))
			| (Con_cb _,_) => (context,GREATER)
			| (_,Con_cb _) => (context,LESS)

			| (Open_cb args1,Open_cb args2) => equate_fun context (args1,args2)
			| (Open_cb _,_) => (context,GREATER)
			| (_,Open_cb _) => (context,LESS)

			| (Code_cb args1,Code_cb args2) => equate_fun context (args1,args2))

		in
		  (case cmp_list' cmp_one (context,binds1,binds2)
		     of (context,EQUAL) => cmp_con (context,con1,con2)
		      | (_,other)       => other)
		end
	      | (Let_c _,_) => GREATER
	      | (_,Let_c _) => LESS


	      | (Closure_c (code1,env1),Closure_c (code2,env2)) =>
		(case cmp_con (context,code1,code2)
		   of EQUAL => cmp_con (context,env1,env2)
		    | other => other)
	      | (Closure_c _,_) => GREATER
	      | (_,Closure_c _) => LESS

	      | (Crecord_c entries1,Crecord_c entries2) =>
		 let
		   fun cmp_one ((lbl1,con1),(lbl2,con2)) =
		     (case cmp_label (lbl1,lbl2)
			of EQUAL => cmp_con (context,con1,con2)
			 | other => other)
		 in cmp_list cmp_one (entries1,entries2)
		 end
	      | (Crecord_c _,_) => GREATER
	      | (_,Crecord_c _) => LESS

	      | (Proj_c (crec1,label1),Proj_c (crec2,label2)) =>
		 (case cmp_label (label1,label2)
		    of EQUAL => cmp_con (context,crec1,crec2)
		     | other => other)
	      | (Proj_c _,_) => GREATER
	      | (_,Proj_c _) => LESS

	      | (App_c (cfun1,actuals1),App_c (cfun2,actuals2)) =>
		(case cmp_con(context,cfun1,cfun2)
		   of EQUAL => cmp_con_list (context,actuals1,actuals2)
		    | other => other)
	      | (App_c _,_) => GREATER
	      | (_,App_c _) => LESS

	      | (Coercion_c {vars = v1, from = f1, to = t1},
		     Coercion_c {vars = v2, from = f2, to = t2}) =>
		     (case cmp_var_list (v1, v2) of
			  EQUAL => (case cmp_con(context, f1, f2)
					of EQUAL => cmp_con(context, t1, t2)
				         | x => x)
			| x => x)

(*
	      | (Typecase_c {arg=arg1,arms=arms1,default=d1,kind=k1},
		 Typecase_c {arg=arg2,arms=arms2,default=d2,kind=k2}) =>
		   let
		     fun cmp_one (context,(pc1,f1,b1),(pc2,f2,b2)) =
		       (case cmp_prim (context,pc1,pc2)
			  of EQUAL =>
			    (case cmp_vklist (context,f1,f2)
			       of (context,EQUAL) => (context,cmp_con (context,b1,b2))
				| other           => other)
			   | other => (context,other))
		   in
		     (case cmp_con(context,arg1,arg2)
			of EQUAL =>
			  (case cmp_con (context,d1,d2)
			     of EQUAL =>
			       (case cmp_kind (context,k1,k2)
				  of EQUAL => #2(cmp_list' cmp_one (context,arms1,arms2))
				   | other => other)
			      | other => other)
			 | other => other)
		   end
*)
	       )
      in res
      end

    val cmp_con = fn(c1,c2) => cmp_con((Alpha.empty_context(),Alpha.empty_context()),c1,c2)

    structure Key =
      struct
	type ord_key = con * con
	val compare = cmp_pair cmp_con
      end

    structure T = SplaySetFn(Key)

    (*Needs to be symmetric: that is, if (a,b) is in the trail,
     * then (b,a) is as well.  This implemented in the lookup
     * to keep the trail smaller.  If the trail becomes a bottleneck,
     * profile this!!
     *)

    type trail = T.set

    val empty  = T.empty
    val equate = T.add

    fun equal (trail,(c1,c2)) =
      (T.member(trail,(c1,c2)) orelse
       T.member(trail,(c2,c1)))

  end
