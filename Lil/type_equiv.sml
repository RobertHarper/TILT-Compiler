structure LilTypeEquiv :> LILTYPEEQUIV = 
  struct
    open Lil
    structure LC = LilContext
    structure LO = Listops
    structure LD = LilDefs
    structure LS = LilSubst
    structure R = Reduce
    structure VarMap = Name.VarMap

    val equate = Stats.ff "LilEquivEquateCons"
    val warn = Stats.ff "LilEquivWarnOnFailure"

    type env = var VarMap.map

    fun empty env = (VarMap.numItems env) = 0

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    fun equal_var env v1 v2 = 
      (Name.eq_var (v1,v2)) orelse
      (case VarMap.find (env,v1)
	 of SOME v2' => Name.eq_var (v2,v2')
	  | NONE     => false)

    fun equate_vars env v1 v2 =
      VarMap.insert(VarMap.insert(env,v1,v2),v2,v1)
      
    fun equal_list equal args1 args2 = LO.all2 (fn (a,b) => equal a b) (args1,args2)
    fun equal_opt equal arg1 arg2 = 
      (case (arg1,arg2) 
	 of (SOME v1,SOME v2) => equal v1 v2
	  | (NONE,NONE) => true

	  | _ => false)

    structure K = 
      struct
	fun equal env (k1 : kind) (k2 : kind) : bool = eq_kind (k1,k2) orelse
	  let
	    val res = 
	      (case (kout k1,kout k2)
		 of (T s1,T s2) => s1 = s2
		  | (Tmem,Tmem) => true
		  | (Unit_k,Unit_k) => true
		  | (Nat_k,Nat_k)   => true
		  | (Arrow_k args1,Arrow_k args2) => equal2 env args1 args2
		  | (Prod_k args1,Prod_k args2)     => equal2 env args1 args2
		  | (Sum_k args1,Sum_k args2) => equal_list (equal env) args1 args2
		  | (Var_k j1,Var_k j2) => equal_var env j1 j2
		  | (Mu_k args1,Mu_k args2) => equaljk env args1 args2
		  | (All_k args1,All_k args2) => equaljk env args1 args2
		  | _ => false)
		 
	    val () =  if (not res) andalso (!warn) then (print "\tWARNING: kind equivalence failure - kinds are:\n";
							 print "k1 = ";PpLil.pp_kind k1;print "\n";
							 print "k2 = ";PpLil.pp_kind k2;print "\n")
		      else ()
	  in res
	  end
	and equal2 env (k11,k12) (k21,k22) = 
	  equal env k11 k21 andalso 
	  equal env k12 k22
	and equaljk env (j1,k1) (j2,k2) = equal (equate_vars env j1 j2) k1 k2
      end      

    structure C = 
      struct
	fun equal_primcon p1 p2 = p1 = p2

	fun a_equal env c1 c2 = 
	  eq_con (c1,c2) orelse
	  let
	    val equal = 
	      case (cout c1,cout c2) 
		of (Var_c a1,Var_c a2) => equal_var env a1 a2
		 | (Nat_c w1,Nat_c w2) => w1 = w2
		 | (App_c args1,App_c args2) => a_equal2 env args1 args2
		 | (APP_c (c1,k1), APP_c (c2,k2)) => a_equalkc env (k1,c1) (k2,c2)
		 | (Pi1_c c1, Pi1_c c2) => a_equal env c1 c2
		 | (Pi2_c c1, Pi2_c c2) => a_equal env c1 c2
		 | (Prim_c pcon1,Prim_c pcon2) => equal_primcon pcon1 pcon2
		 | (Pr_c (j1,(a1,k_in1),k1,r1,c1),Pr_c (j2,(a2,k_in2),k2,r2,c2)) =>
		  let
		    val env' = equate_vars env j1 j2
		    val env'' = equate_vars (equate_vars env' a1 a2) r1 r2
		  in 
		    K.equal env k1 k2 andalso
		    K.equal env' k_in1 k_in2 andalso
		    a_equal env'' c1 c2
		  end
		 | (Case_c (c1,arms1,def1),Case_c (c2,arms2,def2)) =>
		  let
		    fun equalwvc env (w1,(a1,c1)) (w2,(a2,c2)) = 
		      TilWord32.equal (w1,w2) andalso
		      a_equal (equate_vars env a1 a2) c1 c2 
		  in
		    a_equal env c1 c2 andalso
		    equal_list (equalwvc env) arms1 arms2 andalso
		    equal_opt (a_equal env) def1 def2
		  end
		 | (LAM_c (j1,c1), LAM_c (j2,c2)) => a_equal (equate_vars env j1 j2) c1 c2
		 | (Lam_c ((a1,k1),c1), Lam_c ((a2,k2),c2)) =>
		  let
		    val env' = equate_vars env a1 a2
		  in 
		    K.equal env k1 k2 andalso
		    a_equal env' c1 c2
		  end
		 | (Pair_c args1,Pair_c args2) => a_equal2 env args1 args2
		 | (Star_c,Star_c) => true
		 | (Inj_c (w1,k1,c1),Inj_c (w2,k2,c2)) => TilWord32.equal (w1,w2) andalso (a_equalkc env (k1,c1) (k2,c2))
		 | (Fold_c args1,Fold_c args2) => a_equalkc env args1 args2
		 | (Ptr_c c1,Ptr_c c2) => a_equal env c1 c2
		 | _ => false

	  in equal
	  end
	and a_equal2 env (c11,c12) (c21,c22) = a_equal env c11 c21 andalso a_equal env c12 c22
	and a_equalkc env (k1,c1) (k2,c2) = K.equal env k1 k2 andalso a_equal env c1 c2

	fun equal env (c1 : Lil.con) (c2 : Lil.con) : bool = a_equal env c1 c2 orelse
	  let
	    val c1 = R.whnf c1
	    val c2 = R.whnf c2
	    val equal = 
	      case (cout c1,cout c2) 
		of (Var_c a1,Var_c a2) => equal_var env a1 a2
		 | (Nat_c w1,Nat_c w2) => w1 = w2
		 | (App_c args1,App_c args2) => equal2 env args1 args2
		 | (APP_c (c1,k1), APP_c (c2,k2)) => equalkc env (k1,c1) (k2,c2)
		 | (Pi1_c c1, Pi1_c c2) => equal env c1 c2
		 | (Pi2_c c1, Pi2_c c2) => equal env c1 c2
		 | (Prim_c pcon1,Prim_c pcon2) => equal_primcon pcon1 pcon2
		 | (Pr_c (j1,(a1,k_in1),k1,r1,c1),Pr_c (j2,(a2,k_in2),k2,r2,c2)) =>
		  let
		    val env' = equate_vars env j1 j2
		    val env'' = equate_vars (equate_vars env' a1 a2) r1 r2
		  in 
		    K.equal env k1 k2 andalso
		    K.equal env' k_in1 k_in2 andalso
		    equal env'' c1 c2
		  end
		 | (Case_c (c1,arms1,def1),Case_c (c2,arms2,def2)) =>
		  let
		    fun equalwvc env (w1,(a1,c1)) (w2,(a2,c2)) = 
		      TilWord32.equal (w1,w2) andalso
		      equal (equate_vars env a1 a2) c1 c2 
		  in
		    equal env c1 c2 andalso
		    equal_list (equalwvc env) arms1 arms2 andalso
		    equal_opt (equal env) def1 def2
		  end
		 | (LAM_c (j1,c1), LAM_c (j2,c2)) => equal (equate_vars env j1 j2) c1 c2
		 | (Lam_c ((a1,k1),c1), Lam_c ((a2,k2),c2)) =>
		  let
		    val env' = equate_vars env a1 a2
		  in 
		    K.equal env k1 k2 andalso
		    equal env' c1 c2
		  end
		 | (Pair_c args1,Pair_c args2) => equal2 env args1 args2
		 | (Star_c,Star_c) => true
		 | (Inj_c (w1,k1,c1),Inj_c (w2,k2,c2)) => TilWord32.equal (w1,w2) andalso (equalkc env (k1,c1) (k2,c2))
		 | (Fold_c args1,Fold_c args2) => equalkc env args1 args2
		 | (Ptr_c c1,Ptr_c c2) => equal env c1 c2
		 | _ => false

	    val () =  if (not equal) andalso (!warn) then (print "\tWARNING: con equivalence failure - cons are:\n";
							   print "c1 = ";PpLil.pp_con c1;print "\n";
							   print "c2 = ";PpLil.pp_con c2;print "\n")
		      else ()
			
	  in equal
	  end
	(* Assumes WHNF already.
	 * This only does one direction of the eta:
	 * an eta reduction might apply on the opposite side even if this fails.
	 *)
	and eta_equal_left env c1 c2 = 
	  (case cout c1 
	     of (*Case_c args => equal_case_eta env args c2
	      | *)LAM_c args => equal_LAM_eta env args c2
	      | Lam_c args => equal_Lam_eta env args c2
	      | Pair_c args => equal_Pair_eta env args c2
	      |_ => false)
	and equal2 env (c11,c12) (c21,c22) = equal env c11 c21 andalso equal env c12 c22
	and equalkc env (k1,c1) (k2,c2) = K.equal env k1 k2 andalso equal env c1 c2
(*	and equal_case_eta env (retk,argc,((a1,k1),c1),((a2,k2),c2)) otherc =
	  let
	    val sumk = mk_kind (Sum_k (k1,k2))
	    val inl = mk_con (Inl_c (sumk,mk_con (Var_c a1)))
	    val inr = mk_con (Inr_c (sumk,mk_con (Var_c a2)))
	  in
	    equal env c1 inl andalso
	    equal env c2 inr andalso
	    equal env argc otherc
	  end
*)
	and equal_LAM_eta env (j1,c1) c2 = 
	  let
	    val j1' = Name.derived_var j1
	    val knew = (mk_kind (Var_k j1')) 
	    val c1 = LS.varKindConSubst j1 knew c1
	    val c2 = mk_con (APP_c (c2,knew))
	  in equal env c1 c2
	  end
	and equal_Lam_eta env ((a1,k1),c1) c2 = 
	  let
	    val a1' = Name.derived_var a1
	    val cnew = (mk_con (Var_c a1')) 
	    val c1 = LS.varConConSubst a1 cnew c1
	    val c2 = mk_con (App_c (c2,cnew))
	  in equal env c1 c2
	  end
	and equal_Pair_eta env (c1,c2) c3 = 
	  equal env c1 (mk_con (Pi1_c c3)) andalso
	  equal env c2 (mk_con (Pi2_c c3))
      end

    structure K = 
      struct 
	val equal = K.equal VarMap.empty
      end
    structure C = 
      struct 
	fun a_equal c1 c2 = 
	  let
	    val equal = C.a_equal VarMap.empty c1 c2
	  in equal
	  end

	fun equal c1 c2 = 
	  let
	    val equal = C.equal VarMap.empty c1 c2
	  in equal
	  end
      end


    (*
	fun equal env c1 c2 k = 
	  (case kout k
	     of T s => sequal env c1 c2
	      | TD => sequal env c1 c2
	      | TU => sequal env c1 c2
	      | Unit_k => true
	      | Nat_k => sequal env c1 c2
	      | Arrow_k (k1,k2) => 
	       let 
		 val a = mk_con (Var_c (Name.fresh_named_var "a"))
		 val env = bind_con env (a,k1)
	       in
		 equal env (LD.C.app c1 a) (LD.C.app c2 a) k2
	       end
	      | Prod_k (k1,k2)  => 
	       equal env (LD.C.pi1 c1) (LD.C.pi1 c2) k1 andalso
	       equal env (LD.C.pi2 c1) (LD.C.pi2 c2) k1
	      | Sum_k (k1,k2)   => sequal env c1 c2
	      | Var_k j => sequal env c1 c2
	      | Mu_k (j,k)  => equal env (LD.C.do_unfold k c1) (LD.C.do_unfold k c2)
	      | All_k (j,k) => equal env (LD.C.kapp c1 (LD.K.var_k j)) (LD.C.kapp c2 (LD.K.var_k j)))
*)


  end