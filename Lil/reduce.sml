structure Reduce :> REDUCE = 
  struct
    open Lil

    fun error s = Util.error "reduce.sml" s

    structure LS = LilSubst
    structure LU = LilUtil

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    fun whnf (c : Lil.con) : Lil.con = 
      (case #whnf c
	 of ref (SOME whnfc_) => 
	   let
	     val whnfc = mk_con whnfc_
	     val () = set_whnf(whnfc,whnfc)
	   in whnfc
	   end
	  | r => 
	   let
	     

	     val res = 
	       (case cout c
		  of App_c _ => 
		    let
		      (* This is a little ugly, but it turns out that
		       * nary applications can be a real bottleneck. 
		       *)
		      fun beta1 (f,arg,ksubst,csubst) =
			(case cout (whnf f)
			   of Lam_c ((a,_),body) => SOME (ksubst,LS.C.sim_add csubst (a,arg),body)
			    | Pr_c (j,(a,k1),k2,b,body) =>
			     (case cout (whnf arg)
				of Fold_c (k,c) => 
				  let
				    val ksubst = LS.K.sim_add ksubst (j,mk_kind(Mu_k (j,k)))
				    val csubst = LS.C.sim_add csubst (a,c)
				    val csubst = LS.C.sim_add csubst (b,LS.substConKindInCon (csubst,ksubst) f)
				  in SOME (ksubst,csubst,body)
				  end
				 | _ => NONE)
			    | _ => NONE)
			   
		      fun unapp (c,args) =
			(case cout c
			   of App_c (f,arg) => unapp(f,arg::args)
			    | _ => (c,args))
		      fun appn (f,args) = 
			(case args
			   of [] => f
			    | (arg::args) => appn(mk_con (App_c (f,arg)),args))

		      val (f,args) = unapp (c,[])
		      fun applyn (f,args,ksubst,csubst) = 
			(case args 
			   of [] => whnf(LS.substConKindInCon (csubst,ksubst) f)
			    | arg::args => 
			     (case beta1 (f,arg,ksubst,csubst)
				of SOME (ksubst,csubst,body) => applyn (body,args,ksubst,csubst)
				 | NONE => 
				  if LS.C.is_empty csubst then
				    appn (LS.substKindInCon ksubst f,arg::args)
				  else 
				    let
				      val f = LS.substConKindInCon (csubst,ksubst) f
				      val (f,args) = unapp (f,arg::args)
				    in applyn(f,args,LS.K.empty(),LS.C.empty())
				    end))

				  
		    in applyn (f,args,LS.K.empty(),LS.C.empty())
		    end
(*		  of App_c (c1,c2) => 
		    (case cout (whnf c1)
		       of Lam_c ((a,_),body) => 
			 whnf (LS.varConConSubst a c2 body)
			| Pr_c (j,(a,k1),k2,b,body) =>
			 (case cout (whnf c2)
			    of Fold_c (k,c) => 
			      let
				val ksubst = LS.K.sim_add(LS.K.empty()) (j,mk_kind(Mu_k (j,k)))
				val csubst = LS.C.sim_add(LS.C.empty()) (a,c)
				val csubst = LS.C.sim_add(csubst)       (b,c1)
			      in whnf (LS.substConKindInCon (csubst,ksubst) body)
			      end
			     | _ => c)
			| _ => c)*)
		   | APP_c (c1,k) => 
	            (case cout (whnf c1)
		       of LAM_c (j,body) => whnf (LS.varKindConSubst j k body)
			| _ => c)
		   | Pi1_c c1 =>
		    (case cout (whnf c1)
		       of Pair_c (c1,c2) => whnf c1
			| _ => c)
		   | Pi2_c c1 =>
	            (case cout (whnf c1)
		       of Pair_c (c1,c2) => whnf c2
			| _ => c)
		   | Case_c (arg,arms,default)=> 
	            (case cout (whnf arg)
		       of Inj_c (i,_,cl) => 
			 (case (Listops.assoc_eq(TilWord32.equal,i,arms),default)
			    of (SOME (a,c),_) => whnf (LS.varConConSubst a cl c)
			     | (NONE,SOME c) => whnf c
			     | _ => error "Bad case")
			| _ => c)
		   | _ => c)
	     val () = set_whnf (c,res)
	   in res
	   end)
	 
  end