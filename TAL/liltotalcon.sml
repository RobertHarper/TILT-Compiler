structure LilToTalCon :> LILTOTALCON = 
struct
    structure TTD = TalTranslationDefs
    structure LC = LilContext
    structure LS = LilSubst
    structure LD = LilDefs
    structure LU = LilUtil
    structure LO = Listops
    structure Dec = Deconstruct.Dec
    structure Elim = Deconstruct.Elim
    structure TA = TalAbbrev
    structure TE = LilToTalEnv

    val error = fn s => Util.error "liltotal.sml" s
    val debug = Stats.ff "LilToTalConDebug"

    val debuglev = ref 0
    val chatlev = ref 0 

    fun chatp i = !(chatlev) >= i
    fun chat i s = if chatp i then print s else ()
    fun debugdo (i,t) = if (!debug) andalso (i <= !debuglev) then (t(); ()) else ()

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    fun ktrans kind =
      (case TA.K.find kind
	 of SOME tk => tk
	  | NONE => 
	   let
	     val recur = ktrans 
	     val res = 
	       (case kout kind 
		  of Lil.T s => Tal.kbyte s
		   | Lil.Tmem => Tal.kmem
		   | Lil.Unit_k => Tal.kprod []
		   | Lil.Nat_k => Tal.kint 
		   | Lil.Var_k j => Tal.kvar j
		   | Lil.Arrow_k (k1,k2) => TA.K.share kind (Tal.karrow (recur k1) (recur k2))
		   | Lil.Prod_k (k1,k2)  => TA.K.share kind (Tal.kprod [recur k1,recur k2])
		   | Lil.Sum_k ks   => TA.K.share kind (Tal.ksum (map recur ks))
		   | Lil.Mu_k (j,k) => TA.K.share kind (Tal.kmu [(j,recur k)] j)
		   | Lil.All_k (j,k) => error "Forall kinds cannot be translated to TAL")
	   in res
	   end)


    fun fixup_case ks arms def bound = 
	 let

	   val arms = LO.insertion_sort (fn (a,b) => LU.cmpw32(#1 a,#1 b)) arms

	   fun fixarm (a,c) = 
	     let
	       val c = LS.varConConSubst a (LD.C.var bound) c
	     in c
	     end

	   val arms = 
	     (case def 
		of NONE => map (fixarm o #2) arms
		 | SOME def => (* May be missing some arms *)
		  let
		    (* invariant: arm number >= iw *)
		    fun doarms (arms,ks,iw) = 
		      (case (arms,ks)
			 of ([],[]) => []
			  (* Left over arms not covered - use default *)
			  | ([],ks) => (LO.map (fn _ => def) ks) 
			  | ((w,arm)::restarms,ki::ks) => 
			   (* Arms skip a number: insert default and continue *)
			   if w > iw then def :: (doarms (arms,ks,iw + 0w1))
			   (* Found the arm: *)
			   else (* w = iw *)
			     fixarm arm :: doarms (restarms,ks,iw + 0w1)
			  | _ => error "Bad case")
		  in doarms (arms,ks,0w0)
		  end)
	 in arms
	 end

    (* TAL doesn't use lists for stacks, so we need to 
     * special case this if we want to be able to have functions
     * with "dynamic" number of args
     *)
    fun ctrans_stack (env : TE.env) (con : Lil.con) : Tal.con = 
      (case Dec.C.list' con
	 of SOME (_,cs) => TTD.T.stackargs (map (ctrans' env) cs)
	  | NONE => 
      (case Dec.C.cons_ml' con
	 of SOME (hd,tl) => TTD.T.stackargcons (ctrans' env hd) (ctrans_stack env tl)
	  | NONE => 
      (case Dec.C.sumcase' con
	 of SOME (arg,arms,def) =>
	   let
	     val ks = Dec.K.sum (TE.kindof env arg)
	     val arg = ctrans' env arg
	     val bound = Name.fresh_named_var "Case_arg"
	     val arms = fixup_case ks arms def bound
	     val arms = ListPair.map (fn (ki,ci) => ctrans_stack (TE.bind_cvar(env,(bound,ki))) ci) (ks,arms)
	   in TTD.C.sumcase arg bound arms
	   end
	  | _ => 
	   (print "Type is:\n";
	    PpLil.pp_con con;
	    error "Function args must be lists or cases"))))



    and ctrans' (env : TE.env) (con : Lil.con) : Tal.con =  
      let
	val res = 
	  (case TA.C.find con
	     of SOME tc => tc
	      | NONE => 
	       let 
		 val () = chat 6 "Rewriting con\n"
		 val () = debugdo(7,fn () => (print "Con is: \n";
					      PpLil.pp_con con;
					      print "\n"))

		 val docon = ctrans' env 
		 val docons = map docon
		 val dokind = ktrans

		 fun rewrite_defined_form c = 
		   case Dec.C.code' c
		     of SOME (args,fargs,rtype) => SOME(TTD.T.code (ctrans_stack env args) (ctrans_stack env fargs) (docon rtype))
         | NONE => case Dec.C.sum_ml' c
		     of SOME(w,arms) => SOME(TTD.T.sum w (docons arms))
         | NONE => case Dec.C.ksum_ml' c
		     of SOME(which,w,arms) => SOME(TTD.T.ksum which w (docons arms))
         | NONE => case Dec.C.tuple_ml' c
		     of SOME fields => SOME(TTD.T.tuple (docons fields))
         | NONE => case Dec.C.exists_ml' c
		     of SOME ((v,k),c) => SOME(TTD.T.exists (v,dokind k) (ctrans' (TE.bind_cvar (env,(v,k))) c))
         | NONE => case Dec.C.forall_ml' c
		     of SOME ((v,k),c) => SOME(TTD.T.forall (v,dokind k) (ctrans' (TE.bind_cvar (env,(v,k))) c))
         | NONE => case Dec.C.externarrow_ml' c
		     of SOME (sz,args,fargs,rtype) => SOME(TTD.T.externarrow sz (docons args) (docons fargs) (docon rtype))
         | NONE => case Dec.C.polyprim' c
		     of SOME(Lil.Embed_c sz,[],[c]) => SOME(TTD.T.embed sz (docon c))
		      | SOME (Lil.Rec_c,[k],cs) => 
			  let
			    val k = dokind k
			    fun eta () = TTD.T.eta_rek k
			    fun partial clam = 
			      (case Dec.C.lam' clam
				 of SOME ((r,_),c) => TTD.T.partial_rek ((r,k),docon c)
				  | NONE => eta())
			    fun total clam carg =
			      TTD.C.app (partial clam) (docon carg)
			  in
			    (case cs
			       of [] => SOME(eta())
				| [clam] => SOME(partial clam)
				| [clam,carg] => SOME(total clam carg)
				| _ => NONE)
			  end
	 | _ => NONE
			    (*  END get_defined_form *)

		   
		 val res = 
		   case rewrite_defined_form con
		     of SOME c => TA.C.share con c
		      | NONE => 
   	           (* Translate the con directly (assuming all defined forms have been checked for) *)
		   (case cout con 
		     of Lil.Var_c v => TTD.C.var v
		      | Lil.Nat_c w => TTD.C.nat w
		      | Lil.Star_c => TTD.C.unit
		      | Lil.Prim_c p => TTD.T.primcon p
		      | large =>   (* Abbreviate larger types if possible. *)
		       let
			 val res = 
			   (case large
			      of Lil.App_c (c1,c2) => TTD.C.app (docon c1) (docon c2)
			       | Lil.Lam_c ((a,k),c) =>
				let 
				  val tk = dokind k
				  val env = TE.bind_cvar (env,(a,k))
				  val tc = ctrans' env c
				in TTD.C.lam a tk tc
				end
			       | Lil.Pi1_c c => TTD.C.pi1 (docon c)
			       | Lil.Pi2_c c => TTD.C.pi2 (docon c)
			       | Lil.Pr_c (j,(a,k),k',r,body)  => 
				let
				  val tk = dokind k
				  val env = TE.bind_kvar(env,j,LC.Pos)
				  val tk' = dokind k'
				  val env = TE.bind_cvar(env,(a,k))
				  val env = TE.bind_cvar(env,(r,LD.K.arrow (LD.K.var j) k'))
				  val body = ctrans' env body
				in TTD.C.pr (j,(a,tk),tk',r,body)
				end
			       | Lil.Case_c (arg,arms,def) =>
				(* The TAL case construct has no default, and uses the same bound var
				 * in each arm.  This does the impedance matching.
				 *)
				let
				  val ks = Dec.K.sum (TE.kindof env arg)
				  val arg = docon arg
				  val bound = Name.fresh_named_var "Case_arg"
				  val arms = fixup_case ks arms def bound
				  val arms = ListPair.map (fn (ki,ci) => ctrans' (TE.bind_cvar(env,(bound,ki))) ci) (ks,arms)
				in TTD.C.sumcase arg bound arms
				end
			       | Lil.Pair_c (c1,c2) => TTD.C.pair (docon c1) (docon c2)
			       | Lil.Inj_c (w,k,c) => 
				let
				  val k = dokind k
				  val c = docon c
				in TTD.C.inj w k c
				end
			       | Lil.Fold_c (k,c) =>
				let
				  val k = dokind k
				  val c = docon c
				in TTD.C.fold k c
				end
			       | Lil.Ptr_c c => TTD.C.ptr (docon c)
			       | Lil.APP_c (c,k) => error "APP_c: No TAL translation"
			       | Lil.LAM_c (j,c) => error "LAM_c: No TAL translation"
			       | _ => error "Unreachable")
		       in TA.C.share con res
		       end) (* large *)
	       in res
	       end)
      in res 
      end  (* rewrite_con *)

    fun ctrans env con = 
      let
	val con = TE.subst_con env con
      in ctrans' env con
      end

end  (* LilToTal *)