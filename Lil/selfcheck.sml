(*
 * Do some well-formedness checks on the various defined forms to
 * try and ensure that the definitions are correct and consistent.
 *)

structure SelfCheck :> SELFCHECK =
  struct
    open Lil

    structure LC = LilContext
    structure LD = LilDefs
    structure LU = LilUtil
    structure TD = TranslationDefs
    structure LO = Listops
    structure T = LilTypecheck
    structure Elim = Deconstruct.Elim

    val error = fn s => Util.error "selfcheck.sml" s
    type context = LC.context 
      
    fun newk env = 
      let
	val j = Name.fresh_named_var "j"
      in (LC.bind_kvar (env,j,LC.Any),LD.K.var j,j)
      end

    fun mk_cvars env ks = 
      let
	fun newc (k,env) = 
	  let
	    val a = Name.fresh_named_var "a"
	  in ((mk_con (Var_c a),a),LC.bind_cvar (env,(a,k)))
	  end
      in LO.foldl_acc newc env ks
      end

    val report = ref true
    fun msg s = if !report then print ("\t"^s^"\n") else ()

    
    fun docheck () = 
      let
	val () = msg "Beginning Selfcheck"

	val env0 = LC.empty()
	val j0 = Name.fresh_named_var "j"
	val k0 = LD.K.var j0
	val (env,k1,j1) = newk env0
	val (env,k2,j2) = newk env
	val (env,k3,j3) = newk env
	val (env,k4,j4) = newk env
	val (env,k5,j5) = newk env
	val (env,k6,j6) = newk env
	val (env,k7,j7) = newk env
	val (env,k8,j8) = newk env
	val (env,k9,j9) = newk env

	val kind_only_env = env

	val klist = [k1,k2,k3,k4,k5,k6,k7,k8,k9]

	val a0 = Name.fresh_named_var "a"
	val c0 = mk_con (Var_c a0)

	val (mkclist as  [(c1,a1),
			  (c2,a2),
			  (c3,a3),
			  (c4,a4),
			  (c5,a5),
			  (c6,a6),
			  (c7,a7),
			  (c8,a8),
			  (c9,a9)
			  ],env) = mk_cvars env klist
	  
	val(clist,cvarlist) = LO.unzip mkclist
	val cklist = LO.zip clist klist
	  
	(* Tuples *)

	val () = msg "Pair kinds"
	(* Binary *)
	val pairk       = LD.K.pair (k1,k2)
	val _ = T.K.check env pairk
	val con_pair = LD.C.pair c1 c2
	val _ = T.C.check env con_pair pairk

	val () = msg "Nary tuple kinds"
	(* 0 indexed n-ary tuples *)
	val ntuplek  = LD.K.ntuple klist
	val _ = T.K.check env ntuplek
	val ntuplec = LD.C.ntuple clist
	val _ = T.C.check env ntuplec ntuplek
	val prjks = LO.mapcount (fn (i,k) => (LD.C.nproj ntuplec i,k)) klist
	val _ = app (fn (c,k) => T.C.check env c k) prjks

	val () = msg "Arrow kinds"
	(* Arrows *)
	val arrowk      = LD.K.arrow k1 k2
	val _ = T.K.check env arrowk
	val lambda = LD.C.lambda (a0,k1) c2
	val _ = T.C.check env lambda arrowk
	val app = LD.C.app lambda c1
	val _ = T.C.check env app k2

	val () = msg "Nary arrow kinds"
	val nary_arrowk = LD.K.narrow klist k1
	val _ = T.K.check env nary_arrowk
	(* Reusing bound variables already in context, so must rename *)
	val nary_lambda = LilRename.renameCon (LD.C.nlambda (LO.zip cvarlist klist) c1)
	val _ = T.C.check env nary_lambda nary_arrowk
	val nary_app =  LD.C.appn nary_lambda clist
	val _ = T.C.check env nary_app k1

	val () = msg "Kind binary sums"
	(* Sums *)
	val sumk  = LD.K.binsum (k1,k2)
	val _ = T.K.check env sumk
	val inl = LD.C.inl sumk c1
	val inr = LD.C.inr sumk c2
	val _ = T.C.check env inl sumk
	val _ = T.C.check env inr sumk
	val sumcase = LD.C.binsumcase inl (fn _ => c3) (fn _ => c3)
	val _ = T.C.check env sumcase k3

	val () = msg "Kind nary sums"
	val nary_sumk   = LD.K.sum klist
	val _ = T.K.check env nary_sumk
	val injs = LO.mapcount (fn (i,(c,k)) => LD.C.inj (LU.i2w i) nary_sumk c) cklist
	val _ = List.app (fn c => T.C.check env c nary_sumk) injs
	val nary_case = LD.C.sumcase (hd injs) (LO.mapcount (fn (i,c) => (LU.i2w i,fn _ => c)) injs) NONE
	val _ = T.C.check env nary_case nary_sumk 

	val () = msg "Universal kinds"
	(* Universals *)
	val forallk     = LD.K.forall (j0,k0)
	val _ = T.K.check env forallk
	(* No Lambda def in lildefs yet *)
	val forallenv = LC.bind_cvar (env,(a0,forallk))
	val kapp = LD.C.k_app c0 k1
	val _ = T.C.check forallenv kapp k1

	val () = msg "Inductive kinds"
	(* Inductive kinds *)
	val musumk = LD.K.binsum (k0,k1)
	val muk     = LD.K.mu (j0,musumk)
	val _ = T.K.check env muk

	val fold = LD.C.fold muk (LD.C.inr (Elim.K.unfold muk) c1)
	val _ = T.C.check env fold muk
	val unfold = LD.C.unfold muk
	val _ = T.C.check env unfold (LD.K.arrow muk (Elim.K.unfold muk))
	val do_unfold = LD.C.do_unfold muk fold 
	val _ = T.C.check env do_unfold (Elim.K.unfold muk)

	val () = msg "Lists"
	(* Lists *)
	val listk       = LD.K.list k1
	val _ = T.K.check env listk
	val tlistk      = LD.K.tlist ()
	val _ = T.K.check env tlistk

	val nill = LD.C.nill k1
	val _ = T.C.check env nill listk
	val tnill = LD.C.nill (LD.K.T32())
	val _ = T.C.check env tnill tlistk
	val cons = LD.C.cons' k1 (c1,nill)
	val _ = T.C.check env cons listk
	val tcons = LD.C.cons' (LD.K.T32()) (LD.T.void(),tnill)
	val _ = T.C.check env tcons tlistk

	val list_of_cons = LD.C.list k1 [c1,c1,c1,c1,c1]
	val _ = T.C.check env list_of_cons listk

	val () = msg "Listcase"
	(* listcase eltk arg ifnil ifcons  :: retk *)
	val listcase' = LD.C.listcase' k1 list_of_cons c2 (fn (hd,tl) => c2)
	val _ = T.C.check env listcase' k2

	val () = msg "nary listcase"
	(* nary_listcase (eltk : kind) (c : con) (arms : (Lil.con list -> Lil.con) list) : Lil.con  *)
	val nary_listcase = LD.C.nary_listcase k1 list_of_cons [fn _ => c2,fn _ => c2, fn _ => c2]
	val _ = T.C.check env nary_listcase k2

	(*LD.T *)

	val () = msg "Closures"
	(* closures *)
	val _ = 
	  let 
	    val env = LC.empty()
	    val a1 = Name.fresh_named_var "code_targ"
	    val t1 = mk_con (Var_c a1)
	    val args = LD.C.tlist [t1]
	    val fargs = LD.C.nill (LD.K.T64())
	    val rt = t1
	    val clostype = LD.T.closure [(a1,LD.K.T32())] args fargs rt
	    val () = msg "Closure type"
	    val () = T.T.check32 env clostype
	    val cenvv = Name.fresh_named_var "cenv"
	    val codetype = 
	      LD.T.allcode [(cenvv,LD.K.unit()),(a1,LD.K.T32())] 
	      (LD.C.tlist [LD.T.unit(),t1]) fargs rt

	    val () = msg "Code type"
	    val () = T.T.check32 env codetype

	    val codev = Name.fresh_named_var "codev"
	    val venvv = Name.fresh_named_var "venv"
	    val env = LC.bind_var32 (env,(codev,codetype))
	    val env = LC.bind_var32 (env,(venvv,LD.T.unit()))
	    val codeptr = LD.E.tapp' (LD.C.star()) (Var_32 codev)
	    val () = msg "Closure"
	    val closure = P.Lili.to_exp (LD.E.closure (codeptr,Var_32 venvv,clostype,LD.T.unit()))
	    val () = T.E.check env closure clostype
(*

	val closure_app' : (Lil.sv32 * Lil.sv32 list * Lil.sv64 list) -> Lil.op32 P.pexp
	val closure_app  : (Lil.sv32 * Lil.sv32 list * Lil.sv64 list) -> Lil.sv32 P.pexp
*)
	  in ()
	  end
	val () = msg "Translation defs"
      (* Check translation defs*)
	val env = LC.empty()
	val Tmil = TD.Tmil()
	val () = msg "Tmil"
	val _ = T.K.check env Tmil

	val srep1_a = Name.fresh_named_var "static_rep1"
	val srep1 = mk_con (Var_c srep1_a)
	val env = LC.bind_cvar (env,(srep1_a,Tmil))
	val srep2_a = Name.fresh_named_var "static_rep2"
	val srep2 = mk_con (Var_c srep2_a)
	val env = LC.bind_cvar (env,(srep2_a,Tmil))
	val a1 = Name.fresh_named_var "type1"
	val c1 = mk_con (Var_c a1)
	val a2 = Name.fresh_named_var "type2"
	val c2 = mk_con (Var_c a2)
	val env = LC.bind_cvar (env,(a1,LD.K.T32()))
	val env = LC.bind_cvar (env,(a2,LD.K.T32()))

	val () = msg "interp(c)"
	val t = TD.interp srep1
	val _ = T.C.check env t (LD.K.T32())

	val () = msg "R(c)"
	val R_t = TD.R srep1
	val _ = T.C.check env R_t (LD.K.T32())

	val () = msg "Array"
	val _ = T.C.check env (TD.Array srep1) (LD.K.TM())
	val () = msg "Arrayptr"
	val _ = T.C.check env (TD.Arrayptr srep1) (LD.K.T32())
	val () = msg "Vararg"
	val _ = T.C.check env (TD.Vararg srep1 srep2) (LD.K.T32())
	val () = msg "IfTaglike"
	val _ = T.C.check env (TD.IfTaglike srep1 c1 c2) (LD.K.T32())

	val at = Name.fresh_named_var "recordfieldt"
	val ct = mk_con (Var_c at)
	val env = LC.bind_cvar (env,(at,LD.K.T32()))
	val amem = Name.fresh_named_var "a_tmem"
	val cmem = mk_con (Var_c amem)
	val env = LC.bind_cvar (env,(amem,LD.K.TM()))

	val () = msg "TupleRep"
	val _ = T.C.check env (TD.TupleRep [ct]) Tmil
	val () = msg "BfloatRep"
	val _ = T.C.check env (TD.BFloatRep()) Tmil
	val () = msg "PtrRep"
	val _ = T.C.check env (TD.PtrRep cmem) Tmil
	val () = msg "OtherRep"
	val _ = T.C.check env (TD.OtherRep c1) Tmil

	val () = msg "vararg"
	val _ = T.E.synth env (P.Lili.to_exp(TD.mk_vararg_fn()))
	val () = msg "onearg"
	val _ = T.E.synth env (P.Lili.to_exp(TD.mk_onearg_fn()))

	val () = msg "project_dyn"
	val _ = T.E.synth env (P.Lili.to_exp(TD.mk_project_dynamic_fn()))
	val () = msg "inject_dyn"
	val _ = T.E.synth env (P.Lili.to_exp(TD.mk_inject_dynamic_fn()))

	val () = msg "SelfCheck succeeded"

      in ()
      end

    fun check () = (docheck ()) 
      handle any => 
	let 
	  val () = print "\nSelf Check failed\n"
	  val () = case any 
		     of UtilError.BUG {file,msg} => print ("BUG: "^file^": "^msg^"\n")
		      | _ => ()
	in if !report then raise any
	   else (print "Re-running self-check with tracing\n";
		 report := true;
		 (docheck ()) handle any => (report := false;raise any))
	end
  end