structure LilDefs :> LILDEFS  = 
  struct

    val error = fn s => Util.error "lildefs.sml" s
    open Lil
    structure R = Reduce
    structure Dec = Deconstruct.Dec
    structure Elim = Deconstruct.Elim

    structure LO = Listops
    structure LS = LilSubst
    structure LU = LilUtil

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    fun VALOF opt s = 
      (case opt
	 of SOME a => a
	  | NONE => error s)

    structure K = 
      struct 
	fun Type s = mk_kind (T s)
	val T64' = mk_kind (T B8)
	fun T64 () = T64'
	val T32' = mk_kind (T B4)
	fun T32 () = T32'
	val TM' = mk_kind Tmem
	fun TM ()  = TM'

	fun ntuple (ks : kind list) : kind = 
	  (case ks
	     of [] => mk_kind Unit_k
	      | k::ks => mk_kind (Prod_k (k,ntuple ks)))
	     
	fun narrow (ks : kind list) (bk : kind) : kind =
	  (case ks
	     of [] => bk
	      | k::ks => mk_kind (Arrow_k(k,narrow ks bk)))

	fun sum ks = mk_kind (Sum_k ks)
	fun binsum (k1,k2) = sum [k1,k2]

	fun forall (j,k) = mk_kind (All_k (j,k))
	fun forall' f = 
	  let
	    val j = Name.fresh_named_var "j"
	  in forall (j,f j)
	  end
	fun arrow k1 k2 = mk_kind (Arrow_k (k1,k2))
	fun pair (k1,k2) = mk_kind (Prod_k (k1,k2))

	val nat' = mk_kind (Nat_k)
	fun nat () = nat'
	val unit' = mk_kind Unit_k
	fun unit () = unit'

	fun var j = mk_kind (Var_k j)
	fun mu (j,k) = mk_kind (Mu_k (j,k))
	fun mu' f = 
	  let
	    val j = Name.fresh_named_var "j"
	  in mu (j,f j)
	  end

	fun list' (k : Lil.kind) : Lil.kind = 
	  let
	    val j = Name.fresh_named_var "list"
	    val body = binsum (unit (),pair (k,var j))
	  in mk_kind (Mu_k (j,body))
	  end
	val listT32 = list' (T32())
	val listT64 = list' (T64())
	fun list (k : Lil.kind) = 
	  (case kout k
	     of T B4 => listT32
	      | T B8 => listT64
	      | _ => list' k)
	  	
	fun unrolled_list' (k : Lil.kind) : Lil.kind = 
	  let
	    val mu = list k	      
	  in binsum (unit (),pair (k,mu))
	  end
	val ulistT32 = unrolled_list' (T32())
	val ulistT64 = unrolled_list' (T64())

	fun unrolled_list (k : Lil.kind) = 
	  (case kout k
	     of T B4 => ulistT32
	      | T B8 => ulistT64
	      | _ => unrolled_list' k)

	val tlist' = 
	  let
	    val j = Name.fresh_named_var "list"
	    val body = binsum (unit (),pair (T32(),var j))
	  in mk_kind (Mu_k (j,body))
	  end
	  
	fun tlist () : Lil.kind = tlist'

      end


    structure KOps =
      struct
	fun sumw w k = 
	  (case Dec.K.sum' k
	     of SOME ks => ((SOME (List.nth (ks,(LU.w2i w)))) handle _ => NONE)
	      | NONE => NONE)
      end

    structure C =
      struct
	fun var a = mk_con (Var_c a)

	fun app c1 c2 = Lil.mk_con (Lil.App_c (c1,c2))
      
	fun appn c1 [] = c1
	  | appn c1 (c2::cs) = appn (app c1 c2) cs

	fun star () = mk_con Star_c
	fun pair c1 c2 = mk_con (Pair_c (c1,c2))

	fun k_app c k = 
	  Lil.mk_con (Lil.APP_c (c,k))

	fun pi1 c = 
	  (case Dec.C.pair' c
	     of SOME (c1,c2) => c1
	      | _ => mk_con (Pi1_c c))
	fun pi2 c = 
	  (case Dec.C.pair' c
	     of SOME (c1,c2) => c2
	      | _ => mk_con (Pi2_c c))

	fun nproj c i =
	  (case i
	     of 0 => pi1 c
	      | n => nproj (pi2 c) (n-1))
	     
	fun ntuple cs = 
	  (case cs
	     of [] => star()
	      | (c::cs) => pair c (ntuple cs))
	     
	fun nat i = mk_con(Nat_c i)
	fun nat' i = nat (TilWord32.fromInt i)

	fun pcon_app p cs = appn (mk_pcon p) cs
	  
	fun lambda (v,k) c =  mk_con (Lam_c ((v,k),c))

	fun lambda' k f = 
	  let val alpha = Name.fresh_named_var "alpha"
	  in lambda (alpha,k) (f (var alpha))
	  end

	fun nlambda aks body = 
	  (case aks
	     of [] => body
	      | ak::aks => lambda ak (nlambda aks body))
	     
	fun unfold' (k : kind) = 
	  (case kout k 
	     of Mu_k(j,k_in) => 
	       let
		 val a = Name.fresh_named_var "pr_arg"
		 val r = Name.fresh_named_var "pr_rec"
	       in mk_con (Pr_c (j,(a,k_in),k_in,r,mk_con(Var_c a)))
	       end
	      | _ => error "unfold at non-mu kind")

	val unfoldt32l = unfold' (K.tlist())
	val unfoldt64l = unfold' (K.list (K.T64()))

	fun unfold (k : kind) = 
	  (case Dec.K.list' k
	     of SOME eltk => 
	       (case kout eltk
		  of T B4 => unfoldt32l
		   | T B8 => unfoldt64l
		   | _ => unfold' k)
	      | _ => unfold' k)

	fun do_unfold (k : kind) c = 
	  (case Dec.C.fold' c
	     of SOME (k,c) => c
	      | _ => app (unfold k) c)

	fun sumcase (c : con) arms default = 
	  (case Dec.C.inj' c
	     of SOME (i,k,c) => 
	       (case (LO.assoc_eq (TilWord32.equal,i,arms),default) 
		  of (SOME f,_) => f c
		   | (_,SOME c) => c
		   | _ => error "sumcase has no appropriate arm and no default")
	      | NONE => 
		  let
		    fun mk_arm (w,f) = 
		      let
			val alpha = Name.fresh_named_var ("a_"^(TilWord32.toDecimalString w))
			val c =mk_con (Var_c alpha)
		      in (w,(alpha,f c))
		      end
		    val arms = map mk_arm arms
		  in
		    mk_con (Case_c (c,arms,default))
		  end)

	fun binsumcase c left right = sumcase c [(0w0,left),(0w1,right)] NONE

	fun inj w k c = mk_con (Inj_c (w,k,c))
	  
	fun inl k c = inj 0w0 k c
	fun inr k c = inj 0w1 k c

	fun fold k c = mk_con (Fold_c(k,c))

	fun nill' k = 
	  let
	    val listk = K.list k 
	    val sumk = K.unrolled_list k
	    val body = inl sumk (star ())
	  in fold listk body
	  end
	val nillT32 = nill' (K.T32())
	val nillT64 = nill' (K.T64())
	fun nill k = 
	  (case kout k
	     of T B4 => nillT32
	      | T B8 => nillT64
	      | _ => nill' k)

	fun cons k c = 
	  let
	    val listk = K.list k 
	    val sumk = K.unrolled_list k
	    val body = inr sumk c
	  in fold listk body
	  end

	fun cons' k (c1,c2) = cons k (pair c1 c2)

	fun list kind cs = 
	  (case cs 
	     of [] => nill kind
	      | c::cs => cons' kind (c,(list kind cs)))
	val tlist = list (K.T32 ())

	fun listcase (eltk : kind) (c : con) (ifnil : Lil.con) (ifcons : Lil.con -> Lil.con) : Lil.con =
	  let
	    val listk = K.list eltk
	    val c = do_unfold listk c
	  in 
	    binsumcase c (fn _ => ifnil) ifcons
	  end

	fun listcase' (eltk : kind) (c : con) (ifnil : Lil.con) (ifcons : Lil.con * Lil.con -> Lil.con) : Lil.con =
	  let
	    val listk = K.list eltk
	    val c = do_unfold listk c
	    fun ifcons' a = ifcons (pi1 a,pi2 a)
	  in 
	    binsumcase c (fn _ => ifnil) ifcons'
	  end

	(* nary_listcase (retk : kind) (eltk : kind) (c : con) (arms : (Lil.con list -> Lil.con) list) : Lil.con 
	 * c :: list_k[eltk],
	 * listcase c
	 * | [] => arms_1([])
	 * | [a] => arms_2([a])
	 * | [a,b] => arms_3([a,b])
	 * | (a::b::rest) => arms_4([a,b,rest])
	 *)
	fun nary_listcase (eltk : kind) (c : con) (arms : (Lil.con list -> Lil.con) list) : Lil.con =
	  let
	    fun doit (c, args,arm,[]) = arm (rev args)   (* default case *)
	      | doit (c, args,arm,arm'::arms) = 
	      (listcase' eltk c 
	       (arm (rev args))
	       (fn (hd,tl) => doit(tl,hd::args,arm',arms)))
	  in case arms 
	       of [] => error "Must have at least one case"
		| arm::arms => doit (c,[],arm,arms)
	  end


	fun closure () = 
	  lambda' (K.list (K.T32())) 
	  (fn args => 
	   lambda' (K.list (K.T64()))
	   (fn fargs => 
	    lambda' (K.T32())
	    (fn body => 
	     let
	       val venv_v = Name.fresh_named_var "venv_t"
	       val venv_t = mk_con (Var_c venv_v)
	       val args = cons' (K.T32()) (venv_t,args)
	       val code_t = pcon_app Code_c [args,fargs,body]
	       val tup_t = pcon_app Tuple_c [code_t,venv_t]
	       val lam_t = lambda (venv_v,K.T32()) tup_t
	       val exists_t = app (k_app (Lil.mk_pcon Exists_c) (K.T32())) lam_t
	     in exists_t
	     end)))
	  
      end

    structure COps = 
      struct
	fun ksum2sum ksum = 
	  let val (which,tagcount,sum_args) = VALOF (Dec.C.ksum' ksum) "ksum2sum: not a ksum"
	  in C.pcon_app Lil.Sum_c [tagcount,sum_args]
	  end
	
	fun ksum2arm ksum = 
	  let 
	    val (which,tagcount,sum_args) = VALOF (Dec.C.ksum_ml' ksum) "ksum2arm: not a ksum"
	  in if which < tagcount then C.pcon_app Tag_c [C.nat which]
	     else (List.nth (sum_args,TilWord32.toInt (which - tagcount)) handle _ => error "ksum2arm: out of range")
	  end
	
	fun sum2ksum (which : Lil.con) (sumtype : Lil.con) = 
	  let
	    val (tagcount,carriers) = VALOF (Dec.C.sum' sumtype) "sum2ksum: not a sum"
	  in
	    C.pcon_app Lil.KSum_c [which,
				   tagcount,
				   carriers]
	  end

	fun sum2ksum' which sumtype = sum2ksum (C.nat which) sumtype


	fun sum_totalcount c = 
	  let
	    val (tagcount,sum_args) = VALOF (Dec.C.sum_ml' c) "sum_totalcount: not a sum"
	    val len = LU.i2w(List.length sum_args)
	  in TilWord32.uplus (tagcount,len)
	  end

	fun project_i_fields i c = LO.map0count (fn i => C.nproj c i) i
	fun ntuple2tlist i c = C.tlist (project_i_fields i c)
      end


    structure T = 
      struct

	fun tuple ts =  C.pcon_app Tuple_c [ts]
	fun tuple' ts = tuple (C.tlist ts)

	fun ptr t = mk_con (Ptr_c t)

	fun tupleptr ts = ptr (tuple ts)
	fun tupleptr' ts = ptr (tuple' ts)
	fun unit () = tupleptr' []

	fun void () = mk_pcon Void_c

	fun array sz c = C.pcon_app (Lil.Array_c sz) [c]
	fun coercion from to = C.pcon_app Coercion_c [from,to]

	fun sum tagcount carriers = C.pcon_app Lil.Sum_c [tagcount,carriers]
	fun sum' tagcount carriers =
	  let
	    val sum_args = C.tlist carriers
	    val tagcount = C.nat tagcount
	  in sum tagcount sum_args
	  end

	fun ksum which tagcount carriers =
	  C.pcon_app Lil.KSum_c [which,
				 tagcount,
				 carriers]

	fun ksum' which tagcount carriers =
	  let
	    val sum_args = C.tlist carriers
	    val which = C.nat which
	    val tagcount = C.nat tagcount
	  in ksum which tagcount sum_args
	  end

	fun float () = mk_pcon Float_c
	fun intt s = mk_pcon (Int_c s)

	fun boxed s c = C.pcon_app (Boxed_c s) [c]
	fun boxed_float () = boxed B8 (mk_pcon Float_c)

	fun float64 ()     = mk_pcon Float_c

	fun dyntag c = C.pcon_app Dyntag_c [c]
	fun tag iw = C.pcon_app Tag_c [C.nat iw]
	fun bool () = sum' 0w2 []

	fun externarrow size args fargs ret = 
	  C.pcon_app (ExternArrow_c size) [args,fargs,ret]

	fun externarrow' size args fargs ret = 
	  let
	    val args  = C.tlist args
	    val fargs = C.list (K.T64()) fargs
	  in externarrow size args fargs ret
	  end

	fun arrow args fargs body = 
	  C.pcon_app Arrow_c [args,fargs,body]

	fun arrow' args fargs body = 
	  let
	    val args  = C.tlist args
	    val fargs = C.list (K.T64()) fargs
	  in arrow args fargs body
	  end

	fun code args fargs body = 
	  C.pcon_app Code_c [args,fargs,body]

	fun code' args fargs body = 
	  let
	    val args  = C.tlist args
	    val fargs = C.list (K.T64()) fargs
	  in code args fargs body
	  end

	fun forall' k c = C.app (C.k_app (Lil.mk_pcon Forall_c) k) c
	fun exists' k c = C.app (C.k_app (Lil.mk_pcon Exists_c) k) c

	fun forall (v,k) c = forall' k (mk_con (Lam_c ((v,k),c)))
	fun exists (v,k) c = exists' k (mk_con (Lam_c ((v,k),c)))
	    
	fun nary_forall vks c = 
	  (case vks 
	     of [] => c
	      | vk::vks => forall vk (nary_forall vks c))

	fun allarrow vks t32s t64s rt = nary_forall vks (arrow t32s t64s rt)
	fun allarrow' vks t32s t64s rt = nary_forall vks (arrow' t32s t64s rt)

	fun allcode vks t32s t64s rt = nary_forall vks (code t32s t64s rt)
	fun allcode' vks t32s t64s rt = nary_forall vks (code' t32s t64s rt)


	fun closure vks args fargs rtype =
	  let
	    val venv_v = Name.fresh_named_var "venv_t"
	    val venv_t = mk_con (Var_c venv_v)
	    val args = C.cons' (K.T32()) (venv_t,args)
	    val code_t = allcode vks args fargs rtype
	    val tup_t = ptr (tuple' [code_t,venv_t])
	    val lam_t = C.lambda (venv_v,K.T32()) tup_t
	    val exists_t = exists' (K.T32()) lam_t
	  in exists_t
	  end 

	fun exn () = 
	  let
	    val a = Name.fresh_named_var "exnval_t"
	    val c = mk_con (Var_c a)
	    val ttype = tupleptr' [dyntag c,c]
	  in exists (a,K.T32()) ttype
	  end

	fun mu f =
	  let
	    val unit_c = C.star()
	    val unit_k = K.unit()
	      
	    val outer_k = K.arrow unit_k (K.T32())

	    val f = 
	      C.lambda' outer_k 
	      (fn r => C.lambda' unit_k
	       (fn w => f (C.app r w)))
	  in C.app (C.k_app (Lil.mk_pcon Lil.Rec_c) unit_k) f
	  end

	(* Must box the succ case, because sum is taglike.
	 *)
	fun nat () = mu (fn a => sum' 0w1 [tupleptr' [a]])
	fun list t = 
	  mu (fn a => sum' 0w1 [tupleptr' [a,t]])
      end

    (* Coercions *)
    structure Q = 
      struct
	fun forgetknown ksum = Coercion(ForgetKnown,[ksum])
	fun projknown ksum = Coercion(ProjKnown,[ksum])
	fun injunion ksum = Coercion(InjUnion,[ksum])
	fun injforget ksum = Coercion(InjForget,[ksum])
	fun pack t_as t_hiding = Coercion(Pack, [t_as,t_hiding])

	fun coerce q sv = Coerce(q,sv)

	fun coerce_many qs sv = List.foldr Coerce sv qs
      end


    (* I want a compose operator for arbitrary arity, dammit! *)
    fun ret  f = fn a => P.ret (f a)
    fun ret2 f = fn a => fn b => P.ret (f a b)
    fun ret3 f = fn a => fn b => fn c => P.ret (f a b c)
    fun ret4 f = fn a => fn b => fn c => fn d => P.ret (f a b c d)
    fun ret5 f = fn a => fn b => fn c => fn d => fn e => P.ret (f a b c d e)

    fun op2sv  f = fn a => P.SV32.from_op (P.ret (f a))
    fun op2sv2 f = fn a => fn b => P.SV32.from_op (P.ret (f a b))
    fun op2sv3 f = fn a => fn b => fn c => P.SV32.from_op (P.ret (f a b c))
    fun op2sv4 f = fn a => fn b => fn c => fn d => P.SV32.from_op (P.ret (f a b c d))
    fun op2sv5 f = fn a => fn b => fn c => fn d => fn e => P.SV32.from_op (P.ret (f a b c d e))

    fun fop2sv  f = fn a => P.SV64.from_op (P.ret (f a))
    fun fop2sv2 f = fn a => fn b => P.SV64.from_op (P.ret (f a b))
    fun fop2sv3 f = fn a => fn b => fn c => P.SV64.from_op (P.ret (f a b c))


    fun tosv  f = fn a => P.SV32.from_op (f a)
    fun tosv2 f = fn a => fn b => P.SV32.from_op (f a b)
    fun tosv3 f = fn a => fn b => fn c => P.SV32.from_op (f a b c)
    fun tosv4 f = fn a => fn b => fn c => fn d => P.SV32.from_op (f a b c d)
    fun tosv5 f = fn a => fn b => fn c => fn d => fn e => P.SV32.from_op (f a b c d e)
    fun tosv6 f = fn a => fn b => fn c => fn d => fn e => fn g => P.SV32.from_op (f a b c d e g)


    structure E =
      struct


	fun op2exp op32 = P.Lili.op_to_exp (P.ret op32)
	fun mk_lilprim lp cs sv32s sv64s = 
	  LilPrimOp32(lp,cs,sv32s,sv64s)

	fun unit' () = Unit
	val unit = ret unit'

	fun tuple'' vals = case vals of [] => Val Unit | _ => mk_lilprim Tuple [] vals []
	val tuple'  = ret tuple''
	val tuple   = op2sv tuple''

	fun select'' i sv = mk_lilprim (Lil.Select i) [] [sv] []
	val select' = ret2 select''
	val select  = op2sv2 select''

	fun dyntag'' dcon = mk_lilprim Dyntag [dcon] [] []
	val dyntag' = ret dyntag''
	val dyntag = op2sv dyntag''

	fun tag' i = (Tag i)
	val tag = ret tag'

	fun tag_and_box'' i sv = tuple'' [tag' i,sv]
	val tag_and_box' = ret2 tag_and_box''
	val tag_and_box  = op2sv2 tag_and_box''

	fun injunion' ksum sv = 
	  let
	    val q = Q.injunion ksum
	  in Q.coerce q sv
	  end
	val injunion = ret2 injunion'

	fun project' ksum sv = 
	  let
	    val q = Q.projknown ksum
	  in Q.coerce q sv
	  end
	val project = ret2 project'

	fun forget' ksum sv = 
	  let
	    val q = Q.forgetknown ksum
	  in Q.coerce q sv
	  end
	val forget = ret2 forget'

	fun injforget' ksum sv = 
	  let
	    val q = Q.injforget ksum
	  in Q.coerce q sv
	  end
	val injforget = ret2 injforget'

	fun pack' t_as t_hiding sv = 
	  let val q = Q.pack t_as t_hiding
	  in Q.coerce q sv
	  end
	val pack = ret3 pack'

	fun unpack (sv : Lil.sv32) : Lil.sv32 P.pexp = 
	  let
	    val a = Name.fresh_named_var "unpack_t"
	    val x = Name.fresh_named_var "unpack_x"
	    val bnd = Unpack_b (a,x,sv)
	  in
	    P.Lili.from_bnds([bnd],Var_32 x)
	  end

	(*inject into sum and forget *)
	fun inj_nontag_from_sumtype' i sumtype sv = 
	  let
	    val ksum = COps.sum2ksum' i sumtype
	  in injforget' ksum sv
	  end
	val inj_nontag_from_sumtype = ret3 inj_nontag_from_sumtype'

	fun tapp' c sv = TApp (sv,c)
	val tapp = ret2 tapp'

	fun nary_tapp' cons sv = 
	  (case cons
	     of [] => sv
	      | (c::cc) => nary_tapp' cc (tapp' c sv))

	val nary_tapp = ret2 nary_tapp'

	fun allapp'' f cs sv32s sv64s = 
	  let
	    val f = nary_tapp'  cs f
	  in Lil.App (f,sv32s,sv64s)
	  end
	val allapp' = ret4 allapp''
	val allapp  = op2sv4 allapp''

	fun allcall'' f cs sv32s sv64s = 
	  let
	    val f = nary_tapp'  cs f
	  in Lil.Call (f,sv32s,sv64s)
	  end
	val allcall' = ret4 allcall''
	val allcall  = op2sv4 allcall''

	fun nary_tabs' vks sv  = 
	  (case vks
	     of [] => sv
	      | (vk::vks) => Tabs (vk,nary_tabs' vks sv))
	val nary_tabs = ret2 nary_tabs'

	fun unbox'' sv = Unbox sv
	val unbox' = ret unbox''
	val unbox  = fop2sv unbox''

	fun box'' sv64 = mk_lilprim Box [] [] [sv64]
	val box' = ret box''
	val box = op2sv box''


	fun update_array64'' arr i sv64 = Prim32 (Prim.update (Prim.FloatArray Prim.F64),[],[arg32 arr,arg32 i,arg64 sv64])
	val update_array64' = ret3 update_array64''
	val update_array64 = op2sv3 update_array64''

	fun update_array32'' t arr i sv32 = Prim32 (Prim.update (Prim.OtherArray true),[t],[arg32 arr,arg32 i,arg32 sv32])
	val update_array32' = ret4 update_array32''
	val update_array32 = op2sv4 update_array32''

	fun sub_array64'' arr i = Prim64 (Prim.sub (Prim.FloatArray Prim.F64),[arg32 arr,arg32 i])
	val sub_array64' = ret2 sub_array64''
	val sub_array64 = fop2sv2 sub_array64''

	fun sub_array32'' t arr i = Prim32 (Prim.sub (Prim.OtherArray true),[t],[arg32 arr,arg32 i])
	val sub_array32'  = ret3 sub_array32''
	val sub_array32  = op2sv3 sub_array32''

	fun create_array64'' len sv64 = Prim32 (Prim.create_table (Prim.FloatArray Prim.F64),[],[arg32 len,arg64 sv64])
	val create_array64' = ret2 create_array64''
	val create_array64   = op2sv2 create_array64''

	fun create_array32'' t len sv32 = Prim32 (Prim.create_table (Prim.OtherArray true),[t],[arg32 len,arg32 sv32])
	val create_array32'  = ret3 create_array32''
	val create_array32   = op2sv3 create_array32''
	  
	fun create_empty_array64'' () = Prim32 (Prim.create_empty_table (Prim.FloatArray Prim.F64),[],[])
	val create_empty_array64'  = ret create_empty_array64''
	val create_empty_array64   = op2sv create_empty_array64''

	fun create_empty_array32'' t = Prim32 (Prim.create_empty_table (Prim.OtherArray true),[t],[])
	val create_empty_array32'  = ret create_empty_array32''
	val create_empty_array32   = op2sv create_empty_array32''

	fun length_array64'' arr = Prim32 (Prim.length_table (Prim.FloatArray Prim.F64),[],[arg32 arr])
	val length_array64' = ret length_array64''
	val length_array64  = op2sv length_array64''

	fun length_array32'' t arr = Prim32 (Prim.length_table (Prim.OtherArray true),[t],[arg32 arr])
	val length_array32'  = ret2 length_array32''
	val length_array32   = op2sv2 length_array32''

	fun equal_array64'' arr1 arr2 = Prim32 (Prim.equal_table (Prim.FloatArray Prim.F64),[],[arg32 arr1,arg32 arr2])
	val equal_array64' = ret2 equal_array64''
	val equal_array64  = op2sv2 equal_array64''

	fun equal_array32'' t arr1 arr2 = Prim32 (Prim.equal_table (Prim.OtherArray true),[t],[arg32 arr1,arg32 arr2])
	val equal_array32'  = ret3 equal_array32''
	val equal_array32   = op2sv3 equal_array32''

	fun lambda formals rtype body = 
	  let
	    val f = Name.fresh_named_var "anon_lambda"
	    val e = Function {tFormals = [],
			      eFormals = formals,
			      fFormals = [],
			      rtype    = rtype,
			      body     = body}
	  in P.Lili.from_bnds([Fixcode_b [(f,e)]],Var_32 f)
	  end

	
	fun letinj w (c,sv) : Lil.con P.pexp = 
	  let
	    val a = Name.fresh_named_var "inj_a"
	    val bnd = Inj_b (w,a,c,sv)
	  in P.Lili.from_bnds ([bnd],mk_con (Var_c a))
	  end

	fun letfold (c : Lil.con) : Lil.con P.pexp   = 
	  let
	    val a = Name.fresh_named_var "unfolded"
	    val bnd = Unfold_b (a,c)
	  in P.Lili.from_bnds ([bnd],(mk_con (Var_c a)))
	  end

	fun letsplit (con : Lil.con) : (Lil.con * Lil.con) P.pexp =
	  let	    
	    val b = Name.fresh_named_var "pi1"
	    val c = Name.fresh_named_var "pi2"
	    val bnd = Split_b (b,c,con)
	  in P.Lili.from_bnds ([bnd],(mk_con (Var_c b),mk_con (Var_c c)))
	  end

	fun vcase (w : w32) (c : con) (exp : con -> sv32 P.pexp) (sv : sv32) : sv32 P.pexp = 
	  P.bind (letinj w (c,sv)) (fn c => exp c)

	fun letpath (con : Lil.con) : Lil.con P.pexp option = 
	  let
	    exception None
	    fun letpath (con : Lil.con) : Lil.con P.pexp = 
	      (case cout (R.whnf con)
		 of Var_c _ => P.ret con
		  | Pi1_c c => P.bind (letpath c) 
		   (fn cv => P.bind (letsplit cv)
		    (fn (a,b) => P.ret a))
		  | Pi2_c c => P.bind (letpath c) 
		   (fn cv => P.bind (letsplit cv)
		    (fn (a,b) => P.ret b)) 
		  | Fold_c (k,c) => P.bind (letpath c)
		   (fn cv => letfold cv)
		  | Inj_c _ => P.ret con
		  | _ => raise None)
	  in (SOME (letpath con)) handle None => NONE
	  end

	fun list_refine_to_nil (srep : Lil.con) (sv : Lil.sv32) : Lil.con P.pexp = 
	  let 
	  in P.bind (letfold srep) (fn c => letinj 0w0 (c,sv))
	  end

	fun list_refine_to_cons (srep : Lil.con) (sv : Lil.sv32) : Lil.con P.pexp = 
	  let 
	  in P.bind (letfold srep) (fn c => letinj 0w1 (c,sv))
	  end

	fun list_refine_to_cons' (srep : Lil.con) (sv : Lil.sv32) : (Lil.con * Lil.con) P.pexp = 
	  let 
	  in P.bind (letfold srep) (fn c => P.bind (letinj 0w1 (c,sv)) (fn c => letsplit c))
	  end

(*
	fun list_vcase_cons' (rtype : Lil.con) (srep : Lil.con) (eltk : Lil.kind) (sv : Lil.sv32) (exp : Lil.var * Lil.var -> Lil.exp) : Lil.op32 P.pexp = 
	  let
	    val sumk = K.unrolled_list eltk
	    val exp = fn a => P.Lili.exp_to_exp (P.map exp (letsplit (mk_con (Var_c a))))
	  in P.bind (letfold srep) (fn a => 
				    let 
				      val c = mk_con (Var_c a)
				    in vcase' rtype c 0w1 sumk sv exp
				    end)
	  end
	val list_vcase_cons = tosv5 list_vcase_cons'


	fun list_vcase_nth' (rtype : Lil.con) (srep : Lil.con) (eltk : Lil.kind) (sv : Lil.sv32) (exp : Lil.var list -> Lil.exp) (n : int) : Lil.op32 P.pexp = 
	  let
	    fun loop (tvars,srep,i) = 
	      let
	      in
		case i
		  of 0 => list_vcase_nil' rtype srep eltk (exp (rev tvars)) sv
		   | n => 
		    let 
		      val newexp = fn (hd,tl) => P.Lili.op_to_exp(loop(hd::tvars,mk_con (Var_c tl),n-1))
		    in list_vcase_cons' rtype srep eltk sv newexp
		    end
	      end
	  in loop([],srep,n)
	  end
	val list_vcase_nth = tosv6 list_vcase_nth'
*)
	
	fun closure (codeptr,venv,closure_type,venv_type) =
	  let
	    val tup = tuple [codeptr,venv]
	    val closure = P.bind tup (pack closure_type venv_type)
	  in closure
	  end

	fun closure_app' (closure,cargs,args,fargs) = 
	  ((P.bind (unpack closure)
	    (fn tup => 
	     P.bind (select 0w0 tup)
	     (fn f => 
	      P.bind (select 0w1 tup)
	      (fn v => allcall' f cargs (v::args) fargs))))
	   handle any => (print "closure app failure\n";
			  PpLil.pp_sv32 closure;print "\n";
			  raise any))
	   

	val closure_app = tosv closure_app'

	fun inj_exn dcon dtag dval = 
	  let
	    val tup = tuple [dtag,dval]
	    val etype = T.exn()
	  in P.map (pack' etype dcon) tup
	  end

	fun bool' b = 
	  let
	    val iw = if b then 0w1 else 0w0
	    val ksum = COps.sum2ksum' iw (T.bool())
	  in injforget' ksum (tag' iw)
	  end
	val bool = ret bool'

	fun zero' () = 
	  let
	    val sum = T.nat()
	    val ksum = COps.sum2ksum' 0w0 sum
	  in injforget' ksum (tag' 0w0)
	  end
	val zero = ret zero'

	fun succ n = 
	  let
	    val sum = T.nat()
	    val ksum = COps.sum2ksum' 0w1 sum
	  in P.bind (tuple [n]) (fn sv => P.ret (injforget' ksum sv))
	  end

	fun nat n = 
	  if n = 0 then zero () 
	  else P.bind (nat (n-1)) succ

	fun natcase'' rtype n (ifzero : exp) (ifsucc : sv32 -> exp) = 
	  let
	    val arm0 = (0w0,Name.fresh_named_var "zero",ifzero)
	    val arm1 = 
	      let
		val sum = T.nat()
		val ksum = COps.sum2ksum' 0w1 sum
		val x = Name.fresh_named_var "succ"
		val x_i = project ksum (Var_32 x)
		val exp = P.map ifsucc x_i
		val exp = P.Lili.exp_to_exp exp
	      in (0w1,x,exp)
	      end
	  in
	    Switch (Sumcase {arg = n,
			     arms = [arm0,arm1],
			     default = NONE,
			     rtype = rtype})
	  end
	val natcase' = ret4 natcase''
	val natcase  = op2sv4 natcase''

	fun mk_let bnds e = 
	  mk_exp (Let_e (case #e e
			   of Let_e (bnds',e) => (bnds@bnds',e)
			    | _ => (bnds,e)))
      end
    
    structure EOps = 
      struct
	fun project_all_tuple_fields''  sv width : op32 list =  
	  Listops.map0count (fn i => E.select'' (LU.i2w i) sv) width
	val project_all_tuple_fields' = ret2 project_all_tuple_fields''
	fun project_all_tuple_fields sv width = 
	  P.List.map (P.SV32.from_op o P.ret)  (project_all_tuple_fields' sv width)
      end

  end