structure Synthesis :> SYNTHESIS = 
  struct
    open Lil
    structure LD = LilDefs
    structure LC = LilContext
    structure LS = LilSubst
    structure LO = Listops
    structure PU = LilPrimUtil
    structure LU = LilUtil

    structure Dec = Deconstruct.Dec
    structure Elim = Deconstruct.Elim

    fun error s = Util.error "synthesis.sml" s

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    fun find_kvar args  = ((LC.find_kvar args)  handle LC.Unbound s => error ("Kind variable unbound: "^s))
    fun find_cvar args  = ((LC.find_cvar args)  handle LC.Unbound s => error ("Con variable unbound: "^s))
    fun find_var32 args = ((LC.find_var32 args) handle LC.Unbound s => error ("Sv32 variable unbound: "^s))
    fun find_var64 args = ((LC.find_var64 args) handle LC.Unbound s => error ("Sv64 variable unbound: "^s))
    fun find_label args = ((LC.find_label args) handle LC.Unbound s => error ("Label unbound: "^s))

    fun split_cvar args = ((LC.split_cvar args) 
			   handle LC.Unbound s => error ("Split of unbound variable: "^s)
				| LC.Rebound s => error ("Split rebinds variable: "^s))
    fun unfold_cvar args = ((LC.unfold_cvar args) 
			    handle LC.Unbound s => error ("Unfold of unbound variable: "^s)
				 | LC.Rebound s => error ("Unfold rebinds variable: "^s))

    fun vcase_cvar args = ((LC.vcase_cvar args) 
			   handle LC.Unbound s => error ("Vcase of unbound variable: "^s)
				| LC.Rebound s => error ("Vcase rebinds variable: "^s))

    structure Kindof = 
      struct
	type env = LC.context
	val T32 = LD.K.T32()
	val T8 = LD.K.Type B1
	val T64 = LD.K.T64()
	val T32or64 = LD.K.T32or64()
	val TM = LD.K.TM()
	val T32_to_TM = LD.K.arrow T32 TM
	val T64_to_TM = LD.K.arrow T64 TM
	val T32_to_T32 = LD.K.arrow T32 T32
	val T8_to_T32 = LD.K.arrow T8 T32
	val Nat = LD.K.nat()
	val Nat_to_T32 = LD.K.arrow Nat T32
	val T32list = LD.K.tlist()
	val T64list = LD.K.list T64
	val Nat_to_T32list_to_T32 = LD.K.narrow [Nat, T32list] T32
	val Nat_to_Nat_to_T32list_to_T32 = LD.K.arrow Nat Nat_to_T32list_to_T32
	val T32list_to_TM =  LD.K.arrow T32list TM
	val T32list_to_T64list_t_T32_to_T32 = LD.K.narrow [T32list, T64list, T32] T32
	val T32_to_T32_to_T32 = LD.K.arrow T32 T32_to_T32
	val Forallj_jtoT_to_T = LD.K.forall' (fn j => LD.K.arrow (LD.K.arrow (LD.K.var j) T32) T32)
	val RecKind = LD.K.forall' (fn j => 
				    let val jtoT = LD.K.arrow (LD.K.var j) T32
				    in LD.K.arrow (LD.K.arrow jtoT jtoT) jtoT
				    end)
	fun primcon p = 
	  (case p
	     of Int_c s => LD.K.Type s
	      | Float_c => T64
	      | Void_c  => T32
	      | Tag_c      => Nat_to_T32
	      | Sum_c      => Nat_to_T32list_to_T32
	      | KSum_c     => Nat_to_Nat_to_T32list_to_T32
	      | Tuple_c    => T32list_to_TM
	      | Boxed_c B8 => T64_to_TM
	      | Embed_c B1 => T8_to_T32
	      | Array_c B4  => T32_to_TM
	      | Array_c B8  => T64_to_TM
	      | Arrow_c    => T32list_to_T64list_t_T32_to_T32
	      | Code_c     => T32list_to_T64list_t_T32_to_T32
	      | Dyntag_c   => T32_to_T32
	      | Coercion_c => T32_to_T32_to_T32
	      | Exists_c   => Forallj_jtoT_to_T
	      | Forall_c   => Forallj_jtoT_to_T
	      | Rec_c      => RecKind
	      | ExternArrow_c s => LD.K.narrow [LD.K.list T32or64,
						LD.K.Type s] T32
	      | Boxed_c size => LD.K.arrow (LD.K.Type size) TM
	      | Embed_c size => LD.K.arrow (LD.K.Type size) T32
	      | Array_c size  => LD.K.arrow (LD.K.Type size) TM
	      | Ref_c  => T32_to_TM)
	     
	fun con env c = 
	  (case cout c
	     of Var_c a => find_cvar (env,a)
	      | Nat_c _ => Nat
	      | App_c (c1,c2) => 
	       let val k1 = con env c1
	       in Elim.K.app k1
	       end
	      | APP_c (c1,k) =>
	       let val k1 = con env c1
	       in Elim.K.APP k1 k
	       end
	      | Pi1_c c1 => Elim.K.pi1 (con env c1)
	      | Pi2_c c2 => Elim.K.pi2 (con env c2)
	      | Prim_c p => primcon p
	      | Pr_c (j,(a,k),k',r,body)  => LD.K.arrow (LD.K.mu (j,k)) (LS.varKindKindSubst j (LD.K.mu (j,k)) k')
	      | Case_c (arg,arms,def) =>
	       (case (def,arms) 
		  of (SOME c,_) => con env c
		   | (NONE,(w,(a,body))::_) => 
		    (case LD.KOps.sumw w (con env arg)
		       of SOME k => con (LC.bind_cvar(env,(a,k))) body
			| NONE => error "Bad case")
		   | _ => error "Bad case")
	      | LAM_c (j,c) => LD.K.forall (j,con (LC.bind_kvar (env,j,LC.Any)) c)
	      | Lam_c ((a,k),c) => LD.K.arrow k (con (LC.bind_cvar (env,(a,k))) c)
	      | Pair_c (c1,c2) => LD.K.pair (con env c1,con env c2)
	      | Star_c  => LD.K.unit()
	      | Inj_c (i,k,c)  => k
	      | Fold_c (k,c) => k
	      | Ptr_c c => T32)
      end

    structure Typeof = 
      struct
	type env = LC.context

	fun coercion env ((q,args) : coercion) = 
	  (case (q,args)
	     of (Roll, [to])   => LD.T.coercion (Elim.C.unroll to) to
	      | (Unroll, [from]) => LD.T.coercion from (Elim.C.unroll from)
	      | (Pack, [t,hiding])  => LD.T.coercion (Elim.C.instantiate t hiding) t
	      | (ForgetKnown,[from]) => LD.T.coercion from (LD.COps.ksum2sum from)
	      | (ProjKnown,[from]) => LD.T.coercion from (LD.COps.ksum2arm from)
	      | (InjUnion, [to])   => LD.T.coercion (LD.COps.ksum2arm to) to
	      | (InjForget, [ksum]) => LD.T.coercion (LD.COps.ksum2arm ksum) (LD.COps.ksum2sum ksum)
	      | _ => error "Wrong args")
	and primarg env arg = 
	  (case arg
	     of slice (sz,sv) => Elim.C.doslice (sv32 env sv)
	      | arg32 sv => sv32 env sv
	      | arg64 sv => sv64 env sv)
	and sv64 env sv = 
	  (case sv
	     of Var_64 xf => find_var64 (env,xf)
	      | Const_64 v => 
	       (case v 
		  of Prim.float (floatsize, f) => LD.T.float ()
		   | _ => error "Const_64 got bad prim value"))
	and sv32 env sv =
	  (case sv
	     of Var_32 x => find_var32(env,x) 
	      | Label l => find_label(env,l)
	      | Coercion q => coercion env q 
	      | Coerce (q,v) => Elim.C.coerce(sv32 env q)
	      | Tabs (ak,sv) => LD.T.forall ak (sv32 (LC.bind_cvar (env,ak)) sv)

	      (* It's much more elegant to not do it this way,
	       * but unfortunately, this seems to be a bottleneck.
	       *)
	      | TApp _ => 
	       let
		 val (sv,cons) = Dec.E.nary_tapp sv
		 val ftype = sv32 env sv 
		 fun loop (svtype,cons,subst) = 
		   (case cons
		      of [] => LS.substConInCon subst svtype
		       | c::cons => 
			let
			  val ((a,k),svtype) = Dec.C.forall_ml svtype
			  val subst = LS.C.sim_add subst (a,c)
			in loop (svtype,cons,subst)
			end)
	       in loop (ftype,cons,LS.C.empty())
	       end
	      | Tag w32    => LD.T.tag w32
	      | Unit => LD.T.unit()
	      | Const_32 v => 
	       (case v 
		  of (Prim.int (Prim.W8, w))  => LD.T.embed B1 (LD.T.intt B1)
		   | (Prim.int (Prim.W32, w)) => LD.T.intt B4
		   | (Prim.uint (Prim.W8, w)) => LD.T.embed B1 (LD.T.intt B1)
		   | (Prim.uint (Prim.W32, w)) => LD.T.intt B4
		   | _ => error "Const_32 got bad prim value"))
	and op64 env fop = 
	  (case fop
	     of Val_64 sv => sv64 env sv
	      | ExternAppf (f,_) => Elim.C.externapp (sv32 env f)
	      | Unbox sv => Elim.C.unbox (sv32 env sv)
	      | Prim64 (p,pargs) => #3 (PU.get_type () p []))
	and lilprimop32 env (pop32,cons,sv32s,sv64s) =
	  (case (pop32,cons,sv32s,sv64s)
	     of (Box, [],[],[sv])   => LD.T.ptr (LD.T.boxed B8 (sv64 env sv))
	      | (Tuple, [], svs,[]) => LD.T.ptr (LD.T.tuple' (map (sv32 env) svs))
	      | (Select w32,[],[sv],[]) => Elim.C.select w32 (sv32 env sv)
	      | (Dyntag,[c],[],[])      => LD.T.dyntag c
	      | (Ptreq,[],[sv1,sv2],[]) => LD.T.bool()
	      | _ => error "Wrong number of arms to lilprim")
	and op32 env eop = 
	  (case eop
	     of Val sv => sv32 env sv
	      | Prim32 (p,cs,primargs) => #3 (PU.get_type () p cs)
	      | PrimEmbed (sz,p,primargs) => LD.T.embed sz (#3 (PU.get_type () p []))
	      | LilPrimOp32 args => lilprimop32 env args
	      | ExternApp (f,args) => Elim.C.externapp (sv32 env f)
	      | App (f,vs,fvs) => Elim.C.app (sv32 env f)
	      | Call (f,vs,fvs) => Elim.C.call (sv32 env f)
	      | Switch sw => switch env sw
	      | Raise (c,sv) => c
	      | Handle {t,...} => t)

	and bnd env b = 
	  (case b 
	     of Fixcode_b vfs    => (bind_functions (env,vfs),LS.C.empty())
	      | Exp32_b b  => (bind_op32 (env,b),LS.C.empty())
	      | Exp64_b b  => (bind_op64 (env,b),LS.C.empty())
	      | Unpack_b b => (bind_unpack (env,b),LS.C.empty())
	      | Split_b b  => bind_split (env,b)
	      | Unfold_b b => bind_unfold (env,b)
	      | Inj_b b    => bind_inj (env,b))
	and function (Function {tFormals    : (var * kind) list,
				eFormals    : (var * con) list,
				fFormals    : (var * con) list,
				rtype       : con,
				body        : exp}) =
	  let
	    val t32s = LO.seconds eFormals
	    val t64s = LO.seconds fFormals 
	  in LD.T.allarrow' tFormals t32s t64s rtype
	  end
	and code (Function {tFormals    : (var * kind) list,
			    eFormals    : (var * con) list,
			    fFormals    : (var * con) list,
			    rtype       : con,
			    body        : exp}) =
	  let
	    val t32s = LO.seconds eFormals
	    val t64s = LO.seconds fFormals 
	  in LD.T.allcode' tFormals t32s t64s rtype
	  end
	and switch env sw = 
	  (case sw 
	     of Sumcase {arg : sv32,arms :(w32  * var * exp) list, default: exp option, rtype : con} => rtype
	      | Dyncase {arg : sv32,arms :(sv32 * (var * con) * exp) list, default: exp,        rtype : con} => rtype
	      | Intcase {arg : sv32,arms :(w32 * exp) list,size:size,      default: exp,        rtype : con} => rtype
	      | Ifthenelse {rtype,...} => rtype)
      
	and bind_functions (env,vfs) = LC.bind_var32s (env,LO.map_second function vfs)
	and bind_op32 (env,(x,eop))  = LC.bind_var32 (env,(x,op32 env eop))
	and bind_op64 (env,(xf,fop)) = LC.bind_var64 (env,(xf,op64 env fop))
	and bind_unpack (env,(a,x,sv)) = 
	  let
	    val t = sv32 env sv
	    val (k,t) = Elim.C.unpack (a,t)
	    val env = LC.bind_cvar(env,(a,k))
	    val env = LC.bind_var32(env,(x,t))
	  in env
	  end
	and bind_con (env,(a,c)) = LC.bind_cvar(env,(a,Kindof.con env c))
	and bind_split (env,(b,g,c)) =
	  (case cout (Reduce.whnf c)
	     of Var_c a => split_cvar (env,(a,(b,g)))
	      | Pair_c (c1,c2) => 
	       let
		 val subst = LS.C.empty()
		 val subst = LS.C.sim_add subst (b,c1)
		 val subst = LS.C.sim_add subst (g,c2)
	       in (env,subst)
	       end
	      (* Either an error, or else need a non-refining case *)
	      | _ => (print "Oops! con is:\n";
		      PpLil.pp_con c;print "\n";
		      error "Split on path not handled"))
	and bind_inj (env,(w,b,c,sv))= 
	  (case cout (Reduce.whnf c)
	     of Var_c a => 
	       let
		 val envs = vcase_cvar (env,(a,b))
	       in LU.wnth w envs
	       end
	      | Inj_c (w',k,c1) => 
	       if w = w' then 
		 let
		   val subst = LS.C.empty()
		   val subst = LS.C.sim_add subst (b,c1)
		 in (env,subst)
		 end
	       else (env,LS.C.empty())  
	      (* Either an error, or else need a non-refining case *)
	      | _ => (print "Oops! con is:\n";
		      PpLil.pp_con c;print "\n";
		      error "Inj on path not handled"))
	     
	and bind_unfold (env,(b,c)) =
	  (case cout (Reduce.whnf c)
	     of Var_c a => unfold_cvar (env,(a,b))
	      | Fold_c (k,c) => 
	       let
		 val subst = LS.C.empty()
		 val subst = LS.C.sim_add subst (b,c)
		in (env,subst)
		end
	      (* Either an error, or else need a non-refining case *)
	      | _ => (print "Oops! con is:\n";
		      PpLil.pp_con c;print "\n";
		      error "Unfold on path not handled"))

	fun sv32pexp (env : env,p : sv32 P.pexp) : LC.context * LS.con_subst * Lil.con = 
	  let
	    fun dobnd (b,(env,subst)) = 
	      let
		val b = LS.substConInBnd subst b
		val (env,subst') = bnd env b
		val subst = LS.C.compose (subst',subst)
	      in (env,subst)
	      end
	    fun dosv (sv,(env,subst)) = (env,subst,sv32 env (LS.substConInSv32 subst sv))
	  in P.Lili.fold dobnd dosv (env,LS.C.empty()) p
	  end

	fun pexp (env : env, p : 'a P.pexp) : LC.context * LS.con_subst = 
	  let
	    fun dobnd (b,(env,subst)) = 
	      let
		val b = LS.substConInBnd subst b
		val (env,subst') = bnd env b
		val subst = LS.C.compose (subst',subst)
	      in (env,subst)
	      end
	    fun doa (a,state) = state
	  in P.Lili.fold dobnd doa (env,LS.C.empty()) p
	  end

      end


    fun wrap f name printer env arg = 
      let
	val res = ((f env arg) 
		   handle any => (print "Synthesis error in ";print name;print "\n";
				  printer arg;print "\n";
				  raise any))
      in res
      end
    structure Kindof =
      struct
	val primcon = Kindof.primcon (* No exceptions *)
	val con = wrap Kindof.con "con" PpLil.pp_con 
      end
    structure Typeof =
      struct
	val coercion = Typeof.coercion
	val primarg  = Typeof.primarg
	val function = Typeof.function
	val code     = Typeof.code
	val sv64     = wrap Typeof.sv64 "sv64" PpLil.pp_sv64
	val sv32     = wrap Typeof.sv32 "sv32" PpLil.pp_sv32
	val op64     = wrap Typeof.op64 "op64" PpLil.pp_op64
	val op32     = wrap Typeof.op32 "op32" PpLil.pp_op32
	val bnd      = Typeof.bnd
	val switch   = Typeof.switch
	val bind_functions = Typeof.bind_functions
	val bind_op32      = Typeof.bind_op32
	val bind_op64      = Typeof.bind_op64
	val bind_unpack    = Typeof.bind_unpack
	val bind_split     = Typeof.bind_split
	val bind_inj       = Typeof.bind_inj
	val bind_unfold    = Typeof.bind_unfold
	val pexp           = Typeof.pexp
	val sv32pexp       = Typeof.sv32pexp
      end

  end