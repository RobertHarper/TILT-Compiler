structure LilToTalEnv :> LILTOTALENV =
  struct
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet
    structure LC = LilContext
    structure LS = LilSubst
    structure LU = LilUtil
    structure S = Synthesis
    structure Kindof = S.Kindof

    fun error s = Util.error "liltotalenv.sml" s    

    fun warn s = (print "WARNING: ";print s;print "\n")

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    (* generate_functional_updates (NONE) (SOME "env") ["ctxt","csubst","ms_return","tvars"]; *)
    type env = 
      {ctxt : LC.context, 
       csubst : LS.con_subst,
       ms_return : Tal.con option,
       tvars : Lil.var list
       }

    fun empty () = {ctxt = LC.empty (),
		    csubst = LS.C.empty(),
		    ms_return = NONE,
		    tvars = []}

    fun set_ctxt ({ ctxt = _  , csubst , ms_return , tvars } :env) ctxt =
      {
       ctxt = ctxt,
       csubst = csubst,
       ms_return = ms_return,
       tvars = tvars
       }
    fun set_csubst ({ ctxt , csubst = _  , ms_return , tvars } :env) csubst =
      {
       ctxt = ctxt,
       csubst = csubst,
       ms_return = ms_return,
       tvars = tvars
       }
    fun set_ms_return ({ ctxt , csubst , ms_return = _  , tvars } :env) ms_return =
      {
       ctxt = ctxt,
       csubst = csubst,
       ms_return = SOME ms_return,
       tvars = tvars
       }
    fun set_tvars ({ ctxt , csubst , ms_return , tvars = _  } :env) tvars =
      {
       ctxt = ctxt,
       csubst = csubst,
       ms_return = ms_return,
       tvars = tvars
       }

    fun get_ms_return (env : env) = #ms_return env
    fun get_tvars (env : env) = #tvars env

    fun add_tvar (env : env) a = set_tvars env (a :: #tvars env)
    fun add_tvars (env : env) aa = set_tvars env (aa @ #tvars env)

    fun subst_op32 (env : env) op32 = LS.substConInOp32 (#csubst env) op32
    fun subst_sv32 (env : env) sv32 = LS.substConInSv32 (#csubst env) sv32
    fun subst_bnd (env : env) bnd = LS.substConInBnd (#csubst env) bnd
    fun subst_con (env : env) c = LS.substConInCon (#csubst env) c

    fun bind_var32 (env : env,(x,c)) = set_ctxt env (LC.bind_var32(#ctxt env,(x,subst_con env c)))
    fun bind_var64 (env : env,(x,c)) = set_ctxt env (LC.bind_var64(#ctxt env,(x,subst_con env c)))
    fun bind_label (env : env,(l,c)) = set_ctxt env (LC.bind_label(#ctxt env,(l,subst_con env c)))

    fun bind_vars32 (env : env,xcs) = foldl (fn (xc,env) => bind_var32 (env,xc)) env xcs
    fun bind_vars64 (env : env,xcs) = foldl (fn (xc,env) => bind_var64 (env,xc)) env xcs

    fun bind_cvar (env : env,ak) = set_ctxt env (LC.bind_cvar(#ctxt env,ak))
    fun bind_cvars (env : env,aks) = set_ctxt env (LC.bind_cvars(#ctxt env,aks))
    fun bind_kvar (env : env,j,p) = set_ctxt env (LC.bind_kvar(#ctxt env,j,p))

    fun find_var32 (env : env,x) = LC.find_var32(#ctxt env,x)
    fun find_var64 (env : env,x) = LC.find_var64(#ctxt env,x)

    fun kindof (env : env) c = Kindof.con (#ctxt env) c

    fun typeof_op32 (env : env) op32 = (S.Typeof.op32 (#ctxt env) (subst_op32 env op32))
	  handle any => (print "ERROR in typeof_op32\n";raise any)
    fun typeof_sv32 (env : env) sv32 = (S.Typeof.sv32 (#ctxt env) (subst_sv32 env sv32))
	  handle any => (print "ERROR in typeof_sv32\n";raise any)
    fun typeof_code (env : env) f = (S.Typeof.code f)

    fun bind_bnd (env : env) (bnd : Lil.bnd) : env = 
      let
	val bnd = subst_bnd env bnd
	val (ctxt,subst) = S.Typeof.bnd (#ctxt env) bnd
	val env = set_ctxt env ctxt
	val env = set_csubst env (LS.C.compose (subst,#csubst env))
	val env = 
	  (case bnd
	     of Lil.Unpack_b (a,_,_) => add_tvar env a
	      | Lil.Split_b (a1,a2,_) => add_tvars env [a1,a2]
	      | Lil.Unfold_b (a,_) => add_tvar env a
	      | Lil.Inj_b (_,a,_,_) => add_tvar env a
	      | _ => env)

      in env
      end




end  (* LilToTalEnv *)