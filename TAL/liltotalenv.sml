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


    (* generate_functional_updates (NONE) (SOME "env") ["ctxt","talctxt","csubst","ms_return","tvars","label_vars","in_handler"]; *)
    type 'env env' = 
      {ctxt : LC.context, 
       talctxt : ('env -> Tal.con) VarMap.map,
       csubst : LS.con_subst,
       ms_return : Lil.con option,
       tvars : Lil.var list,
       label_vars : Tal.label VarMap.map,
       in_handler : bool
       }
    datatype env = ENV of env env'

    fun empty () = ENV{ctxt = LC.empty (),
		       talctxt = VarMap.empty,
		       csubst = LS.C.empty(),
		       ms_return = NONE,
		       tvars = [],
		       label_vars = VarMap.empty,
		       in_handler = false}
      
    fun set_ctxt (ENV{ ctxt = _  , talctxt , csubst , ms_return , tvars , label_vars , in_handler } :env) ctxt =
      ENV{
       ctxt = ctxt,
       talctxt = talctxt,
       csubst = csubst,
       ms_return = ms_return,
       tvars = tvars,
       label_vars = label_vars,
       in_handler = in_handler
       }
    fun set_talctxt (ENV{ ctxt , talctxt = _  , csubst , ms_return , tvars , label_vars , in_handler } :env) talctxt =
      ENV{
       ctxt = ctxt,
       talctxt = talctxt,
       csubst = csubst,
       ms_return = ms_return,
       tvars = tvars,
       label_vars = label_vars,
       in_handler = in_handler
       }
    fun set_csubst (ENV{ ctxt , talctxt , csubst = _  , ms_return , tvars , label_vars , in_handler } :env) csubst =
      ENV{
       ctxt = ctxt,
       talctxt = talctxt,
       csubst = csubst,
       ms_return = ms_return,
       tvars = tvars,
       label_vars = label_vars,
       in_handler = in_handler
       }
    fun set_ms_return (ENV{ ctxt , talctxt , csubst , ms_return = _  , tvars , label_vars , in_handler } :env) ms_return =
      ENV{
       ctxt = ctxt,
       talctxt = talctxt,
       csubst = csubst,
       ms_return = SOME ms_return,
       tvars = tvars,
       label_vars = label_vars,
       in_handler = in_handler
       }
    fun set_tvars (ENV{ ctxt , talctxt , csubst , ms_return , tvars = _  , label_vars , in_handler } :env) tvars =
      ENV{
       ctxt = ctxt,
       talctxt = talctxt,
       csubst = csubst,
       ms_return = ms_return,
       tvars = tvars,
       label_vars = label_vars,
       in_handler = in_handler
       }
    fun set_label_vars (ENV{ ctxt , talctxt , csubst , ms_return , tvars , label_vars = _  , in_handler } :env) label_vars =
      ENV{
       ctxt = ctxt,
       talctxt = talctxt,
       csubst = csubst,
       ms_return = ms_return,
       tvars = tvars,
       label_vars = label_vars,
       in_handler = in_handler
       }
    fun set_in_handler (ENV{ ctxt , talctxt , csubst , ms_return , tvars , label_vars , in_handler = _  } :env) in_handler =
      ENV{
       ctxt = ctxt,
       talctxt = talctxt,
       csubst = csubst,
       ms_return = ms_return,
       tvars = tvars,
       label_vars = label_vars,
       in_handler = in_handler
       }
    fun unenv f (ENV env) = f env
    fun vartrans (env : env) (var : Lil.var) = 
      (case VarMap.find (unenv #label_vars env,var)
	 of SOME l => l
	  | NONE => 
	   let
	     val s = Name.var2string var
	     val s = Core.makeAsmLabel s
	     val l = Name.internal_label s
	   in l
	   end)

    fun var2con (env : env) (var : Lil.var) = 
      (case VarMap.find (unenv #label_vars env,var)
	 of SOME l => Tal.clab l
	  | NONE => 
	   let
	     val s = Name.var2string var
	     val s = Core.makeAsmLabel s
	     val l = Name.internal_label s
	   in Tal.cvar l
	   end)

    fun label_var (env : env) (var : Lil.var) (label : Tal.label) = set_label_vars env (VarMap.insert(unenv #label_vars env,var,label))

    fun get_ms_return (env : env) = unenv #ms_return env
    fun get_tvars (env : env) = unenv #tvars env

    fun add_tvar (env : env) a = set_tvars env (a :: unenv #tvars env)
    fun delete_tvar (env : env) a = 
      let
	val tvs = unenv #tvars env
	val tvs = List.filter (fn x => not (Name.eq_var (x,a))) tvs
      in set_tvars env tvs
      end
    fun add_tvars (env : env) aa = set_tvars env (aa @ unenv #tvars env)

    fun subst_op32 (env : env) op32 = LS.substConInOp32 (unenv #csubst env) op32
    fun subst_sv32 (env : env) sv32 = LS.substConInSv32 (unenv #csubst env) sv32
    fun subst_bnd (env : env) bnd = LS.substConInBnd (unenv #csubst env) bnd
    fun subst_con (env : env) c = LS.substConInCon (unenv #csubst env) c

    fun bind_var32 (env : env,(x,c)) = set_ctxt env (LC.bind_var32(unenv #ctxt env,(x,subst_con env c)))
    fun bind_var64 (env : env,(x,c)) = set_ctxt env (LC.bind_var64(unenv #ctxt env,(x,subst_con env c)))
    fun bind_label (env : env,(l,c)) = set_ctxt env (LC.bind_label(unenv #ctxt env,(l,subst_con env c)))
    fun bind_talvar (env : env,(x,c)) = set_talctxt env (VarMap.insert(unenv #talctxt env,x,c))

    fun bind_vars32 (env : env,xcs) = foldl (fn (xc,env) => bind_var32 (env,xc)) env xcs
    fun bind_vars64 (env : env,xcs) = foldl (fn (xc,env) => bind_var64 (env,xc)) env xcs

    fun bind_cvar (env : env,ak) = set_ctxt env (LC.bind_cvar(unenv #ctxt env,ak))
    fun bind_cvars (env : env,aks) = set_ctxt env (LC.bind_cvars(unenv #ctxt env,aks))
    fun bind_kvar (env : env,j,p) = set_ctxt env (LC.bind_kvar(unenv #ctxt env,j,p))

    fun find_cvar  (env : env,x) = LC.find_cvar(unenv #ctxt env,x)
    fun find_var32 (env : env,x) = LC.find_var32(unenv #ctxt env,x)
    fun find_var64 (env : env,x) = LC.find_var64(unenv #ctxt env,x)

    fun find_talvar (env : env,x) : Tal.con option = 
      (case VarMap.find (unenv #talctxt env,x)
	 of SOME cf => SOME (cf env)
	  | NONE => NONE)
    fun find_talvar32 (ctrans : env -> Lil.con -> Tal.con) (env : env,x) : Tal.con = 
      (case VarMap.find (unenv #talctxt env,x)
	 of SOME cf => cf env
	  | NONE => ctrans env (find_var32 (env,x)))

    fun find_talvar64 (ctrans : env -> Lil.con -> Tal.con) (env : env,x) : Tal.con = 
      (case VarMap.find (unenv #talctxt env,x)
	 of SOME cf => cf env
	  | NONE => ctrans env (find_var64 (env,x)))

    fun kindof (env : env) c = Kindof.con (unenv #ctxt env) c

    fun typeof_op32 (env : env) op32 = (S.Typeof.op32 (unenv #ctxt env) (subst_op32 env op32))
	  handle any => (print "ERROR in typeof_op32\n";raise any)
    fun typeof_sv32 (env : env) sv32 = (S.Typeof.sv32 (unenv #ctxt env) (subst_sv32 env sv32))
	  handle any => (print "ERROR in typeof_sv32\n";raise any)
    fun typeof_code (env : env) f = (S.Typeof.code f)

    fun bind_bnd (env : env) (bnd : Lil.bnd) : env = 
      let
	val bnd = subst_bnd env bnd
	val (ctxt,subst) = S.Typeof.bnd (unenv #ctxt env) bnd
	val env = set_ctxt env ctxt
	val env = set_csubst env (LS.C.compose (subst,unenv #csubst env))
	val env = 
	  (case bnd
	     of Lil.Unpack_b (a,_,_) => add_tvar env a
	      | Lil.Split_b (a1,a2,c) => 
	       (case cout (Reduce.whnf c)
		  of Lil.Var_c a => delete_tvar (add_tvars env [a1,a2]) a
		   | Lil.Pair_c (c1,c2) => env
		   | _ => add_tvars env [a1,a2])
		  
	      | Lil.Unfold_b (a,c) => 
		(case cout (Reduce.whnf c)
		   of Lil.Var_c a' => delete_tvar (add_tvar env a) a'
		    | Lil.Fold_c (k,c) => env
		    | _ => add_tvar env a)
	      | Lil.Inj_b (_,a,c,_) => 
		(case cout (Reduce.whnf c)
		   of Lil.Var_c b => delete_tvar (add_tvar env b) a
		    | Lil.Inj_c (w',k,c1) => env
		    | _ => add_tvar env a)
	      | _ => env)

      in env
      end

    fun enter_handler (env : env) = set_in_handler env true
    fun in_handler (env : env) = unenv #in_handler env
end  (* LilToTalEnv *)