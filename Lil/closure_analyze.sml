structure LilClosureAnalyze :> LILCLOSURE_ANALYZE = 
  struct
    open Lil

    structure Frees = ClosureState.Frees
    structure State = ClosureState.State
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet
    structure LO = Listops
    structure LD = LilDefs
    structure LC = LilContext 
    structure LS = LilSubst
    structure Typeof = Synthesis.Typeof
    structure Kindof = Synthesis.Kindof
    structure Dec = Deconstruct.Dec

    type fid = ClosureState.fid

    val error = fn s => Util.error "lilclosure_analyze.sml" s
    val staticalloc = Stats.tt "LilClosureStaticAlloc"
    val debug = Stats.ff "LilClosureAnalyzeDebug"
    val do_con_globals = Stats.ff "LilClosureDoConGlobals"

    fun debugdo t = if (!debug) then (t(); ()) else ()

    val chatlev = ref 0       
    fun chatp i = (!chatlev) >= i
      
    fun chat i s = if chatp i then print s else ()

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    type funentry = {static : {code_lbl : Lil.label,     (* The new code name*)
			       venv_var : Lil.var        (* The new value env variable *)
			       },
		     escape : bool,               (*Escape in the sense that we will generate a closure
						   * for it.  This may happen even for "non-escaping"
						   * functions in the current strategy, since we 
						   * only generate direct calls for self-calls
						   *)
		     callee : (fid * State.state) list,  (*Functions which are called directly. *)
		     escapee : (fid * State.state) list, (*Functions for which we must create a local closure *)
		     frees : Frees.frees                 (* Free variables of the function *)
		     }
      
    (* We must assign the value environment a name now
     * so that if we do direct calls for known functions, we can
     * add the value environment of the callee to the free variable list
     * of the caller.  Currently this is not done, since there are numerous
     * complications having to do with not knowing the type of the value
     * environment until after we have computed the transitive closure.
     *)
    fun empty_fun new_fid : funentry = 
      let 
	val name = Name.var2name new_fid
      in   
	{static = {code_lbl = Name.fresh_internal_label(name ^ "_code"),
		   venv_var = Name.fresh_named_var(name ^ "_eEnv")
		   },
	 escape = false,
	 callee = [],
	 escapee = [],
	 frees = Frees.empty_frees}
      end
    

   structure Global = 
      struct 
	

	val empty_table = VarMap.empty : (funentry ref) VarMap.map
	val fids = ref empty_table
	val globals : label VarMap.map ref = ref VarMap.empty
	fun reset () = (fids := empty_table;
			globals := VarMap.empty)
	  
	fun is_fid f = (case (VarMap.find(!fids,f)) of NONE => false | _ => true)
	fun get_fids() = VarMap.foldli (fn (fid,_,acc) => VarSet.add(acc,fid)) VarSet.empty (!fids)
	fun add_fun new_fid = fids := (VarMap.insert(!fids,new_fid,ref (empty_fun new_fid)))
	  
	fun get_entry_ref caller = 
	  (case (VarMap.find(!fids,caller)) of
	     NONE => error ((Name.var2string caller) ^ "fid not found for get_entry")
	   | SOME (r as (ref entry)) => (r,entry))
	fun get_entry caller = #2 (get_entry_ref caller)

	fun get_callee caller = #callee (get_entry caller)
	fun get_escapee caller = #escapee (get_entry caller)
	fun get_frees  caller = #frees (get_entry caller)
	fun get_escape caller = #escape (get_entry caller)
	fun get_venv_var f = #venv_var (#static (get_entry f))

	fun add_escape esc_fid = 
	  let
	    val (r,{static,escape,callee,escapee,frees}) = get_entry_ref esc_fid
	  in
	    r := {static = static,
		  escape = true,
		  callee = callee,
		  escapee = escapee,
		  frees = frees}
	  end


	fun add_callee (caller,callee_fid,state) = 
	  let
	    val (r,{static,escape,callee,escapee,frees}) = get_entry_ref caller
	  in
	    r := {static = static,
		  escape = escape,
		  callee = (callee_fid,state)::callee,
		  escapee = escapee,
		  frees = frees}
	  end

	fun add_escapee (caller,escapee_fid,state) = 
	  let
	    val (r,{static,escape,callee,escapee,frees}) = get_entry_ref caller
	  in
	    r := {static = static,
		  escape = escape,
		  callee = callee,
		  escapee = (escapee_fid,state)::escapee,
		  frees = frees}
	  end

	fun add_frees (fid,f) =
	  let
	    val (r,{static,escape,callee,escapee,frees}) = get_entry_ref fid
	  in
	    r := {static = static,
		  escape = escape,
		  callee = callee,
		  escapee = escapee,
		  frees = Frees.join_frees(frees,f)}
	  end

	fun augment_frees (fid,f) =
	  let
	    val (r,{static,escape,callee,escapee,frees}) = get_entry_ref fid
	    val contained = Frees.contains_frees (frees,f) 
	    val _ = if contained then () 
		    else
		      r := {static = static,
			    escape = escape,
			    callee = callee,
			    escapee = escapee,
			    frees = Frees.join_frees(frees,f)}
	  in not contained
	  end
	
	fun add_global (v,l) = globals := VarMap.insert(!globals,v,l)
	fun add_globals vls = app add_global vls

	fun result () = (VarMap.map (fn e => !e) (!fids),!globals)
      end


    (********* BEGIN  EXPORTS **************************)

    structure FindFV 
      = struct

	val foldl_acc = Listops.foldl_acc
	val foldl_acc2 = Listops.foldl_acc2
	val map_second = Listops.map_second
	val mapopt = Util.mapopt
	val lprintl = Util.lprintl
	  
	val getOpt = Option.getOpt
	  
	fun flip (a,b) = (b,a)

	(* For historical reasons, the first part of the environment is called
	 * the state, but it does not behave like a state and should be renamed.
	 *)

	(* state : the closure state (see closure_state.sml)
	 * ctxt : the lil context.
	 *)
	datatype env = ENV of {state : State.state, ctxt : LC.context,csubst : LS.con_subst}

	(* This is treated like a state for the most part *)
	type frees = Frees.frees
	  
	fun var32_is_free ({vars32,...} :frees) v = VarSet.member (vars32,v)
	  
	fun initial_env topfid = 
	  let 
	    val _ = Global.add_fun topfid
	  in ENV {state = State.initial_state topfid, ctxt = LC.empty(),csubst = LS.C.empty()}
	  end

	fun get_state (ENV{state,...}) = state


	fun LCbind binder (ctxt : LC.context ,csubst,(var,con)) : LC.context = binder(ctxt,(var,LS.substConInCon csubst con))
	fun LCbinds binder (ctxt : LC.context,csubst,vcs) : LC.context = binder(ctxt,LO.map_second (fn c => LS.substConInCon csubst c) vcs)


	(* Careful!! These assume that the con substitution has already been carried
	 * out on the type being bound (see typeof_xxx below). 
	 *)

	fun exp_var32_bind (ENV{state,ctxt,csubst},(var,con)) = ENV{state = State.add_boundvar32 (state,var),
								    ctxt = LC.bind_var32 (ctxt,(var,con)),
								    csubst = csubst}
	fun exp_label_bind (ENV{state,ctxt,csubst},(label,con)) = ENV{state = state,
								      ctxt = LC.bind_label (ctxt,(label,con)),
								      csubst = csubst}
	fun exp_var64_bind (ENV{state,ctxt,csubst},(var,con)) = ENV{state = State.add_boundvar64 (state,var),
								    ctxt = LC.bind_var64 (ctxt,(var,con)),
								    csubst = csubst}
	fun con_var_bind (ENV{state,ctxt,csubst},(var,kind))  = ENV{state = State.add_boundcvar (state,var),
								    ctxt = LC.bind_cvar(ctxt,(var,kind)),
								    csubst = csubst}
	  
	fun con_gvar_bind (ENV{state,ctxt,csubst},(var,kind))  = ENV{state = State.add_globalcvar (state,var),
								     ctxt = LC.bind_cvar(ctxt,(var,kind)),
								     csubst = csubst}

	fun exp_var32_list_bind (ENV{state,ctxt,csubst},vcs) = 
	  let
	    val vs = map #1 vcs
	  in 
	    ENV{state = State.add_boundvars32 (state,vs),
		ctxt = LC.bind_var32s (ctxt,vcs),
		csubst = csubst}
	  end
	fun exp_var64_list_bind (ENV{state,ctxt,csubst},vcs) = 
	  let
	    val vs = map #1 vcs
	  in 
	    ENV{state = State.add_boundvars64 (state,vs),
		ctxt = LC.bind_var64s (ctxt,vcs),
		csubst = csubst}
	  end

	fun con_var_list_bind (ENV{state,ctxt,csubst},vks)    = ENV{state = State.add_boundcvars  (state,map #1 vks),
								    ctxt = LC.bind_cvars(ctxt,vks),
								    csubst = csubst}
	  

	fun typeof_sv32 (ENV{ctxt,csubst,...},sv32) = (Typeof.sv32 ctxt (LS.substConInSv32 csubst sv32))
	  handle any => (print "ERROR in typeof_sv32\n";raise any)
	fun typeof_op32 (ENV{ctxt,csubst,...},op32) = (Typeof.op32 ctxt (LS.substConInOp32 csubst op32))
	  handle any => (print "ERROR in typeof_op32\n";raise any)
	fun typeof_op64 (ENV{ctxt,csubst,...},op64) = (Typeof.op64 ctxt (LS.substConInOp64 csubst op64))

	fun kindof      (ENV{ctxt,csubst,...},c)    = (Kindof.con  ctxt (LS.substConInCon csubst c))
	  handle any => (print "ERROR in kindof\n";raise any)
	fun bind_unpack (ENV{state,ctxt,csubst},(a,x,sv)) = 
	  let
	    val state = State.add_boundcvar (state,a)
	    val state = State.add_boundvar32 (state,x)
	    val ctxt = Typeof.bind_unpack (ctxt, (a,x,sv))
	  in
	    ENV{state = state,
		ctxt = ctxt,
		csubst = csubst}
	  end

	fun bind_split (ENV{state,ctxt,csubst},(a1,a2,c)) = 
	  let
	    val state = State.add_boundcvar (state,a1)
	    val state = State.add_boundcvar (state,a2)
	    val (ctxt,new_csubst) = Typeof.bind_split (ctxt, (a1,a2,LS.substConInCon csubst c))
	    val csubst = LS.C.compose (new_csubst,csubst)
	  in
	    ENV{state = state,
		ctxt = ctxt,
		csubst = csubst}
	  end

	fun bind_unfold (ENV{state,ctxt,csubst},(a,c)) = 
	  let
	    val state = State.add_boundcvar (state,a)
	    val (ctxt,new_csubst) = Typeof.bind_unfold (ctxt,(a,LS.substConInCon csubst c))
	    val csubst = LS.C.compose (new_csubst,csubst)
	  in
	    ENV{state = state,
		ctxt = ctxt,
		csubst = csubst}
	  end
	
	fun bind_inj (ENV{state,ctxt,csubst},arg as (w,a,c,sv)) = 
	  let
	    val state = State.add_boundcvar (state,a)
	    val (ctxt,new_csubst) = Typeof.bind_inj (ctxt,(w,a,LS.substConInCon csubst c,LS.substConInSv32 csubst sv))
	    val csubst = LS.C.compose (new_csubst,csubst)
	  in
	    ENV{state = state,
		ctxt = ctxt,
		csubst = csubst}
	  end

	fun find_var32 (ENV{ctxt,...},v) = LC.find_var32(ctxt,v)
	fun find_var64 (ENV{ctxt,...},v) = LC.find_var64(ctxt,v)

	fun do_csubst_var (ENV{csubst,...},a) = LS.C.substitute csubst a
	fun do_csubst (ENV{csubst,...}) c = LS.substConInCon csubst c

	fun cvar_isavailable (ENV{state,...},frees,v) = State.cvar_isavailable(state,frees,v)
        fun var32_isavailable (ENV{state,...},frees,v) = State.var32_isavailable(state,frees,v)
        fun var64_isavailable (ENV{state,...},frees,v) = State.var64_isavailable(state,frees,v)
	fun is_boundfid (ENV{state,...},v) = State.is_boundfid (state,v)

        fun var32_islocal (ENV{state,...},v) = State.var32_islocal(state,v)

	fun promote_state (ENV{state,ctxt,csubst},vts) = 
	  ENV{state = State.promote_state(state,map #1 vts),
	      ctxt = LCbinds LC.bind_var32s(ctxt,csubst,vts),
	      csubst = csubst}

	fun restrict_frees_with_state (ENV{state,...},frees) = State.restrict_frees_with_state(state,frees)

	fun add_boundfids (ENV{state,ctxt,csubst},vts) = 
	  ENV{state = State.add_boundfids(state,map #1 vts),
	      ctxt = LCbinds LC.bind_var32s(ctxt,csubst,vts),
	      csubst = csubst}

	fun add_gboundfids (ENV{state,ctxt,csubst},vts) = 
	  ENV{state = State.add_gboundfids(state,map #1 vts),
	      ctxt = LCbinds LC.bind_var32s(ctxt,csubst,vts),
	      csubst = csubst}

	fun fid_in_nest (ENV{state,...},v) = State.fid_in_nest state v
	fun get_curfid (ENV{state,...}) = State.get_curfid state
	fun set_curfid (ENV{state,ctxt,csubst}) args = ENV {state = State.set_curfid state args,ctxt = ctxt,csubst = csubst}


	fun findfv_list (findfv : env -> frees -> 'item -> Frees.frees) (env : env) (frees : frees) (items : 'item list) : Frees.frees = 
	  let
	    fun folder (f,frees) = findfv env frees f
	  in foldl folder frees items
	  end
	
	fun findfv_opt findfv env frees opt = 
	  (case opt
	     of SOME i => findfv env frees i
	      | NONE => frees)

	fun findfv_con (env : env) (frees : frees) (con : con) : Frees.frees =  
	  let
	    fun add_cvar (a,frees) = 
	      if (cvar_isavailable(env,frees,a))
		then frees
	      else Frees.free_cvar_add(frees,a)
		
	    fun dovar (a,frees) = 
	      (case do_csubst_var(env,a)
		 of SOME c => VarSet.foldl add_cvar frees (free_cvars_con c)
		  | NONE => add_cvar(a,frees))
	  in VarSet.foldl dovar frees (free_cvars_con con)
	  end
(*
	  let 
	    fun docon (env,frees,con : con) = 
	      let 
		val recur = findfv_con env frees
		fun recur2 c1 c2 = findfv_con env (findfv_con env frees c1) c2
	      in
		(case cout con of
		   Var_c v =>
		     (case do_csubst_var (env,v)
			of SOME c => findfv_con env frees c
			 | NONE => 
			  if (cvar_isavailable(env,frees,v))
			    then frees
			  else Frees.free_cvar_add(frees,v))
		 | Nat_c w => frees
		 | App_c (c1,c2) => recur2 c1 c2
		 | APP_c (c,k) => recur c
		 | Pi1_c c => recur c
		 | Pi2_c c => recur c
		 | Prim_c p => frees
		 | Pr_c (j,(a,k),k',r,body)  => 
		   let
		     val env = con_var_bind (env,(a,k))
		     val env = con_var_bind (env,(r,LD.K.arrow (LD.K.var j) k'))
		     val frees = findfv_con env frees body
		   in frees
		   end
		 | Case_c (c,arms,def) => 
		   let
		     val frees = recur c
		     val sumk = kindof (env,c)
		     fun sumw w = valOf (LD.KOps.sumw w sumk)
		     fun findfv_arm env frees (w,(a,c)) = 
		       let
			 val env = con_var_bind (env,(a,sumw w))
		       in findfv_con env frees c
		       end
		     val frees = findfv_list findfv_arm env frees arms
		     val frees = findfv_opt findfv_con env frees def
		   in frees
		   end
		 | LAM_c (j,c) => findfv_con env frees c
		 | Lam_c ((a,k),c) =>
		     let 
		       val env = con_var_bind (env,(a,k))
		     in findfv_con env frees c
		     end
		 | Pair_c (c1,c2) => recur2 c1 c2
		 | Star_c => frees
		 | Inj_c (w,k,c) => recur c
		 | Fold_c (k,c) => recur c
		 | Ptr_c c => recur c)
		   
	      end
	  in  docon (env,frees,con)
	  end
	*)
	
	and findfv_exp (env : env) (frees : frees) (exp : exp) : frees =
	  let 
	    fun doexp (env,frees,e : exp) = 
	      let
	      in
		(case #e e of
		   Val32_e sv32 => findfv_sv32 env frees sv32
		 | Let_e (bnds,e) => 
		     let
		       val (env,frees) = findfv_bnds env frees bnds
		     in findfv_exp env frees e
		     end)
	      end
	    
	  in doexp (env,frees,exp)
	  end
	and findfv_sv32 (env : env) (frees : frees) (sv32 : sv32) : frees = 
	  let 
	    fun dosv32 (env,frees,sv) = 
	      let
		val recur_sv32 = findfv_sv32 env 
		val recur_sv64 = findfv_sv64 env 
		val recur_c = findfv_con env 
		val recur_e = findfv_exp env 
		fun recur_list findfv items frees = foldl (fn (item,frees) => findfv env frees item) frees items
	      in
		case sv32 
		  of Var_32 v => 
		    let
		      val _ = 
			if (is_boundfid(env,v)) then 
			  (Global.add_escape v;
			   if (var32_islocal(env,v)) then 
			     Global.add_escapee(#1(get_curfid env),v, get_state env)
			   else ())
			else ()
		    in
		      if (var32_isavailable(env,frees,v))
			then frees
		      else 
			let 
			  val frees = findfv_con env frees (find_var32(env,v))
			in Frees.free_var32_add(frees,v)
			end
		    end
		   | Label l => frees
		   | Coercion (c,args) => recur_list findfv_con args frees
		   | Coerce (q,sv)     => recur_list findfv_sv32 [q,sv] frees
		   | Tabs ((a,k),sv) => 
		    let 
		      val env = con_var_bind (env,(a,k))
		    in findfv_sv32 env frees sv
		    end
		   | TApp (sv,c) => 
		    let 
		      val frees = recur_sv32 frees sv
		      val frees = recur_c frees c
		    in frees
		    end
		   | Tag w => frees
		   | Unit => frees
		   | Const_32 v => 
		    (case v of
		       (Prim.int _) => frees
		     | (Prim.uint _) => frees
		     | (Prim.float _) => frees

		     | (Prim.vector (c,array)) =>
			 let
			   fun folder(sv,f) = findfv_primarg env f sv
			   val frees = Array.foldl folder frees array
			   val frees = recur_c frees c
			 in frees
			 end
		     | Prim.tag (t,c) => recur_c frees c
		     | _ => error "array constants shouldn't happen")

	      end
	    
	  in dosv32 (env,frees,sv32)
	  end
	and findfv_vcxxlist (binder : env * (var * con) -> env) (env : env) (frees : frees) (vcs : (var * con) list) : (env * frees) = 
	  let
	    fun folder ((v,c),(env,frees)) = 
	      let
		val frees = findfv_con env frees c
		val env = binder (env,(v,do_csubst env c))
	      in (env,frees)
	      end
	  in List.foldl folder (env,frees) vcs
	  end
	and findvc32list env vcs = findfv_vcxxlist exp_var32_bind env vcs
	and findvc64list env vcs = findfv_vcxxlist exp_var64_bind env vcs
	and findfv_bnds (env : env) (frees : frees) (bnds : bnd list) : (env * frees) = 
	  let
	    fun folder (bnd,(env,frees)) = findfv_bnd env frees bnd
	    val res = foldl folder (env,frees) bnds
	  in res
	  end
	and findfv_bnd (env : env) (frees : frees) (bnd : bnd) : (env * frees) = 
	  let


	    fun do_function env frees
	      (Function {tFormals    : (var * kind) list,
			 eFormals    : (var * con) list,
			 fFormals    : (var * con) list,
			 rtype       : con,
			 body        : exp}) = 
	      let 
		val env = con_var_list_bind  (env, tFormals)
		val (env,frees) = findvc32list env frees eFormals
		val (env,frees) = findvc64list env frees fFormals
		val frees = findfv_con env frees rtype
		val frees = findfv_exp env frees body
	      in  frees
	      end

	    fun do_function_nest env vfs = 
	      let
		val fids= map #1 vfs
		  
		val _ = app Global.add_fun fids
		  
		val vts = LO.map_second Typeof.function vfs
		  
		val internal_env = promote_state (env,vts)
		  
		fun folder ((v,f),frees) = 
		  let
		    val local_env = set_curfid internal_env (v,fids)
		    val local_frees = Frees.empty_frees
		    val f_frees = do_function local_env local_frees f
		    val _ = Global.add_frees(v,f_frees)      
		    val frees = Frees.join_frees (frees,f_frees)
		  in frees 
		  end
		
		val frees = foldl folder frees vfs
		val () = debugdo (fn () => (print "Functions: ";PpLil.pp_list PpLil.pp_var' fids ("",",","",false); print "\n";
					    Frees.print_frees frees))
		val env = 
		  (case (!staticalloc,Frees.is_empty frees)
		     of (true,true) => (Global.add_globals (map (fn f => (f,Name.var2label f)) fids);
					add_gboundfids(env,vts))
		      | _ => add_boundfids (env, vts))
		     
		(* This is a bid ad-hoc.  We record all variables from the current
		 * nest that occur free in the enclosed function as escapees of the 
		 * enclosing nest, so that the close_funs computation can add
		 * their free variables.
		 *)
		val _ = map (fn f => if var32_is_free frees f then Global.add_escapee(#1(get_curfid env),f,get_state env) else ()) (#2(get_curfid env))
		val frees = restrict_frees_with_state(env,frees)
	      in (env,frees)
	      end
	    
	    fun do_bnd (env,frees,bnd) = 
	      (case bnd
		 of Fixcode_b (vfs) => do_function_nest env vfs
		  | Exp32_b (v,op32) => 
		   let
		     val frees = findfv_op32 env frees op32 
		     val t = typeof_op32 (env,op32)
		     val env = exp_var32_bind (env,(v,t))
		   in (env,frees)
		   end
		  | Exp64_b (v,op64) => 
		   let
		     val frees = findfv_op64 env frees op64 
		     val t = typeof_op64 (env,op64)
		   in (exp_var64_bind (env,(v,t)),frees)
		   end
		  | Unpack_b (a,x,sv32) => 
		   let
		     val frees = findfv_sv32 env frees sv32 
		     val env = bind_unpack (env,(a,x,sv32))
		   in (env,frees)
		   end
		  | Split_b (a1,a2,c) => 
		   let
		     val frees = findfv_con env frees c 
		     val env = bind_split (env,(a1,a2,c))
		   in (env,frees)
		   end
		  | Unfold_b (a,c) => 
		   let
		     val frees = findfv_con env frees c 
		     val env = bind_unfold (env,(a,c))
		   in (env,frees)
		   end
		  | Inj_b (w,b,c,sv) => 
		   let
		     val frees = findfv_con env frees c 
		     val frees = findfv_sv32 env frees sv
		     val env = bind_inj (env,(w,b,c,sv))
		   in (env,frees)
		   end)
		 
	  in	    
	    do_bnd (env,frees,bnd)
	  end

	and findfv_op32 (env : env) (frees : frees) (op32 : op32) : Frees.frees = 
	  let
	    
	    val recur_sv32 = findfv_sv32 env
	    val recur_sv64 = findfv_sv64 env
	    val recur_primarg = findfv_primarg env
	    val recur_c = findfv_con env
	    val recur_e = findfv_exp env
	      
	    fun do_op32 (env,frees,op32) = 
	      (case op32
		 of Val sv32 => recur_sv32 frees sv32 
		  | Prim32 (p,cs,primargs) => 
		   let
		     val frees = findfv_list findfv_con env frees cs
		     val frees = findfv_list findfv_primarg env frees primargs
		   in frees
		   end
		  | LilPrimOp32 (lp,cs,sv32s,sv64s) => 
		   let
		     val frees = findfv_list findfv_con env frees cs
		     val frees = findfv_list findfv_sv32 env frees sv32s
		     val frees = findfv_list findfv_sv64 env frees sv64s
		   in frees
		   end
		  | ExternApp (sv,sv32s,sv64s) =>  
		   let
		     val frees = findfv_sv32 env frees sv
		     val frees = findfv_list findfv_sv32 env frees sv32s
		     val frees = findfv_list findfv_sv64 env frees sv64s
		   in frees
		   end
		  | Call (f,sv32s,sv64s) => 
		   let
		     val frees = findfv_sv32 env frees f
		     val frees = findfv_list findfv_sv32 env frees sv32s
		     val frees = findfv_list findfv_sv64 env frees sv64s
		   in frees
		   end
		  | App (f,sv32s,sv64s) => 
		   let 
		     val frees = 
		       (case Dec.E.nary_tapp f of
			  (Var_32 v,cons) => 
			    if Global.is_fid v andalso (fid_in_nest (env,v) orelse var32_islocal (env,v)) then
			      (* Do direct calls for all mutually recursive functions and for all
			       * locally/globally available known functions.
			       * If we had recursive closures, it might be better to only
			       * do this for self calls, since this increases the closure size.
			       * We must record the callees for the transitive closure computation.
			       * Note that for global/local functions, the variables will already
			       * have to be available anyway.
			       *)
			      let
				val frees = findfv_list findfv_con env frees cons
				val _ = if chatp 3 then
				  (
				   print "***** adding callee ";
				   PpLil.pp_var v; print " to ";
				   PpLil.pp_var (#1(get_curfid env)); print "\n"
				   )  else ()
			      in
				Global.add_callee(#1(get_curfid env),v, get_state env);
				frees
			      end
			    (* Not a known function *)
			    else (findfv_sv32 env frees f)
			| _ => (print "Oops!! App is:\n";
				PpLil.pp_sv32 f;print "\n";
				error "Unexpected application"))
		     val frees = findfv_list findfv_sv32 env frees sv32s
		     val frees = findfv_list findfv_sv64 env frees sv64s
		   in frees
		   end
		  | Switch switch => findfv_switch env frees switch
		  | Raise (c,sv32) => 
		   let
		     val frees = findfv_con env frees c
		     val frees = findfv_sv32 env frees sv32
		   in frees
		   end

		  | Handle (t,e1,(v,e2)) => 
		   let
		     val frees = findfv_con env frees t
		     val frees = findfv_exp env frees e1
		     (*exn type is closed, don't need to subst *)
		     val env = exp_var32_bind (env,(v,LD.T.exn()))
		     val frees = findfv_exp env frees e2
		   in frees
		   end)
	  in do_op32 (env,frees,op32)
	  end
	and findfv_sv64 (env : env) (frees : frees) (sv64 : sv64) : frees = 
	  let
	    fun dosv64 (env,frees,sv64) = 
	      (case sv64
		 of Var_64 x => 
		   if (var64_isavailable(env,frees,x))
		     then frees
		   else 
		     let 
		       val frees = findfv_con env frees (find_var64(env,x))
		     in Frees.free_var64_add(frees,x)
		     end
		  | Const_64 _ => frees)
	  in dosv64 (env,frees,sv64)
	  end
	and findfv_op64 (env : env) (frees : frees) (op64 : op64) : Frees.frees = 
	  let
	    val recur_sv32 = findfv_sv32 env
	    val recur_sv64 = findfv_sv64 env
	    val recur_primarg = findfv_primarg env
	    val recur_c = findfv_con env
	  in
	    case op64
	      of Val_64 sv64 => recur_sv64 frees sv64 
	       | Unbox sv32 => recur_sv32 frees sv32 
	       | ExternAppf (sv,sv32s,sv64s) =>  
		let
		  val frees = findfv_sv32 env frees sv
		  val frees = findfv_list findfv_sv32 env frees sv32s
		  val frees = findfv_list findfv_sv64 env frees sv64s
		in frees
		end
	       | Prim64 (p,primargs) =>
		let
		  val frees = findfv_list findfv_primarg env frees primargs
		in frees
		end
	  end
	and findfv_primarg (env : env) (frees : frees) (primarg : primarg) : Frees.frees = 
	  (case primarg 
	     of arg32 sv32 => findfv_sv32 env frees sv32
	      | arg64 sv64 => findfv_sv64 env frees sv64)
	     
	and findfv_switch (env : env) (frees : frees) (sw : switch) : Frees.frees = 
	  let
	    val recur_sv32 = findfv_sv32 env 
	    val recur_exp  = findfv_exp env
	    val recur_con  = findfv_con env
	      
	    fun do_switch (env,frees,sw) = 
	      (case sw
		 of Sumcase {arg : sv32,arms :(w32 * var * exp) list, default: exp option, rtype : con} => 
		   let
		     val frees = recur_sv32 frees arg
		     val frees = findfv_opt findfv_exp env frees default
		     val frees = findfv_con env frees rtype
		     val t = typeof_sv32 (env,arg)
		     val tw = fn iw => ((LD.COps.sum2ksum' iw t) handle any => (print "Oops!\n";
										PpLil.pp_con t;raise any))
		     fun findfv_arm env frees (w,v,e) = 
		       let 
			 val env = exp_var32_bind (env,(v,tw w))
		       in findfv_exp env frees e
		       end
		     val frees = findfv_list findfv_arm env frees arms
		   in frees
		   end
		  | Dyncase {arg : sv32,arms :(sv32 * (var * con) * exp) list, default: exp,        rtype : con} =>
		   let
		     val frees = findfv_sv32 env frees arg
		     val frees = findfv_exp env frees default
		     val frees = findfv_con env frees rtype
		     fun findfv_arm env frees (sv,(v,c),e) = 
		       let 
			 val frees = findfv_con env frees c
			 val frees = findfv_sv32 env frees sv
			 val env = exp_var32_bind (env,(v,do_csubst env c))
		       in findfv_exp env frees e
		       end 
		     val frees = findfv_list findfv_arm env frees arms
		   in frees
		   end
		  | Intcase {arg : sv32,arms :(w32 * exp) list, default: exp,rtype : con} =>
		   let
		     val frees = findfv_sv32 env frees arg
		     val frees = findfv_exp env frees default
		     val frees = findfv_con env frees rtype
		     fun findfv_arm env frees (w,e) = findfv_exp env frees e
		     val frees = findfv_list findfv_arm env frees arms
		   in frees
		   end
		  | Ifthenelse {arg, thenArm, elseArm, rtype} =>
		   let
		     val frees = findfv_cc env frees arg
		     val frees = findfv_exp env frees thenArm
		     val frees = findfv_exp env frees elseArm
		     val frees = findfv_con env frees rtype
		   in frees
		   end)
		 
	  in do_switch (env,frees,sw)
	  end
	and findfv_cc env frees cc = 
	  (case cc
	     of Exp_cc e => findfv_exp env frees e
	      | And_cc(cc1,cc2) => findfv_cc env (findfv_cc env frees cc1) cc2
	      | Or_cc (cc1,cc2) => findfv_cc env (findfv_cc env frees cc1) cc2
	      | Not_cc cc => findfv_cc env frees cc)


	fun findfv_code env 
	  (Function {tFormals    : (var * kind) list,
		     eFormals    : (var * con) list,
		     fFormals    : (var * con) list,
		     rtype       : con,
		     body        : exp}) = 
	  let 
	    val frees = Frees.empty_frees
	    val env = con_var_list_bind  (env, tFormals)
	    val (env,frees) = findvc32list env frees eFormals
	    val (env,frees) = findvc64list env frees fFormals
	    val frees = findfv_con env frees rtype
	    val frees = findfv_exp env frees body
	  in ()
	  end


	fun findfv_datum env d = 
	  (case d
	     of Dboxed (l,sv64) => 
	       let
		 val frees = findfv_sv64 env Frees.empty_frees sv64
	       in ()
	       end
	      | Dtuple (l,t,q,svs) => 
	       let
		 val _ = findfv_con env 
		 val _ = Util.mapopt (findfv_sv32 env Frees.empty_frees) q
		 val () = app (fn sv => ignore (findfv_sv32 env Frees.empty_frees sv)) svs
	       in ()
	       end
	      | Darray (l,sz,t,svs) => 
	       let 
		 val _ = findfv_con env Frees.empty_frees t
		 val () = app (fn sv => ignore(findfv_sv32 env Frees.empty_frees sv)) svs
	       in ()
	       end    
	      | Dcode (l,f) => 
	       let
		 val () = findfv_code env f
	       in ()
	       end)

	fun findfv_data env data = 
	  let
	    fun add_dtype (d,env) = 
	      (case d
		 of Dboxed (l,sv64) => exp_label_bind(env,(l,LD.T.ptr (LD.T.boxed_float())))
		  | Dtuple (l,t,qs,svs) => exp_label_bind(env,(l,t))
		  | Darray (l,sz,c,svs) => exp_label_bind (env,(l,LD.T.ptr (LD.T.array sz c)))
		  | Dcode (l,f) => exp_label_bind(env,(l,Typeof.code f)))
	    val env = foldl add_dtype env data
	    val data = app (findfv_datum env) data
	  in env
	  end


        (* ------- compute the final free-variable list by closing over the callgraph ------ *)

        (* close_fun : fid * (fid VarSet.set) -> fid VarSet.set *)
        (* For each function fid directly called by function curfid, statefully adds the free variables *)
        (* of fid to those of curfid.  If this actually changes the free variables of curfid, returns   *)
        (* (nextset union {curfid}); else, returns nextset unchanged.         joev                      *)
        local
	  fun close_fun (curfid,nextset) =
	    let
	      val callees = Global.get_callee curfid
	      val callee_fvs = foldl (fn ((fid,s),fv) => let val f = Global.get_frees fid
							     val f = State.restrict_frees_with_state(s,f)
							 in  Frees.join_frees(f, fv)
							 end) Frees.empty_frees callees
	      val escapees = Global.get_escapee curfid
	      val escapee_fvs = foldl (fn ((fid,s),fv) => let 
							    val f = Global.get_frees fid
							    val f = State.restrict_frees_with_state(s,f)
							  in  Frees.join_frees(f, fv)
							  end) callee_fvs escapees
	      val changed = Global.augment_frees(curfid,escapee_fvs)
	    in if changed
		 then VarSet.add(nextset,curfid)
	       else nextset
	    end
	in  
	  fun close_funs workset =
	    let  
	      fun loop() =
		let
		  (* Propagates free variables one step from callees to callers, returning a set of *)
		  (* all the fids whose free variables have changed.  (When this set is empty, we have *)
                  (* finished the transitive closure.)               joev, 8/2002                      *)
		  val nextset = (VarSet.foldl close_fun VarSet.empty workset)
		in  
		  if (VarSet.isEmpty nextset)
		    then ()
		  else loop()  (* note that we must start with the whole set again *)
		end
	      val _ = loop()
		
	    in  ()
	    end
	  
	end
	

	(* Scan module for free variables *)
	(* Shouldn't be any code in the data segment yet.
	 *)
	fun findfv_module top_fid (MODULE{timports,data,confun,expfun}) = 
	  let

	    val _ = chat 1 "  Scanning for free variables\n"
	    val _ = Global.reset ()

	    val env = initial_env top_fid

	    val env = 
	      if !do_con_globals then 
		foldl (fn (vk,env) => con_gvar_bind(env,vk)) env timports
	      else
		con_var_list_bind(env,timports)

	    val env = findfv_data env data

	    val _ = findfv_exp env Frees.empty_frees expfun
	      
	    val _ = chat 1 "  Computing transitive closure of close funs\n"
	    val _ = close_funs(Global.get_fids())
	    val _ = chat 1 "  Finished closure analysis\n"
	      
	    val res = Global.result()
	    val _ = Global.reset ()
	      
	  in res
	  end
	
	end (* FindFV *)

    val findfv_module = FindFV.findfv_module
  end (* LilClosureAnalyze *)