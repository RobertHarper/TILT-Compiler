(* This is a very simple closure converter, with enough hooks to support
 * a better job someday.  See comments and code in the NIL closure
 * converter as to how to improve both closure converters
 *)


(* A couple of issues:

 Issue: Shadowing

 Suppose FV(E) = {y}.  It would be nice if we could just write the code of
      fun f x = E
 as 
      fun fcode (x,env) = let y = #y(env) in E end
 Unfortunately, since we do not hoist code to the top level, this creates shadowing
 (as there obviously must already have been a binding for y).  To avoid this, the pass that
 scans for free variables also generates a fresh derived name for each one it sees.  
 Thus in the previous example, our table of function identifiers ends up looking like
   [ (f,[(y,y')]) ]  (where y' is a fresh name)
 and we write the code as
   fun fcode (x,env) = let y' = #y(env) in E[y'/y] end
   
 Note that in general, because functions can be nested, each variable needs a separate 
 derived name for every function in which it occurs free in order to preserve the
 no-shadowing invariant.

 Issue: Direct Calls

 It is sometimes not strictly necessary to create and use closures for a function.  This
 closure converter attempts to include some facilities for direct function calls, but
 does not use them very much.  The current strategy is to generate direct calls for self calls
 and indirect calls for all others.  So
 fun f x = g x
 and g x = g x
 translates to
 reccode f' (x,e) = e.g x
 andcode g' (x,e) = g' x
 recclos f = {f',{g}}
 andclos g = {g',{}}

 
 It is possible to be more aggressive about this, but care must be
 taken not to blow up code and closure size.

 To handle direct calls, the function application cases of both the free-variable pass
 and the rewriting pass recognize the special case where the function being called
 happens to be the current function.  Adding support for other direct calls would
 probably be a simple matter of adding more such special cases, although the rewriting 
 pass would have to do some extra work to assemble environments for the callee.

*)


(* Closure conversion is accomplished in two phases.  
   The first phase scans the program for free type and term variables of 
     all type and term functions and also notes whether a function escapes or not.
     All this information is statefully accumulated in a table; the table is
     indexed by function names, so function variables must be globally unique.

   The second phase then rewrites the functions into codes and closures.  
*)


(*
 * Some LIL specific issues:
 * 1) Instead of making an explicit tuple of the free con vars, it is simpler
 *  and better to just parameterize over them explicitly.  Otherwise, you must
 *  use letsplit to turn the projections into variables again or else the code
 *  may not be well-typed.  
 *)

structure LilClosure :> LILCLOSURE = 
  struct
    open Lil


    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet
    structure LC = LilContext
    structure LS = LilSubst
    structure CA = LilClosureAnalyze
    structure LD = LilDefs
    structure LU = LilUtil
    structure LO = Listops
    structure S = Synthesis
    structure Dec = Deconstruct.Dec
    structure Elim = Deconstruct.Elim
    structure Frees = ClosureState.Frees
    structure State = ClosureState.State
    structure LR = LilRename

    val error = fn s => Util.error "lilclosure.sml" s
    val do_single_venv  = Stats.tt "do_single_venv"
    val debug = Stats.ff "LilClosureDebug"

    val staticalloc = CA.staticalloc

    val debuglev = ref 0
    val chatlev = ref 0 

    fun chatp i = !(chatlev) >= i
    fun chat i s = if chatp i then print s else ()
    fun debugdo (i,t) = if (!debug) andalso (i <= !debuglev) then (t(); ()) else ()

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    val foldl_acc = Listops.foldl_acc
    val foldl_acc2 = Listops.foldl_acc2
    val flatten = Listops.flatten
    val map_second = Listops.map_second
    val mapopt = Util.mapopt
    fun error s = Util.error "toclosure.sml" s
    val lprintl = Util.lprintl
      
    val getOpt = Option.getOpt
      

    fun flip (a,b) = (b,a)


    (* info : the results of the closure_analyze pass
     * dcalls : a mapping from the original function name to it's codeptr and value environment
     *          so that we can catch direct calls here instead of waiting for the (non-existent)
     *          optimizer to catch them.
     * globals : a set of variables that closure_analyze has decided to put in the data segment
     * curfid : the names of the current function nest we're in.
     * context : the (rewritten) types of the rewritten values
     *    Note that we do not keep this up to date while rewriting cons, since
     *    it is only used at the exp level.
     * subst : a mapping from old variables to new things
     *)
    datatype env = ENV of {info    : CA.funentry VarMap.map,
			   dcalls  : {code : sv32,venv : sv32} VarMap.map,
			   globals : label VarMap.map,
			   curfid  : var list,
			   context : LilContext.context, 
			   subst   : LS.sv32_subst * LS.sv64_subst * LS.con_subst}
      
    fun empty_subst () = (LS.SV32.empty(),LS.SV64.empty(),LS.C.empty())

    fun is_global (ENV{globals,...}) v = isSome (VarMap.find (globals,v))
    fun get_global (ENV{globals,...}) v = VarMap.find (globals,v)
      
    fun copy_env (env as ENV{info,globals,dcalls,curfid,context,subst},fid) = 
      ENV{info=info,
	  globals = globals,
	  dcalls = VarMap.filteri (fn (v,_) => is_global env v) dcalls,
	  curfid = fid::curfid,context=context,
	  subst = subst}

    fun new_env (info,globals,topfid) = ENV{info = info,globals = globals,dcalls = VarMap.empty, curfid = [topfid],context = LilContext.empty(),subst=empty_subst()}
    fun current_fid (ENV{curfid,...}) = hd curfid
    fun current_fids (ENV{curfid,...}) = curfid
    fun get_context (ENV{context,...}) = context
    fun set_context (ENV{info,globals,dcalls,curfid,subst,...},context) = ENV{info=info,globals = globals,dcalls = dcalls,curfid=curfid,context=context,subst=subst}

    fun get_info (ENV{info,...}) fname = 
      case VarMap.find(info,fname) 
	of SOME fe => fe
	 | NONE => error "Unbound function name"

    fun get_static env fname = #static (get_info env fname)
    fun get_callee env fname = #callee (get_info env fname)
    fun get_escapee env fname = #escapee (get_info env fname)
    fun get_escape env fname = #escape (get_info env fname)
    fun get_code_lbl env fname = #code_lbl (get_static env fname)
    fun get_venv_var env fname = #venv_var (get_static env fname)

    fun escapes_from (env,fname,gname) = 
      let
	val escapees = get_escapee env fname
      in
	List.exists (fn (g',_) => Name.eq_var(gname,g')) escapees
      end

    fun directcalls env (f,g) = 
      let
	val callees = get_callee env f
      in List.exists (fn (g',_) => Name.eq_var(g,g')) callees
      end
      
    fun get_dcall (env as ENV{dcalls,...},g) = 
      if directcalls env (current_fid env,g) then
	(case VarMap.find (dcalls,g)
	   of NONE => error "No dcall info for callee"
	    | some => some)
      else NONE

    fun add_dcall (ENV{info,globals,dcalls,curfid,context,subst},fid,codeptr,venv) = 
      ENV{info=info,
	  globals = globals,
	  dcalls = VarMap.insert (dcalls,fid,{code=codeptr,venv=venv}),
	  curfid = curfid,context=context,
	  subst = subst}

      
    fun get_subst (ENV{subst,...}) = subst

    fun enterfn (ENV{info,globals,dcalls,subst = (sv32s,sv64s,cs),context,curfid}) = 
      ENV{info = info,
	  globals = globals,
	  dcalls = dcalls,
	  subst=(sv32s,sv64s,LS.C.empty()),
	  context=context,curfid=curfid}
      
    fun add_csubst (ENV{info,globals,dcalls,subst = (sv32s,sv64s,cs),context,curfid},subst') = 
      ENV{info = info,
	  globals = globals,
	  dcalls = dcalls,
	  subst=(sv32s,sv64s,LS.C.compose(subst',cs)),
	  context=context,curfid=curfid}

    fun set_context_and_csubst (env,(ctxt,subst)) = 
      set_context(add_csubst(env,subst),ctxt)

    fun csubst (ENV{info,globals,dcalls,curfid,subst = (sv32subst,sv64subst,csubst),context},v,c) = 
      let
	val subst = (sv32subst,sv64subst,LS.C.addl (v,c,csubst))
      in ENV{info = info,globals = globals,dcalls = dcalls,curfid = curfid,subst = subst,context = context}
      end

    fun sv32subst (ENV{info, globals,dcalls,curfid,subst = (sv32subst,sv64subst,csubst),context},v,sv32) = 
      let
	val _ = debugdo(5,fn () => (print "Replacing var32: ";PpLil.pp_var v;print " -> ";PpLil.pp_sv32 sv32;print "\n"))
	val subst = (LS.SV32.addl (v,sv32,sv32subst),sv64subst,csubst)
      in ENV{info = info,globals = globals,dcalls = dcalls,curfid = curfid,subst = subst,context = context}
      end

    fun sv64subst (ENV{info,globals,dcalls,curfid,subst = (sv32subst,sv64subst,csubst),context},v,sv64) = 
      let
	val subst = (sv32subst,LS.SV64.addl (v,sv64,sv64subst),csubst)
      in ENV{info = info,globals = globals,dcalls = dcalls,curfid = curfid,subst = subst,context = context}
      end

    fun cvarrename (ENV{info,globals,dcalls,curfid,subst = (sv32subst,sv64subst,csubst),context},a,a') = 
      let
	val subst = (sv32subst,sv64subst,LS.C.addl (a,mk_con (Var_c a'),csubst))
      in ENV{info = info,globals = globals,dcalls = dcalls,curfid = curfid,subst = subst,context = context}
      end

    fun sv32rename (ENV{info, globals,dcalls,curfid,subst = (sv32subst,sv64subst,csubst),context},v,v') = 
      let
	val _ = debugdo(5,fn () => (print "Renaming var32: ";PpLil.pp_var v;print " -> ";PpLil.pp_var v';print "\n"))
	val subst = (LS.SV32.addl (v,Var_32 v',sv32subst),sv64subst,csubst)
      in ENV{info = info,globals = globals,dcalls = dcalls,curfid = curfid,subst = subst,context = context}
      end

    fun sv64rename (ENV{info,globals,dcalls,curfid,subst = (sv32subst,sv64subst,csubst),context},v,v') = 
      let
	val subst = (sv32subst,LS.SV64.addl (v,Var_64 v',sv64subst),csubst)
      in ENV{info = info,globals = globals,dcalls = dcalls,curfid = curfid,subst = subst,context = context}
      end

    fun do_csubst (ENV{subst = (sv32s,sv64s,cs),...},c) = LS.substConInCon cs c
    fun do_sv32subst (ENV{subst,...},sv) = 
      LS.substSv32Sv64ConInSv32 subst sv
    fun do_sv64subst (ENV{subst,...},sv) = 
      LS.substSv32Sv64ConInSv64 subst sv

    fun subst_var32 (ENV{subst = (sv32s,sv64s,cs),...},v) = 
      Util.mapopt (LS.substConInSv32 cs) (LS.SV32.substitute sv32s v)
    fun subst_var64 (ENV{subst = (sv32s,sv64s,cs),...},v) = 
      LS.SV64.substitute sv64s v
    fun subst_cvar (ENV{subst = (sv32s,sv64s,cs),...},v) = 
      LS.C.substitute cs v

    fun rename_cvar (env,v) = 
      (case subst_cvar (env,v)
	 of SOME c =>
	   (case cout c
	      of Var_c v => v
	       | _ => 
		(print "Con is:\n";
		 PpLil.pp_con c;
		 error "rename_cvar found non-variable in substitution"))
	  | NONE => v)

    fun rename_var32 (env,v) = 
      (case subst_var32 (env,v)
	 of SOME sv =>
	   (case sv
	      of Var_32 v => v
	       | _ => error "rename_var32 found non-variable in substitution")
	  | NONE => v)

    fun rename_var64 (env,v) = 
      (case subst_var64 (env,v)
	 of SOME sv =>
	   (case sv
	      of Var_64 v => v
	       | _ => error "rename_var64 found non-variable in substitution")
	  | NONE => v)

    fun cvarsrename env cvars = 
      let 
	fun folder (v,env) = 
	  let 
	    val v' = Name.derived_var v
	    val _ = debugdo(5,fn () => (print "Renaming cvar: ";PpLil.pp_var v;print " -> ";PpLil.pp_var v';print "\n"))
	  in (v',cvarrename (env,v,v'))
	  end
      in LO.foldl_acc folder env cvars
      end


    fun exp_var32_bind (env,(var,t)) = set_context(env,LC.bind_var32(get_context env,(var,t)))
    fun exp_label_bind (env,(lbl,t)) = set_context(env,LC.bind_label(get_context env,(lbl,t)))
    fun exp_var64_bind (env,(var,t)) = set_context(env,LC.bind_var64(get_context env,(var,t)))
    fun con_var_bind (env,(var,k))   = set_context(env,LC.bind_cvar(get_context env,(var,k)))
    fun kind_var_bind (env,var)      = set_context(env,LC.bind_kvar(get_context env,var,LC.Any))

    fun exp_var32s_bind (env,vts) = set_context(env,LC.bind_var32s(get_context env,vts))
    fun exp_labels_bind (env,lts) = set_context(env,LC.bind_labels(get_context env,lts))
    fun exp_var64s_bind (env,vts) = set_context(env,LC.bind_var64s(get_context env,vts))
    fun con_vars_bind (env,vks)   = set_context(env,LC.bind_cvars(get_context env,vks))

    fun typeof_sv32 (env,arg)  = (S.Typeof.sv32 (get_context env) arg) 
      handle any => (print "ERROR in typeof_sv32\n";raise any)
    fun typeof_sv64 (env,arg)  = (S.Typeof.sv64 (get_context env) arg) 
      handle any => (print "ERROR in typeof_sv64\n";raise any)
    fun kindof (env,arg)       = (S.Kindof.con (get_context env) (do_csubst (env,arg)))
      handle any => (print "ERROR in kindof\n";raise any)
    fun pexp_define(env,arg)   = (set_context_and_csubst(env,S.Typeof.pexp (get_context env,arg))) 
      handle any => (print "ERROR in pexp_define\n";raise any)

    fun op32_define(env,arg)   = (set_context(env,S.Typeof.bind_op32(get_context env,arg)))
      handle any => (print "ERROR in op32_define\n";raise any)
    fun op64_define(env,arg)   = (set_context(env,S.Typeof.bind_op64(get_context env,arg)))
      handle any => (print "ERROR in op64_define\n";raise any)
    fun unpack_define(env,arg) = (set_context(env,S.Typeof.bind_unpack(get_context env,arg))) 
      handle any => (print "ERROR in unpack_define\n";raise any)
    fun split_define(env,(a1,a2,c))  = 
      set_context_and_csubst(env,S.Typeof.bind_split(get_context env,(a1,a2,do_csubst (env,c))))
    fun unfold_define(env,(a,c)) = 
      set_context_and_csubst(env,S.Typeof.bind_unfold(get_context env,(a,do_csubst (env,c))))
    fun inj_define(env,(w,a,c,sv))    = 
      set_context_and_csubst(env,S.Typeof.bind_inj(get_context env,(w,a,do_csubst (env,c),sv)))

    fun find_cvar env var  = ((LC.find_cvar(get_context env,var)) 
			      handle LC.Unbound s => error ("Unbound cvariable: "^(Name.var2string var)))
    fun find_var32 env var = ((do_csubst(env,LC.find_var32(get_context env,var))) 
			      handle LC.Unbound s => error ("Unbound 32variable: "^(Name.var2string var)))
    fun find_label env lbl = ((do_csubst(env,LC.find_label(get_context env,lbl))) 
			      handle LC.Unbound s => error ("Unbound label: "^(Name.label2string lbl)))
    fun find_var64 env var = ((do_csubst(env,LC.find_var64(get_context env,var))) 
			      handle LC.Unbound s => error ("Unbound 64variable: "^(Name.var2string var)))



    (* We may have renamed the free variables already, so we must
     * be sure to use the new names
     *)
    fun get_frees env fname = 
      let
	val info = get_info env
	val {cvars,vars32,vars64} = #frees (get_info env fname)
      in {cvars = VarSet.map (fn a => rename_cvar (env,a)) cvars,
	  vars32 = VarSet.map (fn v => rename_var32 (env,v)) vars32,
	  vars64 = VarSet.map (fn v => rename_var64 (env,v)) vars64}
      end


    local
      val rewritten : con ConMap.map ref = ref ConMap.empty
    in
      fun get_rewritten c = ConMap.find(!rewritten,c)
      fun reset_rewritten () = rewritten := ConMap.empty
      fun set_rewritten (c1,c2) = rewritten := (ConMap.insert(!rewritten,c1,c2))
    end


    local 
      val data : data list ref = ref []
      fun add_data d = data := d :: !data
    in
      fun add_dtuple (l,c,q,svs) = add_data (Dtuple (l,c,q,svs))
      fun add_dboxed (l,sv)    = add_data (Dboxed (l,sv))
      fun add_dcode (l,f)      = add_data (Dcode (l,f))
      fun get_data () = rev (!data)
      fun reset_data () = data := []
    end

    fun rewrite_con (env : env) (con : con) : con =  
      let 
	val () = chat 6 "Rewriting con\n"
	val () = debugdo(7,fn () => (print "Con is: \n";
				     PpLil.pp_con con;
				     print "\n"))

	(* This part assumes that allarrow has already been caught and handled.
	 *)
	fun docon' (con : con) = 
	  let 
	  in
	    case cout con 
	      of Var_c v => con
	       | Nat_c w => con
	       | App_c (c1,c2) => mk_con(App_c (docon c1,docon c2))
	       | APP_c (c,k) => mk_con (APP_c (docon c,k))
	       | Pi1_c c => LD.C.pi1 (docon c)
	       | Pi2_c c => LD.C.pi2 (docon c)
	       | Prim_c p => con
	       | Pr_c (j,(a,k),k',r,body)  => 
		let
		  val body = docon body
		in mk_con(Pr_c (j,(a,k),k',r,body))
		end
	       | Case_c (arg,arms,def) =>
		let
		  val arg = docon arg
		  fun doarm (w,(a,c)) = (w,(a,docon c))
		  val arms = map doarm arms
		  val def = Util.mapopt docon def
		in mk_con(Case_c (arg,arms,def))
		end
		| LAM_c (j,c) =>
		 let 
		   val c = docon c
		 in mk_con(LAM_c (j,c))
		 end
		| Lam_c ((a,k),c) =>
		 let 
		   val c = docon c
		 in mk_con(Lam_c ((a,k),c))
		 end
		| Pair_c (c1,c2) => mk_con(Pair_c (docon c1,docon c2))
		| Star_c => con
		| Inj_c (w,k,c) => mk_con(Inj_c (w,k,docon c))
		| Fold_c (k,c) => mk_con(Fold_c (k,docon c))
		| Ptr_c c => mk_con (Ptr_c (docon c))
	  end
	and docon con = 
	  (case get_rewritten con
	     of SOME c => c
	      | NONE => 
	       let
		 val rc = 
		   case Dec.C'.allarrow' con
		     of SOME (vks,args,fargs,rt) => 
		       let
			 val () = chat 6 "\t Arrow con\n"
			 val args = docon args
			 val fargs = docon fargs
			 val rt = docon rt
		       in LD.T.closure vks args fargs rt
		       end
		      | NONE => docon' con
		 val () = set_rewritten (con,rc)
	       in rc
	       end)
	      
	val () = chat 6 "Finished  con\n"
      in  docon (do_csubst (env,con))
      end
    
    and rewrite_exp (env : env) (exp : exp) : exp = 
      P.Lili.to_exp (rewrite_exp' env exp)
    and rewrite_exp' (env : env) (exp : exp) : sv32 P.pexp =
      let 
	fun doexp (env,e : exp) = 
	  (case #e e 
	     of Val32_e sv32 => P.ret (rewrite_sv32 env sv32)
	      | Let_e (bnds,e) => 
	       P.bind (rewrite_bnds env bnds)
	       (fn env => rewrite_exp' env e))
	     
      in doexp (env,exp)
      end
    and rewrite_sv32 (env : env) (sv32 : sv32) : sv32 = 
      let 
	fun dosv32 (env,sv) = 
	  let
	    val recur_sv32 = rewrite_sv32 env
	    val recur_sv64 = rewrite_sv64 env
	    val recur_primarg = rewrite_primarg env
	    val recur_c = rewrite_con env
	    val recur_e = rewrite_exp env
	      
	  in
	    case sv32 
	      of Var_32 v => 
		(case subst_var32 (env,v)
		   of SOME sv => sv
		    | _ => sv32)
	       | Label l => sv32
	       | Coercion (c,args) => Coercion (c,map recur_c args)
	       | Coerce (q,sv) => Coerce (recur_sv32 q,recur_sv32 sv)
	       | Tabs ((a,k),sv) => 
		let 
		  val env = con_var_bind (env,(a,k))
		  val sv = rewrite_sv32 env sv
		in Tabs ((a,k),sv)
		end
	       | TApp (sv,c) => 
		let 
		  val sv = recur_sv32 sv
		  val c  = recur_c c
		in TApp (sv,c)
		end
	       | Tag w => Tag w
	       | Unit => Unit
	       | Const_32 v => 
		Const_32
		(case v of
		   (Prim.int _) => v
		 | (Prim.uint _) => v
		 | (Prim.float _) => v
		 | (Prim.array (c,array)) =>
		     let
		       val _ = Array.modify recur_primarg array
		       val c = recur_c c
		     in Prim.array(c,array)
		     end
		 | (Prim.vector (c,array)) =>
		     let
		       val _ = Array.modify recur_primarg array
		       val c = recur_c c
		     in Prim.vector(c,array)
		     end
		 | Prim.refcell (r as (ref e)) => 
		     (r := recur_primarg e; v)
		 | Prim.tag (t,c) => 
		     Prim.tag(t,recur_c c))
	  end
	
      in (dosv32 (env,sv32))
	  handle any => (print "Error while rewriting sv32:\n";
			 PpLil.pp_sv32 sv32;print "\n";
			 raise any)
      end
    and rewrite_bnds (env : env) (bnds : bnd list) : env P.pexp = 
      let
	fun do_bnd (bnd,env) = (rewrite_bnd env bnd)
	  handle any => (print "Error while rewriting bound:\n";
			 PpLil.pp_bnd bnd;print "\n";
			 raise any)
      in P.List.foldl_from_list do_bnd env bnds
      end
    and rewrite_bnd (env : env) (bnd : bnd) : env P.pexp =
      let

	val res = 
	  case bnd
	    of Fixcode_b args => rewrite_fixcodeb env args
	     | Exp32_b args   => rewrite_exp32b env args
	     | Exp64_b args   => rewrite_exp64b env args
	     | Unpack_b args  => rewrite_unpackb env args
	     | Split_b args   => rewrite_splitb env args
	     | Inj_b args     => rewrite_injb env args
	     | Unfold_b args  => rewrite_unfoldb env args
      in	    
	res
      end



    and rewrite_fixcodeb env vfs = 
      let
	
	fun build_cenv_kinds env cvars =
	  let
	    val kinds = map (find_cvar env) cvars
	  in kinds
	  end
	
	fun build_cenv env cvars = 
	  let
	    val cfields = map ((rewrite_con env) o mk_con o Var_c) cvars
	  in cfields
	  end
	
	
	fun build_venv_type env (vars32,vars64) = 
	  let
	    val vars32_types = map (find_var32 env) vars32
	    val vars64_types = map (LD.T.ptr o (LD.T.boxed B8) o (find_var64 env)) vars64
	    val venv_type = 
	      (case (!do_single_venv,vars32_types,vars64_types)
		 of (true,[t],[]) => t
		  | (true,[],[t]) => t
		  | _ => LD.T.tupleptr' (vars32_types @ vars64_types))
	  in venv_type
	  end
	
	
	fun build_venv env (vars32,vars64) : sv32 P.pexp = 
	  let
	    val venv = 
	      (case (!do_single_venv,vars32,vars64)
		 of (true,[v],[]) => P.ret (rewrite_sv32 env (Var_32 v))
		  | (true,[],[vf]) => LD.E.box (rewrite_sv64 env (Var_64 vf))
		  | _ => 
		   let
		     val sv64s = map ((rewrite_sv64 env) o Var_64) vars64 
		     val sv32s = map ((rewrite_sv32 env) o Var_32) vars32
		     val boxps = P.List.map_from_list LD.E.box sv64s
		   in
		     P.bind boxps
		     (fn boxs => 
		      let
			val fields = sv32s @ boxs
		      in LD.E.tuple fields
		      end)
		   end)
	  in venv
	  end
	

	  
	fun build_code_type env (fname, Function {tFormals    : (var * kind) list,
						  eFormals    : (var * con) list,
						  fFormals    : (var * con) list,
						  rtype       : con,
						  body        : exp}) = 
	  let
	    val {cvars,vars32,vars64} = get_frees env fname
	    val cvars  = VarSet.listItems cvars
	    val vars32 = VarSet.listItems vars32
	    val vars64 = VarSet.listItems vars64
	    val cenks = build_cenv_kinds env cvars
	    val (cvars,env) = cvarsrename env cvars 
  
	    val tFormals = (LO.zip cvars cenks)@tFormals
	    val env = con_vars_bind (env,tFormals)
	      
	    val venv_type = build_venv_type env (vars32,vars64)
	    val args = LO.seconds eFormals
	    val fargs = LO.seconds fFormals
	    val args = map (rewrite_con env) args
	    val args = venv_type::args
	    val fargs = map (rewrite_con env) fargs
	    val rtype = rewrite_con env rtype
	    val code_type = LD.T.allcode' tFormals args fargs rtype
	  in (get_code_lbl env fname,code_type)
	  end

      
	(* Produce the bounds to unpack the environment, and an environment
	 * which maps the old variable name to the new name.
	 *)
	fun unpack_venv env (venv_var,vars32,vars64) : env P.pexp =
	  let
	    val (bnds_env) = 
	      (case (!do_single_venv,vars32,vars64)
		 of (true,[v],[]) => P.ret (sv32rename (env,v,venv_var))
		  | (true,[],[vf]) =>  
		   let
		     val unboxed = LD.E.unbox (Var_32 venv_var)
		     val env = pexp_define(env,unboxed)
		   in
		     P.map (fn sv64 => sv64subst (env,vf,sv64)) unboxed
		   end
		  | _ => 
		   let  
		     fun folder32 (v,(i,bnds_env)) = 
		       let
			 val bnds_env = 
			   P.bind bnds_env
			   (fn env => 
			    let
			      val v' = Name.derived_var v
			      (* Improves the naming *)
			      val op32 = LD.E.select' i (Var_32 venv_var)
			      val sv32 = P.Bind.op32' v' op32 (P.ret (Var_32 v'))
			      val env = pexp_define (env,sv32)
			      val env = P.map (fn sv32 => sv32subst (env,v,sv32)) sv32
			    in env
			    end)
		       in (i+0w1,bnds_env)
		       end
		     fun folder64 (vf,(i,bnds_env)) = 	
		       let
			 val bnds_env = 
			   P.bind bnds_env
			   (fn env => 
			    let
			      val sv32 = LD.E.select i (Var_32 venv_var)
			      val vf' = Name.derived_var vf
			      (* Improves the naming *)
			      val op64 = P.lift LD.E.unbox' sv32
			      val sv64 = P.Bind.op64' vf' op64 (P.ret (Var_64 vf'))
			      val env = pexp_define (env,sv64)
			      val env = P.map (fn sv64 => sv64subst (env,vf,sv64)) sv64
			    in env
			    end)
		       in (i+0w1,bnds_env)
		       end		   
		     val (i,bnds_env) = foldl folder32 (0w0,P.ret env) vars32
		     val (_,bnds_env) = foldl folder64 (i,bnds_env) vars64
		   in (bnds_env)
		   end)
	  in bnds_env
	  end

      
	fun build_closure ((fname,fclos_type),env) = 
	  let
	    val _ = chat 4 ("Working on closure for: "^(Name.var2string fname)^"\n")
	    val {cvars,vars32,vars64} = get_frees env fname
	    val cvars  = VarSet.listItems cvars
	    val vars32 = VarSet.listItems vars32
	    val vars64 = VarSet.listItems vars64
	    val code_lbl = get_code_lbl env fname
	    val codeptr = rewrite_sv32 env (Label code_lbl)
	    val codeptr = LD.E.nary_tapp' (build_cenv env cvars) codeptr
	    val venv_type = build_venv_type env (vars32,vars64)
	    val venv = build_venv env (vars32,vars64)
	  in P.bind venv 
	    (fn venv => 
	     let
	       val env = add_dcall (env,fname,codeptr,venv)
	     in
	       if get_escape env fname then
		 case get_global env fname
		   of SOME clos_lbl =>
		     let
		       val _ = chat 4 ("Adding static closure for: "^(Name.var2string fname)^"\n")
		       val env = exp_label_bind (env,(clos_lbl,fclos_type))
		       val tuplbl   = Name.fresh_internal_label ((Name.label2name code_lbl)^"_clos")
		       val q = LD.Q.pack fclos_type venv_type
		       val () = 
			 add_dtuple (clos_lbl,
				     fclos_type,SOME q,
				     [codeptr,venv])
		       val env = sv32subst(env,fname,Label clos_lbl)
		     in P.ret env
		     end
		    | NONE =>
		     let
		       val _ = chat 4 ("Adding dynamic closure for: "^(Name.var2string fname)^"\n")
		       val closurep = LD.E.closure (codeptr,venv,fclos_type,venv_type)
		     in P.SV32.bind2var' fname closurep (P.ret env)
		     end
	       else P.ret env
	     end)
	  end
	
	fun create_local_closures env (fvars,cenvs,venv,venv_type,vars32) : env P.pexp = 
	  let
	    val _ = chat 4 ("Adding local closures\n")
	    fun add_closures [] = P.ret env
	      | add_closures (fname::fvars) = 
	      P.bind (add_closures fvars) 
	      (fn env => 
	       let
		 val code_lbl = get_code_lbl env fname
		 val codetype = find_label env code_lbl
		 val codeptr  = rewrite_sv32 env (Label code_lbl)
		 val codeptr  = LD.E.nary_tapp' cenvs codeptr
		 val env = add_dcall(env,fname,codeptr,venv)

		 (* XXX: This is a conservative approximation.  The function may not
		  * actually escape internally.  Should do better.
		  *)
		 val escapes = escapes_from(env,current_fid env,fname)
	       in
		 if escapes then 
		   let
		     val _ = chat 4 ("Adding local closure for: "^(Name.var2string fname)^"\n")
		     val newv = Name.derived_var fname
		     val clos_type = find_var32 env fname
		     val env = sv32rename(env,fname,newv)
		     val env = exp_var32_bind (env, (newv,clos_type))
		     val closure_p = LD.E.closure (codeptr,venv,clos_type,venv_type)
		     val named_closure = P.SV32.bind2var' newv closure_p (P.ret env)
		   in named_closure
		   end 
		 else 
		   (case get_global env fname
		      of SOME clos_lbl => 
			let
			  val clos_type = (find_var32 env fname)
			  val env = sv32subst(env,fname,Label clos_lbl)
			  val env = exp_label_bind (env, (clos_lbl,clos_type))
			in P.ret env
			end 
		       | NONE => P.ret env)
	       end)
	  in add_closures fvars
	  end


	fun rewrite_function fvars env (fname,Function {tFormals    : (var * kind) list,
							eFormals    : (var * con) list,
							fFormals    : (var * con) list,
							rtype       : con,
							body        : exp}) = 
	  let
	    val _ = chat 4 ("Rewriting function: "^(Name.var2string fname)^"\n")
	    val env = copy_env (env,fname)
	    val {cvars=cvars_set,vars32=vars32_set,vars64=vars64_set} = get_frees env fname
	    val cvars  = VarSet.listItems cvars_set
	    val vars32 = VarSet.listItems vars32_set
	    val vars64 = VarSet.listItems vars64_set
	    val cenv_kinds = build_cenv_kinds env cvars
	    val (cvars,env) = cvarsrename env cvars 
	    val (tvars,tkinds) = LO.unzip tFormals
	    val (tvars,env) = cvarsrename env tvars
	    val tFormals = (LO.zip cvars cenv_kinds)@(LO.zip tvars tkinds)
	    val env = con_vars_bind (env,tFormals)
	      
	    (* Rewrite the eFormals to refer to the closure parameter, and extend
	     * them with the value environment 
	     *)
	    val venv_var = get_venv_var env fname
	    val venv_type = build_venv_type env (vars32,vars64)
	      
	    val eFormals = map_second (rewrite_con env) eFormals
	    val eFormals = (venv_var,venv_type)::eFormals
	    val env = exp_var32s_bind (env,eFormals)
	    val fFormals = map_second (rewrite_con env) fFormals
	    val env = exp_var64s_bind (env,fFormals)
	    val rtype = rewrite_con env rtype
	      
	      
	    (* First, we must unpack the environment *)
	    val unpack_env = unpack_venv env (venv_var,vars32,vars64)
	      
	    val cenv = map (fn a => mk_con (Var_c a)) cvars
	    val venv = Var_32 venv_var

	    val clos_env = P.bind unpack_env 
	      (fn env => create_local_closures env (fvars,cenv,venv,venv_type,vars32_set))

	    val () = chat 4 ("Rewriting function body\n")
	    val body = P.bind clos_env (fn env => P.Lili.from_exp (rewrite_exp env body))
	    val body = P.Lili.to_exp body
	      
	    val code_fun = Function{tFormals=tFormals,
				    eFormals=eFormals,
				    fFormals=fFormals,
				    rtype = rtype,
				    body = body}
	    val lbl = get_code_lbl env fname
	    val _ = add_dcode (lbl,code_fun)
	    val _ = chat 4 ("Done rewriting function: "^(Name.var2string fname)^"\n")
	  in ()
	  end
	
	val vftypes = LO.map_second S.Typeof.function vfs
	val fvars = map #1 vfs


	val () = chat 3 ("Working on function nest: ")
	val () = if chatp 3 then PpLil.pp_list PpLil.pp_var' fvars ("<",",",">",false) else ()
	val () = chat 3 "\n"
	val () = chat 3 ("Rewriting function types to closure types\n")
	val vfclosure_types = LO.map_second (rewrite_con env) vftypes
	val () = chat 3 ("Rewriting function types to code types\n")
	val code_vftypes = map (build_code_type env) vfs

	val env = exp_var32s_bind (env,vfclosure_types)
	val env = exp_labels_bind (env,code_vftypes)


	val () = chat 3 ("Rewriting functions\n")
	val () = app (rewrite_function fvars env) vfs

	val () = chat 3 ("Rewriting closures\n")
	(* XXX: I think all of the functions should share the same venv under this 
	 * strategy.  We may wish to catch this here.
	 *)
	val bnds_env : env P.pexp = 
	  P.List.foldl_from_list build_closure env vfclosure_types

      in  
	bnds_env
      end
    
  
    and rewrite_exp32b env (v,op32) = 
      let
	val oper = rewrite_op32 env op32
	val env = pexp_define (env,oper)
      in
	P.bind oper
	(fn oper => 
	 (case (!staticalloc,get_global env v,oper)
	    of (true,SOME l,LilPrimOp32 (Tuple,_,svs,_)) => 
	      let
		val t = LD.T.tupleptr' (map (fn sv => typeof_sv32(env,sv)) svs)
		val () = add_dtuple (l,t,NONE,svs)
		val env = sv32subst (env,v,Label l)
		val env = exp_label_bind (env,(l,t))
	      in P.ret env
	      end
	     | (true,SOME l,LilPrimOp32 (Box,_,_,[sv])) => 
	      let
		val () = add_dboxed (l,sv)
		val env = sv32subst (env,v,Label l)
		val env = exp_label_bind (env,(l,LD.T.ptr(LD.T.boxed_float())))
	      in P.ret env
	      end
	     | _ => 
	      let
		val env = op32_define (env,(v,oper))
	      in P.Bind.op32' v (P.ret oper) (P.ret env)
	      end))
      end
    and rewrite_exp64b env (v,op64) =
      let
	val oper = rewrite_op64 env op64
	val env = op64_define (env,(v,oper))
      in P.Bind.op64' v (P.ret oper) (P.ret env)
      end
    and rewrite_unpackb env (a,x,sv32) =
      let
	val sv32 = rewrite_sv32 env sv32 
	val env = unpack_define(env,(a,x,sv32))
      in P.Bind.unpack' (a,x) (P.ret sv32) (P.ret env)
      end
    and rewrite_splitb env (a1,a2,c) =
      let
	val c = rewrite_con env c 
	val env = split_define (env,(a1,a2,c))
      in P.Bind.split' (a1,a2) (P.ret c) (P.ret env)
      end
    and rewrite_injb env (w,b,c,sv) =
      let
	val c = rewrite_con env c 
	val sv = rewrite_sv32 env sv 
	val env = inj_define (env,(w,b,c,sv))
      in P.Bind.inj' b (P.ret (w,c,sv)) (P.ret env)
      end
    and rewrite_unfoldb env (a,c) =
      let
	val c = rewrite_con env c 
	val env = unfold_define (env,(a,c))
      in P.Bind.unfold' a (P.ret c) (P.ret env)
      end
    
    and rewrite_op32 (env : env) (op32 : op32) : op32 P.pexp = 
      let
	
	val recur_sv32 = rewrite_sv32 env
	val recur_sv64 = rewrite_sv64 env
	val recur_primarg = rewrite_primarg env
	val recur_c = rewrite_con env
	val recur_e = rewrite_exp env
	  
	fun do_op32 (env,op32) = 
	  (case op32
	     of Val sv32 => P.ret (Val (recur_sv32 sv32))
	      | Prim32 (p,cs,primargs) => 
	       P.ret (Prim32 (p,map recur_c cs, map recur_primarg primargs))
	      | LilPrimOp32 (lp,cs,sv32s,sv64s) => 
	       P.ret (LilPrimOp32 (lp,
				   map recur_c cs, 
				   map recur_sv32 sv32s,
				   map recur_sv64 sv64s))
	      | ExternApp (sv,sv32s,sv64s) =>  P.ret (ExternApp (recur_sv32 sv,map recur_sv32 sv32s,map recur_sv64 sv64s))
	      | Call (sv,sv32s,sv64s) =>  P.ret (Call (recur_sv32 sv,map recur_sv32 sv32s,map recur_sv64 sv64s))
	      | App (f,sv32s,sv64s) => 
	       (* This potentially sucks major rocks.  Because we can only pack
		* small values, we are forced to translate polymorphic functions
		* to existentials with a polymorphic code pointer (that is, Exists().(All...))
		* instead of All().Exists.  But this means that in order to 
		* instantiate, we must first unpack and project.  This could
		* be alleviated with a good coercion language. 
		* Fortunately (sort of) MIL code will always pass a value
		* with the types, so as long as we catch them together, we are
		* not incurring any extra overhead.
		*)

	       let 
		 val _ = debugdo(4,fn () => (print "Entering app - op is:\n";
					     PpLil.pp_op32 op32;print "\n"))
		 val sv32s = map recur_sv32 sv32s 
		 val sv64s = map recur_sv64 sv64s

		 fun dodirectcall (code,cargs,venv) = 
		   let 
		     val _ = debugdo(6,fn() => (print "Type of code is:\n";
						PpLil.pp_con (typeof_sv32(env,code));print "\n"))
		     val venv = recur_sv32 venv
		     val code = recur_sv32 code
		     val sv32s = venv :: sv32s
		     val f = LD.E.nary_tapp' cargs code
		   in P.ret (Call(f, sv32s, sv64s))
		   end
		 val res = 
		   case Dec.E.nary_tapp f
		     of (Var_32 fvar,cargs) => 
		       let
			 val cargs = map recur_c cargs
		       in
			 case get_dcall(env,fvar) 
			   of SOME {code,venv} => dodirectcall (code,cargs,venv)
			    | NONE => LD.E.closure_app'(recur_sv32 (Var_32 fvar),cargs,sv32s,sv64s)
		       end
		      | _ => error "Shouldn't be anything else here, should there?"
		 val _ = debugdo(4,fn () => (print "Leaving app - res is\n";
					     PpLil.pp_op32 (P.out res);print "\n"))
	       in res
	       end
	      | Switch switch => P.ret(Switch (rewrite_switch env switch))
	      | Raise (c,sv32) => P.ret(Raise (recur_c c,recur_sv32 sv32))
	      | Handle (t,e1,(v,e2)) => 
	       let
		 val t = recur_c t
		 val e1 = recur_e e1
		 val env = exp_var32_bind (env,(v,LD.T.exn()))
		 val e2 = rewrite_exp env e2
	       in P.ret(Handle (t,e1,(v,e2)))
	       end)
      in do_op32 (env,op32)
      end
    and rewrite_sv64 (env : env) (sv64 : sv64) : sv64 = 
      let
	fun dosv64 (env,sv64) = 
	  (case sv64
	     of Var_64 x => 
	       (case subst_var64 (env,x)
		  of SOME sv => sv
		   | _ => Var_64 x)
	      | Const_64 v => Const_64 v)
      in dosv64 (env,sv64)
      end
    and rewrite_op64 (env : env) (op64 : op64) : op64 = 
      let
	val recur_sv32 = rewrite_sv32 env
	val recur_sv64 = rewrite_sv64 env
	val recur_primarg = rewrite_primarg env
	val recur_c = rewrite_con env
      in
	case op64
	  of Val_64 sv64 => Val_64 (recur_sv64 sv64)
	   | Unbox sv32 => Unbox (recur_sv32 sv32)
	   | ExternAppf (sv,sv32s,sv64s) =>  ExternAppf (recur_sv32 sv,map recur_sv32 sv32s,map recur_sv64 sv64s)
	   | Prim64 (p,primargs) =>
	    Prim64 (p,map recur_primarg primargs)
      end
    and rewrite_primarg (env : env) (primarg : primarg) : primarg = 
      (case primarg 
	 of arg32 sv32 => arg32 (rewrite_sv32 env sv32)
	  | arg64 sv64 => arg64 (rewrite_sv64 env sv64))
	 
    and rewrite_switch (env : env) (sw : switch) : switch = 
      let
	val recur_sv32 = rewrite_sv32 env 
	val recur_exp  = rewrite_exp env
	val recur_con  = rewrite_con env
	val recur_cc   = rewrite_cc env
	fun do_switch (env,sw) = 
	  (case sw
	     of Sumcase {arg : sv32,arms :(w32 * var * exp) list, default: exp option, rtype : con} => 
	       let
		 val arg = recur_sv32 arg
		 val t = typeof_sv32 (env,arg)
		 val default = mapopt recur_exp default
		 val rtype = recur_con rtype
		 fun mapper (w,v,e) = 
		   let 
		     val env = exp_var32_bind (env,(v,LD.COps.sum2ksum' w t))
		   in (w,v,rewrite_exp env e)
		   end
		 val arms = map mapper arms
	       in Sumcase {arg = arg,arms = arms,default = default, rtype = rtype}
	       end
	      | Dyncase {arg : sv32,arms :(sv32 * (var * con) * exp) list, default: exp,        rtype : con} =>
	       let
		 val arg = recur_sv32 arg
		 val default = recur_exp default
		 val rtype = recur_con rtype
		 fun mapper (sv,(v,t),e) =
		   let 
		     val t = recur_con t
		     val sv = recur_sv32 sv
		     val env = exp_var32_bind (env,(v,t))
		   in (sv,(v,t),rewrite_exp env e)
		   end
		 val arms = map mapper arms
	       in Dyncase {arg = arg,arms = arms,default = default, rtype = rtype}
	       end
	     
	      | Intcase {arg : sv32,arms :(w32 * exp) list, default: exp,rtype : con} =>
	       let
		 val arg = recur_sv32 arg
		 val default = recur_exp default
		 val rtype = recur_con rtype
		 val arms = map_second recur_exp arms
	       in Intcase {arg = arg,arms = arms,default = default, rtype = rtype}
	       end
	      | Ifthenelse {arg : conditionCode,thenArm : exp, elseArm : exp, rtype : con} =>
	       let
		 val arg = recur_cc arg
		 val thenArm = recur_exp thenArm
		 val elseArm = recur_exp elseArm
		 val rtype = recur_con rtype
	       in Ifthenelse {arg = arg,thenArm = thenArm, elseArm = elseArm, rtype = rtype}
	       end)
      in do_switch (env,sw)
      end
    and rewrite_cc env cc = 
      let
	val recur_e = rewrite_exp env
	val recur_cc = rewrite_cc env
      in
	(case cc
	   of Exp_cc e => Exp_cc (recur_e e)
	    | Not_cc cc => Not_cc (recur_cc cc)
	    | And_cc(cc1,cc2) => And_cc(recur_cc cc1,recur_cc cc2)
	    | Or_cc (cc1,cc2) => Or_cc(recur_cc cc1,recur_cc cc2))
      end


    fun show_analysis (info,globals) = 
      let
	fun pp_global (v,l) = (print "(";PpLil.pp_var v;print " -> ";PpLil.pp_label l;print ")\t")

	fun pp_list doer l = PpLil.pp_list doer l ("{",",","}",false)

	fun show_info (v,{static = {code_lbl,venv_var},
			  escape,callee,escapee,
			  frees = {vars32,vars64,cvars}}) = 
	  let in
	    print "Function: ";PpLil.pp_var v;
	    print " code_lbl = ";PpLil.pp_label code_lbl;
	    print " venv_var = ";PpLil.pp_var venv_var;
	    print "\t";print (if escape then "escapes" else "local");print "\n";
	    print "\tcallees are: ";pp_list (fn (f,s) => PpLil.pp_var' f) callee;print "\n";
	    print "\tescapees are: ";pp_list (fn (f,s) => PpLil.pp_var' f) escapee;print "\n";
	    print "\tFree con vars: ";pp_list PpLil.pp_var' (VarSet.listItems cvars);print "\n";
	    print "\tFree 32b vars: ";pp_list PpLil.pp_var' (VarSet.listItems vars32);print "\n";
	    print "\tFree 64b vars: ";pp_list PpLil.pp_var' (VarSet.listItems vars64);print "\n"
	  end
      in
	print "Globals are:\n";
	VarMap.appi pp_global globals;
	print "\nFunction entries are:\n";
	VarMap.appi show_info info
      end


    fun close_mod (module as MODULE{timports,data=[],confun,expfun}) = 
      let 

	val () = reset_data()	  
	val () = reset_rewritten()	  
	val top_fid = Name.fresh_named_var "top_fid"
      
	val (info,globals) = CA.findfv_module top_fid module

	val _ = if !debug then show_analysis(info,globals) else ()
	(* Rewrite module *)
	val env = new_env (info,globals,top_fid)	  

	val _ = chat 1 "  Adding timports\n"
	val env = con_vars_bind (env,timports)

	val _ = chat 1 "  Rewriting confun\n"	  
	val confun = rewrite_con env confun

	val _ = chat 1 "  Rewriting expfun\n"
	val expfun = rewrite_exp env expfun

	val _ = chat 1 "  Module rewritten\n"

	val data = get_data()
	val () = reset_data()
	val () = reset_rewritten()
      in  MODULE{timports = timports,data=data,confun=confun,expfun=expfun}
      end handle any => (reset_data();reset_rewritten();raise any)
    
  end (* ClosureConvert *)

