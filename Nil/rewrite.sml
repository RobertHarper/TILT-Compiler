(*$import Nil NILREWRITE *)
structure NilRewrite :> NILREWRITE = 
  struct
    open Nil
      
    val foldl_acc = Listops.foldl_acc
    val foldl_acc2 = Listops.foldl_acc2
    val map_second = Listops.map_second

    fun error s = Util.error "NilRewrite" s
    val lprintl = Util.lprintl

    datatype 'a changeopt = NOCHANGE | NORECURSE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a
      
    datatype 'state handler =
      HANDLER of {
		  bndhandler : 'state * bnd -> ('state * bnd list) changeopt,
		  cbndhandler : 'state * conbnd -> ('state * conbnd list) changeopt,
		  (*For cbnds, a return of CHANGE_NORECURSE (state,cbnds)
		   * will result in cbnds being bound in state before being returned.
		   *)

		  conhandler : 'state * con -> ('state * con) changeopt,
		  exphandler : 'state * exp -> ('state * exp) changeopt,
		  kindhandler : 'state * kind -> ('state * kind) changeopt,
		  con_var_bind : 'state * var * kind -> ('state * var),
		  con_var_define : 'state * var * con -> ('state * var),
		  exp_var_bind : 'state * var * con -> ('state * var),
		  exp_var_define : 'state * var * exp -> ('state * var)
		  }

    fun rewriters (handler : 'state handler) 
      : {
	 rewrite_kind :'state -> Nil.kind -> Nil.kind,
	 rewrite_con : 'state -> Nil.con -> Nil.con,
	 rewrite_exp : 'state -> Nil.exp -> Nil.exp,
	 rewrite_bnd : 'state -> Nil.bnd -> (Nil.bnd list * 'state),
	 rewrite_cbnd : 'state -> Nil.conbnd -> (Nil.conbnd list * 'state),
	 rewrite_mod : 'state -> Nil.module -> Nil.module
	 }
      =
      let

	val (HANDLER{bndhandler,
		     exphandler,
		     cbndhandler,
		     conhandler,
		     kindhandler,
		     con_var_bind,
		     con_var_define,
		     exp_var_bind,
		     exp_var_define}) = handler

	fun rewrite_cbnd (state : 'state) (cbnd : conbnd) : (conbnd list * 'state) =
	  let
	    fun define wrap (var,vklist,c,k) state = 
	      let
		val var' = Name.derived_var var
		val con = Let_c (Sequential,[wrap (var',vklist,c,k)],Var_c var')
		val (state,var) = con_var_define(state,var,con)
	      in 
		(wrap(var, vklist, c, k), state)
	      end

	    fun cbnd_recur wrap (var, vklist, c, k) oldstate = 
	      let 
		fun folder((v,k),state) = 
		  let 
		    val k = rewrite_kind state k
		    val (state,v) = con_var_bind (state,v,k)
		  in  ((v,k),state)
		  end
		val (vklist,state) = foldl_acc folder oldstate vklist
		val c = rewrite_con state c
		val k = rewrite_kind state k
	      in 
		define wrap (var,vklist,c,k) oldstate
	      end

	    fun do_cbnd recur (cbnd,state) = 
	      (case cbnd 
		 of Con_cb(var, con) =>
		   let 
		     val con = 
		       if recur 
			 then rewrite_con state con
		       else con
		     val (state,var) = con_var_define(state,var,con)
		   in (Con_cb(var, con), state)
		   end
		  | Open_cb (args as (var, vklist, c, k)) =>
		   (if recur then
		      cbnd_recur Open_cb args state 
		    else 
		      define Open_cb args state)
		  | Code_cb args => 
		      (if recur then
			 cbnd_recur Code_cb args state 
		       else 
			 define Code_cb args state))

	  in
	    (case (cbndhandler (state,cbnd)) 
	       of CHANGE_NORECURSE (state,cbnds) => foldl_acc (do_cbnd false) state cbnds
		| CHANGE_RECURSE (state,cbnds) => foldl_acc (do_cbnd true) state cbnds
		| NOCHANGE => 
		 let 
		   val (cb,s) = do_cbnd true (cbnd,state)
		 in ([cb],s)
		 end
		| NORECURSE => ([cbnd],state))
	  end  
	
	and rewrite_con (state : 'state) (con : con) : con =  
	  let 
	    fun docon (state,con) = 
	      let
		val recur = rewrite_con state
	      in
		(case con 
		   of (Prim_c (pcon,args)) => (Prim_c (pcon,map recur args))
		     
		    | (Mu_c (flag,defs)) =>
		     let
		       fun folder ((v,c),state) = 
			 let
			   val (state,v) = con_var_bind (state,v,Type_k)
			 in
			   ((v,c),state)
			 end
		       val (defs,state) = if flag then Sequence.foldl_acc folder state defs else (defs,state)
		       val defs = Sequence.map_second (rewrite_con state) defs
		     in  Mu_c (flag,defs)
		     end
		   
		    | (AllArrow_c (openness, effect, tformals, vars_opt, cons, numfloat, result)) =>
		     let
		       val (tformals,state) = tformals_helper state tformals
		       val (vars_opt,cons,state)  = 
			 (case vars_opt
			    of SOME vars => 
			      let
				fun vcfolder(v,c,s) = 
				  let 
				    val c = rewrite_con s c
				    val (s,v) = exp_var_bind(s,v,c) 
				  in  (v,c,s)
				  end
				val (vars,cons,state) = foldl_acc2 vcfolder state (vars,cons)
			      in
				(SOME vars,cons,state)
			      end
			     | NONE => (vars_opt,map (rewrite_con state) cons,state))
		       val result = rewrite_con state result
		     in
		       AllArrow_c(openness, effect, tformals, vars_opt, cons, numfloat, result)
		     end
		     
		    | ExternArrow_c (cons,con) =>
		     let
		       val cons = map recur cons
		       val con = recur con
		     in ExternArrow_c (cons,con)
		     end
		    | (Var_c var) => con
		     
		    (*This may need to be changed to handler parallel lets separately. 
		     * It's not clear what the semantics should be.
		     *)
		    | (Let_c (letsort, cbnds, body)) => 
		     let
		       fun folder(cbnd,state) = 
			 let 
			   val (cbnds,state) = rewrite_cbnd state cbnd
			 in  (cbnds, state)
			 end
		       val (cbnds_list,state) = foldl_acc folder state cbnds
		       val cbnds = List.concat cbnds_list
		       val body = rewrite_con state body
		     in
		       Let_c (letsort, cbnds, body)
		     end
		    | Typeof_c exp => Typeof_c (rewrite_exp state exp)
		    | (Closure_c (code,env)) =>
		     let
		       val code = recur code
		       val env = recur env
		     in
		       Closure_c(code, env)
		     end
		   
		    | (Crecord_c entries) =>
		     let
		       val entries = map_second recur entries
		     in
		       Crecord_c entries
		     end
		   
		    | (Proj_c (con,lbl)) =>
		     let
		       val con = recur con
		     in
		       Proj_c (con, lbl)
		     end
		   
		    | (App_c (cfun,actuals)) =>
		     let
		       val cfun = recur cfun
		       val actuals = map recur actuals
		     in
		       App_c (cfun, actuals)
		     end
		   
		    | Typecase_c {arg, arms, default, kind} => 
		     let 
		       fun doarm(pc,vklist,body) =   
			 let 
			   val (vklist,state) = tformals_helper state vklist
			   val body = rewrite_con state body
			 in  (pc, vklist, body)
			 end
		       val arg = recur arg
		       val arms = map doarm arms
		       val default = recur default
		       val kind = rewrite_kind state kind
		     in  Typecase_c{arg = arg,
				    arms = arms,
				    default = default,
				    kind = kind}
		     end
		   
		    | (Annotate_c (annot,con)) => 
		     let
		       val con = recur con
		     in
		       Annotate_c (annot, con)
		     end)
	      end
	  in
	    case (conhandler (state,con)) 
	      of CHANGE_NORECURSE (state,c) => c
	       | CHANGE_RECURSE value => docon value
	       | NOCHANGE => docon (state,con)
	       | NORECURSE => con
	  end
	

	and rewrite_kind (state : 'state) (kind : kind) : kind = 
	  let 
	      
	    fun dokind (state,kind) = 
	      (case kind 
		 of Type_k => kind
		  | (Singleton_k con) => Singleton_k(rewrite_con state con)
		  | (Record_k fieldseq) =>
		   let
		     fun fold_one (((lbl,var),kind),state) = 
		       let
			 val kind  = rewrite_kind state kind
			 val (state,var) = con_var_bind (state,var,kind)
		       in
			 (((lbl, var), kind),state)
		       end
		     val (fieldseq,state) = Sequence.foldl_acc fold_one state fieldseq
		   in
		     Record_k fieldseq
		   end
		 
		  | (Arrow_k (openness, args, result)) =>
		   let
		     val (args, state) = tformals_helper state args
		     val result = rewrite_kind state result
		   in
		     Arrow_k (openness, args, result)
		   end)

	  in		 
	    (case (kindhandler (state,kind)) of
	       CHANGE_NORECURSE (state,k) => k
	     | CHANGE_RECURSE value => dokind value
	     | NOCHANGE => dokind (state,kind)
	     | NORECURSE => kind)
	  end
	
	and tformals_helper (state : 'state) (vklist : (var * kind) list) : (var * kind) list * 'state = 
	  let
	
	    fun bind ((var,knd),state) = 
	      let
		val knd = rewrite_kind state knd
		val (state,var) = con_var_bind (state, var,knd)
	      in
		((var,knd),state)
	      end
	  in
	    foldl_acc bind state vklist
	  end

	and fun_helper (state : 'state) (Function(effect,recur,vklist,dependent,vclist,vflist,body,con) : function) : function = 
	  let 
	    val (vklist,state1) = tformals_helper state vklist
	      
	    fun vcfolder((v,c),s) = 
	      let 
		val c' = rewrite_con s c
		val (s,v) = exp_var_bind(s,v,c') 
	      in  ((v,c'),s)
	      end
	    val (vclist, state2) = foldl_acc vcfolder state1 vclist
	    val ftype = Prim_c (Float_c Prim.F64,[])
	    fun folder (v,s) = let val (s,v) = exp_var_bind(s,v,ftype) in (v,s) end
	    val (vflist,state2) = foldl_acc folder state2 vflist
	    val con = if dependent 
			then rewrite_con state2 con
		      else rewrite_con state1 con
	    val body = rewrite_exp state2 body
	  in
	    Function(effect,recur,vklist,dependent,vclist,
		     vflist, body,
		     con)
	  end
	and rewrite_bnd (state : 'state) (bnd : bnd) : (bnd list * 'state) = 
	  let 
	    fun do_fix (recur,maker,vfset) = 
	      let 
		fun folder ((v,f),s) =
		  let 
		    val (s,v) = exp_var_define(s,v,Let_e (Sequential,[maker vfset],Var_e v))
		  in
		    ((v,f),s)
		  end
		val (vfset,s) = Sequence.foldl_acc folder state vfset
		fun doer(v,f) = (v,fun_helper s f)
		val vfset = if recur then (Sequence.map doer vfset) else vfset
	      in  
		([maker vfset],s)
	      end
	    fun do_bnd recur (bnd,state) : bnd list * 'state = 
	      (case bnd 
		 of Con_b(p,cb) => 
		   let val (cbnds,state) = 
		     if recur 
		       then rewrite_cbnd state cb
		     else ([cb],state)
		   in  (map (fn cb => Con_b(p,cb)) cbnds, state)
		   end
		  | Exp_b(v,trace,e) => 
		   let
		     val e = if recur then rewrite_exp state e else e
		     val trace = 
		       (case trace 
			  of TraceCompute var => 
			    (case rewrite_con state (Var_c var)
			       of Var_c var => TraceCompute var
				| _ => (lprintl "Warning - rewrite forgetting trace info";
					TraceUnknown))
			   | TraceKnown (TraceInfo.Compute (var,labels)) => 
			       (case rewrite_con state (Var_c var)
				  of Var_c var => TraceKnown (TraceInfo.Compute (var,labels))
				   | _ => (lprintl "Warning - rewrite forgetting trace info";
					   TraceUnknown))
			   | _ => trace)
		     val (state,v) = exp_var_define(state,v,e)
		   in
		     ([Exp_b(v,trace,e)], state)
		   end
		  | Fixopen_b vfset => do_fix (recur,Fixopen_b,vfset)
		  | Fixcode_b vfset => do_fix (recur,Fixcode_b,vfset)
		  | Fixclosure_b (is_recur,vcset) => 
		   let 
		     fun folder ((v,{tipe,cenv,venv,code}),s) =
		       let 
			 val bnd = Fixclosure_b(is_recur,vcset)
			 val (s,v) = exp_var_define(s,v,Let_e (Sequential,[bnd],Var_e v))
		       in
			 ((v,{tipe=tipe,cenv=cenv,venv=venv,code=code}),s)
		       end

		     val (vcset,s) = Sequence.foldl_acc folder state vcset

		     fun doer s (v,{code,cenv,venv,tipe}) = 
		       (v,{code = (case (exphandler (state,Var_e code)) of
				     NOCHANGE => code
				   | NORECURSE => code
				   | (CHANGE_RECURSE (state,Var_e v')) => v'
				   | (CHANGE_NORECURSE (state,Var_e v')) => v'
				   | _ => error "can't have non-var in closure code comp"),
			   cenv = rewrite_con s cenv,
			   venv = rewrite_exp s venv,
			   tipe = rewrite_con s tipe})
		     val vcset = 
		       if recur 
			 then if is_recur then Sequence.map (doer s) vcset 
			      else Sequence.map (doer state) vcset
		       else vcset
		   in  ([Fixclosure_b(is_recur,vcset)], s)
		   end)
	  in
	    (case (bndhandler (state,bnd)) 
	       of CHANGE_NORECURSE (state,bs) => 
		 let val (bndll, state) = foldl_acc (do_bnd false) state bs
		 in (List.concat bndll,state) end
		| CHANGE_RECURSE (state,bs) => 
		   let val (bndll, state) = foldl_acc (do_bnd true) state bs
		   in (List.concat bndll,state) end
		| NOCHANGE => 
		     let val (bndl,state) = do_bnd true (bnd,state)
		     in (bndl,state) end
		| NORECURSE => ([bnd],state))
	  end

	and switch_helper (state : 'state) (sw : switch) : switch = 
	  (case sw of
	     Intsw_e {arg, size, arms, default} =>
	       Intsw_e {arg = rewrite_exp state arg,
			size = size, 
			arms = map (fn (t,e) => (t,rewrite_exp state e)) arms,
			default = Util.mapopt (rewrite_exp state) default}
	   | Sumsw_e {arg, sumtype, bound, arms, default} =>
	       let
		 val sumtype = rewrite_con state sumtype 
		 val (state',bound) = exp_var_bind(state,bound,sumtype)
	       in  
		 Sumsw_e {arg = rewrite_exp state arg,
			  sumtype = sumtype,
			  bound = bound,
			  arms = map (fn (t,e) => (t,rewrite_exp state' e)) arms,
			  default = Util.mapopt (rewrite_exp state) default}
	       end
	   | Exncase_e {arg, bound, arms, default} =>
	       let 
		 val (state',bound) = exp_var_bind(state,bound,Prim_c(Exn_c,[]))
	       in  Exncase_e {arg = rewrite_exp state arg,
			      bound = bound,
			      arms = map (fn (t,e) => (rewrite_exp state t,rewrite_exp state' e)) arms,
			      default = Util.mapopt (rewrite_exp state) default}
	       end
	   | Typecase_e {arg,arms,default} => error "typecase not handled")    

	and rewrite_exp (state : 'state) (exp : exp) : exp =
	  let 
	    fun doexp (state,e) = 
	      let
		val recur = rewrite_exp state
	      in
		(case e of
		   (Var_e _) => e
		 | (Const_e v) => 	
		     Const_e 
		     (case v of
			(Prim.int _) => v
		      | (Prim.uint _) => v
		      | (Prim.float _) => v
		      | (Prim.array (c,array)) =>
			  (Array.modify recur array;
			   Prim.array(rewrite_con state c,array))
		      | (Prim.vector (c,array)) =>
			  (Array.modify recur array;
			   Prim.vector(rewrite_con state c,array))
		      | Prim.refcell (r as (ref e)) => (r := recur e; v)
		      | Prim.tag (t,c) => Prim.tag(t,rewrite_con state c))
		 | (Let_e (sort,bnds,body)) => 
		    let 
		      fun folder (bnd,s) = 
			let 
			  val (bnds,s) = rewrite_bnd s bnd
			in  (bnds,s)
			end
		      val (bndslist,state) = foldl_acc folder state bnds
		      val bnds = List.concat bndslist
		      val body = rewrite_exp state body
		    in Let_e(sort,bnds,body)
		    end
		 | (Prim_e (ap,clist,elist)) => Prim_e(ap,map (rewrite_con state) clist, map recur elist)
		 | (Switch_e switch) => Switch_e(switch_helper state switch)
		 | (App_e (openness,func,clist,elist,eflist)) => 
			App_e(openness,
			      recur func,
			      map (rewrite_con state) clist,
			      map recur elist, 
			      map recur eflist)
		 | ExternApp_e (exp,args) =>
		   let
		     val exp = recur exp
		     val args = map recur args
		   in
		     ExternApp_e (exp,args)
		   end
		 | Raise_e (e,c) => Raise_e(recur e, rewrite_con state c)
		 | Handle_e (e,v,h) => 
			let 
			  val e = recur e
			  val (state,v) = exp_var_bind(state,v,Prim_c(Exn_c,[]))
			in  Handle_e(e, v, rewrite_exp state h)
			end)
	      end
    
	  in
      	    (case (exphandler (state,exp))
	       of CHANGE_NORECURSE (state,e) => e
		| CHANGE_RECURSE value => doexp value
		| NOCHANGE => doexp (state,exp)
		| NORECURSE => exp)
	  end


	fun import_helper (ImportValue (label,var,con),state) =
	  let
	    val con = rewrite_con state con
	    val (state,var) = exp_var_bind(state,var,con)
	  in
	    (ImportValue (label,var,con),state)
	  end
	  | import_helper (ImportType (label,var,kind),state) = 
	  let
	    val kind = rewrite_kind state kind
	    val (state,var) = con_var_bind(state,var,kind)
	  in
	    (ImportType (label,var,kind),state)
	  end
	
	fun export_helper state (ExportValue (label,var)) = 
	  let
	    val var = 
	      (case rewrite_exp state (Var_e var)
		 of Var_e var => var
		  | _ => error "Export value rewritten to non variable! Don't know what to do!")
	  in  ExportValue (label,var)
	  end
	  | export_helper state (ExportType (label,var)) = 
	  let
	    val var = 
	      (case rewrite_con state (Var_c var)
		 of Var_c var => var
		  | _ => error "Export type rewritten to non variable! Don't know what to do!")
	  in ExportType (label,var)
	  end

      fun rewrite_mod (state : 'state) (module : module) : module =
	let
	  val (MODULE {bnds,imports,exports}) = module
	  val (imports,state) = foldl_acc import_helper state imports
	  fun folder (bnd,s) = rewrite_bnd s bnd
	  val (bndslist,state) = foldl_acc folder state bnds
	  val bnds = List.concat bndslist
	  val exports = map (export_helper state) exports
	in
	  MODULE {bnds=bnds,imports=imports,exports=exports}
	end

      in
	{
	 rewrite_kind = rewrite_kind,
	 rewrite_con = rewrite_con,
	 rewrite_exp = rewrite_exp,
	 rewrite_bnd = rewrite_bnd,
	 rewrite_cbnd = rewrite_cbnd,
	 rewrite_mod = rewrite_mod
	 }
      end


      fun null_binder (state,var,_) = (state,var)

      fun default_handler _ = NOCHANGE

      val default_handler =  
	HANDLER {
		 bndhandler = default_handler,
		 cbndhandler = default_handler,
		 conhandler = default_handler,
		 exphandler = default_handler,
		 kindhandler = default_handler,
		 con_var_bind = null_binder,
		 exp_var_bind = null_binder,
		 con_var_define = null_binder,
		 exp_var_define = null_binder
		 }

      fun set_conhandler (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_conhandler = 
	HANDLER {
		 bndhandler = bndhandler,
		 cbndhandler = cbndhandler,
		 conhandler = new_conhandler,
		 exphandler = exphandler,
		 kindhandler = kindhandler,
		 con_var_bind = con_var_bind,
		 exp_var_bind = exp_var_bind,
		 con_var_define = con_var_define,
		 exp_var_define = exp_var_define
		 }

      fun set_exphandler (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_exphandler = 
	HANDLER {
		 bndhandler = bndhandler,
		 cbndhandler = cbndhandler,
		 conhandler = conhandler,
		 exphandler = new_exphandler,
		 kindhandler = kindhandler,
		 con_var_bind = con_var_bind,
		 exp_var_bind = exp_var_bind,
		 con_var_define = con_var_define,
		 exp_var_define = exp_var_define
		 }

      fun set_exp_binder (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_exp_var_bind = 
	HANDLER {
		 bndhandler = bndhandler,
		 cbndhandler = cbndhandler,
		 conhandler = conhandler,
		 exphandler = exphandler,
		 kindhandler = kindhandler,
		 con_var_bind = con_var_bind,
		 exp_var_bind = new_exp_var_bind,
		 con_var_define = con_var_define,
		 exp_var_define = exp_var_define
		 }

      fun set_exp_definer (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_exp_var_define = 
	HANDLER {
		 bndhandler = bndhandler,
		 cbndhandler = cbndhandler,
		 conhandler = conhandler,
		 exphandler = exphandler,
		 kindhandler = kindhandler,
		 con_var_bind = con_var_bind,
		 exp_var_bind = exp_var_bind,
		 con_var_define = con_var_define,
		 exp_var_define = new_exp_var_define
		 }

      fun set_con_binder (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_con_var_bind = 
	HANDLER {
		 bndhandler = bndhandler,
		 cbndhandler = cbndhandler,
		 conhandler = conhandler,
		 exphandler = exphandler,
		 kindhandler = kindhandler,
		 con_var_bind = new_con_var_bind,
		 exp_var_bind = exp_var_bind,
		 con_var_define = con_var_define,
		 exp_var_define = exp_var_define
		 }

      fun set_con_definer (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_con_var_define = 
	HANDLER {
		 bndhandler = bndhandler,
		 cbndhandler = cbndhandler,
		 conhandler = conhandler,
		 exphandler = exphandler,
		 kindhandler = kindhandler,
		 con_var_bind = con_var_bind,
		 exp_var_bind = exp_var_bind,
		 con_var_define = new_con_var_define,
		 exp_var_define = exp_var_define
		 }
  end