(*$import Nil NILREWRITE *)
structure NilRewrite :> NILREWRITE = 
  struct

    open Curtain
    open Nil 
      
    val foldl_acc = Listops.foldl_acc
    val foldl_acc2 = Listops.foldl_acc2
    val map_second = Listops.map_second
    val mapopt = Util.mapopt
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
		  tracehandler : 'state * niltrace -> ('state * niltrace) changeopt,
		  con_var_bind : 'state * var * kind -> ('state * var option),
		  con_var_define : 'state * var * con -> ('state * var option),
		  exp_var_bind : 'state * var * con -> ('state * var option),
		  exp_var_define : 'state * var * exp -> ('state * var option)
		  }

    fun rewriters (handler : 'state handler) 
      : {
	 rewrite_kind  : 'state -> Nil.kind -> Nil.kind,
	 rewrite_con   : 'state -> Nil.con -> Nil.con,
	 rewrite_exp   : 'state -> Nil.exp -> Nil.exp,
	 rewrite_bnd   : 'state -> Nil.bnd -> (Nil.bnd list * 'state),
	 rewrite_cbnd  : 'state -> Nil.conbnd -> (Nil.conbnd list * 'state),
	 rewrite_trace : 'state -> Nil.niltrace -> Nil.niltrace,
	 rewrite_mod   : 'state -> Nil.module -> Nil.module
	 }
      =
      let

	val (HANDLER{bndhandler,
		     exphandler,
		     cbndhandler,
		     conhandler,
		     kindhandler,
		     tracehandler,
		     con_var_bind,
		     con_var_define,
		     exp_var_bind,
		     exp_var_define}) = handler

	fun map_f f flag state list = 
	  let val changed = ref false
	    val temp = map (f changed state) list 
	    val _ = flag := (!flag orelse !changed)
	  in if !changed then temp else list
	  end
	
	fun foldl_acc_f f flag state list = 
	  let
	    val changed = ref false
	    val (temp,state) = foldl_acc (f changed) state list
	    val _ = flag := (!flag orelse !changed)
	  in if !changed then (temp,state) else (list,state)
	  end

	fun define_e changed (s,v,e) = 
	  (case exp_var_define(s,v,e)
	     of (s,SOME v) => (changed := true;(s,v))
	      | (s,NONE) => (s,v))

	fun bind_e changed (s,v,c) = 
	  (case exp_var_bind(s,v,c)
	     of (s,SOME v) => (changed := true;(s,v))
	      | (s,NONE) => (s,v))

	fun define_c changed (s,v,c) = 
	  (case con_var_define(s,v,c)
	     of (s,SOME v) => (changed := true;(s,v))
	      | (s,NONE) => (s,v))

	fun bind_c changed (s,v,k) = 
	  (case con_var_bind(s,v,k)
	     of (s,SOME v) => (changed := true;(s,v))
	      | (s,NONE) => (s,v))

	(*If this becomes a performance critical piece of code,
	 * it may be useful to bind these locally to encourage inlining
	 *)
	fun recur_e flag state exp = 
	  (case rewrite_exp state exp
	     of SOME exp => (flag := true;exp)
	      | NONE => exp)
	      
	and recur_c flag state con = 
	  (case rewrite_con state con
	     of SOME con => (flag := true;con)
	      | NONE => con)
		  
	and recur_k flag state kind = 
	  (case rewrite_kind state kind
	     of SOME kind => (flag := true;kind)
	      | NONE => kind)
	and recur_trace flag state trace = 
	  (case rewrite_trace state trace
	     of SOME trace => (flag := true;trace)
	      | NONE => trace)

	and rewrite_cbnd (state : 'state) (cbnd : conbnd) : (conbnd list option * 'state) =
	  let

	    fun define wrap (var,vklist,c) state = 
	      let
		val var' = Name.derived_var var
		val con = C.Let_c (Sequential,[wrap (var',vklist,c)],C.Var_c var')
		val (state,varopt) = con_var_define(state,var,con)
	      in 
		case varopt 
		  of SOME var => (SOME (wrap(var, vklist, c)), state)
		   | NONE => (NONE,state)
	      end

	    fun cbnd_recur wrap (var, vklist, c) oldstate = 
	      let 

		fun folder changed ((v,k),state) = 
		  let 
		    val k = recur_k changed state k
		    val (state,v) = bind_c changed (state,v,k)
		  in  ((v,k),state)
		  end
		val changed = ref false
		val (vklist,state) = foldl_acc_f folder changed state vklist
		val c = recur_c changed state c
	      in
		case define wrap (var,vklist,c) oldstate
		  of (SOME bnd,state) => (SOME bnd,state)
		   | (NONE,state) => 
		    if !changed then 
		      (SOME (wrap (var,vklist,c)),state)
		    else
		      (NONE,state)
	      end
	    
	    fun do_cbnd recur (cbnd,state) = 
	      (case cbnd 
		 of Con_cb(var, con) =>
		   let 
		     val changed = ref false
		     val con = 
		       if recur then 
			 recur_c changed state con
		       else con
		     val (state,var) = define_c changed(state,var,con)
		   in if !changed then (SOME (Con_cb(var, con)), state) else (NONE,state)
		   end
		  | Open_cb args =>
		   (if recur then
		      cbnd_recur Open_cb args state 
		    else 
		      define Open_cb args state)
		  | Code_cb args => 
		      (if recur then
			 cbnd_recur Code_cb args state 
		       else 
			 define Code_cb args state))

	    fun do_cbnds recur state cbnds = 
	      let
		val changed = ref false
		fun doit (cbnd,state) = 
		  (case do_cbnd recur (cbnd,state)
		     of (SOME cbnd,state) => (changed := true;(cbnd,state))
		      | (NONE,state) => (cbnd,state))
		val (cbnds,state) = foldl_acc doit state cbnds
	      in (if !changed then SOME cbnds else NONE,state)
	      end
	  in
	    (case (cbndhandler (state,cbnd)) 
	       of CHANGE_NORECURSE (state,cbnds) => do_cbnds false state cbnds
		| CHANGE_RECURSE (state,cbnds) => do_cbnds true state cbnds
		| NOCHANGE => 
		 (case do_cbnd true (cbnd,state)
		    of (SOME cb,s) => (SOME [cb],s)
		     | (NONE,s) => (NONE,s))
		| NORECURSE => (NONE,state))
	  end  
	
	and rewrite_con (state : 'state) (con : con) : con option =  
	  let 
	    fun docon (state,con) = 
	      let
		val res = 
		(case C.expose con 
		   of (Prim_c (pcon,args)) => 
		     let
		       val changed = ref false
		       val args = map_f recur_c changed state args
		     in if !changed then SOME (C.Prim_c (pcon,args)) else NONE
		     end
		     
		    | (Mu_c (flag,defs)) =>
		     let
		       val changed = ref false
		       fun folder ((v,c),state) = 
			 let
			   val (state,v) = bind_c changed (state,v,K.Type_k)
			 in
			   ((v,c),state)
			 end
		       val (defs,state) = 
			 if flag then 
			   let val (temp,state) = Sequence.foldl_acc folder state defs 
			   in if !changed then (temp,state) else (defs,state)
			   end 
			 else (defs,state)
		       val defs = Sequence.map_second (recur_c changed state) defs
		     in  if !changed then SOME (C.Mu_c (flag,defs)) else NONE
		     end
		   
		    | (AllArrow_c {openness, effect, isDependent, tFormals, 
				   eFormals, fFormals, body_type}) =>
		     let
		       val changed = ref false

		       val (tFormals,state) = tformals_helper changed state tFormals
		       val (eFormals,state) = 
			   let
			       fun efolder((vopt,c),s) = 
				   let 
				       val c = recur_c changed state c
				       val (vopt,s) = 
					   (case vopt of
						NONE => (vopt, s)
					      | SOME v => let val (s,v) = bind_e changed (s,v,c) 
							  in  (SOME v, s)
							  end)
				   in  ((vopt,c),s)
				   end
			       val (new_eFormals,state) = foldl_acc efolder state eFormals
			       val eFormals = if !changed then new_eFormals else eFormals
			   in  (eFormals, state)
			   end

		       val body_type = recur_c changed state body_type
		     in
		       if !changed
			 then SOME (C.AllArrow_c{openness = openness, effect = effect, isDependent = isDependent,
					       tFormals = tFormals, eFormals = eFormals, 
					       fFormals = fFormals, body_type = body_type})
		       else NONE
		     end
		     
		    | ExternArrow_c (cons,con) =>
		     let
		       val changed = ref false
		       val cons = map_f recur_c changed state cons
		       val con = recur_c changed state con
		     in if !changed then SOME (C.ExternArrow_c (cons,con)) else NONE
		     end
		    | (Var_c var) => NONE
		     
		    (*This may need to be changed to handler parallel lets separately. 
		     * It's not clear what the semantics should be.
		     *)
		    | (Let_c (letsort, cbnds, body)) => 
		     let
		       val changed = ref false
		       fun folder(cbnd,state) = 
			 let 
			   val (cbnds,state) = 
			     (case rewrite_cbnd state cbnd
				of (SOME cbnds,state) => (changed := true;(cbnds,state))
				 | (NONE,state) => ([cbnd],state))
			 in  (cbnds, state)
			 end
		       val (cbnds_list,state) = foldl_acc folder state cbnds
		       val cbnds = if !changed then List.concat cbnds_list else cbnds
		       val body = recur_c changed state body
		     in if !changed then SOME (C.Let_c (letsort, cbnds, body)) else NONE
		     end
		    | Typeof_c exp => mapopt C.Typeof_c (rewrite_exp state exp)
		    | (Closure_c (code,env)) =>
		     let
		       val changed = ref false
		       val code = recur_c changed state code
		       val env = recur_c changed state env
		     in if !changed then SOME (C.Closure_c(code, env)) else NONE
		     end
		   
		    | (Crecord_c entries) =>
		     let
		       val changed = ref false
		       val entries = map_second (recur_c changed state) entries
		     in if !changed then SOME (C.Crecord_c entries) else NONE
		     end
		   
		    | (Proj_c (con,lbl)) => 
		     (case rewrite_con state con
			of SOME con => SOME (C.Proj_c (con,lbl))
			 | NONE => NONE)
		    | (App_c (cfun,actuals)) =>
		     let
		       val changed = ref false
		       val cfun = recur_c changed state cfun
		       val actuals = map_f recur_c changed state actuals
		     in if !changed then SOME (C.App_c (cfun, actuals)) else NONE
		     end
		   
		    | Typecase_c {arg, arms, default, kind} => 
		     let 
		       val changed = ref false
		       fun doarm(pc,vklist,body) =   
			 let 
			   val (vklist,state) = tformals_helper changed state vklist
			   val body = recur_c changed state body
			 in  (pc, vklist, body)
			 end
		       val arg = recur_c changed state arg
		       val arms = map doarm arms
		       val default = recur_c changed state default
		       val kind = recur_k changed state kind
		     in  
		       if !changed then
			 SOME (C.Typecase_c{arg = arg,
					    arms = arms,
					    default = default,
					    kind = kind})
		       else NONE
		     end)
	      in res
	      end
	  in
	    case (conhandler (state,con)) 
	      of CHANGE_NORECURSE (state,c) => SOME c
	       | CHANGE_RECURSE value => docon value
	       | NOCHANGE => docon (state,con)
	       | NORECURSE => NONE
	  end
	

	and rewrite_kind (state : 'state) (kind : kind) : kind option = 
	  let 
	    fun dokind (state,kind) = 
	      let
		val res = 
		  (case K.expose kind 
		     of Type_k => NONE
		      | (SingleType_k con) => mapopt K.SingleType_k (rewrite_con state con)
		      | (Single_k con) => mapopt K.Single_k (rewrite_con state con)
		      | (Record_k fieldseq) =>
		       let
			 val changed = ref false
			 fun fold_one (((lbl,var),kind),state) = 
			   let
			     val kind  = recur_k changed state kind
			     val (state,var) = bind_c changed (state,var,kind)
			   in
			     (((lbl, var), kind),state)
			   end
			 val (fieldseq,state) = Sequence.foldl_acc fold_one state fieldseq
		       in if !changed then SOME(K.Record_k fieldseq) else NONE
		       end
		     
		      | (Arrow_k (openness, args, result)) =>
		       let
			 val changed = ref false
			 val (args, state) = tformals_helper changed state args
			 val result = recur_k changed state result
		       in if !changed then SOME(K.Arrow_k (openness, args, result)) else NONE
		       end)
	      in res
	      end

	  in		 
	    (case (kindhandler (state,kind)) of
	       CHANGE_NORECURSE (state,k) => SOME k
	     | CHANGE_RECURSE value => dokind value
	     | NOCHANGE => dokind (state,kind)
	     | NORECURSE => NONE)
	  end
	
	and tformals_helper (flag : bool ref) (state : 'state) (vklist : (var * kind) list) : (var * kind) list * 'state = 
	  let
	    fun bind changed ((var,knd),state) = 
	      let
		val knd = 
		  (case rewrite_kind state knd
		     of SOME knd => (changed := true;knd)
		      | NONE => knd)
		val (state,var) = bind_c changed (state, var,knd)
	      in
		((var,knd),state)
	      end

	    val (vklist,state) = foldl_acc_f bind flag state vklist
	  in (vklist,state)
	  end

	and fun_helper (state : 'state) (Function{effect, recursive, isDependent,
						  tFormals, eFormals, fFormals,
						  body, body_type}) : function option = 
	  let 
	    val changed = ref false
	    val (tFormals,state1) = tformals_helper changed state tFormals
	    local
	      fun vcfolder changed ((v,trace,c),s) = 
		let 
		  val c' = recur_c changed s c
		  val trace = recur_trace changed state trace
		  val (s,v) = bind_e changed (s,v,c') 
		in  ((v,trace,c'),s)
		end
	    in
	      val (eFormals, state2) = foldl_acc_f vcfolder changed state1 eFormals
	    end
	    val ftype = C.Prim_c (Float_c Prim.F64,[])
	    fun folder changed (v,s) = let val (s,v) = bind_e changed (s,v,ftype) in (v,s) end
	    val (fFormals,state2) = foldl_acc_f folder changed state2 fFormals
	    val body_type = recur_c changed (if isDependent then state2 else state1) body_type
	    val body = recur_e changed state2 body
	  in
	    if !changed then
	      SOME (Function({effect = effect , recursive = recursive, isDependent = isDependent,
			      tFormals = tFormals, eFormals = eFormals, fFormals = fFormals,
			      body = body, body_type = body_type}))
	    else NONE
	  end

	and rewrite_bnd (state : 'state) (bnd : bnd) : (bnd list option * 'state) = 
	  let 
	    fun do_fix (recur,maker,vfset) = 
	      let 
		val changed = ref false
		fun folder ((v,f),s) =
		  let 
		    val (s,v) = define_e changed (s,v,E.Let_e (Sequential,[maker vfset],E.Var_e v))
		  in
		    ((v,f),s)
		  end
		val (vfset,s) = 
		  let val (temp,s) = Sequence.foldl_acc folder state vfset
		  in if !changed then (temp,s) else (vfset,s)
		  end
		fun doer changed (v,f) = 
		  (v,case fun_helper s f
		       of SOME f => (changed := true;f)
			| NONE => f)
		val vfset =  
		  if recur then 
		    let val flag = ref false 
		      val temp = (Sequence.map (doer flag) vfset)
		      val _ = changed := (!flag orelse !changed)
		    in if !flag then temp else vfset
		    end
		  else vfset
	      in  
		(if !changed then SOME [maker vfset] else NONE,s)
	      end
	  
	    fun do_bnd recur (bnd,state) : bnd list option * 'state = 
	      (case bnd 
		 of Con_b(p,cb) => 
		   let val (cb_opt,state) = 
		     if recur 
		       then rewrite_cbnd state cb
		     else (NONE,state)
		   in  (mapopt (fn cbnds => map (fn cb => Con_b(p,cb)) cbnds) cb_opt, state)
		   end
		  | Exp_b(v,trace,e) => 
		   let
		     val changed = ref false
		     val e = if recur then recur_e changed state e else e
		     val trace = recur_trace changed state trace 
		     val (state,v) = define_e changed (state,v,e)
		   in
		     (if !changed then SOME [Exp_b(v,trace,e)] else NONE, state)
		   end
		  | Fixopen_b vfset => do_fix (recur,Fixopen_b,vfset)
		  | Fixcode_b vfset => do_fix (recur,Fixcode_b,vfset)
		  | Fixclosure_b (is_recur,vcset) => 
		   let 
		     val changed = ref false
		     fun folder ((v,{tipe,cenv,venv,code}),s) =
		       let 
			 val bnd = Fixclosure_b(is_recur,vcset)
			 val (s,v) = define_e changed (s,v,E.Let_e (Sequential,[bnd],E.Var_e v))
		       in
			 ((v,{tipe=tipe,cenv=cenv,venv=venv,code=code}),s)
		       end

		     val (vcset,s) = Sequence.foldl_acc folder state vcset

		     fun doer flag s (arg as (v,{code,cenv,venv,tipe})) = 
		       let 
			 val changed = ref false
			 fun do_change e = 
			   (case E.expose e
			      of Var_e v' => (changed := true;v')
			       | _ => error "can't have non-var in closure code comp")

			 val code = (case (exphandler (state,E.Var_e code))
				       of NOCHANGE => code
					| NORECURSE => code
					| (CHANGE_RECURSE (state,e))   => do_change e
					| (CHANGE_NORECURSE (state,e)) => do_change e)
			   
			   val cenv = recur_c changed s cenv
			   val venv = recur_e changed s venv
			   val tipe = recur_c changed s tipe
			   val _ = flag := (!flag orelse !changed)
		       in
			 if !changed then
			   (v,{code=code,cenv=cenv,venv=venv,tipe=tipe})
			 else arg
		       end
		     val vcset = 
		       if recur 
			 then let val flag = ref false 
				  val temp = if is_recur then Sequence.map (doer flag s) vcset 
					     else Sequence.map (doer flag state) vcset
				  val _ = changed := (!changed orelse !flag)
			      in if !flag then temp else vcset
			      end
		       else vcset
		   in  (if !changed then SOME [Fixclosure_b(is_recur,vcset)] else NONE, s)
		   end)

	    fun do_bnds recur (state,bs) = 
	      let
		val changed = ref false
		fun do_bnd' (bnd,state) = 
		  case do_bnd recur (bnd,state)
		    of (SOME bnds,state) => (changed := true;(bnds,state))
		     | (NONE,state) => ([bnd],state)
		val (bnds,state) = foldl_acc do_bnd' state bs
	      in
		(if !changed then SOME (List.concat bnds) else NONE,state)
	      end
	  in
	    (case (bndhandler (state,bnd)) 
	       of CHANGE_NORECURSE (state,bs) => do_bnds false (state,bs)
		| CHANGE_RECURSE (state,bs) => do_bnds true (state,bs)
		| NOCHANGE => do_bnd true (bnd,state)
		| NORECURSE => (NONE,state))
	  end

	and switch_helper (state : 'state) (sw : switch) : exp option = 
	  (case sw of
	     Intsw_e {arg, size, arms, default, result_type} =>
	       let
		 val changed = ref false
		 val arg = recur_e changed state arg
		 val result_type = recur_c changed state result_type
		 fun recur changed state (t,e) = (t,recur_e changed state e)
		 val arms = map_f recur changed state arms
		 val default = Util.mapopt (recur_e changed state) default
	       in
		 if !changed then
		   SOME (E.Switch_e 
			 (Intsw_e {arg = arg,
				   size = size, 
				   arms = arms,
				   default = default,
				   result_type = result_type}))
		 else NONE
	       end
	   | Sumsw_e {arg, sumtype, bound, arms, default, result_type} =>
	       let
		 val changed = ref false
		 val arg = recur_e changed state arg
		 val sumtype = recur_c changed state sumtype 
		 val (state',bound) = bind_e changed (state,bound,sumtype)
		 val result_type = recur_c changed state result_type
		 fun recur changed state (t,tr,e) = (t,recur_trace changed state tr,recur_e changed state e)
		 val arms = map_f recur changed state' arms
		 val default = Util.mapopt (recur_e changed state) default
	       in  
		 if !changed then
		   SOME (E.Switch_e
			 (Sumsw_e {arg = arg,
				   sumtype = sumtype,
				   bound = bound,
				   arms = arms,
				   default = default,
				   result_type = result_type}))
		 else NONE
	       end
	   | Exncase_e {arg, bound, arms, default, result_type} =>
	       let
		 val changed = ref false
		 val arg = recur_e changed state arg
		 val (state',bound) = bind_e changed (state,bound,C.Prim_c(Exn_c,[]))
		 val result_type = recur_c changed state result_type
		 fun recur changed state (t,tr,e) = (recur_e changed state t, recur_trace changed state tr,
						     recur_e changed state' e)
		 val arms = map_f recur changed state arms
		 val default = Util.mapopt (recur_e changed state) default
	       in  
		 if !changed then 
		   SOME (E.Switch_e
			 (Exncase_e {arg = arg,
				     bound = bound,
				     arms = arms,
				     default = default,
				     result_type = result_type}))
		 else NONE
	       end
	   | Typecase_e {arg,arms,default, result_type} => 
	       let 
		 val changed = ref false
		 fun doarm(pc,vklist,body) =   
		   let 
		     val (vklist,state) = tformals_helper changed state vklist
		     val body = recur_e changed state body
		   in  (pc, vklist, body)
		   end
		 val arg = recur_c changed state arg
		 val arms = map doarm arms
		 val default = recur_e changed state default
		 val result_type = recur_c changed state result_type
	       in  
		 if !changed then
		   SOME (E.Switch_e
			 (Typecase_e{arg = arg,
				     arms = arms,
				     default = default,
				     result_type = result_type}))
		 else NONE
	       end
	     )

	and rewrite_exp (state : 'state) (exp : exp) : exp option =
	  let 
	    fun doexp (state,e) = 
	      let
		val map_e = map_f recur_e
		val map_c = map_f recur_c
		val res = 
		(case E.expose e of
		   (Var_e _) => NONE
		 | (Const_e v) => 	
		     (case v of
			(Prim.int _) => NONE
		      | (Prim.uint _) => NONE
		      | (Prim.float _) => NONE
		      | (Prim.array (c,array)) =>
			  let
			    val changed = ref false
			    val _ = Array.modify (recur_e changed state) array
			    val c = recur_c changed state c
			  in
			    if !changed then
			      SOME (E.Const_e (Prim.array(c,array)))
			    else NONE
			  end
		      | (Prim.vector (c,array)) =>
			  let
			    val changed = ref false
			    val _ = Array.modify (recur_e changed state) array
			    val c = recur_c changed state c
			  in
			    if !changed then
			      SOME (E.Const_e (Prim.vector(c,array)))
			    else NONE
			  end
		      | Prim.refcell (r as (ref e)) => 
			  (case rewrite_exp state e
			     of SOME e => (r := e; SOME (E.Const_e v))
			      | NONE => NONE)
		      | Prim.tag (t,c) => 
			 (case rewrite_con state c
			    of SOME c => SOME (E.Const_e (Prim.tag(t,c)))
			     | NONE => NONE))
		 | (Let_e (sort,bnds,body)) => 
		    let 
		      val changed = ref false
		      fun folder (bnd,s) = 
			let 
			  val (bnds,s) = (case rewrite_bnd s bnd
					    of (SOME bnds,s) => (changed := true; (bnds,s))
					     | (NONE,s) => ([bnd],s))
			in  (bnds,s)
			end
		      val (bndslist,state) = foldl_acc folder state bnds
		      val bnds = if !changed then List.concat bndslist else bnds
		      val body = recur_e changed state body
		    in if !changed then SOME (E.Let_e(sort,bnds,body)) else NONE
		    end
		 | (Prim_e (ap,clist,elist)) => 
		    let
		      val changed = ref false
		      val clist = map_c changed state clist
		      val elist = map_e changed state elist
		    in
		      if !changed then
			SOME (E.Prim_e(ap,clist,elist))
		      else NONE
		    end
		 | (Switch_e switch) => switch_helper state switch
		 | (App_e (openness,func,clist,elist,eflist)) => 
		    let
		      val changed = ref false
		      val func = recur_e changed state func
		      val clist = map_c changed state clist
		      val elist = map_e changed state elist
		      val eflist = map_e changed state eflist
		    in 
		      if !changed
			then SOME (E.App_e(openness,func,clist,elist,eflist))
		      else NONE
		    end
		 | ExternApp_e (exp,args) =>
		    let
		      val changed = ref false
		      val exp = recur_e changed state exp
		      val args = map_e changed state args
		    in
		      if !changed then
			SOME (E.ExternApp_e (exp,args))
		      else NONE
		    end
		 | Raise_e (e,c) => 
		    let
		      val changed = ref false
		      val e = recur_e changed state e
		      val c = recur_c changed state c
		    in if !changed then SOME (E.Raise_e(e,c)) else NONE
		    end
		 | Handle_e {body,bound,handler,result_type} =>
		    let 
		      val changed = ref false
		      val body = recur_e changed state body
		      val result_type = recur_c changed state result_type
		      val (state,bound) = 
			  bind_e changed (state,bound,C.Prim_c(Exn_c,[]))
		      val handler = recur_e changed state handler
		    in if !changed then 
			SOME (E.Handle_e{body = body, bound = bound,
					 handler = handler, 
					 result_type = result_type})
		       else NONE
		    end)
	      in res 
	      end
	    
	  in
      	    (case (exphandler (state,exp))
	       of CHANGE_NORECURSE (state,e) => SOME e
		| CHANGE_RECURSE value => doexp value
		| NOCHANGE => doexp (state,exp)
		| NORECURSE => NONE)
	  end
	and rewrite_trace (state : 'state) (trace : niltrace) : niltrace option = 
	  let

	    fun loop c labs = 
	      (case C.expose c
		 of (Var_c v) => TraceKnown (TraceInfo.Compute (v,labs))
		  | (Proj_c (c,l)) => loop c (l::labs)
		  | _ => error "Non path returned from rewriting trace info")

	    fun do_trace (state,trace) =
	      (case trace of
		 TraceCompute var => 
		   let val changed = ref false
		       val trace = loop (recur_c changed state (C.Var_c var)) []
		   in
		     if !changed then SOME trace else NONE
		   end
	       | TraceKnown (TraceInfo.Compute (var,labels)) => 
		   let val changed = ref false
		       val trace = loop (recur_c changed state (C.Var_c var)) labels
		   in
		     if !changed then SOME trace else NONE
		   end
	       | _ => NONE)
	  in
      	    (case (tracehandler (state,trace))
	       of CHANGE_NORECURSE (state,t) => SOME t
		| CHANGE_RECURSE value => do_trace value
		| NOCHANGE => do_trace (state,trace)
		| NORECURSE => NONE)
	  end

	fun import_helper flag (import as (ImportValue (label,var,trace,con)),state) =
	  let
	    val changed = ref false
	    val trace = recur_trace changed state trace 
	    val con = recur_c changed state con
	    val (state,var) = bind_e changed (state,var,con)
	    val _ = flag := (!changed orelse !flag)
	  in (if !changed then ImportValue (label,var,trace,con) else import,state)
	  end
	  | import_helper flag (import as (ImportType (label,var,kind)),state) = 
	  let
	    val changed = ref false
	    val kind = recur_k changed state kind
	    val (state,var) = bind_c changed (state,var,kind)
	    val _ = flag := (!changed orelse !flag)
	  in (if !changed then ImportType (label,var,kind) else import,state)
	  end
	
	fun export_helper flag state (export as (ExportValue (label,var))) = 
	  (case rewrite_exp state (E.Var_e var)
		 of SOME e => (case E.expose e
				 of (Var_e var) => (flag := true;ExportValue (label,var))
				  | _ => error "Export value rewritten to non variable! Don't know what to do!")
		  | NONE => export)

	  | export_helper flag state (export as (ExportType (label,var))) = 
	   (case rewrite_con state (C.Var_c var)
	      of SOME c => (case C.expose c
			      of (Var_c var) => (flag := true;ExportType (label,var))
			       | _ => error "Export type rewritten to non variable! Don't know what to do!")
	       | NONE => export)


      fun rewrite_mod (state : 'state) (module : module) : module =
	let
	  val changed = ref false 
	  val (MODULE {bnds,imports,exports}) = module
	  val (imports,state) = foldl_acc_f import_helper changed state imports
	  local
	    val flag = ref false
	    fun folder (bnd,s) = 
	      (case rewrite_bnd s bnd
		 of (SOME bndslist,state) => (flag := true;(bndslist,state))
		  | (NONE,state) => ([bnd],state))
	    val (bndslist,state) = foldl_acc folder state bnds
	    val _ = changed := (!flag orelse !changed)
	  in
	    val state = state
	    val bnds = if !flag then List.concat bndslist else bnds
	  end
	  val exports = map_f export_helper changed state exports
	in if !changed then MODULE {bnds=bnds,imports=imports,exports=exports} else module
	end

      fun rewrite_item rewriter state item = 
	(case rewriter state item
	   of SOME item => item
	    | NONE => item)
       
      val rewrite_exp = rewrite_item rewrite_exp
      val rewrite_con = rewrite_item rewrite_con
      val rewrite_kind = rewrite_item rewrite_kind
      val rewrite_trace = rewrite_item rewrite_trace

      val rewrite_bnd = 
	(fn state => fn bnd => 
	 (case rewrite_bnd state bnd
	    of (SOME bnds,state) => (bnds,state)
	     | (NONE,state) => ([bnd],state)))

      val rewrite_cbnd = 
	(fn state => fn cbnd => 
	 (case rewrite_cbnd state cbnd
	    of (SOME cbnds,state) => (cbnds,state)
	     | (NONE,state) => ([cbnd],state)))

      in
	{
	 rewrite_kind  = rewrite_kind,
	 rewrite_con   = rewrite_con,
	 rewrite_exp   = rewrite_exp,
	 rewrite_bnd   = rewrite_bnd,
	 rewrite_cbnd  = rewrite_cbnd,
	 rewrite_trace = rewrite_trace,
	 rewrite_mod   = rewrite_mod
	 }
      end


      fun null_binder (state,var,_) = (state,NONE)

      fun default_handler _ = NOCHANGE

      val default_handler =  
	HANDLER {
		 bndhandler     = default_handler,
		 cbndhandler    = default_handler,
		 conhandler     = default_handler,
		 exphandler     = default_handler,
		 kindhandler    = default_handler,
		 tracehandler   = default_handler,
		 con_var_bind   = null_binder,
		 exp_var_bind   = null_binder,
		 con_var_define = null_binder,
		 exp_var_define = null_binder
		 }

      fun set_kindhandler (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,tracehandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_kindhandler = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = exphandler,
		 kindhandler    = new_kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = con_var_bind,
		 exp_var_bind   = exp_var_bind,
		 con_var_define = con_var_define,
		 exp_var_define = exp_var_define
		 }

      fun set_conhandler (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,tracehandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_conhandler = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = new_conhandler,
		 exphandler     = exphandler,
		 kindhandler    = kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = con_var_bind,
		 exp_var_bind   = exp_var_bind,
		 con_var_define = con_var_define,
		 exp_var_define = exp_var_define
		 }

      fun set_exphandler (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,tracehandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_exphandler = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = new_exphandler,
		 kindhandler    = kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = con_var_bind,
		 exp_var_bind   = exp_var_bind,
		 con_var_define = con_var_define,
		 exp_var_define = exp_var_define
		 }

      fun set_exp_binder (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,tracehandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_exp_var_bind = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = exphandler,
		 kindhandler    = kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = con_var_bind,
		 exp_var_bind   = new_exp_var_bind,
		 con_var_define = con_var_define,
		 exp_var_define = exp_var_define
		 }

      fun set_exp_definer (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,tracehandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_exp_var_define = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = exphandler,
		 kindhandler    = kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = con_var_bind,
		 exp_var_bind   = exp_var_bind,
		 con_var_define = con_var_define,
		 exp_var_define = new_exp_var_define
		 }

      fun set_con_binder (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,tracehandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_con_var_bind = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = exphandler,
		 kindhandler    = kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = new_con_var_bind,
		 exp_var_bind   = exp_var_bind,
		 con_var_define = con_var_define,
		 exp_var_define = exp_var_define
		 }

      fun set_con_definer (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,tracehandler,
				   con_var_bind,exp_var_bind,
				   con_var_define,exp_var_define }) new_con_var_define = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = exphandler,
		 kindhandler    = kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = con_var_bind,
		 exp_var_bind   = exp_var_bind,
		 con_var_define = new_con_var_define,
		 exp_var_define = exp_var_define
		 }
  end