(*$import Name List Sequence Prim Array TraceInfo Listops Util Nil NILREWRITE *)

(*
 * A simple rewriter that does not try to synthesize types.
 *
 * The idea is to provide a generic traversal algorithm that crawls over 
 * a parse tree in simple block structured fashion.  At every node, it
 * provides the client code with the current node under consideration 
 * and a current state.  The client then returns 
 * 
 * NOCHANGE        if the node was not changed, and should be
 *                 recursively traversed
 * NORECURSE       if the node was not changed, and should not be 
 *                 recursively traversed
 * CHANGE_RECURSE (state,term)
 *                 if the node was changed to "term" with new state "state",
 *                 and the new term should be recursively traversed
 * CHANGE_NORECURSE (state,term)
 *                 if the node was changed to "term" with new state "state",
 *                 and the new term should not be recursively traversed
 *
 * One benefit of using this code is that it tries extremely hard to preserve
 * physical sharing. 
 *)

structure NilRewrite :> NILREWRITE = 
  struct
    open Nil

    val exn_con = Prim_c(Exn_c, [])
      
    val foldl_acc = Listops.foldl_acc
    val foldl_acc2 = Listops.foldl_acc2
    val map_second = Listops.map_second
    val mapopt = Util.mapopt
    fun error s = Util.error "NilRewrite" s
    val lprintl = Util.lprintl

    datatype 'a changeopt = NOCHANGE | NORECURSE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a

    (* A term handler is a client function which takes a state and a term,
     * and possibly returns a new term and state.
     *
     * A bnd handler does the same for bnds.  For generality however,
     * the handler has the option of returning a list of bnds.
     * If one of the RECURSE options is returned, then the bnds will
     * be bound in the state returned.  Otherwise, they will not.
     *
     * A binder is a function which takes a variable and a state, and returns 
     * a new state and possibly a new variable.  
     *)

    type ('state,'term) termhandler = 'state * 'term -> ('state * 'term) changeopt
    type ('state, 'bnd) bndhandler  = 'state * 'bnd  -> ('state * 'bnd list) changeopt
    type 'state binder              = 'state * var   -> ('state * var option)

    datatype 'state handler =
      HANDLER of {
		  bndhandler : ('state,bnd) bndhandler,
		  cbndhandler : ('state,conbnd) bndhandler,
		  (*For cbnds, a return of CHANGE_NORECURSE (state,cbnds)
		   * will result in cbnds being bound in state before being returned.
		   *)

		  conhandler   : ('state,con) termhandler,
		  exphandler   : ('state,exp) termhandler,
		  kindhandler  : ('state,kind) termhandler,
		  tracehandler : ('state,niltrace) termhandler,
		  con_var_bind   : 'state binder,
		  exp_var_bind   : 'state binder,
		  labelled_var : 'state * Nil.label * Nil.var -> 'state
		  }

      
    (* There are two mechanisms used here, to reflect two different levels of 
     * physical sharing that we can preserve.
     *
     * First, each of the major rewrite functions returns an option.  This option 
     * will be NONE if the term being rewritten was not changed, and SOME v if 
     * the term was changed to v.  The idea is that if a rewriter returns NONE,
     * then we may simply use the original copy of the term.
     *
     * Sometimes however, we may have to make a new copy of a node because 
     * some part of it has changed.  We can still preserve some physical
     * sharing if only part of the node was changed.  For example, if we are rewriting
     * a let construct and only change the body, we can use the original list
     * of bindings, instead of generating new copies.
     *
     * This second kind of sharing is preserved by threading a ref cell
     * through the list traversal functions.  If any change is made to 
     * the list, the ref is set to true and the new list is kept.
     * otherwise, the new list can be discarded.
     *)

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
		     exp_var_bind,
		     labelled_var}) = handler

	fun ensure (NONE,item) = SOME item
	  | ensure (opt,_)     = opt

	(* Given a function f which expects a flag to set if it changes a term,
	 * and a flag that describes the state of a larger term,
	 * map f across a list of terms with a new local flag.
	 *
	 * If the flag gets tripped, set the original flag to true, and 
	 * return the new list.
	 * Otherwise return the old list and leave the flag unchanged.
	 *)
	fun map_f (f : bool ref -> 'state -> 'term -> 'term) (flag : bool ref) (state : 'state) (list : 'term list) = 
	  let val changed = ref false
	    val temp = map (f changed state) list 
	    val _ = flag := (!flag orelse !changed)
	  in if !changed then temp else list
	  end
	
	(* Given a function f which expects a flag to set if it changes a term
	 * and a flag which describes the state of a larger term,
	 * fold and accumulate f across a list of terms with a new local flag.
	 *
	 * If the flag gets tripped, set the original flag to true, and 
	 * return the new list.
	 * Otherwise return the old list and leave the flag unchanged.
	 * In either case, the new state is returned.
	 *)
	fun foldl_acc_f (f : bool ref -> ('obj * 'state) -> ('obj * 'state) )
			 (flag : bool ref) (state : 'state) (list : 'obj list) : ('obj list * 'state) = 
	  let
	    val changed = ref false
	    val (temp,state) = foldl_acc (f changed) state list
	    val _ = flag := (!flag orelse !changed)

	  in (if !changed then temp else list,state)
	  end

	(* Given a flag and an expression variable to bind in a state,
	 * bind the variable using the client binder, and set the flag
	 * if the client changes the variable.
	 *)
	fun bind_e changed (v : Nil.var,s : 'state) = 
	  (case exp_var_bind(s,v)
	     of (s,SOME v) => (changed := true;(v,s))
	      | (s,NONE) => (v,s))

	fun bind_first_e changed ((v : Nil.var,t),s : 'state) = 
	  let val (v,s) = bind_e changed (v,s)
	  in ((v,t),s)
	  end

	fun bind_firstfirst_e changed (((v : Nil.var,u),t),s : 'state) = 
	  let val (v,s) = bind_e changed (v,s)
	  in (((v,u),t),s)
	  end

	(* Given a flag and a constructor variable to bind in a state,
	 * bind the variable using the client binder, and set the flag
	 * if the client changes the variable.
	 *)
	fun bind_c changed (v : Nil.var,s : 'state) = 
	  (case con_var_bind(s,v)
	     of (s,SOME v) => (changed := true;(v,s))
	      | (s,NONE) => (v,s))
	fun bind_first_c changed ((v : Nil.var,k),s : 'state) = 
	  let val (v,s) = bind_c changed (v ,s)
	  in ((v,k),s)
	  end

	(* Wrap the rewriters up to translate between the 
	 * ref idiom and the option idiom. Given a flag, call
	 * the rewriter and set the flag if the rewriter changes
	 * the term: otherwise return the original term.
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


	(* Given a state and a constructor to be rewritten, rewrite the con
	 * and return it only if it was changed.
	 *)
	and rewrite_con (state : 'state) (con : con) : con option =  
	  let 
	    (* Take a recursive step through the constructor, after
	     * having first provided the client an opportunity to
	     * inspect/change it (below).  Note that this only
	     * gets called if the client returns one of the RECURSE
	     * flags.
	     *
	     * The code is extremely idiomatic: we almost always
	     * create a new flag, recur over the subcomponents
	     * using the flag, and then use the flag to determine
	     * whether or not to create a new result.
	     *)
	    fun docon (state,con) = 
	      let
	      in
		(case con of
		     (Prim_c (pcon,args)) => 
		     let
		       val changed = ref false      (* Will be set if anything changes *)
		       val args = map_f recur_c changed state args
			                            (* Recur over the arguments  *)
		     in if !changed then SOME (Prim_c (pcon,args)) else NONE
		                                    (* Only build a new term if something changed *)
		     end
		    | (Mu_c (flag,defs)) =>
		     let
		       val changed = ref false     (* Will be set if anything changes *)

		       val defslist = Sequence.toList defs
		       (* First bind the variables *)
		       val (defslist,state') = foldl_acc_f bind_first_c changed state defslist

		       val state = if flag then state' else state  (* If not recursive, discard the new state *)
		       val defslist = map_second (recur_c changed state) defslist
		     in  if !changed then SOME (Mu_c (flag,Sequence.fromList defslist)) else NONE
		     end
		   
		    | (AllArrow_c {openness, effect, tFormals, 
				   eFormals, fFormals, body_type}) =>
		     let
		       val changed = ref false   (* Did anything at all change? *)

		       val (tFormals,state) = tformals_helper_arrow changed state tFormals

		       val (eFormals,state) = 
			   let
			       fun efolder(c,s) = 
				   let 
				       val c = recur_c changed state c
				   in  (c,s)
				   end
			       val (new_eFormals,state) = foldl_acc efolder state eFormals
			       val eFormals = if !changed then new_eFormals else eFormals
			   in  (eFormals, state)
			   end

		       val body_type = recur_c changed state body_type
		     in
		       if !changed
			 then SOME (AllArrow_c{openness = openness, effect = effect,
					       tFormals = tFormals, eFormals = eFormals, 
					       fFormals = fFormals, body_type = body_type})
		       else NONE
		     end
		     
		    | ExternArrow_c (cons,con) =>
		     let
		       val changed = ref false
		       val cons = map_f recur_c changed state cons
		       val con = recur_c changed state con
		     in if !changed then SOME (ExternArrow_c (cons,con)) else NONE
		     end
		    | (Var_c var) => NONE
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
		     in if !changed then SOME (Let_c (letsort, cbnds, body)) else NONE
		     end

		    | (Closure_c (code,env)) =>
		     let
		       val changed = ref false
		       val code = recur_c changed state code
		       val env = recur_c changed state env
		     in if !changed then SOME (Closure_c(code, env)) else NONE
		     end
		   
		    | (Crecord_c entries) =>
		     let
		       val changed = ref false
		       val entries = map_second (recur_c changed state) entries
		     in if !changed then SOME (Crecord_c entries) else NONE
		     end
		   
		    | (Proj_c (con,lbl)) => 
		     (case rewrite_con state con                (* Only one subterm, so just recurse *)
			of SOME con => SOME (Proj_c (con,lbl))
			 | NONE => NONE)

		    | (App_c (cfun,actuals)) =>
		     let
		       val changed = ref false
		       val cfun = recur_c changed state cfun
		       val actuals = map_f recur_c changed state actuals
		     in if !changed then SOME (App_c (cfun, actuals)) else NONE
		     end
		   
		    | (Coercion_c {vars,from,to}) =>
		     let
		       val changed = ref false
		       val (vars,state) = foldl_acc_f bind_c changed state vars
		       val from = recur_c changed state from
		       val to = recur_c changed state to
		     in if !changed then SOME (Coercion_c {vars=vars,from=from,to=to}) 
			else NONE
		     end)
	      end
	  in

	    case (conhandler (state,con))         	    (* First pass the constructor to the client  *)
	      of CHANGE_NORECURSE (state,c) => SOME c       (* This constructor is finished *)
	       | CHANGE_RECURSE (state,c)   => ensure (docon (state,c),c)
                                                            (* Recursively traverse the new constructor.
							     * if the recursion produces a new result,
							     * use that, otherwise use the constructor
							     * returned by the client. *)
	       | NOCHANGE  => docon (state,con)             (* Recursively traverse the original constructor*)
	       | NORECURSE => NONE                          (* We're done! *)
	  end
	

	(* Rewrite a kind.  See the documentation in rewrite_con 
	 * for an explanation of the idiom
	 *)
	and rewrite_kind (state : 'state) (kind : kind) : kind option = 
	  let 
	    fun dokind (state,kind) = 
	      (case kind 
		 of Type_k => NONE
		  | (SingleType_k con) => mapopt SingleType_k (rewrite_con state con)
		  | (Single_k con) => mapopt Single_k (rewrite_con state con)
		  | (Record_k fieldseq) =>
		   let
		     val changed = ref false
		     fun fold_one (((lbl,var),kind),state) = 
		       let
			 val kind  = recur_k changed state kind
			 val (var,state) = bind_c changed (var,state)
		       in
			 (((lbl, var), kind),state)
		       end
		     val (fieldseq,state) = Sequence.foldl_acc fold_one state fieldseq
		   in if !changed then SOME(Record_k fieldseq) else NONE
		   end
		 
		  | (Arrow_k (openness, args, result)) =>
		   let
		     val changed = ref false
		     val (args, state) = tformals_helper_arrow changed state args
		     val result = recur_k changed state result
		   in if !changed then SOME(Arrow_k (openness, args, result)) else NONE
		   end)

	  in		 
	    (case (kindhandler (state,kind)) of
	       CHANGE_NORECURSE (state,k) => SOME k
	     | CHANGE_RECURSE (state,k) => ensure (dokind (state,k),k)
	     | NOCHANGE => dokind (state,kind)
	     | NORECURSE => NONE)
	  end
	
	and tformals_helper_arrow (flag : bool ref) (state : 'state) (vklist : (var * kind) list) : (var * kind) list * 'state = 
	  let
	    fun bind changed ((var,knd),state) = 
	      let
		val knd = 
		  (case rewrite_kind state knd
		     of SOME knd => (changed := true;knd)
		      | NONE => knd)
		val (var, state) = bind_c changed (var, state)
	      in
		((var,knd),state)
	      end

	    val (vklist,state) = foldl_acc_f bind flag state vklist
	  in (vklist,state)
	  end

	and tformals_helper (flag : bool ref) (state : 'state) (vlist : var list) : var list * 'state = 
	  let
	    fun bind changed (var,state) = 
	      let
		val (var, state) = bind_c changed (var, state)
	      in
		(var,state)
	      end

	    val (vlist,state) = foldl_acc_f bind flag state vlist
	  in (vlist,state)
	  end

	and fun_helper (state : 'state) (c,
					 Function{effect, recursive,
						  tFormals, eFormals, fFormals,
						  body}) : (con * function) option = 
	  let 
	    val changed = ref false
	    val (tFormals,state1) = tformals_helper changed state tFormals

	    val c = recur_c changed state c
	    local
	      fun vcfolder changed ((v,trace),s) = 
		let 
		  val trace = recur_trace changed state trace
		  val (v,s) = bind_e changed (v,s)
		in  ((v,trace),s)
		end
	    in
	      val (eFormals, state2) = foldl_acc_f vcfolder changed state1 eFormals
	    end

	    fun folder changed (v,s) = bind_e changed (v,s)
	    val (fFormals,state) = foldl_acc_f folder changed state fFormals

	    val body = recur_e changed state2 body
	  in
	    if !changed then
	      SOME (c,
		    Function{effect = effect, recursive = recursive,
			      tFormals = tFormals, eFormals = eFormals, fFormals = fFormals,
			      body = body})
	    else NONE
	  end

	(* Once again, this code is completely idiomatic.  See the documentation 
	 * for the constructor case above for an explanation of the idiom.
	 *)
	and rewrite_exp (state : 'state) (exp : exp) : exp option =
	  let 
	    fun doexp (state,e) = 
	      let
		val map_e = map_f recur_e
		val map_c = map_f recur_c
	      in
		(case e of
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
			      SOME (Const_e (Prim.array(c,array)))
			    else NONE
			  end
		      | (Prim.vector (c,array)) =>
			  let
			    val changed = ref false
			    val _ = Array.modify (recur_e changed state) array
			    val c = recur_c changed state c
			  in
			    if !changed then
			      SOME (Const_e (Prim.vector(c,array)))
			    else NONE
			  end
		      | Prim.refcell (r as (ref e)) => 
			  (case rewrite_exp state e
			     of SOME e => (r := e; SOME (Const_e v))
			      | NONE => NONE)
		      | Prim.tag (t,c) => 
			 (case rewrite_con state c
			    of SOME c => SOME (Const_e (Prim.tag(t,c)))
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
		    in if !changed then SOME (Let_e(sort,bnds,body)) else NONE
		    end
		 | (Prim_e (ap, trlist, clist,elist)) => 
		    let
		      val changed = ref false
		      val trlist = map_f recur_trace changed state trlist
		      val clist = map_c changed state clist
		      val elist = map_e changed state elist
		    in
		      if !changed then
			SOME (Prim_e(ap,trlist,clist,elist))
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
			then SOME (App_e(openness,func,clist,elist,eflist))
		      else NONE
		    end
		 | ExternApp_e (exp,args) =>
		    let
		      val changed = ref false
		      val exp = recur_e changed state exp
		      val args = map_e changed state args
		    in
		      if !changed then
			SOME (ExternApp_e (exp,args))
		      else NONE
		    end
		 | Raise_e (e,c) => 
		    let
		      val changed = ref false
		      val e = recur_e changed state e
		      val c = recur_c changed state c
		    in if !changed then SOME (Raise_e(e,c)) else NONE
		    end
		 | Handle_e {body,bound,handler,result_type} =>
		    let 
		      val changed = ref false
		      val body = recur_e changed state body
		      val result_type = recur_c changed state result_type
		      val (bound,state) = bind_e changed (bound,state)
		      val handler = recur_e changed state handler
		    in if !changed then 
			SOME (Handle_e{body = body, bound = bound,
				       handler = handler, 
				       result_type = result_type})
		       else NONE
		    end
		 | Coerce_e (coercion,cargs,e) => 
		    let
		      val changed = ref false
		      val coercion = recur_e changed state coercion
		      val cargs = map_c changed state cargs
		      val e = recur_e changed state e
		    in if !changed then
		         SOME (Coerce_e (coercion,cargs,e))
		       else NONE
		    end
		 | Fold_e stuff => coercion_helper Fold_e stuff
		 | Unfold_e stuff => coercion_helper Unfold_e stuff)

	      end

	    and coercion_helper whichc (vars,from,to) =
	      let
		val changed = ref false
		val (vars,state) = foldl_acc_f bind_c changed state vars
		val from = recur_c changed state from
		val to = recur_c changed state to
	      in
		if !changed then
		  SOME (whichc (vars,from,to))
		else NONE
	      end

	  in
      	    (case (exphandler (state,exp))
	       of CHANGE_NORECURSE (state,e) => SOME e
		| CHANGE_RECURSE (state,e) => ensure(doexp (state,e),e)
		| NOCHANGE => doexp (state,exp)
		| NORECURSE => NONE)
	  end

	and rewrite_trace (state : 'state) (trace : niltrace) : niltrace option = 
	  let

	    fun loop (Var_c v) labs = TraceKnown (TraceInfo.Compute (v,labs))
	      | loop (Proj_c (c,l)) labs = loop c (l::labs)
	      | loop _ _ = error "Non path returned from rewriting trace info"

	    fun do_trace (state,trace) =
	      (case trace of
		 TraceCompute var => 
		   let val changed = ref false
		       val trace = loop (recur_c changed state (Var_c var)) []
		   in
		     if !changed then SOME trace else NONE
		   end
	       | TraceKnown (TraceInfo.Compute (var,labels)) => 
		   let val changed = ref false
		       val trace = loop (recur_c changed state (Var_c var)) labels
		   in
		     if !changed then SOME trace else NONE
		   end
	       | _ => NONE)
	  in
      	    (case (tracehandler (state,trace))
	       of CHANGE_NORECURSE (state,t) => SOME t
		| CHANGE_RECURSE (state,t) => ensure(do_trace (state,t),t)
		| NOCHANGE => do_trace (state,trace)
		| NORECURSE => NONE)
	  end

	(* Rewrite a switch, returning the full switch expression
	 * if anything changes
	 *)
	and switch_helper (state : 'state) (sw : switch) : exp option = 
	  (case sw of
	     Intsw_e {arg, size, arms, default, result_type} =>
	       let
		 val changed = ref false
		 val arg = recur_e changed state arg
		 val result_type = recur_c changed state result_type
		 fun mapper changed state (t,e) = (t,recur_e changed state e) 
		 val arms = map_f mapper changed state arms
		 val default = Util.mapopt (recur_e changed state) default
	       in
		 if !changed then
		   SOME (Switch_e 
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
		 val result_type = recur_c changed state result_type
		 val (bound,state') = bind_e changed (bound,state)
		 fun recur changed state (t,tr,e) = (t,recur_trace changed state tr,recur_e changed state e)
		 val arms = map_f recur changed state' arms
		 val default = Util.mapopt (recur_e changed state) default
	       in  
		 if !changed then
		   SOME (Switch_e
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
		 val result_type = recur_c changed state result_type

		 val (bound,state') = bind_e changed (bound,state)

		 fun recur changed state (t,tr,e) = (recur_e changed state t, recur_trace changed state tr,
						     recur_e changed state' e)
		 val arms = map_f recur changed state arms
		 val default = Util.mapopt (recur_e changed state) default
	       in  
		 if !changed then 
		   SOME (Switch_e
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
		     val (vklist,state) = tformals_helper_arrow changed state vklist
		     val body = recur_e changed state body
		   in  (pc, vklist, body)
		   end
		 val arg = recur_c changed state arg
		 val arms = map doarm arms
		 val default = recur_e changed state default
		 val result_type = recur_c changed state result_type
	       in  
		 if !changed then
		   SOME (Switch_e
			 (Typecase_e{arg = arg,
				     arms = arms,
				     default = default,
				     result_type = result_type}))
		 else NONE
	       end
	   | Ifthenelse_e {arg,thenArm,elseArm,result_type} => 
	       let
		 val changed = ref false
		 fun do_cc cc = 
		   (case cc
		      of Exp_cc exp       => Exp_cc (recur_e changed state exp)
		       | And_cc (cc1,cc2) => And_cc (do_cc cc1,do_cc cc2)
		       | Or_cc (cc1,cc2)  => Or_cc  (do_cc cc1,do_cc cc2)
		       | Not_cc cc        => Not_cc (do_cc cc))
	   
		 val arg     = do_cc arg
		 val thenArm = recur_e changed state thenArm
		 val elseArm = recur_e changed state elseArm
		 val result_type = recur_c changed state result_type
	       in
		 if !changed then
		   SOME (Switch_e
			 (Ifthenelse_e {arg         = arg,
					thenArm     = thenArm,
					elseArm     = elseArm,
					result_type = result_type}))
		 else NONE
	       end
	     )


	     
	(* Rewrite a bnd to a list of bindings.  Note that the state that is returned
	 * will have bindings for all of the variables in the bindings.  
	 *
	 * if rewrite_bnd state (v=e) => (SOME[v'=e'],state'), then v' will be bound
	 * in state', but not v.  
	 * if rewrite_bnd state (v=e) => (NONE,state'), then v will be bound in
	 * state'.
	 * 
	 * (by "v will be bound in s", we mean that s is the state returned by 
	 * the client binder function.
	 *)

	and rewrite_bnd (state : 'state) (bnd : bnd) : (bnd list option * 'state) = 
	  let 

	    (* Set outerflag to true if anything changed.
	     *)
	    fun fun_helper outerflag state
	      (arg as ((v,c),Function{effect, recursive,
				  tFormals, eFormals, fFormals,
				  body})) = 
	      let 
		(* Set to true if anything changes.  Note that this may
		 * remain false even if outerflag is already true, so we get to preserve
		 * some additional sharing.
		 *)
		val changed = ref false

		val c = recur_c changed state c

		val (tFormals,state) = tformals_helper changed state tFormals
		  
		local
		  fun vcfolder changed ((v,trace),state) = 
		    let 
		      val trace = recur_trace changed state trace
		      val (v,state) = bind_e changed (v,state) 
		    in  ((v,trace),state)
		    end
		in
		  val (eFormals, state) = foldl_acc_f vcfolder changed state eFormals
		end
		val (fFormals,state) = foldl_acc_f bind_e changed state fFormals
		val body = recur_e changed state body
		val _ = outerflag := (!outerflag orelse !changed)
	      in
		if !changed then
		  ((v,c),Function({effect = effect, recursive = recursive,
			       tFormals = tFormals, eFormals = eFormals, fFormals = fFormals,
			       body = body}))
		else arg
	      end
	    
	    fun do_fix (maker,vfset) = 
	      let 
		val changed = ref false
		val vflist = Sequence.toList vfset 

		val (vflist,state) = foldl_acc_f bind_firstfirst_e changed state vflist 

		val vflist =  map_f fun_helper changed state vflist
		val vfset = Sequence.fromList vflist
	      in  
		(if !changed then SOME [maker vfset] else NONE,state)
	      end
	  
	    fun do_bnd (bnd,state) : bnd list option * 'state = 
	      (case bnd 
		 of Con_b(p,cb) => 
		   let val (cb_opt,state) = rewrite_cbnd state cb
		   in  (mapopt (fn cbnds => map (fn cb => Con_b(p,cb)) cbnds) cb_opt, state)
		   end
		  | Exp_b(v,trace,e) => 
		   let
		     val changed = ref false
		     val e = recur_e changed state e 
		     val trace = recur_trace changed state trace 
		     val (v,state) = bind_e changed (v,state)
		   in
		     (if !changed then SOME [Exp_b(v,trace,e)] else NONE, state)
		   end
		  | Fixopen_b vfset => do_fix (Fixopen_b,vfset)
		  | Fixcode_b vfset => do_fix (Fixcode_b,vfset)
		  | Fixclosure_b (is_recur,vcset) => 
		   let 
		     val changed = ref false
		     val vclist = Sequence.toList vcset
		     val oldstate = state
		     val (vclist,state) = foldl_acc_f bind_firstfirst_e changed state vclist
		     val innerstate = if is_recur then state else oldstate

		     fun doer flag s (arg as ((v,tipe),{code,cenv,venv})) = 
		       let 
			 val changed = ref false
			 val code = (case recur_e changed s (Var_e code)
				       of Var_e code => code
					| _ => error "can't have non-var in closure code comp")
			 val cenv = recur_c changed s cenv
			 val venv = recur_e changed s venv
			 val tipe = recur_c changed s tipe
			 val _ = flag := (!flag orelse !changed)
		       in
			 if !changed then
			   ((v,tipe),{code=code,cenv=cenv,venv=venv})
			 else arg
		       end
		     val vclist = map_f doer changed innerstate vclist
		     val vcset = Sequence.fromList vclist
		   in  (if !changed then SOME [Fixclosure_b(is_recur,vcset)] else NONE, state)
		   end)

	  in
	    (case (bndhandler (state,bnd)) 	    (* As usual, call the client code. *)
	       of CHANGE_NORECURSE (state,bs) => (SOME bs,state)   
		                                    (* If we get back a list of bnds, just return them *)
		| CHANGE_RECURSE (state,bs) =>      (* Recur on the new bnds, adding the variables to the state in the process *)
		 let 
		   val changed = ref false
		   fun do_bnd' (bnd,state) = 
		     case do_bnd (bnd,state)
		       of (SOME bnds,state) => (changed := true;(bnds,state))
			| (NONE,state) => ([bnd],state)
		   val (newbnds,state) = foldl_acc do_bnd' state bs
		 in
		   (if !changed then SOME (List.concat newbnds) else SOME bs,state)
		 end
		| NOCHANGE => do_bnd (bnd,state)    (* Recur over the original bnd, binding the variable in the process *)
		| NORECURSE => (NONE,state))        (* NORECURSE doesn't bind the variables *)
	  end

	and rewrite_cbnd (state : 'state) (cbnd : conbnd) : (conbnd list option * 'state) =
	  let

	    
	    fun do_cbnd (cbnd,state) = 
	      (case cbnd 
		 of Con_cb(var, con) =>
		   let 
		     val changed = ref false
		     val con = recur_c changed state con
		     val (var,state) = bind_c changed(var,state)
		   in if !changed then (SOME (Con_cb(var, con)), state) else (NONE,state)
		   end
		  | _ => 
		   let 

		     val oldstate = state
		     val (wrap,(var,vklist,c)) = (case cbnd
						    of Open_cb args => (Open_cb, args)
						     | Code_cb args => (Code_cb, args))
		       
                     fun folder changed ((v,k),state) = 
		       let 
			 val k = recur_k changed state k
			 val (v,state) = bind_c changed (v,state)
		       in  ((v,k),state)
		       end
		     val changed = ref false
		     val (vklist,state) = foldl_acc_f folder changed state vklist
		     val c = recur_c changed state c
		     val (var,state) = bind_c changed (var,oldstate)
		   in 
		     if !changed then (SOME (wrap (var,vklist,c)),state)
		     else (NONE,state)
		   end)

	  in
	    (case (cbndhandler (state,cbnd)) 	    (* As usual, call the client code. *)
	       of CHANGE_NORECURSE (state,cbs) => (SOME cbs,state)   
		                                    (* If we get back a list of bnds, just return them *)
		| CHANGE_RECURSE (state,cbnds) =>      (* Recur on the new bnds, adding the variables to the state in the process *)
		 let 
		   val changed = ref false
		   fun do_cbnd' (cbnd,state) = 
		     case do_cbnd (cbnd,state)
		       of (SOME cbnd,state) => (changed := true;(cbnd,state))
			| (NONE,state) => (cbnd,state)
		   val (newcbnds,state) = foldl_acc do_cbnd' state cbnds
		 in
		   (if !changed then SOME newcbnds else SOME cbnds,state)
		 end
		| NOCHANGE =>
		  (case do_cbnd (cbnd,state)
		     of (SOME cbnd,state) => (SOME [cbnd],state)
		      | (NONE,state) => (NONE,state))
		                                    (* Recur over the original bnd, binding the variable in the process *)
		| NORECURSE => (NONE,state))        (* NORECURSE doesn't bind the variables *)

	  end  



	fun import_helper flag (import as (ImportValue (label,var,trace,con)),state) =
	  let
	    val changed = ref false
	    val trace = recur_trace changed state trace 
	    val con = recur_c changed state con
	    val (var,state) = bind_e changed (var,state)
	    val state = labelled_var (state,label,var)
	    val _ = flag := (!changed orelse !flag)
	  in (if !changed then ImportValue (label,var,trace,con) else import,state)
	  end
	  | import_helper flag (import as (ImportType (label,var,kind)),state) = 
	  let
	    val changed = ref false
	    val kind = recur_k changed state kind
	    val (var,state) = bind_c changed (var,state)
	    val state = labelled_var (state,label,var)
	    val _ = flag := (!changed orelse !flag)
	  in (if !changed then ImportType (label,var,kind) else import,state)
	  end
	
	fun export_helper flag state (export as (ExportValue (label,var))) = 
	  (case rewrite_exp state (Var_e var)
		 of SOME (Var_e var) => (flag := true;ExportValue (label,var))
		  | NONE => export
		  | _ => error "Export value rewritten to non variable! Don't know what to do!")
	  | export_helper flag state (export as (ExportType (label,var))) = 
	   (case rewrite_con state (Var_c var)
	      of SOME (Var_c var) => (flag := true;ExportType (label,var))
	       | NONE => export
	       | _ => error "Export type rewritten to non variable! Don't know what to do!")

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

      fun null_binder (state,_) = (state,NONE)

      fun null_handler _ = NOCHANGE

      fun null_label_binder (state,_,_) = state

      val default_handler =  
	HANDLER {
		 bndhandler     = null_handler,
		 cbndhandler    = null_handler,
		 conhandler     = null_handler,
		 exphandler     = null_handler,
		 kindhandler    = null_handler,
		 tracehandler   = null_handler,
		 con_var_bind   = null_binder,
		 exp_var_bind   = null_binder,
		 labelled_var   = null_label_binder
		 }

      fun set_kindhandler (HANDLER {bndhandler,cbndhandler,
				    conhandler,exphandler,kindhandler,tracehandler,
				    con_var_bind,exp_var_bind,labelled_var}) new_kindhandler = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = exphandler,
		 kindhandler    = new_kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = con_var_bind,
		 exp_var_bind   = exp_var_bind,
		 labelled_var   = labelled_var
		 }

      fun set_conhandler (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,tracehandler,
				   con_var_bind,exp_var_bind,labelled_var}) new_conhandler = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = new_conhandler,
		 exphandler     = exphandler,
		 kindhandler    = kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = con_var_bind,
		 exp_var_bind   = exp_var_bind,
		 labelled_var   = labelled_var
		 }

      fun set_exphandler (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,tracehandler,
				   con_var_bind,exp_var_bind,labelled_var}) new_exphandler = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = new_exphandler,
		 kindhandler    = kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = con_var_bind,
		 exp_var_bind   = exp_var_bind,
		 labelled_var   = labelled_var
		 }

      fun set_exp_binder (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,tracehandler,
				   con_var_bind,exp_var_bind,labelled_var}) new_exp_var_bind = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = exphandler,
		 kindhandler    = kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = con_var_bind,
		 exp_var_bind   = new_exp_var_bind,
		 labelled_var   = labelled_var
		 }


      fun set_con_binder (HANDLER {bndhandler,cbndhandler,
				   conhandler,exphandler,kindhandler,tracehandler,
				   con_var_bind,exp_var_bind,labelled_var}) new_con_var_bind = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = exphandler,
		 kindhandler    = kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = new_con_var_bind,
		 exp_var_bind   = exp_var_bind,
		 labelled_var   = labelled_var
		 }


      fun set_label_binder (HANDLER {bndhandler,cbndhandler,
				     conhandler,exphandler,kindhandler,tracehandler,
				     con_var_bind,exp_var_bind,
				     labelled_var}) new_label_binder = 
	HANDLER {
		 bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = exphandler,
		 kindhandler    = kindhandler,
		 tracehandler   = tracehandler,
		 con_var_bind   = con_var_bind,
		 exp_var_bind   = exp_var_bind,
		 labelled_var   = new_label_binder
		 }

  end
