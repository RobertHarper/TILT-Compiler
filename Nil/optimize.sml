(*$import NIL NILCONTEXT NILSTATIC NILUTIL NILSUBST PPNIL OPTIMIZE Stats NORMALIZE *)
(* A one-pass optimizer with the following goals:
	Convert project_sum to project_sum_record.
	Eliminate dead code.
	Not explode code size through anormalization.
	Recognize certain patterns of Sumsw and convert to Intsw
*)	

functor Optimize(structure Nil : NIL
		 structure NilContext : NILCONTEXT
		 structure Normalize : NORMALIZE
		 structure NilStatic : NILSTATIC
		 structure NilUtil : NILUTIL
		 structure Subst : NILSUBST
		 structure Ppnil : PPNIL
		 sharing Ppnil.Nil = NilUtil.Nil = NilContext.Nil = NilStatic.Nil = Normalize.Nil = Nil 
		 sharing type NilContext.context = NilStatic.context = Normalize.context
		 sharing type Subst.con = Nil.con) 
    :> OPTIMIZE where Nil = Nil = 
struct

	open Util Nil NilUtil NilContext Listops
 	val error = fn s => Util.error "optimize.sml" s

	val debug = ref false
	val do_diag = ref false
	fun diag s = if !do_diag then print s else()

	val do_dead = ref true
	val do_proj = ref true

	(* A transformation state is thread through the optimizer maintaining:
		(1) whether a variable has been 
			(a) definitely used 
			(b) possibly used
			(c) unused
		(2) A map from term variables to an equivalent expression.
			This is used for known term records. In some 
			cases, the substitution must be made and this is indicated
			by the bool flag being true.
		(3) an indicator of what binding we are currently in
			In mutually recursive constucts, we use the first
				item as the name of the group.
		(4) A map from curried functions to the uncurried version.
		(5) A map from constructor variables to their actual or
		      equivalent bindings.  The map can be thought of as
		      a set of equations.
	  (3) and (1.b) together allow cascading dead code to be eliminated.
        *)
	local
	  datatype used_state = UNUSED | USED | DEFER of (used_state ref) list
	  datatype state = STATE of {equation: NilContext.context,
				     used : used_state ref Name.VarMap.map,
				     current : used_state ref,
				     alias : (bool * exp) Name.VarMap.map,
				     uncurry : (int * var) Name.VarMap.map}
	  fun isused (ref USED) = true
	    | isused (ref UNUSED) = false
	    | isused (r as ref (DEFER ls)) = 
	      let val isused = orfold isused ls
		  val _ = r := (if isused then USED else UNUSED)
	      in  isused
	      end
	in
	  type state = state
	  fun new_state() = STATE {equation = NilContext.empty(),
				   used = Name.VarMap.empty,
				   uncurry = Name.VarMap.empty,
				   alias = Name.VarMap.empty,
				   current = ref USED}	
	  fun retain_state(STATE{equation,alias,used,current,uncurry}) = 
			STATE{used=used,alias=alias,equation=equation,
			      uncurry=uncurry,current=ref USED}
	  fun add_vars(STATE{equation,used,alias,current,uncurry},vars) = 
		let val r = ref UNUSED
		in  STATE{equation=equation,
			  used=foldl (fn (v,m) => Name.VarMap.insert(m,v,r)) used vars,
			  alias=alias,
			  current=current,
			  uncurry=uncurry}
		end
	  fun add_var(state,v) = add_vars(state,[v])
	  fun enter_var(STATE{equation,used,alias,current,uncurry},v) = 
		STATE{equation=equation,
		      used=used,
		      alias=alias,
		      uncurry=uncurry,
		      current=case (Name.VarMap.find(used,v)) of
				NONE => error "enter_var given var not in used map"
			      | SOME us => us}
	  fun use_var(STATE{used,current,...},v) = 
	  	(case Name.VarMap.find(used,v) of
		  NONE => ()
		| SOME (ref USED) => ()
		| SOME (r as (ref UNUSED)) => r := DEFER [current]
		| SOME (r as (ref (DEFER ls))) => r := DEFER (current::ls))
          fun is_used_var(STATE{used,...},v) = 
		case Name.VarMap.find(used,v) of
		  NONE => (print "is_used_var given var not in state: ";
		  	   Ppnil.pp_var v; print "\n";
			   error "is_used_var given var not in state")
		| SOME r => if (!do_dead) then isused r else true
	  fun add_aliases(STATE{equation,used,uncurry,current,alias},aliases) =
		STATE{equation=equation,
		      used=used,
		      alias=foldl (fn ((v,b,e),m) => 
					(
	  (*				print "Adding alias of "; Ppnil.pp_var v; 
	                           print " to "; Ppnil.pp_exp e; print "\n";
	  *)
					 Name.VarMap.insert(m,v,(b,e)))) alias aliases,
		      uncurry=uncurry,
		      current=current}		
	  fun add_alias(s,v,b,e) = add_aliases(s,[(v,b,e)])
	  fun add_curry(STATE{equation,alias,used,uncurry,current},v_curry,depth,v_uncurry) =
	      STATE{equation=equation,
		    used=used,
		    alias=alias,
		    uncurry=Name.VarMap.insert(uncurry,v_curry,(depth,v_uncurry)),
		    current=current}
	  fun lookup_curry(STATE{uncurry,...},v) = Name.VarMap.find(uncurry,v)
	  fun lookup_alias(STATE{alias,...},v) = Name.VarMap.find(alias,v)

	  fun lookup_proj(state,v,labs) = 
	      let fun loop e [] = SOME e
		    | loop (Prim_e(NilPrimOp(record labs),_,elist)) (l::rest) = 
		         loop (valOf(assoc_eq(Name.eq_label,l,zip labs elist))) rest
		    | loop _ _ = NONE
	      in  (case lookup_alias(state,v) of
		       NONE => NONE
		     | SOME (_,e) => loop e labs)
	      end

	  fun get_env(STATE{equation,...}) = equation
	  fun find_equation(STATE{equation,...},c) = NilContext.find_kind_equation(equation,c)
	  fun find_con(STATE{equation,...},v) = NilContext.find_con(equation,v)

	  fun add_kind(STATE{equation,alias,used,uncurry,current},v,k) = 
	      STATE{equation=NilContext.insert_kind(equation,v,k),
		    used=used,
		    alias=alias,
		    uncurry=uncurry,
		    current=current}
	  fun add_con(STATE{equation,alias,used,uncurry,current},v,c) = 
	      (STATE{equation=NilContext.insert_con(equation,v,c),
		    used=used,
		    alias=alias,
		    uncurry=uncurry,
		    current=current})
	  fun add_kind_equation(STATE{equation,alias,used,uncurry,current},v,c,k) = 
	      STATE{equation=NilContext.insert_kind_equation(equation,v,c,k),
		    used=used,
		    alias=alias,
		    uncurry=uncurry,
		    current=current}

	  fun type_of(STATE{equation,...},e) = Normalize.type_of(equation,e)
	end



        (* DEBUGGING FUNCTIONS *)
        local
	    val exp_depth = ref 0
	    val con_depth = ref 0
	    fun help str i = (print str; print (Int.toString i); print "\n")
	 in
	     fun reset_debug() = (exp_depth := 0;
				 con_depth := 0)
	     fun push_exp e = (help "call exp_depth = " (!exp_depth); 
			       exp_depth := (!exp_depth) + 1)
	     fun pop_exp e = (help "return exp_depth = " (!exp_depth); 
			       exp_depth := (!exp_depth) - 1)
	     fun push_con c = (help "call con_depth = " (!con_depth); 
			       con_depth := (!con_depth) + 1)
	     fun pop_con c = (help "return con_depth = " (!con_depth); 
			       con_depth := (!con_depth) - 1)
	end

	(* Currently, all nilprimops are pure but may allocate *)
	fun is_pure(Prim_e (NilPrimOp np, _, elist)) = Listops.andfold is_pure elist
	  | is_pure(Var_e _) = true
	  | is_pure _ = false
	fun cbnd_used state (Con_cb(v,_)) = is_used_var(state,v)
	  | cbnd_used state (Open_cb(v,_,_,_)) = is_used_var(state,v)
	  | cbnd_used state (Code_cb(v,_,_,_)) = is_used_var(state,v)
	fun bnd_used state (Con_b(_,cb)) = cbnd_used state cb
	  | bnd_used state (Exp_b(v,_,e)) = is_used_var(state,v)
	  | bnd_used state (Fixopen_b(vfset)) = orfold (fn (v,_) => is_used_var(state,v)) (Sequence.toList vfset)
	  | bnd_used state (Fixcode_b(vfset)) = orfold (fn (v,_) => is_used_var(state,v)) (Sequence.toList vfset)
	  | bnd_used state (Fixclosure_b(_,vclset)) = orfold (fn (v,_) => is_used_var(state,v)) 
								(Sequence.toList vclset)

	fun do_vklist state vklist =
	    let fun folder((v,k),state) = let val k = do_kind state k
					  in  ((v,k),add_kind(state,v,k))
					  end
	    in  foldl_acc folder state vklist
	    end
	and do_vclist state vclist = foldl_acc (fn ((v,c),acc) => let val c = do_con state c
								  in  ((v,c),add_con(acc,v,c))
								  end) state vclist
	and do_kind (state : state) (kind : kind) : kind = 
	  (case kind of 
		Type_k => kind
              | Singleton_k c => Singleton_k(do_con state c)
  	      | Record_k(lvk_seq) => let fun folder(((l,v),k),state) = (((l,v),do_kind state k),
									add_kind(state,v,k))
				     in  Record_k(#1(Sequence.foldl_acc folder state lvk_seq))
				     end
	      | Arrow_k(openness,vklist,k) => let val (vklist,state) = do_vklist state vklist
					      in  Arrow_k(openness,vklist,do_kind state k)
					      end)

(*
	and do_con (state : state) (con : con) : con =
	    let val _ = push_con con
		val result = do_con' state con
		val _ = pop_con con
	    in  result
	    end 
*)
	and do_con (state : state) (con : con) : con =
	   (case con of
		Prim_c(pc,clist) => Prim_c(pc, map (do_con state) clist)
	      | Mu_c(recur,vc_seq) => Mu_c(recur,Sequence.map
					   (fn (v,c) => (v,do_con state c)) vc_seq)
	      | AllArrow_c(openness,effect,vklist,vclist,numfloats,c) =>
			let val (vklist,state) = do_vklist state vklist
			in  AllArrow_c(openness,effect,vklist,
				   map (do_con state) vclist, numfloats, do_con state c)
			end
	      | Var_c v => (use_var(state,v); con)
	      | Crecord_c lclist => Crecord_c(map (fn (l,c) => (l, do_con state c)) lclist)
	      | Proj_c(c,l) => Proj_c(do_con state c, l)
	      | Closure_c(c1,c2) => Closure_c(do_con state c1, do_con state c2)
	      | App_c(c,clist) => App_c(do_con state c, map (do_con state) clist)
	      | Typecase_c _ => error "typecase not handled"
	      | Annotate_c (a,c) => Annotate_c(a,do_con state c)
	      | Let_c(letsort,cbnds,c) => 
			let (* we must put a wrapper in order to perform the filter *)
			    val state = retain_state state
			    val (cbnds,state) = foldl_acc do_cbnd state cbnds
			    val c = do_con state c
			    val cbnds = List.filter (cbnd_used state) cbnds
		        in  Let_c(letsort,cbnds,c)
			end)

	and do_cbnd(cbnd : conbnd, state : state) : conbnd * state = 
	   (case cbnd of
		Con_cb(v,c) => let val state = add_var(state,v)
				   val state' = enter_var(state,v)
(*		   val k = NilStatic.get_shape (get_env state) c *)
				   val k = Singleton_k c
				   val state = add_kind_equation(state,v,c,k)
			       in  (Con_cb(v,do_con state' c), state)
			       end
	      | Open_cb(v,vklist,c,k) => let val state = add_var(state,v)
					     val state' = enter_var(state,v)
					     val (vklist,state') = do_vklist state' vklist
					 in  (Open_cb(v,vklist,
						      do_con state' c, do_kind state' k), state)
					 end
	      | Code_cb(v,vklist,c,k) => let val state = add_var(state,v)
					     val state' = enter_var(state,v)
					     val (vklist,state') = do_vklist state' vklist
					 in  (Code_cb(v,vklist, 
						do_con state' c, do_kind state' k), state)
					 end)

	fun extract_uncurry(openness,curry_name,uncurry_name,function) = 
	    let fun separate(Function(effect,recur,vklist,vclist,vflist,e,c)) = 
		    (e, c, vklist, vclist, vflist,
		     fn c => fn e => Function(effect,recur,vklist,vclist,vflist,e,c))
	        fun is_lambda (Let_e(_,[Fixopen_b vfset],Var_e v)) = 
		    (case (Sequence.toList vfset) of
			 [(v',f)] => if (Name.eq_var(v,v'))
					 then SOME(v,f)
				     else NONE)
		  | is_lambda _ = NONE
		fun loop (depth,alias,funcons,curry_call,args,cwrapper_opt,v,function) = 
		    let val (body,con,vk,vc,vf,wrapper) = separate function
			val alias = if (depth>1) then (v,true,curry_call)::alias else alias
			val funcon = AllArrow_c(Open,Partial,vk,map #2 vc,
						TilWord32.fromInt(length vf),con)
			val funcons = if (depth>1) then (v,funcon)::funcons else funcons
			val curry_call = App_e(Open,curry_call,
						map (Var_c o #1) vk, 
						map (Var_e o #1) vc, map Var_e vf)
			val args = ((#1 args) @ vk, (#2 args) @ vc, (#3 args) @ vf)
			val cwrapper_base = 
			      (case cwrapper_opt of
				   NONE => wrapper
				 | SOME cwrapper => 
				       (fn c => fn e =>
					let val f = wrapper c e
					    val e = Let_e(Sequential,
							  [Fixopen_b(Sequence.fromList[(v,f)])], 
							  Var_e v)
					in  cwrapper c e
					end))
			fun cwrapper_nonbase c e = cwrapper_base con e
			fun base() = 
			    let 
				val (vk,vc,vf) = args
				val uncurry_call = App_e(openness,Var_e uncurry_name, 
							 map (Var_c o #1) vk,
							 map (Var_e o #1) vc,
							 map Var_e vf)
				fun uwrapper(body,con) = Function(Partial,Arbitrary,vk,vc,vf,body,con)
			    in  (depth,uncurry_call,vk,vc,vf,body,con,
				 alias,funcons,cwrapper_base,uwrapper)
			    end
		    in  case (is_lambda body) of
			SOME(v,f) => loop (depth+1,alias,funcons,
					   curry_call,args,SOME cwrapper_nonbase,v,f)
		      | NONE => base()
		    end
	    in  loop(1,[],[],Var_e curry_name,([],[],[]),NONE,curry_name,function)
	    end

	fun do_uncurry(vflist,state) = 
	  let fun folder ((v,f),(acc,state)) = 
	        let (* v' is the name of the uncurried function *)
		    val v' = Name.fresh_named_var ((Name.var2name v) ^ "_uncurry")
		    val (depth,uncurry_call,vk,vc,vf,body,con,aliases,funcons,
			 cwrapper,uwrapper) = extract_uncurry(Open,v,v',f)
		    val temp = (v,vk,vc,vf,body,con,cwrapper,uwrapper,uncurry_call)
		    val state =
			let val Function(effect,recur,vklist,vclist,vflist,e,c) = f
			    val funcon = AllArrow_c(Open,Partial,vklist,map #2 vclist,
						    TilWord32.fromInt(length vflist),c)
			in  add_con(state,v,funcon)
			end
		    val state = 
			if (depth>1)
			    then 
				let
				    val state = foldl (fn ((v,c),s) => add_con(s,v,c))state funcons
				    val state = add_var(state,v')
				    val state = add_curry(state,v,depth,v')
				in  add_aliases(state,aliases)
				end
			else state
		in   (temp::acc,state)
		end
	  in  foldl folder ([],state) vflist
	  end


(*
	fun reduce_once (state : state) (con : con) : con option = 
	  (case con of
	       App_c(Var_c v, args) => 
		 (case (find_equation(state,Var_c v)) of
		      SOME (Let_c(_,[Open_cb(v,vklist,body,_)],Var_c v')) => 
			  if (Name.eq_var(v,v'))
			      then let val ls = zip (map #1 vklist) args
				       val subst = Subst.fromList ls
				   in  SOME (Subst.substConInCon subst body)
				   end
			  else NONE
		    | _ => NONE)
	     | Var_c v => find_equation(state,Var_c v)
	     | Proj_c(Crecord_c lcons,l) => Listops.assoc_eq(Name.eq_label,l,lcons)
	     | Proj_c(c,l) => (case reduce_once state c of
				   NONE => find_equation(state,con) 
				 | SOME c => SOME (Proj_c(c,l)))
	     | _ => (if (!debug)
			 then (print "reduce got non-variable, non-projection,";
			       print "and non-application:\n";
			       Ppnil.pp_con con)
		     else (); 
		     NONE))
*)

	fun reduce (state : state) (pred : con -> 'a option) (con : con) : 'a option = 
	    let fun loop (subst,con) = 
		(case (pred con) of
		     (some as SOME _) => pred (NilStatic.con_subst(subst,con))
		   |  NONE => (case (NilStatic.con_reduce_once (get_env state,subst) con) of
				   (false,_,con) => (if (!do_diag)
						     then (print "reduce stopped at: ";
						     Ppnil.pp_con con;
						     print "\n") else ();
						     NONE)
				 | (true,subst,con) => loop (subst,con)))
	    in  loop (NilStatic.empty_subst,con)
	    end

val reduce = fn state => fn pred => 
    Stats.subtimer("optimize_reduce",reduce state pred)

	fun is_bool state con = 
	    (case reduce state (fn (Prim_c(Sum_c{tagcount=0w2,...},[])) => SOME ()
	                         | _ => NONE) con of
		 SOME () => true
	       | NONE => false)

	 fun getknownsum (Prim_c(Sum_c {tagcount,totalcount,known=SOME k}, [c])) = 
	     SOME(TilWord32.toInt tagcount, TilWord32.toInt totalcount, 
		  TilWord32.toInt k, c)
	   | getknownsum _ = NONE

	 fun getsum (Prim_c(Sum_c {tagcount,totalcount,known}, [c])) = 
	     SOME(TilWord32.toInt tagcount, TilWord32.toInt totalcount, 
		  known, c)
	   | getsum _ = NONE

	 fun getexn (Prim_c(Exntag_c, [c])) = SOME c
	   | getexn _ = NONE

	fun is_sumsw_int state exp = 
	  (case exp of
	     (Switch_e(Sumsw_e{sumtype,
			       result_type,
			       bound,
			       arg=Prim_e(PrimOp(Prim.eq_int is),[],
					  [Var_e v,Const_e (Prim.int(_,w))]),
			       arms=[(0w0,zeroexp),
				     (0w1,oneexp)],
			       default=NONE})) =>
	    if (is_bool state sumtype)
		then
		    SOME (result_type,is,v,TilWord64.toUnsignedHalf w,zeroexp,oneexp)
	    else NONE
	  | _ => NONE)

	fun convert_sumsw state sum_sw =
	    let val exp = Switch_e(Sumsw_e sum_sw)
	    in  (case is_sumsw_int state exp of
	         SOME (result_type,is,commonv,_,_,_) =>
		     let fun loop acc (e : exp) =
			 (case is_sumsw_int state e of
			      NONE => (acc,e)
			    | SOME (_,is,v,w,zeroexp,oneexp) =>
				  if (Name.eq_var(v,commonv))
				      then loop ((w,oneexp)::acc) zeroexp
				  else (acc,e))
			 val (clauses,base) = loop [] exp
		     in  if (length clauses > 1)
			     then Intsw_e{size=is,result_type=result_type,arg=Var_e commonv,
					  arms=rev clauses,default=SOME base}
			 else Sumsw_e sum_sw
		     end
	       | _ => Sumsw_e sum_sw)
	    end	     

(*
	and do_exp (state : state) (exp : exp) : exp = 
	    let val _ = push_exp exp
		val result = do_exp' state exp
		val _ = pop_exp exp
	    in  result
	    end 
*)

	and do_exp (state : state) (exp : exp) : exp = 
	   (case exp of
		  Var_e v =>
			 (case lookup_alias(state,v) of
				  NONE => (use_var(state,v); exp)
				| SOME (false,_) => (use_var(state,v); exp)
				| SOME (true,e) => do_exp state e)
		| Const_e _ => exp
		| Prim_e(p as NilPrimOp(select l),clist, elist as [Var_e v]) => 
			(case (lookup_proj(state,v,[l])) of
				NONE => Prim_e(p,map (do_con state) clist, map (do_exp state) elist)
			      | SOME e => do_exp state e)
		| Prim_e(p as NilPrimOp (inject k),clist,elist as [Var_e v]) => 
			 (case lookup_alias(state,v) of
			      SOME (_,Prim_e(NilPrimOp(record _),_,elist)) =>
				  do_exp state (Prim_e(NilPrimOp (inject_record k),clist,elist))
			    | _ => Prim_e(p,map (do_con state) clist, map (do_exp state) elist))
		| Prim_e(p,clist,elist) => Prim_e(p,map (do_con state) clist, 
						  map (do_exp state) elist)
		| Switch_e sw => Switch_e(do_switch state sw)
		| Let_e (letsort,bnds,e) => 
			let (* we must put a wrapper in order to perform the filter *)
			    val state = retain_state state
			    val (bnds,state) = do_bnds(bnds,state)
			    val e = do_exp state e
			    val bnds = List.filter (bnd_used state) bnds
		        in  Let_e(letsort,bnds,e)
			end
		| App_e(openness,f,clist,elist,eflist) => 
		        let fun extract acc (Var_e v) = 
			         (case (lookup_alias(state,v)) of
				      NONE => SOME(v,acc)
				    | SOME (_,e) => extract acc e)
			      | extract acc (App_e(openness,e, clist,elist,eflist)) = 
				      let val acc = (clist,elist,eflist)::acc
				      in  extract acc e
				      end
			      | extract _ _ = NONE
			    fun default() = App_e(openness, do_exp state f,
						map (do_con state) clist,
						map (do_exp state) elist,
						map (do_exp state) eflist)
			    fun uncurry (depth, v,args) = 
				let 
(*				    val _ = (print "curry case with v = ";
						Ppnil.pp_var v; print "\n")
*)
				    val _ = use_var(state,v)
				    val args1 = List.take(args,depth)
				    val args2 = List.drop(args,depth)
				    fun folder ((clist,elist,eflist),(cacc,eacc,efacc)) = 
					(cacc @ (map (do_con state) clist),
					 eacc @ (map (do_exp state) elist),
			 		 efacc @ (map (do_exp state) eflist))
				    val (clist,elist,eflist)= foldl folder ([],[],[]) args1
				    fun folder ((clist,elist,eflist), f) = 
					App_e(openness,f,
 						map (do_con state) clist,
						map (do_exp state) elist,
						map (do_exp state) eflist)
				in  foldl folder (App_e(openness,Var_e v, clist, elist,eflist)) args2
				end
			in  case (extract [] exp) of
				NONE =>	(diag "could not extract\n"; default())
			      | SOME (v,args) => 
				  (case(lookup_curry(state,v)) of
					NONE => (diag "could not find alias\n"; default())
				      | SOME (depth,v') => 
					    if (length args < depth)
						then (diag "depth too low\n"; default())
					    else uncurry (depth, v',args))
			end

		| Raise_e(e,c) => Raise_e(do_exp state e, do_con state c)
		| Handle_e(e,v,handler,c) => 
			let val ([(v,_)],state) = do_vclist state [(v,Prim_c(Exn_c,[]))]
			in  Handle_e(do_exp state e, v, do_exp state handler, do_con state c)
			end)

	and do_switch (state : state) (switch : switch) : switch = 
	    (case switch of
		 Intsw_e {size,arg,result_type,arms,default} =>
		     let val arg = do_exp state arg
			 val result_type = do_con state result_type
			 val arms = map_second (do_exp state) arms
			 val default = Util.mapopt (do_exp state) default
		     in  Intsw_e {size=size,arg=arg,result_type=result_type,
				  arms=arms,default=default}
		     end
	       | Sumsw_e {sumtype,arg,result_type,bound,arms,default} =>
		     let val arg = do_exp state arg
			 val result_type = do_con state result_type
			 val sumtype = do_con state sumtype
			 val (tagcount,totalcount,_,carrier) = 
			     (case reduce state getsum sumtype of
				  SOME quad => quad
				| NONE => error "sumcon of sumsw_e not reducible to sum")
			 fun make_ssum i = Prim_c(Sum_c{tagcount=TilWord32.fromInt tagcount,
							totalcount=TilWord32.fromInt totalcount,
							known=SOME i},[carrier])
			 fun do_arm(n,body) = 
			     let val ssumtype = make_ssum n
				 val (_,state) = do_vclist state[(bound,ssumtype)]
			     in  (n,do_exp state body)
			     end
			 val arms = map do_arm arms
			 val default = Util.mapopt (do_exp state) default
		     in  Sumsw_e {sumtype=sumtype,bound=bound,arg=arg,result_type=result_type,
				  arms=arms,default=default}
		     end
	       | Exncase_e {arg,result_type,bound,arms,default} =>
		     let val arg = do_exp state arg
			 val result_type = do_con state result_type
			 fun do_arm(tag,body) = 
			     let val tag = do_exp state tag
				 val tagcon = type_of(state,tag)
				 val con = (case (getexn tagcon) of
						SOME c => c
					      | _ => error "type of tag is not exntag_c")
				 val (_,state) = do_vclist state[(bound,con)]
			     in  (tag,do_exp state body)
			     end
			 val arms = map do_arm arms
			 val default = Util.mapopt (do_exp state) default
		     in  Exncase_e {bound=bound,arg=arg,result_type=result_type,
				     arms=arms,default=default}
		     end
	       | Typecase_e _ => error "typecase not done")

	and do_function (state : state) (Function(effect,recur,vklist,vclist,vlist,e,c)) =
		let val (vklist,state) = do_vklist state vklist
		    val (vclist,state) = do_vclist state vclist
		    val e = do_exp state e
		    val c = do_con state c
		in  Function(effect,recur,vklist,vclist,vlist,e,c)
		end

	and do_bnds(bnds : bnd list, state : state) : bnd list * state = 
	    let val (bnds_list,state) = foldl_acc do_bnd state bnds
	    in  (List.concat bnds_list,state)
	    end

	and do_bnd (bnd : bnd, state : state) : bnd list * state = 
	  let fun exp_b(v,c,e) = let val state = add_var(state,v)
				     val state' = enter_var(state,v)
				     val _ = if (is_pure e) then () else use_var(state,v)
				     val e = do_exp state' e
				     val state = (case e of
						    Var_e _ => add_alias(state,v,false,e)
						  | _ => state)
				     val state = add_con(state,v,c)
				 in  ([Exp_b(v,do_con state' c, e)], state)
				 end
	      fun exp_b_proj(v,c,sumcon,k,labs,reccons,sv) = 
		  let val vars = map (fn l => Name.fresh_named_var
							(Name.label2string l)) labs
		      val _ = do_exp state (Var_e sv)
		      val _ = do_con state sumcon
		      val state = foldl (fn (v,s) => add_var(s,v)) state vars
		      fun folder((v,c,l),s) = 
				let val np = project_sum_record(k, l)
			 	in  (Exp_b(v,c,Prim_e(NilPrimOp np,[sumcon],[Var_e sv])),
				     add_con(s,v,c))
				end
		      val (bnds,state) = Listops.foldl_acc folder state
			                 (Listops.zip3  vars reccons labs)
		      val r = Prim_e(NilPrimOp(record labs), reccons, map Var_e vars)
		      val bnd = Exp_b(v,c,r)
		      val (bnds2,state) = do_bnd(bnd,state)
		      val state = add_alias(state,v,false,r)

		  in  (bnds @ bnds2, state)
		  end
	  in	(case bnd of
		     Exp_b(v,_,e as Prim_e(NilPrimOp (project_sum k),[sumcon],[Var_e sv])) =>
		     let val c = type_of(state,e)
			 fun getrecord (Crecord_c lcons) = SOME(map #2 lcons)
			   | getrecord _ = NONE
			 val sv_con = find_con(state,sv)
			 val fieldcon = 
			     (case reduce state getknownsum sv_con of
				  SOME (tagcount,totalcount,k,carrier) => 
				      if (totalcount = tagcount + 1) then carrier
				      else (case (reduce state getrecord carrier) of
						SOME clist => List.nth(clist, k-tagcount)
					      | NONE => error "sumcon of project_sum reduced to bad sum")
				| NONE => error "sumcon of project_sum not reducible to sum")
			 fun getrecord (Prim_c (Record_c labs, reccons)) = SOME(labs,reccons)
			   | getrecord _ = NONE

		     in  case (reduce state getrecord fieldcon) of
			   SOME(labs,reccons) => exp_b_proj(v,c,sumcon,k,labs,reccons,sv)
			 | NONE => exp_b(v,c,e)
		     end
		   (* anormalizes the components of a record *)
		 | Exp_b(v,_,e as Prim_e(NilPrimOp(record labs),clist,elist)) => 
		     let val c = type_of(state,e)
			 fun folder((Var_e v,c),bnds) = (v, bnds)
			   | folder((e,c),bnds) = 
			 let val v = Name.fresh_named_var "named"
			     val bnd = Exp_b(v,c,e)
			 in  (v, bnd ::bnds)
			 end
			 val (vars,rev_bnds) = foldl_acc folder [] (Listops.zip elist clist)
			 val bnds = rev rev_bnds
			 val (bnds,state) = do_bnds(bnds,state)
			 val state = add_var(state,v)
			 val state' = enter_var(state,v)
			 val _ = app (fn v => use_var(state',v)) vars
			 val c = do_con state c
			 val clist = map (do_con state) clist
			 val e = Prim_e(NilPrimOp(record labs),clist,map Var_e vars)
			 val state = add_alias(state,v,false,e)
			 val bnd = Exp_b(v,c,e)
			 val state = add_con(state,v,c)
		     in  (bnds @ [bnd], state)
		     end

		 (* these 2 cases names inject_sum's argument to a variable *)
		   | Exp_b(v,_,e as Prim_e(NilPrimOp (inject _), _, [Var_e _])) => 
		     let val c = type_of(state,e)
		     in  exp_b(v,c,e)
		     end
		   | Exp_b(v,_,e as Prim_e(NilPrimOp (inject k), clist,[injectee])) => 
		     let val c = type_of(state,e)
			 val injectee_con = type_of(state,injectee)
			 val var = Name.fresh_named_var "named2"
			 val bnd1 = Exp_b(var,injectee_con,injectee)
			 val bnd2 = Exp_b(v,c,Prim_e(NilPrimOp (inject k),
						     clist,[Var_e var]))
			 val state = add_con(state,var,injectee_con)
			 val state = add_con(state,v,c)
		     in  do_bnds([bnd1,bnd2], state)
		     end
		   | Exp_b(v,c,Let_e(_,bnds,e)) => do_bnds(bnds @ [Exp_b(v,c,e)], state)
		   | Exp_b(v,c,e) => let val c = type_of(state,e)
				     in  exp_b(v,c,e)
				     end
		   | Con_b(p,cbnd) => let val (cbnd,state) = do_cbnd(cbnd,state)
				      in  ([Con_b(p,cbnd)], state)
				      end
		   | Fixopen_b vfset =>
		     let val vflist = Sequence.toList vfset
			 val (v_vk_vc_b_c_cw_uw_call,state) = do_uncurry(vflist,state)
			 val state = add_vars(state,map #1 vflist)
			 val state = add_vars(state,map #1 v_vk_vc_b_c_cw_uw_call)
			 val state' = enter_var(state,hd(map #1 vflist))
			 fun mapper (v,vk,vc,vf,b,c,cw,uw,call) = 
			     let fun folder((v,k),state) = add_kind(state,v,k)
				 val state = foldl folder state vk
				 fun folder((v,c),state) = add_con(state,v,c)
				 val state = foldl folder state vc
			     in  (v,do_exp state b, do_con state c,
				  cw, uw, call)
			     end
			 val v_b_c_cw_uw_call = map mapper v_vk_vc_b_c_cw_uw_call
			 fun folder ((v,b,c,cw,uw,call),(uncurry,curry)) =
			     case (lookup_curry(state,v)) of
				 NONE => ((v,cw c b)::uncurry,curry)
			       | SOME (_,v') => 
				     let val uncurried = uw(b,c)
					 val curried = cw c call
					 val _ = use_var(enter_var(state,v),v')
				     in  ((v',uncurried)::uncurry,(v,curried)::curry)
				     end
			 val (uncurries,curries) = foldl folder ([],[]) v_b_c_cw_uw_call
			 val used = orfold (fn (v,_) => is_used_var(state,v)) curries
			     
			 val res as (bnds,state) =   
			     if used
				 then ([Fixopen_b(Sequence.fromList (uncurries@curries))], state)
			     else ([Fixopen_b(Sequence.fromList uncurries)] @
				   (if (null curries) then [] 
				    else [Fixopen_b(Sequence.fromList curries)]), state)
		     in res
		     end
		   | Fixcode_b vfset =>
				let val vflist = Sequence.toList vfset
				    val state = add_vars(state,map #1 vflist)
				    val state' = enter_var(state,hd(map #1 vflist))
				    val vflist = map (fn (v,f) => (v,do_function state' f)) vflist
				in  ([Fixcode_b(Sequence.fromList vflist)], state)
				end
		   | Fixclosure_b (recur,vclset) => 
				let val vcllist = Sequence.toList vclset
				    val state = add_vars(state,map #1 vcllist)
				    val state' = enter_var(state,#1(hd vcllist))
				    fun do_closure {code,cenv,venv,tipe} = 
					let val _ = do_exp state' (Var_e code)
					    val cenv = do_con state' cenv
					    val venv = do_exp state' venv
					    val tipe = do_con state' tipe
					in  {code=code,cenv=cenv,venv=venv,tipe=tipe}
					end
				    val vcllist = map (fn (v,f) => (v,do_closure f)) vcllist
				in  ([Fixclosure_b(recur,Sequence.fromList vcllist)], state)
				end)
	  end
	fun do_import(ImportValue(l,v,c),state) = (ImportValue(l,v,do_con state c), 
						   add_con(state,v,c))
	  | do_import(ImportType(l,v,k),state)  = (ImportType(l,v,do_kind state k), 
						   add_kind(state,v,k))

	fun do_export(ExportValue(l,e,c),state) = (ExportValue(l,do_exp state e,do_con state c), state)
	  | do_export(ExportType(l,c,k),state)  = (ExportType(l,do_con state c,do_kind state k), state)

	fun optimize(MODULE{imports, exports, bnds}) = 
	  let val _ = reset_debug()
	      val state = new_state()
	      val (imports,state) = foldl_acc do_import state imports
	      val (bnds,state) = do_bnds(bnds,state)
	      (* we "retain" the state so that no exports are optimized away *)
	      val state = retain_state state
	      val (exports,state) = foldl_acc do_export state exports
              val bnds = List.filter (bnd_used state) bnds
	  in  MODULE{imports=imports,exports=exports,bnds=bnds}
	  end

end
