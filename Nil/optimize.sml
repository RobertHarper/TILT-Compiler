(*$import Nil NilContext NilUtil Ppnil Normalize OPTIMIZE Stats ExpTable *)
(* A one-pass optimizer with the following goals:
	Convert project_sum to project_sum_record.
	Eliminate dead code.
	Not explode code size through anormalization.
	Recognize certain patterns of Sumsw and convert to Intsw
*)	

structure Optimize
    :> OPTIMIZE =
struct

	open Util Nil NilUtil NilContext Listops
 	val error = fn s => Util.error "optimize.sml" s

	val debug = Stats.ff("Optimize_debug")
	val do_diag = ref false
	fun diag s = if !do_diag then print s else()

	val do_lift_array = ref true
	val do_dead = ref true
	val do_proj = ref true
	val do_uncurry = ref true
	val do_cse = ref true

	val local_sub = Name.fresh_named_var "local_subscript"
	val local_update = Name.fresh_named_var "local_update"
	val local_array = Name.fresh_named_var "local_array"
	val local_len = Name.fresh_named_var "local_len"

	val local_bnds = 
	    let val c = Name.fresh_named_var "len_type"
		val array = Name.fresh_named_var "len_array"
		val body = Prim_e(PrimOp(Prim.length_table (Prim.OtherArray false)), [Var_c c], 
				  [Var_e array])
		val len_fun = Function(Partial,Leaf,[(c,Type_k)],false,
				       [(array,Prim_c(Array_c,[Var_c c]))], [],
				       body, Prim_c(Int_c Prim.W32, []))
		val c = Name.fresh_named_var "subscript_type"
		val array = Name.fresh_named_var "subscript_array"
		val index = Name.fresh_named_var "subscript_index"
		val body = Prim_e(PrimOp(Prim.sub (Prim.OtherArray false)), [Var_c c], 
				  [Var_e array, Var_e index])
		val sub_fun = Function(Partial,Leaf,[(c,Type_k)],false,
				       [(array,Prim_c(Array_c,[Var_c c])),
					(index,Prim_c(Int_c Prim.W32 ,[]))], [],
				       body, Var_c c)
		val c = Name.fresh_named_var "update_type"
		val array = Name.fresh_named_var "update_array"
		val index = Name.fresh_named_var "update_index"
		val item = Name.fresh_named_var "update_item"
		val body = Prim_e(PrimOp(Prim.update (Prim.OtherArray false)), [Var_c c], 
				  [Var_e array, Var_e index, Var_e item])
		val update_fun = Function(Partial,Leaf,[(c,Type_k)],false,
					  [(array,Prim_c(Array_c,[Var_c c])),
					   (index,Prim_c(Int_c Prim.W32 ,[])),
					   (item,Var_c c)], [],
					  body, Var_c c)
		val c = Name.fresh_named_var "array_type"
		val size = Name.fresh_named_var "array_size"
		val item = Name.fresh_named_var "array_item"
		val body = Prim_e(PrimOp(Prim.create_table (Prim.OtherArray false)), [Var_c c], 
				  [Var_e size, Var_e item])
		val array_fun = Function(Partial,Leaf,[(c,Type_k)],false,
					 [(size,Prim_c(Int_c Prim.W32 ,[])),
					  (item,Var_c c)], [],
					 body, Prim_c(Array_c,[Var_c c]))
	    in  [Fixopen_b (Sequence.fromList [(local_sub,sub_fun)]),
		 Fixopen_b (Sequence.fromList [(local_len,len_fun)]),
		 Fixopen_b (Sequence.fromList [(local_update,update_fun)]),
		 Fixopen_b (Sequence.fromList [(local_array,array_fun)])]
	    end
		
	(* A transformation state is threaded through the optimizer maintaining:
		(1) whether we are currently in a type (as opposed to constructor)
		(2) a typing context including term and type variables
		(3) an indicator of what binding we are currently in so that
		     uses of variables can be attributed to this binding
		(4) a mapping from variables to an entry which states
		    (a) whether a variable has been 
			(i) definitely used (as constructor or type)
			(ii) possibly used
			(iii) unused
		    (b) possibly equivalent type/term expression 
			(i) unknown - no information is kept for this variable
			(ii) optionalE - an equivalent term expression which may be used
			(iii) mustE - an equivalent term expression which must be used
			(iv) optionalC - an equialent type expression which may be used
			(iv) mustC - an equialent type expression which may be used
		    (c) possibly uncurried function with the curry depth
	  
		(1) allows some reification to occur
		(2) allows type reduction and some code transformation to occur
	        (3) and (4.a) together allow cascading dead code to be eliminated
		(4.b) allows sum and record projections to be optimized
		(4.c) allows functions to be uncurried
        *)

	datatype used_state = UNUSED | USED | TYPE_USED 
	                    | DEFER of (bool * used_state ref) list
	datatype equivalent = UNKNOWN 
	                    | OPTIONALe of exp | MUSTe of exp
	                    | OPTIONALc of con | MUSTc of con
	      
	local
	  type entry = used_state ref * equivalent * (int * var) option
	  datatype state = STATE of {intype : bool,
				     equation : NilContext.context,
				     current : used_state ref,
				     mapping : entry Name.VarMap.map,
				     avail : var ExpTable.Expmap.map * 
				             var ExpTable.Conmap.map}

	  fun isused r = 
	      let fun loop current [] = current
		    | loop current ((intype,l)::rest) = 
		      (case (isused l) of
			   USED => USED
			 | TYPE_USED => if intype then loop TYPE_USED rest else USED
			 | UNUSED => loop current rest
			 | _ => error "got defer")
	      in  case (!r) of
		  USED => USED
		| TYPE_USED => TYPE_USED
		| UNUSED => UNUSED
		| DEFER ls => let val use = loop UNUSED ls
				  val _ = r := use
			      in  use
			      end
	      end

	  fun update_mapping(STATE{intype, equation, current, avail, ...}, mapping) =
			STATE{intype=intype,equation=equation, avail = avail,
			      current=current,mapping=mapping}

	in
	  type state = state
	  fun new_state() = STATE {intype = false,
				   equation = NilContext.empty(),
				   current = ref USED,
				   avail = (ExpTable.Expmap.empty,
					    ExpTable.Conmap.empty),
				   mapping = Name.VarMap.empty}
				   
	  fun retain_state(STATE{intype, equation, current, mapping, avail}) =
			STATE{intype=intype,equation=equation, avail=avail,
			      current=ref USED,mapping=mapping}

	  fun type_state(STATE{intype, equation, current, mapping, avail}) =
			STATE{intype=true,equation=equation, avail = avail,
			      current=current,mapping=mapping}

	  fun enter_var(STATE{intype, equation, current, mapping, avail}, v) =
		STATE{intype = intype,
		      equation=equation,
		      mapping = mapping,
		      avail = avail,
		      current=case (Name.VarMap.find(mapping,v)) of
				NONE => error "enter_var given var not in used map"
			      | SOME (us,_,_) => us}

	  fun add_vars(state as STATE{mapping,...},vars) =
		let val r = ref UNUSED
		    val mapping = foldl (fn (v,m) => Name.VarMap.insert(m,v,(r,UNKNOWN,NONE))) mapping vars
		in  update_mapping(state,mapping)
		end
	  fun add_var(state,v) = add_vars(state,[v])

	  fun use_var(STATE{intype,mapping,current,...},v) = 
	  	(case Name.VarMap.find(mapping,v) of
		  NONE => ()
		| SOME (r,_,_) =>
		   (case !r of
			USED => ()
		      | TYPE_USED => r := DEFER[(intype,current), (true, ref USED)]
		      | UNUSED => r := DEFER[(intype,current)]
		      | DEFER ls => r := DEFER ((intype,current)::ls)))

          fun get_varuse(STATE{mapping,...},v) = 
		case Name.VarMap.find(mapping,v) of
		  NONE => (print "is_used_var given var not in state: ";
		  	   Ppnil.pp_var v; print "\n";
			   error "is_used_var given var not in state")
		| SOME (r,_,_) => (case (!do_dead,isused r) of
				       (false,_) => USED
				     | (_,use) => use)
          fun is_used_var(state,v) = (case get_varuse(state,v) of
					  USED => true
					| TYPE_USED => true
					| UNUSED => false)

	  fun add_aliases(state as STATE{mapping,...},aliases) =
	      let fun folder((v,alias),m) =
		  let val SOME(use,_,uncurry) = Name.VarMap.find(m,v)
		  in  Name.VarMap.insert(m,v,(use,alias,uncurry))
		  end
		  val mapping = foldl folder mapping aliases
	      in  update_mapping(state,mapping)
	      end
	  fun add_alias(s,v,info) = add_aliases(s,[(v,info)])

	  fun add_curry(state as STATE{mapping,...},v_curry,depth,v_uncurry) =
		  let val SOME(use,alias,uncurry) = Name.VarMap.find(mapping,v_curry)
		      val uncurry = SOME(depth,v_uncurry)
		      val mapping = Name.VarMap.insert(mapping,v_curry,(use,alias,uncurry))
		  in  update_mapping(state,mapping)
		  end

	  fun lookup_alias(STATE{mapping,...},v) = 
	      (case (Name.VarMap.find(mapping,v)) of
		   NONE => UNKNOWN
		 | SOME (_,alias,_) => alias)

	  fun lookup_curry(STATE{mapping,...},v) = 
	      (case (Name.VarMap.find(mapping,v)) of
		   NONE => NONE
		 | SOME (_,_,info) => info)

	  fun find_availC(STATE{avail,...},c) = 
	      if (!do_cse) then ExpTable.Conmap.find(#2 avail,c) else NONE
	  fun add_availC(state as STATE{intype,mapping,current,equation,avail},c,v) = 
	      if (!do_cse)
		  then STATE{intype=intype,mapping=mapping,current=current,
			     equation=equation,
			     avail=(#1 avail,
				    ExpTable.Conmap.insert(#2 avail,c,v))}
	      else state

	  fun find_availE(STATE{avail,...},e) = 
	      if (!do_cse) then ExpTable.Expmap.find(#1 avail,e) else NONE
	  fun add_availE(state as STATE{intype,mapping,current,equation,avail},e,v) = 
	      if (!do_cse andalso (not (NilUtil.effect e))
		  andalso (case e of
			       Prim_e(NilPrimOp (unbox_float _), _, _) => false
			     | _ => true))
		  then STATE{intype=intype,mapping=mapping,current=current,
			     equation=equation,
			     avail=(ExpTable.Expmap.insert(#1 avail,e,v),
				    #2 avail)}
	      else state

	  fun lookup_proj(state,v,labs) = 
	      let fun loop e [] = SOME e
		    | loop (Prim_e(NilPrimOp(record labs),_,elist)) (l::rest) = 
		         loop (valOf(assoc_eq(Name.eq_label,l,zip labs elist))) rest
		    | loop (Var_e v) labs = lookup_proj(state,v,labs)
		    | loop _ _ = NONE
	      in  (case lookup_alias(state,v) of
		       OPTIONALe e => loop e labs
		     | MUSTe e => loop e labs
		     | _ => NONE)
	      end
	  fun lookup_cproj(state,v,labs) = 
	      let fun loop c [] = SOME c
		    | loop (Crecord_c lclist) (l::rest) =
		         loop (valOf(assoc_eq(Name.eq_label,l,lclist))) rest
		    | loop (Var_c v) labs = lookup_cproj(state,v,labs)
		    | loop _ _ = NONE
	      in  (case lookup_alias(state,v) of
		       OPTIONALc c => loop c labs
		     | MUSTc c => loop c labs
		     | _ => NONE)
	      end

	  fun get_env(STATE{equation,...}) = equation
	  fun find_equation(STATE{equation,...},c) = NilContext.find_kind_equation(equation,c)
	  fun find_con(STATE{equation,...},v) = NilContext.find_con(equation,v)

	  fun add_kind(STATE{avail,intype,equation,current,mapping},v,k) = 
	      STATE{equation=NilContext.insert_kind(equation,v,k),
		    intype=intype,
		    avail=avail,
		    mapping=mapping,
		    current=current}
	  fun add_con(STATE{avail,equation,mapping,current,intype},v,c) = 
	      (STATE{equation=NilContext.insert_con(equation,v,c),
		    intype=intype,
		     avail=avail,
		    mapping=mapping,
		    current=current})
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



	fun is_sumsw_int state (Switch_e(Sumsw_e{sumtype,bound,arg,arms,default})) = 
	    (case (arg,arms,default) of
		 (Prim_e(PrimOp(Prim.eq_int is),[],
			 [Var_e v,Const_e (Prim.int(_,w))]),
		  [(0w0,zeroexp), (0w1,oneexp)],
		  NONE) => SOME (is,v,TilWord64.toUnsignedHalf w,zeroexp,oneexp)
		 | _ => NONE)
	  | is_sumsw_int state _ = NONE

	fun convert_sumsw state sum_sw =
	    let val exp = Switch_e(Sumsw_e sum_sw)
	    in  (case is_sumsw_int state exp of
	         SOME (is,commonv,_,_,_) =>
		     let fun loop acc (e : exp) =
			 (case is_sumsw_int state e of
			      NONE => (acc,e)
			    | SOME (is,v,w,zeroexp,oneexp) =>
				  if (Name.eq_var(v,commonv))
				      then loop ((w,oneexp)::acc) zeroexp
				  else (acc,e))
			 val (clauses,base) = loop [] exp
		     in  if (length clauses > 1)
			     then Intsw_e{size=is,arg=Var_e commonv,
					  arms=rev clauses,default=SOME base}
			 else Sumsw_e sum_sw
		     end
	       | _ => Sumsw_e sum_sw)
	    end	     


	fun cbnd_used' state cbnd = get_varuse(state, #1(extractCbnd cbnd))
	fun cbnd_used state cbnd = is_used_var(state, #1(extractCbnd cbnd))

	fun bnd_used state bnd = 
	    (case bnd of
		 (Con_b(_,cb)) => (case (cbnd_used' state cb) of
				       UNUSED => NONE
				     | TYPE_USED => SOME(Con_b(Compiletime,cb))
				     | USED => SOME(Con_b(Runtime,cb)))
	       | (Exp_b(v,_,e)) => if is_used_var(state,v) then SOME bnd else NONE
	       | (Fixopen_b vfset) => 
		     if orfold (fn (v,_) => is_used_var(state,v)) (Sequence.toList vfset)
			 then SOME bnd else NONE
	       | (Fixcode_b vfset) => 
			     if orfold (fn (v,_) => is_used_var(state,v)) (Sequence.toList vfset)
			 then SOME bnd else NONE
	       | (Fixclosure_b(_,vclset)) => 
			     if orfold (fn (v,_) => is_used_var(state,v)) 
				 (Sequence.toList vclset)
				 then SOME bnd else NONE)

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

	and do_type (state : state) (con : con) : con = do_con (type_state state) con

	and do_con (state : state) (con : con) : con =
	   (case con of
		Prim_c(pc,clist) => Prim_c(pc, map (do_con state) clist)
	      | Mu_c(recur,vc_seq) => Mu_c(recur,Sequence.map
					   (fn (v,c) => (v,do_con state c)) vc_seq)
	      | ExternArrow_c(clist,c) =>
		    ExternArrow_c(map (do_con state) clist, do_con state c)
	      | AllArrow_c(openness,effect,vklist,vlistopt,clist,numfloats,c) =>
			let val (vklist,state) = do_vklist state vklist
			    val (vlistopt,clist,state) = 
				case vlistopt of
				    SOME vars => let val (vclist,state) = do_vclist state (Listops.zip vars clist)
						 in  (SOME(map #1 vclist), map #2 vclist, state)
						 end
				  | NONE => (NONE, map (do_con state) clist, state)
			in  AllArrow_c(openness,effect,vklist,
				       vlistopt,clist, numfloats, do_con state c)
			end
	      | Var_c v => 
			 (case lookup_alias(state,v) of
				  OPTIONALc c => (use_var(state,v); con)
				| MUSTc c => do_con state c
				| _ => (use_var(state,v); con))
	      | Crecord_c lclist => Crecord_c(map (fn (l,c) => (l, do_con state c)) lclist)
	      | Proj_c(Var_c v, l) =>
			 (case (lookup_cproj(state,v,[l])) of
			      NONE => Proj_c(do_con state (Var_c v), l)
			    | SOME c => do_con state c)
	      | Proj_c(c,l) => Proj_c(do_con state c, l)
	      | Typeof_c e => Typeof_c(do_exp state e)
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
		        in  case cbnds of
			    [] => c
			  | _ => Let_c(letsort,cbnds,c)
			end)

	and do_cbnd(cbnd : conbnd, state : state) : conbnd * state = 
	   (case cbnd of
		Con_cb(v,c) => 
		    let val state = add_var(state,v)
			val state' = enter_var(state,v)
			val c = do_type state' c
			val c =
			    (case find_availC(state,c) of
				 NONE => c
			       | SOME v' => let val _ = use_var(state',v')
					    in  Var_c v'
					    end)
			val alias = (case c of
					 Var_c _ => MUSTc c
				       | _ => OPTIONALc c)
			val state = (case c of
					 Var_c _ => state
				       | _ => add_availC(state, c, v))
			val state = add_kind(state,v,Singleton_k c)
			val state = add_alias(state,v,alias)
		    in  (Con_cb(v,c), state)
		    end
	      | Open_cb(v,vklist,c,k) => let val state = add_var(state,v)
					     val state' = enter_var(state,v)
					     val state = add_kind(state,v,
								  Singleton_k (#2(extractCbnd cbnd)))
					     val (vklist,state') = do_vklist state' vklist
					 in  (Open_cb(v,vklist,
						      do_con state' c, do_kind state' k), state)
					 end
	      | Code_cb(v,vklist,c,k) => let val state = add_var(state,v)
					     val state' = enter_var(state,v)
					     val state = add_kind(state,v,
								  Singleton_k (#2(extractCbnd cbnd)))
					     val (vklist,state') = do_vklist state' vklist
					 in  (Code_cb(v,vklist, 
						do_con state' c, do_kind state' k), state)
					 end)

	and extract_uncurry state (openness,curry_name,uncurry_name,function) = 
	    let fun separate(Function(effect,recur,vklist,dep,vclist,vflist,e,c)) = 
		  let val vklist = map (fn (v,k) => (v,do_kind state k)) vklist
		      val vclist = map (fn (v,c) => (v,do_type state c)) vclist
		  in  (e, c, vklist, vclist, vflist,
		       fn c => fn e => Function(effect,recur,vklist,dep,vclist,vflist,e,c))
		  end
	        fun is_lambda (Let_e(_,[Fixopen_b vfset],Var_e v)) = 
		    (case (Sequence.toList vfset) of
			 [(v',f)] => if (Name.eq_var(v,v') andalso (!do_uncurry))
					 then SOME(v,f)
				     else NONE
			| _ => NONE)
		  | is_lambda _ = NONE
		fun loop (depth,alias,funcons,curry_call,args,cwrapper_opt,v,function) = 
		    let val (body,con,vk,vc,vf,wrapper) = separate function
			val alias = if (depth>1) then (v,MUSTe curry_call)::alias else alias
			val funcon = NilUtil.function_type Open function
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
			fun cwrapper_nonbase c e = cwrapper_base (do_type state con) e
			fun base() = 
			    let 
				val (vk,vc,vf) = args
				val uncurry_call = App_e(openness,Var_e uncurry_name, 
							 map (Var_c o #1) vk,
							 map (Var_e o #1) vc,
							 map Var_e vf)
				fun uwrapper(body,con) = Function(Partial,Arbitrary,vk,false,
								  vc,vf,body,con)
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

	and uncurry(vflist,state) = 
	  let fun folder ((v,f),(acc,state)) = 
	        let (* v' is the name of the uncurried function *)
		    val v' = Name.fresh_named_var ((Name.var2name v) ^ "_uncurry")
		    val (depth,uncurry_call,vk,vc,vf,body,con,aliases,funcons,
			 cwrapper,uwrapper) = extract_uncurry state (Open,v,v',f)
		    val temp = (v,vk,vc,vf,body,con,cwrapper,uwrapper,uncurry_call)
		    val state = add_con(state,v,NilUtil.function_type Open f)

		    val state = 
			if (depth>1)
			    then 
				let
				    val state = foldl (fn ((v,c),s) => add_con(s,v,c)) state funcons
				    val state = add_var(state,v)
				    val state = add_var(state,v')
				    val state = foldl (fn ((v,_),state) => add_var(state,v)) state aliases
				    val state = add_curry(state,v,depth,v')
				in  add_aliases(state,aliases)
				end
			else state
		in   (temp::acc,state)
		end
	  in  foldl folder ([],state) vflist
	  end



(*
	and do_exp (state : state) (exp : exp) : exp = 
	    let val _ = push_exp exp
		val result = do_exp' state exp
		val _ = pop_exp exp
	    in  result
	    end 
*)

	and do_aggregate (state : state) funname_opt (constr,t,clist,elist)  = 
	    let open Prim
		fun helper is_array c = 
		(case (is_array,Normalize.reduce_hnf(get_env state, c)) of
		     (true,(_, Prim_c(Int_c is, _))) => (IntArray is,[]) 
		   | (false,(_, Prim_c(Int_c is, _))) => (IntVector is,[]) 
		   | (true,(_, Prim_c(Float_c is, _))) => (FloatArray is,[]) 
		   | (false,(_, Prim_c(Float_c is, _))) => (FloatVector is,[]) 
		   | (true, (hnf, _)) => (OtherArray hnf,[c]) 
		   | (false, (hnf, _)) => (OtherVector hnf,[c]))
		val (t,clist) = 
		    (case t of
			 OtherArray _ => helper true (hd clist)
		       | OtherVector _ => helper true (hd clist)
		       | _ => (t,clist))
		val is_unknown = (case t of
				      OtherArray false => true
				    | OtherVector false => true
				    | _ => false)
		val clist = map (do_con state) clist
		val elist = map (do_exp state) elist
	    in  (case (funname_opt,is_unknown andalso !do_lift_array) of
		     (SOME funname, true) => 
			  App_e(Open, do_exp state (Var_e funname), clist, elist, [])
		   | _ => Prim_e(PrimOp(constr t), clist, elist))
	    end
		 
	and do_inject (state : state) (k, clist, elist) = 
	    let fun default() = Prim_e(NilPrimOp (inject k),
				       map (do_con state) clist, 
				       map (do_exp state) elist)
		val elist = 
		    (case elist of
			 [Var_e v] => 
			     (case lookup_alias(state,v) of
				  OPTIONALe (e as (Prim_e(NilPrimOp(record _),_,_))) => [e]
				| MUSTe e => [e]
				| _ => elist)
		       | _ => elist)
	    in  
	     (case elist of
		 [] => do_prim state (NilPrimOp(inject_nonrecord k),clist,[])
	       | [Prim_e(NilPrimOp(record _),_,elist)] =>
		     do_prim state (NilPrimOp(inject_record k),clist,elist)
	       | [Var_e v] => 
		     (case (Normalize.reduce_hnf(get_env state, find_con(state,v))) of
			  (_, Prim_c(Record_c (labs,vlist),cons)) => 
			      let val fields = map (fn l =>
						    Prim_e(NilPrimOp(select l),[],
							   [Var_e v]))
				  labs
				  val r = Prim_e(NilPrimOp(record labs),[],fields)
			      in  do_inject state (k,clist,[r])
			      end
			| (true, _) => do_prim state (NilPrimOp (inject_nonrecord k),clist,elist)
			| _ => default())
	       | _ => default())
	    end


	and do_prim (state : state) (prim, clist, elist) = 
	 let open Prim
	 in  (case (prim,elist) of
		 (NilPrimOp(select l),[e as Var_e v]) => 
		     (case (lookup_proj(state,v,[l])) of
			  NONE => Prim_e(prim,[], [do_exp state e])
			| SOME e => do_exp state e)
	       | (NilPrimOp (inject k),_) => do_inject state (k, clist, elist)
	       | (PrimOp(create_table t), _) => do_aggregate state (SOME local_array) (create_table,t,clist,elist)
	       | (PrimOp(create_empty_table t), _) => do_aggregate state NONE
			                                  (create_empty_table,t,clist,elist)
	       | (PrimOp(sub t), _) => do_aggregate state (SOME local_sub) (sub,t,clist,elist)
	       | (PrimOp(update t), _) => do_aggregate state (SOME local_update) (update,t,clist,elist)
	       | (PrimOp(length_table t), _) => do_aggregate state (SOME local_len) (length_table,t,clist,elist)
	       | _ => Prim_e(prim,map ((if Normalize.allprim_uses_carg prim 
					    then do_con else do_type)
				       state) clist, 
			     map (do_exp state) elist))
	 end


	and do_exp (state : state) (exp : exp) : exp = 
	   ((* print "do_exp doing "; Ppnil.pp_exp exp; print "\n";  *)
	    case exp of
		  Var_e v =>
			 (case lookup_alias(state,v) of
				  OPTIONALe e => (use_var(state,v); exp)
				| MUSTe e => do_exp state e
				| _ => (use_var(state,v); exp))
		| Const_e v => 
		         (case v of
			      Prim.int _ => exp
			    | Prim.uint _ => exp
			    | Prim.float _ => exp
			    | Prim.array (c,a) => 
				  let val _ = Array.modify (do_exp state) a
				  in  Const_e(Prim.array(do_con state c, a))
				  end
			    | Prim.vector (c,a) =>
				  let val _ = Array.modify (do_exp state) a
				  in  Const_e(Prim.vector(do_con state c, a))
				  end
			    | Prim.refcell _ => exp
			    | Prim.tag _ => exp)
		| Prim_e(p,clist,elist) => do_prim state (p,clist,elist)
		| Switch_e sw => Switch_e(do_switch state sw)
		| Let_e (letsort,bnds,e) => 
			let (* we must put a wrapper in order to perform the filter *)
			    val state = retain_state state
			    val (bnds,state) = do_bnds(bnds,state)
			    val e = do_exp state e
			    val bnds = List.mapPartial (bnd_used state) bnds
		        in  NilUtil.makeLetE letsort bnds e
			end
		| ExternApp_e(f,elist) =>
			ExternApp_e(do_exp state f, map (do_exp state) elist)
		| App_e(openness,f,clist,elist,eflist) => 
		        let fun extract acc (Var_e v) = 
			         (case (lookup_alias(state,v)) of
				      OPTIONALe e => extract acc e
				    | MUSTe e => extract acc e
				    | _ => SOME(v,acc))
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
					NONE => (diag "could not find uncurry\n"; default())
				      | SOME (depth,v') => 
					    if (length args < depth)
						then (diag "depth too low\n"; default())
					    else uncurry (depth, v',args))
			end

		| Raise_e(e,c) => Raise_e(do_exp state e, do_type state c)
		| Handle_e(e,v,handler) => 
			let val ([(v,_)],state) = do_vclist state [(v,Prim_c(Exn_c,[]))]
			in  Handle_e(do_exp state e, v, do_exp state handler)
			end)

	and do_switch (state : state) (switch : switch) : switch = 
	    (case switch of
		 Intsw_e {size,arg,arms,default} =>
		     let val arg = do_exp state arg
			 val arms = map_second (do_exp state) arms
			 val default = Util.mapopt (do_exp state) default
		     in  Intsw_e {size=size,arg=arg,
				  arms=arms,default=default}
		     end
	       | Sumsw_e {sumtype,arg,bound,arms,default} =>
		     let val arg = do_exp state arg
			 val sumtype = do_type state sumtype
			 val (tagcount,_,carrier) = Normalize.reduceToSumtype(get_env state,sumtype) 
		 	 val totalcount = TilWord32.uplus(tagcount,TilWord32.fromInt(length carrier))	
			 fun make_ssum i = Prim_c(Sum_c{tagcount=tagcount,
							totalcount=totalcount,
							known=SOME i},
						        case carrier of
							  [_] => carrier
							 | _ => [con_tuple_inject carrier])
			 fun do_arm(n,body) = 
			     let val ssumtype = make_ssum n
				 val (_,state) = do_vclist state[(bound,ssumtype)]
			     in  (n,do_exp state body)
			     end
			 val arms = map do_arm arms
			 val default = Util.mapopt (do_exp state) default
		     in  Sumsw_e {sumtype=sumtype,bound=bound,arg=arg,
				  arms=arms,default=default}
		     end
	       | Exncase_e {arg,bound,arms,default} =>
		     let val arg = do_exp state arg
			 fun do_arm(tag,body) = 
			     let val tag = do_exp state tag
				 val tagcon = type_of(state,tag)
				 val Prim_c(Exntag_c, [con]) = tagcon
				 val (_,state) = do_vclist state[(bound,con)]
			     in  (tag,do_exp state body)
			     end
			 val arms = map do_arm arms
			 val default = Util.mapopt (do_exp state) default
		     in  Exncase_e {bound=bound,arg=arg,
				     arms=arms,default=default}
		     end
	       | Typecase_e _ => error "typecase not done")

	and do_function (state : state) (Function(effect,recur,vklist,dep,vclist,vlist,e,c)) =
		let val (vklist,state) = do_vklist state vklist
		    val (vclist,state) = do_vclist state vclist
		    val e = do_exp state e
		    val c = do_type state c
		in  Function(effect,recur,vklist,dep,vclist,vlist,e,c)
		end

	and do_bnds(bnds : bnd list, state : state) : bnd list * state = 
	    let val (bnds_list,state) = foldl_acc do_bnd state bnds
	    in  (List.concat bnds_list,state)
	    end

	and do_bnd (bnd : bnd, state : state) : bnd list * state = 
	  let 
	      (* Rewrite a binding to do the following:
	         (a) name record arguments
		 (b) name inject argument
		 (c) rewrite project_sum to project_sum_record or project_sum_nonrecord
		 If NONE is returned, the binding does not need to be rewrritten.
		 Otherwise, SOME bindings are returned.  The original bindings
		   is guaranteed not to occur in this list.  This prevents looping. *)
	      fun rewrite_bnd(v,e) =
		  (case e of
		       Prim_e(NilPrimOp(record labs),_,elist) => 
	                 let fun folder(Var_e v,bnds) = (v, bnds)
			       | folder(e,bnds) = 
			           let val v = Name.fresh_named_var "namedRecComp"
				       val bnd = Exp_b(v,TraceUnknown,e)
				   in  (v, bnd ::bnds)
				   end
			     val (vars,rev_bnds) = foldl_acc folder [] elist
			     val bnds = rev rev_bnds
			 in  case bnds of
			       [] => NONE
			     | _ => SOME(bnds @ [Exp_b(v,TraceUnknown,Prim_e(NilPrimOp(record labs),[],map Var_e vars))])
			 end
		     | Prim_e(NilPrimOp (inject _), _, [Var_e _]) => NONE
		     | Prim_e(NilPrimOp (inject k), clist,[injectee]) => 
			 let val injectee_con = type_of(state,injectee)
			     val var = Name.fresh_named_var "namedInjectee"
			 in  SOME([Exp_b(var,TraceUnknown,injectee),
				   Exp_b(v,TraceUnknown,Prim_e(NilPrimOp (inject k),clist,[Var_e var]))])
			 end
		     | Prim_e(NilPrimOp (project_sum k),[sumcon],[Var_e sv]) =>
			 let val c = type_of(state,e)
			     val sv_con = find_con(state,sv)
			     val (tagcount,SOME k,clist) = Normalize.reduceToSumtype(get_env state,sv_con)
			     val fieldcon = List.nth(clist, TilWord32.toInt(TilWord32.uminus(k,tagcount)))
			 in  case Normalize.reduce_hnf(get_env state, fieldcon) of
			       (_,Prim_c (Record_c (labs,_), rcons)) => 
				 let val vars = map (fn l => Name.fresh_named_var
						     (Name.label2string l)) labs
				     fun mapper(v,l) =
					 let val np = project_sum_record(k, l)
					 in  Exp_b(v,TraceUnknown,Prim_e(NilPrimOp np,[sumcon],[Var_e sv]))
					 end
				     val bnds = Listops.map2 mapper (vars, labs)
				 in  SOME(bnds @ [Exp_b(v,TraceUnknown,Prim_e(NilPrimOp(record labs), [], map Var_e vars))])
				 end
			     (* not a record for sure *)
			     | (true, _) => SOME[Exp_b(v,TraceUnknown,Prim_e(NilPrimOp (project_sum_nonrecord k),
								[sumcon],[Var_e sv]))]
			     | (_,c) => ((* print "project_sum irreducible...";
					 Ppnil.pp_con c; print "\n"; *)
					 NONE)
			 end
		     | _ => NONE)
	  in	(case bnd of
		     Exp_b(v,niltrace,e) =>
			 ((* print "working on bnd with v = "; Ppnil.pp_var v; print "\n";  *)
			  case rewrite_bnd(v,e) of
			      NONE => 
				  let val c = type_of(state,e)
				      val state = add_var(state,v)
				      val state' = enter_var(state,v)
				      val _ = if (NilUtil.effect e) then use_var(state,v) else ()
				      val e = do_exp state' e
				      val e =
					  (case find_availE(state,e) of
					       NONE => e
					     | SOME v' => let val _ = use_var(state',v')
							  in  Var_e v'
							  end)
				      val alias = (case e of
						       Var_e _ => MUSTe e
						     | _ => OPTIONALe e)
				      val state = (case e of
						       Var_e _ => state
						     | _ => add_availE(state, e, v))
				      val state = add_alias(state,v,alias)
				      val state = add_con(state,v,c)
				  in  ([Exp_b(v, niltrace, e)], state)
				  end
			    | SOME bnds => do_bnds(bnds,state))
		   | Con_b(p,cbnd) => let (* val _ = (print "working on bnd with v = "; 
						   Ppnil.pp_var (#1(extractCbnd cbnd)); 
						   print "\n") *)
					  val (cbnd,state) = do_cbnd(cbnd,state)
				      in  ([Con_b(p,cbnd)], state)
				      end
		   | Fixopen_b vfset =>
		     let val vflist = Sequence.toList vfset
			 val (v_vk_vc_b_c_cw_uw_call,state) = uncurry(vflist,state)
			 val state = add_vars(state,map #1 vflist)
			 val state = add_vars(state,map #1 v_vk_vc_b_c_cw_uw_call)
			 val state' = enter_var(state,hd(map #1 vflist))
			 fun mapper (v,vk,vc,vf,b,c,cw,uw,call) = 
			     let fun folder((v,k),state) = add_kind(state,v,do_kind state k)
				 val state = foldl folder state vk
				 fun folder((v,c),state) = add_con(state,v,do_con state c)
				 val state = foldl folder state vc
			     in  (v,do_exp state b, do_type state c,
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
					    val tipe = do_type state' tipe
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

	fun do_export(ExportValue(l,e),state) = (ExportValue(l,do_exp state e), state)
	  | do_export(ExportType(l,c),state)  = (ExportType(l,do_con state c), state)

	fun optimize {lift_array, dead, projection, uncurry, cse} 
	              (MODULE{imports, exports, bnds}) =
	  let 
	      val _ = do_dead := dead
	      val _ = do_proj := projection
	      val _ = do_uncurry := uncurry
	      val _ = do_cse := cse
	      val _ = reset_debug()
	      val state = new_state()
	      val (imports,state) = foldl_acc do_import state imports

	      val _ = do_lift_array := false
	      val (local_bnds,state) = if lift_array
					   then do_bnds(local_bnds,state)
				       else ([],state)
	      val _ = do_lift_array := lift_array
	      val (bnds,state) = do_bnds(bnds,state)
	      val bnds = local_bnds @ bnds

	      (* we "retain" the state so that no exports are optimized away *)
	      val state = retain_state state
	      val (exports,state) = foldl_acc do_export state exports
              val bnds = List.mapPartial (bnd_used state) bnds
	  in  MODULE{imports=imports,exports=exports,bnds=bnds}
	  end

end
