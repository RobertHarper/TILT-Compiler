(*$import Nil NilContext NilUtil Ppnil Normalize OPTIMIZE Stats ExpTable TraceOps *)
(* A one-pass optimizer with the following goals:
	Convert project_sum to project_sum_record.
        Fold constant expressions
        Propagate constants
	Eliminate dead code.
	Not anormalize (old fear of classifier sizes) 
	Recognize certain patterns of Sumsw and convert to Intsw
*)	

structure Optimize
    :> OPTIMIZE =
struct

	open Util Nil NilUtil Listops
 	val error = fn s => Util.error "optimize.sml" s

	val debug = Stats.ff("Optimize_debug")
	val do_diag = ref false
	fun diag s = if !do_diag then print s else()

fun path2con (v,labs) = 
  let fun loop c [] = c
        | loop c (l::rest) = loop (Proj_c(c,l)) rest
  in  loop (Var_c v) labs
  end

	val do_lift_array = ref true
	val do_dead = ref true
	val do_proj = ref true
	val do_uncurry = ref true
	val do_cse = ref true

	val local_sub = Name.fresh_named_var "local_subscript"
	val local_update = Name.fresh_named_var "local_update"
	val local_array = Name.fresh_named_var "local_array"
	val local_len = Name.fresh_named_var "local_len"
	val local_vsub = Name.fresh_named_var "local_vsubscript"
	val local_vector = Name.fresh_named_var "local_vector"
	val local_vlen = Name.fresh_named_var "local_vlen"

	val local_bnds = 
	    let fun make_length (aggregate,constr) = 
		let val c = Name.fresh_named_var "len_type"
		    val agg = Name.fresh_named_var "len_agg"
		    val body = Prim_e(PrimOp(Prim.length_table 
					     (aggregate false)), [Var_c c], 
				      [Var_e agg])
		    val res_var = Name.fresh_named_var "len_result"
		    val body = Let_e(Sequential, [Exp_b(res_var, TraceKnown TraceInfo.Notrace_Int, body)],
						Var_e res_var)
		    val len_fun = Function{effect = Partial, recursive = Leaf, isDependent = false,
					   tFormals = [(c,Type_k)],
					   eFormals = [(agg,TraceUnknown,Prim_c(constr,[Var_c c]))],
					   fFormals = [],
					   body = body, 
					   body_type = Prim_c(Int_c Prim.W32, [])}
		in  len_fun
		end
		val len_fun = make_length(Prim.OtherArray, Array_c)
		val vlen_fun = make_length(Prim.OtherVector, Vector_c)

		fun make_sub (aggregate,constr) = 
		let val c = Name.fresh_named_var "subscript_type"
		    val agg = Name.fresh_named_var "subscript_agg"
		    val index = Name.fresh_named_var "subscript_index"
		    val res_var = Name.fresh_named_var "subscript_result"
		    val body = Prim_e(PrimOp(Prim.sub (aggregate false)), 
				      [Var_c c], 
				      [Var_e agg, Var_e index])
		    val body = Let_e(Sequential, [Exp_b(res_var, TraceKnown (TraceInfo.Compute(c,[])), body)],
						Var_e res_var)
		    val sub_fun = Function{effect = Partial, recursive = Leaf, isDependent = false,
					   tFormals = [(c,Type_k)],
					   eFormals = [(agg, TraceUnknown, Prim_c(constr,[Var_c c])),
						       (index, TraceUnknown, Prim_c(Int_c Prim.W32 ,[]))], 
					   fFormals = [],
					   body = body, 
					   body_type = Var_c c}
		in  sub_fun
		end
		val sub_fun = make_sub(Prim.OtherArray, Array_c)
		val vsub_fun = make_sub(Prim.OtherVector, Vector_c)

		local
		    val c = Name.fresh_named_var "update_type"
		    val array = Name.fresh_named_var "update_array"
		    val index = Name.fresh_named_var "update_index"
		    val item = Name.fresh_named_var "update_item"
		    val body = Prim_e(PrimOp(Prim.update (Prim.OtherArray false)), 
				      [Var_c c], 
				      [Var_e array, Var_e index, Var_e item])
		    val res_var = Name.fresh_named_var "update_result"
		    val body = Let_e(Sequential, [Exp_b(res_var, TraceKnown TraceInfo.Trace, body)],
						Var_e res_var)
		in  val update_fun = Function{effect = Partial, recursive = Leaf, isDependent = false,
					      tFormals = [(c,Type_k)],
					      eFormals = [(array, TraceUnknown, Prim_c(Array_c,[Var_c c])),
							  (index, TraceUnknown, Prim_c(Int_c Prim.W32 ,[])),
							  (item,  TraceUnknown, Var_c c)],
					      fFormals = [],
					      body = body, 
					      body_type = unit_con}
		end

		fun make_aggregate (aggregate,constr) = 
		    let val c = Name.fresh_named_var "agg_type"
			val size = Name.fresh_named_var "agg_size"
			val item = Name.fresh_named_var "agg_item"
			val body = Prim_e(PrimOp(Prim.create_table 
					 (aggregate false)), [Var_c c], 
					  [Var_e size, Var_e item])
		    in  Function{effect = Partial, recursive = Leaf, isDependent = false,
				 tFormals = [(c,Type_k)],
				 eFormals = [(size, TraceUnknown, Prim_c(Int_c Prim.W32 ,[])),
					     (item, TraceUnknown, Var_c c)], 
				 fFormals = [],
				 body = body, 
				 body_type = Prim_c(constr,[Var_c c])}
		    end
		val array_fun = make_aggregate(Prim.OtherArray, Array_c)
		val vector_fun = make_aggregate(Prim.OtherVector, Vector_c)

	    in  [Fixopen_b (Sequence.fromList [(local_sub,sub_fun)]),
		 Fixopen_b (Sequence.fromList [(local_vsub,vsub_fun)]),
		 Fixopen_b (Sequence.fromList [(local_len,len_fun)]),
		 Fixopen_b (Sequence.fromList [(local_vlen,vlen_fun)]),
		 Fixopen_b (Sequence.fromList [(local_update,update_fun)]),
		 Fixopen_b (Sequence.fromList [(local_vector,vector_fun)]),
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
			(iv) etaE - an equivalent partially applied curried expression
			(iv) optionalC - an equialent type expression which may be used
			(iv) mustC - an equialent type expression which may be used
	  
		(1) allows some reification to occur
		(2) allows type reduction and some code transformation to occur
	        (3) and (4.a) together allow cascading dead code to be eliminated
		(4.b) allows sum and record projections to be optimized
		(4.c) allows functions to be uncurried
        *)

	datatype used_state = UNUSED 
	                    | USED of int
	                    | DEFER of used_state ref list
	datatype equivalent = UNKNOWN 
	                    | OPTIONALe of exp | MUSTe of exp 
	                    | ETAe of int * var * (con list * exp list * exp list) list
	                    | OPTIONALc of con | MUSTc of con
	      
fun pp_alias UNKNOWN = print "unknown"
| pp_alias (OPTIONALe e) = (print "OPTIONALe "; Ppnil.pp_exp e)
| pp_alias (MUSTe e) = (print "MUSTe "; Ppnil.pp_exp e)
| pp_alias (OPTIONALc c) = (print "OPTIONALc "; Ppnil.pp_con c)
| pp_alias (MUSTc c) = (print "MUSTc "; Ppnil.pp_con c)
| pp_alias (ETAe _) = print "ETAe ..."

	local
	  type entry = used_state ref * equivalent
	  datatype state = STATE of {curry_processed : (Name.VarSet.set * var Name.VarMap.map),
				     equation : NilContext.context,
				     current : used_state ref,
				     mapping : entry Name.VarMap.map,
				     avail : var ExpTable.Expmap.map * 
				             var ExpTable.Conmap.map}
	      
	  fun isused r = 
	      let fun loop count current [] = current
		    | loop count current (l::rest) = 
		      (case (isused l) of
			   USED _ => loop (count+1) (USED(count+1)) rest
			 | UNUSED => loop count current rest
			 | _ => error "got defer")
	      in  (case (!r) of
		       DEFER ls => let val use = loop 0 UNUSED ls
				       val _ = r := use
				   in  use
				   end	
		     | r => r)
	      end

	  fun update_mapping(STATE{equation, current, avail, curry_processed, ...}, mapping) =
			STATE{curry_processed=curry_processed,
			      equation=equation, avail = avail,
			      current=current,mapping=mapping}

	in

	  fun show_pair(v1,v2) = 
	      (Ppnil.pp_var v1; print " -> "; Ppnil.pp_var v2; print "\n")
	  fun show_entry(v,(_,eq)) = 
	      let val _ = (Ppnil.pp_var v; print " -> ")
		  val _ = (case eq of
			       ETAe (depth,_,_) => (print "ETAe "; print (Int.toString depth);
						    print "\n")
			     | _ => print "OTHER\n")
	      in  print "\n"
	      end
	  fun show_mapping(STATE{mapping,curry_processed,...}) = 
	      (Name.VarMap.appi show_entry mapping;
	       Name.VarMap.appi show_pair (#2 curry_processed))

	  type state = state
	  fun new_state() = STATE {curry_processed = (Name.VarSet.empty, Name.VarMap.empty),
				   equation = NilContext.empty(),
				   current = ref (USED 1),
				   avail = (ExpTable.Expmap.empty,
					    ExpTable.Conmap.empty),
				   mapping = Name.VarMap.empty}
				   
	  fun retain_state(STATE{equation, current, mapping, avail, curry_processed}) =
			STATE{equation=equation, avail=avail,
			      curry_processed = curry_processed,
			      current=ref (USED 1),mapping=mapping}


	  fun enter_var(STATE{equation, curry_processed, current, mapping, avail}, v) =
		STATE{equation=equation,
		      mapping = mapping,
		      avail = avail,
		      curry_processed = curry_processed,
		      current=case (Name.VarMap.find(mapping,v)) of
				NONE => error "enter_var given var not in used map"
			      | SOME (us,_) => us}

	  fun add_vars(state as STATE{mapping,...},vars) =
		let val r = ref UNUSED
		    val mapping = foldl (fn (v,m) => Name.VarMap.insert(m,v,(r,UNKNOWN))) mapping vars
		in  update_mapping(state,mapping)
		end
	  fun add_var(state,v) = add_vars(state,[v])

	  fun use_var(STATE{mapping,current,...},v) = 
	  	(case Name.VarMap.find(mapping,v) of
		  NONE => ()
		| SOME (r,_) =>
		   (case !r of
			USED n => r := (USED(n+1))
		      | UNUSED => r := DEFER[current]
		      | DEFER ls => r := DEFER (current::ls)))

          fun get_varuse(STATE{mapping,...},v) = 
		case Name.VarMap.find(mapping,v) of
		  NONE => (print "is_used_var given var not in state: ";
		  	   Ppnil.pp_var v; print "\n";
			   error "is_used_var given var not in state")
		| SOME (r,_) => (case (!do_dead,isused r) of
				       (false,_) => USED 999
				     | (_,use) => use)
          fun is_used_var(state,v) = (case get_varuse(state,v) of
					  USED _ => true
					| UNUSED => false)

	  fun add_alias(state as STATE{mapping,...},v,alias) =
	      let val SOME(use,_) = Name.VarMap.find(mapping,v)
		  val mapping = Name.VarMap.insert(mapping,v,(use,alias))
(*
		  val _ = (print "add_alias for "; Ppnil.pp_var v; 
				print " --> "; pp_alias alias; print "\n")
*)
	      in  update_mapping(state,mapping)
	      end

	  fun lookup_alias(STATE{mapping,...},v) = 
	      (case (Name.VarMap.find(mapping,v)) of
		   NONE => UNKNOWN
		 | SOME (_,alias) => alias)


	  fun find_availC(STATE{avail,...},c) = 
	      if (!do_cse) then ExpTable.Conmap.find(#2 avail,c) else NONE
	  fun add_availC(state as STATE{curry_processed,mapping,current,equation,avail},c,v) = 
	      if (!do_cse)
		  then STATE{mapping=mapping,current=current,
			     equation=equation,
			     curry_processed = curry_processed,
			     avail=(#1 avail,
				    ExpTable.Conmap.insert(#2 avail,c,v))}
	      else state

	  fun find_availE(STATE{avail,...},e) = 
	      if (!do_cse) then ExpTable.Expmap.find(#1 avail,e) else NONE

          fun anormal_valuable (ctxt, App_e(_,Var_e v,_,_,_)) = 
	      (case NilContext.find_con (ctxt,v) of
		   AllArrow_c{effect=Total,...} => true
                 | _ => false)
            | anormal_valuable (ctxt, e) = not (NilUtil.effect e)

	  fun add_availE(state as STATE{mapping,current,curry_processed,equation,avail},e,v) = 
	      if (!do_cse andalso anormal_valuable(equation,e))
		  then STATE{mapping=mapping,current=current,
			     curry_processed = curry_processed,
			     equation=equation,
			     avail=(ExpTable.Expmap.insert(#1 avail,e,v),
				    #2 avail)}
	      else state

	  fun lookup_proj(state,v,labs) = 
	      let fun loop e [] = SOME e
		    | loop (Prim_e(NilPrimOp(record labs),_,elist)) (l::rest) = 
		         let val SOME e = assoc_eq(Name.eq_label,l,zip labs elist)
			 in  loop e rest
			 end
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
		         let val _ = (case assoc_eq(Name.eq_label,l,lclist) of
					  NONE => (print "lookup_cproj could not find ";
						   Ppnil.pp_label l; print "  among ";
						   app (fn (l,_) => (Ppnil.pp_label l; print "  ")) lclist;
						   print "\n")
					| _ => ())
			     val SOME c = assoc_eq(Name.eq_label,l,lclist)
			 in  loop c rest
			 end
		    | loop (Var_c v) labs = lookup_cproj(state,v,labs)
		    | loop _ _ = NONE
	      in  (case lookup_alias(state,v) of
		       OPTIONALc c => loop c labs
		     | MUSTc c => loop c labs
		     | _ => (case find_availC(state,path2con(v,labs)) of
				NONE =>	NONE
			      | SOME v => SOME(Var_c v)))
	      end

	  fun get_env(STATE{equation,...}) = equation
	  fun find_equation(STATE{equation,...},c) = NilContext.find_kind_equation(equation,c)
	  fun find_con(STATE{equation,...},v) = NilContext.find_con(equation,v)

	  fun add_kind(STATE{avail,curry_processed,equation,current,mapping},v,k) = 
	      STATE{equation=NilContext.insert_kind(equation,v,k),
		    avail=avail,
		    curry_processed = curry_processed,
		    mapping=mapping,
		    current=current}
	  
	  fun add_con(STATE{avail,equation,mapping,curry_processed,current},v,c) = 
	      (if (!debug)
		   then (print "optimize: add_con "; Ppnil.pp_var v;
			 print " -> "; Ppnil.pp_con c; print "\n")
	       else ();
	       STATE{equation=NilContext.insert_con(equation,v,c),
		     avail=avail,
		     curry_processed = curry_processed,
		     mapping=mapping,
		     current=current})

	  fun add_exp(STATE{avail,equation,mapping,curry_processed,current},v,e) = 
	      (STATE{equation=NilContext.insert_exp(equation,v,e),
		     avail=avail,
		     curry_processed = curry_processed,
		     mapping=mapping,
		     current=current})

	  fun add_curry_processed(STATE{avail,equation,mapping,curry_processed,current},v) =
	       STATE{equation=equation,
		     avail=avail,
		     curry_processed = (Name.VarSet.add(#1 curry_processed,v), #2 curry_processed),
		     mapping=mapping,
		    current=current}

	  fun add_curry_pair(STATE{avail,equation,mapping,curry_processed,current},
			     curry_name,uncurry_name) =
	       STATE{equation=equation,
		     avail=avail,
		     curry_processed = (#1 curry_processed, Name.VarMap.insert(#2 curry_processed,
									       curry_name,uncurry_name)),
		     mapping=mapping,
		    current=current}

	  fun find_curry_pair(STATE{curry_processed,...},v) = Name.VarMap.find(#2 curry_processed,v)
	  fun is_curry_processed(STATE{curry_processed,...},v) = Name.VarSet.member(#1 curry_processed,v)

	  val Normalize_reduce_hnf = Stats.timer("optimize_reduce_hnf", Normalize.reduce_hnf);
	  val Normalize_reduceToSumtype = Stats.timer("optimize_reduce_tosumtype", Normalize.reduceToSumtype);
	  val Normalize_type_of = Stats.timer("optimize_typeof", Normalize.type_of);
	  fun type_of(STATE{equation,...},e) = Normalize_type_of(equation,e)
	  fun get_trace(STATE{equation,...},t) = TraceOps.get_trace (equation,t)
	end

        (* Helper functions for uncurry optimizations *)
	fun get_lambda (Let_e(_,[Fixopen_b vfset],Var_e v)) =
	    (case (Sequence.toList vfset) of
		 [(v',f)] => if (Name.eq_var(v,v')) 
				then SOME (v,f)
			    else NONE
	       | _ => NONE)
	  | get_lambda _ = NONE

	fun make_lambda (v,f) = Let_e(Sequential,[Fixopen_b(Sequence.fromList[(v,f)])],Var_e v)

	type wrap = var * effect * recursive * (var * kind) list * bool *
	            (var * niltrace * con) list * var list * con
	fun extract_lambdas vf = 
	    let val e = make_lambda vf
		fun loop acc e = 
		    (case get_lambda e of
			 NONE => (rev acc, e)
		       | SOME (v,Function{effect=eff,recursive=r,isDependent=dep,
					  tFormals=vklist,eFormals=vclist,fFormals=vflist,
					  body=body,body_type=con}) =>
			     loop ((v,eff,r,vklist,dep,vclist,vflist,con)::acc) body)
	    in  loop [] e
	    end

	fun create_lambdas ([],_) = error "no wraps to create_lambdas"
	  | create_lambdas (wraps,body) = 
	    let fun loop [] body = body
		  | loop ((v,eff,r,vklist,dep,vclist,vflist,con)::rest) body = 
		          make_lambda(v,Function{effect=eff,recursive=r,isDependent=dep,
						 tFormals=vklist,eFormals=vclist,fFormals=vflist,
						 body=loop rest body,
						 body_type=con})
		val lambda = loop wraps body
		val SOME vf = get_lambda lambda 
	    in  vf
	    end
	    
	fun wraps2args_bnds (wraps : wrap list) = 
	    let fun loop acc ([] : wrap list) = error "no wraps to wraps2args_bnds"
		  | loop acc [_] = rev acc
		  | loop acc ((v,_,_,vklist,dep,vclist,vflist,_)::(rest as (next::_))) =
		    let val vk = map #1 vklist
			val vc = map #1 vclist
			val info = Exp_b(#1 next,TraceKnown TraceInfo.Trace,
					 App_e(Open, Var_e v, map Var_c vk, map Var_e vc, map Var_e vflist))
		    in  loop (info::acc) rest
		    end
		val vklist = Listops.flatten(map #4 wraps)
		val vclist = Listops.flatten(map #6 wraps)
		val vflist = Listops.flatten(map #7 wraps)
	    in  ((vklist,vclist,vflist), loop [] wraps)
	    end

	fun create_flatlambda(_,[],_) = error "no wraps to create_flatlamba"
	  | create_flatlambda(name,wraps,body) = 
	    let val (_,eff,r,_,dep,_,_,con) = List.last wraps
		 val ((vklist,vclist,vflist),_) = wraps2args_bnds wraps
	    in  (name,Function{effect=eff,recursive=r,isDependent=dep,
			       tFormals=vklist,eFormals=vclist,fFormals=vflist,
			       body=body,body_type=con})
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



	fun is_sumsw_int state (Switch_e(Sumsw_e{sumtype,bound,arg,arms,default,...})) = 
	    let	val arg = 
		  (case arg of
		       Var_e v =>
			   (case lookup_alias(state,v) of
				OPTIONALe 
				(e as (Prim_e(PrimOp(Prim.eq_int is),[],
					      [Var_e v,Const_e (Prim.int(_,w))]))) => e
			     | _ => arg)
		     | _ => arg)
	    in
		(case (arg,arms,default) of
		     (Prim_e(PrimOp(Prim.eq_int is),[],
			     [Var_e v,Const_e (Prim.int(_,w))]),
		      [(0w0,_,zeroexp), (0w1,_,oneexp)],
		      NONE) => SOME (is,v,TilWord64.toUnsignedHalf w,zeroexp,oneexp)
		    | _ => NONE)
	    end
	  | is_sumsw_int state _ = NONE

	fun convert_sumsw state (sum_sw as {result_type,...}) =
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
		     in  if (null clauses)
			     then NONE
			 else (SOME{size=is,arg=Var_e commonv,
				    arms=rev clauses,default=SOME base,
				    result_type = result_type})
			 
		     end
	       | _ => NONE)
	    end	     


	fun cbnd_used' state cbnd = get_varuse(state, #1(extractCbnd cbnd))
	fun cbnd_used state cbnd = is_used_var(state, #1(extractCbnd cbnd))

	fun bnd_used state bnd = 
	    (case bnd of
		 (Con_b(p,cb)) => (case (cbnd_used' state cb) of
				       UNUSED => NONE
				     | USED _ => SOME(Con_b(p,cb)))
	       | (Exp_b(v,_,e)) => if is_used_var(state,v) then SOME bnd else NONE
	       | (Fixopen_b vfset) => 
		     let val vflist = Sequence.toList vfset
			 val vflist = List.filter (fn (v,_) => is_used_var(state,v)) vflist
			 fun eliminate_uncurry(a,b,af,Function{body,...}) =
			     let val (wraps,_) = extract_lambdas(a,af)
				 val curry = create_lambdas(wraps,body)
			     in  curry
			     end
			 fun loop [] = []
			   | loop (ls as [_]) = ls
			   | loop ((a,af)::(b,bf)::rest) = 
			     (case find_curry_pair(state,a) of
				  NONE => (a,af)::loop((b,bf)::rest)
				| SOME v => 
				      (if (Name.eq_var(v,b) andalso 
					   (case (get_varuse(state,b)) of
						USED 1 => true
					      | USED n => false
					      | UNUSED => false
					      | _ => false))
					  then (eliminate_uncurry(a,b,af,bf))::(loop rest)
				      else (a,af)::loop((b,bf)::rest)))
			 val vflist = loop vflist
		     in  (case vflist of
			      [] => NONE
			    | _ => SOME(Fixopen_b (Sequence.fromList vflist)))
		     end
	       | (Fixcode_b vfset) => 
		     let val vflist = Sequence.toList vfset
			 val vflist = List.filter (fn (v,_) => is_used_var(state,v)) vflist
		     in  (case vflist of
			      [] => NONE
			    | _ => SOME(Fixcode_b (Sequence.fromList vflist)))
		     end
	       | (Fixclosure_b(recur,vclset)) => 
		     let val vcllist = Sequence.toList vclset
			 val vcllist = List.filter (fn (v,_) => is_used_var(state,v)) vcllist
		     in  (case vcllist of
			      [] => NONE
			    | _ => SOME(Fixclosure_b (recur,Sequence.fromList vcllist)))
		     end)

	fun do_vklist state vklist =
	    let fun folder((v,k),state) = let val k = do_kind state k
					  in  ((v,k),add_kind(state,v,k))
					  end
	    in  foldl_acc folder state vklist
	    end
	and do_vc ((v,c), state) =  let val c = do_con state c
				    in  ((v,c),add_con(state,v,c))
				    end
	and do_voptc ((vopt,c), state) = let val c = do_con state c
					     val state = (case vopt of 
							      NONE => state
							    | SOME v => add_con(state,v,c))
					 in  ((vopt,c),state)
					 end
	and do_vtrc ((v,tr,c),state) =  let val c = do_con state c
					    val state = add_con(state,v,c)
					in  ((v,tr,c),state)
					end
	and do_vclist state vclist = foldl_acc do_vc state vclist
	and do_voptclist state vclist = foldl_acc do_voptc state vclist
	and do_eFormals state eFormals = foldl_acc do_vtrc state eFormals


	and do_kind (state : state) (kind : kind) : kind = 
	  (case kind of 
		Type_k => kind
              | SingleType_k c => SingleType_k(do_con state c)
              | Single_k c => Single_k(do_con state c)
  	      | Record_k(lvk_seq) => let fun folder(((l,v),k),state) = (((l,v),do_kind state k),
									add_kind(state,v,k))
				     in  Record_k(#1(Sequence.foldl_acc folder state lvk_seq))
				     end
	      | Arrow_k(openness,vklist,k) => let val (vklist,state) = do_vklist state vklist
					      in  Arrow_k(openness,vklist,do_kind state k)
					      end)

	and do_con (state : state) (con : con) : con =
	    let val _ = if (!debug) then push_con con else ()
		val result = do_con' state con
		val _ = if (!debug) then pop_con con else ()
	    in  result
	    end 

	and do_con' (state : state) (con : con) : con =
	   (case con of
		Prim_c(Record_c(labs,SOME vlist),clist) => 
		    let val (vclist,state) = do_vclist state (Listops.zip vlist clist)
			val (vlist,clist) = Listops.unzip vclist
		    in  Prim_c(Record_c(labs,SOME vlist),clist)
		    end
	      | Prim_c(pc,clist) => Prim_c(pc, map (do_con state) clist)
	      | Mu_c(recur,vc_seq) => Mu_c(recur,Sequence.map
					   (fn (v,c) => (v,do_con state c)) vc_seq)
	      | ExternArrow_c(clist,c) =>
		    ExternArrow_c(map (do_con state) clist, do_con state c)
	      | AllArrow_c{openness,effect,isDependent,tFormals,eFormals,fFormals,body_type} =>
			let val (tFormals,state) = do_vklist state tFormals
			    val (eFormals,state) = do_voptclist state eFormals
			in  AllArrow_c{openness=openness, effect=effect, isDependent=isDependent,
				       tFormals=tFormals, eFormals=eFormals, 
				       fFormals = fFormals, body_type = do_con state body_type}
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
		Con_cb(v,Let_c(_,[Open_cb(v',vklist,c)],Var_c v'')) => 
		    if (Name.eq_var(v',v''))
			then do_cbnd(Open_cb(v,vklist,c), state)
		    else do_cbnd(Con_cb(v,Var_c v''),state)
	      |	Con_cb(v,c) => 
		    let val state = add_var(state,v)
			val state' = enter_var(state,v)
			val c = do_con state' c
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
			val state = add_kind(state,v,Single_k c)
			val state = add_alias(state,v,alias)
		    in  (Con_cb(v,c), state)
		    end
	      | Open_cb(v,vklist,c) => let val state = add_var(state,v)
					   val state' = enter_var(state,v)
					   val state = add_kind(state,v,
							  Single_k (#2(extractCbnd cbnd)))
					   val (vklist,state') = do_vklist state' vklist
					in  (Open_cb(v,vklist, do_con state' c), state)
					end	
	      | Code_cb(v,vklist,c) => let val state = add_var(state,v)
					   val state' = enter_var(state,v)
					   val state = add_kind(state,v,
								  Single_k (#2(extractCbnd cbnd)))
					   val (vklist,state') = do_vklist state' vklist
				        in  (Code_cb(v,vklist, do_con state' c), state)
					end)

		 
	and rewrite_uncurry ((name,function),orig_state) : (var * function) list * state =
	    let val uncurry_name = Name.fresh_named_var ((Name.var2name name) ^ "_uncurry")
		val state = add_curry_pair(orig_state,name,uncurry_name)
		val state = add_curry_processed(state,name)
		val state = add_curry_processed(state,uncurry_name)
		val state = add_var(state,name)
		val state = add_var(state,uncurry_name)
		val (wraps, body) = extract_lambdas (name,function)
	    in  if ((not (!do_uncurry)) 
		    orelse is_curry_processed(orig_state,name)
		    orelse (length wraps <= 1))
		  then ([(name,function)],state)
		else 
		 let val ((vklist,vclist,vflist),bnds) = wraps2args_bnds wraps
		     val body = Let_e(Sequential,bnds,body)
		     val vkarg = map (Var_c o #1) vklist
		     val vcarg = map (Var_e o #1) vclist
		     val vfarg = map Var_e vflist
		     val v = Name.fresh_named_var "call_result"
		     val call = App_e(Open, Var_e uncurry_name, vkarg, vcarg, vfarg)

		     val (_,temp_state) = do_vklist state vklist
		     val return_con = #8(List.last wraps)
		     val (bnds,tinfo) = 
			 (case get_trace (temp_state, return_con) of
			      SOME tinfo => ([],TraceKnown tinfo)
			    | NONE =>
				  let val v' = Name.fresh_named_var "reify"
				  in  ([Con_b(Runtime,Con_cb (v', return_con))], TraceCompute v')
				  end)
		     val bnd = Exp_b(v,tinfo,call)
		     val callbody = Let_e(Sequential,bnds @ [bnd],Var_e v)
		     val curry = create_lambdas(wraps,callbody)
		     val uncurry = create_flatlambda(uncurry_name,wraps,body)
		     val state = add_var(state,name)
		     val state = foldl (fn (v,s) => add_curry_processed(s,v)) state (map #1 wraps)
		     val state = add_alias(state,name,ETAe (length wraps,uncurry_name,[]))
		 in  ([curry,uncurry], state)
		 end
	    end


(*
	and do_exp (state : state) (exp : exp) : exp = 
	    let val _ = push_exp exp
		val result = do_exp' state exp
		val _ = pop_exp exp
	    in  result
	    end 
*)

	and do_aggregate (state : state) funnames_opt (constr,t,clist,elist)  = 
	    let open Prim
		fun helper is_array c = 
		(case (is_array,Normalize_reduce_hnf(get_env state, c)) of
		     (true,(_, Prim_c(Int_c is, _))) => (IntArray is,[]) 
		   | (false,(_, Prim_c(Int_c is, _))) => (IntVector is,[]) 
		   | (true,(_, Prim_c(Float_c is, _))) => (FloatArray is,[]) 
		   | (false,(_, Prim_c(Float_c is, _))) => (FloatVector is,[]) 
		   | (true, (hnf, _)) => (OtherArray hnf,[c]) 
		   | (false, (hnf, _)) => (OtherVector hnf,[c]))
		val (t,clist) = 
		    (case t of
			 OtherArray _ => helper true (hd clist)
		       | OtherVector _ => helper false (hd clist)
		       | _ => (t,clist))
		val array_flavor = (case t of
					OtherArray false => SOME true
				      | OtherVector false => SOME false
				      | _ => NONE)
		val clist = map (do_con state) clist
		val elist = map (do_exp state) elist
	    in  (case (funnames_opt,array_flavor, !do_lift_array) of
		     (SOME (aname,vname), SOME is_array, true) => 
			  App_e(Open, 
				do_exp state 
				(Var_e (if is_array then aname else vname)), 
				clist, elist, [])
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
	       | [Var_e rec_var] => 
		     (case (Normalize_reduce_hnf(get_env state, find_con(state,rec_var))) of
			  (_, Prim_c(Record_c (labs,vlist),cons)) => 
			      let val vars = map (fn l => Name.fresh_named_var (Name.label2name l)) labs
				  fun mapper (v,l,c) =
					let val e = Prim_e(NilPrimOp(select l),[], [Var_e rec_var])
					in  case get_trace(state,c) of
						SOME tinfo => [Exp_b(v,TraceKnown tinfo,e)]
					      | NONE => let val rv = Name.fresh_named_var "opt_reify"
							in  [Con_b(Runtime,Con_cb(rv,c)),
							     Exp_b(v,TraceKnown (TraceInfo.Compute(rv,[])),e)]
							end
					end
				  val bnds = Listops.flatten(map3 mapper (vars,labs,cons))
				  val e = do_prim state (NilPrimOp(inject_record k),clist,map Var_e vars)
			      in  do_exp state (Let_e (Sequential,bnds,e))
			      end
			| (true, _) => do_prim state (NilPrimOp (inject_nonrecord k),clist,elist)
			| _ => default())
	       | _ => default())
	    end


	and do_prim (state : state) (prim, clist, elist) = 
	 let open Prim

	     val elist = map (do_exp state) elist
             fun default() = Prim_e(prim,
				    map (do_con state) clist, 
				    elist)
	     fun getVals [] = SOME []
	       | getVals (e::rest) = 
			if (NilUtil.is_closed_value e) then
			  (case (getVals rest) of
			       NONE => NONE
			     | SOME es => SOME (e :: es))
		        else
			  NONE

	     fun help (e as Var_e v) = 
		 (case lookup_alias(state,v) of
		      MUSTe e => e
		    | OPTIONALe e => e
		    | _ => e)
	       | help e = e

	 in  case (prim,map help elist) of
		 (NilPrimOp(select l),[e as Var_e v]) => 
		     (case (lookup_proj(state,v,[l])) of
			  NONE => Prim_e(prim,[], [do_exp state e])
			| SOME e => do_exp state e)
	       | (NilPrimOp (inject k),_) => do_inject state (k, clist, elist)
	       | (PrimOp(create_table t), _) => do_aggregate state (SOME (local_array,local_vector)) 
							(create_table,t,clist,elist)
	       | (PrimOp(create_empty_table t), _) => 
		   do_aggregate state NONE (create_empty_table,t,clist,elist)
	       | (PrimOp(sub t), _) => 
		   do_aggregate state 
		      (SOME (local_sub,local_vsub)) (sub,t,clist,elist)
	       | (PrimOp(update t), _) => 
		   do_aggregate state (SOME (local_update,local_update)) 
		      (update,t,clist,elist)
	       | (PrimOp(length_table t), _) => 
		   do_aggregate state (SOME (local_len,local_vlen)) 
		      (length_table,t,clist,elist)
	       | (NilPrimOp unroll, [Prim_e(NilPrimOp roll,_,[e])]) => e
	       | (NilPrimOp (unbox_float _), [Prim_e(NilPrimOp (box_float _),_,[e])]) => e
               | (PrimOp p, _) => 
		      (case (getVals elist) of
			   NONE => default ()
			 | SOME elist => 
			       (NilPrimUtil.apply p clist elist) 
			       handle _ => default())
               | _ => default()
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
			    | Prim.tag (t,c) => Const_e(Prim.tag(t,do_con state c)))
		| Prim_e(p,clist,elist) => do_prim state (p,clist,elist)
		| Switch_e sw => do_switch state sw
		| Let_e (letsort,bnds,e) => 
			let (* we must put a wrapper in order to perform the filter *)
			    val state = retain_state state
			    val (bnds,state) = do_bnds(bnds,state)
			    val e = do_exp state e
			    val bnds = List.mapPartial (bnd_used state) bnds
		        in  
			    NilUtil.makeLetE letsort bnds e
			end
		| ExternApp_e(f,elist) =>
			ExternApp_e(do_exp state f, map (do_exp state) elist)
		| App_e(openness,f,clist,elist,eflist) => 
		     let fun default() = App_e(openness, do_exp state f, 
					       map (do_con state) clist,
					       map (do_exp state) elist, 
					       map (do_exp state) eflist)
			 (* assumes anormalization by not performing purity check *)
			 fun do_eta (uncurry,args) = 
			     let val (cargs,eargs,fargs) = Listops.unzip3 args
				 val cargs = Listops.flatten cargs
				 val eargs = Listops.flatten eargs
				 val fargs = Listops.flatten fargs
				 val new_exp = App_e(Open, Var_e uncurry, cargs @ clist, 
						     eargs @ elist, eflist @ eflist)
			     in  do_exp state new_exp
			     end
		     in (case f of 
			     Var_e v =>
			         (case (lookup_alias(state,v)) of
				      ETAe (1,uncurry,args)=> do_eta (uncurry,args)
				    | _ => default())
			      | _ => default())
		     end
		| Raise_e(e,c) => Raise_e(do_exp state e, do_con state c)
		| Handle_e(e,v,handler) => 
			let val ([(v,_)],state) = do_vclist state [(v,Prim_c(Exn_c,[]))]
			in  Handle_e(do_exp state e, v, do_exp state handler)
			end)



	and do_switch (state : state) (switch : switch) : exp = 
	    let fun sum_switch {sumtype,arg,bound,arms,default,result_type} =
		let val arg = do_exp state arg
		    val (tagcount,_,carrier) = Normalize_reduceToSumtype(get_env state,sumtype) 
		    val totalcount = TilWord32.uplus(tagcount,TilWord32.fromInt(length carrier))	
		    fun make_ssum i = Prim_c(Sum_c{tagcount=tagcount,
						   totalcount=totalcount,
						   known=SOME i},
					     case carrier of
						 [_] => carrier
					       | _ => [con_tuple_inject carrier])
		    fun do_arm(n,tr,body) = 
			let val ssumtype = make_ssum n
			    val tr = do_niltrace state tr
			    val (_,state) = do_vclist state [(bound,ssumtype)]
			in  (n,tr,do_exp state body)
			end
		    
		    val known_tag =
			(case arg of 
			     Prim_e(NilPrimOp (inject w), _, _) => SOME w
			   | Var_e v => 
				 (case (lookup_alias(state,v)) of
				      OPTIONALe(Prim_e(NilPrimOp (inject w), _, _)) =>
					  SOME w
				    | _ => NONE)
			   | _ => NONE)
		in
		    case known_tag of
			SOME w => 
			    (* Reduce known switch *)
			    (case List.find (fn (w',_,_) => w = w') arms of
				 SOME (_,_,arm_body) =>
				     do_exp state
				     (Let_e(Sequential,
					    [Exp_b(bound,TraceUnknown,
						   arg)],
					    arm_body))
			       | NONE =>
				     do_exp state
				     (Let_e(Sequential,
					    [Exp_b(bound,TraceUnknown,arg)],
					    Option.valOf default)))
		      | _ => let
				 val sumtype = do_con state sumtype
				 val result_type = do_con state result_type
				 val arms = map do_arm arms
				 val default = Util.mapopt (do_exp state) default
			     in  
				 Switch_e(Sumsw_e {sumtype=sumtype,bound=bound,arg=arg,
						   arms=arms,default=default,
						   result_type = result_type})
			     end
		end
		fun int_switch {size,arg,arms,default,result_type} =
		     (case (do_exp state arg) of
			  Const_e (Prim.int(_,n)) => 
			      let val n32 = TilWord64.toSignedHalf n
				  val arm = case Listops.assoc (n32, arms) of
				      SOME arm => arm 
				    | NONE => Option.valOf default
			      in
				  do_exp state arm
			      end
			| arg => 
			      let 
				  val result_type = do_con state result_type
				  val arms = map_second (do_exp state) arms
				  val default = Util.mapopt (do_exp state) default
			      in  Switch_e(Intsw_e {size=size,arg=arg, arms=arms,default=default,
						    result_type=result_type})
			      end)
			  
	    in
	    (case switch of
		 Intsw_e int_sw => int_switch int_sw
	       | Sumsw_e sum_sw =>
			  (case convert_sumsw state sum_sw of
			       NONE => sum_switch sum_sw
			     | SOME int_sw => int_switch int_sw)
	       | Exncase_e {arg,bound,arms,default,result_type} =>
		     let val arg = do_exp state arg
			 val result_type = do_con state result_type
			 fun do_arm(tag,tr,body) = 
			     let val tag = do_exp state tag
				 val tr = do_niltrace state tr
				 val tagcon = type_of(state,tag)
				 val Prim_c(Exntag_c, [con]) = tagcon
				 val (_,state) = do_vclist state[(bound,con)]
			         val body = do_exp state body
			     in  (tag,tr,body)
			     end
			 val arms = map do_arm arms
			 val default = Util.mapopt (do_exp state) default
		     in  Switch_e(Exncase_e {bound=bound,arg=arg,
					     arms=arms,default=default,
					     result_type = result_type})
		     end
	       | Typecase_e _ => error "typecase not done")
	    end

	and do_function (state : state) (v,Function{effect,recursive,isDependent,
						    tFormals, eFormals, fFormals,
						    body, body_type = c}) =
		let val state = enter_var(state,v)
		    val (tFormals,state) = do_vklist state tFormals
		    val (eFormals,state) = do_eFormals state eFormals
		    val body = do_exp state body
		    val c = do_con state c
		in  (v,Function{effect=effect,recursive=recursive,isDependent=isDependent,
				tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
				body=body, body_type = c})
		end

	and do_niltrace state niltrace = 
	    (case niltrace of
		 TraceCompute v => (use_var(state,v); niltrace)
	       | TraceKnown (TraceInfo.Compute(path as (v,_))) => 
			(case find_availC(state,path2con path) of
				NONE =>	(use_var(state,v); niltrace)
			      | SOME v => (use_var(state,v); TraceKnown(TraceInfo.Compute(v,[]))))
	       | _ => niltrace)

	and do_bnds(bnds : bnd list, state : state) : bnd list * state = 
	    let val (bnds_list,state) = foldl_acc do_bnd state bnds
	    in  (List.concat bnds_list,state)
	    end

	and do_bnd (bnd : bnd, state : state) : bnd list * state = 
	  let 
	      (* Rewrite a binding to do the following:
	         (a) check record arguments are variables
		 (b) check inject argument are variables 
		 (c) rewrite project_sum to project_sum_record or project_sum_nonrecord
		 If NONE is returned, the binding does not need to be rewrritten.
		 Otherwise, SOME bindings are returned.  The original bindings
		   is guaranteed not to occur in this list.  This prevents looping. *)
	      fun rewrite_bnd(v,niltrace,e) =
		  (case e of
		       Prim_e(NilPrimOp(record labs),_,elist) => 
	                 let fun check(Var_e v) = ()
			       | check (Const_e v) = ()
			       | check _ = error "record argument is not a variable"
			 in  NONE
			 end

		     | Prim_e(NilPrimOp roll, _, [Var_e _]) => NONE
		     | Prim_e(NilPrimOp roll, _, [Const_e _]) => NONE
		     | Prim_e(NilPrimOp roll, clist,[injectee]) =>
			 error "roll argument is not a value"

		     | Prim_e(NilPrimOp (box_float _), _, [Var_e _]) => NONE
		     | Prim_e(NilPrimOp (box_float _), _, [Const_e _]) => NONE
		     | Prim_e(NilPrimOp (box_float _), _, _ ) => 
			 error "box_float argument is not a value"

		     | Prim_e(NilPrimOp (inject _), _, [Var_e _]) => NONE
		     | Prim_e(NilPrimOp (inject _), _, [Const_e _]) => NONE
		     | Prim_e(NilPrimOp (inject k), clist,[injectee]) =>
			 error "inject argument is not a value"
		     | Prim_e(NilPrimOp (project_sum k),[sumcon],[Var_e sv]) =>
			 let val sv_con = find_con(state,sv)
			     val (tagcount,k,clist) = Normalize_reduceToSumtype(get_env state,sv_con)
			     val known = (case k of
					  SOME k => k
					| NONE => (print "Expression: "; Ppnil.pp_exp e;
						   print "\nVariable "; Ppnil.pp_var sv;
						   print " has non-special-sum type:\n";
						   Ppnil.pp_con sv_con; print "\n\n";
						   error "Type is not a special sum\n"))
			     val fieldcon = List.nth(clist, TilWord32.toInt
						            (TilWord32.uminus(known,tagcount)))
			 in  case Normalize_reduce_hnf(get_env state, fieldcon) of
			       (_,Prim_c (Record_c (labs,_), rcons)) => 
				 let val vars = map (fn l => Name.fresh_named_var
						     (Name.label2string l)) labs
				     fun mapper(v,l,t) =
					 let val (bnds,tinfo) = 
						 (case get_trace (state, t) of
						      SOME tinfo => ([],TraceKnown tinfo)
						    | NONE =>
							  let val v' = Name.fresh_named_var "reify"
							  in  ([Con_b(Runtime,Con_cb (v', t))], TraceCompute v')
							  end)
					     val np = project_sum_record(known, l)
					 in  bnds @ [Exp_b(v,tinfo,Prim_e(NilPrimOp np,[sumcon],[Var_e sv]))]
					 end
				     val bnds = Listops.map3 mapper (vars, labs, rcons)
				     val bnd = Exp_b(v,TraceKnown TraceInfo.Trace,
						     Prim_e(NilPrimOp(record labs), [], map Var_e vars))
				 in  SOME((Listops.flatten bnds) @ [bnd])
				 end
			     (* not a record for sure *)
			     | (true, _) => SOME[Exp_b(v,niltrace,
						       Prim_e(NilPrimOp (project_sum_nonrecord known),
							      [sumcon],[Var_e sv]))]
			     | _ => NONE
			 end
		     | _ => NONE)
	  in	(case bnd of
		     Exp_b(v,niltrace,e) =>
			 ((* print "working on bnd with v = "; Ppnil.pp_var v; print "\n";  *)
			  case rewrite_bnd(v,niltrace,e) of
			      NONE => 
				  let
				      val state = add_var(state,v)
				      val state' = enter_var(state,v)
				      val niltrace = do_niltrace state' niltrace
				      val e = do_exp state' e
				      val e =
					  (case find_availE(state,e) of
					       NONE => e
					     | SOME v' => let val _ = use_var(state',v')
							  in  Var_e v'
							  end)
				      val eff = not (anormal_valuable(get_env(state),e))
				      val (effect,alias) = 
					  (case e of
					       
					       Var_e v' => 
						   let
						       val n = Name.var2name v
						       val n' = Name.var2name v'
						       val _ = if (String.size(n') = 0) then
							          Name.rename_var (v', n)
							       else ()
						   in
						       (false,MUSTe e)
						   end

					     (* Constant Propagation *)
					     | Const_e(Prim.int _) => (false, MUSTe e)
					     | Prim_e(NilPrimOp (box_float _), _, _) =>
						   (false, OPTIONALe e)
					     | Prim_e(NilPrimOp roll, _, _) =>
						   (false, OPTIONALe e)

					     | App_e(openness,Var_e v,clist,elist,eflist) => 
						   (case (lookup_alias(state,v)) of
							ETAe (depth,uncurry,args) => 
							    (print "found ETAe in etabnd\n";
							     if (depth > 1) 
								then
								    let val new_info = (depth-1,uncurry,
											args @ [(clist,elist,eflist)])
								    in  (false,ETAe new_info)
								    end
							    else (eff,OPTIONALe e))
					              | _ => (eff,OPTIONALe e))
					     | _ => (eff,OPTIONALe e))
				      val _ = if effect then use_var(state,v) else ()
				      val state = (case e of
						       Var_e _ => state
						     | _ => add_availE(state, e, v))
				      val state = add_alias(state,v,alias)
				      val state = add_exp(state,v,e)
				  in  ([Exp_b(v, niltrace, e)], state)
				  end
			    | SOME bnds => do_bnds(bnds,state))
		   | Con_b(p,cbnd) => let val (cbnd,state) = do_cbnd(cbnd,state)
				      in  ([Con_b(p,cbnd)], state)
				      end
		   | Fixopen_b vfset =>
		     let val vflist = Sequence.toList vfset
			 fun folder ((v,f),state) = add_con(state,v,NilUtil.function_type Open f)
			 val (vflistlist,state) = foldl_acc rewrite_uncurry state vflist
			 val vflist = Listops.flatten vflistlist
			 val state = foldl folder state vflist
			 val newbnd = Fixopen_b(Sequence.fromList vflist)
			 val vflist = map (do_function state) vflist
		     in  ([Fixopen_b(Sequence.fromList vflist)], state)
		     end
		   | Fixcode_b vfset =>
				let val vflist = Sequence.toList vfset
				    val state = add_vars(state,map #1 vflist)
				    val vflist = map (do_function state) vflist
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
	fun do_import(ImportValue(l,v,tr,c),state) = (ImportValue(l,v,tr,do_con state c), 
						      add_con(state,v,c))
	  | do_import(ImportType(l,v,k),state)  = (ImportType(l,v,do_kind state k), 
						   add_kind(state,v,k))

	fun do_export state (ExportValue(l,v)) = 
	    let val e = do_exp state (Var_e v)
		val _ = use_var(state,v)
	    in
		(NONE, ExportValue(l,v))
	    end
	  | do_export state (ExportType(l,v)) =
	    let val c = do_con state (Var_c v)
		val _ = use_var(state,v)
	    in  
		(NONE, ExportType(l,v))
	    end

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

	      (* we "retain" the state so that no exports are optimized away *)
	      val state = retain_state state
	      val temp = map (do_export state) exports
	      val export_bnds = List.mapPartial #1 temp
              val _ = Ppnil.pp_bnds export_bnds
	      val exports = map #2 temp

	      val bnds = local_bnds @ bnds @ export_bnds
              val bnds = List.mapPartial (bnd_used state) bnds
	  in  MODULE{imports=imports,exports=exports,bnds=bnds}
	  end

end
