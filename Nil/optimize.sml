(* A one-pass optimizer with the following goals.
   Those marked - are controlled by the input parameters.
   Those marked * require a typing context.
*-	Perform CSE (context needed to maintain to determine valuability of applications)
-	Eliminate dead code
-       Replace projections of known records with the known value if value is small
-       Uncurry functions
        Propagate constants
*	Convert project_sum to project_known
	Cancel make_vararg and make_onearg
*       Reduce vararg
*	Cancel fold and unfold
*       Cancel coercions
        Fold constant expressions
	Convert Sumsw to Intsw
	Flatten int switches.
	Not anormalize (old fear of classifier sizes)
	Reduce known switch
*)

structure Optimize
    :> OPTIMIZE =
struct

	open Util Nil NilUtil Listops
 	val error = fn s => Util.error "optimize.sml" s

	val warn = fn s => print ("WARNING: "^s^"\n")

	val debug = Stats.ff("OptimizeDebug")
	val doTimer = Stats.ff("DoOptimizeTimer")
	fun subtimer(str,f) = if (!doTimer) then Stats.subtimer(str,f) else f

	fun inc r = r := !r + 1
	fun dec r = r := !r - 1

	val uncurry_pushes_cbnds = CompilerControl.UncurryPushCbnds
	val SpecializeArrayofChar = CompilerControl.SpecializeArrayofChar

	val coercion_cancel = Stats.tt "CancelCoercions"
	val reduce_varargs = Stats.tt "ReduceVarargs"
	val reduce_oneargs = Stats.tt "ReduceOneargs"
	val reduce_onearg_apps = Stats.tt "ReduceOneargApps"
	val kill_imports  = CompilerControl.KillDeadImport

	val chatlev = ref 0
	val folds_reduced = ref 0
	val coercions_cancelled = ref 0
	val switches_flattened  = ref 0
	val switches_reduced    = ref 0
	val varargs_reduced = ref 0
	val oneargs_reduced = ref 0
	val onearg_apps_reduced = ref 0
	val imports_killed = ref 0
	val uncurried = ref 0
	val record_eta = ref 0
	val fun_eta = ref 0
	val proj_inj = ref 0
	val inj_proj = ref 0
	fun reset_stats() =
	  let in
	    folds_reduced :=  0;
	    coercions_cancelled := 0;
	    switches_flattened := 0;
	    switches_reduced := 0;
	    varargs_reduced := 0;
	    oneargs_reduced := 0;
	    onearg_apps_reduced := 0;
	    imports_killed := 0;
	    uncurried := 0;
	    record_eta := 0;
	    fun_eta := 0;
	    proj_inj := 0;
	    inj_proj := 0
	  end

	fun chat lev str = if (!chatlev) >= lev then print str else ()
	val chat0 = chat 0
	val chat1 = chat 1
	val chat2 = chat 2

	fun chat_stats () =
	  if !chatlev > 0 then
	    (print "\t";
	     print (Int.toString (!folds_reduced));
	     print " fold/unfold pairs reduced\n";
	     print "\t";
	     print (Int.toString (!coercions_cancelled));
	     print " coercion pairs reduced\n";
	     print "\t";
	     print (Int.toString (!switches_flattened));
	     print " int switches flattened\n" ;
	     print "\t";
	     print (Int.toString (!switches_reduced));
	     print " known switches reduced\n" ;
	     print "\t";
	     print (Int.toString (!varargs_reduced));
	     print " varargs reduced\n";
	     print "\t";
	     print (Int.toString (!oneargs_reduced));
	     print " oneargs reduced\n" ;
	     print "\t";
	     print (Int.toString (!onearg_apps_reduced));
	     print " onearg apps reduced\n";
	     print "\t";
	     print (Int.toString (!imports_killed));
	     print " imports killed\n";
	     print "\t";
	     print (Int.toString (!uncurried));
	     print " functions uncurried\n";
	     print "\t";
	     print (Int.toString (!record_eta));
	     print " records eta reduced\n";
	     print "\t";
	     print (Int.toString (!fun_eta));
	     print " functions eta reduced\n";
	     print "\t";
	     print (Int.toString (!proj_inj));
	     print " sum projections beta reduced\n";
	     print "\t";
	     print (Int.toString (!inj_proj));
	     print " sum injections eta reduced \n"
	     ) else ()


        (* Generate polymorphic aggregrate handling functions that use PrimOp's *)
	local
	    fun make_length (aggregate,constr) =
		let val c = Name.fresh_named_var "len_type"
		    val agg = Name.fresh_named_var "len_agg"
		    val body = Prim_e(PrimOp(Prim.length_table
					     (aggregate false)), [],[Var_c c],
				      [Var_e agg])
		    val res_var = Name.fresh_named_var "len_result"
		    val body = Let_e(Sequential, [Exp_b(res_var, TraceKnown TraceInfo.Notrace_Int, body)],
						Var_e res_var)
		in  (AllArrow_c {openness = Code, effect = Partial, tFormals = [(c, Type_k)], eFormals = [Prim_c(constr,[Var_c c])], fFormals = 0w0, body_type = Prim_c(Int_c Prim.W32, [])},
		     Function{effect = Partial, recursive = Leaf,
			      tFormals = [c],
			      eFormals = [(agg,TraceKnown TraceInfo.Trace)],
			      fFormals = [],
			      body = body})
		end

		fun make_sub (aggregate,constr) =
		    let val c = Name.fresh_named_var "subscript_type"
			val agg = Name.fresh_named_var "subscript_agg"
			val index = Name.fresh_named_var "subscript_index"
			val res_var = Name.fresh_named_var "subscript_result"
			val body = Prim_e(PrimOp(Prim.sub (aggregate false)), [],
					  [Var_c c],
					  [Var_e agg, Var_e index])
			val body = Let_e(Sequential, [Exp_b(res_var, TraceKnown (TraceInfo.Compute(c,[])), body)],
					 Var_e res_var)
		    in  (AllArrow_c {openness = Code, effect = Partial, tFormals = [(c, Type_k)], eFormals = [Prim_c(constr,[Var_c c]), Prim_c(Int_c Prim.W32 ,[])], fFormals = 0w0, body_type = Var_c c},
			 Function{effect = Partial, recursive = Leaf,
				  tFormals = [c],
				  eFormals = [(agg, TraceKnown TraceInfo.Trace),
					     (index, TraceKnown TraceInfo.Notrace_Int)],
				  fFormals = [],
				  body = body})
		    end


		fun make_update (aggregate,constr) =
		    let val c = Name.fresh_named_var "update_type"
			val array = Name.fresh_named_var "update_array"
			val index = Name.fresh_named_var "update_index"
			val item = Name.fresh_named_var "update_item"
			val body = Prim_e(PrimOp(Prim.update (aggregate false)), [],
					  [Var_c c],
					  [Var_e array, Var_e index, Var_e item])
			val res_var = Name.fresh_named_var "update_result"
			val body = Let_e(Sequential, [Exp_b(res_var, TraceKnown TraceInfo.Trace, body)],
					 Var_e res_var)
		    in (AllArrow_c {openness = Code, effect = Partial, tFormals = [(c, Type_k)], eFormals = [Prim_c(constr,[Var_c c]), Prim_c(Int_c Prim.W32 ,[]), Var_c c], fFormals = 0w0, body_type = NilDefs.unit_con},
			Function{effect = Partial, recursive = Leaf,
				 tFormals = [c],
				 eFormals = [(array, TraceKnown TraceInfo.Trace),
					     (index, TraceKnown TraceInfo.Notrace_Int),
					     (item,  TraceCompute c)],
				 fFormals = [],
				 body = body})
		    end

		fun make_aggregate (aggregate,constr) =
		    let val c = Name.fresh_named_var "agg_type"
			val size = Name.fresh_named_var "agg_size"
			val item = Name.fresh_named_var "agg_item"
			val body = Prim_e(PrimOp(Prim.create_table
					 (aggregate false)), [],[Var_c c],
					  [Var_e size, Var_e item])
		    in (AllArrow_c {openness = Code, effect = Partial, tFormals = [(c, Type_k)], eFormals = [Prim_c(constr,[Var_c c]), Var_c c], fFormals = 0w0, body_type = Prim_c(constr,[Var_c c])},
			Function{effect = Partial, recursive = Leaf,
				 tFormals = [c],
				 eFormals = [(size, TraceKnown TraceInfo.Notrace_Int),
					     (item, TraceCompute c)],
				 fFormals = [],
				 body = body})
		    end

		val subVar = Name.fresh_named_var "local_subscript"
		val updateVar = Name.fresh_named_var "local_update"
		val local_array = Name.fresh_named_var "local_array"
		val local_len = Name.fresh_named_var "local_len"
		val local_vsub = Name.fresh_named_var "local_vsubscript"
		val local_vector = Name.fresh_named_var "local_vector"
		val local_vlen = Name.fresh_named_var "local_vlen"

		val lenFun = make_length(Prim.OtherArray, Array_c)
		val vlenFun = make_length(Prim.OtherVector, Vector_c)
		val subFun = make_sub(Prim.OtherArray, Array_c)
		val vsubFun = make_sub(Prim.OtherVector, Vector_c)
		val updateFun = make_update(Prim.OtherArray, Array_c)
		val arrayFun = make_aggregate(Prim.OtherArray, Array_c)
		val vectorFun = make_aggregate(Prim.OtherVector, Vector_c)

		fun mkEntry(name, (fun_con, function)) =
		    let val v = Name.fresh_named_var name
		    in  (((v,fun_con), function),
			 (Name.internal_label name, v,
			  TraceKnown TraceInfo.Notrace_Code,
			  fun_con))
		    end
		val lenEntry = mkEntry ("polyLen",lenFun)
		val vlenEntry = mkEntry("polyVlen",vlenFun)
		val subEntry = mkEntry("polySub",subFun)
		val vsubEntry = mkEntry("polyVsub",vsubFun)
		val updateEntry = mkEntry("polyUpdate",updateFun)
		val arrayEntry = mkEntry("polyArray",arrayFun)
		val vectorEntry = mkEntry("polyVector",vectorFun)

		val bnds = map (fn (vf, _) => Fixcode_b (Sequence.fromList [vf]))
				 [lenEntry, vlenEntry, subEntry, vsubEntry,
				  updateEntry, arrayEntry, vectorEntry]

	    in  fun generate() = {sub = #2 subEntry,
				  vsub = #2 vsubEntry,
				  len = #2 lenEntry,
				  vlen = #2 vlenEntry,
				  update = #2 updateEntry,
				  array = #2 arrayEntry,
				  vector = #2 vectorEntry,
				  bnds = bnds}
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
			(v) optionalC - an equialent type expression which may be used
			(vi) mustC - an equialent type expression which must be used

		(1) allows some reification to occur
		(2) allows type reduction and some code transformation to occur
	        (3) and (4.a) together allow cascading dead code to be eliminated
		(4.b) allows sum and record projections to be optimized
		(4.b.iv) allows functions to be uncurried
        *)

	datatype used_state = UNUSED
	                    | USED of int
	                    | DEFER of used_state ref list
	datatype equivalent = UNKNOWN
	                    | OPTIONALe of exp
                            | MUSTe of exp
	                              (*f -> ETAe (i,f_uncurry,args) means
				       *  f(a_1)(a_2)...(a_i) === f_uncurry(args@a_1@a_2@...@a_i)
				       *)
	                    | ETAe of int * var * (con list * exp list * exp list) list
	                    | OPTIONALc of con
                            | MUSTc of con

	fun pp_alias UNKNOWN = print "unknown"
	  | pp_alias (OPTIONALe e) = (print "OPTIONALe "; Ppnil.pp_exp e)
	  | pp_alias (MUSTe e) = (print "MUSTe "; Ppnil.pp_exp e)
	  | pp_alias (OPTIONALc c) = (print "OPTIONALc "; Ppnil.pp_con c)
	  | pp_alias (MUSTc c) = (print "MUSTc "; Ppnil.pp_con c)
	  | pp_alias (ETAe (i,v,args)) = print ("ETAe (arity="^(Int.toString i)^",fun="^(Name.var2string v)^")")

        type params = {doDead : bool,
		       doProjection : int option,
		       doUncurry : bool,
		       doPolyUncurry : bool,
		       doCse : bool}
	local
	  type entry = used_state ref * equivalent
	  datatype state = STATE of {curry_processed : (Name.VarSet.set * var Name.VarMap.map),
				     equation : NilContext.context,
				     current : used_state ref,
				     mapping : entry Name.VarMap.map,
				     avail : var ExpTable.Expmap.map *
				             var ExpTable.Conmap.map,
				     params : params}

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

	  fun update_mapping(STATE{params,equation, current, avail, curry_processed, ...}, mapping) =
			STATE{params=params,
			      curry_processed=curry_processed,
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
	  fun newState params = STATE {curry_processed = (Name.VarSet.empty, Name.VarMap.empty),
				       equation = NilContext.empty(),
				       current = ref (USED 1),
				       avail = (ExpTable.Expmap.empty,
						ExpTable.Conmap.empty),
				       mapping = Name.VarMap.empty,
				       params = params}
	  fun getParams (STATE{params,...}) = params

	  (* Mark the current binding as used,
	   * so that all variables encountered will be
	   * kept.
	   *)
	  fun retain_state(STATE{equation, current, mapping, avail, curry_processed, params}) =
			STATE{equation=equation, avail=avail,
			      curry_processed = curry_processed,
			      current=ref (USED 1),mapping=mapping,
			      params=params}


	  fun enter_var(STATE{equation, curry_processed, current, mapping, avail, params}, v) =
		STATE{equation=equation,
		      mapping = mapping,
		      avail = avail,
		      curry_processed = curry_processed,
		      current=case (Name.VarMap.find(mapping,v)) of
				NONE => error "enter_var given var not in used map"
			      | SOME (us,_) => us,
		      params = params}

	  fun add_vars(state as STATE{mapping,...},vars) =
		let val r = ref UNUSED
		    (* This shared ref isn't important because add_vars will only be used with variables that can't be eliminated.
		     * (i.e., variables bound in coercions)
		     *)
		    val mapping = foldl (fn (v,m) => Name.VarMap.insert(m,v,(r,UNKNOWN))) mapping vars
		in  update_mapping(state,mapping)
		end
	  fun add_var(state,v) = add_vars(state,[v])

	  fun use_var(STATE{mapping,current,...},v) =
	  	((* print "XXX use_var on "; Ppnil.pp_var v; print "\n"; *)
		 case Name.VarMap.find(mapping,v) of
		  NONE => ()
		| SOME (r,_) =>
		   (case !r of
			USED n => r := (USED(n+1))
		      | UNUSED => r := DEFER[current]
		      | DEFER ls => r := DEFER (current::ls)))

          fun get_varuse(STATE{mapping,params,...},v) =
		case Name.VarMap.find(mapping,v) of
		  NONE => (print "is_used_var given var not in state: ";
		  	   Ppnil.pp_var v; print "\n";
			   error "is_used_var given var not in state")
		| SOME (r,_) => if (#doDead params)
				    then isused r
				else USED 999

          fun is_used_var(state,v) = (case get_varuse(state,v) of
					  USED _ => true
					| UNUSED => false)

	  fun add_alias(state as STATE{mapping,...},v,alias) =
	      let val SOME(use,_) = Name.VarMap.find(mapping,v)
		  val mapping = Name.VarMap.insert(mapping,v,(use,alias))

(*		  val _ = (print "add_alias for "; Ppnil.pp_var v;
				print " --> "; pp_alias alias; print "\n") *)

	      in  update_mapping(state,mapping)
	      end

	  fun lookup_alias(STATE{mapping,...},v) =
	      (case (Name.VarMap.find(mapping,v)) of
		   NONE => UNKNOWN
		 | SOME (_,alias) => alias)

	  (*Get an alias if possible, otherwise
	   * return the original expression
	   *)
	  fun unalias (state,e) =
	    let
	      fun loop (Var_e v) =
		(case lookup_alias(state,v) of
		   MUSTe e => loop e
		 | OPTIONALe e => e
		 | _ => e)
		| loop e = e
	    in loop e
	    end

	  (*Same for cons
	   *)
	  fun unalias_c (state,c) =
	    let
	      fun loop (Var_c a) =
		(case lookup_alias(state,a) of
		   MUSTc c => loop c
		 | OPTIONALc c => c
		 | _ => c)
		| loop c = c
	    in loop c
	    end

	  fun getVals state [] = SOME []
	    | getVals state (e::rest) =
	    let
	      val e = unalias (state,e)
	    in
	      if (NilDefs.is_closed_value e) then
		(case (getVals state rest) of
		   NONE => NONE
		 | SOME es => SOME (e :: es))
	      else
		NONE
	    end

(*	  val lookup_alias = fn (s,v) =>
	    let val alias = lookup_alias (s,v)
	    in
	      print "Alias for ";Ppnil.pp_var v;print " found to be\n";
	      pp_alias alias; print "\n";
	      alias
	    end
*)

	  fun get_env(STATE{equation,...}) = equation
	  fun find_con(STATE{equation,...},v) = NilContext.find_con(equation,v)

	  fun updateContext(STATE{avail,curry_processed,equation=_,current,mapping,params},equation) =
	      STATE{equation=equation,
		    avail=avail,
		    curry_processed = curry_processed,
		    mapping=mapping,
		    current=current,
		    params=params}

	  fun add_kind(state as STATE{equation,...},v,k) =
	      updateContext(state,NilContext.insert_kind(equation,v,k))

	  fun add_con(state as STATE{equation,...},v,c) =
	      updateContext(state,NilContext.insert_con(equation,v,c))

	  fun add_exp(state as STATE{equation,...},v,e) =
	      updateContext(state,NilContext.insert_exp(equation,v,e))


	  fun add_curry_processed(STATE{avail,equation,mapping,curry_processed,current,params},v) =
	       STATE{equation=equation,
		     avail=avail,
		     curry_processed = (Name.VarSet.add(#1 curry_processed,v), #2 curry_processed),
		     mapping=mapping,
		     current=current,
		     params=params}

	  fun add_curry_pair(STATE{avail,equation,mapping,curry_processed,current,params},
			     curry_name,uncurry_name) =
	       STATE{equation=equation,
		     avail=avail,
		     curry_processed = (#1 curry_processed, Name.VarMap.insert(#2 curry_processed,
									       curry_name,uncurry_name)),
		     mapping=mapping,
		     current=current,
		     params=params}

	  fun find_curry_pair(STATE{curry_processed,...},v) = Name.VarMap.find(#2 curry_processed,v)
	  fun is_curry_processed(STATE{curry_processed,...},v) = Name.VarSet.member(#1 curry_processed,v)

	  fun reduce_hnf(STATE{equation,...},c) =
	      subtimer("optimizeReduceHnf", Normalize.reduce_hnf)(equation,c)

	  fun reduceToSumtype(STATE{equation,...},c) =
	      subtimer("optimizeReduceToSumtype", Normalize.reduceToSumtype)(equation,c)

	  fun type_of(STATE{equation,...},e) =
	      subtimer("optimizeTypeof", Normalize.type_of)(equation,e)

	  fun kind_of(STATE{equation,...},c) =
	      subtimer("optimizeKindof", NilContext.kind_of)(equation,c)

	  fun type_equiv(STATE{equation,...},c1,c2) =
	      subtimer("optimizeTypeEquiv", NilStatic.type_equiv)(equation,c1,c2)

	  fun sub_type(STATE{equation,...},c1,c2) =
	      subtimer("optimizeSubType", NilStatic.sub_type)(equation,c1,c2)

	  fun reduce_vararg(STATE{equation,...},openness,effect,argc,resc,arg) =
	      subtimer("optimizeReduceVararg", Vararg.reduce_vararg)(equation,openness,effect,argc,resc,arg)

	  fun reduce_onearg(STATE{equation,...},openness,effect,argc,resc,arg) =
	      subtimer("optimizeReduceOnearg", Vararg.reduce_onearg)(equation,openness,effect,argc,resc,arg)

	  fun find_availC(STATE{avail,params,...},c) =
	      if (#doCse params) then ExpTable.Conmap.find(#2 avail,c) else NONE
	  fun add_availC(state as STATE{curry_processed,mapping,current,equation,avail,params},c,v) =
	      if (#doCse params)
		  then STATE{mapping=mapping,current=current,
			     equation=equation,
			     curry_processed = curry_processed,
			     avail=(#1 avail,
				    ExpTable.Conmap.insert(#2 avail,c,v)),
			     params=params}
	      else state

	  fun find_availE(STATE{avail,params,...},e) =
	      if (#doCse params) then ExpTable.Expmap.find(#1 avail,e) else NONE

(*	  val find_availC = fn (s,c) =>
	    let val res = find_availC (s,c)
	    in (case res
		  of SOME c' => (print "XXX!!!\n";
				 Ppnil.pp_con c;
				 print "\n Rewritten with \n";
				 Ppnil.pp_var c';
				 print "\n")
		   | _ => ())
	      ;res
	    end

	  val find_availE = fn (s,c) =>
	    let val res = find_availE (s,c)
	    in (case res
		  of SOME c' => (print "XXX!!!\n";
				 Ppnil.pp_exp c;
				 print "\n ExpRewritten with \n";
				 Ppnil.pp_var c';
				 print "\n")
		   | _ => ())
	      ;res
	    end
*)

	  (* Valuable expressions are expressions which do not have effects of
	   * any kind.  These may be eliminated as dead code, or replicated, or
	   * coalesced at will
	   *)
          fun valuable (state, e) =
	      (case e of
		 App_e(_,Var_e v,_,elist,eflist) =>
		   (case reduce_hnf (state,find_con (state,v))
		      of (true,AllArrow_c{effect,...}) => (effect=Total) andalso (Listops.andfold (fn e => valuable (state, e)) (elist @ eflist))
		       | (false,_) => (warn "Optimize unable to get head normal form for function type";false))
		 | _ => not (NilDefs.anyEffect e))

	  (* Pure expressions are expressions which do not have
	   * any store effects (that is, they neither depend on nor modify
	   * the store.  This means that either
	   *    1. They always compute the same value
	   * or 2. They always diverge or throw an exception
	   * Pure expressions cannot be eliminated without changing the
	   * behaviour of the program, but they can be coalesced with
	   * a previous binding of the same expression. c.g. Tarditi 6.1
	   *)
          fun pure (state, e) =
	    (case e of
	       App_e(_,Var_e v,_,elist,eflist) =>
		 (case reduce_hnf (state,find_con (state,v))
		    of (true,AllArrow_c{effect,...}) => (effect=Total) andalso (Listops.andfold (fn e => valuable (state, e)) (elist @ eflist))
		     | (false,_) => (warn "Optimize unable to get head normal form for function type";false))
	     | _ => not (NilDefs.storeEffect e))

	  fun add_availE(state as STATE{mapping,current,curry_processed,equation,avail,params},e,v) =
	      if (#doCse params andalso pure(state,e))
		  then STATE{params=params,
			     mapping=mapping,current=current,
			     curry_processed = curry_processed,
			     equation=equation,
			     avail=(ExpTable.Expmap.insert(#1 avail,e,v),
				    #2 avail)}
	      else state

	  (* Normalize a type to an arrow *)
	  fun strip_arrow (STATE{equation,...},c) =
	      Normalize.strip_arrow_norm equation c

	  fun var_is_unit (state,x) =
	    (case NilUtil.strip_record(#2(reduce_hnf(state,(type_of(state,Var_e x)))))
	       of SOME ([],[]) => true
		| _ => false)

	  (* Look up constructor level record projection *)

	  (* XXX I'm not sure what this was supposed to do, but I'm pretty sure
	   * it's not doing it.  Note that length(labs) will never exceed 1. -leaf
	   *)
	  fun lookup_cproj(state,v,labs) =
	      let fun loop c [] = SOME c
		    | loop (Crecord_c lclist) (l::rest) =
		         let val c = (case assoc_eq(Name.eq_label,l,lclist) of
					  NONE => (print "lookup_cproj could not find ";
						   Ppnil.pp_label l; print "  among ";
						   app (fn (l,_) => (Ppnil.pp_label l; print "  ")) lclist;
						   print "\n";
						   error "lookup_cproj: could not find label")
					| SOME c => c)
			 in  loop c rest
			 end
		    | loop (Var_c v) labs = lookup_cproj(state,v,labs)
		    | loop _ _ = NONE
		  val res1 = 
		    (case lookup_alias(state,v) of
		       OPTIONALc c => loop c labs
		     | MUSTc c => loop c labs
		     | _ => NONE)
	      in   
		case res1
		  of NONE => (case find_availC(state,NilDefs.path2con(v,labs)) of
				NONE =>	NONE
			      | SOME v => SOME(Var_c v))
		   | _ => res1
	      end


	  (* Full get_trace may rewrite the con in a way that the optimizer
	   * won't like.  We have to use get_trace' and trust that the optimizer
	   * has Done the Right Thing. -Leaf
	   *)
	  fun get_trace(STATE_,t) = TraceOps.get_trace' t
	  val con2trace = TraceOps.con2trace
	  fun con2trace' t =
	    (case TraceOps.get_trace' t
	       of SOME ti => ([],TraceKnown ti)
		| NONE =>
		 let
		   val v' = Name.fresh_named_var "tracevar"
		   val t = NilRename.renameCon t
		 in
		   ([Con_b(Runtime,Con_cb (v', t))],
		    TraceCompute v')
		 end)

	end


        (* Helper functions for uncurry optimizations *)
	fun get_lambda e = 
	  let
	    fun lambda (subst,cbs,((v,c),
				   Function{effect,recursive,
					    tFormals,eFormals,fFormals,
					    body=body})) = 
	      let
		val c = NilSubst.substConInCon subst c
		val eFormals = Listops.map_second (NilSubst.substConInTrace subst) eFormals
		val body = NilUtil.makeLetE Sequential cbs body
		val f = 
		  Function{effect=effect,recursive=recursive,
			   tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
			   body=body}
	      in SOME ((v,c),f)
	      end
	    
	    fun loop (e,subst,cbs) = 
	      (case e
		 of Let_e(t,((cb as Con_b(_,Con_cb(a,c)))::bnds),e) => 
		   if !uncurry_pushes_cbnds then
		     loop (Let_e(t,bnds,e),NilSubst.C.addr(subst,a,c),cb::cbs)
		   else NONE
		  | Let_e(_,[Fixopen_b vfset],Var_e v) =>
		   (case vfset 
		      of [arg as ((v',c),f)] => 
			if (Name.eq_var(v,v'))
			  then lambda (subst,rev cbs,arg)
			else NONE
		       | _ => NONE)
		  | _ => NONE)
	  in loop (e,NilSubst.C.empty(),[])
	  end

	fun make_lambda ((v,c),f) = Let_e(Sequential,[Fixopen_b([((v,c),f)])],Var_e v)

	datatype wrap = WRAP of {v: var, c : con,
				 openness: openness, eff: effect,
				 r: recursive,
				 vklist: (var * kind) list,
				 clist: con list,
				 vtlist: (var * niltrace) list,
				 vflist: var list,
				 body_type: con}
        (* 1: Function name
	 * 2: Function type
	 * 3: Function effect
	 * 4: Function recursion status
	 * 5: Formal constructor parameters
	 * 6, 7: Formal term parameters
	 * 8: Formal float parameters
	 * 9: Result type
	 *)

	fun wrap_get_v (WRAP {v,...}) = v
	fun wrap_get_c (WRAP {c,...}) = c
	fun wrap_get_openness (WRAP {openness,...}) = openness
	fun wrap_get_eff (WRAP {eff,...}) = eff
	fun wrap_get_r (WRAP {r,...}) = r
	fun wrap_get_vklist (WRAP {vklist,...}) = vklist
	fun wrap_get_clist (WRAP {clist,...}) = clist
	fun wrap_get_vtlist (WRAP {vtlist,...}) = vtlist
	fun wrap_get_vflist (WRAP {vflist,...}) = vflist
	fun wrap_get_body_type (WRAP {body_type,...}) = body_type

	fun extract_lambdas state vf =
	    let
	      val e = make_lambda vf
	      fun loop acc e =
		(case get_lambda e
		   of NONE => (rev acc, e)
		   | SOME ((v, c),Function{effect=eff,recursive=r,tFormals,
					   eFormals=vtlist,fFormals=vflist,body=body,...}) =>
		     let
		       val {openness, tFormals = vklist, eFormals = clist,body_type,...} = rename_arrow(strip_arrow (state, c),tFormals)
		     in
		       if (#doPolyUncurry (getParams state)) orelse (null vklist) then
			 loop (WRAP {v = v, c = c, openness = openness, eff = eff, r = r, vklist = vklist, vtlist = vtlist,
				     clist = clist, vflist = vflist, body_type = body_type}::acc) body
		       else (rev acc, e)
		     end)
	    in  loop [] e
	    end

	fun create_lambdas ([],_) = error "no wraps to create_lambdas"
	  | create_lambdas (wraps,body) =
	  let
	    fun loop [] body = body
	      | loop (WRAP {v,c,eff,r,vklist,clist,vtlist,vflist,body_type,openness,...}::rest) body =
	      let
		val body = loop rest body
		val total = not (null rest)
		val f = Function{effect=if total then Total else eff,recursive=r,
				 tFormals=map #1 vklist,eFormals=vtlist,fFormals=vflist,
				 body=body}
	      in
		make_lambda((v,c),f)
	      end
	    val lambda = loop wraps body
	    val SOME vf = get_lambda lambda
	  in  vf
	  end


	fun wraps2args_bnds (wraps : wrap list) =
	    let
	      fun loop acc ([] : wrap list) = error "no wraps to wraps2args_bnds"
		| loop acc [_] = rev acc
		| loop acc (WRAP {v,vklist,vtlist,vflist,...} ::(rest as (next::_))) =
		let 
		  val vk = map #1 vklist
		  val vc = map #1 vtlist
		  val info = Exp_b(wrap_get_v next,TraceKnown TraceInfo.Trace,
				   App_e(Open, Var_e v, map Var_c vk, map Var_e vc, map Var_e vflist))
		in  loop (info::acc) rest
		end
	      val vklist = Listops.flatten(map wrap_get_vklist wraps)
	      val clist = Listops.flatten(map wrap_get_clist wraps)
	      val vtlist = Listops.flatten(map wrap_get_vtlist wraps)
	      val vflist = Listops.flatten(map wrap_get_vflist wraps)
	    in  ((vklist,clist,vtlist,vflist), loop [] wraps)
	    end

	fun wraps2properties wraps = 
	  let
	    fun promote_effect(cureff,neweff) = 
	      (case cureff
		 of Total => neweff
		  | _ => cureff)
	    fun promote_recursive(currec,newrec) =
	      (case (currec,newrec)
		 of (Leaf,_) => newrec
		  | (_,Leaf) => currec
		  | (Arbitrary,_) => currec
		  | _ => newrec)
	    fun loop (wraps,cureff,currec) = 
	      (case wraps
		 of [] => (cureff,currec)
		  | (WRAP{eff,r,...}::rest) => loop (rest,promote_effect(cureff,eff),promote_recursive(currec,r)))
	  in loop (wraps,Total,Leaf)
	  end
	fun create_flatlambda(_,[],_) = error "no wraps to create_flatlamba"
	  | create_flatlambda(name,wraps,body) =
	    let 
	      val WRAP {body_type,openness,...} = List.last wraps
	      val ((vklist,clist,vtlist,vflist),_) = wraps2args_bnds wraps
	      val (eff,r) = wraps2properties wraps
	      val c = AllArrow_c {openness = openness, 
				  effect = eff, 
				  tFormals = vklist, 
				  eFormals = clist, 
				  fFormals =  TilWord32.fromInt(length vflist), 
				  body_type = body_type}
	      val (bnds,c) = NilUtil.nameType ((Name.var2name name )^"_type") (NilRename.renameCon c)

	    in  (bnds,
		 ((name, c),
		  Function{effect=eff,recursive=r,
			   tFormals=map #1 vklist,eFormals=vtlist,fFormals=vflist,
			   body=body}))
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


	(*** Code for inteq to intneq conversion  ***)
	(* Find an internal boolean constant *)
	fun get_bool_const state e = 
	  (case unalias (state,e)
	     of Coerce_e (q,[],v) => 
	       (case (unalias (state,q),unalias (state,v))
		  of (ForgetKnown_e (sumcon,w),Prim_e(NilPrimOp(inject_known _),[],clist,[])) =>
		    (case NilUtil.strip_sum (#2(reduce_hnf(state,sumcon)))
		       of SOME (0w2,0w2,_,_) => 
			 (case w 
			    of 0w0 => SOME false
			     | 0w1 => SOME true
			     | _ => NONE)
			| _ => NONE)
		   | _ => NONE)
	      | _ => NONE)



	fun intsw_negate state {size,arg,arms,default,result_type} = 
	  (case (arms,default)
	     of ([(w,thenArm)],SOME elseArm) => 
	       (case (get_bool_const state thenArm, get_bool_const state elseArm)
		  of (SOME false,SOME true) => 
		    let
		      val c = Const_e(Prim.int(size,TilWord64.fromUnsignedHalf w))
		    in 
		      SOME (Prim_e(PrimOp(Prim.neq_int size),[],[],[arg,c]))
		    end
		   | (SOME true,SOME false) => 
		    let
		      val c = Const_e(Prim.int(size,TilWord64.fromUnsignedHalf w))
		    in 
		      SOME (Prim_e(PrimOp(Prim.eq_int size),[],[],[arg,c]))
		    end
		   | _ => NONE)
	      | _ => NONE)


	(*** Code for Sumsw to Intsw conversion ***)

	fun get_int_eq state e = 
	  let
	    val res = case unalias (state,e)
			of Prim_e(PrimOp(Prim.eq_int is),[],[],[v1,v2]) => SOME(is,v1,v2)
			 | _ => NONE
	  in res
	  end

	fun get_eq_args state e = 
	  (case get_int_eq state e
	     of SOME (is,Var_e v,Const_e (Prim.int(_,w))) => SOME(is,v,w)
	      | SOME (is,Const_e (Prim.int(_,w)),Var_e v) => SOME(is,v,w)
	      | _ => NONE)  (* Not eq, or not compared to a constant *)

	fun is_sumsw_int state (Switch_e(Sumsw_e{sumtype,bound,arg,arms,default,...})) =
	  (case (get_eq_args state arg,arms,default) of
	     (SOME(is,v,w), [(0w0,_,zeroexp), (0w1,_,oneexp)], NONE) =>
	       SOME (is,v,TilWord64.toUnsignedHalf w,zeroexp,oneexp)
	   | _ => NONE)
	  | is_sumsw_int state (Let_e (_, [Exp_b(v, _, e)], Var_e v')) =
	       if (Name.eq_var(v,v')) then is_sumsw_int state e else NONE
	  | is_sumsw_int state _ = NONE

	(* This makes some effort to try and flatten the generated
	 * int switches.  It will almost always fail,
	 * since there are always intervening bindings
	 * in a-normal form.  Might be worth getting rid of this.
	 * -leaf
	 *)
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
				    arms=rev clauses,default=SOME base,result_type=result_type})

		     end
	       | _ => NONE)
	    end


	(* This code tries to catch code of the form
 	 * Intswitch x of i1 => e1
	 *             |  _  => Intswitch x of i2 => e2
	 *                                  | _   => .....
	 * and turn it into code of the form
 	 * Intswitch x of i1 => e1
	 *             |  i2 => e2
	 *             |  ....
	 * This code arises commonly from the result of optimizing
	 * the code that the elaborator generates for all sorts of
	 * switches.
	 *
	 * This is a fairly brain dead version - one could do better.
	 *
	 * The important thing we must ensure is that that we do not accidentally
	 * move switches with the same pattern into the same switch, since we
	 * do not have a well-defined semantics for this.
	 * We accomplish this by doing a very simple form of redundant comparison
	 * elimination.  Any comparison of x to i1 in the default switch continuation
	 * is dead.  Therefore, we simply always drop arms which have the same
	 * tag as an arm we've already found.
	 *
	 * A perhaps better approach would be to do full redundant comparison elimination.
	 *
	 * Another extension to this would be to catch code where  e1 (above) is also
	 * a switch on x.  This complicates the optimization however,
	 * since we cannot hoist these comparisons up without ensuring that we are not
	 * killing off a comparison to the same value in the default continuation (or in
         * the general case, in subsequent arms).
	 * Given our current translation of switches, this "optimization" would probably
         * produce worse code in many cases, since we would be taking a tree-structured
	 * decision tree and turning it into a linear one.  If we ever start generating
         * good code for switches, we could reconsider this.
	 *  -leaf
	 *)
	local
	  (* We traverse the switch from top to bottom, maintaining the arms
	   * that we have seen so far in a sorted list.  At each switch in a
	   * default continuation, we add in the arms (deleting any new duplicates)
	   * and recur on the new default continuation
	   *
	   * The number of arms we will collect up is small, so insertion sort
	   * is probably a good choice.
	   *)

	  fun arm_cmp (arm1 : w32 * exp,arm2 : w32 * exp) =
	    let
	      val i1 = #1 arm1
	      val i2 = #1 arm2
	    in
	      if TilWord32.slt (i1,i2) then LESS
	      else if TilWord32.sgt (i1,i2) then GREATER
	      else EQUAL
	    end

	  (* This implements the redudant switch elimination:
	   * if the arm tags are equal, then we throw away the
	   * arm that we are adding, since it follows (in the control flow)
	   * the previous comparison
	   * We keep the arms in descending order, since ML code tends to
	   * generate them in ascending order and we are traversing top down
	   *)
	  fun add_arm(arm,arms) =
	    let
	      fun loop [] = [arm]
		| loop (a as (arm'::arms)) =
		(case arm_cmp(arm,arm')
		   of LESS => arm'::loop arms
		    | GREATER => arm::a
		    | EQUAL => a)
	    in loop arms
	    end

	  fun sort_arms arms =
	    let fun loop (acc,[]) = acc
		  | loop (acc,arm::arms) = loop(add_arm(arm,acc),arms)
	    in loop ([],arms)
	    end

	  fun add_arms (new,sorted) = List.foldl add_arm sorted new

	  (* Given the variable that we are collecting switches on,
	   * the current list of arms, and the current default,
	   * try to find more arms in the default to hoist if possible
	   *)
	  fun get_arms v (cur as (arms,default)) =
	    (case default
	       of Switch_e(Intsw_e{size,arg=Var_e v',arms=newarms,default=SOME next,result_type}) =>
		 if Name.eq_var (v,v') then
		   (inc switches_flattened;
		    get_arms v (add_arms(newarms,arms),next))
		 else cur
		| Let_e (_, [Exp_b(v', _, e)], Var_e v'') =>
		   if (Name.eq_var(v',v'')) then get_arms v (arms,e) else cur
		| _ => cur)

	in
	  (*This is the function that actually goes through
	   * and tries to collect up the arms
	   *)
	  fun flatten_int_sw int_sw=
	    (case int_sw
	       of {size,arms,default=SOME next,result_type,arg=Var_e v} =>
		 let
		   val (arms,default) = get_arms v (arms,next)
		   (* Arms will be sorted in decreasing order.  We put
		    * them in increasing order, since this is more
		    * standard, and is what the above code is optimized for
		    *)
		   val arms = rev arms
		 in
		   {size=size,arg=Var_e v,
		    arms=arms,default=SOME default,
		    result_type = result_type}
		 end
		| _ => int_sw)
	end

	fun flattenBnds [] = []
	  | flattenBnds (Exp_b(v,tr,Let_e(Sequential,innerBnds,body))::rest) =
	    let val innerBnds = flattenBnds innerBnds
		val bnd = Exp_b(v,tr,body)
	    in  innerBnds @ (bnd :: (flattenBnds rest))
	    end
	  | flattenBnds (bnd::rest) = bnd :: (flattenBnds rest)

	fun cbnd_var cb =
	  (case cb
	     of  Con_cb(v,c)   => v
	      | Open_cb(v,_,_) => v
	      | Code_cb(v,_,_) => v)

	fun cbnd_used' state cbnd = get_varuse(state, cbnd_var cbnd)
	fun cbnd_used state cbnd = is_used_var(state, cbnd_var cbnd)

	fun bnd_used state bnd =
	    (case bnd of
		 (Con_b(p,cb)) => (case (cbnd_used' state cb) of
				       UNUSED => []
				     | USED _ => [Con_b(p,cb)])
	       | (Exp_b(v,_,e)) => if is_used_var(state,v) then [bnd] else []
	       | (Fixopen_b vfset) =>
		     let val vflist = Sequence.toList vfset
			 val vflist = List.filter (fn ((v,_),_) => is_used_var(state,v)) vflist
			 fun eliminate_uncurry(curry,uncurry,curryf,uncurryf as Function {body,...}) =
			     let 
			       val _ = chat2 ("Undoing uncurry of "^(Name.var2name (#1 curry))^"\n")
			       val _ = dec uncurried
			       val (wraps,_) = extract_lambdas state (curry,curryf)
			       val inner_wrap_names = map wrap_get_v (tl wraps)
			       fun non_partial bnd = 
				 (case bnd
				    of Exp_b (v,_,_) => not (Listops.member_eq (Name.eq_var,v,inner_wrap_names))
				     | _ => true)

			       (*Erase the partial applications *)
			       val body = 
				 (case body 
				    of Let_e(s,bnds,b) => Let_e(s,List.filter non_partial bnds,b)
				     | _ => body)
			       val curry = create_lambdas(wraps,body)
			     in  curry
			     end
			 fun loop [] = []
			   | loop (ls as [_]) = ls
			   | loop ((A as (ap as (a,ac),af))::(B as (bp as (b,bc),bf))::rest) =
			     (case find_curry_pair(state,a) of
				  NONE => A::loop(B::rest)
				| SOME v =>
				    (if Name.eq_var(v,b) then  (* A curry pair *)
				       let
					 (* Drop if only use of uncurry_f is in curry_f. *)
					 val drop = case (get_varuse(state,b)) 
						      of USED 1 => true
						       | USED n => false
						       | UNUSED => false
						       | _ => false
				       in
					 if drop then 
					   (eliminate_uncurry(ap,bp,af,bf))::(loop rest)
					 else 
					   let
					     (* B duplicates names from A *)
					     val B = (bp,NilRename.renameFunction bf)
					   in  (* Keep the pair *)
					     A::B::(loop rest)
					   end
				       end
				     else  (* Not a pair, keep going with rest *) 
				       (A::loop(B::rest))))

			 val vflists = 
			   if #doUncurry (getParams state) then
			     let
			       val vflist = loop vflist
			       val vflists = NilUtil.break_fix vflist
			     in vflists
			     end
			   else [vflist]
		     in  map Fixopen_b vflists
		     end
	       | (Fixcode_b vfset) =>
		     let val vflist = Sequence.toList vfset
			 val vflist = List.filter (fn ((v,_),_) => is_used_var(state,v)) vflist
		     in  (case vflist of
			      [] => []
			    | _ => [Fixcode_b (Sequence.fromList vflist)])
		     end
	       | (Fixclosure_b(recur,vclset)) =>
		     let val vcllist = Sequence.toList vclset
			 val vcllist = List.filter (fn ((v,_),_) => is_used_var(state,v)) vcllist
		     in  (case vcllist of
			      [] => []
			    | _ => [Fixclosure_b (recur,Sequence.fromList vcllist)])
		     end)

	fun do_vklist state vklist =
	    let 
	      fun dosingle(v,mker,c,state) = 
		let 
		  val state = add_var(state,v)
		  val state' = enter_var(state,v)
		  val c = do_con state' c
		  val c =
		    (case find_availC(state,c) of
		       NONE => c
		     | SOME v' => let val _ = use_var(state,v')
				  in  Var_c v'
				  end)
		  val alias = (case c of
				 Var_c _ => MUSTc c
			       (* Must use the old variable instead of the new one bound here later *)
			       | _ => OPTIONALc c)
		  val state = (case c of
				 Var_c _ => state
			       | _ => add_availC(state, c, v))
		  val k = mker c
		  val state = add_kind(state,v,k)
		  val state = add_alias(state,v,alias)
		  val _ = use_var (state,v)
		in ((v,k),state)
		end
	      fun folder((v,k),state) = 
		(case k
		   of Single_k c     => dosingle(v, Single_k, c, state)
		    | SingleType_k c => dosingle(v, SingleType_k, c, state)
		    | _ => let val k = do_kind state k
			   in  ((v,k),add_kind(state,v,k))
			   end)
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
	and do_c (c, state) = let val c = do_con state c
			      in  (c,state)
			      end
	and do_vtrc ((v,tr,c),state) =  let val tr = do_niltrace state tr
					    val c = do_con state c
					    val state = add_con(state,v,c)
					in  ((v,tr,c),state)
					end
	and do_vtr ((v,tr),state) =  let val tr = do_niltrace state tr
					in  ((v,tr),state)
					end
	and do_vclist state vclist = foldl_acc do_vc state vclist
	and do_voptclist state vclist = foldl_acc do_voptc state vclist
	and do_clist state clist = foldl_acc do_c state clist
	and do_eFormals state eFormals = foldl_acc do_vtrc state eFormals
	and do_vtrlist state vtrlist = foldl_acc do_vtr state vtrlist


	and do_kind (state : state) (kind : kind) : kind =
	  let
	  in 
	    case kind of
	      Type_k => kind
	    | SingleType_k c => SingleType_k(do_con state c)
	    | Single_k c => Single_k(do_con state c)
	    | Record_k(lvk_seq) => 
		let 
		  val (ls,vks) = Listops.map_unzip (fn ((l,v),k) => (l,(v,k))) lvk_seq
		  val (vks,state) = do_vklist state vks
		  val lvks = Listops.map2 (fn (l,(v,k)) => ((l,v),k)) (ls,vks)
		in  Record_k lvks
		end
	    | Arrow_k(openness,vklist,k) => 
		let val (vklist,state) = do_vklist state vklist
		in  Arrow_k(openness,vklist,do_kind state k)
		end
	  end

	and do_con (state : state) (con : con) : con =
	    let val _ = if (!debug) then push_con con else ()
		val result = do_con' state con
		val _ = if (!debug) then pop_con con else ()
	    in  result
	    end

	and do_con' (state : state) (con : con) : con =
	  let
	  in
	    case con of
	      Prim_c(Vararg_c _,clist) =>
		(case reduce_hnf(state,con)  (* Try to reduce Varargs if possible *)
		   of (_,Prim_c (pc,_)) => Prim_c(pc, map (do_con state) clist)
		    | (_,con) => do_con state con)
	    | Prim_c(Array_c,clist)  => 
		(case reduce_hnf(state,con)  (* Try to reduce Array if possible *)
		   of (_,Prim_c (Array_c,_)) => Prim_c(Array_c, map (do_con state) clist)
		    | (_,con) => do_con state con)
	    | Prim_c(Vector_c,clist)  => 
		(case reduce_hnf(state,con)  (* Try to reduce Vector if possible *)
		   of (_,Prim_c (Vector_c,_)) => Prim_c(Vector_c, map (do_con state) clist)
		    | (_,con) => do_con state con)
	    | Prim_c(pc,clist) => Prim_c(pc, map (do_con state) clist)
(*
	    | Mu_c(recur,vc_seq) => Mu_c(recur,Sequence.map
					 (fn (v,c) => (v,do_con state c)) vc_seq)
*)
	    | Mu_c(recur,vc_seq) => 
		   let fun folder ((v,c),s) = add_kind(s,v,Type_k)
		       val state = Sequence.foldl folder state vc_seq
		       fun mapper (v,c) = (v, do_con state c)
		       val vc_seq' = Sequence.map mapper vc_seq
		   in
		       Mu_c(recur,vc_seq')
		   end
	    | Nurec_c (v,k,c) => 
		   let val ([(v,k)],state) = do_vklist state [(v,k)]
		       val c = do_con state c
		   in  Nurec_c(v,k,c)
		   end
	    | ExternArrow_c(clist,c) =>
		   ExternArrow_c(map (do_con state) clist, do_con state c)
	    | AllArrow_c{openness,effect,tFormals,eFormals,fFormals,body_type} =>
		   let val (tFormals,state) = do_vklist state tFormals
		     val (eFormals,state) = do_clist state eFormals
		   in  AllArrow_c{openness=openness, effect=effect,
				  tFormals=tFormals, eFormals=eFormals,
				  fFormals = fFormals, body_type = do_con state body_type}
		   end
	    | Var_c v =>
		   let
		     val res =
		       case lookup_alias(state,v) of
			 MUSTc c => do_con state c
		       | OPTIONALc c => if NilDefs.small_con c then do_con state c
					else (use_var(state,v); con)
		       | _ => (use_var(state,v); con)
		   in res
		   end
	    | Crecord_c lclist => 
	       let 
		 fun eta_crecord(state,lclist as ((_,Proj_c(Var_c a,_))::_)) = 
		   let
		     fun check ((l,Proj_c(Var_c a',l')),((l'',_),_)) = 
		       Name.eq_var(a,a') andalso Name.eq_label(l,l') andalso Name.eq_label(l',l'')
		       | check _ = false
		     val is_eta = 
		       (case kind_of(state,(Var_c a))
			  of Record_k (lvks) => Listops.all2 check (lclist,lvks)
			   | _ => false)
		   in if is_eta then SOME (Var_c a)
		      else NONE
		   end
		   | eta_crecord _ = NONE

		 val lclist = map (fn (l,c) => (l, do_con state c)) lclist
		 val con = 
		   (case eta_crecord (state,Listops.map_second (fn c => unalias_c(state,c)) lclist)
		      of SOME c => c
		       | NONE => Crecord_c lclist)
	       in con
	       end
	    | Proj_c(Var_c v, l) =>
		   (case (lookup_cproj(state,v,[l])) of
		      NONE => Proj_c(do_con state (Var_c v), l)
		    | SOME c => do_con state c)
	    | Proj_c(c,l) => Proj_c(do_con state c, l)
	    | Closure_c(c1,c2) => Closure_c(do_con state c1, do_con state c2)
	    | App_c(c,clist) => App_c(do_con state c, map (do_con state) clist)
	    | Coercion_c {vars,from,to} =>
		      let
			fun folder (v,s) = add_kind(s,v,Type_k)
			val state = foldl folder state vars
		      in
			Coercion_c {vars=vars,from=do_con state from,
				    to=do_con state to}
		      end
	    | Let_c(letsort,cbnds,c) =>
		      let val cbnds = flattenCbnds cbnds
			(* we must put a wrapper in order to perform the filter *)
			val state = retain_state state
			val (cbnds,state) = foldl_acc do_cbnd state cbnds
			val c = do_con state c
			val cbnds = List.filter (cbnd_used state) cbnds
		      in  NilUtil.makeLetC cbnds c
		      end
	  end

	and do_cbnd(cbnd : conbnd, state : state) : conbnd * state =
	   (case cbnd of
		Con_cb(v,c) =>
		  (case c of
		       Let_c(_,[Open_cb(v',vklist,c)],Var_c v'') =>
		       if (Name.eq_var(v',v''))
			 then do_cbnd(Open_cb(v,vklist,c), state)
		       else do_cbnd(Con_cb(v,Var_c v''),state)  (* Lambda is dead code *)
		      |	_ =>
			 let 
			   val state = add_var(state,v)
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
					             (* Must use the old variable instead of the new one bound here later *)
					| _ => OPTIONALc c)
			   val state = (case c of
					 Var_c _ => state
				       | _ => add_availC(state, c, v))
			   val state = add_kind(state,v,Single_k c)
			   val state = add_alias(state,v,alias)
			 in  (Con_cb(v,c), state)
			 end)
	      | Open_cb(v,vklist,c) => let val state = add_var(state,v)
					   val state' = enter_var(state,v)
					   val state = add_kind(state,v,
							  Single_k (#2(extractCbnd cbnd)))
					   val (vklist,state') = do_vklist state' vklist
					   val c = do_con state' c
					   fun check_eta_args (vklist,args) =
					     (Listops.all2 (fn ((v,k),(Var_c v')) => Name.eq_var (v,v') | _ => false) (vklist,args))
				       in  
					 case c
					   of Let_c (_,[Con_cb(a,App_c(Var_c v',args))],Var_c a') => 
					     if Name.eq_var (a,a') andalso check_eta_args(vklist,args)
					       then (Con_cb(v,Var_c v'),state)
					     else(Open_cb(v,vklist, c), state)
					    | App_c(Var_c v',args) => 
					       if check_eta_args(vklist,args) 
						 then (Con_cb(v,Var_c v'),state)
					       else(Open_cb(v,vklist, c), state)
					    | _ => (Open_cb(v,vklist, c), state)
				       end
	      | Code_cb(v,vklist,c) => let val state = add_var(state,v)
					   val state' = enter_var(state,v)
					   val state = add_kind(state,v,
								  Single_k (#2(extractCbnd cbnd)))
					   val (vklist,state') = do_vklist state' vklist
				        in  (Code_cb(v,vklist, do_con state' c), state)
					end)


	and rewrite_uncurry (vf as ((name,con),function),orig_state) : (conbnd list * ((var * con) * function) list) * state =
	    let val uncurry_name = Name.fresh_named_var ((Name.var2name name) ^ "_uncurry")
		val state = add_curry_pair(orig_state,name,uncurry_name)
		val state = add_curry_processed(state,name)
		val state = add_curry_processed(state,uncurry_name)
		val state = add_var(state,name)
		val state = add_var(state,uncurry_name)
		val (wraps, body) = extract_lambdas state vf
	    in  if ((not (#doUncurry (getParams orig_state)))
		    orelse is_curry_processed(orig_state,name)
		    orelse (length wraps <= 1))
		  then (([],[vf]),state)
		else
		 let 
		   val _ = chat2 ("Attempting to uncurry "^(Name.var2string name)^"\n")
		   val _ = inc uncurried
		   val ((vklist,_,vtlist,vflist),bnds) = wraps2args_bnds wraps
		   val body = Let_e(Sequential,bnds,body)
		   val vkarg = map (Var_c o #1) vklist
		   val vcarg = map (Var_e o #1) vtlist
		   val vfarg = map Var_e vflist
		   val v = Name.fresh_named_var "call_result"
		   val call = App_e(Open, Var_e uncurry_name, vkarg, vcarg, vfarg)
		     
		   val (_,temp_state) = do_vklist state vklist
		   val return_con = wrap_get_body_type(List.last wraps)
		   val (bnds,tinfo) = con2trace' return_con
		   val bnd = Exp_b(v,tinfo,call)
		   val callbody = Let_e(Sequential,bnds @ [bnd],Var_e v)
		   val curry = create_lambdas(wraps,callbody)
		   val (cbnds,uncurry) = create_flatlambda(uncurry_name,wraps,body)
		   val state = add_var(state,name)
		   val state = foldl (fn (v,s) => add_curry_processed(s,v)) state (map wrap_get_v wraps)
		   val state = add_alias(state,name,ETAe (length wraps,uncurry_name,[]))
		 in  ((cbnds,[curry,uncurry]), state)
		 end
	    end



	and do_exp (state : state) (exp : exp) : exp =
	    let val _ = if !debug then push_exp exp else ()
		val result = do_exp' state exp
		val _ = if !debug then pop_exp exp else ()
	    in  result
	    end


	and do_aggregate (state,constr,t,clist,elist)  =
	  let open Prim

	      fun unbox sv = 
		let
		  val x = Name.fresh_named_var "unboxed_arr_arg"
		in Let_e (Sequential,[Exp_b(x,TraceKnown(TraceInfo.Notrace_Real),Prim_e(NilPrimOp(unbox_float Prim.F64), [],[], [sv]))],Var_e x)
		end
	      fun box oper = 
		let
		  val xf = Name.fresh_named_var "float_arr_res"
		  val x = Name.fresh_named_var "boxed_arr_res"
		in Let_e (Sequential,[Exp_b(xf,TraceKnown(TraceInfo.Notrace_Real),oper),
				      Exp_b(x,TraceKnown(TraceInfo.Trace),Prim_e(NilPrimOp(box_float Prim.F64), [],[], [Var_e xf]))],Var_e x)
		end

	      fun do_boxfloat (p,elist) = 
		(case (p,elist)
		   of (create_table _,[len,init]) => (false,p,[],[len,unbox init])
		    | (create_empty_table _,_)    => (false,p,[],elist)
		    | (sub _,[arr,i])             => (true,p,[],elist)
		    | (update _,[arr,i,v])        => (false,p,[],[arr,i,unbox v])
		    | (length_table _,_)          => (false,p,[],elist)
		    | _ => error "Impossible")
		   

	      fun helper is_array c elist =
		let
		  val intconstr = if is_array then IntArray else IntVector
		  val floatconstr = if is_array then FloatArray else FloatVector
		  val otherconstr = if is_array then OtherArray else OtherVector
		in
		  (case reduce_hnf(state, c)
		   of (_, Prim_c(Int_c is, _)) => 
		     (case (!SpecializeArrayofChar,is)
			of (true,_) => (false,constr(intconstr is),[],elist)
			 | (_,Prim.W32) => (false,constr(intconstr is),[],elist)
			 | _ => (false,constr(otherconstr true),[],elist))
		    | (_, Prim_c(BoxFloat_c Prim.F64, _)) => do_boxfloat(constr(floatconstr Prim.F64),elist)
		    | (_, Prim_c(Float_c is, _)) => error "This shouldn't happen?"
		    | (hnf, _) => (false,constr(otherconstr hnf),[c],elist))
		end
		   
	      val (boxit,p,clist,elist) =
		(case t of
		   OtherArray _ => helper true (hd clist) elist
		 | OtherVector _ => helper false (hd clist) elist
		 | _ => (false,constr t,clist,elist))
	      val clist = map (do_con state) clist
	      val elist = map (do_exp state) elist
	      val oper = Prim_e(PrimOp p, [],clist, elist)
	  in if boxit then box oper else oper
	  end
	
	(* Convert some inject's to inject_known's *)
	and do_inject (state : state) (k, clist, elist) =
	    let fun default() = Prim_e(NilPrimOp (inject k),[],
				       map (do_con state) clist,
				       map (do_exp state) elist)
	    in
	     (case elist of
		 [] => do_prim state (NilPrimOp(inject_known k),[],clist,[])
	       | [injectee] =>
		     let
			 val injectee_type = type_of(state,injectee)
			 val sum_type = hd clist
		     in  (case (reduce_hnf(state, injectee_type)) of
			  (true, injectee_hnf) =>
			    (case (reduce_hnf (state, sum_type)) of
				   (true,sum_hnf) =>
				     let
				       val has_one_carrier =
					 (case sum_hnf
					    of Prim_c (Sum_c {tagcount,totalcount,...},_) =>
					      TilWord32.equal(TilWord32.uminus(totalcount,tagcount),0w1)
					     | _=> error "Inject into non-sum!")

				       val carrier_not_tagged = has_one_carrier andalso not (NilUtil.is_taglike injectee_hnf)

				       val e =
					 if carrier_not_tagged then
					   Prim_e(NilPrimOp(inject_known k), [], clist, elist)
					 else
					   let
					     val typname = Name.fresh_named_var "sumtype"
					     val typbnd = Con_b(Compiletime,Con_cb(typname,NilRename.renameCon
										   (NilUtil.convert_sum_to_special (sum_hnf,k))))
					     val tgname = Name.fresh_named_var "sumgctag"
					     val tgtr = con2trace (Prim_c(GCTag_c,[Var_c typname]))
					     (* This will always be known, since we have a hnf *)
					     val tr = con2trace injectee_hnf
					     val tgbnd = Exp_b(tgname,tgtr ,Prim_e(NilPrimOp mk_sum_known_gctag,[tr],[Var_c typname],[]))
					     val e = Prim_e(NilPrimOp(inject_known k), [],clist, (Var_e tgname)::elist)
					   in Let_e(Sequential,[typbnd,tgbnd],e)
					   end
				     in do_exp state e
				     end

				 | _ => error "Injectee has known type, but sum has no hnf")
			| _ => default())
		     end
	       | _ => error "Inject got more than one argument")
	    end


	and do_prim (state : state) (prim, trlist,clist, elist) =
	 let open Prim
	     
	     fun help e = unalias (state,e)

             fun default() = Prim_e(prim,
				    map (do_niltrace state) trlist,
				    map (do_con state) clist,
				    map (do_exp state) elist)

	 in  (case (prim,map help elist) of
		 (NilPrimOp(select l),[Prim_e(NilPrimOp(record labs),_,_,gctag::elist)]) =>
		     let val SOME new_exp = assoc_eq(Name.eq_label,l,zip labs elist)
                     in  (case (#doProjection (getParams state)) of
			      NONE => default()
			    | SOME maxSize => if (NilUtil.exp_size new_exp < maxSize)
						  then do_exp state new_exp
					      else default())
		     end
	       | (NilPrimOp (record labs),(gctag::(exps as (Prim_e(NilPrimOp(select l),_,_,[Var_e v]))::_))) => 
		     let  (*Record eta reduction *)
		       fun check (e,l,l') =
			 (case e
			    of Prim_e(NilPrimOp(select l''),_,_,[Var_e v']) => 
			      Name.eq_var(v,v') andalso Name.eq_label (l,l') andalso Name.eq_label (l',l'')
			     | _ => false)
		       val is_eta =
			 (case NilUtil.strip_record (#2(reduce_hnf(state,type_of(state,Var_e v))))
			    of SOME(labs',cons) => Listops.all3 check (exps,labs,labs')
			     | _ => false)
		     in if is_eta then (inc record_eta;Var_e v) else default()
		     end
	       | (NilPrimOp (project k),[Prim_e(NilPrimOp (inject k'),_,[sumcon],[arg])]) => (inc proj_inj;arg)
	       | (NilPrimOp (project_known k),[Prim_e(NilPrimOp (inject_known k'),_,[sumcon],[arg])]) => (inc proj_inj;arg)
	       | (NilPrimOp (project_known k),[Prim_e(NilPrimOp (inject_known k'),_,[sumcon],[_,arg])]) => (inc proj_inj;arg)
	       | (NilPrimOp (inject k),[Prim_e(NilPrimOp (project k'),_,[sumcon],[arg])]) => 
		  (case clist
		     of [sumcon'] => if TilWord32.equal (k,k') andalso type_equiv(state,sumcon,sumcon') 
				       then (inc inj_proj;arg)
				     else do_inject state (k,clist,elist))
	       | (NilPrimOp (inject_known k),[Prim_e(NilPrimOp (project_known k'),_,[sumcon],[arg])]) => 
	          (case clist
		     of [sumcon'] => if TilWord32.equal (k,k') andalso type_equiv(state,sumcon,sumcon') 
				       then (inc inj_proj;arg)
				     else default())
	       | (NilPrimOp (inject_known k),[_,Prim_e(NilPrimOp (project_known k'),_,[sumcon],[arg])]) => 
	          (case clist
		     of [sumcon'] => if TilWord32.equal (k,k') andalso type_equiv(state,sumcon,sumcon') 
				       then (inc inj_proj;arg)
				     else default())
	       | (NilPrimOp (inject k),_) => do_inject state (k, clist, elist)
	       | (PrimOp(p as (create_table t)), _) => do_aggregate (state,create_table,t,clist,elist)
	       | (PrimOp(p as (create_empty_table t)), _) =>
							do_aggregate (state,create_empty_table,t,clist,elist)
	       | (PrimOp(p as (sub t)), _)          => do_aggregate (state,sub,t,clist,elist)
	       | (PrimOp(p as (update t)), _)       => do_aggregate (state,update,t,clist,elist)
	       | (PrimOp(p as (length_table t)), _) => do_aggregate (state,length_table,t,clist,elist)
	       | (PrimOp(p as (equal_table t)), _)  => do_aggregate (state,equal_table,t,clist,elist)
	       | (NilPrimOp (unbox_float _), [Prim_e(NilPrimOp (box_float _),_,_,[e])]) => do_exp state e
	       | (NilPrimOp (make_vararg oe), [Prim_e(NilPrimOp (make_onearg _), _, _, [e])]) => do_exp state e
	       | (NilPrimOp (make_onearg oe), [Prim_e(NilPrimOp (make_vararg _), _, _, [e])]) => do_exp state e
	       | (NilPrimOp (make_vararg (openness,effect)), _) =>
		   let
		     val [arg] = elist (* Original unaliased version *)
		     val [argc,resc] = clist
		   in case reduce_vararg (state,openness,effect,argc,resc,arg)
			of SOME e => (inc varargs_reduced;if !reduce_varargs then do_exp state e else default())
			 | NONE => default()
		   end
	       | (NilPrimOp (make_onearg (openness,effect)), _) =>
		   let
		     val [arg] = elist (* Original unaliased version *)
		     val [argc,resc] = clist
		   in case reduce_onearg (state,openness,effect,argc,resc,arg)
			of SOME e => (inc oneargs_reduced;if !reduce_oneargs then do_exp state e else default())
			 | NONE => default()
		   end
               | (PrimOp p, _) =>
		      (case (getVals state elist) of
			   NONE => default ()
			 | SOME elist =>
			       (do_exp state (NilPrimUtil.apply (get_env state) p clist elist))
			       handle _ => default())
               | _ => default())
	 end


	and do_exp' (state : state) (exp : exp) : exp =
	   (  (*print "XXX do_exp doing "; Ppnil.pp_exp exp; print "\n";   *)
	    case exp of
		  Var_e v =>
			 (case lookup_alias(state,v) of
			    MUSTe e => do_exp state e
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
			    | Prim.intarray (sz,a) =>
				  let val _ = Array.modify (do_exp state) a
				  in  Const_e(Prim.intarray(sz, a))
				  end
			    | Prim.intvector (sz,a) =>
				  let val _ = Array.modify (do_exp state) a
				  in  Const_e(Prim.intvector(sz, a))
				  end
			    | Prim.floatarray (sz,a) =>
				  let val _ = Array.modify (do_exp state) a
				  in  Const_e(Prim.floatarray(sz, a))
				  end
			    | Prim.floatvector (sz,a) =>
				  let val _ = Array.modify (do_exp state) a
				  in  Const_e(Prim.floatvector(sz, a))
				  end
			    | Prim.refcell _ => exp
			    | Prim.tag (t,c) => Const_e(Prim.tag(t,do_con state c)))
		| Prim_e(p,trlist,clist,elist) => do_prim state (p,trlist,clist,elist)
		| Switch_e sw => do_switch state sw
		| Let_e (letsort,bnds,e) =>
			let
                         (* we must put a wrapper in order to perform the filter *)
			    val state = retain_state state
			    val (bnds,state) = do_bnds(bnds,state)
			    val e = do_exp state e
			    (* bnd_used will discard any bounds that were definitively unused.
			     * Variables that appear free in any of the bounds will be deferred
			     * to that variable being bound.  Variables that appear free in
			     * in e will be deferred to current.  Therefore,
			     * we retain_state to say that current is definitely used.  Otherwise,
			     * all of the bnds will be marked as unused and discarded, since
			     * we don't yet know the status if the binding that we are in.
			     *)
			    val bnds = Listops.map_concat (bnd_used state) bnds
			    (* We cannot flatten bnds inside do_bnds because bnd_used would access undefined vars *)
			    val bnds = flattenBnds bnds
		        in  NilUtil.makeLetE letsort bnds e
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
			 fun do_onearg(arg_con, onearg_var, orig_var, args as [Var_e arg_var]) =
			   let
			     val (reducible,con) = reduce_hnf(state,arg_con)
			     val _ = if reducible then inc onearg_apps_reduced else ()
			     val res =
			       if reducible then
				 case con
				   of Prim_c(Record_c labels, rcons) =>
				     if (List.length labels <= (!Nil.flattenThreshold)) then
				       let
					 val newvars = map (fn _ => Name.fresh_var()) labels
					 val args' = map Var_e newvars
					 fun mkbnds (l,v,c) =
					   let val (bnds,tr) = con2trace' c
					   in
					     bnds @ [Exp_b(v,tr,
							   Prim_e(NilPrimOp(select l), [],[], args))]
					   end
					 val bndss' = Listops.map3 mkbnds (labels, newvars,rcons)
					 val bnds'  = Listops.flatten bndss'
					 val call = App_e(Open, Var_e orig_var, [], args', [])
					 val new_exp = NilUtil.makeLetE Sequential bnds' call
				       in
					 do_exp state new_exp
				       end
				     else do_exp state (App_e(Open, Var_e orig_var, [], args, []))
				 | _ => do_exp state (App_e(Open, Var_e orig_var, [], args, []))
			       else default()
			   in res
			   end
                           | do_onearg (_,onearg_var, orig_var, args) =
	                                   (print "do_onearg: bad app ";
	                                    Ppnil.pp_var onearg_var;
	                                    print " = onearg of ";
	                                    Ppnil.pp_var orig_var;
	                                    print " applied to ";
	                                    Ppnil.pp_list Ppnil.pp_exp' args
                                                  (" ", " ", " ", false);
	                                    print "\n";
	                                    error "do_onearg: found application of onearg'ed function not given a single variable argument!")

			 fun doapp f =
			   (case f of
			      Var_e v =>
				(case (lookup_alias(state,v))
				   of ETAe (1,uncurry,args)=> do_eta (uncurry,args)
				   | OPTIONALe(alias_exp as Prim_e(NilPrimOp (make_onearg _),[], [c1,_],[Var_e v'])) =>
				     ((if !reduce_onearg_apps then do_onearg(c1, v, v', elist) else default())
					 handle e => (print "Error detected from do_onearg on expression ";
						      Ppnil.pp_exp exp;
						      print "\nwhere the function part has alias ";
						      Ppnil.pp_exp alias_exp; print "\n"; raise e))
				   | MUSTe f => doapp f
				   | _ => default())
			    | _ => default())
		     in doapp f
		     end
		| Raise_e (e, c) => Raise_e(do_exp state e, do_con state c)
		| Handle_e{body,bound,handler,result_type} =>
		     let val body = do_exp state body
		       val result_type = do_con state result_type
		       val ([(bound,_)],state') =
			 do_vclist state [(bound,Prim_c(Exn_c,[]))]
		       val handler = do_exp state' handler
		     in  Handle_e{body = body, bound = bound,
				  handler = handler,
				  result_type = result_type}
		     end

		(*Cancel fold/unfold if possible.
		 *)
		| Coerce_e args => do_coercion state args
		| ForgetKnown_e (sumcon, field) =>
		  let 
		      val sumcon = do_con state sumcon
		  in ForgetKnown_e (sumcon,field)
		  end
		| Fold_e (vars,from,to) =>
		  let val state = add_vars(state,vars)
		      val from = do_con state from
		      val to = do_con state to
		  in Fold_e (vars,from,to)
		  end
		| Unfold_e (vars,from,to) =>
		  let val state = add_vars(state,vars)
		      val from = do_con state from
		      val to = do_con state to
		  in Unfold_e (vars,from,to)
		  end)

	and do_coercion (state : state) (coercion1,cargs1,exp1) =
	  let
	    val default = fn () =>
	      let
		val coercion = do_exp state coercion1
		val cargs = map (do_con state) cargs1
		val exp = do_exp state exp1
	      in Coerce_e (coercion,cargs,exp)
	      end

	    val res =
	      case unalias (state,exp1)
		of Coerce_e (coercion2,cargs2,exp2) =>
		  (case (unalias (state,coercion1),unalias (state,coercion2))
		     of (Unfold_e _,Fold_e _) => (inc folds_reduced;do_exp state exp2)
		      | _ =>
		       (* This optimization is based on a particular operational
			* interpretation. Since coercions have no runtime effect,
			* we can always safely delete them without changing the runtime
			* behaviour.  However, they do have a typing effect, so we can
			* only cancel two coercions if we verify that the type of the
			* double application is the same as the type of the original
			* object being coerced.
			*)
		       if !coercion_cancel then
			 let
			   val (true,Coercion_c {vars,from,to}) = reduce_hnf(state,type_of(state,coercion1))
			   val to_type = Let_c (Sequential,Listops.map2 Con_cb (vars,cargs1),to)
			   val e2_type = type_of(state,exp2)
			 in
			   if sub_type(state,e2_type,to_type)
			     then (inc coercions_cancelled;
				   do_exp state exp2)
			   else default()
			 end
		       else default())
		 | _ => default()
	  in  res
	  end


	and do_switch (state : state) (switch : switch) : exp = 
	    let 

	      fun sum_switch {sumtype,arg,bound,arms,default,result_type} =
		let val arg = do_exp state arg
		    val (tagcount,_,carrier) = reduceToSumtype(state,sumtype)
		    val totalcount = TilWord32.uplus(tagcount,TilWord32.fromInt(length carrier))
		    fun make_ssum i = Prim_c(Sum_c{tagcount=tagcount,
						   totalcount=totalcount,
						   known=SOME i},
					     case carrier of
						 [_] => carrier
					       | _ => [NilDefs.con_tuple carrier])
		    fun do_arm(n,tr,body) =
			let val ssumtype = make_ssum n
			    val tr = do_niltrace state tr
			    val (_,state) = do_vclist state [(bound,ssumtype)]
			    val state = 
			      (case arg
				 of Var_e x => 
				   let
				     val q = ForgetKnown_e (sumtype,n)
				     val q = (case find_availE(state,q)
						of NONE => q
						 | SOME x => Var_e x)
				     val state = add_availE(state,Coerce_e(q,[],Var_e bound) ,x)
				   in state
				   end
				  | _ => state)
			in  (n,tr,do_exp state body)
			end

		    val tag_value =
			(case unalias(state,arg) 
			   of Coerce_e(q,_,inj) => 
			     (case unalias (state,q)
				of ForgetKnown_e (sumcon,w) => SOME (w,inj)
				 | _ => NONE)
			    | _ => NONE)

		in

		    case tag_value of
			SOME (w,inj) => 
			  let
			    val _ = inc switches_reduced
			  in
			    (* Reduce known switch *)
			    (case List.find (fn (w',_,_) => w = w') arms of
			       SOME (_,tr,arm_body) =>
				 do_exp state (NilUtil.makeLetE Sequential 
					       [Exp_b(bound,tr,inj)]
					       arm_body)
			     | NONE =>
				 do_exp state (Option.valOf default))
			     (* A switch which does not either cover all of
			      * the possibilities, or have a default, is 
			      * ill-formed.  We are in the case where a possibility
			      * was not covered, therefore default must be SOME.
			      *)
			  end
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
		fun int_switch int_sw =
		  let val {size,arg,arms,default,result_type} = flatten_int_sw int_sw
		  in
		    case (do_exp state arg) of
		      Const_e (Prim.int(_,n)) =>
			let val _ = inc switches_reduced
			  val n32 = TilWord64.toSignedHalf n
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
			end
		  end

	    in
	    (case switch of
		 Intsw_e int_sw => 
		   (case intsw_negate state int_sw
		      of SOME e => do_exp state e
		       | NONE => int_switch int_sw)
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
				 val Prim_c(Exntag_c, [con]) =
				   (case reduce_hnf(state,tagcon)
				      of (true,con) => con
				       | (false,_)  => error "Not able to reduce tagcon")
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
	       | Typecase_e _ => error "typecase not done"
	       | Ifthenelse_e _ => error "Ifthenelse not done yet")
	    end

	and do_function (state : state) ((v,c)
					 ,Function{effect,recursive,
						   tFormals, eFormals, fFormals,
						   body}) =
		let val arr = strip_arrow (state,c)
		    val {openness, tFormals = tFa, eFormals = eFa, fFormals = fFa, ...} =
			NilUtil.rename_arrow (arr, tFormals)

		    val state = enter_var(state,v)
		    val (tFa,state) = do_vklist state tFa
		    val (r,state) = do_eFormals state (ListPair.map (fn ((v, tr), c) => (v, tr, c)) (eFormals, eFa))
		    val (eFormals, eFa) = unzip (map (fn (v, tr, c) => ((v, tr), c)) r)
		    val body = do_exp state body
		    val c = do_con state c
		in  ((v, c),
		     Function{effect=effect,recursive=recursive,
				tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
				body=body})
		end

	and do_niltrace state niltrace =
	  let
	    fun compute path =
	      let
		val c = NilDefs.path2con path
		val c = do_con state c
	      in con2trace c
	      end
	    val res =
	      case niltrace of
		TraceCompute v => compute (v,[])
	      | TraceKnown (TraceInfo.Compute path) => compute path
	      | _ => niltrace
	  in res
	  end
	and do_bnds(bnds : bnd list, state : state) : bnd list * state =
	    let
		val bnds = flattenBnds bnds
		val (newbnds_list,state) = foldl_acc do_bnd state bnds
		val newbnds = List.concat newbnds_list
	    in  (newbnds,state)
	    end

	and do_bnd (bnd : bnd, state : state) : bnd list * state =
	  let
	      (* Rewrite a binding to do the following:
	         (a) check record arguments are variables
		 (b) check inject argument are variables
		 (c) rewrite project to project_known
		 If NONE is returned, the binding does not need to be rewritten.
		 Otherwise, SOME bindings are returned.  The original binding
		   is guaranteed not to occur in this list.  This prevents looping. *)
	      fun rewrite_bnd(v,niltrace,e) =
		  (case e of
		       Prim_e(NilPrimOp(record labs),_, _,elist) =>
	                 let fun check(Var_e v) = ()
			       | check (Const_e v) = ()
			       | check _ = error "record argument is not a variable"
			     val _ = app check elist
			 in  NONE
			 end

		     | Prim_e(NilPrimOp (box_float _), _, _, [Var_e _]) => NONE
		     | Prim_e(NilPrimOp (box_float _), _, _, [Const_e _]) => NONE
		     | Prim_e(NilPrimOp (box_float _), _, _, _ ) =>
			 error "box_float argument is not a value"

		     | Prim_e(NilPrimOp (inject _), _, _, [Var_e _]) => NONE
		     | Prim_e(NilPrimOp (inject _), _, _, [Const_e _]) => NONE
		     | Prim_e(NilPrimOp (inject k), _, clist,[injectee]) => error "inject argument is not a value"

		     | Prim_e(NilPrimOp (project k), _, [sumcon],[Var_e sv]) =>
			 let val sv_con = find_con(state,sv)
			     val (tagcount,k,clist) = reduceToSumtype(state,sv_con)
			     val known = (case k of
					  SOME k => k
					| NONE => (print "Expression: "; Ppnil.pp_exp e;
						   print "\nVariable "; Ppnil.pp_var sv;
						   print " has non-special-sum type:\n";
						   Ppnil.pp_con sv_con; print "\n\n";
						   error "Type is not a special sum\n"))
			     val fieldcon = List.nth(clist, TilWord32.toInt
						            (TilWord32.uminus(known,tagcount)))
			 in  case reduce_hnf(state, fieldcon) of
			     (true, _) =>
				 let val bnd = Exp_b(v,niltrace,
						   Prim_e(NilPrimOp(project_known known),[],[sumcon],[Var_e sv]))
				 in  SOME [bnd]
				 end
			     | _ => NONE
			 end
		     | Prim_e(NilPrimOp (project k), _, _, _) => error "project argument is not a value"
		     | Prim_e(PrimOp p, _,clist,elist) =>
			 (* apply may return a let (not a-normal form).  catching this here 
			  * instead of waiting for do_prim makes us "more idempotent"
			  *)
			 (case (getVals state elist) 
			    of NONE => NONE
			     | SOME elist =>
			      ((case (NilPrimUtil.apply (get_env state) p clist elist)
				  of Let_e (_,bnds,body) => SOME (bnds @ [Exp_b(v,niltrace,body)])
				   | e => SOME [Exp_b(v,niltrace,e)])
				  handle _ => NONE))
				 
		     | _ => NONE)
	  in	(case bnd of
		     Exp_b(v,niltrace,e) =>
			  (case rewrite_bnd(v,niltrace,e) of
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
				      val eff = not (valuable(state,e))

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
					     | Prim_e(NilPrimOp (box_float _), _, _, _) =>
						   (false, OPTIONALe e)
					     | Prim_e(NilPrimOp roll, _, _, _) =>
						   (false, OPTIONALe e)

					     | App_e(openness,Var_e v,clist,elist,eflist) =>
						   let
						     fun uncurry v =
						       (case (lookup_alias(state,v)) of
							  ETAe (depth,uncurry,args) =>
							    (if (depth > 1)
							       then
								 let val new_info = (depth-1,uncurry,
										     args @ [(clist,elist,eflist)])
								 in  (false,ETAe new_info)
								 end
							     else (eff,OPTIONALe e))
							| MUSTe (Var_e v) => uncurry v
							| _ => (eff,OPTIONALe e))
						   in uncurry v
						   end
					     | _ => (eff,OPTIONALe e))

				      val _ = if effect then use_var(state,v) else ()
				      val state = (case alias 
						     of MUSTe _ => state
						      | _ => add_availE(state, e, v))

				      val state = add_alias(state,v,alias)
				      val state = add_exp(state,v,e)
				  in  ([Exp_b(v,niltrace,e)], state)
				  end
			    | SOME bnds => do_bnds(bnds,state))
		   | Con_b(p,cbnd) => let val (cbnd,state) = do_cbnd(cbnd,state)
				      in  ([Con_b(p,cbnd)], state)
				      end
		   | Fixopen_b vfset => 
		     let 
		       val vflist = Sequence.toList vfset
		       val (cbndslists_vflistlist,state) = foldl_acc rewrite_uncurry state vflist
		       val (cbndslists,vflistlist) = unzip cbndslists_vflistlist
			 
		       val cbnds = Listops.flatten cbndslists
		       val (cbnds,state) = foldl_acc do_cbnd state cbnds
			 
		       val vflist = Listops.flatten vflistlist
		       fun folder (((v,c),f),state) = add_con(state,v,c)
		       val state = foldl folder state vflist
		       val vflist = map (do_function state) vflist
		       val (state,fixbnds) = 
			 (case vflist
			    of [((v,_),Function{effect=effect,recursive=recursive,
						tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
						body=Let_e(_,[Exp_b(x,_,App_e(openness,Var_e v',clist,elist,eflist))],Var_e x')})] =>
			      let
				fun ccheck(a,Var_c a') = Name.eq_var (a,a') 
				  | ccheck _ = false
				fun echeck((x,_),Var_e x') = 
				  Name.eq_var(x,x') (*orelse (var_is_unit(state,x) andalso var_is_unit(state,x'))*)
				  | echeck _ = false
				fun efcheck(x,Var_e x') = Name.eq_var(x,x')
				  | efcheck _ = false
			      in
				if not (Name.eq_var(v,v')) andalso (Name.eq_var(x,x')) andalso
				  (Listops.all2 ccheck (tFormals,clist)) andalso
				  (Listops.all2 echeck (eFormals,elist)) andalso
				  (Listops.all2 efcheck (fFormals,eflist)) 
				  then (inc fun_eta;
					(add_alias(state,v,MUSTe(Var_e v')),[]))
				else (state,[Fixopen_b(vflist)])
			      end
			     | _ => (state,[Fixopen_b(vflist)]))
		     in  ((NilUtil.cbnds2bnds cbnds) @ fixbnds, state)
		     end

		   | Fixcode_b vfset =>
		     let 
		       val vflist = Sequence.toList vfset
		       val state = add_vars(state,map (#1 o #1)  vflist)
		       val vflist = map (do_function state) vflist
		     in  ([Fixcode_b(Sequence.fromList vflist)], state)
		     end
		   | Fixclosure_b (recur,vclset) =>
		     let 
		       val vcllist = Sequence.toList vclset
		       val state = add_vars(state,map (#1 o #1) vcllist)
		       val state' = enter_var(state,#1(#1(hd vcllist)))
		       fun do_closure {code,cenv,venv} =
			 let val _ = do_exp state' (Var_e code)
			   val cenv = do_con state' cenv
			   val venv = do_exp state' venv
			 in  {code=code,cenv=cenv,venv=venv}
			 end
		       val vcllist = map (fn (v,f) => (v,do_closure f)) vcllist
		     in  ([Fixclosure_b(recur,Sequence.fromList vcllist)], state)
		     end)
	  end

	fun do_import(ImportValue(l,v,tr,c),state) = 
	  let
	    val state = add_var (state,v) 
	    val inner_state = enter_var (state,v) 
	    val tr = do_niltrace inner_state tr
	    val c = do_con inner_state c
	    val state = add_con(state,v,c)
	    val _ = if not (!kill_imports) orelse Name.keep_import l then use_var(state,v) else ()
	  in (ImportValue(l,v,tr,c),state)
	  end
	  | do_import(ImportType(l,v,k),state)  = 
	  let
	    val state = add_var (state,v) 
	    val inner_state = enter_var (state,v) 
	    val k = do_kind inner_state k
	    val state = add_kind(state,v,k)
	    val _ = if not (!kill_imports) orelse Name.keep_import l then use_var(state,v) else ()
	  in (ImportType(l,v,k),state)
	  end
	  | do_import(ImportBnd (phase, cb),state)  =
	  let
	    val (cb, state) = do_cbnd (cb, state)
	  in
	    (ImportBnd (phase, cb),
	     state)
	  end

	fun do_export state (ExportValue(l,v)) =
	    let val v = (case lookup_alias(state,v) of
			     MUSTe (Var_e v) => v
			   | _ => v)
		val _ = use_var(state,v)
	    in	(NONE, ExportValue(l,v))
	    end
	  | do_export state (ExportType(l,v)) =
	    let val v = (case lookup_alias(state,v) of
			     MUSTc (Var_c v) => v
			   | _ => v)
		val _ = use_var(state,v)
	    in  (NONE, ExportType(l,v))
	    end


	fun filter_imports state imports = 
	  let
	    fun import_used imp = 
	      let 
		fun dolv (l,v) = 
		  if not (!kill_imports) then true
		  else if is_used_var (state,v) then true
		  else
		    let val _ = (inc imports_killed;
				 chat2 ("Filtering label " ^ Name.label2name l ^ "\n"))
		    in false
		    end
		  
	      in
		case imp
		  of ImportBnd (_, cbnd) => cbnd_used state cbnd
		   | ImportType (l,v,_) => dolv (l,v)
		   | ImportValue (l,v,_,_) => dolv (l,v)
	      end
	    
	    val imports = List.filter import_used imports
	  in imports 
	  end

	fun use_export state exp = 
	  (case exp
	     of ImportValue(l,v,tr,c) => use_var (state,v)
	      | ImportType(l,v,k)  =>  use_var (state,v)
	      | _ => ())

	fun optimize params (MODULE{imports, exports, bnds,exports_int}) =
	  let
	      val _ = reset_debug()
	      val _ = reset_stats()

	      val state = newState params
	      val (imports,state) = foldl_acc do_import state imports
	      val (bnds,state) = do_bnds(bnds,state)
	      val (exports_int,state) = 
		(case exports_int
		   of SOME ei => 
		     let
		       val (ei,state) = foldl_acc do_import state ei
		     in (SOME ei,state)
		     end
		    | NONE => (NONE,state))

	      (* we "retain" the state so that no exports are optimized away *)
	      val state = retain_state state
	      val temp = map (do_export state) exports
	      val export_bnds = List.mapPartial #1 temp
	      val exports = map #2 temp
	      val bnds = if (null export_bnds) then bnds else bnds @ export_bnds

	      val exports_int = 
		(case exports_int
		   of SOME ei => 
		     let
		       val () = List.app (use_export state) ei
		       val ei = filter_imports state ei
		     in SOME ei
		     end
		    | NONE => NONE)
              val bnds = Listops.map_concat (bnd_used state) bnds
	      val bnds = flattenBnds bnds
	      val imports = filter_imports state imports

	      val _ = chat_stats ()
	      val _ = reset_debug()
	      val _ = reset_stats()

	  in  MODULE{imports=imports,exports=exports,bnds=bnds,exports_int = exports_int}
	  end



	fun optimize_int params (INTERFACE{imports, exports}) =
	  let
	      val _ = reset_debug()
	      val _ = reset_stats()

	      val state = newState params
	      val (imports,state) = foldl_acc do_import state imports
	      val (exports,state) = foldl_acc do_import state exports

	      (* we "retain" the state so that no exports are optimized away *)
	      val state = retain_state state
	      val () = List.app (use_export state) exports
	      val imports = filter_imports state imports
	      val exports = filter_imports state exports

	      val _ = chat_stats ()
	      val _ = reset_debug()
	      val _ = reset_stats()

	  in  INTERFACE{imports=imports,exports=exports}
	  end

end
