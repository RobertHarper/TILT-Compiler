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

structure LilOptimize :> LILOPTIMIZE =
struct
  open Lil
  structure LU = LilUtil
  structure LD = LilDefs
  structure LS = LilSubst
  structure LC = LilContext
  structure Table = OpTable
  structure EQ = LilTypeEquiv
  structure R = Reduce
  structure Typeof = Synthesis.Typeof
  structure Dec = Deconstruct.Dec
  structure VarSet = Name.VarSet
  structure LO = Listops

  fun cout (c : Lil.con) : Lil.con_ = #c c
  fun kout (k : Lil.kind) : Lil.kind_ = #k k

  exception None

  fun obind opt f = 
    (case opt 
       of SOME a => f a
	| NONE => NONE)

  val error = fn s => Util.error "optimize.sml" s

  val warn = fn s => print ("WARNING: "^s^"\n")

  val debug = Stats.ff("LilOptimizeDebug")
  val debuglev = ref 0
  fun debugdo (i,t) = if (!debug) andalso (i <= !debuglev) then (t(); ()) else ()

  fun inc r = r := !r + 1
  fun dec r = r := !r - 1
    
  val chatlev = ref 0
  val folds_reduced = ref 0
  val coercions_cancelled = ref 0
  val switches_flattened  = ref 0
  val switches_reduced    = ref 0
  val record_eta = ref 0
  val fun_eta = ref 0
  val record_beta = ref 0
  val boxes_cancelled = ref 0
    
  fun reset_stats() =
    let in
      folds_reduced :=  0;
      switches_flattened := 0;
      switches_reduced := 0;
      coercions_cancelled := 0;
      record_eta := 0;
      fun_eta := 0;
      record_beta := 0;
      boxes_cancelled := 0
    end
  
  fun chat lev str = if (!chatlev) >= lev then print str else ()
  val chat0 = chat 0
  val chat1 = chat 1
  val chat2 = chat 2
    
  fun chat_stats () =
    if !chatlev > 0 then
      (print "\t"; print (Int.toString (!folds_reduced));    print " fold/unfold pairs reduced\n";
       print "\t"; print (Int.toString (!coercions_cancelled));    print " other coercions reduced\n";
       print "\t"; print (Int.toString (!switches_reduced)); print " known switches reduced\n" ;
       print "\t"; print (Int.toString (!record_eta));       print " records eta reduced\n";
       print "\t"; print (Int.toString (!fun_eta));          print " functions eta reduced\n";
       print "\t"; print (Int.toString (!record_beta));      print " records beta reduced\n";
       print "\t"; print (Int.toString (!boxes_cancelled));  print " box/unboxes reduced\n"
       ) else ()



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

  datatype equivalent = 
    UNKNOWN
    | OPTIONAL32 of op32
    | MUST32 of sv32
    | OPTIONAL64 of op64
    | MUST64 of sv64

  fun pp_alias UNKNOWN = print "unknown"
    | pp_alias (OPTIONAL32 oper) = (print "OPTIONAL32 "; PpLil.pp_op32 oper)
    | pp_alias (MUST32 sv) = (print "MUST32 "; PpLil.pp_sv32 sv)
    | pp_alias (OPTIONAL64 oper) = (print "OPTIONAL64 "; PpLil.pp_op64 oper)
    | pp_alias (MUST64 sv) = (print "MUST64 "; PpLil.pp_sv64 sv)

  local
    type params = {doCse : bool}

    type entry = used_state ref * equivalent
    datatype state = STATE of {ctxt : LC.context,
			       cglobals : Name.VarSet.set,
			       subst : LS.con_subst,
			       current : used_state ref,
			       mapping : entry Name.VarMap.map,
			       avail : var Table.Op32map.map * var Table.Op64map.map,
			       params : params}

    fun isused r =
      let 
	fun loop count current [] = current
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
    
    fun update_mapping(STATE{params,subst,ctxt, current, avail, cglobals,...}, mapping) =
      STATE{params=params,subst = subst,cglobals = cglobals,
	    ctxt=ctxt, avail = avail,
	    current=current,mapping=mapping}
      
  in
    
    type state = state

    fun newState (params,cglobals) = STATE {ctxt = LC.empty(),
					    subst = LS.C.empty(),
					    cglobals = cglobals,
					    current = ref (USED 1),
					    avail = (Table.Op32map.empty,
						     Table.Op64map.empty),
					    mapping = Name.VarMap.empty,
					    params = params}
    fun getParams (STATE{params,...}) = params

    (* Mark the current binding as used,
     * so that all variables encountered will be
     * kept.
     *)
    fun retain_state(STATE{ctxt, cglobals, subst,current, mapping, avail, params}) =
      STATE{ctxt=ctxt, subst = subst, avail=avail, cglobals = cglobals,
	    current=ref (USED 1),mapping=mapping,
	    params=params}
			

    fun enter_var(STATE{ctxt, cglobals, subst, current, mapping, avail, params}, v) =
      STATE{ctxt=ctxt,
	    subst = subst,
	    cglobals = cglobals,
	    mapping = mapping,
	    avail = avail,
	    params = params,
	    current=
	    (case (Name.VarMap.find(mapping,v)) 
	       of
		 NONE => error "enter_var given var not in used map"
	       | SOME (us,_) => us)}

    fun add_vars(state as STATE{mapping,...},vars) =
      let val r = ref UNUSED
	val mapping = foldl (fn (v,m) => Name.VarMap.insert(m,v,(r,UNKNOWN))) mapping vars
      in  update_mapping(state,mapping)
      end

    fun add_var(state,v) = add_vars(state,[v])

    fun use_var(STATE{mapping,current,...},v) =
      (
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
		 PpLil.pp_var v; print "\n";
		 error "is_used_var given var not in state")
      | SOME (r,_) => isused r

    fun is_used_var(state,v) = (case get_varuse(state,v) of
				  USED _ => true
				| UNUSED => false)
      
    fun add_alias(state as STATE{mapping,...},v,alias) =
      let 
	val SOME(use,_) = Name.VarMap.find(mapping,v)
	val mapping = Name.VarMap.insert(mapping,v,(use,alias))
	  
      in  update_mapping(state,mapping)
      end

    fun lookup_alias(STATE{mapping,...},v) =
      (case (Name.VarMap.find(mapping,v)) of
	 NONE => UNKNOWN
       | SOME (_,alias) => alias)
	 
    (*Get an alias if possible, otherwise
     * return the original expression
     *)
    fun unaliassv32 (state,e) =
      let
	fun loop (Var_32 v) =
	  (case lookup_alias(state,v) 
	     of MUST32 sv => loop sv
	     | OPTIONAL32(Val sv) => loop sv
	     | _ => e)
	  | loop e = e
      in loop e
      end
    fun unaliassv64 (state,e) =
      let
	fun loop (Var_64 v) =
	  (case lookup_alias(state,v) 
	     of MUST64 sv => loop sv
	      | OPTIONAL64(Val_64 sv) => loop sv
	      | _ => e)
	  | loop e = e
      in loop e
      end
    fun unaliasop32 (state,e) =
      let
	fun loop (Var_32 v) =
	  (case lookup_alias(state,v) 
	     of MUST32 sv => loop sv
	      | OPTIONAL32 oper => oper
	      | _ => Val(Var_32 v))
	  | loop e = Val e
      in loop e
      end
    fun unaliasop64 (state,e) =
      let
	fun loop (Var_64 v) =
	  (case lookup_alias(state,v) 
	     of MUST64 sv => loop sv
	      | OPTIONAL64 oper => oper
	      | _ => Val_64(Var_64 v))
	  | loop e = Val_64 e
      in loop e
      end

    fun get_subst(STATE{subst,...}) = subst
      
    fun set_subst(STATE{avail,subst = _, cglobals,ctxt,current,mapping,params},subst) =
      STATE{ctxt=ctxt,
	    cglobals = cglobals,
	    subst = subst,
	    avail=avail,
	    mapping=mapping,
	    current=current,
	    params=params}

    fun get_cglobals(STATE{cglobals,...}) = cglobals
      
    fun set_cglobals(STATE{avail,subst, cglobals = _ ,ctxt,current,mapping,params},cglobals) =
      STATE{ctxt=ctxt,
	    cglobals = cglobals,
	    subst = subst,
	    avail=avail,
	    mapping=mapping,
	    current=current,
	    params=params}

    fun add_cglobal state a = set_cglobals(state,VarSet.add(get_cglobals state,a))

    fun get_context(STATE{ctxt,...}) = ctxt
      
    fun set_context(STATE{avail,subst, cglobals,ctxt=_,current,mapping,params},ctxt) =
      STATE{ctxt=ctxt,
	    cglobals = cglobals,
	    subst = subst,
	    avail=avail,
	    mapping=mapping,
	    current=current,
	    params=params}

    fun set_context_and_csubst(STATE{avail,cglobals,subst, ctxt=_,current,mapping,params},(ctxt,subst')) =
      STATE{ctxt=ctxt,
	    subst = LS.C.compose(subst',subst),
	    cglobals = cglobals,
	    avail=avail,
	    mapping=mapping,
	    current=current,
	    params=params}

    fun replace_con state (a,c) = set_subst(state,LS.C.addl (a,c,get_subst state))
    fun do_csubst (state,c) = LS.substConInCon (get_subst state) c

    fun bind_var32 (env,(var,t)) = set_context(env,LC.bind_var32(get_context env,(var,t)))
    fun bind_label (env,(lbl,t)) = set_context(env,LC.bind_label(get_context env,(lbl,t)))
    fun bind_var64 (env,(var,t)) = set_context(env,LC.bind_var64(get_context env,(var,t)))
    fun bind_cvar (env,(var,k))   = set_context(env,LC.bind_cvar(get_context env,(var,k)))
    fun bind_kvar (env,var)      = set_context(env,LC.bind_kvar(get_context env,var,LC.Any))

    fun bind_cvars (env,vks)   = set_context(env,LC.bind_cvars(get_context env,vks))

    fun typeof_sv32 (env,arg)  = (Typeof.sv32 (get_context env) arg) 
      handle any => (print "ERROR in typeof_sv32\n";raise any)
    fun typeof_sv64 (env,arg)  = (Typeof.sv64 (get_context env) arg) 
      handle any => (print "ERROR in typeof_sv64\n";raise any)
    fun pexp_define(env,arg)   = (set_context_and_csubst(env,Typeof.pexp (get_context env,arg))) 
      handle any => (print "ERROR in pexp_define\n";raise any)

    fun op32_define(env,arg)   = (set_context(env,Typeof.bind_op32(get_context env,arg)))
      handle any => (print "ERROR in op32_define\n";raise any)
    fun op64_define(env,arg)   = (set_context(env,Typeof.bind_op64(get_context env,arg)))
      handle any => (print "ERROR in op64_define\n";raise any)
    fun unpack_define(env,arg) = (set_context(env,Typeof.bind_unpack(get_context env,arg))) 
      handle any => (print "ERROR in unpack_define\n";raise any)
    fun split_define(env,(a1,a2,c))  = 
      set_context_and_csubst(env,Typeof.bind_split(get_context env,(a1,a2,do_csubst (env,c))))
    fun unfold_define(env,(a,c)) = 
      set_context_and_csubst(env,Typeof.bind_unfold(get_context env,(a,do_csubst (env,c))))
    fun inj_define(env,(w,a,c,sv))    = 
      set_context_and_csubst(env,Typeof.bind_inj(get_context env,(w,a,do_csubst (env,c),sv)))

    fun find_cvar env var  = ((LC.find_cvar(get_context env,var)) 
			      handle LC.Unbound s => error ("Unbound cvariable: "^(Name.var2string var)))
    fun find_var32 env var = ((do_csubst(env,LC.find_var32(get_context env,var))) 
			      handle LC.Unbound s => error ("Unbound 32variable: "^(Name.var2string var)))
    fun find_label env lbl = ((do_csubst(env,LC.find_label(get_context env,lbl))) 
			      handle LC.Unbound s => error ("Unbound label: "^(Name.label2string lbl)))
    fun find_var64 env var = ((do_csubst(env,LC.find_var64(get_context env,var))) 
			      handle LC.Unbound s => error ("Unbound 64variable: "^(Name.var2string var)))
	
    fun find_avail32(STATE{avail,params,...},oper) =
      if (#doCse params) then Table.Op32map.find(#1 avail,oper) else NONE

    fun find_avail64(STATE{avail,params,...},oper) =
      if (#doCse params) then Table.Op64map.find(#2 avail,oper) else NONE


    (* Valuable expressions are expressions which do not have effects of
     * any kind.  These may be eliminated as dead code, or replicated, or
     * coalesced at will
     *)
    fun notvaluable32 e = LU.anyEffect32 e
    fun notvaluable64 e = LU.anyEffect64 e

    (* Pure expressions are expressions which do not have
     * any store effects (that is, they neither depend on nor modify
     * the store.  This means that either
     *    1. They always compute the same value
     * or 2. They always diverge or throw an exception
     * Pure expressions cannot be eliminated without changing the
     * behaviour of the program, but they can be coalesced with
     * a previous binding of the same expression. c.g. Tarditi 6.1
     *)
    fun pure32 (state, e) = not (LU.storeEffect32 e)
    fun pure64 (state, e) = not (LU.storeEffect64 e)

    fun add_avail32(state as STATE{mapping,cglobals,current,ctxt,subst,avail,params},e,v) =
      if (#doCse params andalso pure32(state,e))
	then STATE{params=params,
		   cglobals = cglobals,
		   mapping=mapping,
		   current=current,
		   ctxt=ctxt,
		   subst = subst,
		   avail=(Table.Op32map.insert(#1 avail,e,v),
			  #2 avail)}
      else state

    fun add_avail64(state as STATE{mapping,cglobals,current,ctxt,subst,avail,params},e,v) =
      if (#doCse params andalso pure64(state,e))
	then STATE{params=params,
		   cglobals = cglobals,
		   mapping=mapping,current=current,
		   ctxt=ctxt,subst = subst,
		   avail=(#1 avail,
			  Table.Op64map.insert(#2 avail,e,v))}
      else state


    fun closed_con state c = VarSet.isSubset(free_cvars_con c,get_cglobals state)

    fun closed_sv32 state sv =
      (case sv
	 of Var_32 _ => false
	  | Label _ => true
	  | Coercion (tag,cons) => List.all (closed_con state) cons
	  | Coerce (sv1,sv2) => closed_sv32 state sv1 andalso closed_sv32 state sv2
	  | Tabs ((a,_),sv) => closed_sv32 (add_cglobal state a) sv
	  | TApp (sv,con) => closed_sv32 state sv andalso closed_con state con
	  | Const_32 _ => true
	  | Tag _ => true
	  | Unit => true)

    fun closed_sv64 sv =
      (case sv
	 of Var_64 _ => false
	  | Const_64 _ => true)


    fun get_exp_val (e : exp) =
      (case #e e
	 of Val32_e sv => SOME sv
	  | _ => NONE)
	 
    fun get_exp_cc_val cc =
      (case cc 
	 of Exp_cc e => get_exp_val e
	  | _ => NONE)
	 
    (* Does not rewrite subterms. Only use as a sub-routine.*)
    fun reduce_coercion (state : state) (coercion1,arg1) = 
      let
	val res =
	  (case Dec.E.coerce' (unaliassv32 (state,arg1))
	     of SOME(coercion2,arg2) =>
	       let
	       in
		 case (unaliassv32 (state, coercion1),unaliassv32 (state, coercion2))
		   of (Coercion(Unroll, _),Coercion(Roll,_)) => (inc folds_reduced;SOME arg2)
		    | (Coercion(ForgetKnown,_),Coercion(InjUnion,[ksum])) => 
		     let
		       val new = LD.Q.injforget ksum 
		     in 
		       case reduce_coercion state (new,arg2)
			 of NONE => SOME (LD.E.injforget' ksum arg2)
			  | res => res
		     end
		    | _ => 
		     (* This optimization is based on a particular operational
		      * interpretation. Since coercions have no runtime effect,
		      * we can always safely delete them without changing the runtime
		      * behaviour.  However, they do have a typing effect, so we can
		      * only cancel two coercions if we verify that the type of the
		      * double application is the same as the type of the original
		      * object being coerced.
		      * Note that this subsumes the ProjKnown(InjUnion) optimization.
		      *)
		     let
		       val (_,to) = Dec.C.coercion (typeof_sv32 (state,coercion1))
		       val from = typeof_sv32 (state,arg2)
		     in 
		       if EQ.C.equal from to then (inc coercions_cancelled; SOME arg2)
		       else NONE
		     end
	       end
	      | _ => NONE)
      in res
      end

    fun remember_forgotten state  arg = 
      let
	val tag_value =
	  (case Dec.E.coerce' (unaliassv32(state,arg))
	     of SOME(q,inj) => 
	       (case Dec.Q.forgetknown' (unaliassv32 (state,q))
		  of SOME ksum => 
		    let
		      val (w,_,_) = Dec.C.ksum ksum
		      val w = Dec.C.nat w
		    in SOME (w,inj)
		    end
		   | NONE => (case Dec.Q.injforget' (unaliassv32 (state,q))
				of SOME ksum => 
				  let
				    val (w,_,_) = Dec.C.ksum ksum
				    val w = Dec.C.nat w
				  in SOME (w,LD.E.injunion' ksum inj)
				  end
				 | _ => NONE))
	      | _ => NONE)
      in tag_value
      end
    fun lilbool2bool state sv =
      (case remember_forgotten state sv 
	 of SOME (0w0,_) => SOME false
	  | SOME(0w1,_) => SOME true
	  | _ => NONE)
	 
    fun lilboolexp2bool state e =
      obind (get_exp_val e)
      (fn sv => lilbool2bool state sv)


    fun get_coercedbool state (qs,exp) = 
      let
	fun reduce_loop (qs,sv) = 
	  (case qs
	     of [] => SOME sv
	      | q::qs => 
	       (case reduce_coercion state (q,sv)
		  of SOME sv => reduce_loop(qs,sv)
		   | NONE => NONE))
      in 
	obind (get_exp_val exp)
	(fn sv => obind (reduce_loop (qs,sv))
	 (fn sv => lilbool2bool state sv))
      end

    fun coerce_exp qs e = 
      let
	val e = P.Lili.from_exp e
	val e = P.bind e (fn sv => P.ret (LD.Q.coerce_many qs sv))
      in P.Lili.to_exp e
      end

    fun valuable_cc cc = 
      (case cc
	 of Exp_cc e => isSome (get_exp_val e)
	  | Not_cc cc => valuable_cc cc
	  | And_cc (cc1,cc2) => (valuable_cc cc1) andalso (valuable_cc cc2)
	  | Or_cc (cc1,cc2) =>  (valuable_cc cc1) andalso (valuable_cc cc2))
	 
    (* inline indicates whether or not it is safe to 
     * inline the contents of the operator (safe when the switch 
     * was either local or known to be used linearly here)
     *)
    fun reduce_nested_if (inline,state,qs,oper) = 
      let
      in
	case oper
	  of Switch (Ifthenelse {arg,thenArm,elseArm,rtype}) =>
	    (case (inline,get_coercedbool state (qs,thenArm),get_coercedbool state (qs, elseArm))
	       of (_,SOME true,SOME false) => if inline orelse (valuable_cc arg) then SOME arg else NONE
		| (_,SOME false,SOME true) => if inline orelse (valuable_cc arg) then SOME (Not_cc arg) else NONE
		| (_,SOME a,SOME a')  => SOME (Exp_cc (mk_exp(Val32_e(LD.E.bool' a))))  (* a == a'*)
		| (true,SOME true,_)  => SOME (Or_cc(arg,Exp_cc(coerce_exp qs elseArm)))
		| (true,SOME false,_) => SOME (And_cc(Not_cc arg,Exp_cc(coerce_exp qs elseArm)))
		| (true,_,SOME true)  => SOME (Or_cc(Not_cc arg,Exp_cc(coerce_exp qs thenArm)))
		| (true,_,SOME false) => SOME (And_cc(arg,Exp_cc(coerce_exp qs thenArm)))
		| _ => NONE)
	   | _ => NONE
      end
	 
    fun reduce_exp_cc state e = 
      (case get_exp_val e
	 of SOME sv => 
	   let
	     val (qs,sv) = Dec.E.coercen sv
	   in reduce_nested_if(false,state,qs,unaliasop32 (state,sv))
	   end
	  | NONE => 
	   (case #e e
	      of Let_e([Exp32_b(x,oper)],body) => 
		(case #e body
		   of Val32_e sv => 
		     let
		       val (qs,sv) = Dec.E.coercen sv
		     in case sv
			  of Var_32 x' => 
			    (if Name.eq_var (x,x') then 
			       reduce_nested_if(true,state,qs,oper)
			     else NONE)
			   | _ => NONE
		     end
		    | _ => NONE)
	       | _ => NONE))

    (* Work way the fuck too hard to reduce switches to a form
     * from which we can generate good code.  What *freak* made bool
     * an abstract datatype anyway?  You realize that we pay
     * a function call (vararg-ed no less) for negating a condition?
     *
     * The thing that makes this all a pain in the butt is that we can't 
     * just look for true and false.  We have to look for 
     * "bool_in@true" and "bool_in@false".  Yeesh.
     *)
    fun reduce_cc state cc = 
      let
		     
	fun static_cc b = Exp_cc (mk_exp (Val32_e (LD.E.bool' b)))
	  
	  
	fun static cc = 
	  obind (get_exp_cc_val cc)
	  (fn sv => lilbool2bool state sv)

      in
	case cc
	  of Exp_cc e => 
	    (case reduce_exp_cc state e
	       of SOME cc => reduce_cc state cc
		| NONE => Exp_cc e)
	   | Not_cc cc => 
	       let
		 val cc = reduce_cc state cc
	       in case static cc
		    of SOME true => static_cc false
		     | SOME false => static_cc true
		     | NONE => Not_cc (reduce_cc state cc)
	       end
	    | And_cc(cc1,cc2) => 
	       let
		 val cc1 = reduce_cc state cc1
		 val cc2 = reduce_cc state cc2
	       in case (static cc1,static cc2)
		    of (SOME tag1,SOME tag2) => static_cc (tag1 andalso tag2)
		     | (SOME true,NONE) => cc2
		     | (SOME false,NONE) => static_cc false
		     | (NONE,SOME tag) => 
		      if tag then cc1  (* cc2 is valuable and true*)
		      else             (* cc2 is valuable and false *)
			if valuable_cc cc1 then  
			  static_cc false
			else And_cc (cc1,static_cc false)
		     | (NONE, NONE) => And_cc (cc1,cc2)
	     end
	    | Or_cc (cc1,cc2) => 
	     let
	       val cc1 = reduce_cc state cc1
	       val cc2 = reduce_cc state cc2
	     in case (static cc1,static cc2)
		  of (SOME tag1,SOME tag2) => static_cc (tag1 orelse tag2)
		   | (SOME true,NONE) => static_cc true
		   | (SOME false,NONE) => cc2
		   | (NONE,SOME tag) => 
		    if tag then (* cc2 is valuable and true *)
		      if valuable_cc cc1 then
			static_cc true   
		      else Or_cc(cc1,cc2)
		    else        (* cc2 is valuable and false *)
		      cc1
		   | (NONE, NONE) => Or_cc (cc1,cc2)
	     end
      end


    (* Without knowing more about the valuability of oper, 
     * we may only sink into the leftmost cc, which is guaranteed to 
     * be evaluated.
     *)
    fun sink state (x,oper,cc) = 
      let 
	fun build_exp sv = mk_exp(Let_e([Exp32_b(x,oper)],mk_exp (Val32_e sv)))
	  
	fun replace_exp (qs,sv) =
	  (case sv
	     of Var_32 x' => 
	       if Name.eq_var (x,x') then  
		 (case reduce_nested_if (true,state,qs,oper)
		    of SOME cc => cc
		     | NONE => Exp_cc (build_exp (LD.Q.coerce_many qs sv)))
	       else raise None
            | _ => raise None)

	fun sink_exp e =
	  (case get_exp_val e
	     of NONE => raise None
	      | SOME sv => replace_exp(Dec.E.coercen sv))

	fun sink' cc = 
	  (case cc
	     of Not_cc cc => Not_cc(sink' cc)
	      | And_cc(cc1,cc2) => And_cc(sink' cc1,cc2)
	      | Or_cc(cc1,cc2)  => Or_cc(sink' cc1,cc2)
	      | Exp_cc e => sink_exp e)
      in (SOME(sink' cc)) handle None => NONE
      end


    fun bnd_used state bnd =
      (case bnd 
	 of (Exp32_b(v,e)) => if is_used_var(state,v) then SOME bnd else NONE
	  | (Exp64_b(v,e)) => if is_used_var(state,v) then SOME bnd else NONE
	  | (Unpack_b (a,x,sv)) => if is_used_var(state,x) orelse is_used_var(state,a) then SOME bnd else NONE
	  | (Fixcode_b vflist) => 
	   let
	     val vflist = List.filter (fn (v,f) => is_used_var (state,v)) vflist
	   in case vflist 
		of [] => NONE
		 | _ => SOME (Fixcode_b vflist)
	   end
	  | _ => SOME bnd  (* We could eliminate these, but it's a but more subtle than
			    * I want to do right now.  Note that redundant refinements
			    * get eliminated anyway.
			    *)
	   )

    (* Try to fold bnds into the argument of the Ifthenelse construct.
     * In principle, we can generate better code by knowing that these 
     * are only used to decide a continuation *)
    fun switch_opts (state,bnds,acc) =
      (case bnds 
	 of ((bnd1 as Exp32_b(x1,oper))::
	     (bnd2 as Exp32_b(x2,Switch (Ifthenelse{arg,thenArm,elseArm,rtype})))::rest) =>
	   let
	     fun default() = switch_opts2(state,bnds,acc)
	   in
	     (case get_varuse(state,x1)
	       of USED n => 
		 (if (n = 1) orelse ((n = 2) andalso (notvaluable32 oper)) then
		    (case sink state (x1,oper,arg)
		       of SOME arg => 
			 let
			   val bnd = Exp32_b(x2,Switch (Ifthenelse{arg=arg,thenArm = thenArm, elseArm = elseArm,rtype=rtype}))
			 in switch_opts_backtrack(state,bnd::rest,acc)
			 end
			| _ => default())
		  else default())
		| _ => default())
	   end
	   | _ => switch_opts2(state,bnds,acc))

    (* Progress was made and a bnd eliminated.  Backtrack to try
     * with the next pair of adjacent bnds.
     *)
    and switch_opts_backtrack(state,bnds,acc) =
      (case acc
	 of (bnd::acc) => switch_opts (state,bnd::bnds,acc)
	  | _ => switch_opts (state,bnds,acc))

    (* Continue with other opts, in this case none.    *)
    and switch_opts2(state,bnds,acc) = 
      (case bnds
	 of [] => rev acc
	  | bnd::bnds => switch_opts (state,bnds,bnd::acc))


    fun killbnds state bnds = 
      let
	val bnds = List.mapPartial (bnd_used state) bnds
	val bnds = switch_opts(state,bnds,[])
      in bnds
      end

    fun getVals state l = 
      let
	fun loop [] = []
	  | loop ((arg32 sv) ::rest) =
	  let
	    val sv = unaliassv32 (state,sv)
	  in
	    if (closed_sv32 state sv) then (arg32 sv) :: (loop rest) 
	    else raise None
	  end
	  | loop ((slice(sz, sv)) ::rest) =
	  let
	    val sv = unaliassv32 (state,sv)
	  in
	    if (closed_sv32 state sv) then (slice(sz, sv)) :: (loop rest) 
	    else raise None
	  end
	  | loop ((arg64 sv) ::rest) =
	  let
	    val sv = unaliassv64 (state,sv)
	  in
	    if (closed_sv64 sv) then (arg64 sv) :: (loop rest) 
	    else raise None
	  end
      in ((SOME (loop l)) handle None => NONE)
      end


    fun do_con (state : state) (con : con) : con = 
      let
	val con = do_csubst (state,con)
	val () = VarSet.app (fn v => use_var(state,v)) (free_cvars_con con)
      in con
      end

    fun do_vc ((v,c),state) = 
      let
	val c = do_con state c
	val state = add_var(state,v)
      in ((v,c),bind_var32(state,(v,c)))
      end

    fun do_vclist state vcs = LO.foldl_acc do_vc state vcs

    fun do_vc64 ((v,c),state) = 
      let
	val c = do_con state c
	val state = add_var(state,v)
      in ((v,c),bind_var64(state,(v,c)))
      end

    fun do_vc64list state vcs = LO.foldl_acc do_vc64 state vcs
    fun do_vklist state vks = (vks,bind_cvars(state,vks))

    fun do_lilprim32 (state : state) (prim, clist, sv32s, sv64s) =
      let 
	open Prim
	
	
	fun default() = LilPrimOp32(prim,
				    map (do_con state) clist,
				    map (do_sv32 state) sv32s,
				    map (do_sv64 state) sv64s)

	fun help32 sv = unaliasop32 (state,sv)	  
	fun help64 sv = unaliasop64 (state,sv)	  
      in
	(case (prim,map help32 sv32s,map help64 sv64s)
	   of (Box,_,[Unbox sv]) => (inc boxes_cancelled;Val (do_sv32 state sv))
	    | (Tuple,args as (LilPrimOp32(Select 0w0,_,[Var_32 v],_)::_),_) => 
	       let  (*Record eta reduction *)

		 fun check (iw,len,l) =
		   (case l
		      of [] => len = 0
		       | (LilPrimOp32(Select iw',_,[Var_32 v'],_)::rest) => 
			Name.eq_var(v,v') andalso iw = iw' andalso check(iw+0w1,len - 1,rest)
		       | _ => false)
		 val len = List.length (Dec.C.tuple_ml (Dec.C.ptr (typeof_sv32 (state,Var_32 v))))
		 val is_eta =  check (0w0,len,args)
	       in if is_eta then (inc record_eta;Val(do_sv32 state (Var_32 v))) else default()
	       end
	    | (Select iw,[LilPrimOp32(Tuple,_,args,_)],_) => (inc record_beta;Val(do_sv32 state (LU.wnth iw args)))
	    | _ => default ())
      end


    and do_prim32 (state : state) (prim, clist, primargs) =
      let 
	fun default() = Prim32(prim,
			       map (do_con state) clist,
			       map (do_primarg state) primargs)
	  
      in  
	(case getVals state primargs
	   of NONE => default ()
	    | SOME elist =>
	     ((case LilPrimUtil.apply () prim clist elist
		 of arg32 sv => Val (do_sv32 state sv)
		  | _ => error "bad prim result")
		 handle _ => default()))
      end

    and do_prim_embed (state : state) (sz,prim, primargs) =
      let 
	fun default() = PrimEmbed(sz,prim,
			       map (do_primarg state) primargs)
	  
      in  
	(case getVals state primargs
	   of NONE => default ()
	    | SOME elist =>
	     ((case LilPrimUtil.apply () prim [] elist
		 of slice (sz,sv) => Val (do_sv32 state sv)
		  | _ => error "bad prim result")
		 handle _ => default()))
      end

    and do_prim64 (state : state) (prim, primargs) =
      let 
	fun default() = Prim64(prim, map (do_primarg state) primargs)
	  
      in  
	(case getVals state primargs
	   of NONE => default ()
	    | SOME elist =>
	     ((case LilPrimUtil.apply () prim [] elist
		 of arg64 sv => Val_64 (do_sv64 state sv)
		  | _ => error "bad prim result")
		 handle _ => default()))
      end

    and do_coercion (state : state) (coercion1,arg1) =
      let
	val res =
	  (case reduce_coercion state (coercion1,arg1)
	     of SOME sv => do_sv32 state sv
	      | NONE => 	  
	       let
		 val coercion = do_sv32 state coercion1
		 val arg = do_sv32 state arg1
	       in Coerce (coercion,arg)
	       end)
      in  res
      end
    and do_primarg state parg = 
      (case parg
	 of arg32 sv => arg32 (do_sv32 state sv)
	  | arg64 sv => arg64 (do_sv64 state sv)
	  | slice(sz,sv) => slice (sz,do_sv32 state sv))

    and do_value state v = 
      (case v of
	 Prim.int _ => v
       | Prim.uint _ => v
       | Prim.float _ => v
       | Prim.vector (c,a) =>
	   let val _ = Array.modify (do_primarg state) a
	   in  Prim.vector(do_con state c, a)
	   end
       | Prim.intvector (sz,a) =>
	   let val _ = Array.modify (do_primarg state) a
	   in  Prim.intvector(sz, a)
	   end
       | Prim.floatvector (sz,a) =>
	   let val _ = Array.modify (do_primarg state) a
	   in  Prim.floatvector(sz, a)
	   end
       | Prim.tag (t,c) => Prim.tag(t,do_con state c)
       | _ => error "ref and array constants not supported"
	   )

    and do_sv32 (state : state) (sv : sv32) : sv32 =
      (case sv
	 of Var_32 v =>
	   (case lookup_alias(state,v) of
	      MUST32 sv => do_sv32 state sv
	    | _ => (use_var(state,v); sv))
	  | Const_32 v => Const_32 (do_value state v)
	  | Label l => sv  (* Could do dead code elimination with labels too *)
	  | Coercion (ctag,cons) => Coercion(ctag,map (do_con state) cons)
	  | Coerce args => do_coercion state args
	  | Tabs ((a,k),sv) => Tabs((a,k),do_sv32 (bind_cvar (state, (a,k))) sv)
	  | TApp (sv,c) =>
	    (case unaliassv32 (state,sv)
	       of Tabs ((a,_),sv) => do_sv32 (replace_con state (a,do_con state c)) sv
		| _ => TApp (do_sv32 state sv,do_con state c))
	  | Tag _ => sv
	  | Unit => sv)
    and do_sv64 (state : state) (sv : sv64) : sv64 = 
      (case sv
	 of Var_64 v =>
	   (case lookup_alias(state,v) of
	      MUST64 sv => do_sv64 state sv
	    | _ => (use_var(state,v); sv))
	    | Const_64 v => sv)

    (* Some operations may result in new bindings in the context *)
    and do_op32 (state : state) (oper : op32) : (bnd list * op32) = 
      (case oper
	 of Switch sw => do_switch state sw
	  | _ => ([],do_op32_small state oper))
    and do_op32_small (state : state) (oper : op32) : op32 = 
      (case oper
	 of Val sv => Val (do_sv32 state sv)
	  | Prim32 args => do_prim32 state args
	  | PrimEmbed args => do_prim_embed state args
	  | LilPrimOp32 args => do_lilprim32 state args
	  | Switch sw => error "Impossible"
	  | ExternApp(f,sv32s,sv64s) =>
	   ExternApp(do_sv32 state f, map (do_sv32 state) sv32s, map (do_sv64 state) sv64s)
	  | App(f,elist,eflist) =>
	   App(do_sv32 state f,
	       map (do_sv32 state) elist,
	       map (do_sv64 state) eflist)
	  | Call(f,elist,eflist) =>
	   Call(do_sv32 state f,
		map (do_sv32 state) elist,
		map (do_sv64 state) eflist)
	  | Raise (c,sv) => Raise(do_con state c,do_sv32 state sv)
	  | Handle (result_type,body,(bound,handler)) => 
	   let 
	     val body = do_exp' state body
	     val result_type = do_con state result_type
	     val (_,state) = do_vc ((bound,LD.T.exn()),state)
	     val handler = do_exp' state handler
	   in  
	     Handle (result_type,body,(bound,handler))
	   end)

    and do_op64 (state : state) (oper : op64) : op64 = 
      (case oper
	 of Val_64 sv => Val_64(do_sv64 state sv)
	  | Unbox sv => 
	   (case unaliasop32 (state,sv)
	      of LilPrimOp32(Box,[],[],[sv64]) => Val_64(do_sv64 state sv64)
	       | _ => Unbox (do_sv32 state sv))
	  | Prim64 args => do_prim64 state args
	  | ExternAppf (f,sv32s,sv64s) =>
	      ExternAppf(do_sv32 state f, map (do_sv32 state) sv32s, map (do_sv64 state) sv64s))

    and do_sum_switch state sw = 
      let

	fun reduce_known_switch {arg,arms,default,rtype} =
	  let
	    val tag_value = remember_forgotten state arg
	  in
	    case tag_value 
	      of SOME (w,inj) => 
		let
		  val _ = inc switches_reduced
		in
		  (* Reduce known switch *)
		  (case List.find (fn (w',_,_) => w = w') arms 
		     of SOME (_,bound,arm_body) =>
		       SOME (LD.E.mk_let [Exp32_b (bound,Val inj)] arm_body)
		      | NONE =>
		       SOME (Option.valOf default))
		(* A switch which does not either cover all of
		 * the possibilities, or have a default, is 
		 * ill-formed.  We are in the case where a possibility
		 * was not covered, therefore default must be SOME.
		 *)
		end
	       | _ => NONE
	  end

	fun sum_switch {arg,arms,default,rtype} =
	  let 
	    val arg = do_sv32 state arg
	    val sum = typeof_sv32 (state,arg)
	    fun do_arm(w,x,body) =
	      let 
		val ksum = LD.COps.sum2ksum' w sum
		val state = bind_var32 (state,(x,ksum))
		val state = 
		  (case arg
		     of Var_32 y => 
		       let
			 val argeta = LD.E.forget' ksum (Var_32 x)
			 val state = add_avail32(state,Val(argeta) ,y)
		       in state
		       end
		      | _ => state)
	      in  (w,x,do_exp' state body)
	      end
	    val rtype = do_con state rtype
	    val arms = map do_arm arms
	    val default = Util.mapopt (do_exp' state) default
	  in  
	    ([],Switch(Sumcase {arg=arg,
				arms=arms,
				default=default,
				rtype = rtype}))
	  end

      in
	case (reduce_known_switch sw)
	  of SOME exp => do_exp_op state exp
	   | NONE => sum_switch sw
      end
    and do_ifthenelse (state : state) {arg,thenArm,elseArm,rtype} =
      let
	val () = debugdo (5, fn () => print "Working on ifthenelse\n")

	val arg = do_cc state arg

	val mkstate = 
	  (case get_exp_cc_val arg
	     of SOME (Var_32 x) => (fn b => add_alias(state,x,MUST32 (LD.E.bool' b)))
	      | _ => fn b => state)

	fun default () = 
	  let
	    val thenArm = do_exp' (mkstate true) thenArm
	    val elseArm = do_exp' (mkstate false) elseArm
	    val rtype = do_con state rtype
	  in ([],Switch(Ifthenelse {arg = arg,thenArm = thenArm,elseArm = elseArm, rtype = rtype}))
	  end

      in 
	case arg
	  of Exp_cc e => 
	    (case lilboolexp2bool state e
	       of SOME true => do_exp_op state thenArm
		| SOME false => do_exp_op state elseArm
		| NONE => default())
	   | _ => default()
      end
	    
    and do_switch (state : state) (switch : switch) : (bnd list * op32) = 
      let 
	
	fun int_switch {arg,arms,size,default,rtype} =
	  let
	  in 
	    case (do_sv32 state arg) 
	      of Const_32 (Prim.int(_,n)) =>
		let 
		  val _ = inc switches_reduced
		  val n32 = TilWord64.toSignedHalf n
		  val arm = 
		    case Listops.assoc (n32, arms) of
		      SOME arm => arm
		    | NONE => default
		in
		  do_exp_op state arm
		end
	       | arg =>
		let
		  val rtype = do_con state rtype
		  val arms = LO.map_second (do_exp' state) arms
		  val default = do_exp' state default
		in  ([],Switch(Intcase {arg=arg, 
					arms=arms,
					size = size,
					default=default,
					rtype=rtype}))
		end
	  end
	fun intsw_to_if {arg,arms,default,size,rtype} =
	  (case arms
	     of [(w,thenArm)] => 
	       let
		 val e = P.Lili.to_exp (LD.E.inteq size (arg,LD.E.intconst' size w))
	       in SOME {arg = Exp_cc e, thenArm = thenArm, elseArm = default, rtype = rtype}
	       end
	      | _ => NONE)
	fun sumsw_to_if {arg,arms,default,rtype} =
	  (case Dec.C.sum_ml' (typeof_sv32 (state,arg))
	     of SOME (0w2,[]) => 
	       (case (arms,default)
		  of ([(0w0,_,elseArm),(0w1,_,thenArm)],NONE) => 
		    SOME {arg=Exp_cc(mk_exp (Val32_e arg)),
			  thenArm=thenArm,
			  elseArm=elseArm,
			  rtype = rtype}
		   | ([(0w1,_,thenArm),(0w0,_,elseArm)],NONE) => 
		    SOME {arg=Exp_cc(mk_exp (Val32_e arg)),
			  thenArm=thenArm,
			  elseArm=elseArm,
			  rtype = rtype}
		   |  ([(0w0,_,elseArm)],SOME thenArm) => 
		    SOME {arg=Exp_cc(mk_exp (Val32_e arg)),
			  thenArm=thenArm,
			  elseArm=elseArm,
			  rtype = rtype}
		   | ([(0w1,_,thenArm)],SOME elseArm) => 
		    SOME {arg=Exp_cc(mk_exp (Val32_e arg)),
			  thenArm=thenArm,
			  elseArm=elseArm,
			  rtype = rtype}
		   | _ => NONE)
	      | _ => NONE)
      in
	case switch 
	  of Intcase int_sw => 
	    (case intsw_to_if int_sw
	       of SOME ifte => do_ifthenelse state ifte
		| NONE => int_switch int_sw)
	   | Sumcase sum_sw => 
	    (case sumsw_to_if sum_sw
	       of SOME ifte => do_ifthenelse state ifte
		| NONE => do_sum_switch state sum_sw)
	   | Dyncase {arg,arms,default,rtype} =>
	    let 
	      val arg = do_sv32 state arg
	      val rtype = do_con state rtype
	      fun do_arm(tag,(bound,con),body) =
		let 
		  val tag = do_sv32 state tag
		  val con = do_con state con
		  val state = bind_var32 (state,(bound,con))
		  val body = do_exp' state body
		in  (tag,(bound,con),body)
		end
	      val arms = map do_arm arms
	      val default = do_exp' state default
	    in  ([],Switch(Dyncase {arg=arg,
				    arms=arms,
				    default=default,
				    rtype = rtype}))
	    end
	   | Ifthenelse ifte => do_ifthenelse state ifte
      end
    and do_cc (state : state) (cc : conditionCode) = 
      let
	(* Two passes. First we try to reduce it, then we rewrite it 
	 * (expanding out aliases, etc). Reduction can use the context 
	 * to try to reduce coercions, but shouldn't do any further
	 * rewriting of expressions.
	 *
	 * Doing it in the other order might be better, but then we have a 
	 * problem with inlined expressions not being rewritten in their
	 * new context.
	 *)
	val cc = reduce_cc state cc

	val cc = 
	  case cc
	    of Exp_cc e => Exp_cc (do_exp' state e)
	     | Not_cc cc => Not_cc (do_cc state cc)
	     | And_cc(cc1,cc2) => And_cc (do_cc state cc1,do_cc state cc2)
	     | Or_cc(cc1,cc2) => Or_cc (do_cc state cc1,do_cc state cc2)
      in cc
      end

    and do_function (state : state) (v,Function{tFormals, 
						eFormals, 
						fFormals,
						body,
						rtype}) =
      let 
	val state = enter_var(state,v)
	val (tFormals,state) = do_vklist state tFormals
	val (eFormals,state) = do_vclist state eFormals
	val (fFormal,state) = do_vc64list state fFormals
	val body = do_exp' state body
      in  (v,
	   Function{tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
		    body=body,rtype=rtype})
      end

    and do_bnds(bnds : bnd list, state : state) : (bnd list * state) = 
      let
	fun loop(bnds,state,racc) = 
	  (case bnds
	     of [] => (rev racc,state)
	      | (bnd::bnds) => 
	       let
		 val (bnds',state) = do_bnd (bnd,state)
	       in loop (bnds,state,List.revAppend(bnds',racc))
	       end)
      in loop(bnds,state,[])
      end

    and do_bnd (bnd : bnd, state : state) : (bnd list * state) =
      let

	val () = debugdo (6, fn () => (print "Working on bnd:\n";
				       PpLil.pp_bnd bnd;
				       print "\n"))
	val res = 
	  case bnd 
	    of Exp32_b(v,oper) =>
	      let
		val orig_state = state
		val state = add_var(state,v)
		val state' = enter_var(state,v)
		val (bnds,oper) = do_op32 state' oper

	      (* We may get more bnds out from switch reduction.  If so,
	       * start over so that the flattened bindings are available
	       * in the current environment.
	       *)
	      in 
		if not (null bnds) then do_bnds (bnds @ [Exp32_b (v,oper)],orig_state)
		else
		  let
		    val oper =
		      (case find_avail32(state,oper) of
			 NONE => oper
		       | SOME v' => 
			   let 
			     val () = use_var(state',v')
			   in  Val (Var_32 v')
			   end)
		    val eff = notvaluable32 oper
		    val _ = if eff then use_var(state,v) else ()
		    val state=
		      (case oper
			 of Val sv => 
			   (case sv
			      of Var_32 v' => 
				let
				  val n = Name.var2name v
				  val n' = Name.var2name v'
				  val _ = if (String.size(n') = 0) then
				    Name.rename_var (v', n)
					 else ()
				  val state = add_alias(state,v,MUST32 sv)
				in
				  state
				end
			       | _ => add_alias(state,v,MUST32 sv))
			  | _ => 
			      let
				val state = add_avail32(state,oper,v)
				val state = add_alias(state,v,OPTIONAL32 oper)
			      in state
			      end)
		    val state = op32_define (state,(v,oper))
		  in  ([Exp32_b(v,oper)],state)
		  end
	      end
	     | Exp64_b(v,oper) =>
	      let
		val state = add_var(state,v)
		val state' = enter_var(state,v)
		val oper = do_op64 state' oper
		val oper =
		  (case find_avail64(state,oper) of
		     NONE => oper
		   | SOME v' => 
		       let 
			 val () = use_var(state',v')
		       in  Val_64 (Var_64 v')
		       end)
		val eff = notvaluable64 oper
		val _ = if eff then use_var(state,v) else ()
		val state=
		  (case oper
		     of Val_64 sv => 
		       (case sv
			  of Var_64 v' => 
			    let
			      val n = Name.var2name v
			      val n' = Name.var2name v'
			      val _ = if (String.size(n') = 0) then
				Name.rename_var (v', n)
				      else ()
			      val state = add_alias(state,v,MUST64 sv)
			    in
			      state
			    end
			   | _ => 
			    let
			      val state = add_alias(state,v,MUST64 sv)
			    in state
			    end)
		      | _ => 
			  let
			    val state = add_avail64(state,oper,v)
			    val state = add_alias(state,v,OPTIONAL64 oper)
			  in state
			  end)
		val state = op64_define (state,(v,oper))
	      in  ([Exp64_b(v,oper)],state)
	      end
	     | Fixcode_b vflist => 
	      let 
		fun folder ((v,f),state) = add_var(bind_var32(state,(v,Typeof.function f)),v)
		val state = foldl folder state vflist
		val vflist = map (do_function state) vflist
	      in
		(case vflist
		   of ([(v,Function{tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
				    body,
				    rtype})]) =>
		     (case #e body
			of Let_e([Exp32_b(x,App(sv,elist,eflist))],ret) => 
			  (case (Dec.E.nary_tapp sv,#e ret)
			     of ((Var_32 v',clist),Val32_e(Var_32 x')) =>
			       let
				 fun ccheck((a,_),c) = EQ.C.equal (mk_con(Var_c a)) c
				 fun echeck((x,_),Var_32 x') = Name.eq_var(x,x') 
				   | echeck _ = false
				 fun efcheck((x,_),Var_64 x') = Name.eq_var(x,x')
				   | efcheck _ = false
			       in
				 if not (Name.eq_var(v,v')) andalso (Name.eq_var(x,x')) andalso
				   (Listops.all2 ccheck (tFormals,clist)) andalso
				   (Listops.all2 echeck (eFormals,elist)) andalso
				   (Listops.all2 efcheck (fFormals,eflist)) 
				   then (inc fun_eta;
					 (([],add_alias(state,v,MUST32(Var_32 v')))))
				 else ([Fixcode_b vflist],state)
			       end
			      | _ => ([Fixcode_b vflist],state))
			 | _ => ([Fixcode_b vflist],state))
		  | _ => ([Fixcode_b vflist],state))
	      end
	     | Unpack_b (a,x,sv) =>
	      let
		val state = add_vars(state,[a,x])

		(* doesn't matter which variable we give to enter. -leaf *)
		val state' = enter_var(state,x)
		val sv = do_sv32 state' sv
	      in 
		case Dec.E.pack'(unaliassv32 (state,sv))
		  of SOME (tas,thiding,sv) => 
		    let
		      val state = add_alias(state,x,MUST32 sv)
		      val state = replace_con state (a,thiding)
		    in ([],state)
		    end
		   | NONE => 
		    let
		      val state = unpack_define (state,(a,x,sv)) 
		      val state = 
			(case sv
			   of Var_32 v => 
			     let
			       val tas = typeof_sv32 (state,sv)
			       val thiding  = mk_con (Var_c a)
			     in
			       add_alias(state,v,OPTIONAL32(Val(LD.E.pack' tas thiding (Var_32 x))))
			     end
			    | _ => state)
		    in ([Unpack_b (a,x,sv)],state)
		    end
	      end
	     | Split_b (a1,a2,c) =>
	      let
		val c = do_con state c 
		val state = split_define (state,(a1,a2,c))
	      in case cout (R.whnf c)
		   of Pair_c _ => ([],state )
		    | _ => ([Split_b (a1,a2,c)],state)
	      end
	     | Unfold_b (a,c) => 
	      let
		val c = do_con state c 
		val state = unfold_define (state,(a,c))
	      in case cout (R.whnf c)
		   of Fold_c _ => ([],state)
		    | _ => ([Unfold_b(a,c)], state)
	      end
	     | Inj_b (w,a,c,sv) =>
	      let
		val c = do_con state c 
		val sv = do_sv32 state sv 
		val state = inj_define (state,(w,a,c,sv))
	      in case cout (R.whnf c)
		   of Inj_c (w',_,_) => 
		     if w = w' then ([],state)
		     else ([Inj_b(w,a,c,sv)],state)
		    | _ =>  ([Inj_b(w,a,c,sv)],state)
	      end

	val () = debugdo (6, fn () => (print "Finished bnd:\n"))

      in res
      end
    and do_exp' state exp = 
      let
	val (bnds,sv) = do_exp state exp
	val body = mk_exp (Val32_e sv)
      in 
	case bnds 
	  of [] => body
	   | _ => mk_exp(Let_e (bnds,body))
      end
    and do_exp_op (state : state) (exp : exp) : (bnd list * Lil.op32)  = 
      let
	val (bnds,sv) = do_exp state exp
      in (bnds, Val sv)
      end
    and do_exp (state : state) (exp : exp) : (bnd list * Lil.sv32)  = 
      let
	(* we must put a wrapper in order to perform the filter *)
	val state = retain_state state

	val ((bnds,sv),state) = do_exp_flatten state exp

	(* bnd_used will discard any bounds that were definitively unused.
	 * Variables that appear free in any of the bounds will be deferred
	 * to that variable being bound.  Variables that appear free in
	 * in e will be deferred to current.  Therefore,
	 * we retain_state to say that current is definitely used.  Otherwise,
	 * all of the bnds will be marked as unused and discarded, since
	 * we don't yet know the status if the binding that we are in.
	 *)

	val bnds = killbnds state bnds
      in (bnds,sv)
      end
    (* Return a rewritten exp with the innermost state to use in the 
     * filter above.
     *)
    and do_exp_flatten state exp = 
      let
	fun flatten_loop (state,e,acc) = 
	  (case #e e
	     of Let_e (bnds,e) => 
	       let
		 val (bnds,state) = do_bnds(bnds,state)
	       in flatten_loop (state,e,List.revAppend(bnds,acc))
	       end
	      | Val32_e sv => 
	       let
		 val sv = do_sv32 state sv
		 val bnds = rev acc
	       in ((bnds,sv),state)
	       end)
      in flatten_loop(state,exp,[])
      end


    fun do_datum state d = 
      (case d
	 of Dboxed (l,sv64) => Dboxed (l,do_sv64 state sv64)
	  | Dtuple (l,t,qs,svs) => Dtuple (l,do_con state t,
					   map (fn (ctag,cons) => (ctag,map (do_con state) cons)) qs,
					   map (do_sv32 state) svs)
	  | Darray (l,sz,t,vs) => Darray (l,sz,do_con state t,map (do_value state) vs)
	  | Dcode (l,f) => 
	   let
	     val fvar = Name.fresh_named_var (Name.label2name l)
	     val state = add_var (state,fvar)
	     val (_,f) = do_function state (fvar,f)
	   in Dcode(l,f)
	   end)

    fun do_data state data = 
      let
	fun add_dtype (d,state) = 
	  (case d
	     of Dboxed (l,sv64) => bind_label(state,(l,LD.T.ptr (LD.T.boxed_float())))
	      | Dtuple (l,t,qs,svs) => bind_label(state,(l,t))
	      | Darray (l,sz,c,svs) => bind_label (state,(l,LD.T.ptr (LD.T.array sz c)))
	      | Dcode (l,f) => bind_label(state,(l,Typeof.code f)))
	val state = foldl add_dtype state data
	val data = map (do_datum state) data
      in (data,state)
      end

    fun optimize params (MODULE{unitname,parms,entry_c,entry_r,timports, data, confun}) =
      let
	val _ = reset_stats()
	  
	val state = newState (params,VarSet.empty)
	val state = foldl (fn ((l,a,k),state) => bind_cvar(state,(a,k))) state timports
	val (data,state) = do_data state data
	val confun = do_con state confun

	val _ = chat_stats ()
	  
      in  MODULE{unitname = unitname,parms = parms,entry_c = entry_c, entry_r = entry_r,timports=timports,data=data,confun=confun}
      end
  end
end (* Optimize *)