(*$import Util Listops Name Int Normalize List Prim Sequence Nil NilUtil Ppnil LibBase SPECIALIZE NilContext NilDefs NilStatic ListPair Stats *)

(* A two-pass optimizer to remove unnecesssarily polymorphic code:
     Essentially, convert
       fun f _ = 5
       fun g() = f () + f ()
    into
       fun f () = 5
       fun g () = f () + f ()

     or convert
       fun f x = (x,x)            [really fun 'a f (x:'a) = (x,x)]
       fun g () = (f 1, f 2)      [really fun g () = (f[int](1), f[int](2)) ]
     into
       fun f (x:int) = (x,x)
       fun g () = (f 1, f 2)


    This is possible when f does not escape.  
    This is desirable since calling a polymorphic function
       requires two calls and can lead to unnecessary constructor code.

    INVARIANT:
      Because this is a 2-pass algorithm, where first information on
      all the variables is gathered and then the program is rewritten,
      all bound variables in the input code must be distinct.

    Someday we may want to extend this to allow multiple specializations
    for polymorphic functions only instantiated at a few different types.


*)	

structure Specialize :> SPECIALIZE =
struct

	open Util Nil NilContext NilUtil Listops Ppnil
 	val error = fn s => Util.error "specialize.sml" s

	val chatref = Stats.ff "SpecializeChat"

	fun chat s = if !chatref then print s else ()

	val debug = ref false
	val diag = ref false
	val do_dead = ref true
	val do_proj = ref true

	val type_of = Normalize.type_of
	val strip_arrow_norm = Normalize.strip_arrow_norm
	val rename_arrow     = NilUtil.rename_arrow

	(* In the first pass, we locate all candidate functions.
	   Candidate functions are term-level functions which
	   (0) have valuable bodies
	   (1) (1) Can take constructor arguments.
	       (2) Cannot take floating point arguments.
	       (3) Can only take term arguments of unit type.
	   (2) Cannot escape.  That is, their uses can appear only
	       in application position.
	   (3) are given the same arguments at each application site.
	       (1) The term arguments must be valuable.
	           They are already equal since they are of type unit.
	       (2) The type arguments must all be equivalent to a type
	           which is well-formed at the context where the 
		   candidate function was defined. 

          [Note that properties 0 and 1 are satisfied by the
           translation of polymorphic functions, except that functions
           using equality polymorphism don't satisfy 1.3, and hence
           such functions won't be specialized by the current code.]

	 There is a global state mapping each candidate function
	     to its relevant information.  As the program is 
	     traversed in the first pass, candidate functions 
	     are added and removed.  Any remaining functions 
	     after pass 1 are candidates.

	 An important refinement of this algorithm is that we do the scan
	 phase in a bottom up fashion.  This is important, because it allows 
	 us to specialize chains of instantiations in a single pass.  For code 
         of the form
              fun f x = x
              fun g x = f x
              fun h x = g x
              val x = g 3
         the top-down traversal does not allow us to know the status of g and h 
	 when we traverse their bodies.  Therefore, each run of specialize will 
	 only catch one instantiation.  A bottom-up traversal allows us to 
	 specialize all three functions in a single pass, since we don't traverse 
	 the bodies of the functions until we have already decided their status.

	 A second refinement of this algorithm is to extend the analysis to recursive
	 functions in the following manner:  if we have tentatively decided to 
	 specialize a function f : A[a::k].unit->t to some type c, then we 
	 scan f using the definition a=c instead of a::k.  This allows us to catch
	 trivial uses of polymorphic recursion where the recursion is at the same type
	 (which is always the case for SML).

	  In the second pass, we rewrite candidate functions and
	     their applications.  For non-recursive functions,
             we rewrite the code as follows:
	  (1) We rewrite the function by removing its formals
	      and wrapping the body with a let of the formals
	      bound to the type arguments as determined by step
	      3.2 of pass 1 and the term arguments to unit.
	  (2) Replace each application of the candidate function
              with the variable which is now bound to the binding.

	  For (mutually) recursive functions, it can be hard in general to eliminate
              the arguments to the functions.   Consider for example the following code
	      (which is what we generate if we use polymorphic recursion):
              fun f = /\a.\x:unit.E[g[a]x]
              and g = /\a.\x:unit.E[g[a]x]
              val x = g[int]()
	      If this code happens to be in a fairly idiomatic form, (which it often will
	      be) we can specialize this to
	      a = int
              x = ()
              fun f = \x':unit.E[g x]
              and g = \x':unit.E[g x]
              val x = g ()
	      This is done as follows:
           (1) For specializable recursive functions whose bodies are of the form
	       let bnds, fun f_inner (...).... in f_inner, rewrite the function by
               hoisting the formals bound to the type arguments (as above) as well as
	       the local bnds in to the enclosing scope, and then rewrite the fix
	       to use the f_inner lambda instead of the original lambda.  	       
           (2) Rewrite each application of a candidate function to be a use of the
	       inner function.
            
	  In general, code may not always be of this idiomatic form.  We can still 
	      do a more restricted form of specialization, wherein we leave the functions 
              in place, but replace all uses of their arguments with a specialized version.  
	      This allows us to specialize code like the above to
	      a = int
              x = ()
              fun f = /\a'.\x':unit.g[a]x
              and g = /\a'.\x':unit.g[a]x
              val x = g[int]()
	      Notice that the bodies of the new functions now refer to the actual
	      arguments that will be passed.  This allows some new reductions to
	      occur internally.  In the best of all worlds, dead-argument elimination
	      would now get rid of the additional lambda abstractions, but this is not
	      done currently.


	 In some sense this process is sort of a reverse inlining,
         where the type arguments in an application are moved up into
         the definition of the function.  This means, however, that
         we must check that the type arguments specified make sense in
	 the appropriate context.

       *)

       (****************)
       (* Global State *)
       (****************)

	local
	    (*
	     Impure:  Marked as potentially recursive, has side-effects,
                      or has floating-point arguments.

	     Polymorphic:  Either has non-valuable or non-unit term
	                   arguments, or is used at more than one type.

             Escaping: Appears elsewhere than in the function-part of 
                       an application expression (including the exports).

	     Candidate:  So far, satisfies the conditions listed above.
                context  = the context where the function was defined
                tFormals = the function's formal type parameters
		eFormals = the function's formal term parameters
                              [since it's a candidate, no float parameters]
                tKinds   = the kinds of the function's type parameters,
                           used when checking whether two instantiations
                           are using the same types.
                body     = the code for the function body
                replace  = the name that the new statically-specialized
                           function will use.
	        isrecur  = true if the body of the function contains any self-applications.
            *)

	    datatype status = 
	      Candidate of {context  : context,
			    tipe     : con, 
			    tFormals : var list, 
			    eFormals : var list,
			    tKinds   : kind list,
			    tActuals : con list option ref,
			    body     : exp,
			    replace  : var,
			    isrecur  : bool ref}
	      | Impure
	      | Escaping
	      | Polymorphic

	    datatype rewrite = 
	      Dead 
	    | Elim of bnd list * var
	    | Eta of (bnd list * ((var * con) * function) * var)
	    | Spec of (bnd list * ((var * con) * function))

	    (* In order to correctly detect (mutually) recursive functions, we 
	     * allocate a ref cell for each fix cluster, which is shared among the
	     * entries for all of the functions in the cluster.  Any time we encounter
	     * an application of a function f, we set the called ref for f to true.
	     * Before traversing each function body, we set the shared ref to false.  
	     * After traversing the function body, this ref will be true iff the body
	     * of the function calls one of the mutually recursive functions from the
	     * fix.  We then set the isrecur flag appropriately.
	     *
	     * After the first pass, the called flag is re-used to indicate whether or
	     * not the function nest is live, or whether it is can eliminated as dead-code.
	     *)
	    type entry = {called : bool ref,
			  status : status}

	    val mapping = ref (Name.VarMap.empty : entry Name.VarMap.map)
	    val rewrites = ref (Name.VarMap.empty : rewrite Name.VarMap.map)

	    fun getStatus v = case Name.VarMap.find (!mapping,v)
				of SOME{called,status} => SOME status
				 | NONE => NONE

	    fun getCalled v = case Name.VarMap.find (!mapping,v)
				of SOME{called,status} => SOME called
				 | NONE => NONE
	in
	  (* Reset the global state 
	   *)
	  fun reset() = (mapping := Name.VarMap.empty;rewrites := Name.VarMap.empty)

	  (* Summarize the global state by printing statistics about it.
	   *)
	  fun showCandidates() = 
	    let fun folder(v,{status,called},(c,i,e,p)) = 
	      (case status of
		 Candidate {tActuals,...} => (c+1,i,e,p)
	       | Impure => (c,i+1,e,p)
	       | Escaping => (c,i,e+1,p)
	       | Polymorphic => (c,i,e,p+1))
		val (c,i,e,p) = 
		  Name.VarMap.foldli folder (0,0,0,0) (!mapping)
		  
	    in  
	      print("  " ^ (Int.toString c)^ " optimizable candidates.\n");
	      print ("  " ^ (Int.toString i) ^ " impure.\n");
	      print ("  " ^ (Int.toString e) ^ " escaping.\n");
	      print ("  " ^ (Int.toString p) ^ " used polymorphically.\n")
	    end

	  (* Replace an existing entry for the variable v, but do
	   nothing if the variable has not already appeared in the
	   mapping.
	   *)
	  fun editEntry (v,status) = 
	    (case Name.VarMap.find(!mapping,v) of
	       NONE => ()
	     | SOME {called,...} => 
		 (* XXX:  Is this remove redundant? *)
		 mapping := 
		 Name.VarMap.insert(#1(Name.VarMap.remove(!mapping,v)),
				    v, {status=status,called=called}))
		
	  (* Insert an entry into the mapping for the variable v
	   *)					     
	  fun addEntry (v,entry) = 
	    mapping := Name.VarMap.insert(!mapping,v,entry)
	    
	  (* Mark a (previously-seen) variable as Escaping
	   *)
	  fun escapeCandidate v = editEntry (v,Escaping)

	  (* Mark a (previously-seen) variable as Polymorphic
	   *)
	  fun polyCandidate v = editEntry (v,Polymorphic)
	    
	  (* Given a function definition, add it to the global state
	   *)
	  fun addCandidate (context, v : var, c : con, shared_ref : bool ref,
			    Function {recursive, effect, tFormals, eFormals, 
				      fFormals, body, ...}) = 
	    let 
	      val _ = chat ( "Adding candidate: "^(Name.var2string v)^"\n")
		
	      val recur = (case recursive of
			     NonRecursive => false  (*Leaf functions can be recursive within an inner lambda *)
			   | _ => true)
		
	      val {tFormals=tFormals',...} = rename_arrow (strip_arrow_norm context c,tFormals)
	      val tKinds = map #2 tFormals'
		
	      val pure = (case effect of Total => true | Partial => not (NilDefs.anyEffect body))
	      val entry = 
		{called = shared_ref,
		 status = 
		 (case fFormals of
		    [] => 
		      
		      (* No floating-point arguments? (Property 1.2) 
		       *)
		    if pure then
		      (* Nonrecursive and no effects? (Property 1) 
		       *)
		      (chat "Ok candidate\n";
		       Candidate{body = body,
				 context = context,
				 tipe    = c,
				 tFormals = tFormals,
				 tKinds   = tKinds,
				 eFormals = map #1 eFormals,
				 tActuals = ref NONE,
				 replace = Name.derived_var v,
				 isrecur = ref recur})
		    else (chat "Has effects\n";Impure)
		  | _ => (chat "Has fformals\n"; Impure))}
	    in  addEntry (v,entry)
	    end
	  
	  
	  (* On input, the constructor c is well-formed with respect
	   * to currentCtxt, and sub-context targetCtxt is a sub-context
	   * of currentCtxt (so that c might not be well-formed with
	   * respect to targetCtxt).
	   *
	   * reductTo tries to return a constructor that is provably
	   * equivalent to c (under currentCtxt), but which is
	   * well-formed with respect to targetCtxt.
	   *)
	  fun reduceTo (targetCtxt, currentCtxt) (c : con) : con option = 
	    let 
	      (* Is the constructor con well-formed with respect to
	       * targetCtxt?  Since we know that it was well-formed
	       * in currentCtxt, and there's no variable shadowing,
	       * it suffices to check that all the free variables in
	       * c appear in targetCtxt.
	       *)
	      fun wellFormed (con : con) = 
		let 
		  val fvs = freeConVarInCon (true,0,con)
		in  
		  Name.VarSet.foldl 
		  (fn (v,ok) => ok andalso bound_con(targetCtxt,v)) 
		  true fvs
		end
	      
	      (* We could just normalize c, since the result would
	       * contain the type variables bound earliest possible in 
	       * currentCtxt, and hence would most likely be well-formed
	       * in targetCtxt.  But this is expensive and could
	       * cause the type to blow up in size, so we try to
	       * avoid this if possible.
	       *)
  
	      val copt = 
		if (wellFormed c) then
		  (* If c by itself is well-formed in targetCtxt then
		   we're done.
		   *)
		  SOME c
		else
		  let 
		    val (_,c) = Normalize.reduce_hnf(currentCtxt, c)
		  in  
		    (* If c is well-formed in targetCtxt after being
		     head-normalized then we're done.
		     *)
		    if (wellFormed c) then
		      SOME c
		    else 
		      let 
			val c = Normalize.con_normalize currentCtxt c
		      in
			(* Finally, try fully-normalizing c *)
			if (wellFormed c) then
			  SOME c
			else 
			  NONE
		      end
		  end
	    in   
	      copt
	    end
	  
	  fun checkCandidate (v, callContext, tArgs, eArgs) = 
	    let 
	      val _ = chat( "Checking candidate: "^(Name.var2string v)^"\n")
		
	      (* Is the expression e valuable and of type unit? 
	       *)
	      fun isUnit (e : exp) = 
		(not (NilDefs.anyEffect e)) andalso 
		(case (Normalize.reduce_hnf(callContext,
					    Normalize.type_of (callContext,
							       e))) of
		   (_,Prim_c(Record_c [], _)) => true
		 | _ => false)
		   
	      (* Given the typing context at the point where the
		     function was defined and the ref containing the
                     type applications we've seen so far, see whether
		     the new type arguments tArgs are can be hoisted
                     up to the definition and are consistent with
                     any other type arguments we've seen.
                   *)
		  fun matches defContext 
		              (tActuals : con list option ref) 
			      (tKinds : kind list)
                          : bool =
		      (case !tActuals of
			 NONE => 
			   (* This is the first instantitation we've come
			      across.  See if we can hoist all the type
			      arguments, and if so, record them.
			    *)
			   let 
			     val reduced = 
			       List.mapPartial 
			          (reduceTo (defContext, callContext)) tArgs
			   in   
			     if (length reduced = length tArgs) then
			       (* All the type arguments could be hoisted *)
			       (tActuals := SOME reduced; chat "Actuals could be hoisted\n";true) 
			     else 
			       (chat "Actuals could not be hoisted\n";false)
			   end
		       | SOME tAct =>
			   let 
			     (* If we've seen an application before, we don't
			        have to worry about figuring out from scratch
				whether tArgs can be hoisted.  If they're
				equivalent to the previously-hoisted arguments
				(in the context where the current instantiation
				is occurring, since that's where tArgs and
				the previously-hoisted types both make sense)
				then we know that tArgs can be hoisted; if
				they're not equivalent, we don't care whether
				tArgs can be hoisted or not.
			      *)

			     (* Compare the type at which the function
                                was previously instantiated with the
                                current instantiated type.
                              *)
			     fun equal(c1 : con, c2 : con, k : kind) = 
			       NilStatic.con_equiv(callContext,c1,c2,k)

			     val isEqual = 
			       Listops.andfold equal 
			                       (Listops.zip3 tAct tArgs tKinds)
			     val _ = if isEqual then chat "Actuals are equal\n" 
				     else chat "Actuals differ\n"
			   in  
			     isEqual
			   end)
		in
		  case Name.VarMap.find(!mapping, v) 
		    of SOME {called,status} => 
		      (called := true;  
		       case status
			 of Candidate{context, tFormals, eFormals, 
				      tActuals, body, tKinds,...} =>
			   if ((Listops.andfold isUnit eArgs) andalso
			       matches context tActuals tKinds) 
			     then ()
			   else polyCandidate v
                          | _ => ())
		     | NONE => ()
		end


	    fun getCandidateBnds (v : var) : (bnd list) option = 
	      (case getStatus v of
		 SOME (Candidate{tActuals = ref(SOME tActs),
				 tFormals,eFormals,...}) =>
		 let 
		   (* The type arguments were only
		    instantiated in one way.  Create
		    bindings that set the type arguments to
		    these types.  
		    *)
		   val bnds1 = 
		     Listops.map2 
		     (fn (v,c) => (Con_b(Runtime,Con_cb(v,c)))) 
		     (tFormals, tActs)
		     
		   (* Create bindings that set the term arguments
		    to unit.
		    *)
		   val bnds2 = 
		     map (fn v => Exp_b(v,TraceUnknown,
						NilDefs.unit_exp)) 
		     eFormals
		 in  
		   SOME(bnds1 @ bnds2)
		 end
	       | _ => NONE)

	    (* *)
	    fun unconstrainedFunction (v : var) : bool = 
		(case getStatus v of
		     SOME (Candidate{tActuals = ref NONE,...}) => true
		   | _ => false)

	    fun escapingFunction (v : var) = 
		(case getStatus v of
		   SOME Escaping => true
		 | _ => false)

	    (* Before we traverse a function, we set the shared ref
	     * for its fix cluster to false.  
	     *)
	    fun enterFunction (v : var) : unit = 
	      (case getCalled v of
		 SOME called => called := false
	       | _ => error "No info for function")

	    (* After traversing a function, we look at the shared ref
	     * for its fix cluster. If it has been set, then one of the
	     * functions in the cluster was called in the body of the
	     * current function, and so we mark the function as recursive.
	     * Otherwise, the funciton is not recursive.
	     *)
	    fun leaveFunction (v : var) : unit = 
	      (case Name.VarMap.find(!mapping,v) of
		 SOME {called,status=(Candidate{isrecur,...})} =>  isrecur := !called
	       | _ => ())

	    (****************These functions handle the rewrites*********************)

	    (* Look for the phase-splitter idiom for polymorphic recursion.
	     * This function takes a function body and checks to see if it 
	     * is of the form   
	     *   let bnds @ (Fix v f) in v 
	     *)
	    fun getInnerLambdaFun body = 
	      (case body 
		 of Let_e (_,bnds,e) =>
		   (case (rev bnds,e)
		      of ((Fixopen_b (vfset))::restbnds,Var_e v) =>
			(case Sequence.toList vfset
			   of [((v',c),f)] => 
			     if Name.eq_var (v',v) then
			       SOME (rev restbnds,((v,c),f))
			     else NONE
			    | _ => NONE)
		       | _ => NONE)
		  | _ => NONE)
		 
		 
	    fun rewriteCandidate (v : var,{status,called}) : rewrite option= 
	      (case status
		 of Candidate{replace, body, tipe, tActuals = ref(SOME tActs),
			      tFormals,eFormals,isrecur,...} =>
		 let 

		   (* The type arguments were only
		    instantiated in one way.  Create
		    bindings that set the type arguments to
		    these types.  
		    *)
		   val bnds1 = 
		     Listops.map2 
		     (fn (v,c) => (Con_b(Runtime,Con_cb(v,c)))) 
		     (tFormals, tActs)
		     
		   (* Create bindings that set the term arguments
		    to unit.
		    *)
		   val bnds2 = 
		     map (fn v => Exp_b(v,TraceUnknown,
					NilDefs.unit_exp))
		     eFormals
		   val outerbnds = bnds1@bnds2

		   val rewrite = 
		     if !isrecur then
		       (case getInnerLambdaFun body
			  of SOME (innerbnds,((v',c),Function{effect,               (*...with a lambda body *)
							      recursive,
							      tFormals, eFormals, fFormals, 
							      body})) =>
			    let 
			      (*In this case, we have a specializable recursive function
			       * of the idiomatic form
			       *     Fix  v (/\a.\x.let bnds @ Fix v' f in v')
			       * We specialize it by eliminating the outer Fix, 
			       * hoisting the inner bnds, and renaming the inner fix
			       * to use the chosen replace name.  We will eventually 
			       * return a list of bnds of the general form:
			       *    a=c, x=(), bnds, Fix replace (let v' = replace in f)
			       *)
			      
			      val newfun = Function{effect=effect,
						    recursive=Arbitrary,
						    tFormals=tFormals, 
						    eFormals=eFormals, 
						    fFormals=fFormals, 
						    body=body}
				
			    in Eta (outerbnds@innerbnds,((v',c),newfun),v')
			    end
			
			     (* The code was not recognizably in the above idiom.  We cannot
			      * eliminate the outer fix, be we can at least rewrite the function
			      * to use the actual arguments instead of its formals.  We do this
			      * by renaming the formals to fresh variables, and adding bindings of
			      * the actual parameters to the old formal parameter names.
			      *)
		         | NONE => 
			    let
			      val f =
				Function{effect=Total,   (*We only consider specializing total functions*)
					 recursive=Leaf, (*The only total recursive functions are Leaf functions*)
					 tFormals=map Name.derived_var tFormals,
					 eFormals=map (fn v => (Name.derived_var v,TraceUnknown)) eFormals,
					 fFormals=[],    (*We only specialize functions with no float args *)
					 body = body}
			    in Spec (outerbnds,((v,tipe),f))
			    end)
		     else
		       (* A non-recursive specializable function.  Just extract
			* the body and wrap it with the actuals. *)
		       let 
			 val bodybnds = case body (* flatten, while we're here. *)
					  of Let_e (_,bnds,Var_e bvar) => 
					    bnds @ [Exp_b(replace, TraceUnknown,Var_e bvar)]
					   | _ =>  [Exp_b(replace, TraceUnknown,body)]
		       in  Elim (outerbnds@bodybnds,replace)
		       end
		 in  
		   SOME rewrite 
		 end
	      | Candidate _ => SOME Dead
	      (* If after the first pass is done the
	       function is still a candidate but tActuals
	       is NONE, then the function was never
	       applied nor referenced, and so is dead.
	       *)
	       
	      | _ => if !called then NONE else SOME Dead )


	    fun rewriteCandidates () : unit = 
	      (rewrites := Name.VarMap.mapPartiali rewriteCandidate (!mapping))

            (* *)
	    fun rewriteApplication (v : var) : var option=
	      (case Name.VarMap.find(!rewrites,v)
		 of SOME (Elim(_,replace)) => SOME replace
		  | SOME (Eta(_,_,replace)) => SOME replace
		  | _ => NONE)

	    fun getRewriteBnds ((v : var,_),_) : bnd list option = 
	      (case Name.VarMap.find(!rewrites,v)
		 of SOME (Elim(bnds,_)) => SOME bnds
		  | SOME (Eta(bnds,_,_)) => SOME bnds
		  | SOME (Spec(bnds,_)) => SOME bnds
		  | _ => NONE)

	    fun getRewriteFns ((v : var,c : con),f :function) = 
	      (case Name.VarMap.find(!rewrites,v)
		 of SOME (Elim _) => NONE
		  | SOME Dead => NONE
		  | SOME (Eta(_,vcf,_)) => SOME vcf
		  | SOME (Spec(_,vcf)) => SOME vcf
		  | NONE => SOME ((v,c),f))
	      
	end (* local *)


        (* This first set of functions collects functions that are
	   candidates for specialization.  In the second pass, we
	   eliminate all calls to specializable functions and patch
	   the specialized function by wrapping the constructor
	   arguments around the function. 
        *)

       (************)
       (* Pass One *)
       (************)

       fun scan_exp ctxt (exp : exp) : unit = 
	 (case exp of
	    Var_e v => escapeCandidate v

	  | Const_e _ => ()

	  | Prim_e(p,trlist,clist,elist) => app (scan_exp ctxt) elist

	  | Switch_e sw => scan_switch ctxt sw

	  | Let_e (letsort,bnds,e) => scan_bnds(bnds,ctxt, fn ctxt => scan_exp ctxt e)

	  | App_e(openness,f,clist,elist,eflist) => 
	      let 
		val _ = app (scan_exp ctxt) elist
		val _ = app (scan_exp ctxt) eflist
	      in  
		(case (f,eflist) of
		   (Var_e v, []) => checkCandidate(v,ctxt,clist,elist)
		 | _ => scan_exp ctxt f)
	      end

	  | ExternApp_e(f,elist) => (scan_exp ctxt f; 
				     app (scan_exp ctxt) elist)

	  | Raise_e(e,c) => scan_exp ctxt e

	  | Handle_e{body,bound,handler,result_type} =>
	      (scan_exp ctxt body; 
	       scan_exp ctxt handler)

	  | Coerce_e (coercion,cargs,exp) =>
	      (scan_exp ctxt coercion; scan_exp ctxt exp)
	  | Fold_e _ => ()
	  | Unfold_e _ => ())

	and scan_switch ctxt (switch : switch) : unit = 
	  let 
	    fun scan_default NONE = ()
	      | scan_default (SOME e) = scan_exp ctxt e
	  in  
	    (case switch of
	       Intsw_e {arg,arms,default,size,result_type} => 
		 let 
		   val _ = scan_exp ctxt arg
		   val _ = scan_default default
		 in  
		   app (fn (_,e) => (scan_exp ctxt e)) arms
		 end
	     | Sumsw_e {arg,arms,default,bound,sumtype,result_type} =>
		 let 
		   val _ = scan_exp ctxt arg
		   val _ = scan_default default
		   val ctxt = NilContext.insert_con(ctxt,bound,sumtype)
		 in  
		   app (fn (_,_,e) => (scan_exp ctxt e)) arms
		 end

	     | Exncase_e {arg,arms,default,bound,result_type} =>
		 let 
		   val _ = scan_exp ctxt arg
		   val _ = scan_default default
		 in
		   app (fn (e1,_,e2) => 
			let val _ = scan_exp ctxt e1
			  val c1 = Normalize.type_of(ctxt,e1)
			  val (_,c1) = Normalize.reduce_hnf(ctxt,c1)
			  val SOME carriedCon = NilUtil.strip_exntag c1
			  val ctxt = NilContext.insert_con(ctxt,bound,carriedCon)
			in  scan_exp ctxt e2
			end)
		       arms
		 end

	     | Typecase_e _ => error "typecase_e not done")
	  end
(*
<<<<<<< specialize.sml
	and scan_function (((v:var, c),
			    f as Function{tFormals,fFormals,eFormals,body,recursive,...}), ctxt) =
	    let	
		val {tFormals=tFa,eFormals=eFa,...} = strip_arrow_norm ctxt c
		val fctxt = NilContext.insert_con(ctxt, v, c)
		val ctxt = NilContext.insert_kind_list(fctxt, ListPair.map (fn (v, (_, k)) => (v, k)) (tFormals, tFa))
		val ctxt = NilContext.insert_con_list(ctxt, ListPair.map (fn ((v,_),c) => (v,c)) (eFormals, eFa))
		val float = NilDefs.unboxfloat_con
		val ctxt = NilContext.insert_con_list(ctxt,map (fn v => (v, float)) fFormals)
		val _ = scan_exp ctxt body
	    in  fctxt
	    end

	and scan_bnds(bnds : bnd list, ctxt) = foldl scan_bnd ctxt bnds

	and scan_cbnd (cbnd, ctxt) = 
	    (case cbnd of
	       Con_cb(v,c) => NilContext.insert_equation(ctxt,v,c)

	     | Open_cb (v,vklist,c) => 
		 let 
		   val k = Arrow_k(Open,vklist,Single_k c)
		 in  
		   NilContext.insert_kind(ctxt,v,k)
		 end

	     | Code_cb (v,_,_) => error "Cannot handle Code_cb")
=======*)
       (*Scan a cluster of functions.
	* fctxt already contains the types of all of the functions in this cluster
	*)
       and scan_functions (vflist : ((var * con) * function) list, fctxt : context) : unit  = 
	 let 
	  
	   (* We may traverse the functions in any order, since they are 
	    * mututally recursive.  However, we can do more specialization
	    * if we choose a good order - that is, if we choose to first traverse
	    * the bodies of functions about which we have already made specialization
	    * decisions.  For example, in the following code:
	    *   fun f /\a.\(x:a):unit = g[a]x
            *   and g /\a.\(x:a):unit = f[a]x
            *   val x = g[int]3
	    * g is known to be potentially specializable after traversing the last
	    * binding.  But if we traverse f before g, then the application g[a]x
	    * will be viewed as a polymorphic use of g, and so g will not be specialized.
	    * However, if we traverse g first using the assumption a=int, then we will
	    * mark f as potentially specializable at type int as well.  And when we 
	    * subsequently traverse f using the assumption a=int, the specialization
	    * will succeed, and the functions will be specialized.
	    *
	    * Therefore, we first split the cluster into the unconstrained functions
	    * which are those for which no application has been seen, and the 
	    * constrained functions for which we have made some tentative decision.
	    * After traversing the constrained functions, we recur on the unconstrained
	    * functions.  Once we run out of constrained functions, any functions that 
	    * are still unconstrained are dead.
	    *
            * The only dead functions that may potentially be scanned by this are impure
	    * functions, since we currently don't record whether or not they are ever applied.
	    *)
	   fun loop vflist = 
	     (case List.partition (fn ((v,_),_) => unconstrainedFunction v) vflist
		of (_,[]) => ()  (* All remaining functions are dead *)
		 | (unconstrained,constrained) => 
		    (
		     app (scan_function fctxt) constrained;
		     loop unconstrained
		     ))
	 in loop vflist
	 end


       and scan_function fctxt ((v:var,c:con), f as Function{tFormals,fFormals,eFormals,
						     body,recursive,...}) =
	 let 
	   (* Set the shared ref to false *)
	   val _ = enterFunction v

	   (* If we have made a (tentative) decision, assume that we have
	    * specialized the function, and check whether the function is 
	    * still specializable under these assumptions.  This allows us 
	    * to specialize trivial uses of polymorphic recursion.
	    *)
	   val ctxt = 
	     (case getCandidateBnds v
		of SOME bnds => foldl (fn (b,c) => NilContext.insert_bnd(c,b)) fctxt bnds
		 | NONE => 
		  let	
		    val {tFormals,eFormals=eFa,...} = rename_arrow (strip_arrow_norm fctxt c,tFormals)
		    val ctxt = NilContext.insert_kind_list(fctxt, tFormals)
		    val ctxt = NilContext.insert_con_list(ctxt, ListPair.map (fn ((v,_),c) => (v,c)) (eFormals, eFa))
		    val ctxt = NilContext.insert_con_list(ctxt,map (fn v => (v, NilDefs.unboxfloat_con)) fFormals)
		  in ctxt
		  end)
	   val _ = scan_exp ctxt body

	   (* Check the shared ref to see if the function was recursive*)
	   val _ = leaveFunction v
	 in ()
	 end



       (* In order to implement the bottom up traversal, we give scan_bnds
	* a continuation to call when it reaches the bottom.  Note that
	* we must do work on the way down, adding candidates and keeping 
	* a context.  But the bnds are not actually traversed until the
	* upward phase.  The continuation is necessary, since we might
	* be in a let or might be at the top-level.
	*)
	and scan_bnds (bnds : bnd list, ctxt, k : context -> unit) : unit = 
	  (case bnds 
	     of [] => k ctxt
	      | bnd::bnds =>
	       let
		 val kctxt = NilContext.insert_bnd (ctxt,bnd)
		 val _ = 
		   (case bnd of
		      Exp_b(v,_,e) => (scan_bnds (bnds,kctxt,k); scan_exp ctxt e)
		    | Con_b(p,cbnd) => scan_bnds (bnds,kctxt,k)
		    | Fixopen_b vfset =>
			let 
			  val vflist = (Sequence.toList vfset)
			  (* Allocate a shared ref for the cluster and add the candidates *)
			  val called = ref false
			  val _ = app (fn ((v,c),f) => addCandidate(ctxt,v,c,called,f)) vflist

			  val _ = scan_bnds (bnds,kctxt,k)

			  (* If none of the functions have been called yet, nor do any of the
			   * functions escape, then the whole cluster is dead.  Noticing this
			   * may allow us specialize functions that are used polymorphically
			   * in dead code that has not yet been eliminated.
			   *)
			  val live = !called orelse List.exists (fn ((v,_),_) => escapingFunction v) vflist
			  val _ = if live then scan_functions (vflist,kctxt) else () 
			  (* Reuse the called flag to indicate whether or not the function is live *)
			  val _ = called := live
			in ()
			end
		    | Fixcode_b _ => error "sorry: Fixcode not handled"
		       
		    | Fixclosure_b _ => error "sorry: Fixclosure not handled")
	       in ()
	       end)
(*>>>>>>> 1.35*)


	fun scan_import(ImportValue(l,v,tr,c),ctxt) = 
	       insert_label(insert_con(ctxt,v,c),l,v)
	  | scan_import(ImportType(l,v,k),ctxt)  = 
	       insert_label(insert_kind(ctxt,v,k),l,v)
	  | scan_import(ImportBnd (_, cb),ctxt) =
	       let
		   val (v, k) = NilStatic.kind_of_cbnd (ctxt, cb)
	       in
		   insert_kind(ctxt,v,k)
	       end

	fun scan_export ctxt (ExportValue(l,v)) = (scan_exp ctxt (Var_e v))
	  | scan_export ctxt (ExportType(l,v)) = ()

	fun scan_module(MODULE{imports, exports, bnds}) : unit = 
	  let 
	    val ctxt = NilContext.empty()
	    val ctxt = foldl scan_import ctxt imports
	    val _ = scan_bnds(bnds,ctxt, fn ctxt => app (scan_export ctxt) exports)
	  in 
	    ()
	  end

       (************)
       (* Pass Two *)
       (************)

	fun do_exp (exp : exp) : exp = 
	   (case exp of
	      Var_e v => exp

	    | Const_e _ => exp

	    | Prim_e(p,trlist,clist,elist) => Prim_e(p, trlist,clist, 
						     do_explist elist)

	    | Switch_e sw => Switch_e(do_switch sw)

	    | Let_e (letsort,bnds,e) => Let_e(letsort,do_bnds bnds, do_exp e)

	    | App_e(openness,Var_e v,clist,elist,[]) => 
		(* If this is an instantiation that we're eliminating,
		   replace it with a reference to the statically-instantiated
		   code
		 *)
		(case (rewriteApplication v) of
		   NONE => App_e(openness,Var_e v,clist,do_explist elist,[])
		 | SOME replace => Var_e replace)

	    | App_e(openness,f,clist,elist,eflist) => 
		App_e(openness, do_exp f, clist, do_explist elist, 
		      do_explist eflist) 

	    | ExternApp_e(f,elist) => ExternApp_e(do_exp f,do_explist elist)
		
	    | Raise_e(e,c) => Raise_e(do_exp e, c)
		
	    | Handle_e{body,bound,handler,result_type} => 
		Handle_e{body = do_exp body, bound = bound,
			 handler = do_exp handler, 
			 result_type = result_type}

	    | Coerce_e (coercion,cargs,exp) =>
		Coerce_e (do_exp coercion, cargs, do_exp exp)

	    | Fold_e _ => exp
		
	    | Unfold_e _ => exp)

	and do_explist (explist : exp list) = map do_exp explist

	and do_expopt NONE = NONE
	  | do_expopt (SOME e) = SOME (do_exp e)

	and do_switch (switch : switch) : switch = 
	   (case switch of
		Intsw_e {size,arg,arms,default,result_type} =>
		    let val arg = do_exp arg
			val arms = map (fn (w,e) => (w,do_exp e)) arms
			val default = do_expopt default
		    in  Intsw_e {size=size,arg=arg,arms=arms,default=default,
				 result_type=result_type}
		    end

	      | Sumsw_e {sumtype,arg,bound,arms,default,result_type} =>
		    let val arg = do_exp arg
			val arms = map (fn (t,tr,e) => (t,tr,do_exp e)) arms
			val default = do_expopt default
		    in  Sumsw_e {sumtype=sumtype,arg=arg,
				 bound=bound,arms=arms,default=default,
				 result_type=result_type}
		    end

	      | Exncase_e {arg,bound,arms,default,result_type} =>
		let val arg = do_exp arg
		    val arms = map (fn (e1,tr,e2) => 
				       (do_exp e1, tr, do_exp e2)) 
                                   arms
		    val default = do_expopt default
		in  Exncase_e {arg=arg,
			       bound=bound,arms=arms,default=default,
			       result_type=result_type}
		end
	      | Typecase_e _ => error "typecase not handled"
	      | Ifthenelse_e _ => error "Ifthenelse not handled")

	and do_bnds(bnds : bnd list) : bnd list = 
	    let 
	      val bnds_list = map do_bnd bnds
	    in  
	      List.concat bnds_list
	    end

	and do_bnd (bnd : bnd) : bnd list =
	  (case bnd of
	     Exp_b(v,traceinfo,e) => [Exp_b(v,traceinfo,do_exp e)]
	   | Con_b(v,c) => [bnd]

	   | Fixopen_b vcflist =>
	       let 
		 val bndss = List.mapPartial getRewriteBnds vcflist
		 val bnds = List.concat bndss
		 val vcflist = List.mapPartial getRewriteFns vcflist
		 val bnds = do_bnds bnds
		 val vcflist = map (fn ((v,c),f) => ((v,c),do_function f)) vcflist
	       in  
		 bnds @ [Fixopen_b vcflist] 
	       end

	   | Fixcode_b _ => error "sorry: Fixcode not handled"
	       
	   | Fixclosure_b _ => error "sorry: Fixclosure not handled")

        and do_function (Function {effect,recursive,tFormals,eFormals,fFormals,body}) = 
	     Function{effect=effect,
		      recursive=recursive,
		      tFormals=tFormals,
		      eFormals=eFormals,
		      fFormals=fFormals,
		      body = do_exp body}


	fun do_import import = import

	fun do_export(ExportValue(l,v)) = ExportValue(l,v)
	  | do_export(ExportType(l,v))  = ExportType(l,v)
	    
	fun do_module(MODULE{imports, exports, bnds}) = 
	  let 
	    val imports = map do_import imports
	    val bnds = do_bnds bnds
	    val exports = map do_export exports
	  in 
	    MODULE{imports=imports,exports=exports,bnds=bnds}
	  end

	(* Main optimization routine:
             Scan module for specializable candidates
	     Rewrite module by specializing candidate and 
	     rewriting calls to candidates
	   *)

      fun optimize module = 
	  let val _ = reset()
	      val _ = scan_module module
	      val _ = showCandidates()
	      val _ = rewriteCandidates()
	      val result = do_module module 
	      val _ = reset()
	  in result
	  end

end
