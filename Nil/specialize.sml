(*$import Prelude TopLevel Util Listops Name Int Normalize List Prim Sequence Nil NilUtil Ppnil LibBase SPECIALIZE NilContext NilDefs NilStatic *)

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

	val debug = ref false
	val diag = ref false
	val do_dead = ref true
	val do_proj = ref true

	(* In the first pass, we locate all candidate functions.
	   Candidate functions are term-level functions which
	   (0) have valuable bodies and are non-recursive
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

	  In the second pass, we rewrite candidate functions and
	     their applications.
	  (1) We rewrite the function by removing its formals
	      and wrapping the body with a let of the formals
	      bound to the type arguments as determined by step
	      3.2 of pass 1 and the term arguments to unit.
	  (2) Replace each application of the candidate function
              with the variable which is now bound to the binding.

	 There is a global state mapping each candidate function
	     to its relevant information.  As the program is 
	     traversed in the first pass, candidate functions 
	     are added and removed.  Any remaining functions 
	     after pass 1 are candidates.

	 In some sense this process is sort of a reverse inlining,
         where the type arguments in an application are moved up into
         the definition of the function.  This means, however, that
         we must check that the type arguments specified 

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
            *)

	    datatype entry = 
		Candidate of {context  : context,
			      tFormals : var list, 
			      eFormals : var list,
			      tKinds   : kind list,
			      tActuals : con list option ref,
			      body     : exp,
			      replace  : var}
	      | Impure
	      | Escaping
	      | Polymorphic

	    val mapping = ref (Name.VarMap.empty : entry Name.VarMap.map)
	in
	    (* Reset the global state 
             *)
	    fun resetCandidates() = mapping := Name.VarMap.empty

            (* Summarize the global state by printing statistics about it.
             *)
	    fun showCandidates() = 
		let fun folder(v,entry,(c,i,e,p)) = 
		    (case entry of
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
	    fun editEntry (v,entry) = 
		(case Name.VarMap.find(!mapping,v) of
		     NONE => ()
		   | SOME _ => 
		       (* XXX:  Is this remove redundant? *)
		       mapping := 
		         Name.VarMap.insert(#1(Name.VarMap.remove(!mapping,v)),
					    v, entry))
		
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
	    fun addCandidate (context, v : var, 
			      Function {recursive, tFormals, eFormals, 
					fFormals, body, ...}) = 
		let 
		  val nonRecur = (case recursive of
				    Arbitrary => false
				  | _ => true)

		  val entry = 
		    (case fFormals of
		       [] => 
			 (* No floating-point arguments? (Property 1.2) ]
			  *)
			 if (nonRecur andalso not (NilDefs.effect body)) then
			   (* Nonrecursive and no effects? (Property 1) 
			    *)
			   Candidate{body = body,
				     context = context,
				     tFormals = map #1 tFormals,
				     tKinds   = map #2 tFormals,
				     eFormals = map #1 eFormals,
				     tActuals = ref NONE,
				     replace = Name.derived_var v}
			 else Impure
		     | _ => Impure)
		in  addEntry (v,entry)
		end


	    (* On input, the constructor c is well-formed with respect
               to currentCtxt, and sub-context targetCtxt is a sub-context
               of currentCtxt (so that c might not be well-formed with
               respect to targetCtxt).

               reductTo tries to return a constructor that is provably
               equivalent to c (under currentCtxt), but which is
               well-formed with respect to targetCtxt.
             *)
	    fun reduceTo (targetCtxt, currentCtxt) (c : con) : con option = 
		let 
		  (* Is the constructor con well-formed with respect to
		     targetCtxt?  Since we know that it was well-formed
                     in currentCtxt, and there's no variable shadowing,
                     it suffices to check that all the free variables in
                     c appear in targetCtxt.
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
                     contain the type variables bound earliest possible in 
                     currentCtxt, and hence would most likely be well-formed
                     in targetCtxt.  But this is expensive and could
		     cause the type to blow up in size, so we try to
                     avoid this if possible.
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
		  (* Is the expression e valuable and of type unit? 
                   *)
		  fun isUnit (e : exp) = 
		    (not (NilDefs.effect e)) andalso 
		    (case (Normalize.reduce_hnf(callContext,
						Normalize.type_of (callContext,
								   e))) of
		       (_,Prim_c(Record_c ([],_), _)) => true
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
			       (tActuals := SOME reduced; true) 
			     else 
			       false
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
			   in  
			     isEqual
			   end)
		in   
		  (case Name.VarMap.find(!mapping, v) of
			 SOME(Candidate{context, tFormals, eFormals, 
					tActuals, body, tKinds, ...}) =>
			 if ((Listops.andfold isUnit eArgs) andalso
			     matches context tActuals tKinds) then
			   ()
			 else 
			   polyCandidate v
		       | _ => ())
		end

            (* *)
	    fun rewriteApplication (v : var) : var option=
		(case Name.VarMap.find(!mapping,v) of
		     SOME (Candidate{replace,...}) => SOME replace
		   | _ => NONE)

            (* Rewriter for functions during the second pass.  If the
	       variable v identifies a function that is still a
	       candidate after the first pass, this code returns the
	       new name of the specialized function, bindings of the
	       arguments to the specialized values, and the function's
	       body.  Otherwise it returns NONE.  
	     *)
	    fun rewriteFunction (v : var) : (var * bnd list * exp) option = 
		(case Name.VarMap.find(!mapping,v) of
		     SOME (Candidate{replace, body, tActuals = ref(SOME tActs),
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
			   SOME(replace, bnds1 @ bnds2, body)
			 end

		   | SOME (Candidate _) => 
			 (* If after the first pass is done the
                            function is still a candidate but tActuals
                            is NONE, then the function was never
                            applied nor referenced, and so is dead.
			  *)
			 (if (!debug) then
			    (print "Warning: dead function "; 
			     pp_var v; 
			     print "\n")
			  else (); 
			  NONE)
		   | _ => NONE)

	end

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

	  | Let_e (letsort,bnds,e) => 
	      let 
		val ctxt = scan_bnds(bnds,ctxt)
	      in
		scan_exp ctxt e
	      end

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

	and scan_function ((v:var, f as Function{tFormals,fFormals,eFormals,
						 body,recursive,...}), 
			   ctxt) =
	    let	
	      val fctxt = NilContext.insert_con
		              (ctxt, v, NilUtil.function_type Open f)
	      val ctxt  = NilContext.insert_kind_list(fctxt,tFormals)
	      val ctxt  = NilContext.insert_con_list
		              (ctxt, map (fn (v,_,c) => (v,c)) eFormals)
	      val float = NilDefs.unboxfloat_con
	      val ctxt  = NilContext.insert_con_list
                              (ctxt,map (fn v => (v, float)) fFormals)
	      val _ = scan_exp ctxt body
	    in  
	      fctxt
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

	and scan_bnd (bnd : bnd, ctxt) : context = 
	  (case bnd of
	     Exp_b(v,_,e) => let 
			       val _ = scan_exp ctxt e
			       val ctxt = NilContext.insert_exp(ctxt,v,e)
			     in
			       ctxt
			     end

	   | Con_b(p,cbnd) => scan_cbnd(cbnd,ctxt)

	   | Fixopen_b vfset =>
	        let 
		  val vflist = (Sequence.toList vfset)
		  val _ = app (fn (v,f) => addCandidate(ctxt,v,f)) vflist
		  val ctxt = foldl scan_function ctxt vflist
		in 
		  ctxt
		end
	   
	   | Fixcode_b _ => error "sorry: Fixcode not handled"
	   
	   | Fixclosure_b _ => error "sorry: Fixclosure not handled")


	fun scan_import(ImportValue(l,v,tr,c),ctxt) = 
	       insert_label(insert_con(ctxt,v,c),l,v)
	  | scan_import(ImportType(l,v,k),ctxt)  = 
	       insert_label(insert_kind(ctxt,v,k),l,v)

	fun scan_export (ExportValue(l,v),ctxt) = (scan_exp ctxt (Var_e v); 
						   ctxt)
	  | scan_export (ExportType(l,v),ctxt) = ctxt

	fun scan_module(MODULE{imports, exports, bnds}) : unit = 
	  let 
	    val ctxt = NilContext.empty()
	    val ctxt = foldl scan_import ctxt imports
	    val ctxt = scan_bnds(bnds,ctxt)
	    val ctxt = foldl scan_export ctxt exports
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

	      | Typecase_e _ => error "typecase not handled")


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

	   | Fixopen_b vfset =>
	       let fun getBnd(v,_) = 
		         (case rewriteFunction v of
			    NONE => NONE
			  | SOME (replace,bnds,body) => 
			      let val body = do_exp body
			      in  SOME(Exp_b(replace, TraceUnknown,
					     makeLetE Sequential bnds body))
			      end)

		   fun getFun(v,Function{effect,recursive,isDependent,
					 tFormals, eFormals, fFormals, 
					 body, body_type}) =
		         (case rewriteFunction v of
			    NONE => 
			      let 
				val body = do_exp body
			      in  
				SOME(v,Function{effect=effect,
						recursive=recursive,
						isDependent=isDependent,
						tFormals=tFormals, 
						eFormals=eFormals, 
						fFormals=fFormals,
						body = body, 
						body_type = body_type})
			      end
			  | SOME _ => NONE)
			  val vflist = (Sequence.toList vfset)
			  val bnds = List.mapPartial getBnd vflist
			  val funs = List.mapPartial getFun vflist
		      in  
			[Fixopen_b (Sequence.fromList funs)] @ bnds
		      end

		   | Fixcode_b _ => error "sorry: Fixcode not handled"

		   | Fixclosure_b _ => error "sorry: Fixclosure not handled")

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
	  let val _ = resetCandidates()
	      val _ = scan_module module
	      val _ = showCandidates()
	      val result = do_module module 
	      val _ = resetCandidates()
	  in result
	  end

end
