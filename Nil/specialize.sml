(*$import Prelude TopLevel Util Listops Name Int Normalize List Prim Sequence Nil NilUtil Ppnil LibBase SPECIALIZE NilContext NilDefs *)

(* A two-pass optimizer to remove unnecesssarily polymorphic code:
     Essentially, convert
       fun f _ = 5
       fun g() = f () + f ()
    into
       fun f () = 5
       fun g() = f () + f ()
    This is possible when f does not escape.  
    This is desirable since calling a polymorphic function
       requires two calls and can lead to unnecessary constructor code.
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
	   Cadidatate functions are term-level functions which
	   (0) have valuable bodies and are non-recursive
	   (1) Can take constructor arguments.
	       Cannot take floating point arguments.
	       Can only take term arugments of unit type.
	   (2) Cannot escape.  That is, they can appear only
	       in application position.
	   (3) are given the same arguments at each application site.
	       (1) The terms arguments must be valuable.
	           They are already equal since they are of type unit.
	       (2) The type arguments must all be equivalent to a type
	           which is well-formed at the context where the 
		   candidate function was defined. 

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

       *)


	local
	    datatype entry = 
		Candidate of {context : context,
			      tFormals : var list, 
			      eFormals : var list,
			      tActuals : (con * kind) list option ref,
			      body : exp,
			      replace : var}
	      | Impure
	      | Escaping
	      | Polymorphic
	    val mapping = ref (Name.VarMap.empty : entry Name.VarMap.map)
	in

	    fun resetCandidates() = mapping := Name.VarMap.empty
	    fun showCandidates() = 
		let fun folder(v,entry,(c,i,e,p)) = 
		    (case entry of
			 Candidate {tActuals,...} => (c+1,i,e,p)
		       | Impure => (c,i+1,e,p)
		       | Escaping => (c,i,e+1,p)
		       | Polymorphic => (c,i,e,p+1))
		    val (c,i,e,p) = Name.VarMap.foldli folder (0,0,0,0) (!mapping)
		in  print ("  " ^ (Int.toString c) ^ " optimizable candidates.\n");
		    print ("  " ^ (Int.toString i) ^ " impure.\n");
		    print ("  " ^ (Int.toString e) ^ " escaping.\n");
		    print ("  " ^ (Int.toString p) ^ " used polymorphically.\n")
		end
	    fun editEntry (v,entry) = 
		(case Name.VarMap.find(!mapping,v) of
		     NONE => ()
		   | SOME _ => mapping := Name.VarMap.insert(#1(Name.VarMap.remove(!mapping,v)),
							     v, entry))
							     
	    fun addEntry (v,entry) = mapping := Name.VarMap.insert(!mapping,v,entry)
	    fun escapeCandidate v = editEntry (v,Escaping)
	    fun polyCandidate v = editEntry (v,Polymorphic)
	    fun addCandidate (context, v : var, Function {recursive, tFormals, eFormals, fFormals, body, ...}) = 
		let val nonRecur = (case recursive of
					Arbitrary => false
				      | _ => true)
		    val entry = 
			(case fFormals of
			     [] => if (nonRecur andalso not (NilDefs.effect body))
				       then Candidate{body = body,
						      context = context,
						      tFormals = map #1 tFormals,
						      eFormals = map #1 eFormals,
						      tActuals = ref NONE,
						      replace = Name.derived_var v}
				   else Impure
			   | _ => Impure)
		in  addEntry (v,entry)
		end
	    fun reduceTo (targetCtxt, currentCtxt) (c : con) : (con * kind) option = 
		let fun wellFormed c = 
		    let val fvs = freeConVarInCon (true,0,c)
		    in  Name.VarSet.foldl (fn (v,ok) => ok andalso bound_con(targetCtxt,v)) true fvs
		    end
		    val copt = 
			if (wellFormed c)
			    then SOME c
			else let val (_,c) = Normalize.reduce_hnf(currentCtxt, c)
			     in  if (wellFormed c)
				     then SOME c
				 else let val c = Normalize.con_normalize currentCtxt c
				      in  if (wellFormed c)
					      then SOME c
					  else NONE
				      end
			     end
		in   (case copt of
			  NONE => NONE
			| SOME c => SOME(c, NilContext.kind_of(targetCtxt, c)))
		end
	    fun checkCandidate (v, callContext, tArgs, eArgs) = 
		let fun isUnit e = 
		       (not (NilDefs.effect e)) andalso 
		       (case (Normalize.reduce_hnf(callContext,Normalize.type_of (callContext,e))) of
			    (_,Prim_c(Record_c ([],_), _)) => true
			  | _ => false)
		    fun matches defContext tActuals =
			(case !tActuals of
			     NONE => 
				 let val reduced = List.mapPartial (reduceTo (defContext, callContext)) tArgs
				 in   if (length reduced = length tArgs)
					  then (tActuals := SOME reduced; true) else false
				 end
			   | SOME tAct =>
				 let fun equal((c1,k),c2) = 
				     let val c1 = Normalize.con_normalize callContext c1
					 val c2 = Normalize.con_normalize callContext c2
					 val eq = NilUtil.alpha_equiv_con(c1,c2)					     
				     in  eq
				     end
				     val isEqual = Listops.andfold equal (Listops.zip tAct tArgs)
				 in  isEqual
				 end)
		in  (case Name.VarMap.find(!mapping, v) of
			 SOME(Candidate{context, tFormals, eFormals, tActuals, body, ...}) =>
			      if ((Listops.andfold isUnit eArgs) andalso
				  matches context tActuals)
				 then ()
			     else polyCandidate v
		       | _ => ())
		end

	    fun rewriteApplication (v : var) =
		(case Name.VarMap.find(!mapping,v) of
		     SOME (Candidate{replace,...}) => SOME replace
		   | _ => NONE)
	    fun rewriteFunction v = 
		(case Name.VarMap.find(!mapping,v) of
		     (* If tActuals is NONE, then the function is dead *)
		     SOME (Candidate{replace,body,tActuals=ref(SOME tActs),tFormals,eFormals,...}) =>
			 let val bnds1 = Listops.map2 (fn (v,(c,_)) => (Con_b(Runtime,Con_cb(v,c)))) 
			                (tFormals, tActs)

			     val bnds2 = map (fn v => Exp_b(v,TraceUnknown,NilDefs.unit_exp)) eFormals
			 in  SOME(replace, bnds1 @ bnds2, body)
			 end
		   | SOME (Candidate _) => (if (!debug)
						then (print "Warning: dead function "; pp_var v; print "\n")
					    else (); NONE)
		   | _ => NONE)

	end

        (* This first set of functions collects functions that are candidates
	   for specialization.  In the second pass, we eliminate all
          calls to specializable functions and patch the specialized
	   function by wrapping the constructor arguments around the function. *)

        (* ----------------------------- PASS ONE -------------------- *)
	fun scan_exp ctxt exp : unit = 
	   (case exp of
		  Var_e v => escapeCandidate v
		| Const_e _ => ()
		| Prim_e(p,trlist,clist,elist) => app (scan_exp ctxt) elist
		| Switch_e sw => scan_switch ctxt sw
		| Let_e (letsort,bnds,e) => 
			let val ctxt = scan_bnds(bnds,ctxt)
			in  scan_exp ctxt e
			end
		| App_e(openness,f,clist,elist,eflist) => 
			let val _ = app (scan_exp ctxt) elist
			    val _ = app (scan_exp ctxt) eflist
			in  (case (f,eflist) of
				 (Var_e v, []) => checkCandidate(v,ctxt,clist,elist)
			       | _ => scan_exp ctxt f)
			end
		| ExternApp_e(f,elist) => 
			(scan_exp ctxt f; app (scan_exp ctxt) elist)
		| Raise_e(e,c) => scan_exp ctxt e
		| Handle_e{body,bound,handler,result_type} =>
			(scan_exp ctxt body; scan_exp ctxt handler)
		| Coerce_e (coercion,cargs,exp) =>
		  (scan_exp ctxt coercion; scan_exp ctxt exp)
		| Fold_e _ => ()
		| Unfold_e _ => ())

	and scan_switch ctxt (switch : switch) : unit = 
	  let fun scan_default NONE = ()
		| scan_default (SOME e) = scan_exp ctxt e
	  in  (case switch of
		  Intsw_e {arg,arms,default,size,result_type} => 
		      let val _ = scan_exp ctxt arg
			  val _ = scan_default default
		      in  app (fn (t,e) => (scan_exp ctxt e)) arms
		      end
		| Sumsw_e {arg,arms,default,bound,sumtype,result_type} =>
		      let val _ = scan_exp ctxt arg
			  val _ = scan_default default
			  val ctxt = NilContext.insert_con(ctxt,bound,sumtype)
		      in  app (fn (t,_,e) => (scan_exp ctxt e)) arms
		      end
		| Exncase_e {arg,arms,default,bound,result_type} =>
		      let val _ = scan_exp ctxt arg
			  val _ = scan_default default
		      in  app (fn (e1,_,e2) => 
			       let val _ = scan_exp ctxt e1
				   val c1 = Normalize.type_of(ctxt,e1)
				   val (_,c1) = Normalize.reduce_hnf(ctxt,c1)
				   val SOME carriedCon = NilUtil.strip_exntag c1
				   val ctxt = NilContext.insert_con(ctxt,bound,carriedCon)
			       in  scan_exp ctxt e2
			       end) arms
		      end
		| Typecase_e _ => error "typecase_e not done")
	  end

	and scan_function ((v:var,f as Function{tFormals,fFormals,eFormals,body,recursive,...}), ctxt) =
	    let	val fctxt = NilContext.insert_con(ctxt, v, NilUtil.function_type Open f)
		val ctxt = NilContext.insert_kind_list(fctxt,tFormals)
		val ctxt = NilContext.insert_con_list(ctxt,map (fn (v,_,c) => (v,c)) eFormals)
		val float = Prim_c(Float_c Prim.F64, [])
		val ctxt = NilContext.insert_con_list(ctxt,map (fn v => (v, float)) fFormals)
		val _ = scan_exp ctxt body
	    in  fctxt
	    end

	and scan_bnds(bnds : bnd list, ctxt) = foldl scan_bnd ctxt bnds

	and scan_cbnd (cbnd,ctxt) = 
	    (case cbnd of
		 Con_cb(v,c) => NilContext.insert_equation(ctxt,v,c)
	       | Open_cb (v,vklist,c) => 
		     let val k = Arrow_k(Open,vklist,Single_k c)
		     in  NilContext.insert_kind(ctxt,v,k)
		     end
	       | Code_cb (v,_,_) => error "Cannot handle Code_cb")

	and scan_bnd (bnd : bnd, ctxt) : context = 
	  	(case bnd of
		     Exp_b(v,_,e) => let val _ = scan_exp ctxt e
					 val ctxt = NilContext.insert_exp(ctxt,v,e)
				     in  ctxt
				     end
		   | Con_b(p,cbnd) => scan_cbnd(cbnd,ctxt)
		   | Fixopen_b vfset =>
		      let val vflist = (Sequence.toList vfset)
			  val _ = app (fn (v,f) => addCandidate(ctxt,v,f)) vflist
			  val ctxt = foldl scan_function ctxt vflist
		      in ctxt
		      end
		   | Fixcode_b _ => error "sorry: Fixcode not handled"
		   | Fixclosure_b _ => error "sorry: Fixclosure not handled")


	fun scan_import(ImportValue(l,v,tr,c),ctxt) = insert_label(insert_con(ctxt,v,c),l,v)
	  | scan_import(ImportType(l,v,k),ctxt)  = insert_label(insert_kind(ctxt,v,k),l,v)

	fun scan_export (ExportValue(l,v),ctxt) = (scan_exp ctxt (Var_e v); ctxt)
	  | scan_export (ExportType(l,v),ctxt) = ctxt

	fun scan_module(MODULE{imports, exports, bnds}) : unit = 
	  let val ctxt = NilContext.empty()
	      val ctxt = foldl scan_import ctxt imports
	      val ctxt = scan_bnds(bnds,ctxt)
	      val ctxt = foldl scan_export ctxt exports
	  in  ()
	  end


        (* ----------------------------- PASS TWO -------------------- *)
	fun do_exp (exp : exp) : exp = 
	   (case exp of
		  Var_e v => exp
		| Const_e _ => exp
		| Prim_e(p,trlist,clist,elist) => Prim_e(p, trlist,clist, do_explist elist)
		| Switch_e sw => Switch_e(do_switch sw)
		| Let_e (letsort,bnds,e) => Let_e(letsort,do_bnds bnds, do_exp e)
		| App_e(openness,Var_e v,clist,elist,[]) => 
		      (case (rewriteApplication v) of
			  NONE => App_e(openness,Var_e v,clist,do_explist elist,[])
			| SOME replace => Var_e replace)
		| App_e(openness,f,clist,elist,eflist) => 
		      App_e(openness, do_exp f, clist, do_explist elist, do_explist eflist) 
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
		    val arms = map (fn (e1,tr,e2) => (do_exp e1, tr, do_exp e2)) arms
		    val default = do_expopt default
		in  Exncase_e {arg=arg,
			       bound=bound,arms=arms,default=default,
			       result_type=result_type}
		end
	      | Typecase_e _ => error "typecase not handled")


	and do_bnds(bnds : bnd list) : bnd list = 
	    let val bnds_list = map do_bnd bnds
	    in  (List.concat bnds_list)
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
						tFormals, eFormals, fFormals, body, body_type}) =
			      (case rewriteFunction v of
				   NONE => 
				       let val body = do_exp body
				       in  SOME(v,Function{effect=effect,recursive=recursive,isDependent=isDependent,
							   tFormals=tFormals, eFormals=eFormals, fFormals=fFormals,
							   body = body, body_type = body_type})
				       end
				 | SOME _ => NONE)
			  val vflist = (Sequence.toList vfset)
			  val bnds = List.mapPartial getBnd vflist
			  val funs = List.mapPartial getFun vflist
		      in  [Fixopen_b (Sequence.fromList funs)] @ bnds
		      end
		   | Fixcode_b _ => error "sorry: Fixcode not handled"
		   | Fixclosure_b _ => error "sorry: Fixclosure not handled")

	fun do_import import = import

	fun do_export(ExportValue(l,v)) = ExportValue(l,v)
	  | do_export(ExportType(l,v))  = ExportType(l,v)
	    
	fun do_module(MODULE{imports, exports, bnds}) = 
	  let val imports = map do_import imports
	      val bnds = do_bnds bnds
	      val exports = map do_export exports
	  in  MODULE{imports=imports,exports=exports,bnds=bnds}
	  end

      (* Main optimization routine:
          Scan module for specializable candidates
	   Rewrite module by specializing candidate and rewriting calls to candidates
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
