(*$import Util Stats Sequence Array Prim Ppnil Int Nil Listops Name NilUtil NilSubst ANALYZE NilDefs *)

(* This module performs function usage analysis.
   The result is some information for each term-level function
      and the way it is used each time it appears.  First, 
      we record the size of the function.  Second, each occurrence 
      of the function variable is classified by 
	  (1) a flag indicating whether the usage occurred
	     inside the scope of the function (or one mutually defined)
	  (2) a level number indicating how fully the (curried) 
	  function is applied.  Note that if the result of early 
	  applications "escape", then later applications are not eligible.

   Here are three examples of showing the occurrence levels 
      of the function variable f.

   val g = (f, 5)      level = 0
   val g = f 5         level = 1
   val g = f 5 6       level = 2

   The analysis will attempt to compute the maximum level for each use.  

   Consider this expression:

   let val g = f 5
       val h = g 6
   in  ...
   end	 

   If g occurs in ..., then the level number for the occurrence of f is 1.
   Otherwise, g does not escape (i.e., is used only in a further application)
   and so the occurrence level of f is 2.

   The result of the analysis is useful for inlining and uncurrying.

   (1) If all occurrences of a function occurs outside the scope
       of the function, then the function is not recursive.
   (2) If all occurrences of a function has a level number higher than 0,
       then the function does not escape.
   (3) If a function is used only once non-recursively with a level number 
       higher than 0, then the "inline once" optimization can be used.
   (4) If a function f is curried (e.g. has type int -> int -> int -> int)
       and each occurrence is non-recursive and has a level number higher
       than 2, then the first two arrows may be flattened to yield a function
       with type (int * int) -> int -> int, and each application of f
       is re-written.  For example, "f a b" becomes "f (a,b)".  It is guaranteed
       that "f a" will not occur in isolation and escape.  
       This optimization is always beneficial as it requires no construction
       of wrapper functions.
*)

(* Some optimizations depend on all applications being of variables. *)

structure Analyze :> ANALYZE = 
struct
  open Nil Name NilUtil

  val error = fn s => Util.error "analyze.sml" s
  val debug = Stats.ff("DebugAnalyze")

  type funinfo = {size : int,
		  definition : function,
		  occurs : (bool * int) list}

  local
      type occur = var * bool * int ref
	  (* 1: variable that occurs
	   * 2: whether it occurs recursively
	   * 3: currently determined highest possible level
	   *)

      val sizeDefs = ref (Name.VarMap.empty : (int * Nil.function) Name.VarMap.map)
	  (* 1: size of function body
	   * 2: function itself
	   *)

      val occurs = ref ([] : occur list)

      val constraints = ref (Name.VarMap.empty : (int * int ref) Name.VarMap.map)
	  (* 1: Level that bounds from above the level of the function of which the variable represented by this constraint
	   *    is some degree of partial application, if that variable occurs in code
	   * 2: Currently determined highest possible level of that function
           *)

      fun isFunction f = (case Name.VarMap.find(!sizeDefs,f) of
			      NONE => false
			    | SOME _ => true)
  in
      fun reset() = (sizeDefs := Name.VarMap.empty; occurs := [])
      fun addFunction(v,i,f) = sizeDefs := Name.VarMap.insert(!sizeDefs, v, (i, f))
      fun addConstraint(v, n, r) = constraints := Name.VarMap.insert(!constraints, v, (n,r))

      (* Add level constraints for a particular sequence of applications starting from a curried function. *)
      fun applyOccur(f,binds : var list) = 
	  let val potentialLevel = 1 + (length binds)
	      val r = ref potentialLevel
	      val constraint = Listops.mapcount (fn (n,v) => (addConstraint(v, n, r))) (f::binds)
	      (* While in the body of a function, we have not added the function yet *)
	      val inside = not (isFunction f)
	  in  occurs := (f, inside, r) :: (!occurs)
	  end

      (* Possibly update max level of a function based on an occurrence of a variable that is the result of an application
       * of it. *)
      fun varOccur v = 
	  (occurs := (v, not (isFunction v), ref 0) :: (!occurs);
	   case Name.VarMap.find(!constraints, v) of
	       NONE => ()
	     | SOME (lower, r) => if (lower < !r) then r := lower else ())

      (* Create the map to be returned based on data kept in sizeDefs and occurs *)
      fun collect() : funinfo Name.VarMap.map =
	  let val table = Name.VarMap.map (fn (i,f) => {size = i, definition = f, occurs = []}) (!sizeDefs)
	      fun folder ((v, inside, ref level), table) = 
		  (case Name.VarMap.find(table, v) of
		       NONE => table  (* application variable not a function *)
		     | SOME {size, occurs, definition} =>
			   let val entry = {size = size, definition = definition,
					    occurs = (inside, level) :: occurs}
			   in  Name.VarMap.insert(table, v, entry)
			   end)
	      val table = foldl folder table (!occurs)
	      val _ = reset()
	  in  table
	  end
  end

  (* The following functions primarily calculate the sizes of various Nil objects *)

  fun doList doObj objs = foldl (fn( obj,s) => (doObj obj) + s) 0 objs

  fun doKind (kind:kind):int = 
      1 + (case kind of
	       Type_k => 0
	     | SingleType_k c => doCon c
	     | Single_k c => doCon c
	     | Record_k lvkSeq => let val lvk = Sequence.toList lvkSeq
				  in  (length lvk) + (doList doKind (map #2 lvk))
				  end
	     | Arrow_k (ot,vklist,k) => (doKind k) + (doVklist vklist))

  and doVklist vkList = doList doKind (map #2 vkList)

  and doCons cons = doList doCon cons
  and doCon (con:con):int = 
      1 + 
      (case con of
	   Prim_c(pc,cs) => doCons cs
         | Mu_c(b,vcs) => doCons (map #2 (Sequence.toList vcs))
         | AllArrow_c{tFormals,eFormals,body_type,...} =>
	       (doVklist tFormals) + (doCons (map #2 eFormals)) + (doCon body_type)
         | ExternArrow_c(cs,c) => doCons (c::cs)
         | Var_c v => 0
         | Let_c(_,cbnds,c) => (doList doCbnd cbnds) + (doCon c)
         | Typeof_c e => doExp e
         | Crecord_c lcs => doCons (map #2 lcs)
         | Proj_c(c,l) => doCon c
         | Closure_c(c1,c2) => doCons [c1,c2]
         | App_c(c,cs) => doCons (c::cs)
	 | Coercion_c{vars,from,to} => (doCon from) + (doCon to)
         | Typecase_c{arg,arms,default,kind} =>
	       (doKind kind;
	        (doCon arg) + (doCon default) + 
	       (doList (fn (pc,vks,c) => (doVklist vks) + (doCon c)) arms))
         | Annotate_c (ka,c) => doCon c)

  and doCbnd (conbnd:conbnd):int = 
	1 + (case conbnd of
		 Con_cb(v,c) => doCon c
	       | Open_cb (v,vks,c) => (doVklist vks) + (doCon c)
	       | Code_cb (v,vks,c) => (doVklist vks) + (doCon c))

  (* This is the function where variable occurrences are noticed and possibly used to update function max levels. *)
  and doExp (exp:exp) : int = 
	1 + 
        (case exp of
           Var_e v => (varOccur v; 0)
	 | Const_e pv =>
	   let fun doArray(c,ea) = Array.foldl (fn (e,s) => (s + (doExp e))) (doCon c) ea
           in
	      case pv of
		 Prim.array cea => doArray(cea)
	       | Prim.vector cea => doArray(cea)
               | Prim.refcell(er) => doExp (!er)
               | Prim.tag(t,c) => doCon c
               | _ => 0 (* small constants *)
           end
         | Let_e(ls,bnds,e) => (doBnds bnds) + (doExp e)
         | Prim_e(ap,_,cs,es) => 
	     (if (NilDefs.allprim_uses_carg ap) then
                 doCons cs
	      else
	         (doCons cs; 0)) 
             + (doExps es)
         | Switch_e sw => doSwitch sw
         | App_e(ot,e,cs,es1,es2) => (doCons cs) + (doExps (e :: (es1 @ es2))) 
         | ExternApp_e(e,es) => doExps (e::es)
         | Raise_e(e,c) => (doExp e) + (doCon c)
         | Handle_e{body,bound,handler,result_type} => 
	       (doExps [body,handler]) + (doCon result_type)
	 | Coerce_e (coercion,cargs,exp) =>
	   (doCons cargs) + (doExp coercion) + (doExp exp)
	 | Fold_e (vars,from,to) => (doCon from) + (doCon to)
	 | Unfold_e (vars,from,to) => (doCon from) + (doCon to))

      and doExps (exps:exp list) : int = doList doExp exps
    
      (* We look for as many applications as possible to find *)
      and doBnds [] = 0
	| doBnds ((Exp_b(v, tr, App_e(ot, Var_e f, cons, exps, fexps))) :: rest) = 
	  let val junkVar = fresh_var()
	      val e = App_e(ot, Var_e junkVar, cons, exps, fexps)
	      fun findApp (rPartials,cur,eAcc,
			   all as ((Exp_b(v, tr, App_e(ot, Var_e f, cons, exps, fexps))) :: rest)) = 
		  if (eq_var(f,cur))
		      then let val e = App_e(ot, Var_e junkVar, cons, exps, fexps)
			   in  findApp(v::rPartials,v,e::eAcc,rest)
			   end
		  else (rev rPartials, eAcc, all)
		| findApp (rPartials, cur, eAcc, all) = (rev rPartials, eAcc, all)
	      val (partials, exps, rest) = findApp ([],v,[e],rest)
	      val _ = applyOccur(f, partials)
	      val sz = doExps exps
	  in  sz + (doBnds rest)
	  end
	| doBnds (first::rest) = (doBnd first) + (doBnds rest)

      and doBnd (bnd: bnd) : int = 
        1 + 
	(case bnd of
	   Con_b(Runtime,cb) => doCbnd cb
         | Con_b(Compiletime,cb) => (doCbnd cb; 0)
         | Exp_b(v,nt,e) => doExp e
         | Fixopen_b vfSeq => 
	     let val vf = Sequence.toList vfSeq
		 fun doFunction(v,f as Function{tFormals,eFormals,fFormals,body,body_type,...}) = 
	            let 
	              val _ = (doVklist tFormals)
	              val _ = (doCons (map #3 eFormals))
	              val _ = (doCon body_type)
	              val size = doExp body
	              val _ = if (!debug) then 
	                         (print "size of ";
	                          Ppnil.pp_var v;
	                          print " is ";
	                          print (Int.toString size);
	                          print "\n")
	                      else ()
	            in
		      (v, size, f)
                    end
		 val vsizedef = map doFunction vf
		 val _ = map addFunction vsizedef
	     in  doList #2 vsizedef
	     end
         | Fixcode_b _ => error "Fixcode_b not handled"
         | Fixclosure_b _ => error "Fixclosure_b not handled")

     and doExpopt NONE = 1
       | doExpopt (SOME e) = 1 + doExp e

     and doSwitch switch = 
        (case switch of
           Intsw_e{arg,size,arms,default,result_type} =>
	       (doExps (arg :: (map #2 arms))) +
	       (doExpopt default) 
         | Sumsw_e{arg,sumtype,bound,arms,default,result_type} =>
	       (doExps (arg :: (map #3 arms))) + 
	       (doExpopt default) 
         | Exncase_e{arg,bound,arms,default,result_type} =>
	       (doExps (arg :: ((map #1 arms) @ (map #3 arms)))) +
	       (doExpopt default)
         | Typecase_e{arg,arms,default,result_type} => 
	       (doCon arg) + (doExp default) + 
	       doList (fn (pc,vks,e) => (doExp e) + (doVklist vks)) arms)

  fun doExport (ExportValue(l,v)) = doExp(Var_e v)
    | doExport (ExportType(l,v)) = doCon(Var_c v)

  (* Produce statistics on occurrences of function symbols in a module *)
  fun analyze (Nil.MODULE{imports, bnds, exports}) = 
      let val _ = reset()
	  (* Skip imports *)
	  val _ = doBnds bnds
	  val _ = map doExport exports
      in  collect()
      end    

end