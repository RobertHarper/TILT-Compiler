(* Inline functions that are non-recursive and either are called once
   or else are sufficiently small and called a sufficiently small number
   of times.

   First, we call the Analyze.analyze function to gather function usage
   information.  Then we select candidates for inlining.

   A function should be inlined if the following are true
        (1) Called once non-recursively and does not escape.
	(2) Called multiply, is non-recursive,
	    and we remain within the size threshold.

   The transformation pass inlines and removes definitions according to the
   rules above.  Care must be taken when substituting the definition of the
   function in order to avoid variable capture.  Hence, if we are substituting
   {e1/x1,...,en/xn} and hit a binding-occurrence of a variable, then we must
   alpha-convert the variable if it occurs within the free variables of e1,...,en.
   We just alpha-convert everything to be on the safe side.

   Also, when inlining a function more than one time, we need to rename
   its bound term variables to maintain the invariant that term variables
   are unique.

   This pass depends on applications being inlined being in A-normal form.
*)

structure Inline :> INLINE =
struct
  open Nil Name NilUtil

  val error = fn s => Util.error "inline.sml" s

  val chat = Stats.ff "InlineChat"
  val InlineDiag = Stats.ff "InlineDiag"
  fun msg str = if (!InlineDiag) then print str else ()

  val debug = ref false
  fun debugpr s = if (!debug) then print s else ()
  fun inc(r:int ref) = r := (!r) + 1

  datatype inlineStatus =
    NoInline | InlineOnce
    | InlineMany of int (* Inline if inlined version is below threshold *)

  (*Keep list of the functions we are currently in, so that we can update
   * their sizes when we inline
   *)
  val functionList = ref ([] : var list )
  val analyzeTable = ref (Name.VarMap.empty : Analyze.funinfo Name.VarMap.map)
  val optimizeTable = ref (Name.VarMap.empty : inlineStatus Name.VarMap.map)
  val inlineOnce = ref 0
  val inlineManyFun = ref 0
  val inlineManyCall = ref 0
  val hasCandidates  = ref false

  fun updateDefinition(v,f) =
      let val (table, {definition = _, size, occurs}) = Name.VarMap.remove(!analyzeTable, v)
      in  analyzeTable := (Name.VarMap.insert(table,v,{definition=f,size=size,occurs=occurs}))
      end

  fun size v =
    (case Name.VarMap.find(!analyzeTable,v)
       of SOME {size,...} => size
	| NONE => error "Must have definition to get size")

  fun updateSize(v,s) =
      let val (table, {definition, size, occurs}) = Name.VarMap.remove(!analyzeTable, v)
      in  analyzeTable := (Name.VarMap.insert(table,v,{definition=definition,size=size+s,occurs=occurs}))
      end

  fun updateSizes s = map (fn v => updateSize(v,s)) (!functionList)

  fun enterFunction f  = functionList := f :: (!functionList)
  fun leaveFunction () = functionList := tl (!functionList)

  fun makeNoInline v =
    let
      val _ = if !chat then (print "\tMarking function ";Ppnil.pp_var v;print " NoInline\n") else ()
      val (table,_) = Name.VarMap.remove(!optimizeTable, v)
    in optimizeTable := Name.VarMap.insert(table,v,NoInline)
    end

  fun showFunInfo(v,size,occurs,status) =
    let
      fun showOccur(isRecur, level) = (print "("; print (Bool.toString isRecur);
				       print ", "; print (Int.toString level);
				       print ") ")
    in
      print "\tFunction ";Ppnil.pp_var v;print " size=";print (Int.toString size);
      print " occurrences:\n\t\t";
      app showOccur occurs;
      print "\n";
      print "\tInline status is ";
      print (case status
	       of NoInline => "NoInline"
		| InlineOnce => "InlineOnce"
		| InlineMany n => "InlineMany "^(Int.toString n));
      print "\n"
    end

  val inline_tiny = Stats.tt "inline_tiny"

  fun analyzeInfo {iterate,tinyThreshold, sizeThreshold, occurThreshold}
      (v,{size : int,
	definition = _,
	occurs} : Analyze.funinfo) : inlineStatus =
      let val calledOnce = (case occurs of
				[] => true  (* or dead *)
			      | [(false, n)] => n >= 1 (* Match a single non-recursive call to the function w/ no escaping *)
			      | _ => false)
	  val recursive = Listops.orfold #1 occurs
	  val escaping = Listops.orfold (fn (_, level) => level = 0) occurs
	  val has_nonescape = Listops.orfold (fn (_, level) => level > 0) occurs
	  val verysmall = size <= tinyThreshold
	  val numapps = length occurs

	  val small = numapps * size < occurThreshold * sizeThreshold
	  (*(size <= sizeThreshold) andalso
	   numapps <= (occurThreshold)*)

	  val status =
	    (* We don't inline any recursive functions.
	     * We may want to consider inlining functions
	     * that are not self-recursive and do not escape.
	     * this is a common idiom.
	     *
	     * If there are no non-escaping occurrences, then
	     * there are no places to inline.
	     *)
	    if recursive orelse (not has_nonescape) then NoInline
	    else if calledOnce                      then InlineOnce
	    else if (not escaping andalso small)    then InlineMany (sizeThreshold * occurThreshold div numapps)
	    else if verysmall                       then InlineMany tinyThreshold
            else                                         NoInline

	  val _ = case status of NoInline => () |  _ => hasCandidates := true

	  val _ = if !chat then showFunInfo (v,size,occurs,status) else ()
      in status
      end

  fun ropt r opt = (case opt of NONE => NONE | SOME x => SOME(r x))

  fun rbnds bnds = List.concat(map rbnd bnds)

  and rcon con = con
  and rcbnd conbnd = conbnd

  and rexp e =
      (case e of
	   Var_e _ => e
	 | Const_e _ => e
	 | Let_e(ls,bnds,e) => Let_e(ls,rbnds bnds,rexp e)
	 | Prim_e (p, trs,cons, exps) =>
	       Prim_e (p, trs,map rcon cons, map rexp exps)
	 | Switch_e sw => Switch_e(rswitch sw)
	 | App_e (openness,f,cs,es1,es2) =>
	       App_e (openness, rexp f, map rcon cs,
		      map rexp es1, map rexp es2)
	 | ExternApp_e (f,es) => ExternApp_e (rexp f, map rexp es)
	 | Handle_e {body,bound,handler,result_type} =>
	       Handle_e{body = rexp body, bound = bound,
			handler = rexp handler, result_type = rcon result_type}
	 | Raise_e (exp, con) =>
		    Raise_e (rexp exp, rcon con)
	 | Fold_e (vars,from,to) => Fold_e (vars,rcon from,rcon to)
	 | Unfold_e (vars,from,to) => Unfold_e (vars,rcon from, rcon to)
	 | Coerce_e (coercion,cargs,exp) =>
	   Coerce_e (rexp coercion,map rcon cargs,rexp exp))


  and rbnd b =
      (case b of
	   Con_b (p, cbnd) => [(Con_b (p, rcbnd cbnd))]
	 | Exp_b(v,nt,e) =>
	       (case e of
		    App_e(Open,Var_e f,cs,es1,es2) =>
			let val funinfoOpt = Name.VarMap.find(!analyzeTable, f)
			    val defOpt =
			        (case Name.VarMap.find(!optimizeTable, f) of
				     SOME NoInline => NONE
				   | SOME InlineOnce =>
					 (case funinfoOpt of
					      SOME {definition,size, ...} =>
						(inlineOnce := 1 + (!inlineOnce);
						 updateSizes size;
						 SOME definition)
					    | _ => error "must have definition here")
				   | SOME (InlineMany _) =>
					 (case funinfoOpt of
					      SOME {definition,size, ...} =>
						  (inlineManyCall := 1 + (!inlineManyCall);
						   updateSizes size;
						   SOME (NilRename.renameFunction definition))
					    | _ => error "must have definition here")
				   | NONE => NONE)
			    val cs = map rcon cs
			    val es1 = map rexp es1
			    val es2 = map rexp es2
			in  (case defOpt of
				 NONE => [Exp_b(v,nt,App_e(Open,Var_e f,cs,es1,es2))]
			       | SOME (Function{tFormals,eFormals,fFormals,body,...}) =>
				 let  val bnd1 =
					 Listops.map2 (fn (v,c) =>
						       Con_b(Runtime,Con_cb(v,c)))(tFormals,cs)
				     val bnd2 =
					 Listops.map2 (fn ((v,tr),e) =>
						       Exp_b(v,tr,e))
					 (eFormals,es1)
				     val bnd3 =
					 Listops.map2 (fn (v,e) =>
						       Exp_b(v,TraceKnown TraceInfo.Notrace_Real, e))
					 (fFormals,es2)
				     val (bnd4,bnd5) =
					 (case body of
					      Let_e(ls,bnds,e) => (bnds,[Exp_b(v,nt,e)])
					    | _ => ([],[Exp_b(v,nt,body)]))


				     val bnds = List.concat [bnd1,bnd2,bnd3,bnd4,bnd5]

				     val bnds = rbnds bnds
				 in bnds
				 end)
			end
		  | _ =>  [Exp_b(v,nt, rexp e)])
	| Fixopen_b vfSeq =>
	    let val vf = Sequence.toList vfSeq
		val vf = List.mapPartial
		    (fn ((v,c),f) => (case Name.VarMap.find(!optimizeTable,v) of
				      SOME InlineOnce =>
					(* Tarditi's thesis suggests rewriting the definition
					 * here before inlining, and I can't see any reason
					 * not to.  In particular, rewriting it first may
					 * allow inlining to converge more quickly.
					 * -leaf
					 *)
					let
					  val f = rfunction f
					  val _ = updateDefinition(v,f)
					in NONE
					end
				    | SOME (InlineMany n) =>
					  let val _ = inlineManyFun := 1 + (!inlineManyFun)
					      val c = rcon c
					      val _ = enterFunction v
					      val f = rfunction f
					      val _ = leaveFunction ()
					      val _ = if size v <= n then updateDefinition(v,f) else makeNoInline v
					  in  SOME ((v,c),f)
					  end
				    | _ => SOME((v,rcon c),rfunction f))) vf
	    in  (case vf of
		     [] => []
		   | _ => [Fixopen_b vf])
	    end
	| Fixcode_b vfs =>
	      [Fixcode_b(Sequence.fromList
			 (List.map(fn ((v,c),f) => ((v,rcon c),rfunction f))
			  (Sequence.toList vfs)))]
	| Fixclosure_b _ => [b])
      and rfunction(Function{effect,recursive,
			     tFormals,eFormals,fFormals,
			     body}) =
	  Function{effect=effect,recursive=recursive,
		   tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
		   body=rexp body}

      and rswitch sw =
	(case sw of
	   Intsw_e{arg,size,arms,default,result_type} =>
	       Intsw_e{arg=rexp arg,size=size,
		       arms=List.map(fn(w,e) => (w,rexp e)) arms,
		       default=ropt rexp default, result_type = rcon result_type}
	 | Sumsw_e{arg,sumtype,bound,arms,default,result_type} =>
	       Sumsw_e{arg=rexp arg,sumtype=rcon sumtype,bound=bound,
		       arms=List.map(fn(w,tr,e) => (w,tr,rexp e)) arms,
		       default=ropt rexp default, result_type = rcon result_type}
	 | Exncase_e {arg,bound,arms,default,result_type} =>
	       Exncase_e{arg=rexp arg,bound=bound,
			 arms=List.map(fn(e1,tr,e2) => (e1,tr,rexp e2)) arms,
			 default=ropt rexp default, result_type = rcon result_type}
	 | Ifthenelse_e _ => error "Ifthenelse not implemented"
	 | Typecase_e {arg,arms,default,result_type} =>
	       Typecase_e{arg=rcon arg,
			  arms=List.map(fn (pc,vks,e) => (pc,vks,rexp e)) arms,
			  default= rexp default,
			  result_type = rcon result_type})

  fun reset () =
    let in
      hasCandidates := false;
      inlineOnce := 0;
      inlineManyCall := 0;
      inlineManyFun := 0
    end

  fun inline_once iterate nilmod =
    let
      val _ = reset();
      val threshold = {iterate=iterate,tinyThreshold=0,sizeThreshold=0,occurThreshold=0}

      fun loop (nilmod,n) =
	let
	  val _ = hasCandidates := false
	  val _ = functionList := []
	  val _ = analyzeTable := Analyze.analyze nilmod
	  val _ = optimizeTable := (Name.VarMap.mapi (analyzeInfo threshold) (!analyzeTable))

	  val MODULE{bnds,imports,exports} = nilmod
	  val bnds = if !hasCandidates then rbnds bnds else bnds
	  val nilmod = MODULE{bnds=bnds,imports=imports,exports=exports}

	  val _ = functionList := []
	  val _ = analyzeTable := Name.VarMap.empty
	  val _ = optimizeTable := Name.VarMap.empty

	in if !hasCandidates andalso iterate then loop (nilmod,n+1)
	   else nilmod
	end

      val nilmod = loop (nilmod,1)

      val _ = msg ("  " ^ Int.toString (!inlineOnce) ^
		   " functions inlined once.\n")

    in  nilmod
    end

  fun inline threshold nilmod =
      let
	val _ = reset()
	val _ = functionList := []
	val _ = analyzeTable := Analyze.analyze nilmod
	val _ = optimizeTable := (Name.VarMap.mapi (analyzeInfo threshold) (!analyzeTable))
	val MODULE{bnds,imports,exports} = nilmod
	val bnds = if !hasCandidates then rbnds bnds else bnds
	val nilmod = MODULE{bnds=bnds,imports=imports,exports=exports}
	val _ = functionList := []
	val _ = analyzeTable := Name.VarMap.empty
	val _ = optimizeTable := Name.VarMap.empty

	val _ =
	  if !hasCandidates then
	    msg ("  " ^ Int.toString (!inlineOnce) ^ 
		 " functions inlined once.\n  " ^
		 Int.toString (!inlineManyCall) ^ " copies of " ^
		 Int.toString (!inlineManyFun) ^ " other functions inlined.\n")
	  else
	    msg "  No candidate functions for inlining\n"

	val nilmod = if !hasCandidates andalso #iterate threshold
		       then (msg "  Iterating\n";
			     inline_once (#iterate threshold) nilmod)
		     else nilmod
      in  nilmod
      end

end (* struct *)

