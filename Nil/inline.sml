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

  datatype func = datatype Analyze.func
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
  val inlineOnceC = ref 0
  val inlineManyCFun = ref 0
  val inlineManyCCall = ref 0

  val hasCandidates  = ref false
  val doCons = ref false

  fun renameCFUN (vks,body) = 
    let
      val junk = Name.fresh_named_var "junk"
    in case NilRename.renameCBnd (Open_cb(junk,vks,body))
	 of (Open_cb(_,vks,body),_) => (vks,body)
	  | _ => error "Unexpected result from renaming"
    end

  fun updateDefinition(v,f) =
      let val (table, {definition = _, size, occurs}) = Name.VarMap.remove(!analyzeTable, v)
      in  analyzeTable := (Name.VarMap.insert(table,v,{definition= FUN f,size=size,occurs=occurs}))
      end
  fun updateCDefinition(v,f) =
      let val (table, {definition = _, size, occurs}) = Name.VarMap.remove(!analyzeTable, v)
      in  analyzeTable := (Name.VarMap.insert(table,v,{definition= CFUN f,size=size,occurs=occurs}))
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

  fun is_cfun d = (case d of CFUN _ => true | _ => false)

  fun analyzeInfo {iterate,tinyThreshold, sizeThreshold, occurThreshold,inlinecons}
      (v,{size : int,
	definition,
	occurs} : Analyze.funinfo) : inlineStatus =
      let val calledOnce = (case occurs of
				[] => true  (* or dead *)
			      | [(false, n)] => n >= 1 (* Match a single non-recursive call to the function w/ no escaping *)
			      | _ => false)
	  val recursive = Listops.orfold #1 occurs
	  val escaping = Listops.orfold (fn (_, level) => level = 0) occurs
	  val has_nonescape = Listops.orfold (fn (_, level) => level > 0) occurs
	  val verysmall = size <= tinyThreshold
	  val numapps = List.foldl (fn ((recur,n),count) => if (not recur) andalso (n >= 1) then count+1 else count) 0 occurs

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
	    if (not inlinecons) andalso is_cfun definition then NoInline
	    else if recursive orelse (not has_nonescape) then NoInline
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

  and rcbnds cbnds = List.concat(map rcbnd cbnds)
  and rcon con = 
    if not (!doCons) then con
    else
      (case con
	 of Var_c _ => con
	  | Prim_c (pc,cons) => Prim_c(pc,map rcon cons)
	  | Mu_c (flag,vc_seq) => Mu_c(flag,Listops.map_second rcon vc_seq)
	  | ExternArrow_c (clist,c) => ExternArrow_c(map rcon clist,rcon c)
	  | AllArrow_c {openness,effect,tFormals,eFormals,fFormals,body_type} =>
	   AllArrow_c{openness=openness,effect=effect,tFormals=tFormals,eFormals=map rcon eFormals,fFormals=fFormals,body_type=rcon body_type}
	  | Let_c (letsort,cbnds,c) => Let_c(letsort,rcbnds cbnds,rcon c)
	  | Crecord_c lc_list => Crecord_c (Listops.map_second rcon lc_list)
	  | Proj_c (c,l) => Proj_c(rcon c,l)
	  | Closure_c (c1,c2) => 
	   Closure_c(rcon c1,rcon c2)
	  | App_c (c,clist) => App_c(rcon c,map rcon clist)
	  | Coercion_c {vars,from,to} => Coercion_c{vars = vars,from = rcon from,to = rcon to})
      
  and rcbnd conbnd = 
    if not (!doCons) then [conbnd]
    else
      (case conbnd
	 of Con_cb(a,c) =>
	   (case c of
	      App_c(Var_c f,cons) =>
		let 
		  val funinfoOpt = Name.VarMap.find(!analyzeTable, f)
		  val defOpt =
		    (case Name.VarMap.find(!optimizeTable, f) of
		       SOME NoInline => NONE
		     | SOME InlineOnce =>
			 (case funinfoOpt of
			    SOME {definition = CFUN definition,size, ...} =>
			      (inlineOnceC := 1 + (!inlineOnceC);
			       updateSizes size;
			       SOME (renameCFUN definition))
			  | _ => error "must have definition here")
		     | SOME (InlineMany _) =>
			(case funinfoOpt of
			   SOME {definition = CFUN definition,size, ...} =>
			     (inlineManyCCall := 1 + (!inlineManyCCall);
			      updateSizes size;
			      SOME (renameCFUN definition))
			 | _ => error "must have definition here")
		     | NONE => NONE)
		  val cons = map rcon cons
		in  (case defOpt of
		       NONE => [Con_cb(a,App_c(Var_c f,cons))]
		     | SOME (vks,body) =>
			 let  
			   val bnds1 =
			     Listops.map2 (fn ((v,_),c) => Con_cb(v,c)) (vks,cons)
			   val (bnds2,bnds3) =
			     (case body 
				of Let_c(_,cbnds,c) => 
				  (case c
				     of Var_c a' =>
				       let
					 val (cbnds,last) = Listops.split cbnds
				       in case last
					    of Con_cb(a'',c) =>
					      if Name.eq_var (a',a'') then
						(cbnds,[Con_cb(a,c)])
					      else
						(cbnds,[last,Con_cb(a,Var_c a')])
					     | _ => 
						(cbnds,[last,Con_cb(a,Var_c a')])
				       end
				      | _ => (cbnds,[Con_cb(a,c)]))
				 | _ => ([],[Con_cb(a,body)]))
				
			   val bnds = List.concat [bnds1,bnds2,bnds3]
			     
			   val bnds = rcbnds bnds
			 in bnds
			 end)
		end
	    | _ =>  [Con_cb(a,rcon c)])
	  | Open_cb (f,vks,body) => 
	      (case Name.VarMap.find(!optimizeTable,f) 
		 of SOME InlineOnce =>
		   let
		     val _ = enterFunction f
		     val body = rcon body
		     val _ = leaveFunction ()
		     val _ = updateCDefinition(f,(vks,body))

		   (* Note: occurrence info is not precise for type functions,
		    * since we don't analyze types (only constructors). Therefore.
		    * it is not safe to delete inlineOnce con functions, since they
		    * may appear in types.  We still always inline: in general, type
		    * functions are small anyway, and only con occurrences affect
		    * run time code size.
		    *)
		   in [Open_cb(f,vks,body)]
		   end
		  | SOME (InlineMany n) =>
		   let 
		     val _ = inlineManyCFun := 1 + (!inlineManyCFun)
		     val _ = enterFunction f
		     val body = rcon body
		     val _ = leaveFunction ()
		     val _ = if size f <= n then updateCDefinition(f,(vks,body)) else makeNoInline f
		   in  [Open_cb(f,vks,body)]
		   end
		  | _ => [Open_cb(f,vks,rcon body)])
	| Code_cb (f,vks,body) => [Code_cb (f,vks,rcon body)])

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
	 | ForgetKnown_e (sumcon,which) => ForgetKnown_e (rcon sumcon,which)
	 | Fold_e (vars,from,to) => Fold_e (vars,rcon from,rcon to)
	 | Unfold_e (vars,from,to) => Unfold_e (vars,rcon from, rcon to)
	 | Coerce_e (coercion,cargs,exp) =>
	   Coerce_e (rexp coercion,map rcon cargs,rexp exp))


  and rbnd b =
      (case b of
	   Con_b (p, cbnd) => NilUtil.cbnds2bnds (rcbnd cbnd)
	 | Exp_b(v,nt,e) =>
	       (case e of
		    App_e(Open,Var_e f,cs,es1,es2) =>
			let val funinfoOpt = Name.VarMap.find(!analyzeTable, f)
			    val defOpt =
			        (case Name.VarMap.find(!optimizeTable, f) of
				     SOME NoInline => NONE
				   | SOME InlineOnce =>
					 (case funinfoOpt of
					      SOME {definition = FUN definition,size, ...} =>
						(inlineOnce := 1 + (!inlineOnce);
						 updateSizes size;
						 SOME definition)
					    | _ => error "must have definition here")
				   | SOME (InlineMany _) =>
					 (case funinfoOpt of
					    SOME {definition = FUN definition,size, ...} =>
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
					 (case body 
					    of Let_e (ls,bnds,e) =>
					      (case e
						 of Var_e v' =>
						   let
						     val (bnds,last) = Listops.split bnds
						   in case last
							of Exp_b(v'',tr,e) => 
							  if Name.eq_var (v',v'') then
							    (bnds,[Exp_b(v,tr,e)])
							  else 
							    (bnds,[last,Exp_b(v,nt,Var_e v')])
							 | _ =>	(bnds,[last,Exp_b(v,nt,Var_e v')])
						   end
						  | _ => (bnds,[Exp_b(v,nt,e)]))
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

  fun rimports imps = 
    let
      fun doimport (ImportBnd(Runtime,conbnd)) = cbnds2importbnds (rcbnd conbnd)
	| doimport other = [other]
    in List.concat (map doimport imps)
    end

  fun reset () =
    let in
      hasCandidates := false;
      inlineOnceC := 0;
      inlineManyCCall := 0;
      inlineManyCFun := 0;
      inlineOnce := 0;
      inlineManyCall := 0;
      inlineManyFun := 0
    end


  fun inline_once (inlinecons,iterate) nilmod =
    let
      val _ = reset();
      val threshold = {iterate=iterate,tinyThreshold=0,sizeThreshold=0,occurThreshold=0,inlinecons = inlinecons}

      fun loop (nilmod,n) =
	let
	  val _ = msg ("  Pass "^(Int.toString n)^"....")

	  val _ = hasCandidates := false

	  val _ = doCons := (#inlinecons threshold)
	  val _ = functionList := []
	  val _ = analyzeTable := Analyze.analyze nilmod
	  val _ = optimizeTable := (Name.VarMap.mapi (analyzeInfo threshold) (!analyzeTable))

	  val MODULE{bnds,imports,exports,exports_int} = nilmod
	  val imports = if !hasCandidates andalso !doCons then rimports imports else imports
	  val bnds = if !hasCandidates then rbnds bnds else bnds
	  val exports_int = 
	    (case exports_int
	       of SOME ei => if !hasCandidates andalso !doCons then SOME (rimports ei) else SOME ei
		| NONE => NONE)
	  val nilmod = MODULE{bnds=bnds,imports=imports,exports=exports,exports_int = exports_int}

	  val _ = doCons := false
	  val _ = functionList := []
	  val _ = analyzeTable := Name.VarMap.empty
	  val _ = optimizeTable := Name.VarMap.empty

	in if !hasCandidates andalso iterate then loop (nilmod,n+1)
	   else nilmod
	end

      val nilmod = loop (nilmod,1)

      val _ = msg ("  " ^ Int.toString (!inlineOnce) ^
		   " functions inlined once.\n")
      val _ = 
	if (#inlinecons threshold) then 
	  msg ("  " ^ Int.toString (!inlineOnceC) ^
	       " con functions inlined once.\n")
	else ()
	  
    in  nilmod
    end

  fun inline threshold nilmod =
      let
	val _ = reset()

	val _ = doCons := (#inlinecons threshold)
	val _ = functionList := []
	val _ = analyzeTable := Analyze.analyze nilmod
	val _ = optimizeTable := (Name.VarMap.mapi (analyzeInfo threshold) (!analyzeTable))

	val MODULE{bnds,imports,exports,exports_int} = nilmod
	val imports = if !hasCandidates andalso !doCons then rimports imports else imports
	val bnds = if !hasCandidates then rbnds bnds else bnds
	val exports_int = 
	  (case exports_int
	     of SOME ei => if !hasCandidates andalso !doCons then SOME (rimports ei) else SOME ei
	      | NONE => NONE)
	val nilmod = MODULE{bnds=bnds,imports=imports,exports=exports,exports_int=exports_int}

	val _ = doCons := false
	val _ = functionList := []
	val _ = analyzeTable := Name.VarMap.empty
	val _ = optimizeTable := Name.VarMap.empty

	val _ =
	  if !hasCandidates then
	    (msg ("  " ^ Int.toString (!inlineOnce) ^ 
		  " functions inlined once.\n  " ^
		  Int.toString (!inlineManyCall) ^ " copies of " ^
		  Int.toString (!inlineManyFun) ^ " other functions inlined.\n");

	     if (#inlinecons threshold) then 
	       msg ("  " ^ Int.toString (!inlineOnceC) ^ 
		    " con functions inlined once.\n  " ^
		    Int.toString (!inlineManyCCall) ^ " copies of " ^
		    Int.toString (!inlineManyCFun) ^ " other con functions inlined.\n")
	     else ())
	  else
	    msg "  No candidate functions for inlining\n"

	val nilmod = if !hasCandidates andalso #iterate threshold
		       then (msg "  Iterating\n";
			     inline_once (#iterate threshold,#inlinecons threshold) nilmod)
		     else nilmod
      in  nilmod
      end


end (* struct *)

