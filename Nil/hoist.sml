(* Purpose:
     (1) Tries to lift computations as early as possible in the program,
         (to improve the possibilities for CSE), without causing the
         program to do too much extra work.

     (2) Because this phase is computing the valuability of each
         subexpression (to see whether it's safe to move), we take
         advantage of this information to mark functions as total
         when their bodies are valuable, even if the elaborator
	 originally marked them as partial because they weren't
	 constructors or built-in equality functions.
*)

(* Assumptions:

 * Assumes no duplicate variables in program.

*)

(* Known Bugs:

 *)

(* Known problems:

      Currently always marks switches and handles as non-valuable.
      In principle, switches with defaults where all the of the arms
      are valuable, and handles with valuable bodies and handlers
      could be lifted.  Total switches without defaults could also be
      lifted, but this would be harder to catch.  It's not immediately
      clear if there are any issues with hoisting switches.
 *)

structure Hoist :> HOIST =

struct
    open Nil Name

    fun error s = Util.error "hoist.sml" s

    fun extract NONE = error "Missing type information for analysis term"
      | extract (SOME x) = x

    (************************************************************************
     Flags:
     debug                : Print debugging information during hoisting
     ************************************************************************)

    val debug = Stats.ff("HoistDebug")
    val HoistDiag = Stats.ff("HoistDiag")

    fun msg str = if (!HoistDiag) then print str else ()

    (*Record inferred totality information in the types of functions *)
    val infer_totality = Stats.tt "infer_totality"

    (************************************************************************
     Tracing effects:
     It seems too expensive --- and unnecessary --- to maintain a full
     typing context during the hoisting transformation.  However, it
     is important to know which term applications are total (e.g.,
     polymorphic instantiations) and can be hoisted.  Therefore,
     we maintain a mapping from term variables to "abstract" types.
     The abstraction preserves only the outermost arrow (total or
     partial) and record structure, in positive positions.
     ************************************************************************)

    datatype hoist_effs =
	ARROW_EFF of effect * hoist_effs
      | REC_EFF of (label * hoist_effs) list
      | UNKNOWN_EFF

    type econtext = hoist_effs VarMap.map

    val empty_econtext  = VarMap.empty : econtext

    fun lookupvar(vmap, v) =
	(case VarMap.find(vmap, v) of
	     NONE => (print "missing variable ";
		      Ppnil.pp_var v; print "\n";
		      error "lookupvar: variable not found")
	   | SOME level => level)

    fun checkvar(vmap, v) =
	(case VarMap.find(vmap, v) of
	     NONE => (Ppnil.pp_var v; print " missing\n")
	   | SOME _ => (Ppnil.pp_var v; print " present\n"))

    (* ereclookup (effs, l)
         Returns the hoist_effs value associated with
	 label l in the given REC_EFF,
         and UNKNOWN_EFF if given UNKNOWN_EFF.
	 Because the abstraction on types is sound (though
	 not complete), we should never get ARROW_EFF here.
     *)
    fun ereclookup (effs, l) =
	(case effs of
	     REC_EFF lst =>
		 (case (Listops.assoc_eq (Name.eq_label,l,lst)) of
		      SOME effs => effs
		    | NONE => UNKNOWN_EFF)

	   | UNKNOWN_EFF => UNKNOWN_EFF
	   | _ => error "ereclookup got ARROW_EFF")

    (* con2eff (con)
         Does a best-effort abstraction of con into the
	 hoist_effs datatype.  Because we don't have a context,
	 this is sound but not complete.

         XXX  After hoisting, best-effort may not be enough, since what
              used to be a let_c is now simply a variable, with the
              information-containing bindings not visible, having been
              hoisted to an outer level.

              If this turns out to be a problem, the environment could
              maintain bindings for type variables as well.
     *)
    local
	fun cbndsToSubst ([], accum) = accum
          | cbndsToSubst ((Con_cb args)::rest, accum) =
	       cbndsToSubst (rest, args :: accum)
          | cbndsToSubst (_::rest, accum) =
	       cbndsToSubst (rest, accum)

	fun con2eff' subst (AllArrow_c{effect,body_type,...}) =
	      ARROW_EFF (effect, con2eff' subst body_type)
	  | con2eff' subst (Prim_c(Record_c lbls, types)) =
	      REC_EFF (ListPair.zip (lbls, map (con2eff' subst) types))
	  | con2eff' subst (Var_c v) =
	      (case (Listops.assoc_eq(Name.eq_var, v, subst)) of
		   NONE => UNKNOWN_EFF
		 | SOME c => con2eff' subst c)
	  | con2eff' subst (Let_c(Sequential, cbnds, c)) =
	      con2eff' (cbndsToSubst(cbnds,subst)) c
	  | con2eff' _ _ = UNKNOWN_EFF
    in
	val con2eff = con2eff' []
    end


    (************************************************************************
     Levels:
     Each variable-binding site has a nesting depth, which is called
     its level.  The top-level is level 0, and the level increases
     with increasing level depth.

     Roughly speaking, the hoisting algorithm proceeds as follows:
     Each variable will be associated with a level (where the binding
     of that variable will be hoisted to).  For each bnd, we look at
     the right-hand side, and look at the levels of each of its free
     variables.  Since this bnd cannot be hoisted above the bindings
     of any of its free variables, we look at the maximum of the
     free-variable levels.  This is the level to which we will hoist
     the bnd.  (Non-valuable bnds, variables bound as function
     arguments, etc. obviously cannot be hoisted, and so are assigned
     the level of their binding site.)

     Unfortunately, this is overly simplistic and we must make two
     refinements.  First, for each list of bindings we remember *all*
     the levels of the free variables on the right-hand sides.  This
     makes it more efficient to determine the levels of the free
     variables of let-expressions (including those generated by the
     hoisting process itself).

     Secondly, we have to decide what to do about switches.  In
     general hoisting code out of switches will slow down the program,
     because we're now doing parts of multiple arms before picking the
     "correct" arm rather than just doing the work of the correct arm.
     (This is also potientially a problem with hoisting code out of
     functions.  However, since most functions get called at least once
     and often multiple times, on average it is a win to hoist
     computations out of functions.)

     The simplest heuristic would be to never hoist code out of the
     arms of a switch, but this seems too restricted.  The
     second-simplest heuristic is to hoist only closed bindings out of
     switches (to the top level).  Since we're only hoisting valuable
     bindings (and the compiler isn't very smart) the cost of
     evaluating a valuable binding once at top level is pretty small,
     so this heuristic is very likely to be safe.  Unfortunately, it's
     pretty restrictive.

     Therefore we will implement the following heuristic: hoist any
     valuable binding out of a switch if it can also be hoisted
     outside the enclosing function.  If not, hoist it just to the
     beginning of the arm.

     Note that if we run hoist before uncurrying, this is guaranteed
     to lift all type computations in a polymorphic function outside
     of the term lambda, though they may be blocked by the enclosing
     type lambda.  For SML, this effectively lets us do all the
     required type computations "first" and then just refer to
     closures later [see optimal type lifting, etc.].

     [This last claim isn't quite true because the phase-splitter
     is currently implementing polymorphic recursive functions with
     polymorphic recursion.  If this "optimization" (?) were turned
     off, the claim would hold.]
     ************************************************************************)

    type level     = int
    type levels    = int list  (* invariant: sorted in decreasing order *)
    val  toplevel = 0
    val  emptyLevels = []

    (************************************************************************
     The information maintained by the hoisting algorithm is divided into
     an environment and a state.  The distinction is whether the information
     is threaded through the translation (state) or maintained in a
     stack-like fashion (environment).

     The environment includes:
           currentlevel  = The level number of the immediately enclosing
                           binding site;

           context       = A Nil context containing complete constructor
                           information, but no term information;

           econtext      = A mapping from term-level variables to
                           the hoist_effs abstraction of its type;

           levelmap      = A mapping from term- and type-level variables to
                           the level number of the binding site to which
                           their binding will be (initially) hoisted.

	   lastfnlevel   = level number of the binding site of the
                           enclosing function.

     The state includes:
           hoistmap      = Mapping from binding site numbers to
                           term-level bindings that we want to hoist to here.
                           NB: Bindings are kept in reversed order!
                           Each level also contains a bool indicating whether
			   the binding is valuable and a sorted list of
                           level numbers corresponding to the
                           free variables in these bindings.


           choistmap     = Mapping from binding site numbers to
                           type-level bindings that we want to hoist to here,
			   along with the phase of the binding.
                           NB: Bindings are kept in reversed order!
                           Each level also contains a sorted list of
                           level numbers corresponding to the
                           free variables in these bindings.

     ************************************************************************)

    local
	structure IntKey : ORD_KEY = struct
					 type ord_key = int
					 val compare = Int.compare
				     end
	structure IntMap = SplayMapFn(IntKey)

	fun lookup (imap, i, default) =
	    (case IntMap.find(imap, i) of
		 NONE => default
	       | SOME answer => answer)

        fun pp_levelmap levelmap =
	    let
		fun printer (v, l) = (print "[";
				      Ppnil.pp_var v;
				      print "=>";
				      print (Int.toString l);
				      print "] ")
	    in
		VarMap.appi printer levelmap;
		print "\n"
	    end


        fun pp_hoistmap (hoistmap: (bnd list * levels * bool) IntMap.map) =
	    let
		val (top_bnds,_,_) = lookup (hoistmap, 0, ([],[], true))
	    in
		print "top bnds: ";
                print (Int.toString (List.length top_bnds));
		print "\n";
		Ppnil.pp_bnds top_bnds;
		print "\n"
	    end
        fun pp_choistmap (choistmap: ((phase * conbnd) list * levels) IntMap.map) =
	    let
		fun printLevel (level, (pcbnds, _)) =
		  let
		    val (_,cbnds) = Listops.unzip pcbnds
		  in
		    (print "level "; print (Int.toString level);
		     print " cbnds: ";
		     print (Int.toString (List.length cbnds));
		     print "\n";
		     Ppnil.pp_conbnds cbnds;
		     print "\n")
		  end
	    in
		IntMap.appi printLevel choistmap
 	    end


    in
	type hoistmap = (bnd list * levels * bool) IntMap.map
        type choistmap = ((phase * conbnd) list * levels) IntMap.map
	datatype env = ENV of {currentlevel : level,
			       context : NilContext.context,
			       econtext : econtext,
			       levelmap : level VarMap.map,
			       lastfnlevel : level}
	datatype state = STATE of {hoistmap : hoistmap,choistmap : choistmap}

	val empty_env = ENV{currentlevel = toplevel,
			    context = NilContext.empty(),
			    econtext = empty_econtext,
			    levelmap = VarMap.empty,
			    lastfnlevel = toplevel}

	val empty_state = STATE{hoistmap = IntMap.empty, choistmap = IntMap.empty}

	fun pp_env (ENV{levelmap,...}) = pp_levelmap levelmap

        fun pp_state (STATE{hoistmap,choistmap}) = (pp_hoistmap hoistmap;pp_choistmap choistmap)

	(* mergeLevels : levels * levels -> levels
	     Merges two sorted (in decreasing order) lists of integers
	 *)
	fun mergeLevels' ([] : levels, levels' : levels) : levels = levels'
          | mergeLevels' (levels, []) = levels
          | mergeLevels' (levels as level::rest, levels' as level'::rest') =
	    if (level > level') then
		level :: (mergeLevels' (rest, levels'))
	    else if (level = level') then
		level :: (mergeLevels' (rest, rest'))
	    else
		level' :: (mergeLevels' (levels, rest'))

        and mergeLevels (levels1, levels2) =
	    let
(*		val _ = (print "merging ";
			 app (print o Int.toString) levels1;
			 print " with ";
			 app (print o Int.toString) levels2)
*)
		val ans = mergeLevels' (levels1, levels2)
(*
		val _= (print " to get ";
			app (print o Int.toString) ans;
			print "\n")
*)
	    in
		ans
	    end

	(*Merge many levels lists together using a tree structured algorithm.
	 * Takes advantage of the fact that we don't care about order of merging.
	 * If needed, there are better algorithms, but probably not worth it.
	 *)
	fun mergeMultiLevels levelslists =
	  let
	    fun loop ([],[levels]) = levels   (*All pairs have been merged to one list*)
(*	      | loop ([],[])                  Impossible - we always call this with one or the other non-empty *)
	      | loop ([],merged)   = loop (merged,[])   (*All pairs have been merged to a new list, recur on that list *)
	      | loop ([levels1],merged) = loop([],levels1::merged)  (* We had an odd number of lists, so just move it onto the merged list. *)
	      | loop (l1::l2::todo,merged) = loop (todo,mergeLevels(l1,l2)::merged)   (*Take two of todo list, merge them, and recur *)
	  in case levelslists
	       of [] => []
		| _ => loop (levelslists,[])
	  end


        (* mergeStates : state * state -> state *)
        fun mergeStates (STATE{hoistmap = hoistmap1, choistmap = choistmap1},
                         STATE{hoistmap = hoistmap2, choistmap = choistmap2}) =
	    let
		fun folder (level, (bnds,levels,valuable), accum_map) =
		    let val (newbnds, newlevels, newvaluable) =
                          (case IntMap.find (accum_map, level)
			       of NONE => (bnds,levels,valuable)
                                | SOME (bnds',levels',valuable') =>
				   (bnds @ bnds',
				    mergeLevels (levels,levels'),
				    valuable andalso valuable'))
		    in
			IntMap.insert(accum_map, level,
				      (newbnds, newlevels, newvaluable))
		    end

		fun cfolder (level, (bnds,levels), accum_map) =
		    let val (newbnds, newlevels) =
                          (case IntMap.find (accum_map, level)
			       of NONE => (bnds,levels)
                                | SOME (bnds',levels') =>
				   (bnds @ bnds',
				    mergeLevels (levels,levels')))
		    in
			IntMap.insert(accum_map, level, (newbnds, newlevels))
		    end

		val hoistmap =
		    IntMap.foldli folder hoistmap1 hoistmap2
		val choistmap =
		    IntMap.foldli cfolder choistmap1 choistmap2
	    in
		STATE{hoistmap = hoistmap,choistmap = choistmap}
	    end

        (* currentLevel : env -> level *)
	fun currentLevel (ENV{currentlevel,...}) = currentlevel

        (* lookupLevel : var -> level
              Tells what level the binding of this variable is
              being hoisted to *)
	fun lookupLevel (ENV{levelmap,...}, v) = lookupvar(levelmap, v)

	fun checkLevel (ENV{levelmap,...}, v) = checkvar(levelmap, v)

	(* headNormalize : env * con -> con *)
	fun headNormalize (ENV{context,...}, con) =
	    #2 (Normalize.reduce_hnf (context, con))

	(* stripArrow : env * con -> NilUtil.arrow *)
	fun stripArrow (ENV{context,...}, c) =
	    Normalize.strip_arrow_norm context c

	(* insertLabel : env * label * var -> env *)
	fun insertLabel (ENV{currentlevel,context,econtext,levelmap,lastfnlevel}, l, v) =
	    let
		val context' = NilContext.insert_label (context, l, v)
	    in
		ENV{context = context',
		    econtext = econtext,
		    currentlevel = currentlevel,
		    levelmap = levelmap,
		    lastfnlevel = lastfnlevel}
	    end

	(* insertKind : env * var * con -> env *)
	fun insertKind (ENV{currentlevel,context,econtext,levelmap,lastfnlevel}, v, k) =
	    let
		val context' = NilContext.insert_kind (context, v, k)
	    in
		ENV{context = context',
		    econtext = econtext,
		    currentlevel = currentlevel,
		    levelmap = levelmap,
		    lastfnlevel = lastfnlevel}
	    end

	(* insertEquation : env * var * con -> env *)
	fun insertEquation (ENV{currentlevel,context,econtext,levelmap,lastfnlevel}, v, c) =
	    let
		val context' = NilContext.insert_equation (context, v, c)
	    in
		ENV{context = context',
		    econtext = econtext,
		    currentlevel = currentlevel,
		    levelmap = levelmap,
		    lastfnlevel = lastfnlevel}
	    end

        (* lookupEff : env * var -> hoist_effs *)
	fun lookupEff (ENV{econtext,...}, v) =
                (lookupvar(econtext, v))
                handle e => (print "exception detected in lookupEff\n";
			     raise e)

        (* bindEff : env * var * hoist_effs -> env *)
	fun bindEff (ENV{context,econtext,currentlevel,levelmap,lastfnlevel},
		      v, effs) =
	    let
		val econtext' = VarMap.insert(econtext, v, effs)
	    in
		ENV{context = context,
		    econtext = econtext',
		    currentlevel = currentlevel,
		    levelmap = levelmap,
		    lastfnlevel = lastfnlevel}
	    end

        (* Bind a series of variables with given eff's *)
        fun bindsEff (env, [], []) = env
          | bindsEff (env, v::vs, eff::effs) =
	    bindsEff (bindEff(env, v, eff), vs, effs)
          | bindsEff _ = error "bindsEff: length mismatch"

        (* clearLevel : level * state -> state *)
	fun clearLevel (levnum, STATE{hoistmap,choistmap}) =
	    let
		val hoistmap' =
		    IntMap.insert(hoistmap, levnum, ([], emptyLevels, true))
		val choistmap' =
		    IntMap.insert(choistmap, levnum, ([], emptyLevels))
	    in
		STATE{hoistmap = hoistmap', choistmap = choistmap'}
	    end

        (* bumpCurrentlevel : env * state -> env * state * level
              increments the current level counter, clears this level
              in the state, returns updated env and state and returns
              the new current level
         *)
	fun bumpCurrentlevel (ENV{currentlevel,context,econtext,levelmap,lastfnlevel},
			      state) =
	    let
		val newlevel = currentlevel + 1
		val env' = ENV{currentlevel = newlevel,
			       context = context,
			       econtext = econtext,
			       levelmap = levelmap,
			       lastfnlevel = lastfnlevel}
		val state' = clearLevel (newlevel, state)
	    in
		(env', state', newlevel)
	    end

	fun enterFunction (ENV{currentlevel, context, econtext, levelmap, lastfnlevel})=
	    ENV{currentlevel = currentlevel,
		context = context,
		econtext = econtext,
		levelmap = levelmap,
		lastfnlevel = currentlevel}


	fun lastFnLevel (ENV{lastfnlevel,...}) = lastfnlevel

        fun deepestLevel [] = toplevel
          | deepestLevel (level::_) = level

        (* Separates out the actual target hoisting level and the other free variable levels *)
        fun splitLevels [] = (toplevel, [])
          | splitLevels (level::rest) = (level, rest)

        (* bindLevel : env * var * level -> env
              Record that the binding of the var will be at the
              given level (for now)
         *)
	fun bindLevel(ENV{currentlevel, context, econtext, levelmap, lastfnlevel},
		       v, level) =
	    ENV{currentlevel = currentlevel,
		context = context,
		econtext = econtext,
		levelmap = VarMap.insert(levelmap, v, level),
		lastfnlevel = lastfnlevel}

        (* Bind a series of variables at the same level *)
	fun bindsLevel(env, [], level) = env
          | bindsLevel(env, v::vs, level) =
	    bindsLevel(bindLevel(env, v, level), vs, level)

        (* Add a cbnd to the lowest possible hoisting level *)
	fun hoistCbnd phase (STATE{hoistmap, choistmap}, cbndlevels, cbnd) =
	    let
		val (hoistlevel, levels) = splitLevels cbndlevels
(*
		val _ = (print "Hoisting cbnd ";
			 Ppnil.pp_conbnd cbnd;
			 print "\nto level ";
			 print (Int.toString hoistlevel);
			 print "\n")
*)
		val (rev_cbnds, levels') =
                    lookup (choistmap, hoistlevel, ([],[]))
		val levels = mergeLevels (levels, levels')
		val choistmap =
		    IntMap.insert(choistmap, hoistlevel,
				  ((phase,cbnd) :: rev_cbnds, levels))
	    in
		STATE{hoistmap = hoistmap, choistmap = choistmap}
	    end

        (* Add a bnd to the lowest possible hoisting level *)
	fun hoistBnd(STATE{hoistmap,choistmap}, bndlevels, bnd, bnds_valuable)=
	    let
		val (hoistlevel, levels) = splitLevels bndlevels
(*
		val _ = (print "Hoisting ";
			 Ppnil.pp_bnd bnd;
			 print "\nto level ";
			 print (Int.toString hoistlevel);
			 print "\n")
*)
		val (rev_bnds, levels', bnds_valuable') =
                    lookup (hoistmap, hoistlevel, ([],[], true))
		val levels = mergeLevels (levels, levels')
		val bnds_valuble = bnds_valuable' andalso bnds_valuable
		val hoistmap' =
		    IntMap.insert(hoistmap, hoistlevel,
				  (bnd :: rev_bnds, levels, bnds_valuable))
	    in
		STATE{hoistmap = hoistmap', choistmap = choistmap}
	    end

        (* Extract constructor bindings at a specific hoisting level for final inclusion in the output code.
	 * Returns those bindings, the levels of free variables at the given binding level, and a state with the extracted
	 * bindings removed.
	 *)
        fun extractCbnds (STATE{hoistmap,choistmap}, levnum) =
	    let
		val (cbnds, levels) =
		    (case IntMap.find(choistmap, levnum) of
			 NONE => ([], emptyLevels)
		       | SOME (rev_choists, levels) =>
			   (rev rev_choists, levels))


		val choistmap =
		  IntMap.insert(choistmap, levnum, ([], emptyLevels))
		  (* Empty out the level we've extracted *)
		val state = STATE{hoistmap = hoistmap,choistmap = choistmap}
	    in
	      (cbnds, levels, state)
	    end

        (* Extract bindings at a specific hoisting level for final inclusion in the output code.
	 * Returns those bindings, the levels of free variables at the given binding level, and a state with the extracted
	 * bindings removed.
	 *)
        fun extractBnds (STATE{hoistmap,choistmap}, level) =
	    let
	      val (cbnds, clevels) =
		(case IntMap.find(choistmap, level) of
		   NONE => ([], emptyLevels)
		 | SOME (rev_choists, clevels) =>
		     (rev rev_choists, clevels))
	      val (bnds, levels, bnds_valuable) =
		(case IntMap.find(hoistmap, level) of
		   NONE => ([], emptyLevels, true)
		 | SOME (rev_hoists, levels, bnds_valuable) =>
		     (rev rev_hoists, levels, bnds_valuable))
	      val choistmap =
		IntMap.insert(choistmap, level, ([], emptyLevels))
	      val hoistmap =
		IntMap.insert(hoistmap, level, ([], emptyLevels, true))
	      (* Empty out the level we've extracted *)
	      val state = STATE{hoistmap = hoistmap, choistmap = choistmap}
	      val bnds = (map Con_b cbnds) @ bnds
	      val levels = mergeLevels (clevels, levels)
	    in
	      (bnds, levels, state, bnds_valuable)
	    end

	(* limitLevels: level * levels -> levels
	     Delete all level numbers in the list >= given level
             Assumes levels is sorted in decreasing order.
         *)
	fun limitLevels (levnum, []) = []
          | limitLevels (levnum, levels as lev::levs) =
	    if (levnum <= lev) then
		limitLevels (levnum, levs)
	    else
		levels

	(* Called on constructors that are directly enclosed by variable binders.
	 * Extracts bindings that have been assigned to be hoisted to be just under the corresponding binder and wraps
	 * them around con with a Let. The levels returned are those of free variables at this binding site.
	 *)
	fun limitCon (limitlevel, con, env, state, levels) =
	    let
		val currentlevel = currentLevel env
		fun loop (levnum, cbnds, levelslist, state) =
		    if (levnum > currentlevel) then
			(cbnds, mergeMultiLevels levelslist, state)
		    else
			let
			    val (cbnds', levels, state') =
				extractCbnds(state, levnum)
			in
			    loop(levnum + 1, cbnds @ cbnds', levels::levelslist, state')
			end

		val (pcbnds, levels', state') =
		    loop (limitlevel, [], [levels], state)
		val levels'' = limitLevels (currentlevel, levels')
		val cbnds = map #2 pcbnds
		val con' = NilUtil.makeLetC cbnds con
	    in
		(con', state', levels'')
	    end

	(* Called on expressions that are directly enclosed by variable binders.
	 * Extracts bindings that have been assigned to be hoisted to be just under the corresponding binder and wraps
	 * them around exp with a Let. The levels returned are those of free variables at this binding site, and the bool
	 * indicates whether all binds so wrapped are valuable.
	 *)
	fun limitExp (limitlevel : level, exp : exp, exp_valuable : bool,
		      env : env, state : state, levels : level list)
	       : exp * state * level list * bool =
	    let

		val currentlevel = currentLevel env
(*
		val _ = (print "limitExp:  levels were ";
			 app (print o Int.toString) levels;
			 print "\ncurrentlevel = ";
			 print (Int.toString currentlevel);
			 print "\nlimitlevel = ";
			 print (Int.toString limitlevel);
			 print "\n")
*)
		fun loop (levnum, bnds, levelslist, state, bnds_valuable) =
		    if (levnum > currentlevel) then
			(bnds, bnds_valuable, mergeMultiLevels levelslist, state)
		    else
			let
			    val (bnds', levels, state', bnds_valuable') =
				extractBnds(state, levnum)
			in
			    loop(levnum + 1, bnds @ bnds', levels::levelslist, state',
				 bnds_valuable' andalso bnds_valuable)
			end

		val (bnds, bnds_valuable, levels, state) =
		    loop (limitlevel, [], [levels], state, true)

		val levels = limitLevels (currentlevel, levels)
(*
		val _ = (print "limitExp:  levels' = ";
			 app (print o Int.toString) levels';
			 print "\nlimitExp:  levels'' = ";
			 app (print o Int.toString) levels'';
			 print "\n")
*)
		val exp = NilUtil.makeLetE Sequential bnds exp
		val exp_valuable = exp_valuable andalso bnds_valuable
	    in
		(exp, state, levels, exp_valuable)
	    end

    end (* local *)

    (************************************************************************

     General translation functions, with Let_e's/Let_c's being removed and
     their bindings added to state, and then hoisted in translating
     constructors/expressions that bind variables.

     val rcon  : con      * env * state -> con      * state * levels
     val rcons : con list * env * state -> con list * state * levels

     val rcbnd : conbnd      * env * state -> env * state
     val rcbnds: conbnd list * env * state -> env * state

     val rexp  : con option -> exp      * env * state ->
                   exp      * state * levels * hoist_effs * bool
     val rexps : exp list * env * state ->
                   exp list * state * levels * hoist_effs list * bool

     ************************************************************************)

    fun traceLevel env tr =
      (case tr
	 of TraceKnown (TraceInfo.Compute (v,_)) => SOME(lookupLevel(env,v))
	  | TraceCompute v => SOME(lookupLevel(env,v))
	  | _ => NONE)

    fun traceLevels env tr = case traceLevel env tr of SOME l => [l] | NONE => []

    fun rtype args = rtc Compiletime args
    and rcon args = rtc Runtime args
    and rtc phase (args as (con,env,_)) =
      let
	  val _ = if (! debug) then
	              (print "rcon: "; Ppnil.pp_con con; print "\n")
		  else
		      ()
      in
	  rtc' phase args before (if !debug then print "/rtc\n" else ())
      end

  and rtc' phase (Prim_c (primcon,cons), env, state) =
      let
	  val (cons, state, levels) = rtcs phase (cons, env, state)
      in
	  (Prim_c(primcon, cons), state, levels)
      end

    | rtc' phase (Mu_c (isRecursive, vcseq), env, state) =
      let
	  val (vars, cons) = Listops.unzip (Sequence.toList vcseq)

	  val (env, state, mulevel) = bumpCurrentlevel (env, state)
	  val env = bindsLevel (env, vars, mulevel)
	  val env = foldl (fn (v, env) => insertKind (env, v, Type_k)) env vars

	  val (cons, state, levels) = rtcs_limited phase mulevel (cons, env, state)

	  val vcseq = Sequence.fromList (Listops.zip vars cons)
      in
	  (Mu_c (isRecursive, vcseq), state, levels)
      end

    | rtc' phase (con as AllArrow_c {openness, effect, tFormals,
			       eFormals, fFormals, body_type},
	    env, state) =
      let
	  val (env, state, arglevel) = bumpCurrentlevel (env, state)

          val (tFormals, env, state, levels1) =
	        rtFormals_arrow(tFormals, env, state, arglevel)
          val (eFormals, env, state, levels2) =
	        reFormals_arrow(eFormals, env, state, arglevel)

          val (body_type, state, levels3) =
                rtc_limited phase arglevel (body_type, env, state)

	  val levels = mergeMultiLevels[levels1, levels2, levels3]

      in
	  (AllArrow_c{openness=openness,
		      effect=effect,
		      tFormals = tFormals,
		      eFormals = eFormals,
		      fFormals = fFormals,
		      body_type = body_type},
	   state, levels)
      end

    | rtc' phase (ExternArrow_c (cons, con), env, state) =
      let
	  val (cons, state, levels1) = rtcs phase (cons, env, state)
	  val (con,  state, levels2) = rtc phase (con,  env, state)
	  val levels = mergeLevels(levels1,levels2)
      in
	  (ExternArrow_c(cons, con), state, levels)
      end

    | rtc' phase (c as Var_c v, env, state) =
      let
	  val level = lookupLevel (env, v)
      in
	  (c, state, [level])
      end

    | rtc' phase (Let_c(Sequential,cbnds,cbody), env, state) =
      let
	  val (env, state) = rcbnds phase (cbnds, env, state)
      in
          rtc phase (cbody, env, state)
      end

    | rtc' phase (Let_c(Parallel,_,_),_,_) = error "rcon: Parallel Let_c found"

    | rtc' phase (Crecord_c lclist, env, state) =
      let
	  val (labels, cons) = Listops.unzip lclist
	  val (cons, state, levels) = rtcs phase (cons, env, state)
	  val lclist = Listops.zip labels cons
      in
	  (Crecord_c lclist, state, levels)
      end

    | rtc' phase (Proj_c (con,lab), env, state) =
      let
	  val (con, state, levels) = rtc phase (con, env, state)
      in
	  (Proj_c(con,lab), state, levels)
      end

    | rtc' phase (Closure_c(c1, c2), env, state) =
      let
	  val (c1, state, levels1) = rtc phase (c1, env, state)
	  val (c2, state, levels2) = rtc phase (c2, env, state)
	  val levels = mergeLevels (levels1, levels2)
      in
	  (Closure_c(c1,c2), state, levels)
      end

    | rtc' phase (App_c(con,cons), env, state) =
      let
	  val (con,  state, levels1) = rtc phase (con,  env, state)
	  val (cons, state, levels2) = rtcs phase (cons, env, state)
	  val levels = mergeLevels (levels1, levels2)
      in
	  (App_c(con,cons), state, levels)
      end

    | rtc' phase (Coercion_c {vars,from,to}, env, state) =
      let
	val (env, state, arglevel) = bumpCurrentlevel (env, state)
	val env = foldl (fn (v,e) => insertKind(bindLevel(e,v,arglevel),v,Type_k)) env vars
	val (from,state,levels1) = rtc_limited phase arglevel (from,env,state)
	val (to,state,levels2) = rtc_limited phase arglevel (to,env,state)
	val levels = mergeLevels (levels1,levels2)
      in
	(Coercion_c {vars=vars,from=from,to=to},state,levels)
      end

  and rtcs phase (cons, env, state) =
      let
	  fun loop ([], rev_accum, state, levelslist) =
                 (rev rev_accum, state, mergeMultiLevels levelslist)
            | loop (con::cons, rev_accum, state, levelslist) =
	      let
		  val (con, state, new_levels) = rtc phase (con, env, state)
	      in
		  loop(cons, con::rev_accum, state, new_levels::levelslist)
	      end
      in
	  loop (cons, [], state, [])
      end
    and rcons args = rtcs Runtime args
    and rtypes args = rtcs Compiletime args

  (* Translation for constructors just below a binder, such that appropriate bindings are hoisted *)
  and rtcs_limited phase levnum (cons, env, state) =
      let
	  fun loop ([], rev_accum, state, levelslist) =
              (rev rev_accum, state, mergeMultiLevels levelslist)
            | loop (con::cons, rev_accum, state, levelslist) =
	      let
		  val (con, state, new_levels) =
                        rtc_limited phase levnum (con, env, state)
	      in
		  loop(cons, con::rev_accum, state, new_levels::levelslist)
	      end
      in
	  loop (cons, [], state, [])
      end
    and rcon_limited args = rtc_limited Runtime args
    and rtype_limited args = rtc_limited Compiletime args

  (* Translation for a constructor just below a binder, such that appropriate bindings are hoisted *)
  and rtc_limited phase levnum (con, env, state) =
      let
	  val (con, state, levels') = rtc phase (con, env, state)
      in
	  limitCon (levnum, con, env, state, levels')
      end

  and rcbnd phase (Con_cb(v,con), env, state) =
      let
	  val (con, state, levels) = rtc phase (con, env, state)
	  val env = bindLevel (env, v, deepestLevel levels)
	  (*val _ = (print "Bind ";
		   Ppnil.pp_var v;
		   print " at ";
		   print (Int.toString (deepestLevel levels));
		   print "\n")*)
	  val env = insertEquation (env, v, con)
	  val state = hoistCbnd phase (state, levels, Con_cb(v,con))
      in
	  (env, state)
      end

    | rcbnd phase (cbnd as Open_cb(v,tFormals,body), env, state) =
	let
	    val inner_env = enterFunction env
	    val (inner_env, state, arglevel) =
		bumpCurrentlevel (inner_env, state)

	    val (tFormals, inner_env, state, levels1) =
		rtFormals_arrow(tFormals, inner_env, state, arglevel)

	    val (body, state, levels2) =
	        rtc_limited phase arglevel (body, inner_env, state)

	    val levels = mergeLevels(levels1, levels2)
	    val env = bindLevel (env, v, deepestLevel(levels))

	    val (cbnd', vmap) = NilRename.renameCBnd cbnd
	    val v' = valOf (Name.VarMap.find (vmap, v))

	    val env = insertEquation (env, v, Let_c (Sequential, [cbnd'], Var_c v'))

	    val cbnd = Open_cb(v, tFormals, body)
	    val state = hoistCbnd phase (state, levels, cbnd)
	in
	    (env, state)
	end

    | rcbnd phase (Code_cb _, _, _) = error "rcbnd: found code_cb"

  and rcbnds phase ([], env, state) = (env, state)
    | rcbnds phase (cbnd::cbnds, env, state) =
      let
	  val (env,state) = rcbnd phase (cbnd, env, state)
      in
	  rcbnds phase (cbnds, env, state)
      end

  and rexp (args as (exp, env, state)) =
      let
	  val _ =
	      if (! debug) then
		  (print "rexp: "; Ppnil.pp_exp exp; print "\n"
		   (*; print "  env = "; pp_env env
		   ; print " state = "; pp_state state*))
	      else
		  ()
      in
	  rexp' args
      end

  and rexp' (exp as Var_e v, env : env, state : state) =
      let
	  val level = lookupLevel (env, v)
	  val effs = lookupEff(env, v)
(*
	  val _ = (print "looking up variable";
		   Ppnil.pp_var v;
		   print "and got level";
		   print (Int.toString level);
		   print "\n")
*)
      in
	  (exp, state, [level], effs, true)
      end

    | rexp' (exp as Const_e c, env, state) =
	 (exp, state, [], UNKNOWN_EFF, true)

    | rexp' (Let_e(Sequential, bnds, body), env, state) =
      let
	(* We implement flattening by viewing these bnds as existing
	 * at the current level.  We do not bump the current level,
	 * but instead just allow the bnds to be pulled upwards.
	 * Valuable bnds will be moved, but non-valuable bnds will
	 * be dumped (in order) as soon as we start to leave the current
	 * level.  So for example, if this let occurs as follows:
	 * fn () => let x = raise foo in e
	 * then we will "hoist" the binding of x, and then dump it immediately,
	 * producing the same code.  However, for code such as the following:
	 * fn () => switch (let x = raise foo in x) (....)
	 * we will hoist the binding of x as follows:
	 * fn () => let x = raise foo in switch x (....)
	 *)

	val (env, state, bnds_valuable) = rbnds(bnds, env, state)

	val (exp, state, levels, eff, body_valuable) =
	  rexp (body, env, state)

(*	  val (env, state, bndlevel) = bumpCurrentlevel (env, state)
	  val (env, state, bnds_valuable) = rbnds(bnds, env, state)
	  val (exp, state, levels, eff, body_valuable) =
	      rexp_limited bndlevel (body, env, state)*)
	  val valuable = bnds_valuable andalso body_valuable
      in
	  (exp, state, levels, eff, valuable)
      end

    | rexp' (exp as Prim_e (prim,trs,cons,exps), env, state) =
      let
	val phase = if NilDefs.allprim_uses_carg prim then Runtime else Compiletime
	val (cons, state, levels1) = rtcs phase (cons, env, state)
	val (exps, state, levels2, eff_list, args_valuable) =
	  rexps(exps, env, state)

	val levels3 = List.mapPartial (traceLevel env) trs

	(* we don't try very hard to track effect types through primops. *)
	  val eff =
	    (case (prim, eff_list) of
	       (NilPrimOp (record lbls),  tageff::eff_list) =>
		 REC_EFF(ListPair.zip (lbls, eff_list))
	     | (NilPrimOp (select lbl), [eff]) =>
		 ereclookup (eff, lbl)
	     | _ => UNKNOWN_EFF)

	  val levels = mergeMultiLevels[levels1, levels2,levels3]

	  val valuable = (not (NilDefs.anyEffect exp)) andalso args_valuable
      in
	  (Prim_e (prim, trs,cons, exps), state, levels, eff, valuable)
      end

    | rexp' (Switch_e sw, env, state) =
      let
	  val (sw, state, levels, effs, valuable) =
	      rswitch (sw, env, state)
      in
	  (Switch_e sw, state, levels, effs, valuable)
      end

    | rexp' (App_e (openness, exp, cons, exps1, exps2), env, state) =
      let
	  val (exp, state, levels1, exp_effs, valuable1) =
	      rexp (exp, env, state)
	  val (cons, state, levels2) =
              rcons (cons, env, state)
	  val (exps1, state, levels3, _, valuable3) =
	      rexps(exps1, env, state)
	  val (exps2, state, levels4, _, valuable4) =
	      rexps(exps2, env, state)

	  val (valuable, effs) =
	      (case exp_effs of
		   (ARROW_EFF (Total,rest)) =>
		       (valuable1 andalso valuable3 andalso valuable4, rest)
		 | (ARROW_EFF (Partial,rest)) => (false, rest)
		 | _ => (false, UNKNOWN_EFF))

	  val levels = mergeMultiLevels[levels1,levels2,levels3,levels4]

      in
	  (App_e (openness, exp, cons, exps1, exps2),
	   state, levels, effs, valuable)
      end

    | rexp' (ExternApp_e (exp,exps), env, state) =
      let
	  val (exp, state, levels1, _, valuable1) = rexp (exp, env, state)
	  val (exps, state, levels2, _, valuable2) = rexps(exps, env, state)
	  val levels = mergeLevels(levels1,levels2)
	  val effs = UNKNOWN_EFF
	  val valuable = false
      in
	(ExternApp_e (exp,exps), state, levels, effs, valuable)
      end

    | rexp' (Raise_e(exp,con), env, state) =
      let
	  val (exp, state, levels1, _, _) = rexp(exp, env, state)
	  val (con, state, levels2) = rtype(con, env, state)
	  val levels = mergeLevels(levels1,levels2)
	  val effs = UNKNOWN_EFF
	  val valuable = false
      in
	  (Raise_e(exp,con), state, levels, effs, valuable)
      end

    | rexp' (Handle_e {body,bound,handler,result_type}, env, state) =
      let
	(* We must bump the current level and limit on the new level
	 * to prevent non-valuable bindings from being hoisted out of
	 * the scope of the handler.
	 *)
	val (body_env,state,body_level)   =  bumpCurrentlevel (env, state)
	val (body, state, levels1, _, _)  =  rexp_limited body_level (body, body_env, state)

	val (result_type, state, levels2) =  rtype (result_type, env, state)

	val (inner_env, state, handlerlevel) = bumpCurrentlevel (env, state)
	val inner_env = bindLevel(inner_env, bound, handlerlevel)
	val inner_env = bindEff(inner_env, bound, UNKNOWN_EFF)

	val limitlevel = lastFnLevel env
	val (handler, handler_leftover_state, levels3, _, _) =
	  rexp_limited limitlevel (handler, inner_env, empty_state)
	val state = mergeStates (state, handler_leftover_state)

	val levels = mergeMultiLevels[levels1, levels2, levels3]
	val eff = UNKNOWN_EFF
	val valuable = false
      in
	(Handle_e {body = body, bound = bound,
		   handler = handler, result_type = result_type},
	 state, levels, eff, valuable)
      end
    | rexp' (ForgetKnown_e (sumcon,field), env, state) =
      let
	val (sumcon,state,levels) = rtype(sumcon,env,state)
	val valuable = true
	val eff = UNKNOWN_EFF
      in 
	(ForgetKnown_e (sumcon,field),state,levels,eff,valuable)
      end

    | rexp' (Fold_e (vars,from,to), env, state) =
      let
	  val (inner_env,state,ccnlevel) = bumpCurrentlevel (env,state)
	  fun folder (v,e) = bindLevel (e,v,ccnlevel)
	  val inner_env = foldl folder inner_env vars
	  val (from,state,levels1) =
	      rtype_limited ccnlevel (from,inner_env,state)
	  val (to,state,levels2) =
	      rtype_limited ccnlevel (to,inner_env,state)
	  val levels = mergeLevels (levels1,levels2)
	  val valuable = true
	  val eff = UNKNOWN_EFF
      in
	  (Fold_e (vars,from,to),state,levels,eff,valuable)
      end
    | rexp' (Unfold_e (vars,from,to), env, state) =
      let
	  val (inner_env,state,ccnlevel) = bumpCurrentlevel (env,state)
	  fun folder (v,e) = bindLevel (e,v,ccnlevel)
	  val inner_env = foldl folder inner_env vars
	  val (from,state,levels1) =
	      rtype_limited ccnlevel (from,inner_env,state)
	  val (to,state,levels2) =
	      rtype_limited ccnlevel (to,inner_env,state)
	  val levels = mergeLevels (levels1,levels2)
	  val valuable = true
	  val eff = UNKNOWN_EFF
      in
	  (Unfold_e (vars,from,to),state,levels,eff,valuable)
      end
    | rexp' (Coerce_e (coercion,cargs,exp), env, state) =
      let
	  val (coercion,state,levels1,effs1,valuable1) =
	      rexp (coercion,env,state)
	  val (cargs,state,levels2) = rtypes (cargs,env,state)
	  val (exp,state,levels3,effs3,valuable3) = rexp (exp,env,state)
	  val levels = mergeMultiLevels[levels1, levels2,levels3]
	  val valuable = valuable1 andalso valuable3
	  val eff = UNKNOWN_EFF
      in
	  (Coerce_e (coercion,cargs,exp),state,levels,eff,valuable)
      end

  (* Translation for an expression just under a binder, such that bindings hoisted to this level are included *)
  and rexp_limited levnum (exp, env, state) =
      let
	  val (exp, state, levels, eff, valuable) = rexp (exp, env, state)
	  val (exp, state, levels, valuable) =
	      limitExp (levnum, exp, valuable, env, state, levels)
      in
	  (exp, state, levels, eff, valuable)
      end

  and rexps' (limitopt, exps, env, state) =
      let
	  fun loop ([], rev_accum, state, levelslist, rev_eff_list, valuable) =
	      (rev rev_accum, state, mergeMultiLevels levelslist, rev rev_eff_list, valuable)
	    | loop (exp::exps,rev_accum,state,levelslist,rev_eff_list,valuable) =
	      let
		  val (exp, state, new_levels, new_eff, new_valuable) =
		      rexp (exp, env, state)
		  val (exp, state, new_levels, new_valuable) =
		      (case limitopt of
			   NONE => (exp, state, new_levels, new_valuable)
			 | SOME limit =>
			       limitExp(limit, exp, new_valuable,
					env, state, new_levels))

	      in
		  loop(exps, exp::rev_accum, state,
		       new_levels::levelslist,
		       new_eff :: rev_eff_list,
		       valuable andalso new_valuable)
	      end
      in
	  loop(exps, [], state, [], [], true)
      end

  and rexps (exps, state, env) = rexps' (NONE, exps, state, env)

  (* Translation for expressions such that bindings hoisted to this level are included.
   * Used in translating switches. *)
  and rexps_limited limit (exps, state, env) =
      rexps' (SOME limit, exps, state, env)

  and rbnd (Con_b(p,cb), env, state) =
      let
	  val (env, state) = rcbnd p (cb, env, state)
	  val valuable = true
      in
	  (env, state, valuable)
      end

    | rbnd (Exp_b(v,nt,e), env, state) =
      let
	  val (e, state, levels, effs, valuable) = rexp (e, env, state)

	  val levels = mergeLevels (traceLevels env nt,levels)

	  val newbnd = Exp_b(v,nt,e)

          (* We can only hoist valuable bindings to higher levels.
             If it is not valuable, we "hoist" to the current level
             (i.e., don't move the binding).  This is accomplished
             by stipulating that the binding is dependent on a
             binding at the current level.
           *)
	  val levels =
	      if valuable then
		  levels
	      else
		  mergeLevels ([currentLevel env], levels)

	  val hoistlevel = deepestLevel levels

	  val env = bindLevel(env, v, hoistlevel)
	  val env = bindEff(env, v, effs)

	  val state = hoistBnd (state, levels, newbnd, valuable)
      in
	  (env, state, valuable)
      end

    | rbnd (Fixopen_b vfseq, env : env, state : state) =
	let
	    val vfs = Sequence.toList vfseq
	    val (pairs, functions) = Listops.unzip vfs
	    val (vars, cons) = Listops.unzip pairs

	    (* Inside the recursion we cannot treat these functions
               as total, even if their bodies are lambdas.  Otherwise
               we might hoist applications of these functions to the
               top of the function body, introducing an infinite loop.

               E.g., turning

                    f = /\t. \x:t.  f[t](x)

               into

                    f = /\t. let f' = f[t] in \x:t.  f'(x)

               would be bad because the function would go into
               an infinite loop as soon as it was instantiated,
               rather than once an argument is applied.  Similar
               problems can occur with mutually-recursive
               polymorphic-recursive functions.
             *)
	    val partial_effs = map (fn _ => UNKNOWN_EFF) functions
            val env = bindsEff(env, vars, partial_effs)

	    (* We temporarily assign the functions in this nest
               to a new level.  This prevents references to
               the function from being hoisted out of the function.
               Further, we need to be able to distinguish recursive
               references (which do not prevent these functions from
               being hoisted) from references to prior definitions
               in a sequence of bindings. We re-assign levels
               to the functions defined here after we know
	       where the recursive nest is going. *)
            val (inner_env, state, fixopenlevel) =
		bumpCurrentlevel (env, state)
            val inner_env = enterFunction inner_env
	    val inner_env = bindsLevel (inner_env, vars, fixopenlevel)

	    fun loop ([], rev_fns, rev_fn_effs, state, levelslist) =
		(rev rev_fns, rev rev_fn_effs, state, mergeMultiLevels levelslist)
	      | loop (((v,c),f)::rest, rev_fns, rev_fn_effs, state, levelslist) =
		let
		    val (c, f, eff, state, new_levels) =
			rfun ((v, c), f, inner_env, state)
		in
		    loop (rest, ((v, c),f)::rev_fns, eff :: rev_fn_effs,
			  state, new_levels::levelslist)
		end

	    val (vfs, function_effs, state, levels) =
		loop (vfs, [], [], state, [])

	    val hoistlevel = deepestLevel levels

	    (* Fix effects *)
            val env = bindsEff(env, vars, function_effs)

	    val env = bindsLevel (env, vars, hoistlevel)
	    val newbnd = Fixopen_b(Sequence.fromList vfs)
	    val state = hoistBnd (state, levels, newbnd, true)

	    val valuable = true
	in
	    (env, state, valuable)
	end

    | rbnd (Fixcode_b _, _, _) = error "rbnd: found fixcode"
    | rbnd (Fixclosure_b _, _, _) = error "rbnd: found fixclosure"

  and rbnds ([], env, state) = (env, state, true)
    | rbnds (Exp_b(v,nt,Let_e(Sequential,bnds,body))::rest, env, state) =
         (* A bit of on-the-fly flattening. Doesn't happen automatically
            for non-valuable bindings because they're never hoisted. *)
         rbnds (bnds @ [Exp_b(v,nt,body)] @ rest, env, state)
    | rbnds (bnd::bnds, env, state) =
      let
	  val (env, state, bnd_valuable) = rbnd(bnd, env, state)
	  val (env, state, rest_valuable) = rbnds(bnds, env, state)
	  val valuable = rest_valuable andalso bnd_valuable
      in
	  (env, state, valuable)
      end

  and rswitch (Intsw_e{arg,size,arms,default,result_type}, env, state) =
      let
	  val (arg, state, levels1, _, _) = rexp(arg, env, state)

	  val (ints, exps) = Listops.unzip arms
          val limitlevel = lastFnLevel env
	  (* We do not need to bump the current level, since we do not
	   * bind a variable.  We will still discharge any non-valuable
	   * bindings, since we limit to the last function level,
	   * which is guaranteed to include the current level
	   *)
	  val (exps, arms_leftover_state, levels2, _, _) =
		  rexps_limited limitlevel (exps, env, empty_state)
	  val arms = ListPair.zip(ints, exps)

          val state = mergeStates(state, arms_leftover_state)

	  val (default, default_leftover_state, levels3, _, _) =
		  rexpopt_limited limitlevel (default,env,empty_state)
          val state = mergeStates(state, default_leftover_state)

	  val (result_type, state, levels4) = rtype(result_type, env, state)

	  val levels = mergeMultiLevels[levels1, levels2,levels3, levels4]

	  val effs = con2eff result_type
	  val valuable = false
      in
	  (Intsw_e {arg=arg, size=size, arms=arms, default=default,
		    result_type = result_type},
	   state, levels, effs, valuable)
      end

    | rswitch (Sumsw_e {arg,sumtype,bound,arms,default,result_type},
	       env, state) =
      let

	  val (arg, state, levels1, _, _) = rexp(arg, env, state)

          val limitlevel = lastFnLevel env

	  val (inner_env, state, armlevel) = bumpCurrentlevel (env, state)
	  val inner_env = bindLevel(inner_env, bound, armlevel)
          val inner_env = bindEff(inner_env, bound, UNKNOWN_EFF)

	  (*Known sums are always Trace, so no need to look at nts
	   *)
	  val (tags, nts, exps) = Listops.unzip3 arms

	  val (exps, arms_leftover_state, levels2, _, _) =
		  rexps_limited limitlevel (exps, inner_env, empty_state)
	  val arms = Listops.zip3 tags nts exps
          val state = mergeStates(state, arms_leftover_state)

	  val (default, default_leftover_state, levels3, _, _) =
		  rexpopt_limited limitlevel (default, env, empty_state)
          val state = mergeStates(state, default_leftover_state)

	  val (result_type, state, levels4) = rtype(result_type, env, state)

	  val levels = mergeMultiLevels[levels1, levels2,levels3, levels4]

	  val effs = con2eff result_type
	  val valuable = false
      in
	  (Sumsw_e {arg = arg,
		    sumtype = sumtype,
		    bound = bound,
		    arms = arms,
		    default = default,
		    result_type = result_type},
	   state, levels, effs, valuable)
      end

    | rswitch (Exncase_e {arg,bound,arms,default,result_type},
	       env, state) =
      let
	  val (arg, state, levels0, _, _) = rexp(arg, env, state)

	  val (tags, nts, exps) = Listops.unzip3 arms

	  val levels1 = List.mapPartial (traceLevel env) nts

          (* CS: Not all tags are variables.  It's not guaranteed
                 that hoisting from the tags is a good idea.  Maybe
                 they also should be required to be hoisted out
                 of the enclosing function? *)
	  val (tags, state, levels2, _, _) = rexps(tags, env, state)


	  val (inner_env, state, armlevel) = bumpCurrentlevel (env, state)
	  val inner_env = bindLevel(inner_env, bound, armlevel)
          val inner_env = bindEff(inner_env, bound, UNKNOWN_EFF)

	  val limitlevel = lastFnLevel inner_env
	  val (exps, arms_leftover_state, levels3, _, _) =
		  rexps_limited limitlevel (exps, inner_env, empty_state)
          val state = mergeStates(state, arms_leftover_state)

	  val arms = Listops.zip3 tags nts exps

	  val (default, default_leftover_state, levels4, _, _) =
		  rexpopt_limited limitlevel (default, env, empty_state)
          val state = mergeStates(state, default_leftover_state)

	  val (result_type, state, levels5) = rtype(result_type, env, state)

	  val levels = mergeMultiLevels[levels0,levels1, levels2,levels3,levels4, levels5]

	  val effs = con2eff result_type
	  val valuable = false
      in
	  (Exncase_e {arg         = arg,
		      bound       = bound,
		      arms        = arms,
		      default     = default,
		      result_type = result_type},
	   state, levels, effs, valuable)
    end

    | rswitch (Typecase_e _, _, _) =
       (error "rswitch: Typecase_e not implemented yet")
    | rswitch (Ifthenelse_e _, _, _) = error "Ifthenelse not implemented"

  and rexpopt' (_, NONE, env, state) = (NONE, state, emptyLevels,
					UNKNOWN_EFF, true)
    | rexpopt' (limitopt, SOME e, env, state) =
      let
	  val (e, state, levels, effs, valuable) = rexp (e, env, state)
	  val (e, state, levels, valuable) =
	      (case limitopt of
		   NONE => (e, state, levels, true)
		 | SOME limit => limitExp(limit, e, valuable,
					  env, state, levels))
      in
	  (SOME e, state, levels, effs, valuable)
      end

  and rexpopt (expopt, env, state) = rexpopt' (NONE, expopt, env, state)

  and rexpopt_limited limit (expopt, env, state) =
      rexpopt'(SOME limit, expopt, env, state)

  and rfun ((fnvar, con),
	    Function{recursive,tFormals,eFormals,
		     fFormals, body, ...},
	    env, state) =
      let
	  val {openness, effect, tFormals=tFa, eFormals=eFa, fFormals=fFa, body_type, ...} = stripArrow (env, con)

	  (* rfun is only called to process a part of a FixOpen_b.
	     Therefore, the current level has already been bumped. *)
	  val arglevel = currentLevel env

          val env =
                  rtFormals(tFormals, tFa, env, arglevel)

          val (eFormals, env, state, levels1) =
                  reFormals(eFormals, env, state, arglevel)

	  val env = bindsLevel(env, fFormals, arglevel)

	  (* Since we bumped the level of the function variables
	   * and the arguments, this call will remove the function
	   * variable levels from the list of levels, which allows
	   * recursive functions to be hoisted.
	   *)
	  val (body, state, levels2, body_eff, body_valuable) =
                  rexp_limited arglevel (body, env, state)

	  val (con, effect, env, state) =
	      case effect of
                  Total => (con, Total, env, state)
		| Partial =>
		      if body_valuable andalso !infer_totality then
			  let
			      (* We must create a new variable binding for a Total version of this function's type *)
			      val cv = Name.fresh_named_var (Name.var2string fnvar ^ "_type")
			      val newarrow = AllArrow_c {openness = openness,
							 effect = Total,
							 tFormals = tFa,
							 eFormals = eFa,
							 fFormals = fFa,
							 body_type = body_type}
			      val newarrow = NilRename.renameCon newarrow
			      val newbnd = Con_cb (cv, newarrow)

			      val (_, _, levels) = rtype (con, env, state)

			      val env = bindLevel (env, cv, deepestLevel levels)
			      val state = hoistCbnd Compiletime (state, levels, newbnd)

			      (*val con' = Let_c (Sequential, [Con_cb (cv,
								  AllArrow_c {openness = openness,
									      effect = Total,
									      tFormals = tFa,
									      eFormals = eFa,
									      fFormals = fFa,
									      body_type = body_type})], Var_c cv)
			      val con' = NilRename.renameCon con'*)
			  in
			      (Var_c cv, Total, env, state)
			  end
		      else
			  (con, Partial, env, state)

	  val (con, state, levels3) = rtype (con, env, state)

	  val levels = mergeMultiLevels[levels1,levels2,levels3]

	  (* The effect annotation on the function may have been
	     overly conservative, saying Partial where the function
             is really Total.  The effect is always sound, so if it
	     said Total it's still going to be Total. (Hoisting ought
	     not change valuability).  However, our estimation of
	     valuability is also sound, so if it says the body is
	     valuable then this is also correct regardless of the
	     original annotation. *)
	  (*val effect = (case effect of
			    Total => Total
			  | Partial => if body_valuable then
				          (if (!debug)
					       then (print "Making function ";
						     Ppnil.pp_var fnvar;
						     print " Total\n")
					   else ();
					   Total)
				       else Partial)*)
(*
	  val _ = (print "Function ";
		   Ppnil.pp_var fnvar;
		   print " claims to have body levels ";
		   app (print o Int.toString) levels)
*)
      in
	  (con, Function{effect = effect, recursive = recursive,
			 tFormals = tFormals,
			 eFormals = eFormals, fFormals = fFormals,
			 body = body},
	   ARROW_EFF(effect, body_eff),
	   state, levels)
      end

  (* Translates kinds of var/kind pairs and binds levels for the variables *)
  and rtFormals (tFormals, tFa, env, arglevel) =
      let
	  fun loop ([], [], env) =
	      env
            | loop (v::rest, (_, k)::rest', env) =
	      let
		  val env = bindLevel(env, v, arglevel)
		  val env = insertKind(env, v, k)
	      in
		  loop(rest, rest', env)
	      end
	    | loop _ = raise Fail "tFormals length mismatch in function vs. its type annotation"
      in
	  loop(tFormals, tFa, env)
      end

  and rtFormals_arrow (tFormals, env, state, arglevel) =
      let
	  fun loop ([], rev_accum, env, state, levelslist) =
	      (rev rev_accum, env, state, mergeMultiLevels levelslist)
            | loop ((v,k)::rest, rev_accum, env, state, levelslist) =
	      let
		  val (k, state, new_levels) =
                         rkind_limited arglevel (k, env, state)
		  val env = bindLevel(env, v, arglevel)
		  val env = insertKind(env, v, k)
	      in
		  loop(rest, (v,k)::rev_accum, env, state, new_levels::levelslist)
	      end
      in
	  loop(tFormals, [], env, state, [])
      end

  (* Translates cons of var/con pairs and binds levels/eff's for the variables *)
  and reFormals (eFormals, env, state, arglevel) =
      let
	  fun loop ([], rev_accum, env, state, levelslist) =
	      (rev rev_accum, env, state, mergeMultiLevels levelslist)
            | loop ((v,nt)::rest, rev_accum, env, state, levelslist) =
	      let
		  val tr_levels = traceLevels env nt
		  val env = bindLevel(env, v, arglevel)

		  (* The unhoisted type is likely to contain more
                     local information about partial/total arrows *)
		  val env = bindEff(env, v, UNKNOWN_EFF)

	      in
		  loop(rest, (v,nt)::rev_accum, env, state, tr_levels::levelslist)
	      end
      in
	  loop(eFormals, [], env, state, [])
      end

  (* Version of the above for using with arrow constructors, taking optional named term parameters into account *)
  and reFormals_arrow (eFormals, env, state, arglevel) =
      let
	  fun loop ([], rev_accum, env, state, levelslist) =
	      (rev rev_accum, env, state, mergeMultiLevels levelslist)
            | loop (c_orig::rest, rev_accum, env, state, levelslist) =
	      let
		  val (c, state, new_levels) =
                        rtype_limited arglevel (c_orig, env, state)
	      in
		  loop(rest, c::rev_accum, env, state, new_levels::levelslist)
	      end
      in
	  loop(eFormals, [], env, state, [])
      end


  and rkind_limited _ (Type_k, env : env, state : state) =
         (Type_k, state, emptyLevels)
    | rkind_limited limitlevel (Single_k c, env, state) =
      let
	  val (c, state, levels) = rtype_limited limitlevel (c, env, state)
      in
	  (Single_k c, state, levels)
      end
    | rkind_limited limitlevel (SingleType_k c, env, state) =
      let
	  val (c, state, levels) = rtype_limited limitlevel (c, env, state)
      in
	  (SingleType_k c, state, levels)
      end
    | rkind_limited limitlevel (Arrow_k(openness, tFormals, kind),env,state) =
      let
          (* Don't bump the level because there's no
             place to put bindings here *)
	  val (tFormals, env, state, levels1) =
	         rtFormals_arrow(tFormals, env, state, limitlevel)

	  val (kind, state, levels2) =
                 rkind_limited limitlevel (kind, env, state)

	  val levels = mergeLevels(levels1, levels2)
      in
	  (Arrow_k(openness, tFormals, kind), state, levels)
      end

    | rkind_limited limitlevel (Record_k lvk_seq, env, state) =
      let
	  val (lvlist, kinds) = Listops.unzip (Sequence.toList lvk_seq)
	  val (labels, vars) = Listops.unzip lvlist
	  val tFormals = Listops.zip vars kinds

	  val (tFormals, _, state, levels) =
	      rtFormals_arrow(tFormals, env, state, limitlevel)

	  val kinds = map (#2) tFormals
	  val lvk_seq = Sequence.fromList (ListPair.zip (lvlist, kinds))
      in
	  (Record_k lvk_seq, state, levels)
      end

  fun optimize (MODULE {bnds, imports, exports}) =
      let
	  (* mark imports as top-level variables;
             estimate totality of term-level imports *)

	  fun split ([], env, imps) = (env, rev imps)
	    | split ((imp as ImportValue(l,v,_,c))::rest, env, imps) =
	      let
		  val env = bindLevel(env, v, toplevel)
		  val env = bindEff(env, v, con2eff c)
	      in
		  split(rest, env, imp :: imps)
	      end
	    | split ((imp as ImportType(l,v,k))::rest, env, imps) =
	      let
		  val env = insertKind(env, v, k)
		  val env = insertLabel(env, l, v)
		  val env = bindLevel(env, v, toplevel)
	      in
		  split(rest, env, imp :: imps)
	      end
	    | split (ImportBnd (phase, cb)::rest, env, imps) =
	      let
		  (*val (inner_env, state, inner_level) =
		      bumpCurrentlevel (env, empty_state)*)

		val (env, state) =
		  rcbnd phase (cb, env, empty_state)
		  
		val (bnds, _, _) = extractCbnds (state, toplevel)
		val ibnds = map ImportBnd bnds
	      in
		split(rest, env, List.revAppend(ibnds, imps))
	      end
	    
	  val (initial_env, imports) = split (imports, empty_env, [])
	  val initial_state = empty_state

          val _ = msg "  Imports processed\n"

	  val (_, final_state, _) =
	      rbnds(bnds, initial_env, initial_state)

	  val (bnds',_,_,_) = extractBnds(final_state, toplevel)
      in
	  MODULE {bnds=bnds',imports=imports,exports=exports}
      end

end
