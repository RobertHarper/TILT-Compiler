(* Reorder term-level bindings to allow operations that require memory allocation to be coalesced *)

(* This phase is not currently being used *)

structure Reorder =
struct
    open Prim Nil
    structure VS = Name.VarSet

    fun error s = Util.error "reorder.sml" s


    fun freeVarInExp exp =
	let
	    val (set1, set2) = NilUtil.freeExpConVarInExp(true, 0, exp)
	in
	VS.union (set1,set2)
    end

    fun freeVarInCon c =
    let
	val (set1,set2) = NilUtil.freeExpConVarInCon(true, 0, c)
    in
	VS.union (set1,set2)
    end

    fun freeVarInKind k = NilUtil.freeVarInKind (0, k)

    (************************************************************************
     Flags:
      debug                : Print debugging information during hoisting
     ************************************************************************)
    val debug = Stats.ff("ReorderDebug")

    (************************************************************************
     Binding categories
        APPLY    : Does application (blocks GC check hoisting)
        ALLOCATE : Does an allocation (requires GC check)
        OTHER    : Neither blocks nor requires a GC check

        These form a lattice where OTHER < ALLOCATE < APPLY.
        The mergeCategories function performs the binary join on this
        lattice.
     ************************************************************************)

    datatype category = APPLY | ALLOCATE | OTHER

    fun mergeCategories (APPLY, _) = APPLY
      | mergeCategories (_, APPLY) = APPLY
      | mergeCategories (OTHER, OTHER) = OTHER
      | mergeCategories _ = ALLOCATE


    (************************************************************************
     Movability

       Movability terms are either valuable or have only an
       (observable) allocation side-effect (such as array creation).
       It is unsafe to coalesce two equal movable terms, but there is
       no problem with reordering them.  Movability forms a lattice
       with Movable < Immovable.  Constructors, of course, are always
       considered Movable.
     ************************************************************************)

    datatype movability = Movable | Immovable

    fun mergeMovabilities (Movable, Movable) = Movable
      | mergeMovabilities _ = Immovable

    (************************************************************************
     The information maintained by the reordering routines is kept in
     an environment.  Right now this environment contains (is) only a
     set of "interesting" variables.

     Interesting variables:
        To shrink the size of our variable sets and reduce
        computations on sets, we keep track of those variables bound
        at term-level bindings.  These are the only interesting
        variables for the purposes of the analysis.
     ************************************************************************)

    type env = VS.set

    val empty_env = VS.empty
    fun interestingVar (env, v) = VS.member(env, v)
    fun makeInteresting (env, vs) = VS.addList(env, vs)

    (************************************************************************
     Statistics counting

     The whole point of this optimization is to reduce the number of
     alternations between ALLOCATE and APPLY bindings.  So we count
     this statistic before and after.
     ************************************************************************)

     local
	 val old_count = ref 0
	 val new_count = ref 0
     in
	 fun clearStats () = (old_count := 0; new_count := 0)
	 fun declareAlternations{old, new} =
	     (print "old = ";
	      print (Int.toString old);
	      print " and new = ";
	      print (Int.toString new);
	      print "\n";
	      old_count := (!old_count) + old;
	      new_count := (!new_count) + new)
	 fun printStats () =
	     (print "Started with ";
	      print (Int.toString (! old_count));
	      print " alternations, of which ";
	      print (Int.toString (! new_count));
	      print " remain.\n")
     end


    (************************************************************************
     Reordering routines.

     val rexp   : exp      * env -> exp      * VS.set * category * movability
     val rexps  : exp list * env -> exp list * VS.set * category * movability

     val rcon   : con      * env -> con      * VS.set * category
     val rcons  : con list * env -> con list * VS.set * category

     val rbnds'  : bnd list * env ->
	            {immovable : (bnd * VS.set * category) list,
		     apply       : (bnd * VS.set) list, (* unordered *)
		     allocates   : (bnd * VS.set) list, (* unordered *)
                     others      : (bnd * VS.set) list, (* unordered *)
		     boundvars   : VS.set}
     ************************************************************************)

    fun rexp (exp as Var_e var, env) =
	let
	    val vset = if interestingVar(env, var) then
		          VS.singleton var
		       else
			  VS.empty
	in
	    (exp, vset, OTHER, Movable)
	end
      | rexp (exp as Const_e _, env) =
	let
	    (* XXX Assumes A-normal form *)
	    val vset = VS.intersection(freeVarInExp exp, env)
	in
	    (exp, vset, OTHER, Movable)
	end
      | rexp (Let_e(letsort, bnds, body), env) =
	  let
	      val (bnds, bnds_vset, bnds_category, bnds_movability) =
		     rbnds (bnds, env)
	      val (body, body_vset, body_category, body_movability) =
		     rexp (body, env)
	  in
	      (Let_e(letsort, bnds, body),
	       VS.union (bnds_vset, body_vset),
	       mergeCategories (bnds_category, body_category),
	       mergeMovabilities (bnds_movability, body_movability))
	  end
      | rexp (exp as Prim_e(prim, trs, cons, exps), env) =
	  let
	      val (prim_category,prim_movability) =
		  (case prim of
		     (* The empty record is represeted with an integer,
		        rather than by allocation *)
                     NilPrimOp (record []) => (OTHER, Movable)
		   | NilPrimOp (record _) => (ALLOCATE, Movable)
		   | NilPrimOp (partialRecord _) =>
			 (print "Warning: are partialRecord's movable?\n";
			  (ALLOCATE, Movable))
                   | NilPrimOp (inject _) =>  (ALLOCATE, Movable)
                   | NilPrimOp (inject_known _) => (ALLOCATE, Movable)
                   | NilPrimOp (inject_known_record _) => (ALLOCATE, Movable)
                   | NilPrimOp (box_float _) => (ALLOCATE, Movable)
		   | NilPrimOp (inj_exn _) => (ALLOCATE, Movable)
                     (* The following two primitives are big and so
                        not inlined.  *)
		   | NilPrimOp (make_vararg _) => (APPLY, Movable)
		   | NilPrimOp (make_onearg _) => (APPLY, Movable)

		   | NilPrimOp mk_record_gctag   => (OTHER, Movable)
		   | NilPrimOp mksum_known_gctag => (OTHER, Movable)

                     (* We don't inline the typecasing code for polymorphic
                        array operations; they're turned into function calls *)
		   | PrimOp (sub (OtherArray false)) => (APPLY, Immovable)
		   | PrimOp (sub (OtherVector false)) => (APPLY, Immovable)
		   | PrimOp (update (OtherArray false)) => (APPLY, Immovable)
		   | PrimOp (update (OtherVector false)) => (APPLY, Immovable)
		   | PrimOp (create_table (OtherArray false)) =>
                                                            (APPLY, Movable)
		   | PrimOp (create_table (OtherVector false)) =>
                                                            (APPLY, Movable)
		   | PrimOp (create_empty_table (OtherArray false)) =>
                                                            (APPLY, Movable)
		   | PrimOp (create_empty_table (OtherVector false)) =>
                                                            (APPLY, Movable)
		   | PrimOp (length_table (OtherArray false)) =>
                                                            (APPLY, Movable)
		   | PrimOp (length_table (OtherVector false)) =>
                                                            (APPLY, Movable)
		   | PrimOp (equal_table (OtherArray false)) =>
                                                            (APPLY, Movable)
		   | PrimOp (equal_table (OtherVector false)) =>
                                                            (APPLY, Movable)
		     (* The following two lines handle updates to arrays
                        of pointers.  Such updates modify the garbage
                        collector's write list, which requires memory
                        allocation for the new entry *)
		   | PrimOp (update (OtherArray true)) => (ALLOCATE, Immovable)
		   | PrimOp (update (OtherVector true))=> (ALLOCATE, Immovable)
                     (* All other primitives neither allocate nor block
                        hoisting of GC checks.  They may or may not
                        be movable, which is approximated as valuable *)
		   | _ => (OTHER,
                           if NilDefs.anyEffect exp then Immovable else Movable))

	      val (cons, cons_vset, cons_category) =
		     rcons (cons, env)
	      val (exps, exps_vset, exps_category, exps_movability) =
		     rexps (exps, env)

	      val exp        = Prim_e(prim, trs,cons, exps)
	      val vset       = VS.union(cons_vset, exps_vset)
	      val category   = mergeCategories
		                (mergeCategories(prim_category, cons_category),
				 exps_category)
              val movability = mergeMovabilities(prim_movability,
						 exps_movability)
	  in
	      (exp, vset, category, movability)
	  end
      | rexp (Switch_e sw, env) = rswitch (sw, env)
      | rexp (App_e(openness,f,cons,exps,fexps), env) =
	  let
	      val (f, f_vset, _, f_movability) = rexp (f, env)
	      val (cons, cons_vset, _) = rcons (cons, env)
	      val (exps, exps_vset, _, exps_movability) = rexps(exps, env)
	      val (fexps, fexps_vset, _, fexps_movability) = rexps(fexps, env)

	      val exp = App_e(openness, f, cons, exps, fexps)
	      val vset = VS.union(VS.union(f_vset, cons_vset),
				  VS.union(exps_vset, fexps_vset))
	      val category =  APPLY

	      (* XXX We do not have totality information at this time *)
(*
	      val movability = mergeMovabilities
		                (mergeMovabilities(f_movability,
						   exps_movability),
				 fexps_movability)
*)
	      val movability = Immovable
	  in
	      (exp, vset, category, movability)
	  end

      | rexp (ExternApp_e (f, exps), env) =
	  let
	      val (f, f_vset, f_category, f_movability) =
		     rexp (f, env)
	      val (exps, exps_vset, exps_category, exps_movability) =
		     rexps (exps, env)

	      val exp = ExternApp_e(f, exps)
	      val vset = VS.union(f_vset, exps_vset)

	      val movability = mergeMovabilities(f_movability,
						 exps_movability)
	  in
	      (exp, vset, APPLY, movability)
	  end

      | rexp (Raise_e (exp, con), env) =
	  let
	      val con_vset = VS.intersection(freeVarInCon con, env)

	      val (exp, exp_vset, exp_category, _) = rexp (exp, env)
	  in
	      (Raise_e (exp, con), exp_vset, exp_category, Immovable)
	  end

      | rexp (Handle_e{body, bound, handler, result_type}, env) =
	  let
	      val (body, body_vset, body_category, body_movability) =
		     rexp (body, env)
	      val (handler, handler_vset, handler_category,
		   handler_movability) = rexp (handler, env)

	      (* The result type is not evaluated at run-time,
	         so we don't care whether it includes an application
		 or not *)
	      val (result_type, result_type_vset, _) = rcon(result_type, env)

	      val exp = Handle_e{body = body, bound = bound,
				 handler = handler,
				 result_type = result_type}
	      val vset = VS.union(VS.union(body_vset, handler_vset),
				  result_type_vset)
	      val category = mergeCategories(body_category, handler_category)
	      val movability = mergeMovabilities(body_movability,
						 handler_movability)
	  in
	      (exp, vset, category, movability)
	  end

    and rexps ([], _) = ([], VS.empty, OTHER, Movable)
      | rexps (exp::exps, env) =
	let
	    val (exp, exp_vset, exp_category, exp_movability) =
		  rexp (exp, env)
	    val (exps, exps_vset, exps_category, exps_movability) =
		  rexps (exps, env)
	in
	    (exp::exps,
	     VS.union(exp_vset, exps_vset),
	     mergeCategories(exp_category, exps_category),
	     mergeMovabilities(exp_movability, exps_movability))
	end

    and rexpopt (NONE, env) = (NONE, VS.empty, OTHER, Movable)
      | rexpopt (SOME exp, env) =
	let
	    val (exp, exp_vset, exp_category, exp_movability) =
		   rexp (exp, env)
	in
	    (SOME exp, exp_vset, exp_category, exp_movability)
	end

    and rcon (Let_c(letsort, cbnds, cbody), env) =
	let
	    val (cbnds, cbnds_vset, cbnds_category) = rcbnds (cbnds, env)
	    val (cbody, con_vset, cbody_category) = rcon (cbody, env)

	    val con = Let_c(letsort, cbnds, cbody)
	    val vset = VS.union(cbnds_vset, con_vset)
	    val category = mergeCategories(cbnds_category, cbody_category)
	in
	    (con, vset, category)
	end

      | rcon (Prim_c (prim, cons), env) =
	let
	    val (cons, cons_vset, cons_category) = rcons(cons, env)
	    val con = Prim_c(prim, cons)
	    val prim_category =
		(case prim of
		     Int_c _ => OTHER
		   | Float_c _ => OTHER
		   | BoxFloat_c _ => OTHER
		   | Exn_c => OTHER
                   | _ => ALLOCATE)

	    val category = mergeCategories (prim_category, cons_category)
	in
	    (con, cons_vset, category)
	end

      | rcon (con as Var_c var, env) =
	let
	    val vset = if interestingVar(env, var) then
		          VS.singleton var
		       else
			  VS.empty
	in
	    (con, vset, OTHER)
	end

      | rcon (Typeof_c exp, env) =
	let
	    val _ = print "WARNING: reorder/rcon found a Typeof_c\n"

	    val (exp, exp_vset, exp_category, _) =
		rexp (exp, env)
	in
	    (Typeof_c exp, exp_vset, exp_category)
	end

      | rcon (Crecord_c lclist, env) =
	let
	    val (labels, cons) = Listops.unzip lclist
	    val (cons, cons_vset, cons_category) =
		rcons (cons, env)

	    val vset = cons_vset
	    val category = mergeCategories(cons_category, ALLOCATE)
	in
	    (Crecord_c (Listops.zip labels cons),
	     vset, category)
	end

      | rcon (Proj_c (con, label), env) =
	let
	    val (con, con_vset, con_category) = rcon (con, env)
	in
	    (Proj_c(con,label), con_vset, con_category)
	end

      | rcon (Closure_c(con1, con2), env) =
	let
	    val (con1, con1_vset, con1_category) = rcon (con1, env)
	    val (con2, con2_vset, con2_category) = rcon (con2, env)

	    val vset = VS.union(con1_vset, con2_vset)
	    val category = mergeCategories
		            (mergeCategories(con1_category, con2_category),
			     ALLOCATE)
	in
	    (Closure_c(con1, con2), vset, category)
	end

      | rcon (App_c(con, cons), env) =
	let
	    val (con, con_vset, _) = rcon (con, env)
	    val (cons, cons_vset, _) = rcons (cons, env)

	    val vset = VS.union(con_vset, cons_vset)
	in
	    (App_c(con,cons), vset, APPLY)
	end

      | rcon (Typecase_c _, _) = error "rcon: Typecase_c unimplemented"

      | rcon (Annotate_c(annot, con), env) =
	let
	    val (con, vset, category) = rcon (con, env)
	in
	    (Annotate_c(annot, con), vset, category)
	end

      | rcon (con as AllArrow_c _, env) =
	let
	    val vset = VS.intersection(freeVarInCon con, env)
	    val category = ALLOCATE
	in
	    (con, vset, category)
	end

      | rcon (con as ExternArrow_c (arg_cons, result_con), env) =
	let
	    val vset = VS.intersection(freeVarInCon con, env)
	    val category = ALLOCATE
	in
	    (con, vset, category)
	end

      | rcon (con as Mu_c _, env) =
	let
	    val vset = VS.intersection(freeVarInCon con, env)
	    val category = ALLOCATE
	in
	    (con, vset, category)
	end

    and rcons ([], _) = ([], VS.empty, OTHER)
      | rcons (con::cons, env) =
	let
	    val (con, con_vset, con_category)    = rcon (con, env)
	    val (cons, cons_vset, cons_category) = rcons (cons, env)
	in
	    (con::cons,
	     VS.union(con_vset, cons_vset),
	     mergeCategories(con_category, cons_category))
	end

    and rswitch (Intsw_e{arg,size,result_type,arms,default}, env) =
	let
	    val (arg, arg_vset, arg_category, arg_movability) =
		   rexp (arg, env)
	    (* The result_type is not evaluated at run-time,
	       so we don't care if it's an allocation or not *)
	    val result_type_vset =
		   VS.intersection(freeVarInCon result_type, env)
	    fun loop [] = ([], VS.empty, OTHER, Movable)
              | loop ((w32, arm)::arms) =
		let
		    val (arm, arm_vset, arm_category, arm_movability) =
			rexp(arm, env)
		    val (arms, arms_vset, arms_category, arms_movability) =
			loop arms
		in
		    ((w32,arm) :: arms,
		     VS.union(arm_vset, arms_vset),
		     mergeCategories (arm_category, arms_category),
		     mergeMovabilities(arm_movability, arms_movability))
		end
	    val (arms, arms_vset, arms_category, arms_movability) =
			loop arms
	    val (default, default_vset, default_category, default_movability) =
		(case default of
		     (* if there's no default, the match might fail
		      and so this switch is potentially side-effecting *)
		     (* But if the elaborator and other parts of the compiler prevent that from being possible, then
		      * this code could needlessly block optimizations.
		      *)
		     NONE => (NONE, VS.empty, OTHER, Immovable)
		   | SOME arm => rexpopt (default, env))

	    val exp = Switch_e(Intsw_e{arg = arg, size = size,
				       result_type = result_type,
				       arms = arms, default = default})
	    val vset = VS.union(VS.union(arg_vset, result_type_vset),
				VS.union(arms_vset, default_vset))
	    val category = mergeCategories
		            (mergeCategories(arg_category, arms_category),
			     default_category)
	    val movability = mergeMovabilities
		             (mergeMovabilities(arg_movability,
						arms_movability),
			      default_movability)
	in
	    (exp, vset, category, movability)
	end

      | rswitch (Sumsw_e{arg,sumtype, result_type,
			 bound, arms, default}, env) =
	let
	    val (arg, arg_vset, arg_category, arg_movability) =
		   rexp (arg, env)
	    (* The sum_type is not evaluated at run-time,
	       so we don't care if it's an allocation or not *)
	    val sumtype_vset =
		   VS.intersection(freeVarInCon sumtype, env)
	    (* The result_type is not evaluated at run-time,
	       so we don't care if it's an allocation or not *)
	    val result_type_vset =
		   VS.intersection(freeVarInCon result_type, env)
	    fun loop [] = ([], VS.empty, OTHER, Movable)
              | loop ((w32, nt, arm)::arms) =
		let
		    val (arm, arm_vset, arm_category, arm_movability) =
			rexp(arm, env)
		    val (arms, arms_vset, arms_category, arms_movability) =
			loop arms
		in
		    ((w32,nt,arm) :: arms,
		     VS.union(arm_vset, arms_vset),
		     mergeCategories (arm_category, arms_category),
		     mergeMovabilities(arm_movability, arms_movability))
		end
	    val (arms, arms_vset, arms_category, arms_movability) =
			loop arms
	    val (default, default_vset, default_category, default_movability) =
		(case default of
		     (* if there's no default, the match might fail
		      and so this switch is potentially side-effecting *)
		     NONE => (NONE, VS.empty, OTHER, Immovable)
		   | SOME arm => rexpopt (default, env))

	    val exp = Switch_e(Sumsw_e{arg = arg, sumtype = sumtype,
				       result_type = result_type,
				       bound = bound,
				       arms = arms, default = default})
	    val vset = VS.union
		         (VS.union(VS.union(arg_vset, result_type_vset),
				   VS.union(arms_vset, default_vset)),
			  sumtype_vset)
	    val category = mergeCategories
		            (mergeCategories(arg_category, arms_category),
			     default_category)
	    val movability = mergeMovabilities
		             (mergeMovabilities(arg_movability,
						arms_movability),
			      default_movability)
	in
	    (exp, vset, category, movability)
	end

      | rswitch (Exncase_e{arg, result_type,
			   bound, arms, default}, env) =
	let
	    val (arg, arg_vset, arg_category, arg_movability) =
		   rexp (arg, env)
	    (* The result_type is not evaluated at run-time,
	       so we don't care if it's an allocation or not *)
	    val result_type_vset =
		   VS.intersection(freeVarInCon result_type, env)
	    fun loop [] = ([], VS.empty, OTHER, Movable)
              | loop ((tagexp, nt, arm)::arms) =
		let
		    val (tagexp, tag_vset, tag_category, tag_movability) =
			rexp(tagexp, env)
		    val (arm, arm_vset, arm_category, arm_movability) =
			rexp(arm, env)
		    val (arms, arms_vset, arms_category, arms_movability) =
			loop arms
		in
		    ((tagexp,nt,arm) :: arms,
		     VS.union(VS.union(tag_vset, arm_vset), arms_vset),
		     mergeCategories
		       (mergeCategories(arm_category, arms_category),
			tag_category),
		     mergeMovabilities
		        (mergeMovabilities(tag_movability, arm_movability),
			 arms_movability))
		end
	    val (arms, arms_vset, arms_category, arms_movability) =
			loop arms
	    val (default, default_vset, default_category, default_movability) =
		(case default of
		     (* if there's no default, the match might fail
		      and so this switch is potentially side-effecting *)
		     NONE => (NONE, VS.empty, OTHER, Immovable)
		   | SOME arm => rexpopt (default, env))

	    val exp = Switch_e(Exncase_e{arg = arg,
					 result_type = result_type,
					 bound = bound,
					 arms = arms, default = default})
	    val vset = VS.union(VS.union(arg_vset, result_type_vset),
				   VS.union(arms_vset, default_vset))

	    val category = mergeCategories
		            (mergeCategories(arg_category, arms_category),
			     default_category)

	    val movability = mergeMovabilities
		             (mergeMovabilities(arg_movability,
						arms_movability),
			      default_movability)
	in
	    (exp, vset, category, movability)
	end

    and rcbnd (Con_cb(var, con), env) =
	let
	    val (con, vset, category) = rcon (con, env)
	in
	    (Con_cb(var, con), vset, category)
	end
      | rcbnd (Open_cb(var, vklist, cbody), env) =
	let
	    val kinds_vsets = map (fn (v,k) => freeVarInKind k) vklist
	    val kinds_vset = List.foldl VS.union VS.empty kinds_vsets
	    val kinds_vset = VS.intersection(kinds_vset, env)
	    val (cbody, cbody_vset, _) = rcon (cbody, env)

	    val vset = VS.union(kinds_vset, cbody_vset)
	in
	    (Open_cb(var, vklist, cbody), vset, ALLOCATE)
	end
      | rcbnd (Code_cb(var, vklist, cbody), env) =
	let
	    val kinds_vsets = map (fn (v,k) => freeVarInKind k) vklist
	    val kinds_vset = List.foldl VS.union VS.empty kinds_vsets
	    val kinds_vset = VS.intersection(kinds_vset, env)
	    val (cbody, cbody_vset, _) = rcon (cbody, env)

	    val vset = VS.union(kinds_vset, cbody_vset)
	in
	    (Code_cb(var, vklist, cbody), vset, ALLOCATE)
	end

    and rcbnds ([], _) = ([], VS.empty, OTHER)
      | rcbnds (cbnd::cbnds, env) =
	let
	    val (cbnd, cbnd_vset, cbnd_category) = rcbnd (cbnd, env)
	    val (cbnds, cbnds_vset, cbnds_category) = rcbnds(cbnds, env)
	in
	    (cbnd :: cbnds,
	     VS.union(cbnd_vset, cbnds_vset),
	     mergeCategories(cbnd_category, cbnds_category))
	end

    and rbnd (bnd as Con_b(phase, cbnd), env) =
	let
	    val (cbnd, cbnd_vset, cbnd_category) = rcbnd (cbnd, env)

	    (* If the binding is marked Compiletime then
	       it won't appear in the code *)
	    val category = (case phase of
				Compiletime => OTHER
			      | Runtime => cbnd_category)
	in
	    (bnd, cbnd_vset, category, Movable)
	end

      | rbnd (Exp_b (var, nt, exp), env) =
	let
	    val (exp, exp_vset, exp_category, exp_movability) =
		  rexp (exp, env)
	    val nt_vset = TraceOps.get_free_vars nt
	    val vset = VS.union(nt_vset, exp_vset)
	in
	    (Exp_b(var, nt, exp), vset, exp_category, exp_movability)
	end

      | rbnd (Fixopen_b vfseq, env) =
	let
	    val (vfseq, vset) = rvfseq (vfseq, env)
	in
	    (Fixopen_b vfseq, vset, ALLOCATE, Movable)
	end

      | rbnd (Fixcode_b vfseq, env) =
	let
	    val (vfseq, vset) = rvfseq (vfseq, env)
	in
	    (Fixcode_b vfseq, vset, ALLOCATE, Movable)
	end

      | rbnd (Fixclosure_b (isRecursive, closureseq), env) =
	let
	    fun loop [] = ([], VS.empty, OTHER, Movable)
	      | loop ((var,{code,cenv,venv,tipe})::rest) =
		let
		    val code_vset =
			(VS.intersection(VS.singleton code,
					 env))
		    val (cenv, cenv_vset, cenv_category) =
			rcon (cenv, env)
		    val (venv, venv_vset, venv_category, venv_movability) =
			rexp (venv, env)
		    val tipe_vset =
			VS.intersection(freeVarInCon tipe, env)

		    val (rest, rest_vset, rest_category, rest_movability) =
			loop rest

		    val vset = VS.union
			        (VS.union(VS.union(code_vset, cenv_vset),
					  VS.union(venv_vset, tipe_vset)),
				 rest_vset)

		    (* The closures always at least ALLOCATE
		       (assuming they're not hoisted to top level, which
			can't really be detected here),
		       and might be APPLY if we're not in A-normal form. *)
		    val category =
			   mergeCategories
			    (mergeCategories(cenv_category, venv_category),
			     mergeCategories(rest_category, ALLOCATE))

		    val movability =
			   mergeMovabilities(venv_movability, rest_movability)
		in
		    ((var, {code = code, cenv = cenv,
			    venv = venv, tipe = tipe}) :: rest,
		     vset, category, movability)
		end

	    val closurelist = Sequence.toList closureseq
	    val (closurelist, vset, category, movability) = loop closurelist
	    val closureseq = Sequence.fromList closurelist
	in
	    (Fixclosure_b(isRecursive, closureseq),
	     vset, category, movability)
	end

    and rvfseq (vfseq, env) =
	let
	    fun loop [] = ([], VS.empty)
	      | loop ((v,f)::rest) =
		let
		    val (f, f_vset) = rfunction (f, env)
		    val (rest, rest_vset) = loop rest
		    val vset = VS.union(f_vset, rest_vset)
		in
		    (((v,f)::rest), vset)
		end
	    val vflist = Sequence.toList vfseq
	    val (vflist, vset) = loop vflist
	    val vfseq = Sequence.fromList vflist
	in
	    (vfseq, vset)
	end

    and rfunction(Function{effect, recursive, isDependent,
			   tFormals, eFormals, fFormals,
			   body, body_type}, env) =
	let
	    val tFormals_vsets =
		   map (fn (_,k) => freeVarInKind k) tFormals
	    val tFormals_vset = List.foldl VS.union VS.empty tFormals_vsets
	    val tFormals_vset = VS.intersection(tFormals_vset, env)

	    val eFormals_vsets =
		   map (fn (_,nt,con) =>
			 VS.union(TraceOps.get_free_vars nt,
				  freeVarInCon con)) eFormals
	    val eFormals_vset = List.foldl VS.union VS.empty eFormals_vsets
	    val eFormals_vset = VS.intersection(eFormals_vset, env)

	    val (body, body_vset, _, _) = rexp(body, env)

	    val body_type_vset =
  		   VS.intersection(freeVarInCon body_type, env)

	    val vset = VS.union(VS.union(tFormals_vset, eFormals_vset),
				VS.union(body_vset, body_type_vset))
	in
	    (Function{effect = effect, recursive = recursive,
		      isDependent = isDependent, tFormals = tFormals,
		      eFormals = eFormals, fFormals = fFormals,
		      body = body, body_type = body_type},
	     vset)
	end

    and rbnds (bnds, env) =
	let

(* XXX need to calculate free varables *not* bound in this loop,
   and subtract them out from the vsets *)

	    val outer_variables = env

	    fun loop ([], env, boundvars)  = ([], VS.empty, VS.empty, OTHER, Movable)
	      | loop (bnd::bnds, env, boundvars) =
		let
		    val (bnd, bnd_vset, bnd_category, bnd_movability) = rbnd(bnd, env)
		    val newvars = NilUtil.varsBoundByBnds [bnd]
		    val boundvars = VS.addList(boundvars, newvars)
		                    (* add to the running list of variables bound in this sequence *)
                    val outer_vset = VS.difference(bnd_vset, boundvars)
		                     (* find free variables in the current binding not bound in this sequence *)
		    val local_vset = VS.intersection(bnd_vset, boundvars)
			             (* find free variables in the current binding bound in this sequence *)
		    val env = makeInteresting (env, newvars)
		    val (anss, bnds_boundvars, bnds_outer_vset,
			 bnds_category, bnds_movability) =
			loop (bnds, env, boundvars)
		    val ans = (bnd, local_vset, bnd_category, bnd_movability)
			(* Entry for this binding: the binding itself, which local variables appear free, and its
			 * category and movability.
			 *)
		in
		    (ans::anss, boundvars,
		     VS.union(outer_vset, bnds_outer_vset),
		     mergeCategories(bnd_category, bnds_category),
		     mergeMovabilities(bnd_movability, bnds_movability))
		end

	    val (unsorted_answer, boundvars, bnds_outer_vset,
		 bnds_category, bnds_movability) = loop (bnds, env, VS.empty)

	    (* Count how many times we switch between allocate and apply modes *)
	    fun countAlternations ([], _, alts) = alts
(*
		 (* If there is at least one alternation then nothing
		    we can do will remove all alternations.  So we
		    ignore the first one. *)
		 Int.max(0, alts-1)
*)
              | countAlternations ((_,_,category,_)::rest, mode, alts) =
		(case (category,mode) of
		     (OTHER, _) => countAlternations (rest, mode, alts)
		   | (_, OTHER) => countAlternations (rest, category, alts)
		   | (ALLOCATE, ALLOCATE) => countAlternations (rest, mode, alts)
		   | (APPLY, APPLY) => countAlternations (rest, mode, alts)
		   | _ => countAlternations(rest, category, alts + 1))

	    val alternations_before =
		countAlternations (unsorted_answer, OTHER, 0)

	    (* Divide bindings by category/movability *)
	    fun split ([], rev_immovable, rev_apply, rev_allocate, rev_other) =
		(rev rev_immovable, rev rev_apply,
		 rev rev_allocate, rev rev_other)
	      | split ((bndstuff as (_,_,category,movable))::rest,
		      rev_immovable, rev_apply, rev_allocate, rev_other) =
		(case (category,movable) of
		     (_, Immovable) =>
			 split(rest, bndstuff :: rev_immovable,
			      rev_apply, rev_allocate, rev_other)
		   | (OTHER, _) =>
			 split(rest, rev_immovable,
			      rev_apply, rev_allocate, bndstuff::rev_other)
		   | (ALLOCATE, _) =>
			 split(rest, rev_immovable,
			      rev_apply, bndstuff::rev_allocate, rev_other)
		   | (APPLY, _) =>
			 split(rest, rev_immovable,
			      bndstuff::rev_apply, rev_allocate,rev_other))


	    val (immovables, applies, allocates, others) =
		     split (unsorted_answer, [], [], [], [])

(*
	    val _ = (print "Immovables:\n";
		     Ppnil.pp_bnds (map #1 immovables);
		     print "Applies:\n";
		     Ppnil.pp_bnds (map #1 applies);
		     print "Allocates:\n";
		     Ppnil.pp_bnds (map #1 allocates);
		     print "Others:\n";
		     Ppnil.pp_bnds (map #1 others);
		     print "\n")
*)

            (* removes all entries in the list whose dependencies are satisfied *)
	    fun pickValidDepSet ([], alreadybound_vset, rev_bnds, rev_keep) =
		   (alreadybound_vset, rev rev_bnds, rev rev_keep)
	      | pickValidDepSet ((stuff as (bnd,vset,category,movability)) :: rest,
				 alreadybound_vset, rev_bnds, rev_keep) =
		   let
		       val vset = VS.difference (vset, alreadybound_vset)
		   in
		       if (VS.numItems vset = 0) then (* no free variables have not already been processed in this loop *)
			   pickValidDepSet (rest, VS.addList(alreadybound_vset,
							     NilUtil.varsBoundByBnds
							     [bnd]),
					    stuff :: rev_bnds, rev_keep)
		       else (* Defer this binding for later *)
			   pickValidDepSet (rest, alreadybound_vset,
					    rev_bnds,
					    (bnd, vset, category, movability) :: rev_keep)
		   end

            (* removes all entries in the initial prefix of the list with (1) the right
               category and (2) whose dependencies are satisfied
	     *)
	    fun pickValidDepList ([], sought_category,
				  alreadybound_vset, rev_bnds) =
		   (alreadybound_vset, rev rev_bnds, [])
	      | pickValidDepList (bindings as ((stuff as (bnd,vset,category,_)) :: rest),
				  sought_category, alreadybound_vset, rev_bnds) =
		   let
		       val vset = VS.difference (vset, alreadybound_vset)
		   in
		       if (VS.numItems vset = 0) then
			   if (category = OTHER) orelse (category = sought_category) then
			       pickValidDepList (rest, sought_category,
						VS.addList
						 (alreadybound_vset,
						  NilUtil.varsBoundByBnds [bnd]),
						stuff :: rev_bnds)
			   else
			       (alreadybound_vset, rev rev_bnds, bindings)
		       else
			   (alreadybound_vset, rev rev_bnds, bindings)
		   end

	    (* Extract as many other/allocate bindings as possible, and then continue in the appopriate mode *)
	    fun allocateMode(alreadybound_vset, [], [], [], []) = []
              | allocateMode(alreadybound_vset, immovables, applies, allocates, others) =
		let
(* 		    val _ = print "allocateMode\n"*)

		    val (alreadybound_vset, bnds1, others) =
			pickValidDepSet (others, alreadybound_vset, [], [])
		    val (alreadybound_vset, bnds2, allocates) =
			pickValidDepSet (allocates, alreadybound_vset, [], [])
		    val (alreadybound_vset, bnds3, immovables) =
			pickValidDepList(immovables, ALLOCATE, alreadybound_vset, [])

		    val bnds = bnds1 @ bnds2 @ bnds3
		in
		    case bnds of
			[] => applyMode(alreadybound_vset, immovables,
					applies, allocates, others)
                      | _ => bnds @ allocateMode(alreadybound_vset, immovables,
						 applies, allocates, others)
		end

	    (* Extract as many other/apply bindings as possible, and then continue in the appropriate mode *)
	    and applyMode(alreadybound_vset, [], [], [], []) = []
              | applyMode(alreadybound_vset, immovables, applies, allocates, others) =
		let
(*
		    val _ = print "applyMode\n"
*)
		    val (alreadybound_vset, bnds1, others) =
			pickValidDepSet (others, alreadybound_vset, [], [])
(*
		    val _ = (print "\nbnds1 = "; Ppnil.pp_bnds (map #1 bnds1))
*)
		    val (alreadybound_vset, bnds2, applies) =
			pickValidDepSet (applies, alreadybound_vset, [], [])
(*
		    val _ = (print "\nbnds2 = "; Ppnil.pp_bnds (map #1 bnds2))
*)
		    val (alreadybound_vset, bnds3, immovables) =
			pickValidDepList(immovables, APPLY, alreadybound_vset, [])
(*
		    val _ = (print "\nbnds3 = "; Ppnil.pp_bnds (map #1 bnds3))
*)
		    val bnds = bnds1 @ bnds2 @ bnds3

		in
		    case bnds of
			[] => allocateMode(alreadybound_vset, immovables,
					   applies, allocates, others)
                      | _ => bnds @ applyMode(alreadybound_vset, immovables,
					      applies, allocates, others)
		end


(*
	    fun doit [] =
		   allocateMode(VS.empty, immovables, applies, allocates, others)
              | doit ((bnd,_,ALLOCATE,_)::_) =
		    allocateMode(VS.empty, immovables, applies, allocates, others)
              | doit ((bnd,_,APPLY,_)::_) =
		    applyMode(VS.empty, immovables, applies, allocates, others)
              | doit ((bnd,_,_,_)::rest) = doit rest
*)

            (* Extract the bindings with minimal alternation while still preserving dependency order *)
	    fun doit _ =
		   allocateMode(VS.empty, immovables, applies, allocates, others)

            val sorted_answer = doit immovables

	    val alternations_after =
		countAlternations (sorted_answer, OTHER, 0)


	    val _ = if ((List.length unsorted_answer) <>
			(List.length sorted_answer)) then
		       (print "ERROR: Dropped binding!\nBEFORE:\n";
			Ppnil.pp_bnds (map #1 unsorted_answer);
			print "\nAFTER:\n";
			Ppnil.pp_bnds (map #1 sorted_answer))
		    else ()


	    val _ = declareAlternations {old = alternations_before,
					 new = alternations_after}

	in
	    (map #1 sorted_answer,
	     bnds_outer_vset, bnds_category, bnds_movability)
	end

    fun rmod (MODULE {bnds, imports, exports}) =
        let
	    val _ = clearStats ()
	    val (bnds, _, _, _) = rbnds (bnds, empty_env)
	    val _ = printStats ()
	in
            MODULE {bnds = bnds, imports = imports, exports = exports}
	end

    val optimize = rmod
end
