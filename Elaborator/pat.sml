(*
   The pattern matcher is used to compile external language patterns into IL
   expressions. These are generated as a series of 'if'-style tests (for
   constants -- which can sometimes be changed into intswitches later in
   the compiler) or EXN_CASE and CASE constructs for exns and sums.

   The pattern matcher first compiles the external AST patterns into
   an internal set of core patterns. These are divided into two sets:
       irrefutable: Record, Ref, and Wild.
       refutable: various sum types such as Int, datatype constructors, ...

   The core routine is a function called match which takes
   (1) a typing context
   (2) a list of arguments  (n > 0)
   (3) a list of rules each containing
       (a) a list of patterns equal in length to the argument list
       (b) a list of variables that have been bound as we descend the pattern
       (c) a function which takes the bound variables and generate a HIL expression
   (4) a function that generates a default expression if no rules match

   We can lay out the rules' patterns into a matrix with each row corresonding
   to a rule.  We can always exchange columns of patterns provided we make
   the corresponding permutation in the argument list.  However, we can
   only reorder rows in certain circumstances.

   For simplicity, we explain the algorithm using 4 core patterns:
   Record, Wild, Int, and Sum.  The others can be treated similarly using
   the following association.

   (1) Record includes Record, Ref
   (2) Wild includes Wild
   (3) Int includes Int, Word, Char, String, Vector
   (4) Sum includes Constructor and Exception

   Each column of patterns can be classified by the types of core patterns
   that occur in it.

   (1) IRREF if a Record occurs in the column
   (2) WILD if the column contains only Wilds
   (3) SUM if the column contains only Sums
   (4) INT if the column contains only Ints
   (5) SUMWILD (of int) if the column contains Sums and Wilds.
       The int indicates the first row that has a Wild.
   (6) INTWILD (of int) if the column contains Ints and Wilds.
       The int indicates the first row that has a Wild.

   We prefer pattern-matching on columns that are IRREF, WILD above all
   since these do not cause code duplication.  That is, the decision tree
   will not have multiple leaves that correspond to the same rule.

   Next we prefer SUMs to INTs since branching on SUMs early can reduce
   the amount of case constructs and provides on an opportunity to
   reduce the decision tree.

   Finally, SUMWILD and INTWILD are undesirable because they may require
   code duplication.  That is, multiple paths in the decision tree
   to the same rule.  These last two cases can be improved by carefully
   rearranging the rules when possible.

   (1) Choose WILD column - all arms will be used
   (2) Choose IRREF column - all arms will be used
   (3) Choose SUM column - all arms will be used
   (4) Choose INT column
       (a) All columns are INT columns.  Branch on all arms of 1st column.
           This can be considered a degenerate use of untilSelfWild.
       (b) Otherwise, there is a wild somewhere.
           Choose only arms in which there is a Wild in the arm.
	   We must also include the arms which have the
	   same Int pattern as arms we are including.  Finally,
	   we must preserve the relative ordering of the arms we select out.
	   This is a use of untilSelfWild.
   (5) Choose SUMWILD or INTWILD column with the latest occurring Wild.
       Prefer SUMWILD in case of ties.  If still tied, go from left to right.
       (a) If the first row is composed entirely of Wilds (other than in the
           chosen column) and it is non-trivial (there's more than one column),
           then select only arms starting from the beginning
           that are also composed entirely of Wilds (other than in the
	   chosen column).  This is a use of untilOtherNonwild.
       (b) Otherwise, chose arms up to but not including the first
           row with a Wild in the chosen column.
	   This is a use of untilSelfWild.

   XXX datatypes that have only one constructor could also be considered IRREF
               - Tom

   XXX change choosing criterion of (5) to stop at first non-wild in other column
       since non-wilds will require defaults

   XXX should coalesce constants

   Some invariants to be aware of:
    - Arguments are always variables, so we can discard or duplicate them without
      any effects
*)


structure Pat :> PAT =
struct

    open Il
    open Prim

    structure U = IlUtil
    structure C = IlContext
    structure N = Name

    val do_result_type = Stats.tt("PatResultType")
    val debug = Stats.ff("PatDebug")

    (* Use error to signal a compiler error.  Print an error message
       and then abort() to signal an error in the source program.
       Top-level functions in the pattern compiler should handle
       Abort. *)
    val error = fn s => Util.error "pat.sml" s
    exception Abort
    fun abort () = raise Abort
    fun abort' s =
	let in
	    Error.error_region ();
	    print s;
	    print "\n";
	    abort()
	end

    fun printint i = print (Int.toString i)
    fun debugdo t = if (!debug) then (t(); ()) else ()
    val wildSymbol = (Symbol.varSymbol "_")

    type symbol = Symbol.symbol

    (* This is not the same as Ast.pat *)
    datatype basePattern =
	(* Irrefutable or non-branching patterns *)
	Record of {fields : (label * pattern) list, flexibility : bool}
      | Ref of pattern
      | Wild
	(* Refutable or branching patterns *)
      | Int of Ast.literal
      | Word of Ast.literal
      | Char of string
      | String of string
      | Constructor of Ast.path * pattern option  (* Datatype constructors may be value-carrying *)
      | Exception of Ast.path * exp * (con * pattern) option    (* Exn constructors may be value-carrying *)

    (* The symbol list comes from VarPats as well as LayeredPats.
       (for instance, an external language pattern "x" is compiled
        an IL ([x], [], Wild).)
       The con list comes from ConstraintPats. *)
    withtype pattern = symbol list * con list * basePattern

    type arg = var * con
    type bound = (symbol * var * con) list
    type body = context * bound -> exp * con
    type rule = pattern list * bound * body
    type baseRule = basePattern list * bound * body
    type default = unit -> exp
    type resCon = {shortCon : con ref, fullCon : con ref}

    datatype columnType = IRREF of basePattern (* must be Ref or Record *)
                        | WILD
                        | SUM of basePattern (* must be Constructor or Exception *)
                        | INT
                        | SUMWILD of int * basePattern  (* must be Constructor or Exception *)
                        | INTWILD of int (* zero based *)

    (* ------------ pattern specific pretty-printing stuff  ------------------ *)
    fun pp_arg (v,c) = (Ppil.pp_var v; print " : "; Ppil.pp_con c)
    fun pp_bound bound =
	let fun pp_svc (s,v,c) = (AstHelp.pp_sym s; print " -> ";
				  Ppil.pp_var v; print " : "; Ppil.pp_con c; print "    ")
	in  map pp_svc bound
	end

    fun pp_app(path, NONE) = AstHelp.pp_path path
      | pp_app(path, SOME pat) = (AstHelp.pp_path path; print " "; pp_pattern pat)

    and pp_basePattern (Record {fields, flexibility}) =
	(print "{";
	 map (fn (l, p) => (Ppil.pp_label l; print " : "; pp_pattern p; print ", ")) fields;
	 print "}")
      | pp_basePattern (Ref pat) = (print "Ref "; pp_pattern pat)
      | pp_basePattern Wild = print "_"
      | pp_basePattern (Int lit) = print (TilWord64.toDecimalString lit)
      | pp_basePattern (Word lit) = print (TilWord64.toDecimalString lit)
      | pp_basePattern (Char str) = (print "#\""; print str; print "\"")
      | pp_basePattern (String str) = (print "\""; print str; print "\"")
      | pp_basePattern (Constructor (path, patOpt)) = pp_app(path,patOpt)
      | pp_basePattern (Exception (path, _, NONE)) = pp_app(path,NONE)
      | pp_basePattern (Exception (path, _, SOME(_,pat))) = pp_app(path,SOME pat)

    and pp_pattern (syms, cons, basePattern) =
	((case syms of
	      [] => ()
	    | s::_ => (AstHelp.pp_sym s; print " as "));
	      pp_basePattern basePattern)

    fun pp_rule ((pats, bound, body) : rule) =
	(map (fn p => (pp_pattern p; print "   ")) pats;
	 print "    ....   ")

    fun pp_baseRule ((bpats, bound, body) : baseRule) =
	let in
	    map (fn p => (pp_basePattern p; print "   ")) bpats ;
	    print "\n ... bound: "; pp_bound bound
	end

    (* Link in some functions from modules that depend on this one.
       The linking is performed in linkil.sml;
       polyinst will be Toil.polyinst,
       typecompile will be Toil.typecompile,
       expcompile will be Toil.expcompile.
    *)
    local
	val cxexp = ref (NONE : (Il.context * Ast.exp -> Il.exp * Il.con * bool) option)
	val cxty = ref (NONE : (Il.context * Ast.ty -> Il.con) option)
	val cpolyinst = ref (NONE : (Il.context * Il.sdecs -> Il.sbnd list * Il.sdecs * Il.con list) option)
    in
	fun installHelpers {typecompile, expcompile, polyinst} =
	    let in
		(case !cxexp of
		     NONE => ()
		   | SOME _ => print ("WARNING: installHelpers called more than once.\n" ^
				      "         Possibly because CM.make does not have the semantics of a fresh make\n"));
	        cxty := SOME typecompile;
		cpolyinst := SOME polyinst;
		cxexp := SOME expcompile
	    end
	fun typecompile arg = (Option.valOf (!cxty)) arg
	fun polyinst arg = (Option.valOf (!cpolyinst)) arg
	fun expcompile arg = (Option.valOf (!cxexp)) arg
    end

    (* If an overloaded expression is known, use it.
       In fact, these aren't really "overloaded"; they
       are delayed computations used in 'compile' to
       prevent code from being duplicated.
       *)
    fun derefOverexp (exp as (OVEREXP (_,_,eOneshot))) =
	(case Util.oneshot_deref eOneshot of
	     NONE => exp
	   | SOME e => e)
      | derefOverexp e = e

    (* reduces eta-expanded records
       This translation is not general, but appears to be
       OK for the limited circumstances in which it is used.

       (1)
       let ... no variable bindings ...
       in { l1 = v }                        ==>     { l1 = v }
       end

       (2)
       let v1 = #1 r
	   ...
	   v3 = #3 r
	   ...                              ==>     r
	   v2 = #2 r
	   ...
       in { l1 = v1, l2 = v2, l3 = v3 }
       end

       XXX this will do the transformation even if r is shorter than the
           record created (is this ok because of width subtyping?)
       XXX how do we know this won't erase effectful bindings in (2)?
       *)
    fun reduce_let e =
      (case e of
	 LET(bnds, letBody) =>
	   (case derefOverexp letBody of
		(* 1 *)
	      body as (RECORD [(_,VAR _)]) => if (List.all (fn BND_EXP _ => false
	                                                     | _ => true)) bnds
						  then body
					      else e
	        (* 2 *)
	    | RECORD le_exp =>
		 let
		     val dummy = N.fresh_named_var "dummy"
		     val vars = map (fn (_,VAR v) => v
	                              | _ => dummy) le_exp

		     (* see if the variable used in the expression for this field is
			bound as a projection of the same field from another record *)
		     fun search [] (n,v) = NONE
		       | search ((BND_EXP(v',RECORD_PROJECT(VAR base,label,_))) :: rest) (n,v) =
			                             (* XXX tom - why tuple_label and not the actual label? *)
			      if (N.eq_var(v,v') andalso N.eq_label(label,U.generate_tuple_label (n+1)))
				  then SOME base
			      else search rest (n,v)
		       | search (_::rest) nv = search rest nv
		     fun bound [] _ = false
		       | bound ((BND_EXP(v',_)) :: rest) v =
			    N.eq_var (v,v') orelse bound rest v
		       | bound (_ :: rest) v = bound rest v
		     val baseVarOpts = Listops.mapcount (search bnds) vars
		 in
		     case baseVarOpts of
			 SOME baseVar :: rest =>
			     (* If every field is a projection from the existing record variable
				and that variable was not bound in this let *)
			     if (List.all (fn SOME v => N.eq_var (v,baseVar)
			                         | _ => false) rest) andalso
				 not (bound bnds baseVar)
			     then VAR baseVar
			     else e
			 | _ => e
		 end
	    | _ => e)
       | _ => e)

    (* Arms and defaults of cases are an expression option. We might
       as well make them NONE if the expression is a case that won't
       ever match anything. This checks for such cases and returns
       NONE (empty case) or SOME e (a possibly reduced expression)
       accordingly.
       *)
    fun compress_case (exp as (CASE{arms,default,...})) : exp option =
	let fun do_arms (no_arms,acc,[]) = (no_arms, rev acc)
	      | do_arms (no_arms,acc,NONE::rest) = do_arms(no_arms,NONE::acc,rest)
	      | do_arms (no_arms,acc,SOME e::rest) = (case compress_case e of
							    NONE => do_arms(no_arms,NONE::acc,rest)
							  | se => do_arms(false,se::acc,rest))
	    val (no_arms, arms) = do_arms (true, [], arms)
	in
	    if no_arms then
		case default of
		    SOME def => compress_case def
		  | NONE => NONE
	    else SOME exp
	end
      | compress_case exp = SOME exp


    (* compute the columnType of a column *)
    fun computeColumnType (pat::pats) =
	let fun reduce (bp as (Record _)) = IRREF bp
	      | reduce (bp as (Ref _)) = IRREF bp
	      | reduce (bp as (Constructor _)) = SUM bp
	      | reduce (bp as (Exception _)) = SUM bp
	      | reduce Wild = WILD
	      | reduce _ = INT
	    fun loop (n, acc, []) = acc
	      | loop (n, acc, cur::rest) =
		(case (acc,cur) of
		     (IRREF _, _) => acc
		   | (SUMWILD _, _) => acc
		   | (INTWILD _, _) => acc
		   | (WILD, IRREF _) => cur
		   | (WILD, WILD) => loop (n+1, WILD, rest)
		   | (WILD, SUM bp) => SUMWILD (0, bp)
		   | (WILD, INT) => INTWILD 0
		   | (WILD, _) => abort' "cannot get a SUMWILD or INTWILD in one element"
		   | (SUM _, SUM _) => loop (n+1, acc, rest)
		   | (SUM bp, WILD) => SUMWILD (n, bp)
		   | (SUM _, _) => abort' "patterns do not agree: expected SUM or WILD"
		   | (INT, INT) => loop (n+1, INT, rest)
		   | (INT, WILD) => INTWILD n
		   | (INT, _) => abort' "patterns do not agree: expected INT or WILD")
	in  loop (1, reduce pat, map reduce pats)
	end


    (* pat2Pattern takes the AST patterns and produces a simplified internal pattern *)
    local
	(* XXX bug 0084 -- We can't just look these up, because they may have been
	   rebound. -- Tom *)
	(* Actually, we can just look them up if we implement the Definition's
	   syntactic restriction. *)
	 fun listpat2pat [] = Ast.VarPat [Symbol.varSymbol "nil"]
	   | listpat2pat (p::rest) = Ast.AppPat{constr=Ast.VarPat[Symbol.varSymbol "::"],
						argument=Ast.TuplePat[p,listpat2pat rest]}

	 fun unmarkpat (Ast.MarkPat (pat, _)) = unmarkpat pat
	   | unmarkpat p = p
    in
	 fun pat2Pattern context (pat : Ast.pat) : pattern =
	   let open Ast
	       val ptop = pat2Pattern context
	   in
	       case pat of
		  WildPat => ([], [], Wild)
		| VarPat p => if (Datatype.is_constr context p)
				      then ([],[], Constructor(p,NONE))
			      else (case Datatype.exn_lookup context p of
				    NONE => (case p of
						 [s] =>
						     if (Symbol.name s = "ref") then
							 abort' "constructor used without argument in pattern"
						     else ([s], [], Wild)
					       | _ => (Error.error_region();
						       print "non-constructor path pattern: ";
						       AstHelp.pp_path p; print "\n";
						       abort()))
				  | SOME {stamp, carried_type=NONE} => ([], [], Exception(p, stamp, NONE))
				  | SOME _ => abort' "exception constructor is missing pattern")
		| IntPat lit => ([], [], Int lit)
		| WordPat lit => ([], [], Word lit)
		| StringPat str => ([], [], String str)
		| CharPat str => ([], [], Char str)
		| RecordPat {def:(symbol * pat) list, flexibility:bool} =>
			  let val fields = map (fn (s,p) => (N.symbol_label s, ptop p)) def
			  in  ([], [], Record {fields = fields, flexibility = flexibility})
			  end
		| ListPat p => ptop (listpat2pat p)
		| TuplePat pats =>
		    let val fields = Listops.mapcount (fn (i,p) => (U.generate_tuple_symbol (i+1), p)) pats
		    in  ptop(RecordPat{def = fields, flexibility = false})
		    end
		| FlatAppPat patfixes => error "flatapppat should be parsed away"
		| AppPat {constr:pat, argument:pat} =>
		      (case unmarkpat constr of
			   Ast.VarPat p =>
			       if (Datatype.is_constr context p)
				   then ([], [], Constructor(p, SOME (ptop argument)))
			       else (case Datatype.exn_lookup context p of
					 NONE => if (case p of
						      [s] => Symbol.name s = "ref"
						    | _ => false)
						     then ([], [], Ref (ptop argument))
						 else (Error.error_region();
						       print "non-constructor in app pattern: ";
						       AstHelp.pp_path p; print "\n";
						       abort())
				       | SOME {stamp, carried_type=NONE} =>
						     abort' ("non-value-carrying exception constructor " ^
								"applied to argument in pattern")
				       | SOME {stamp, carried_type=SOME c} =>
					     ([], [], Exception(p, stamp, SOME (c, ptop argument))))
			 | _ => abort' "AppPat applied a non-path")
		| ConstraintPat{pattern,constraint} =>
		      let val c = typecompile (context,constraint)
			  val (syms, cons, bp) = ptop pattern
		      in  (syms, c :: cons, bp)
		      end
		| VectorPat pats => error "Vector patterns are not supported"
		| LayeredPat{varPat=Ast.VarPat[s],expPat} =>
		      let val (syms, cons, bp) = ptop expPat
		      in  (s :: syms, cons, bp)
		      end
		| LayeredPat _ => error "Funny varPat in LayeredPat"
		| MarkPat (p,r) => ptop p
	   end
     end


    (* take the intersection of two binding lists ((symbol * var * con) list):
       keep the members of b2 whose symbols are in b1 in the order of b1 *)
    fun bound_intersect (b1 : bound, b2 : bound) : bound =
	let fun teq (s,_,_) (s',_,_) = Symbol.eq(s,s')
	    fun mapper t = List.find (teq t) b2
	in  List.mapPartial mapper b1
	end

    (* checks that con <: fullCon,
       otherwise promote fullCon to its supertype
        (makes total arrows partial, removes special sum info)
       and try again...
       *)
    fun check_rescon (context, {fullCon, ...} : resCon, con) =
	if IlStatic.sub_con(context ,con, !fullCon) orelse
	   IlStatic.sub_con(context, con, (fullCon := IlStatic.supertype (!fullCon); !fullCon)) then ()
	else let in
		Error.error_region();
		print "Result type mismatch.\n  Expected type: "; Ppil.pp_con (!fullCon);
		print "\n  Actual type: "; Ppil.pp_con con; print "\n"
	     end

    (* wrapbnds creates a let (but does some simple local optimization)
       wrapbnds' does the same, but alpha-varies all BND_EXP bindings.
       *)
    val wrapbnds = U.make_let

    fun wrapbnds' ([] : bnds, e : exp) = e
      | wrapbnds' (bnds, e) =
	let fun folder(BND_EXP(v,e),subst) =
	         let val e' = IlUtil.exp_subst(e,subst)
		     val v' = N.fresh_named_var (N.var2string v)
		 in  (BND_EXP(v',e'), U.subst_add_expvar(subst,v,VAR v'))
		 end
	      | folder(BND_CON(v,c),subst) = (BND_CON(v,IlUtil.con_subst(c,subst)),subst)
	      | folder(BND_MOD(v,b,m),subst) = (BND_MOD(v,b,IlUtil.mod_subst(m,subst)),subst)
	    val (bnds,subst) = Listops.foldl_acc folder U.empty_subst bnds
	    val e = U.exp_subst(e,subst)
	in  U.make_let (bnds, e)
	end

    (* Given a record variable rv,
       A list of n variables, n constructors, and n labels,
       return bindings
           v1 = #l1 (rv : rcon)
	   v2 = #l2 (rv : rcon)
           ...
           vn = #ln (rv : rcon)
       where rcon is {l1 : c1, l2 : c2, ..., ln : cn }

       XXX - bind rcon as well to save space
       *)

    fun letprojecthelp (rv : Il.var,
			vars : var list, cons : con list,
			labels : label list) : Il.bnds =
	let val rcon = CON_RECORD(U.sort_labelpair(Listops.zip labels cons))
	    val bnds = Listops.map2 (fn (v,l) => BND_EXP(v,RECORD_PROJECT(VAR rv,l,rcon))) (vars,labels)
	in  bnds
	end

    local
	(* unify or fail with error message *)
	fun check context actual_c constrain_c =
	    if (IlStatic.eq_con(context, actual_c, constrain_c))
		then ()
	    else (Error.error_region();
		  print "actual type does not match constraint in pattern\n";
		  print "  Actual type: "; Ppil.pp_con actual_c; print "\n";
		  print "  Pattern type: "; Ppil.pp_con constrain_c; print "\n")

	(* update list of bound variables, maintaining no shadowing invariant *)
	fun extendBound((s,(v,c) : arg), bound : bound) : bound =
	    let fun notshadow (s2,_,_) = not(Symbol.eq(s,s2))
		val bound = List.filter notshadow bound
	    in  (s,v,c)::bound
	    end

    in

	(* adds variables in 'bound' to context *)
	fun extendContext(ctxt, bound : bound) : context =
	    let
		fun folder((s,v,c), ctxt) =
		    let val l = N.symbol_label s
		    in  (case C.Context_Lookup_Var_Raw(ctxt,v)
			   of NONE => C.add_context_exp (ctxt, l, v, c)
			    | SOME _ => C.add_context_label (ctxt, l, v)) (* ugly hack *)
		    end
	    in  foldl folder ctxt bound
	    end

	(* cons a new basepattern onto a baserule *)
	fun extendBaseRule context (baseRule : baseRule, pat : pattern, arg : arg) : baseRule =
	    let val (basePats, bound, body) = baseRule
		val (_, actual_c) = arg
		val (syms, cons, basePat) = pat
		val _ = app (check context actual_c) cons
		val bound = foldl (fn (s, bound) => extendBound((s, arg), bound)) bound syms
	    in  (basePat :: basePats, bound, body)
	    end

	fun reduceRule context (rule : rule, args : arg list) : baseRule =
	    let val (pats, bound, body) = rule
		fun folder (((syms, cons, bp), arg as (_, actual_con)), bound) =
		    let val _ = app (check context actual_con) cons
			val bound = foldl (fn (s,bound) => extendBound((s, arg), bound)) bound syms
		    in  (bp, bound)
		    end
	    val (basePats,bound) = Listops.foldl_acc folder bound (Listops.zip pats args)
	in  (basePats, bound, body)
	end

    end

    (* find all the variables that are bound by a pattern *)
    fun get_bound context (pat : Ast.pat) : Ast.symbol list =
	let open Ast
	in  (case pat of
	    WildPat => []
          | VarPat [s] => if (Datatype.is_constr context [s] orelse
			      (case Datatype.exn_lookup context [s] of
				   NONE => false
				 | SOME _ => true))
			      then []
			  else [s]
          | VarPat _ => []
          | IntPat _ => []
          | WordPat _ => []
          | StringPat _ => []
          | CharPat _ => []
          | RecordPat {def:(symbol * pat) list, ...} =>
		 Listops.flatten(map (fn (_,p) => get_bound context p) def)
          | ListPat p => Listops.flatten(map (get_bound context) p)
          | TuplePat p => Listops.flatten(map (get_bound context) p)
	  | FlatAppPat patfixes => error "flatapppat should be parsed away"
          | AppPat {constr:pat, argument:pat} => get_bound context argument
          | ConstraintPat {pattern:pat, constraint:ty} => get_bound context pattern
          | LayeredPat {varPat:pat, expPat:pat} => (get_bound context varPat) @ (get_bound context expPat)
          | VectorPat _ => error "Vector patterns are not supported"
          | MarkPat (p,r) => get_bound context p)
	end

    (* listExtract n l
       returns the nth element and the list with that element removed *)
    fun listExtract _ [] = error "listExtract given list too short"
      | listExtract 0 (a::rest) = (a,rest)
      | listExtract n (a::rest) =
	let val (elem,remain) = listExtract (n-1) rest
	in  (elem, a::remain)
	end

    (* extract the nth pattern from a rule, return the pattern
       and the rule with that pattern removed
       extractpat : int -> baseRule -> basePattern * baseRule
       *)
    fun extractpat n (pats : basePattern list, bound : bound, body : body) =
	let val (targetPat, pats) = listExtract n pats
	in  (targetPat, (pats, bound, body))
	end


    fun eqLiteral (Int lit, Int lit') = TilWord64.equal(lit, lit')
      | eqLiteral (Word lit, Word lit') = TilWord64.equal(lit, lit')
      | eqLiteral (String str, String str') = str = str'
      | eqLiteral (Char str, Char str') = str = str'
      | eqLiteral _ = abort' "eqLiteral: pattern is not literal"

    (*
	Note that special constant overloading is also handled by
	toil.sml:/make_uint_overload and friends.
    *)
    datatype literal =
	IntLit of TilWord64.word * intsize
      | WordLit of TilWord64.word * intsize
      | FloatLit of TilWord64.word * floatsize
      | StringLit of string
      | CharLit of string

    fun conLiteral (litPat : basePattern) : (literal * con) list =
	(case litPat
	   of Int lit => [(IntLit (lit, W32), CON_INT W32)]
	    | Word lit => [(IntLit (lit, W32), CON_UINT W32),
			   (IntLit (lit, W8), CON_UINT W8)]
	    | String str => [(StringLit str, CON_VECTOR(CON_UINT W8))]
	    | Char str => [(CharLit str, CON_UINT W8)]
	    | _ => abort' "conLiteral: pattern is not literal")

    (* generate an expression testing v against b (a literal pattern).
       This is used in constant_case below to generate the decision tree. *)
    fun equalerLiteral (context, v : Il.var, lit : literal) : Il.exp =
	(case lit of
	     IntLit (lit,sz) =>
		 let val const = int(sz, lit)
		 in  PRIM (eq_int sz,[],[VAR v,SCON const])
		 end
	   | WordLit (lit,sz) =>
		 let val const = uint(sz, lit)
		 in  ILPRIM (eq_uint sz,[],[VAR v,SCON const])
		 end
	   | StringLit str =>
		 let fun mapper c = SCON(uint(W8,TilWord64.fromInt (ord c)))
		     val str = SCON(vector (CON_UINT W8, Array.fromList(map mapper (explode str))))
		 in  APP(U.string_eq context, U.exp_tuple[VAR v, str])
		 end
	   | CharLit cstr =>
		 let val char = Util.CharStr2char cstr
		 in  ILPRIM (eq_uint W8, [], [VAR v, SCON(uint(W8, TilWord64.fromInt (ord char)))])
		 end)

    (* For "code snippet that inlines string equality", see CVS before 13 Aug 02 *)


    fun record_ref_dispatch (extractFun,n,args,arms) =
	let (* Can never fail -- type error otherwise *)
	    val (targetArg, restArgs) = listExtract n args
	    fun mapper rule =
		let val (targetPat, rule) = extractpat n rule
		in  (extractFun targetPat, rule)
		end
	in  (targetArg, restArgs, map mapper arms)
	end

    (* When we decide to split on a column, we call one of these routines.
       read match (below) before this code *)

    (* wild_case just removes that entire column and the corresponding arg *)
    fun wild_case (col, context, args, arms : baseRule list, def, resCon) : exp =
	let fun mapper rule =
		case extractpat col rule of
		    (Wild, r) => r
		  | _ => error "must have Wild here"
	    val (_, argRest) = listExtract col args
	    val arms = map mapper arms
	in  match(context, argRest, arms, def, resCon)
	end

    (* ref_case binds a new variable to the contents of the ref cell, uses that
       as the new argument for that column, and recurses with the ref constructor
       stripped from those patterns.

       case a b c of
           ref p11    p12    p13  => ...
           ref p21    p22    p23  => ...
           ref p31    p32    p33  => ...

       becomes

       let a' = !a
       in
       case a' b c of
           p11    p12    p13  => ...
           p21    p22    p23  => ...
           p31    p32    p33  => ...
       end

       *)
    and ref_case (col, context, args, arms, def, resCon) : exp =
	let fun extractRefInfo (Ref pattern) = pattern
	      | extractRefInfo Wild = ([], [], Wild)
	      | extractRefInfo _ = error "must have ref or wild here"

	    val ((var,con), restArgs, info_arms) = record_ref_dispatch(extractRefInfo, col, args, arms)

	    (* unify with ref *)
	    val elemcon = U.fresh_con context
	    val _ = if (IlStatic.eq_con(context, con, CON_REF elemcon))
			then ()
		    else (abort' "ref pattern used on a non-ref argument")

	    (* bind a new variable for the contents of the ref *)
	    val v = N.fresh_var()
	    val bnd = BND_EXP(v,ILPRIM(deref,[elemcon],[VAR var]))
	    val newarg = (v,elemcon)
	    val newargs = newarg::restArgs
	    val newarms = map (fn (p,rule) => extendBaseRule context (rule, p, newarg)) info_arms
	(* emit this binding and recurse *)
	in  wrapbnds([bnd], match(context,newargs,newarms,def,resCon))
	end

  (* record_case projects the components of the record, binds them to new variables, then
     adds each one as an argument (replacing the original record argument) and exploding
     the components of the record pattern.

     case a of
	 (p1, p2, p3) => ...
         (q1, q2, q3) => ...

     becomes

     let a1 = #1 a
	 a2 = #2 a
	 a3 = #3 a
     in
     case a1 a2 a3 of
	 p1   p2   p3 => ...
	 q1   q2   q3 => ...
     end

     Most of the extra complication below arises from the need to handle flex records.
     We need to pick the set of labels actually used (and project only those), but also
     make sure that the patterns can all belong to the same record!

     *)
  and record_case (col, context, args, arms, def, resCon) : exp =
    let fun extractRecordInfo(Record {fields,flexibility}) = (fields, flexibility)
	  | extractRecordInfo Wild = ([], true)
	  | extractRecordInfo _ = error "must have record or wild here"
	val (targetArg, restArgs, info_arms) = record_ref_dispatch (extractRecordInfo,col,args,arms)

	fun same(s1,s2) = if Listops.sameset_eq N.eq_label s1 s2 then s2
			  else abort' "flex records do not all agree in record patterns (test: same)"

	fun subset(s1,s2) = if Listops.subset_eq N.eq_label s1 s2 then s2
			    else abort' "flex records do not all agree in record patterns (test: subset)"

	(* check that there are no duplicate labels *)
	fun unique (splist : (label * pattern) list) =
	    let fun folder((l,_),acc) = if (Listops.member_eq(N.eq_label,l,acc))
					    then abort' "duplicate field names in record pattern"
					else l::acc
	    in foldr folder [] splist (* foldr preserves the order *)
	    end

	(* get the list of used labels and verify that flex records are ok...
	   flex: true if "still flexible"
	   flag: true if this record is flexible *)
	fun folder (((splist : (label * pattern) list,flag),_),(syms,flex)) =
	    (case (flag,flex) of
		 (false,false) => (same(unique splist,syms), false)
	       | (true,false) => (subset(unique splist,syms), false)
	       | (false,true) => (subset(syms,unique splist), false)
	       | (true,true) => (Listops.list_sum_eq (N.eq_label, unique splist, syms), true))
	val (syms,flex) = foldl folder ([],true) info_arms

	(* create bindings and recurse *)
	fun fetch splist s : pattern = (case Listops.assoc_eq(N.eq_label,s,splist) of
					    SOME p => p
					  | NONE => ([], [], Wild))
	fun acc_normalize((splist,_),arm) = (map (fetch splist) syms, arm)
	val accs' = map acc_normalize info_arms
	val (var1,con1) = targetArg
	val rvars = map (fn _ => N.fresh_var()) syms
	val rcons = map (fn _ => U.fresh_con context) syms
 	val rbnds = letprojecthelp(var1,rvars,rcons,syms)
	val lc = U.sort_labelpair (Listops.zip syms rcons)
	val argcon = if flex
			 then CON_FLEXRECORD(ref (FLEXINFO(Tyvar.new_stamp(),false,lc)))
		     else CON_RECORD lc
	val _ = if (IlStatic.eq_con(context, con1, argcon))
		    then ()
		else (Error.error_region();
		      print "tuple/record pattern used on a non-record argument\n";
		      print "Actual type: "; Ppil.pp_con con1; print "\n";
		      print "Pattern type: "; Ppil.pp_con argcon; print "\n")
	val newargs = Listops.zip rvars rcons
	(* foldr because we concatenate new args to front of list *)
	fun extender (pats,rule) = foldr (fn ((p,arg),rule) => extendBaseRule context (rule, p, arg))
	                             rule (Listops.zip pats newargs)
	val newarms = map extender accs'
	val e = match(context,newargs @ restArgs,newarms,def,resCon)
    in  wrapbnds(rbnds,e)
    end

  (* exn_case uses the selector much like constant_case (below) to
     get info_arms (the patterns and success continuations we'll be splitting on)
     and unmatchedArms (the new failure continuation)

     case x of
	 Match =>  ...
       | Div =>  ...
       | Fail s =>  ...

     becomes

     EXN_CASE x of
         Match   with   fn () => ...
       | Div     with   fn () => ...
       | Fail    with   fn s  => ...

     *)

  and exn_case(col, context, args, arms, def, resCon, selector) : exp =
      let val (pat_arms, unmatchedArms) = selector(col, arms)
	  val info_arms = map (fn (Exception info, arm) => (info, arm)
			        | _ => error "must have exception here") pat_arms

	  val ((exnvar,exncon), restArgs) = listExtract col args
	  val def = (case unmatchedArms of
			 [] => def
		       | _ => fn () => match(context,args,unmatchedArms,def,resCon))

	  (* generate the arms for the exn_case.
	     Basically just pull out the stamp and con (or use unit if not value-carrying),
	     and generate a lambda as the handler. *)
	  val rescon = ref(U.fresh_con context)
	  val v = N.fresh_named_var "exnarg_var"
	  fun helper ((path,stamp,typepat_opt), arm : baseRule) =
	      let val con = (case typepat_opt of
				 SOME (c,_) => c
			       | _ => U.con_unit)
		  val e =
		      (case typepat_opt of
			   NONE => match(context,restArgs,[arm],def,resCon)
			 | SOME (_, argpat) =>
			       let val arg = (v,con)
				   val arm' = extendBaseRule context (arm, argpat, arg)
			       in  match(context, arg::restArgs, [arm'], def, resCon)
			       end)
		  val body = #1(U.make_lambda(v,con,!(#shortCon resCon),e))
	      in (stamp,con,body)
	      end

	  val _ = if (IlStatic.eq_con(context,exncon,CON_ANY))
		      then ()
		  else (Error.error_region();
			print "exception pattern used on a non-exception argument\n";
			print "Argument type: "; Ppil.pp_con exncon; print "\n")
	  val exnarg = VAR exnvar
	  val arms' : (exp * con * exp) list = map helper info_arms
	  val default' =
	      let val e = def()
	      in  compress_case e
	      end
      in  EXN_CASE{arg=exnarg, arms=arms', default = default', tipe = !(#shortCon resCon)}
      end

  (* XXX document *)

  and constructor_case (col, context, args, arms, def, resCon, selector) : exp =
    let

      (* get the argument we're testing, and the rest *)
      val ((casevar, casecon), restArgs) = listExtract col args

      (* use selector to find our series of tests (with their success continuations)
	 and the failure continuation *)
      val (pat_arms, unmatchedArms) =  selector(col, arms)
      val info_arms = map (fn (Constructor info, arm) => (info, arm)
			    | _ => error "must have constructor here") pat_arms
      val def = (case unmatchedArms of
		     [] => def
		   | _ => fn () => match(context,args,unmatchedArms,def,resCon))
      val casearg = VAR casevar
      val rescon_var = N.fresh_named_var "rescon_var"
      val rescon = ref(U.fresh_con context)
      val rsvar = N.fresh_named_var "sumswitch_arg"

      (* can't fail; this column has stuff in it or we'd be done in match! *)
      val ((ast_path,_),_) = hd info_arms

      val {instantiated_type = datacon,
	   instantiated_sumtype = sumtype,
	   arms = constr_patconopt_list,
	   expose_exp} = Datatype.instantiate_datatype_signature(context, ast_path, polyinst)
      val jopt = (case C.Context_Lookup_Var(context, casevar)
		    of SOME(_,PHRASE_CLASS_EXP(_,CON_SUM{special,...},_,_)) => special
		     | _ => NONE)

      val _ = if IlStatic.sub_con(context,casecon,datacon)
		  then ()
	      else (Error.error_region();
		    print "datacon is "; Ppil.pp_con datacon; print "\n";
		    print "casecon is "; Ppil.pp_con casecon; print "\n";
		    print "constructor pattern used on an argument of the wrong type\n")

      fun getarm (i,{name=cur_constr,arg_type}) : exp option =
	let
	    val argopt = (case arg_type of
			      NONE => NONE
			    | SOME c => SOME(N.fresh_var(), c))
	    fun armhelp ((path,patopt), baseRule : baseRule) : baseRule option =
		(case (N.eq_label(cur_constr, N.symbol_label (List.last path)), patopt, argopt) of
		     (false,_, _) => NONE
		   | (true,NONE, NONE) => SOME baseRule
		   | (true,SOME argpat, SOME arg) => SOME(extendBaseRule context (baseRule, argpat, arg))
		   | (true, _, _) => abort' "value-carrying vs non-value-carrying mismatch")
	    val relevants : baseRule list = List.mapPartial armhelp info_arms
	in  if (case jopt of
		    NONE => false
		  | SOME j => i <> j) then NONE
	    else (case (relevants, argopt) of
	      ([],_) => NONE
	    | (_,NONE) => let val me = match(context, restArgs, relevants, def, resCon)
			  in  compress_case me
			  end
	    | (_,SOME (newArg as (var,rcon))) =>
			  let val me = match(context,newArg::restArgs, relevants, def, resCon)
			  in SOME(U.make_let([BND_EXP(var,SUM_TAIL(i,sumtype,VAR rsvar))],me))
			  end)
	end

      val expopt_list = Listops.mapcount getarm constr_patconopt_list

      val exhaustive = List.all (fn NONE => false | SOME _ => true) expopt_list

      val arg = APP(expose_exp,casearg)
      val arg = IlUtil.exp_try_reduce (context, arg)

      val case_exp =
	  (CASE{sumtype=sumtype,
		arg = arg,
		bound=rsvar,
		arms = expopt_list,
		default = if exhaustive then NONE
			  else compress_case (def ()),
		tipe = !(#shortCon(resCon))})

    in   case_exp
    end

    (* constant_case emits a series of branches on the selected column.
       The selector function determines
         info_arms: In order, check against the basePattern, and continue with the baseRule
	            if a match.
	 unmatchedArms: the default for recursive calls will "fall through" to test these.

       The selector function is chosen by some magic guesses about what will lead to
       less duplication. See the code in 'match' and description at the top of the file.

       *)
    and constant_case(col, context, args, arms, def, resCon,
		      selector : int * baseRule list -> (basePattern * baseRule) list * baseRule list) : exp =
	let val (info_arms, unmatchedArms) =  selector(col, arms)

	    (* default for recursive calls -- handle the unmatched arms, if any *)
	    val def = (case unmatchedArms of
			   [] => def
			 | _ => fn () => match(context,args,unmatchedArms,def,resCon))

	    val ((var, con), restArgs) = listExtract col args

	    (* generate the series of if tests. matchOne collects up
	       all the rules that have the same constant as the one we're
	       about to test, so that we don't do any redundant tests.
	       *)
	    fun matchOne [] = error "matchOne given no rules"
	      | matchOne (info_arms as (targetPat,_)::_) =
		let val litCons = conLiteral targetPat
		    val (default,_) :: _ = litCons
		    fun try (_,c) = IlStatic.eq_con(context,c,con)
		    val lit =
			(case List.find try litCons
			   of SOME (l,_) => l
			    | NONE =>
				(Error.error_region ();
				 print "type of pattern does not agree with argument\n";
				 print "Argument type: "; Ppil.pp_con con; print "\n";
				 default))
		    fun pred (p, _) = eqLiteral(p,targetPat)
		    val (matches,mismatches) = List.partition pred info_arms
		    val match_rules = map #2 matches
		    val e = match(context, restArgs, match_rules, def, resCon)
		in  (lit, e, mismatches)
		end

	    fun matchAll acc [] = rev acc
	      | matchAll acc info_arms =
		let val (lit, e, rest) = matchOne info_arms
		in  matchAll ((lit,e)::acc) rest
		end

	    val lit_e_list = matchAll [] info_arms
	    val resultType = !(#shortCon resCon)
	    fun folder((lit,e), rest) =
		U.make_ifthenelse context (equalerLiteral(context,var,lit), e, rest, resultType)

	in  foldr folder (def()) lit_e_list
	end

  (* match context args arms def resCon
     args   : (var * con) list
     arms   : (basePattern list * bound * body) list
     def    : unit -> exp
     resCon : {shortCon : con ref, fullCon : con ref}

     This is the meat of the pattern matcher, which implements the
     strategy described at the top of the file.

     *)
  and match (context,
	     args : arg list,
	     arms : baseRule list,
	     def : default,
	     resCon : resCon) : exp =
      let

	  val _ = debugdo (fn () =>
			   if length arms > 0 orelse length args > 0 then
			   let in
			       print "\nMATCH called with ";
			       printint (length args); print " args:\n";
			       Listops.mapcount (fn(i,arg) => (print "  ARG #"; printint i; print ": ";
							       pp_arg arg; print "\n")) args;
			       print "\nand with "; printint (length arms); print " arms:\n";
			       Listops.mapcount (fn(i,br) => (print "  ARM #"; printint i;
							      print ": "; pp_baseRule br; print "\n")) arms;
			       ()
			   end
			   else ())

	  (* Choose rules until pred is satisfied.
	     The pred is run on the pair of the pattern in question and the list of
	        patterns with that one removed.
	     Splits the list of rules into (matched, unmatched) where matched @ unmatched
	     is the original list and matched is the longest prefix of rules that don't satisfy pred.
	     We require that the first rule does not satisfy the predicate (so matched is not nil).
	     *)
	  fun untilPred (pred : (basePattern * basePattern list) -> bool, col, rules : baseRule list) =
	      let
		  fun loop acc [] = (rev acc, [])
		    | loop acc (rules as (rule::rest)) =
		       let val split as (selfPat, restrules) = extractpat col rule
			   val (otherPats,_,_) = restrules
		       in  if (pred(selfPat,otherPats))
			       then (rev acc, rules)
			   else loop (split::acc) rest
		       end
	      in
		  case loop nil rules of
		      ([], _) => error "untilPred: matched empty"
		    | res => res
	      end

	  fun isWild Wild = true
	    | isWild _ = false

	  fun untilSelfWild (col, rules) =
	      untilPred (isWild o #1, col, rules)

	  fun anyNonwild pats = not (List.all isWild pats)

	  fun untilOtherNonwild (col,rules) =
	      let fun isOtherNonwild (_, otherPats) = anyNonwild otherPats
	      in  untilPred (isOtherNonwild,col,rules)
	      end

	  fun chooseAnyWild (col, rules : baseRule list) : (basePattern * baseRule) list * baseRule list =
	      let
		  (* get rules that have wild anywhere*)
		  fun ruleHasWild(pats,_,_) = List.exists isWild pats
		  val rulesWithWild = List.filter ruleHasWild rules

		  (* get just the column in question of those rules *)
		  fun getPat (pats,_,_) = List.nth (pats, col)
		  val targetPats = map getPat rulesWithWild

		  (* basepattern at column from rule is in targetpats *)
		  fun pred (pats,_,_) =
		      let val curPat = List.nth (pats, col)
		      in  Listops.member_eq(eqLiteral, curPat, targetPats)
		      end

		  val (matches,mismatches) = List.partition pred rules
		  val info_arms = map (extractpat col) matches
	      in  (info_arms, mismatches)
	      end

      in
	  case (arms,args) of
	    (* no rules: use default *)
	      ([],_) => def()
	    (* no arguments: match succeeds, use rule *)
	    | ([([],bound,body)], []) => let val (e,c) = body(context,bound)
					     val _ = check_rescon(context, resCon, c)
					 in  e
					 end
	    (* no arguments, but many rules: redundant *)
	    | (_, []) => (Error.error_region();
			  print "Redundant matches.\n";
			  #1(Error.dummy_exp (context,"RedundantMatch")))
	    | _ =>
		  let
		      val matrix = map #1 arms

		      (* compute the columntypes of columns *)
		      val colTypes = map computeColumnType (Listops.transpose matrix)

		      (* the following functions are chained from the bottom up
			 (as described in the strategy at the top of this file);
			 first we look for a column of wilds, then irrefutable,
			 sum, int, mix... On success, we dispatch to the appropriate
			 mutually-recursive function.
			 *)
		      fun lookForMIX() =
			  let fun greater(SUMWILD (m,_), SUMWILD (n,_)) = m > n
				| greater(INTWILD m, INTWILD n) = m > n
				| greater(INTWILD m, SUMWILD (n,_)) = m > n
				| greater(SUMWILD (m,_), INTWILD n) = m >= n
				| greater _ = error "must have SUMWILD or INTWILD"
			      fun loop col acc [] = acc
				| loop col (acc as (maxCol, maxColType)) (colType::rest) =
				  let val acc = if (greater(colType, maxColType))
						    then (col, colType)
						else acc
				  in  loop (col+1) acc rest
				  end
			      val firstCol :: restCol = colTypes
			      val (maxCol, maxColType) = loop 1 (0, firstCol) restCol
			      fun handleSum f =
				  let val rule::_ = arms
				      val (_, (otherPats, _, _)) = extractpat maxCol rule
				      val selector = if (anyNonwild otherPats orelse (length args = 1))
							 then untilSelfWild
						     else untilOtherNonwild
				  in  f(maxCol, context, args, arms, def, resCon, selector)
				  end
			  in  (case maxColType of
				   SUMWILD (_,Constructor _) => handleSum constructor_case
				 | SUMWILD (_,Exception _) => handleSum exn_case
				 | INTWILD _ => constant_case(maxCol, context, args, arms,
							      def, resCon, untilSelfWild)
				 | _ => error "must have SUMWILD or INTWILD here")
			  end

		      (* If all columns are INT, choose the first with untilSelfWild (??)
			 otherwise, choose the first INT we see, with chooseAnyWild (??)
			 *)
		      fun lookForINT() =
			  let fun isINT INT = true
				| isINT _ = false
			      fun loop _ [] = lookForMIX()
				| loop n (INT::rest) =
				  constant_case(n, context, args, arms, def, resCon, chooseAnyWild)
				| loop n (_::rest) = loop (n+1) rest

			  in  if (Listops.andfold isINT colTypes)
				  then constant_case(0, context, args, arms, def, resCon, untilSelfWild)
			      else loop 0 colTypes
			  end

		      (* Datatype or exception constructors *)
		      fun lookForSUM() =
			  let fun loop _ [] = lookForINT()
				| loop n ((SUM bp)::rest) =
			           (case bp of
					Constructor _ => constructor_case(n, context, args, arms, def, resCon, untilSelfWild)
				      | Exception _ => exn_case(n, context, args, arms, def, resCon, untilSelfWild)
				      | _ => error "must have Constructor or Exception here")
				| loop n (_::rest) = loop (n+1) rest
			  in  loop 0 colTypes
			  end

		      fun lookForIRREF() =
			  let fun loop _ [] = lookForSUM()
				| loop n ((IRREF bp)::rest) =
			           (case bp of
					Record _ => record_case(n, context, args, arms, def, resCon)
				      | Ref _ => ref_case(n, context, args, arms, def, resCon)
				      | _ => error "must have Record or Ref here")
				| loop n (_::rest) = loop (n+1) rest
			  in  loop 0 colTypes
			  end

		      fun lookForWILD() =
			  let fun loop _ [] = lookForIRREF()
				| loop n (WILD::rest) = wild_case(n, context, args, arms, def, resCon)
				| loop n (_::rest) = loop (n+1) rest
			  in  loop 0 colTypes
			  end

		  in lookForWILD ()
		  end
      end (* end of match *)


 (* Compile takes external-language expressions and patterns and
    invokes match appropriately. One tricky thing that this does
    is make sure that no code is ever duplicated by setting up
    thunks and hoisting the body of an arm up as a function if
    it is emitted multiple times. Read on ...

    default : con -> unit -> exp
    This is a suspended function to generate the default. In
    general it needs the return type of the match in order to
    do a RAISE at the right type.
    *)

 fun compile (context : context,
	      compile_args : arg list,
	      compile_arms : (Ast.pat list * Ast.exp) list,
	      default : Il.con -> unit -> exp)
     : exp * con =
    let
	val _ = debugdo (fn () => print "--------------- COMPILE called -----------------\n\n")
	val fullCon = ref (U.fresh_named_con (context, "fullResultType"))
	val shortCon = if (!do_result_type)
			   then ref(CON_VAR(N.fresh_named_var "shortResultType"))
		       else fullCon

	(* generate "r" and rules for each arm
	   The first field of r is a list of exp oneshots where we
	      emitted the body (and the variables bound at that location).
	      Below, these will be set with either the direct expression or
	      a call to the hoisted function.
	   The second field is a suspended elaboration of the expression.
	      (or, eventually, the memoized result of elaboration!)
	   *)
	fun mapper (clause : Ast.pat list, e : Ast.exp) =
	    let
		val r = ref([], NONE)
		val pats = map (pat2Pattern context) clause

		(* Suspended body-creation function.
		   Each time it's called, it adds a new oneshot onto
		   the list in "r", and emits OVEREXP of that oneshot. *)
		fun body (ctxt,bound) =
		    let val ctxt = extendContext(ctxt,bound)
			val one = Util.oneshot()
			val carried as (_,(e,c,va)) =
			    (case #2(!r) of
				 NONE => (bound, expcompile(ctxt,e))
			       | SOME memoized => memoized)
			val _ = r := ((one,bound)::(#1(!r)), SOME carried)
		    (* abuse OVEREXP to hold a delayed expression *)
		    in  (OVEREXP(c, va, one), c)
		    end
		val rule = (pats,[],body)
	    in
		(r, rule)
	    end
	val (rs, rules) = Listops.unzip (map mapper compile_arms)

	(* Loop over the whole "rs" list. If the arm was used only
	   once, put the body in there directly. If it was used more than once,
	   bind it above the match as a function, and make calls to it
	   at each location where it was used. *)
	fun wrapper (ec : exp) : exp =
	    let fun folder(ref(one_bounds : (exp Util.oneshot * bound) list,SOME(bound,(e,c,va))),bnds) =
		if (length one_bounds<2)
		    then (Util.oneshot_set(#1(hd one_bounds),e); bnds)
		else
		    let
			val bound = foldl bound_intersect bound (map #2 one_bounds)
			val bound = List.filter (fn (s,_,_) => not(Symbol.eq(s,wildSymbol))) bound
			val one_bounds = map (fn (one,b) =>
					      (one,bound_intersect(bound,b))) one_bounds
			val bound_vars = map #2 bound
			val bound_cons = map #3 bound
			val funvar = N.fresh_named_var "repeat_patbody"
			val (argvar, argcon) =
			    (case bound of
				 [(_,v,c)] => (N.derived_var v, c)
			       | _ => (N.fresh_named_var "repeat_casevar",
				       U.con_tuple bound_cons))
			val labels = Listops.mapcount
			    (fn (n,_) => U.generate_tuple_label(n+1)) bound
			val lbnds = letprojecthelp(argvar,bound_vars,
						   bound_cons,labels)
			val e =
			    (case bound of
				 [(_,bound_var,_)] =>
				     IlUtil.exp_subst
				      (e,U.subst_add_expvar(U.empty_subst,bound_var,VAR argvar))
			       | _ =>  wrapbnds'(lbnds,e))
			val (lambda,funcon) = U.make_lambda(argvar,argcon,!shortCon,e)
			val bnd = BND_EXP(funvar,lambda)

			(* set all of the oneshots to be an application. *)
			fun apper(one,bounds : bound) =
			    let val bound_vars = map #2 bounds
				val call = APP(VAR funvar,
					       (case bounds of
						    [(_,bound_var,_)] => VAR bound_var
						  | _ => U.exp_tuple(map VAR bound_vars)))
			    in  Util.oneshot_set(one,call)
			    end
			val _ = app apper one_bounds
		     in  bnd :: bnds
		     end
		  (* if the expression is NONE, then it was never emitted.
		     This is a little bogus, because the expression will never be
		     type-checked. But this also shouldn't happen unless there are
		     redundant arms, which we catch and fail on.
		     *)
		  | folder(ref([],NONE),bnds) = bnds
		  | folder(ref(_,NONE),bnds) = error "non-empty oneshot list with NONE"

		val bnds = foldl folder [] rs
	    in  wrapbnds (bnds, ec)
	    end

	val baseRules = map (fn r => reduceRule context (r, compile_args)) rules

	(* finally call match *)
	val almost_e = match(context,compile_args,baseRules,
			     (default (!shortCon)),
			     {fullCon = fullCon, shortCon = shortCon})
	val very_nearly_e = wrapper almost_e
	(* bind "short" result type if flag set *)
	val final_e = (case (!do_result_type, !shortCon) of
			   (true, CON_VAR v) => U.make_let([BND_CON(v,!fullCon)], very_nearly_e)
			 | _ => very_nearly_e)

    in  (final_e, !fullCon)
    end

    (* ============ client interfaces =========================== *)
    (* ---- bindCompile creates bindings ----------------------- *)
    fun bindCompile {context : context,
		     bindpat : Ast.pat,
		     arg = (argvar : var, argc : con)}
                    : (sbnd * sdec) list =
      let

	(* the one arm for this pattern will be a tuple of all the variables
	   it binds. *)
	val boundsyms = get_bound context bindpat

	val args = [(argvar,argc)]
	val astTuple = Ast.TupleExp(map (fn s => Ast.VarExp [s]) boundsyms)
	val arms = [([bindpat],astTuple)]

	fun default returntype () =
	    let in
		(* if we generate this code, then (syntactically) there is a path to
		   a Match or Bind exception. So, print a warning. *)
		Error.warn_region_with "binding nonexhaustive\n";
		RAISE(returntype, U.bind_exn context)
	    end

	val (binde,bindc) = compile(context, args, arms, default)
	val binde = reduce_let binde

	(* generate bindings for the variables bound in the pattern, if applicable.

	   val (a, 2, c) = (1, 2, 3)

	   becomes

	   bindTuple = case ... => (1, 3)
	   a = #1 bindTuple
           c = #2 bindTuple
	   *)
	local
	    val tupleLabel = N.internal_label "bindTuple"
	    val tupleVar = N.fresh_named_var "bindTuple"
	in  val (tupleSbndSdec,base) =
	         (case binde of
		      VAR v => ([],binde)
		    | _ => ([(SBND(tupleLabel,(BND_EXP(tupleVar,binde))),
			      SDEC(tupleLabel,(DEC_EXP(tupleVar,bindc,NONE,false))))],
			    VAR tupleVar))
	end

	val lc_list = (case IlStatic.con_head_normalize(context, bindc) of
			   CON_RECORD lc_list => lc_list
			 | c => error "bindc not a tuple")

	fun mapper(n,s) =
	    let val l = N.symbol_label s
		val v = N.gen_var_from_symbol s
		val (fieldLabel, fieldCon) = List.nth(lc_list,n)
		val fieldExp = RECORD_PROJECT(base, fieldLabel, bindc)
	    in   (SBND(l,(BND_EXP(v,fieldExp))),
		  SDEC(l,(DEC_EXP(v,fieldCon,NONE,false))))
	    end

	val external_sbnd_sdecs = Listops.mapcount mapper boundsyms
	val final_sbnd_sdecs =
	    (case (tupleSbndSdec,external_sbnd_sdecs) of
		 ([(SBND(_,BND_EXP(_,RECORD[(f1,e)])), _)],
		  [(SBND(l,BND_EXP(v,RECORD_PROJECT(_,f2,c))),sdec)]) =>
		     if (N.eq_label(f1,f2))
			 then [(SBND(l, BND_EXP(v, e)), sdec)]
		     else tupleSbndSdec @ external_sbnd_sdecs
	       | _ => tupleSbndSdec @ external_sbnd_sdecs)

      in final_sbnd_sdecs
      end handle Abort => []

    (* Compile an AST case into an IL exp/con. This essentially
       just involves calling match. When used for an SML handle
       expression, we want to re-raise the exception (case
       argument) rather than raising Match. *)
    fun caseCompile {context : context,
		     arms = cases : (Ast.pat * Ast.exp) list,
		     arg = (argv : Il.var, argc : Il.con),
		     reraise}
      : Il.exp * Il.con =
      let
	val args = [(argv, argc)]
	val arms = map (fn (pat,body) => ([pat], body)) cases

	val _ = if reraise andalso not (IlStatic.eq_con(context,argc,CON_ANY))
		    then abort' "default of pattern not an exn type"
		else ()

	fun default returntype () =
	    if reraise then RAISE(returntype, VAR argv)
	    else let in
		    Error.warn_region_with "match nonexhaustive\n";
		    RAISE(returntype, U.match_exn context)
		 end

      in  compile (context, args, arms, default)
      end handle Abort => Error.dummy_exp (context, "case_pat")

    (* ---- funCompile creates a curried function ----------------------- *)
    fun funCompile {context : context,
		    rules : (Ast.pat list * Ast.exp) list}
      : {arglist : (Il.var * Il.con) list, body : Il.exp * Il.con} =
      let
	(* -------- we begin by creating arguments of the same name as the
	   -------- as the variable pattern when possible; just for legibility *)

	  fun getname [] = "mvar"
	    | getname ((Ast.VarPat[s])::rest) = if Datatype.is_constr context [s]
						    then getname rest
						else Symbol.name s
	    | getname (_::rest) = getname rest
	  fun getnames ([]::_) = []
	    | getnames clauses = (getname (map hd clauses)) :: (getnames (map tl clauses))
	  val names = getnames (map #1 rules)

	  val args = map (fn s => (N.fresh_named_var s,
				   U.fresh_named_con (context,s))) names

	  (* ---- call main routine to get compiled body ----- *)

	  fun default returntype () =
	      let in
		  Error.warn_region_with "match nonexhaustive\n";
		  RAISE(returntype, U.match_exn context)
	      end

	  val (e,c) = compile (context,args,rules,default)

      in {arglist = args,
	  body = (e,c) }
      end handle Abort => {arglist = nil,
			   body = Error.dummy_exp (context, "fun_pat")}

  end
