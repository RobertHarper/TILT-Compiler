(*$import Util Listops Name Prim Int Symbol TilWord64 Array Tyvar List Ast Il IlStatic Ppil IlUtil IlContext Datatype Error PAT AstHelp Stats *)

(* XXX should coalesce constants (what do you mean?) *)

(* The pattern matcher compiles the external AST patterns into an internal set
   of core patterns.  These are divided into two sets:
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
           This can be considered a degenerate use of untilAnyWild.    XXX tom ?
       (b) Otherwise, there is a wild somewhere. 
           Choose only arms in which there is a Wild in the arm.  
	   We must also include the arms which have the
	   same Int pattern as arms we are including.  Finally,
	   we must preserve the relative ordering of the arms we select out.
	   This is a use of chooseAnyWild.
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

XXXXX change choosing criterion of (5) to stop at first non-wild in other column
         since non-wilds will require defaults
*)



structure Pat :> PAT =
struct

    open Il
    open Prim

    structure U = IlUtil
    structure C = IlContext
    structure N = Name

    val do_result_type = Stats.tt("PatResultType")
    val debug = Stats.ff("Pattern_debug")

    val error = fn s => Util.error "pat.sml" s
    fun printint i = print (Int.toString i)
    fun debugdo t = if (!debug) then (t(); ()) else ()
    val wildSymbol = (Symbol.varSymbol "_")

    (* ------ Reduces eta-expanded records ------ *)
    fun derefOverexp (e as (OVEREXP (_,_,eOneshot))) = 
	(case Util.oneshot_deref eOneshot of
	     NONE => e
	   | SOME e => e)
      | derefOverexp e = e

    fun reduce_expression e = 
      (case e of
	 LET(bnds, letBody) =>
	   (case derefOverexp letBody of
	      body as (RECORD [(_,VAR v)]) => if (List.all (fn BND_EXP _ => false
	                                                     | _ => true)) bnds
						  then body
					      else e
            | RECORD le_exp =>
		 let val vars = map (fn (_,VAR v) => v
	                              | _ => N.fresh_named_var "dummy") le_exp
		     fun search [] (n,v) = NONE
		       | search ((BND_EXP(v',RECORD_PROJECT(VAR base,label,_))) :: rest) (n,v) = 
			      if (N.eq_var(v,v') andalso N.eq_label(label,U.generate_tuple_label (n+1)))
				  then SOME base 
			      else search rest (n,v)
		       | search (_::rest) nv = search rest nv
		     val baseVarOpts = Listops.mapcount (search bnds) vars
		 in  (case baseVarOpts of
			  (SOME baseVar) :: rest =>
			      if ((List.all (fn SOME v => N.eq_var(v,baseVar)
			                      | _ => false) rest) andalso
				  (case search bnds (0,baseVar) of
				       NONE => true
				     | _ => false))
				  then VAR baseVar
			      else e
		        | _ => e)
		 end
	    | _ => e)
       | _ => e)

    (* ------ Eliminate empty CASE expressions ------ *)
    fun compress_expression (exp as (CASE{arms,default,...})) : exp option = 
	let fun do_arms (no_arms,acc,[]) = (no_arms, rev acc)
	      | do_arms (no_arms,acc,NONE::rest) = do_arms(no_arms,NONE::acc,rest)
	      | do_arms (no_arms,acc,(SOME e)::rest) = (case compress_expression e of
							    NONE => do_arms(no_arms,NONE::acc,rest)
							  | se => do_arms(false,se::acc,rest))
	    val (no_arms,arms) = do_arms (true,[],arms)
	in  (case default of
		 NONE => if no_arms then NONE else SOME exp
	       | SOME def => if no_arms then compress_expression def else SOME exp)
	end
      | compress_expression exp = SOME exp


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

    (* The core routine is a match function that takes 
     
     (0) a typing context
     (1) a list of arguments - variables and types
     (2) a list of rules 
         (a) ML source level patterns
	 (b) variables that have already been bound
	 (c) a function called with the bound variables which generates the HIL expression
     (3) a default expression generator to be executed if the patterns all fail

     *)

    type symbol = Symbol.symbol

    (* This is not the same as the Ast.pat.   *)
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
      | Vector of pattern list
      | Constructor of Ast.path * pattern option  (* Datatype constructors may be value-carrying *)
      | Exception of Ast.path * exp * (con * pattern) option    (* Exn constructors may be value-carrying *)

    (* The symbol list comes from VarPats as well as LayeredPats.
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

    (* ------------ file specific pretty-printed stuff  ------------------ *)
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
      | pp_basePattern (Vector _) = print "Vector"
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
	(map (fn p => (pp_basePattern p; print "   ")) bpats
(*	 print "    BOUND = "; pp_bound bound; 
	 print "    ARM =  ..."
*)
	 )

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
		   | (WILD, _) => error "cannot get a SUMWILD or INTWILD in one element"
		   | (SUM _, SUM _) => loop (n+1, acc, rest)
		   | (SUM bp, WILD) => SUMWILD (n, bp)
		   | (SUM _, _) => error "expected SUM or WILD"
		   | (INT, INT) => loop (n+1, INT, rest)
		   | (INT, WILD) => INTWILD n
		   | (INT, _) => error "expeceted INT or WILD")
	in  loop (1, reduce pat, map reduce pats)
	end


    (* pat2Pattern takes the AST patterns and produces a simplified internal pattern *)
     local
	 fun listpat2pat [] = Ast.VarPat [Symbol.varSymbol "nil"]
	   | listpat2pat (p::rest) = Ast.AppPat{constr=Ast.VarPat[Symbol.varSymbol "::"],
						argument=Ast.TuplePat[p,listpat2pat rest]}
     in
	 fun pat2Pattern context (pat : Ast.pat) : pattern = 
	   let open Ast
	       val pat2Pattern = pat2Pattern context
	   in  (case pat of
		  WildPat => ([], [], Wild)
		| VarPat p => if (Datatype.is_constr context p)
				      then ([],[], Constructor(p,NONE))
			      else (case Datatype.exn_lookup context p of
				    NONE => (case p of
						 [s] => ([s], [], Wild)
					       | _ => (Error.error_region();
						       print "non-constructor path pattern: ";
						       AstHelp.pp_path p; print "\n";
						       error "non-constructor path pattern"))
				  | SOME {stamp, carried_type=NONE} => ([], [], Exception(p, stamp, NONE))
				  | SOME {stamp, carried_type} => error "exception constructoris missing pattern")
		| IntPat lit => ([], [], Int lit)
		| WordPat lit => ([], [], Word lit)
		| StringPat str => ([], [], String str)
		| CharPat str => ([], [], Char str)
		| RecordPat {def:(symbol * pat) list, flexibility:bool} => 
			  let val fields = map (fn (s,p) => (N.symbol_label s, pat2Pattern p)) def
			  in  ([], [], Record {fields = fields, flexibility = flexibility})
			  end
		| ListPat p => 
		    let fun listpat2pat [] = Ast.VarPat [Symbol.varSymbol "nil"]
			  | listpat2pat (p::rest) = Ast.AppPat{constr=Ast.VarPat[Symbol.varSymbol "::"],
							       argument=Ast.TuplePat[p,listpat2pat rest]}
		    in pat2Pattern(listpat2pat p) 
		    end
		| TuplePat pats => 
		    let fun tuple_case _ [] = []
			  | tuple_case n (p::r) = (U.generate_tuple_symbol n,p)::(tuple_case (n+1) r)
			val fields = tuple_case 1 pats
		    in  pat2Pattern(RecordPat{def = fields, flexibility = false})
		    end
		| FlatAppPat patfixes => error "flatapppat should be parsed away"
		| AppPat {constr:pat,argument:pat} => 
		      (case (case constr of Ast.MarkPat(p,_) => p | _ => constr) of
			   Ast.VarPat p => 
			       if (Datatype.is_constr context p)
				   then ([], [], Constructor(p, SOME (pat2Pattern argument)))
			       else (case Datatype.exn_lookup context p of
					 NONE => if (case p of
						      [s] => Symbol.name s = "ref"
						    | _ => false)
						     then ([], [], Ref (pat2Pattern argument))
						 else (Error.error_region();
						       print "non-constructor path pattern: ";
						       AstHelp.pp_path p; print "\n";
						       error "non-constructor path pattern")
				       | SOME {stamp, carried_type=NONE} => error "exception constris not value-carrying"
				       | SOME {stamp, carried_type=SOME c} => 
					     ([], [], Exception(p, stamp, SOME (c, pat2Pattern argument))))
			 | _ => error "AppPat applied a non-path")
		| ConstraintPat{pattern,constraint} =>
		      let val c = typecompile (context,constraint)
			  val (syms, cons, bp) = pat2Pattern pattern
		      in  (syms, c :: cons, bp)
		      end
		| VectorPat pats => ([], [], Vector(map pat2Pattern pats))
		| LayeredPat{varPat=Ast.VarPat[s],expPat} =>
		      let val (syms, cons, bp) = pat2Pattern expPat
		      in  (s :: syms, cons, bp)
		      end
		| LayeredPat _ => error "Funny varPat in LayeredPat"
		| MarkPat (p,r) => pat2Pattern p)
	   end
     end


    (* keep the members of b2 whose symbols are in b1 in the order of b1 *)
    fun bound_intersect (b1 : bound, b2 : bound) : bound = 
	let fun teq (s,_,_) (s',_,_) = Symbol.eq(s,s')
	    fun mapper t = List.find (teq t) b2
	in  List.mapPartial mapper b1
	end

    fun arm2sdecs(_,svc_list,_) = map (fn (s,v,c) => (SDEC(N.symbol_label s,
							   DEC_EXP(v,c,NONE, false)))) svc_list

    fun check_rescon (context, {fullCon, ...} : resCon, con) = 
	let val ok = IlStatic.sub_con(context,con,!fullCon)
	    val _ = if ok then () else fullCon := (IlStatic.supertype (!fullCon))
	    val ok = ok orelse IlStatic.sub_con(context,con,!fullCon)
	in  if ok then ()
	    else (Error.error_region();
		  print "Result type mismatch.\n  Expected type: "; Ppil.pp_con (!fullCon);
		  print "\n  Actual type: "; Ppil.pp_con con; print "\n")
	end

    (* ----- creates a let binding construct ----------------------------------
     It returns 
     (1) a bnd option that should (if present) wrap the exp to be bound
     (2) a var/type pair (with externally available symbol)
           to augment the arm with so that eventual
           compilation will occur in the right context.
	   If the let was created for case_exps, then the augmentation is unnecessary.
     ------------------------------------------------------------------------ *)

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

    fun wrapbnds ([] : bnds, e : exp) = e
      | wrapbnds (bnds, e) = U.make_let (bnds, e)
    fun wrapbnd (b, e) = wrapbnds([b],e)


    fun letprojecthelp (rv : Il.var,
			vars : var list, cons : con list, 
			labels : label list)
      : Il.bnds =
      let val rcon = CON_RECORD(U.sort_labelpair(Listops.zip labels cons))
	  val bnds = Listops.map2 (fn (v,l) => BND_EXP(v,RECORD_PROJECT(VAR rv,l,rcon))) (vars,labels)
      in  bnds
      end

  (* ------------- MAIN ROUTINE --------------------------------------
   compile takes a list of arguments the entire match is applied to
   and a list match-rules, each of which consists of
   a list of (pattern list * bound var * Ast.exp) that represents pattern 
   sequence for that arm. The option denotes the to-be-compiled body 
   of the rule if the match succeeds.
     If the option is NONE, the return value should be a tuple of the 
     variables bound by that pattern.  If there is only one variable,
     then it is NOT tupled up.  To be well-typed, 
     the option can be NONE only if there is one match-rule.
     
     The result is a case expression with its type.  
     Also returned is a list, one for each match-rule supplied, 
     of the variables and their types bound in each pattern.
    -------------------------------------------------------------- *)

    local
	fun check context actual_c constrain_c = 
	    if (IlStatic.eq_con(context,actual_c, constrain_c))
		then ()
	    else (Error.error_region();
		  print "Actual type: "; Ppil.pp_con actual_c; print "\n";
		  print "Pattern type: "; Ppil.pp_con constrain_c; print "\n")

	fun extendBound((s,(v,c) : arg), bound : bound) : bound = 
	    let fun notshadow (s2,_,_) = not(Symbol.eq(s,s2))
		val bound = List.filter notshadow bound
		val bound = (s,v,c)::bound
	    in  bound
	    end
	
    in
	
	fun extendContext(ctxt, bound : bound) : context = 
	    let fun folder((s,v,c), ctxt) = C.add_context_sdec(ctxt, SDEC(N.symbol_label s,
									  DEC_EXP(v, c, NONE, false)))
	    in  foldl folder ctxt bound
	    end

	fun extendBaseRule context (baseRule : baseRule, pat : pattern, arg : arg) : baseRule =
	    let val (basePats, bound, body) = baseRule
		val (_,actual_c) = arg
		val (syms, cons, basePat) = pat
		val _ = app (check context actual_c) cons
		val bound = foldl (fn (s,bound) => extendBound((s, arg), bound)) bound syms
	    in  (basePat :: basePats, bound, body)
	    end

	fun reduceRule context (rule : rule, args : arg list) : baseRule =
	    let val (pats, bound, body) = rule
		fun folder (((syms, cons, bp), arg as (_, actual_con)), bound) = 
		    let val _ = app (check context actual_con) cons
			val bound = foldl (fn (s,bound) => extendBound((s, arg), bound)) bound syms
		    in  (bp, bound)
		    end
	    val (basePats,bound) = Listops.foldl_list folder bound (Listops.zip pats args)
	in  (basePats, bound, body)
	end

    end

 (* ------------------------------------------------------------------------------
    various cases to handle certain collected pattern types 
    --------------------------------------------------------------------------- *)

    fun listExtract _ [] = error "listExtract given list too short"
      | listExtract 0 (a::rest) = (a,rest)
      | listExtract n (a::rest) = 
	let val (elem,remain) = listExtract (n-1) rest
	in  (elem, a::remain)
	end
    fun ruleExtract n (pats, bound, body) = 
	let val (targetPat, pats) = listExtract n pats
	in  (targetPat, (pats, bound, body))
	end

    fun record_ref_dispatch (extractFun,n,args,arms) = 
	let (* Can never fail -- type error otherwise *) 
	    val (targetArg, restArgs) = listExtract n args
	    fun mapper rule = 
		let val (targetPat, rule) = ruleExtract n rule
		    val info = extractFun targetPat
		in  (info, rule)
		end
	    val info_arms = map mapper arms
	in  (targetArg,restArgs,info_arms)
	end

    (* -----------------------------------------------------------------
     Input:  (1) column number n
             (2) a value-returning predicate on base patterns,
	     (3) a list of arguments
	     (4) a list of baseRules
     Output: (1) the nth argument
             (2) the remaining arguments
	     (3) maximal prefix of given baseRules that satisfy the predicate
	         along with the value returned by the predicate
	     (4) the remaining baseRules
     ---------------------------------------------------------------- *)
    fun getPrefix (n, patpred, args, arms)
	: (arg * arg list * ('a * baseRule) list * baseRule list) = 
	let val (targetArg, restArgs) = listExtract n args
	    fun loop [] acc = (rev acc,[])
	      | loop (arms as (rule::restRules)) acc =
		let val (targetPat, rule') = ruleExtract n rule
		in  (case (patpred targetPat) of
			 SOME obj => loop restRules ((obj,rule')::acc)
		       | NONE => (rev acc,arms))
		end
	    val (info_arms, unmatchedArms) = loop arms []
	in (targetArg, restArgs, info_arms, unmatchedArms)
	end

    fun eqLiteral (Int lit, Int lit') = TilWord64.equal(lit, lit') 
      | eqLiteral (Word lit, Word lit') = TilWord64.equal(lit, lit') 
      | eqLiteral (String str, String str') = str = str'
      | eqLiteral (Char str, Char str') = str = str'
      | eqLiteral _ = error "eqLiteral: pattern is not literal"
    fun conLiteral (Int lit) = CON_INT W32
      | conLiteral (Word lit) = CON_UINT W32
      | conLiteral (String str) = CON_VECTOR(CON_UINT W8)
      | conLiteral (Char str) = CON_UINT W8
      | conLiteral _ = error "conLiteral: pattern is not literal"
    fun isLiteral (Int lit) = true
      | isLiteral (Word lit) = true
      | isLiteral (String str) = true
      | isLiteral (Char str) = true
      | isLiteral _ = false

    fun equalerLiteral (context, v, basePat) = 
	(case basePat of
	     Int lit =>
		 let val const = int(W32, lit)
		 in  PRIM (eq_int W32,[],[VAR v,SCON const])
		 end
	   | Word lit =>
		 let val const = uint(W32, lit)
		 in  ILPRIM (eq_uint W32,[],[VAR v,SCON const])
		 end
	   | String str =>
		 let fun mapper c = SCON(uint(W8,TilWord64.fromInt (ord c)))
		     val str = SCON(vector (CON_UINT W8, Array.fromList(map mapper (explode str))))
		     val string_eq_label = N.to_eq(N.symbol_label (Symbol.tycSymbol "string"))
		     val string_eq = (case (C.Context_Lookup_Label(context,string_eq_label)) of
					  SOME(_,PHRASE_CLASS_EXP(e,_,_,_)) => e
					| _ => error "string-equality undefined")
		 in  APP(string_eq, U.exp_tuple[VAR v, str])
		 end
	   | Char cstr =>
		 let val char = Util.CharStr2char cstr
		 in  ILPRIM (eq_uint W8,[], [VAR v,SCON(uint(W8,TilWord64.fromInt (ord char)))])
		 end
	   | _ => error "equalerLiteral given non-literal pattern")


(* XXXXXXXXXXXXXXXXXXXXXXXXX
 code snippet that inlines string equality
			    fun equaler v = 
			    let val len = size ss
				val lenbool = ILPRIM(eq_uint W32,[],
						   [PRIM(length_table (IntVector W8),[],[VAR v]),
						    SCON(uint (W32, TilWord64.fromInt len))])
				val z = chr 0
				val v' = N.fresh_var()
				fun loop _ [] = true_exp
				  | loop n [a] = loop n [a,z,z,z]
				  | loop n [a,b] = loop n [a,b,z,z]
				  | loop n [a,b,c] = loop n [a,b,c,z]
				  | loop n (a::b::c::d::rest) = 
				    let fun shift sh ch = TilWord64.lshift(TilWord64.fromInt(ord ch),sh)
					val (a,b,c,d) = if (!(Stats.bool "littleEndian"))
								then (a,b,c,d)
								else (d,c,b,a)
					val (a,b,c,d) = (shift 0 a, shift 8 b, shift 16 c, shift 24 d)
					val e = TilWord64.orb(TilWord64.orb(a,b),TilWord64.orb(c,d))
					val match = ILPRIM(eq_uint W32,[],
							 [SCON(uint (W32, e)),
							  PRIM(sub (IntVector W32),[],
							       [VAR v', 
								SCON(uint (W32, TilWord64.fromInt n))])])
				    in  (case rest of
					     [] => match
					   | _ => make_ifthenelse(match,loop (n+1) rest,
								  false_exp, con_bool))
				    end
				val content_bool = 
				    U.make_let([BND_EXP(v',PRIM(uintv2uintv (W8,W32),[],[VAR v]))],
					loop 0 (explode ss))
			    in  make_ifthenelse(lenbool,content_bool,false_exp,con_bool)
			    end
XXXXXXXXXXXXXXXXXXXXXXXXXXX *)


    fun wild_case (col, context, args, arms, def, resCon) : exp = 
	let val _ = debugdo (fn () => print "wild_case\n")
	    fun wildpred Wild = SOME ()
	      | wildpred _ = error "must have Wild here"
	    val (targetArg,argRest,info_arms,unmatchedArms) = getPrefix(col,wildpred,args,arms)
	    val _ = if (null unmatchedArms)
			then ()
		    else error "must have all Wilds here"
	    val arms = map #2 info_arms
	in  match(context, argRest, arms, def, resCon)
	end

    and ref_case (col, context, args, arms, def, resCon) : exp = 
	let val _ = debugdo (fn () => print "ref_case\n")
	    fun extractRefInfo (Ref pattern) = pattern
	      | extractRefInfo Wild = ([], [], Wild)
	      | extractRefInfo _ = error "must have ref or wild here"
	    val (targetArg, restArgs, info_arms) = record_ref_dispatch(extractRefInfo,col,args,arms)
	    val elemcon = U.fresh_con context
	    val (var,con) = targetArg
	    val _ = if (IlStatic.eq_con(context, con, CON_REF elemcon))
			then ()
		    else (Error.error_region();
			  print "ref pattern used on a non-ref argument\n")
	    val v = N.fresh_var()
	    val bnd = BND_EXP(v,ILPRIM(deref,[elemcon],[VAR var]))
	    val newarg = (v,elemcon)
	    val newargs = newarg::restArgs
	    val context = C.add_context_dec(context,DEC_EXP(v,elemcon,NONE, false))
	    val newarms = map (fn (p,rule) => extendBaseRule context(rule, p, newarg)) info_arms
	    val ec = match(context,newargs,newarms,def,resCon)
	in  wrapbnd(bnd, ec)
	end

  and record_case (col, context, args, arms, def, resCon) : exp = 
    let val _ = debugdo (fn () => print "record_case\n")
	fun extractRecordInfo(Record {fields,flexibility}) = (fields, flexibility)
	  | extractRecordInfo Wild = ([], true)
	  | extractRecordInfo _ = error "must have record or wild here"
	val (targetArg, restArgs, info_arms) = record_ref_dispatch (extractRecordInfo,col,args,arms)
	fun is_subset ([],s2) = true
	  | is_subset (r1::s1,s2) = Listops.member_eq(N.eq_label,r1,s2) andalso is_subset(s1,s2)
	fun same(s1,s2) = if (is_subset(s1,s2) 
			      andalso is_subset(s2,s1))
			      then s2
			  else error "tuprec_case failed same"
	fun subset(s1,s2) = if (is_subset(s1,s2)) 
				then s2 
			    else error "tuprec_case failed subset"
	fun merge([],s2) = s2
	  | merge(s1::r1,s2) = if (Listops.member_eq(N.eq_label,s1,s2))
				   then merge(r1,s2)
			       else merge(r1,s1::s2)
	fun unique (splist : (label * pattern) list) = 
	    let fun folder((l,_),acc) = if (Listops.member_eq(N.eq_label,l,acc))
					    then error "tuprec_case failed due to duplicate field names"
					else l::acc
	    in foldr folder [] splist (* foldr preserves the order *)
	    end
	fun folder (((splist : (label * pattern) list,flag),_),(syms,flex)) = 
	    (case (flag,flex) of
		 (false,false) => (same(unique splist,syms),false)
	       | (true,false) => (subset(unique splist,syms),false)
	       | (false,true) => (subset(syms,unique splist),false)
	       | (true,true) => (merge(unique splist,syms),true))
	val (syms,flex) = foldl folder ([],true) info_arms
	fun fetch splist s : pattern = (case Listops.assoc_eq(N.eq_label,s,splist) of
					    SOME p => p
					  | NONE => ([], [], Wild))
	fun acc_normalize((splist,flex),arm) = (map (fetch splist) syms, arm)
	val accs' = map acc_normalize info_arms
	val (var1,con1) = targetArg
	val rvars = map (fn _ => N.fresh_var()) syms
	val rcons = map (fn _ => U.fresh_con context) syms
 	val rbnds = letprojecthelp(var1,rvars,rcons,syms)
	val lc = U.sort_labelpair (Listops.zip syms rcons)
	val argcon = if flex 
			 then CON_FLEXRECORD(ref (FLEXINFO(Tyvar.new_stamp(),false,lc)))
		     else CON_RECORD lc
	val _ = if (IlStatic.eq_con(context,con1,argcon))
		    then ()
		else (Error.error_region();
		      print "tuple/record pattern used on a non-record argument\n";
		      print "Actual type: "; Ppil.pp_con con1; print "\n";
		      print "Pattern type: "; Ppil.pp_con argcon; print "\n")
	val newargs = Listops.zip rvars rcons
	val context = foldl (fn ((v,c),ctxt) => C.add_context_dec(ctxt,DEC_EXP(v,c,NONE, false))) context newargs
	(* foldr because we concatenate new args to front of list *)
	fun extender (pats,rule) = foldr (fn ((p,arg),rule) => extendBaseRule context (rule, p, arg))
	                             rule (Listops.zip pats newargs)
	val newarms = map extender accs'
	val e = match(context,newargs @ restArgs,newarms,def,resCon)
    in  wrapbnds(rbnds,e)
    end

  and exn_case(col, context, args, arms, def, resCon, selector) : exp = 
      let val _ = debugdo (fn () => print "exn_case\n")
	  val (pat_arms, unmatchedArms) = selector(col, arms)
	  val info_arms = map (fn (Exception info, arm) => (info, arm)
			        | _ => error "must have exception here") pat_arms
	  val (targetArg, restArgs) = listExtract col args
	  val def = (case unmatchedArms of
			 [] => def
		       | _ => fn () => match(context,args,unmatchedArms,def,resCon))
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
				   val context = C.add_context_dec(context,DEC_EXP(v,con,NONE, false))
			       in  match(context, arg::restArgs, [arm'], def, resCon)
			       end)
		  val body = #1(U.make_lambda(v,con,!(#shortCon resCon),e))
	      in (stamp,con,body)
	      end
	  val (exnvar,exncon) = targetArg
	  val _ = if (IlStatic.eq_con(context,exncon,CON_ANY))
		      then ()
		  else (Error.error_region();
			print "exception pattern used on a non-exception argument\n";
			print "Argument type: "; Ppil.pp_con exncon; print "\n")
	  val exnarg = VAR exnvar
	  val arms' : (exp * con * exp) list = map helper info_arms
	  val default' = 
	      let val e = def()
	      in  compress_expression e 
	      end
      in  EXN_CASE{arg=exnarg, arms=arms', default = default', tipe = !(#shortCon(resCon))}
      end


  and constructor_case (col, context, args, arms, def, resCon, selector) : exp = 
    let val _ = debugdo (fn () => print "constructor_case\n")
      val (pat_arms, unmatchedArms) =  selector(col, arms)
      val info_arms = map (fn (Constructor info, arm) => (info, arm)
			    | _ => error "must have constructor here") pat_arms
      val (targetArg, restArgs) = listExtract col args
      val def = (case unmatchedArms of
		     [] => def	
		   | _ => fn () => match(context,args,unmatchedArms,def,resCon))
      val (casevar, casecon) = targetArg
      val casearg = VAR casevar
      val rescon_var = N.fresh_named_var "rescon_var"
      val rescon = ref(U.fresh_con context)
      val rsvar = N.fresh_named_var "sumswitch_arg"

      val ((ast_path,_),_)::_ = info_arms
      val {instantiated_type = datacon,
	   instantiated_sumtype = sumtype,
	   arms = constr_patconopt_list,
	   expose_exp} = Datatype.instantiate_datatype_signature(context,ast_path,polyinst)

      local
	  val (names,noncarriers,carrier) = 
	      (case (IlStatic.con_head_normalize(context,sumtype)) of
		   CON_SUM{names,noncarriers,carrier,special} => (names,noncarriers,carrier)
		 | c => (print "sumcon not reducible to SUM_CON: ";
			 Ppil.pp_con c; print "\n";
			 error "sumcon not reducible to SUM_CON"))
      in  fun mk_ssumcon i = CON_SUM{names=names,
				     noncarriers=noncarriers,
				     carrier=carrier,
				     special=SOME i}
      end


      local
	  val SOME(_,PHRASE_CLASS_EXP(_,actualtype,_,_)) = C.Context_Lookup_Var(context,casevar)
      in  val jopt = (case actualtype of
			  CON_SUM{names,noncarriers,carrier,special} => special
			| _ => NONE)
      end


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
		   | (true, _, _) => error "value-carrying vs non-value-carrying mismatch")
	    val relevants : baseRule list = List.mapPartial armhelp info_arms

	    val context = C.add_context_dec(context,DEC_EXP(casevar,mk_ssumcon i,NONE, false))

	in  if (case jopt of
		    NONE => false
		  | SOME j => i <> j) then NONE
	    else (case (relevants, argopt) of
	      ([],_) => NONE
	    | (_,NONE) => let val me = match(context, restArgs, relevants, def, resCon)
			  in  compress_expression me 
			  end
	    | (_,SOME (newArg as (var,rcon))) => 
			  let val context = C.add_context_dec(context,DEC_EXP(var,rcon,NONE,false))
			      val me = match(context,newArg::restArgs, relevants, def, resCon)
			  in SOME(U.make_let([BND_EXP(var,SUM_TAIL(i,sumtype,VAR rsvar))],me))
			  end)
	end


      val expopt_list = Listops.mapcount getarm constr_patconopt_list

      val exhaustive = List.all (fn NONE => false | SOME _ => true) expopt_list

      val arg = APP(expose_exp,casearg)
      val arg = (case IlUtil.exp_reduce (context, arg) of
		     NONE => arg
		   | SOME e => e)

      val case_exp = 
	  (CASE{sumtype=sumtype,
		arg = arg,
		bound=rsvar,
		arms = expopt_list,
		default = 
		if exhaustive
		    then NONE
		else 
		    let val e = def()
		    in  compress_expression e
		    end,
		tipe = !(#shortCon(resCon))})

    in   case_exp
    end


    and constant_case(col, context, args, arms, def, resCon, selector) : exp = 
	let val _ = debugdo (fn () => print "constant_case\n")
	    val (info_arms, unmatchedArms) =  selector(col, arms)
	    val def = (case unmatchedArms of
			   [] => def
			 | _ => fn () => match(context,args,unmatchedArms,def,resCon))
	    val (targetArg, restArgs) = listExtract col args
	    val (var,con) = targetArg
	    fun matchOne [] = error "matchOne given no rules"
	      | matchOne (info_arms as (targetPat,_)::_) = 
		let val litCon = conLiteral targetPat
		    val _ = if (IlStatic.eq_con(context,litCon,con))
				then ()
			    else (Error.error_region();
				  print "base type mismatches argument type\n")
		    fun pred (p, _) = eqLiteral(p,targetPat)
		    val (matches,mismatches) = List.partition pred info_arms
		    val match_rules = map #2 matches
		    val e = match(context, restArgs, match_rules, def, resCon)
		in  (targetPat, e, mismatches)
		end

	    fun matchAll acc [] = rev acc
	      | matchAll acc info_arms = 
		let val (pat, e, rest) = matchOne info_arms
		in  matchAll ((pat,e)::acc) rest
		end

	    val pat_e_list = matchAll [] info_arms
	    val resultType = !(#shortCon resCon)
	    fun folder((pat,e), rest) = 
		U.make_ifthenelse context (equalerLiteral(context,var,pat), e, rest, resultType)

	in  foldr folder (def()) pat_e_list
	end

  and match (context,
	     args : arg list,
	     arms : baseRule list,
	     def : default, 
	     resCon : resCon) : exp = 
      let 

	  val _ = debugdo (fn () =>
			   let in
			       print "\nMATCH called with "; 
			       printint (length args); print " args:\n";
			       Listops.mapcount (fn(i,arg) => (print "  ARG #"; printint i; print ": "; 
							       pp_arg arg; print "\n")) args;
			       print "\nand with "; printint (length arms); print " arms:\n";
			       Listops.mapcount (fn(i,br) => (print "  ARM #"; printint i; 
							      print ": "; pp_baseRule br; print "\n")) arms
			   end)

	  (* Choose rules until pred is satisfied. First rule MUST fail predicate. *)
	  fun untilPred (pred,col,rules) = 
	      let fun loop acc [] = (rev acc, [])
		    | loop acc (rules as (rule::rest)) =
		       let val split as (selfPat, reducedRule) = ruleExtract col rule
			   val (otherPats,_,_) = reducedRule
		       in  if (pred(selfPat,otherPats))
			       then (rev acc, rules)
			   else loop (split::acc) rest
		       end
		  val (matched, unmatched) = loop [] rules
		  val _ = (case matched of
			       [] => error "untilPred: matched empty"
			     | _ => ())
	      in  (matched, unmatched)
	      end
	  fun untilSelfWild (col,rules) = 
	      let fun isSelfWild (Wild,_) = true
		    | isSelfWild (bp, _) = false
	      in  untilPred (isSelfWild,col,rules)
	      end
	  fun anyNonwild pats = Listops.orfold (fn Wild => false | bp => true) pats
	  fun untilOtherNonwild (col,rules) = 
	      let fun isOtherNonwild (_, otherPats) = anyNonwild otherPats
	      in  untilPred (isOtherNonwild,col,rules)
	      end
	  fun chooseAnyWild (col,rules) = 
	      let 
		  fun ruleHasWild(pats,_,_) = Listops.orfold (fn Wild => true | _ => false) pats
		  val rulesWithWild = List.filter ruleHasWild rules
		  fun getPat rule = #1(ruleExtract col rule)
		  val targetPats = map getPat rulesWithWild
		  fun pred rule = 
		      let val (curPat,_) = ruleExtract col rule
		      in  Listops.member_eq(eqLiteral, curPat, targetPats)
		      end
		  val (matches,mismatches) = List.partition pred rules
		  val info_arms = map (ruleExtract col) matches
	      in  (info_arms, mismatches)
	      end

      in
	  case (arms,args) of
	      ([],_) => def()
	    | ([([],bound,body)], []) => let val (e,c) = body(context,bound)
					     val _ = check_rescon(context, resCon, c)
					 in  e
					 end
	    | (_, []) => (Error.error_region();
			  print "Redundant matches.\n";
			  debugdo (fn () => Ppil.pp_context context);
			  #1(Error.dummy_exp (context,"RedundantMatch")))
	    | _ =>
		  let
		      val matrix = map #1 arms
		      val transposedMatrix = Listops.transpose matrix
		      val colTypes = map computeColumnType transposedMatrix

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
				      val (_, (otherPats, _, _)) = ruleExtract maxCol rule
				      val selector = if (anyNonwild otherPats orelse ((length args) = 1))
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

		      val ec = lookForWILD()

		  in ec
		  end
      end (* end of match *)



 fun compile (context : context,
	      compile_args : arg list, 
	      compile_arms : (Ast.pat list * Ast.exp) list,
	      default_exn : exp)
     : exp * con = 
    let
	val _ = debugdo (fn () => print "--------------- COMPILE called -----------------\n\n")
	val fullCon = ref (U.fresh_named_con (context, "fullResultType"))
	val shortCon = if (!do_result_type)
			   then ref(CON_VAR(N.fresh_named_var "shortResultType"))
		       else fullCon
	val resCon = {fullCon = fullCon, shortCon = shortCon}
	fun mapper (clause,e) =
	    let val r = ref([], NONE)
		val pats = map (pat2Pattern context) clause
		fun body (ctxt,bound) =
		    let val ctxt = extendContext(ctxt,bound)
			val one = Util.oneshot()
			val carried as (_,(e,c,va)) = 
			    (case #2(!r) of
				 NONE => (bound,expcompile(ctxt,e))
			       | SOME t => t)
			val _ = r := ((one,bound)::(#1(!r)), SOME carried)
			val e = OVEREXP(c,va,one) 
		    in  (e,c)
		    end
		val rule = (pats,[],body) 
	    in  (r, rule)
	    end
	val (list,rules) = Listops.unzip (map mapper compile_arms)
	    
	fun wrapper ec =
	    let fun folder(ref(one_bounds,SOME(bound,(e,c,va))),bnds) = 
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
			val (lambda,funcon) = U.make_lambda(argvar,argcon,!(#shortCon resCon),e)
			val bnd = BND_EXP(funvar,lambda)
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
		  | folder(ref([],NONE),bnds) = bnds
		  | folder(ref(_,NONE),bnds) = error "non-empty oneshot list with NONE"
		val bnds = foldl folder [] list
		val res = wrapbnds(bnds,ec)
	    in  res
	    end

	val baseRules = map (fn r => reduceRule context (r, compile_args)) rules
	fun default _ = RAISE(!shortCon, default_exn)
	val almost_e = match(context,compile_args,baseRules,default,resCon)
	val very_nearly_e = wrapper very_nearly_e
	val final_e = (case (!do_result_type, !shortCon) of
			   (true, CON_VAR v) => U.make_let([BND_CON(v,!fullCon)], very_nearly_e)
			 | _ => very_nearly_e)

    in  (final_e, !fullCon)
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
          | RecordPat {def:(symbol * pat) list, flexibility:bool} => 
		 Listops.flatten(map (fn (_,p) => get_bound context p) def)
          | ListPat p => Listops.flatten(map (get_bound context) p)
          | TuplePat p => Listops.flatten(map (get_bound context) p)
	  | FlatAppPat patfixes => error "flatapppat should be parsed away"
          | AppPat {constr:pat, argument:pat} => get_bound context argument
          | ConstraintPat {pattern:pat, constraint:ty} => get_bound context  pattern
          | LayeredPat {varPat:pat, expPat:pat} => (get_bound context varPat) @ (get_bound context expPat)
          | VectorPat p => Listops.flatten(map (get_bound context) p)
          | MarkPat (p,r) => get_bound context p)
	end


    (* ============ client interfaces =========================== *)
    (* ---- bindCompile creates bindings ----------------------- *)
    fun bindCompile {context : context,
		     bindpat : Ast.pat, 
		     arg = (argvar : var, argc : con)}
                    : (sbnd * sdec) list =
      let 
	val boundsyms = get_bound context bindpat

	val context = C.add_context_dec(context,DEC_EXP(argvar,argc,NONE,false))
	val args = [(argvar,argc)] 
	val astTuple = Ast.TupleExp(map (fn s => Ast.VarExp [s]) boundsyms)
	val arms = [([bindpat],astTuple)]
	val default_exn = U.matchexn_exp
	val (binde,bindc) = compile(context,args,arms,default_exn)
	val binde = reduce_expression binde


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
			 | c => (Ppil.pp_con c; print "\n"; error "bindc not a tuple"))
	fun mapper(n,s) = 
	    let val l = N.symbol_label s
		val v = N.gen_var_from_symbol s
		val (fieldLabel, fieldCon) = List.nth(lc_list,n)
		val fieldExp = RECORD_PROJECT(base, fieldLabel ,bindc)
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
      end



    (* ---- caseCompile compiles a case expression in to an Il.exp/Il.con --- *)
    fun caseCompile {context : context,
		     arms = cases : (Ast.pat * Ast.exp) list, 
		     arg = (argvar : Il.var, argc : Il.con),
		     reraise}
      : Il.exp * Il.con = 
      let 
	val args = [(argvar,argc)]
	val context = C.add_context_dec(context,DEC_EXP(argvar,argc,NONE,false))
	val arms = map (fn (pat,body) => ([pat], body)) cases
	val default_exn = 
	    if reraise
		then let val (v,c)::_ = args
			 val _ =  if (IlStatic.eq_con(context,c,CON_ANY))
				      then ()
				  else (Error.error_region();
					print "default of pattern not an exn type\n")
		     in  VAR v
		     end
	    else U.matchexn_exp

      in  compile (context,args,arms,default_exn)
      end


    (* ---- funCompile creates a curried function ----------------------- *)
    fun funCompile {context : context,
		    rules : (Ast.pat list * Ast.exp) list}
      : {arglist : (Il.var * Il.con) list, body : Il.exp * Il.con} =
      let 
	(* -------- we begin by creating arguments of the same name as the 
	   -------- as the variable pattern when possible; just for legibility *)
	local
	  fun getname [] = "mvar"
	    | getname ((Ast.VarPat[s])::rest) = if Datatype.is_constr context [s]
						    then getname rest
						else Symbol.name s
	    | getname (_::rest) = getname rest
	  fun getnames ([]::_) = []
	    | getnames clauses = (getname (map hd clauses)) :: (getnames (map tl clauses))
	  val clauses = map #1 rules
	  val names = getnames clauses
	in 
	  val args = map (fn s => (N.fresh_named_var s,
				   U.fresh_named_con (context,s))) names
	end
	(* ---- call main routine to get compiled body; creating arms by
	   ---- adding context entries to reflect these arguments ----- *)
	val context = foldl (fn ((v,c),ctxt) => C.add_context_dec(ctxt,DEC_EXP(v,c,NONE,false))) context args
	val default_exn = U.matchexn_exp
	val (e,c) = compile (context,args,rules,default_exn)
		    
      in {arglist = args, body = (e,c) }
      end

  end

