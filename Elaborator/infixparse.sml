(*$import Prelude TopLevel Fixity Symbol List Name Listops Ast Util INFIXPARSE Il Ppil AstHelp ListMergeSort Stats *)
structure InfixParse
  :> INFIXPARSE =
  struct

    structure Il = Il

    structure Symbol = Symbol
    structure Fixity = Fixity

    open Il
    open Util AstHelp Listops Name

    val debug = Stats.ff("InfixParseDebug")
    fun debugdo t = if (!debug) then (t(); ()) else ()
    val error = fn s => error "infixparse.sml" s

    (* ----------------------------------------------------------------------      
      Useful values to make application explicit
      ---------------------------------------------------------------------- *)
    val app_sym = Symbol.varSymbol("internal_app")
    val app_lab = symbol_label app_sym
    val app_exp = Ast.VarExp [app_sym]
    val app_pat = Ast.VarPat [app_sym]


    (* -----------------------------------------------------------------------------      
      Utility functions for checking if an expression or path is infix and if so
      what its fixity is.  Also convert Fixity.fixity to precedence levels(0-9)
      ----------------------------------------------------------------------------- *)
    fun fixity_to_level (Fixity.NONfix) = error "no level for NONfix"
      | fixity_to_level (Fixity.INfix (a,b)) = a div 2
    fun path_fixity_lookup [s] fm = 
	(case Name.LabelMap.find(fm,symbol_label s) of
	     NONE => NONE
	   | SOME (f as (Fixity.INfix _)) => SOME f 
	   | _ => NONE)
      | path_fixity_lookup _ _ = NONE
    fun exp_fixity_lookup table (Ast.VarExp p) = path_fixity_lookup p table
      | exp_fixity_lookup table (Ast.MarkExp (e,r)) = exp_fixity_lookup table e
      | exp_fixity_lookup _ _ = NONE
    fun pat_fixity_lookup table (Ast.VarPat p) = path_fixity_lookup p table
      | pat_fixity_lookup table (Ast.MarkPat (p,r)) = pat_fixity_lookup table p
      | pat_fixity_lookup _ _ = NONE

    (* ----------------------------------------------------------------------
      driver is the main work routine.  
      It takes a list of objects, 
	        a routine for printing objects,
		  a routine to look up the fixity/precedence of a possibly infix object,
               a predicate for whether an object is applicable,
		  a unique application object,
		  a routine to perform an application,
	        a routine to perform a tupling.
           and returns minimal list of nested AppExps and TupleExps.
      If is_applicable is true, then the list will be of length 1.
      Otherwise, implicit application is not assumed so
          that the result list may be of arbitrary length.
      ---------------------------------------------------------------------- *)
    fun driver(objlist       : 'a list, 	
	       print_obj     : 'a -> unit,
	       get_fixity    : 'a -> Fixity.fixity option, 
	       is_app        : 'a -> bool,
	       convert       : 'a -> 'b,
	       is_app_obj    : 'a -> bool , 
	       app_obj       : 'a, 
	       apper         : 'a * 'a -> 'a, 
	       tupler        : 'a list -> 'a)
      : 'b list = 
      let
	  fun get_fix obj = (case (get_fixity obj) of
					NONE => (print "obj has no fixity: ";
						 print_obj obj;
						 print "\n";
						 error "no fixity for this obj")
				      | SOME f => f)
	(* --------------------------------------------------------
	 normalize takes the FlatAppExp and returns a list of exps 
            with implicit application made explicit;
            at the same time, preclist is updated to contain only 
	     the precendence levels we need to collapse 
	     -------------------------------------------------- *)
	fun normalize (objlist : 'a list) : ('a list * int list) = 
	  let 
	    val preclist = ref []
	    fun is_infix obj =  (case (get_fixity (obj)) of
				     NONE => false
				   | SOME f => true)
	    fun add_prec e = (case (get_fixity e) of
				  NONE => ()
				| SOME f =>
				      let val level = fixity_to_level f
				      in if (List.exists (fn x => x = level) (!preclist)) then ()
					 else preclist := level::(!preclist)
				      end)
	    fun loop (e1::e2::rest) = 
		(add_prec e1;
		 if ((not (is_app e1)) orelse (is_infix e1) orelse (is_infix e2))
		     then e1::(loop (e2::rest)) else 
			 (add_prec app_obj; 
			  e1 :: app_obj :: (loop (e2 :: rest))))
	      | loop leftover = leftover
	    val objs = loop objlist
	    val _ = debugdo (fn () => (print "objlist is\n"; 
				       app (fn e => (print "  "; print_obj e; print "\n")) objlist; print "\n"))
	    val _ = debugdo (fn () => (print "objs is\n"; 
				       app (fn e => (print "  "; print_obj e; print "\n")) objs; print "\n"))
	  in  (objs, !preclist)
	  end
	(* --------------------------------------------------------
	 takes a precedence level and a list of expression and collapses the list
	 of all left AND right associative operators at that precendence level
	   -------------------------------------------------------- *)
	fun collapse cur_prec acc = 
	  let
            fun rewrite(op1,v1,v2) = 
	      let 
		val (f,a) = if (is_app_obj op1) then (v1,v2) else (op1,tupler([v1,v2]))
	      in apper(f,a)
	      end
            fun right_rewrite_list(v2::op1::v1::rest) = right_rewrite_list((rewrite(op1,v1,v2))::rest)
	      | right_rewrite_list [e] = e
	      | right_rewrite_list _ = error "right_rewrite_list got ill-formed exp list"
	     (* --------------------------------------------------------
	     The list we maintain is of the form "var op var op var ...".
	      We scan left to right until we find the first operator of that precendence.
	      If the operator is left-associative, we rewrite that operator. 
		 If the operator is right-associative, we scan oprators to the right
		 until we come to the end or find an operator that is not right-associative
		 and of the current precendence.  We take this subsequence and
		 right-rewrite it. 
		 -------------------------------------------------------- *)
	    fun scan (v1::op1::v2::rest) = (if ((get_fix op1) = Fixity.infixleft cur_prec)
					      then scan (rewrite(op1,v1,v2) :: rest)
					    else if ((get_fix op1) = Fixity.infixright cur_prec)
						   then scan(rightscan rest [v2,op1,v1])
						 else v1::op1::(scan (v2::rest)))
              | scan [e] = [e]
              | scan _ = error "scan encountered ill-formed pattern or expression"
            and rightscan (op1::v2::rest) acc = if ((get_fix op1) = Fixity.infixright cur_prec)
						   then rightscan rest (v2::op1::acc)
						 else (right_rewrite_list acc)::op1::v2::rest
	      | rightscan [] acc = [right_rewrite_list acc]
              | rightscan _ acc = error "rightscan encountered ill-formed expression"
	  in
	    scan acc
	  end

	(* -------------------------------------------------------------
	 call normalize to do implcit aplpication and compute precedence 
	 levels that are used.  then sort the precendence levels and
	 iterate over the sorted precedence levels from highest to lowest
         and calls collapse repeatedlty 
	  ------------------------------------------------------------- *)
	val (normed_list,preclist) = normalize objlist
	val descending_prec_list = ListMergeSort.sort (op <) preclist
	val res = foldl (fn (n,acc) => collapse n acc) normed_list descending_prec_list 
      in map convert res
      end


    fun parse_exp (table : Fixity.fixity Name.LabelMap.map, Ast.FlatAppExp exp_fix_list) = 
      let
	val table = Name.LabelMap.insert(table,app_lab, Fixity.infixleft 10) 
	fun apper ((_,f),(_,a)) = (false,Ast.AppExp{function=f,argument=a})
	fun tupler (args : (bool * Ast.exp) list) = (false,Ast.TupleExp(map #2 args))
	fun exp_recurse reduced_driver e = e
      in
	case driver(map (fn {item,fixity=NONE,...} => (false, item)
			  | {item,fixity=SOME _,...} => (true,item)) exp_fix_list,
		    fn (_,e) => pp_exp e,
		    fn (false,_) => NONE
		     | (true,e) => exp_fixity_lookup table e, 
		    fn _ => true,
		    fn (_,e) => e,
		    fn (true,Ast.VarExp[s]) => Symbol.eq(s,app_sym)
		     | _ => false,
		    (true,app_exp),
		    apper,
		    tupler) of
	  [e] => (debugdo (fn () => (print "e is "; pp_exp e; print "\n")); e)
	| elist => (print "done with all precedence level and still have a list of exps";
		    app (fn e => (pp_exp e; print "\n")) elist;
		    print "\n";
		    error "done with all precedence level and still have a list of exps")
      end
      | parse_exp (table : Fixity.fixity Name.LabelMap.map, e) = e

    fun parse_pat (table : Fixity.fixity Name.LabelMap.map, 
		   is_nonconst_constr : Ast.symbol list -> bool, 
		   pat_list : Ast.pat list) : Ast.pat list = 
      let
	fun self arg = parse_pat(table,is_nonconst_constr,arg)
	fun self_one arg = (case (parse_pat(table,is_nonconst_constr,[arg])) of
				[p] => p
			      | pats => (print "parse_pat on subcall yielded multiple patterns";
					 app (fn p => (AstHelp.pp_pat p; print "\n")) pats;
					 error "parse_pat on subcall yielded multiple patterns"))
	val _ = debugdo (fn () => (print "entered parse_pat\n"))
	val table = Name.LabelMap.insert(table,app_lab, Fixity.infixleft 10) 
	fun apper (f,a) = Ast.AppPat{constr=f,argument=a}
	val tupler = Ast.TuplePat
	fun is_applicable (Ast.VarPat p) = is_nonconst_constr p
	  | is_applicable _ = false
	fun help (pat : Ast.pat) : Ast.pat = 
	    (case pat of
		 Ast.WildPat  => pat
	       | Ast.VarPat _  => pat
	       | Ast.IntPat _  => pat
	       | Ast.WordPat _  => pat
	       | Ast.StringPat _  => pat
	       | Ast.CharPat _ => pat
	       | Ast.RecordPat {def,flexibility} => Ast.RecordPat{def=map (fn (s,p) => (s,self_one p)) def,
								   flexibility=flexibility}
	      | Ast.ListPat pats => Ast.ListPat (map self_one pats)
	      | Ast.TuplePat pats => Ast.TuplePat (map self_one pats)
	      | Ast.FlatAppPat fixpats => 
		 let val pats = map (fn {item,...} => item) fixpats
		     val pats' = map self_one pats
		 in case (self pats') of
		     [p] => p
		   | pats => (print "parse_pat on subcall yielded multiple patterns:\n";
			      app (fn p => (AstHelp.pp_pat p; print "\n")) pats;
			      error "parse_pat of flatapppat on subcall yielded multiple patterns")
		 end
	      | Ast.AppPat {constr,argument} => Ast.AppPat {constr=self_one constr,argument=self_one argument}
	      | Ast.ConstraintPat {pattern,constraint} => Ast.ConstraintPat {pattern=self_one pattern,
									     constraint=constraint}
	      | Ast.LayeredPat {varPat,expPat} => Ast.LayeredPat {varPat=self_one varPat, expPat=self_one expPat} 
	      | Ast.VectorPat pats => Ast.VectorPat (map self_one pats)
	      | Ast.MarkPat (p,r) => Ast.MarkPat(self_one p,r))
	  val pat_list' = map help pat_list

		   
	  val res = driver(pat_list', 
			   pp_pat,
			   pat_fixity_lookup table, 
			   is_applicable,
			   fn x => x,
			   fn (Ast.VarPat[s]) => Symbol.eq(s,app_sym)
			    | _ => false,
			   app_pat, 
			   apper, 
			   tupler)
	  val _ = debugdo (fn () => (print "leaving parse_pat\n"))
      in res
      end

    (* takes a destructured Asy.DatatypeDec and expand out the withtypes *)
    open Ast
    fun parse_datbind (datatycs: db list, withtycs: tb list) =
	let
	    fun get_dbname (Db {tyc,...}) = tyc
	      | get_dbname (MarkDb (db,_)) = get_dbname db
	    fun get_tbname (Tb {tyc,def,tyvars}) = (tyc,(def,tyvars))
	      | get_tbname (MarkTb (tb,_)) = get_tbname tb
	    val db_names = map get_dbname datatycs
	    val with_table = map get_tbname withtycs
	    val names = db_names @ (map #1 with_table)
	    fun loop (a::rest) seen = if (Listops.member_eq(Symbol.eq, a, seen))
					  then error "datbind/withbind binding duplicate variables"
				      else loop rest seen
	      | loop [] _ = ()
	    val _ = loop names []
	    fun subst_ty (handler : Ast.ty -> Ast.ty option) ty = 
		(case (handler ty, ty) of
		     (SOME res, _) => res
		   | (_,VarTy _) => ty
		   | (_,ConTy (syms, tys)) => ConTy(syms, map (subst_ty handler) tys)
		   | (_,RecordTy sym_tys) => RecordTy(map (fn (s,ty) => (s,subst_ty handler ty)) sym_tys)
		   | (_,TupleTy tys) => TupleTy (map (subst_ty handler) tys)
		   | (_,MarkTy (ty,r)) => MarkTy(subst_ty handler ty, r))
	    fun eq_tv (Tyv s1, Tyv s2) = Symbol.eq(s1,s2)
	      | eq_tv (TempTyv s1, TempTyv s2) = Symbol.eq(s1,s2)
	      | eq_tv (MarkTyv(tv1,_), tv2) = eq_tv(tv1,tv2)
	      | eq_tv (tv1,MarkTyv(tv2,_)) = eq_tv(tv1,tv2)
	      | eq_tv _ = false
	    fun varty_handler table (VarTy tv) = Listops.assoc_eq(eq_tv, tv, table)
	      | varty_handler _ _ = NONE
	    fun conty_handler (ConTy ([sym], tys)) = 
		(case (Listops.assoc_eq(Symbol.eq, sym, with_table)) of
		     NONE => NONE
		   | SOME (def,tyvars) => 
			 let val table = Listops.zip tyvars tys
			     val def' = subst_ty (varty_handler table) def
			     val def'' = subst_ty conty_handler def' 
			 in SOME def''
			 end)
	      | conty_handler _ = NONE
	    fun subst_def (s,NONE) = (s,NONE)
	      | subst_def (s,SOME ty) = (s, SOME (subst_ty conty_handler ty))
	    fun subst_rhs (rhs as (Repl _)) = rhs
	      | subst_rhs (Constrs defs) = Constrs(map subst_def defs)
	    fun subst (Db {tyc, tyvars, rhs}) = Db{tyc = tyc, tyvars = tyvars, 
						   rhs = subst_rhs rhs}
	      | subst (MarkDb(db,r)) = MarkDb(subst db, r)
	    val datatycs' = map subst datatycs
	in  (datatycs', withtycs)
	end

    val parse_pat = fn arg => SOME(parse_pat arg) handle e => NONE
    val parse_exp = fn arg => SOME(parse_exp arg) handle e => NONE
    val parse_datbind = fn arg => SOME(parse_datbind arg) handle e => NONE

  end;
