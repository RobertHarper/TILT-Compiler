(*
   tvclose

   Bind open type variables in ValDec, ValRecDec, and FunDec declarations.

   This is a two-pass operation in which tyvar bindings (tyvar list
   refs) are destructively modified:

   1) Close all val declarations in a bottom-up sweep, using TempTyv's
      rather then Tyv's to distinguish the added bindingd from any
      specified in the source.  Later these bindings may turn out to
      be unnecessary and be removed.

   2) Do a top-down pass, maintaining the type variable environment as
      we go. All TempTyv's encountered in val declaration tyvar
      bindings that are already bound are eliminated, and those
      remaining are converted to Tyv's.

*)

signature TVCLOSE =
sig
  val closeDec: Ast.dec -> unit
end

structure TVClose: TVCLOSE =
struct
  open Ast

  structure TVSet =
    struct
      infix < >
      fun op< (s0,s1) = Int.< (Symbol.number s0, Symbol.number s1)
      fun op> (s0,s1) = Int.> (Symbol.number s0, Symbol.number s1)

      fun enter(new,l) =
	let fun f [] = [new]
	      | f (l as h::t) = if new<h then new::l
				else if new>h then h::f t else l
	in  f l
	end

      fun uniq l =
	    let fun loop([],acc) = acc
		  | loop(a::r,acc) = loop(r,enter(a,acc))
	    in loop(l,[])
	    end

      fun merge (a,[]) = a
	| merge([],a) = a
	| merge(l as i::a, m as j::b) = 
	    if j<i then j::merge(l,b) else i::merge(a,if i<j then m else b)

      fun union [] = []
	| union [l] = l
	| union (l::rest) = merge (l, union rest)

      fun remove(x as xl::xr, y as yl::yr) =
	    if xl>yl then yl::remove(x,yr)
	    else remove(xr,if xl<yl then y else yr)
	| remove(_,y) = y

    end

  fun pass1_exp (VarExp path) = []
    | pass1_exp (FnExp rules) = TVSet.union (map pass1_rule rules)
    | pass1_exp (FlatAppExp fixitems) =
        TVSet.union (map (pass1_exp o #item) fixitems)
    | pass1_exp (AppExp {function, argument}) =
	TVSet.merge (pass1_exp function, pass1_exp argument)
    | pass1_exp (CaseExp {expr, rules}) =
	TVSet.union (pass1_exp expr :: map pass1_rule rules)
    | pass1_exp (LetExp {dec, expr}) =
	TVSet.merge (pass1_dec dec, pass1_exp expr)
    | pass1_exp (SeqExp exprs) = TVSet.union (map pass1_exp exprs)
    | pass1_exp (IntExp literal) = []
    | pass1_exp (WordExp literal) = []
    | pass1_exp (RealExp string) = []
    | pass1_exp (StringExp string) = []
    | pass1_exp (CharExp string) = []
    | pass1_exp (RecordExp symexps) =
	TVSet.union (map (pass1_exp o #2) symexps)
    | pass1_exp (ListExp exprs) = TVSet.union (map pass1_exp exprs)
    | pass1_exp (TupleExp exprs) = TVSet.union (map pass1_exp exprs)
    | pass1_exp (SelectorExp symbol) = []
    | pass1_exp (ConstraintExp {expr, constraint}) =
	TVSet.merge (pass1_exp expr, pass1_ty constraint)
    | pass1_exp (HandleExp {expr, rules}) =
	TVSet.union (pass1_exp expr :: map pass1_rule rules)
    | pass1_exp (RaiseExp expr) = pass1_exp expr
    | pass1_exp (IfExp {test, thenCase, elseCase}) =
	TVSet.union (map pass1_exp [test, thenCase, elseCase])
    | pass1_exp (AndalsoExp (expr0, expr1)) =
	TVSet.merge (pass1_exp expr0, pass1_exp expr1)
    | pass1_exp (OrelseExp (expr0, expr1)) =
	TVSet.merge (pass1_exp expr0, pass1_exp expr1)
    | pass1_exp (VectorExp exprs) = TVSet.union (map pass1_exp exprs)
    | pass1_exp (WhileExp {test, expr}) =
	TVSet.merge (pass1_exp test, pass1_exp expr)
    | pass1_exp (MarkExp (exp, region)) = pass1_exp exp
    | pass1_exp (DelayExp expr) = pass1_exp expr

  and pass1_rule (Rule {pat, exp}) = TVSet.merge (pass1_pat pat, pass1_exp exp)

  and pass1_pat WildPat = []
    | pass1_pat (VarPat path) = []
    | pass1_pat (IntPat literal) = []
    | pass1_pat (WordPat literal) = []
    | pass1_pat (StringPat string) = []
    | pass1_pat (CharPat string) = []
    | pass1_pat (RecordPat {def, flexibility}) =
        TVSet.union (map (pass1_pat o #2) def)
    | pass1_pat (ListPat pats) = TVSet.union (map pass1_pat pats)
    | pass1_pat (TuplePat pats) = TVSet.union (map pass1_pat pats)
    | pass1_pat (FlatAppPat fixitems) =
	TVSet.union (map (pass1_pat o # item) fixitems)
    | pass1_pat (AppPat {constr, argument}) =
	TVSet.merge (pass1_pat constr, pass1_pat argument)
    | pass1_pat (ConstraintPat {pattern, constraint}) =
	TVSet.merge (pass1_pat pattern, pass1_ty constraint)
    | pass1_pat (LayeredPat {varPat, expPat}) =
	TVSet.merge (pass1_pat varPat, pass1_pat expPat)
    | pass1_pat (VectorPat pats) = TVSet.union (map pass1_pat pats)
    | pass1_pat (MarkPat (pat, region)) = pass1_pat pat
    | pass1_pat (OrPat pats) = TVSet.union (map pass1_pat pats)
    | pass1_pat (DelayPat pat) = pass1_pat pat

  and pass1_strexp (VarStr path) = ()
    | pass1_strexp (StructStr dec) = (pass1_dec dec; ())
    | pass1_strexp (AppStr (path, strexpbools)) =
        app (pass1_strexp o #1) strexpbools
    | pass1_strexp (LetStr (dec, strexp)) =
	(pass1_dec dec; pass1_strexp strexp)
    | pass1_strexp (MarkStr (strexp, region)) = pass1_strexp strexp

  and pass1_fctexp (VarFct _) = ()
    | pass1_fctexp (FctFct {params, body, constraint}) = pass1_strexp body
    | pass1_fctexp (LetFct (dec, fctexp)) =
        (pass1_dec dec; pass1_fctexp fctexp)
    | pass1_fctexp (AppFct (path, strexpbools, fsigconst)) =
	app (pass1_strexp o #1) strexpbools
    | pass1_fctexp (MarkFct (fctexp, region)) = pass1_fctexp fctexp

  and pass1_dec (ValDec (vbs, tvbref)) =
        (tvbref := !tvbref @ map TempTyv (TVSet.union (map pass1_vb vbs));
	 [])
    | pass1_dec (ValrecDec (rvbs, tvbref)) =
	(tvbref := !tvbref @ map TempTyv (TVSet.union (map pass1_rvb rvbs));
	 [])
    | pass1_dec (FunDec (fbs, tvbref)) =
	(tvbref := !tvbref @ map TempTyv (TVSet.union (map pass1_fb fbs));
	 [])
    | pass1_dec (TypeDec tbs) = TVSet.union (map pass1_tb tbs)
    | pass1_dec (DatatypeDec {datatycs, withtycs}) =
	TVSet.merge (TVSet.union (map pass1_db datatycs),
		     TVSet.union (map pass1_tb withtycs))
    | pass1_dec (AbstypeDec {abstycs, withtycs, body}) =
	TVSet.union [TVSet.union (map pass1_db abstycs),
		     TVSet.union (map pass1_tb withtycs),
		     pass1_dec body]
    | pass1_dec (ExceptionDec ebs) = TVSet.union (map pass1_eb ebs)
    | pass1_dec (StrDec strbs) = (app pass1_strb strbs; [])
    | pass1_dec (AbsDec strbs) = (app pass1_strb strbs; [])
    | pass1_dec (FctDec fctbs) = (app pass1_fctb fctbs; [])
    | pass1_dec (SigDec sigbs) = []
    | pass1_dec (FsigDec fsigbs) = []
    | pass1_dec (LocalDec (dec0, dec1)) = (pass1_dec dec0; pass1_dec dec1)
    | pass1_dec (SeqDec decs) = TVSet.union (map pass1_dec decs)
    | pass1_dec (OpenDec paths) = []
    | pass1_dec (OvldDec _) = []
    | pass1_dec (FixDec {fixity: fixity, ops: symbol list}) = []
    | pass1_dec (ImportDec _) = []
    | pass1_dec (MarkDec (dec, region)) = pass1_dec dec

  and pass1_vb (Vb {pat, exp}) = TVSet.merge (pass1_pat pat, pass1_exp exp)
    | pass1_vb (MarkVb (vb, region)) = pass1_vb vb

  and pass1_rvb (Rvb {var, fixity, exp, resultty}) =
        (case resultty of
	   SOME ty => TVSet.merge (pass1_exp exp, pass1_ty ty)
	 | NONE => pass1_exp exp)
    | pass1_rvb (MarkRvb (rvb, region)) = pass1_rvb rvb

  and pass1_fb (Fb clauses) = TVSet.union (map pass1_clause clauses)
    | pass1_fb (MarkFb (fb, region)) = pass1_fb fb

  and pass1_clause (Clause {pats, resultty, exp}) =
        TVSet.union (pass1_exp exp :: 
		     (case resultty of SOME ty => pass1_ty ty | NONE => []) ::
		     map (pass1_pat o #item) pats)

  and pass1_tb (Tb {tyc, def, tyvars}) =
        TVSet.remove (TVSet.union (map pass1_tyvar tyvars), pass1_ty def)
    | pass1_tb (MarkTb (tb, region)) = pass1_tb tb

  and pass1_db (Db {tyc, tyvars, def}) =
        let fun dodef (_, SOME ty) = pass1_ty ty
	      | dodef _ = []
	in
	  TVSet.remove (TVSet.union (map pass1_tyvar tyvars),
			TVSet.union (map dodef def))
	end
    | pass1_db (MarkDb (db, region)) = pass1_db db

  and pass1_eb (EbGen {exn, etype as SOME ty}) = pass1_ty ty
    | pass1_eb (EbGen {exn, etype as NONE}) = []
    | pass1_eb (EbDef _) = []
    | pass1_eb (MarkEb (eb, region)) = pass1_eb eb

  and pass1_strb (Strb {name, def, constraint}) = pass1_strexp def
    | pass1_strb (MarkStrb (strb, region)) = pass1_strb strb

  and pass1_fctb (Fctb {name, def}) = pass1_fctexp def
    | pass1_fctb (MarkFctb (fctb, region)) = pass1_fctb fctb

  and pass1_tyvar (Tyv symbol) = [symbol]
    | pass1_tyvar (TempTyv symbol) = [symbol]
    | pass1_tyvar (MarkTyv (tyvar, region)) = pass1_tyvar tyvar

  and pass1_ty (VarTy tyvar) = pass1_tyvar tyvar
    | pass1_ty (ConTy (symbols, tys)) = TVSet.union (map pass1_ty tys)
    | pass1_ty (RecordTy symtys) = TVSet.union (map (pass1_ty o #2) symtys)
    | pass1_ty (TupleTy tys) = TVSet.union (map pass1_ty tys)
    | pass1_ty (MarkTy (ty, region)) = pass1_ty ty

  fun pass2_exp env (VarExp path) = ()
    | pass2_exp env (FnExp rules) = app (pass2_rule env) rules
    | pass2_exp env (FlatAppExp fixitems) =
        app ((pass2_exp env) o #item) fixitems
    | pass2_exp env (AppExp {function, argument}) =
	(pass2_exp env function; pass2_exp env argument)
    | pass2_exp env (CaseExp {expr, rules}) =
	(pass2_exp env expr; app (pass2_rule env) rules)
    | pass2_exp env (LetExp {dec, expr}) =
	(pass2_dec env dec; pass2_exp env expr)
    | pass2_exp env (SeqExp exprs) = app (pass2_exp env) exprs
    | pass2_exp env (IntExp literal) = ()
    | pass2_exp env (WordExp literal) = ()
    | pass2_exp env (RealExp string) = ()
    | pass2_exp env (StringExp string) = ()
    | pass2_exp env (CharExp string) = ()
    | pass2_exp env (RecordExp symexps) = app ((pass2_exp env) o #2) symexps
    | pass2_exp env (ListExp exprs) = app (pass2_exp env) exprs
    | pass2_exp env (TupleExp exprs) = app (pass2_exp env) exprs
    | pass2_exp env (SelectorExp symbol) = ()
    | pass2_exp env (ConstraintExp {expr, constraint}) = pass2_exp env expr
    | pass2_exp env (HandleExp {expr, rules}) =
	(pass2_exp env expr; app (pass2_rule env) rules)
    | pass2_exp env (RaiseExp expr) = pass2_exp env expr
    | pass2_exp env (IfExp {test, thenCase, elseCase}) =
	app (pass2_exp env) [test, thenCase, elseCase]
    | pass2_exp env (AndalsoExp (expr0, expr1)) =
	(pass2_exp env expr0; pass2_exp env expr1)
    | pass2_exp env (OrelseExp (expr0, expr1)) =
	(pass2_exp env expr0; pass2_exp env expr1)
    | pass2_exp env (VectorExp exprs) = app (pass2_exp env) exprs
    | pass2_exp env (WhileExp {test, expr}) =
	(pass2_exp env test; pass2_exp env expr)
    | pass2_exp env (MarkExp (exp, region)) = pass2_exp env exp
    | pass2_exp env (DelayExp expr) = pass2_exp env expr

  and pass2_rule env (Rule {pat, exp}) = pass2_exp env exp

  and pass2_strexp (VarStr path) = ()
    | pass2_strexp (StructStr dec) = pass2_dec [] dec
    | pass2_strexp (AppStr (path, strexpbools)) =
        app (pass2_strexp o #1) strexpbools
    | pass2_strexp (LetStr (dec, strexp)) =
	(pass2_dec [] dec; pass2_strexp strexp)
    | pass2_strexp (MarkStr (strexp, region)) = pass2_strexp strexp

  and pass2_fctexp (VarFct _) = ()
    | pass2_fctexp (FctFct {params, body, constraint}) = pass2_strexp body
    | pass2_fctexp (LetFct (dec, fctexp)) =
        (pass2_dec [] dec; pass2_fctexp fctexp)
    | pass2_fctexp (AppFct (path, strexpbools, fsigconst)) =
	app (pass2_strexp o #1) strexpbools
    | pass2_fctexp (MarkFct (fctexp, region)) = pass2_fctexp fctexp

  and rebind env tvlistref =
        let fun split_tvs [] = ([], [])
	      | split_tvs (Tyv s::rest) =
	          (case split_tvs rest of (full, temp) => (s::full, temp))
	      | split_tvs (TempTyv s::rest) =
	          (case split_tvs rest of (full, temp) => (full, s::temp))
	      | split_tvs (MarkTyv (tyv, region)::rest) = split_tvs (tyv::rest)
	    val (full, temp) = split_tvs (!tvlistref)
	    val env' = TVSet.merge (env, full)
	    val newtvs = TVSet.remove (env', temp)
	    val newbinding = TVSet.merge (full, newtvs)
	    val newenv = TVSet.merge (env', newtvs)
	in
	  tvlistref := map Tyv newbinding;
	  newenv
	end

  and pass2_dec env (ValDec (vbs, tvlistref)) =
        app (pass2_vb (rebind env tvlistref)) vbs
    | pass2_dec env (ValrecDec (rvbs, tvlistref)) =
        app (pass2_rvb (rebind env tvlistref)) rvbs
    | pass2_dec env (FunDec (fbs, tvlistref)) =
        app (pass2_fb (rebind env tvlistref)) fbs
    | pass2_dec env (TypeDec tbs) = ()
    | pass2_dec env (DatatypeDec {datatycs, withtycs}) = ()
    | pass2_dec env (AbstypeDec {abstycs, withtycs, body}) = pass2_dec env body
    | pass2_dec env (ExceptionDec ebs) = ()
    | pass2_dec env (StrDec strbs) = app pass2_strb strbs
    | pass2_dec env (AbsDec strbs) = app pass2_strb strbs
    | pass2_dec env (FctDec fctbs) = app pass2_fctb fctbs
    | pass2_dec env (SigDec sigbs) = ()
    | pass2_dec env (FsigDec fsigbs) = ()
    | pass2_dec env (LocalDec (dec0, dec1)) =
	(pass2_dec env dec0; pass2_dec env dec1)
    | pass2_dec env (SeqDec decs) = app (pass2_dec env) decs
    | pass2_dec env (OpenDec paths) = ()
    | pass2_dec env (OvldDec _) = ()
    | pass2_dec env (FixDec {fixity: fixity, ops: symbol list}) = ()
    | pass2_dec env (ImportDec _) = ()
    | pass2_dec env (MarkDec (dec, region)) = pass2_dec env dec

  and pass2_vb env (Vb {pat, exp}) = pass2_exp env exp
    | pass2_vb env (MarkVb (vb, region)) = pass2_vb env vb

  and pass2_rvb env (Rvb {var, fixity, exp, resultty}) = pass2_exp env exp
    | pass2_rvb env (MarkRvb (rvb, region)) = pass2_rvb env rvb

  and pass2_fb env (Fb clauses) = app (pass2_clause env) clauses
    | pass2_fb env (MarkFb (fb, region)) = pass2_fb env fb

  and pass2_clause env (Clause {pats, resultty, exp}) =pass2_exp env exp

  and pass2_strb (Strb {name, def, constraint}) = pass2_strexp def
    | pass2_strb (MarkStrb (strb, region)) = pass2_strb strb

  and pass2_fctb (Fctb {name, def}) = pass2_fctexp def
    | pass2_fctb (MarkFctb (fctb, region)) = pass2_fctb fctb

  fun closeDec dec = (pass1_dec dec; pass2_dec [] dec; ())

end
