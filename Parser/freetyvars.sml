(*$import Ast Symbol SortedList *)

structure FreeTypeVars =
struct
  open Ast

  fun union [] = []
    | union [l] = l
    | union (l::rest) = SortedList.merge (l, union rest)

  fun addtyvars (l, []) = l
    | addtyvars (l, (Tyv symbol)::tvs) =
        addtyvars (SortedList.enter (Symbol.number symbol, l), tvs)
    | addtyvars (l, (TempTyv symbol)::tvs) =
        addtyvars (SortedList.enter (Symbol.number symbol, l), tvs)
    | addtyvars (l, (MarkTyv (tyv, region))::tyvs) = addtyvars (l, tyv::tyvs)

  fun fv_exp env (VarExp path) = []
    | fv_exp env (FnExp rules) = union (map (fv_rule env) rules)
    | fv_exp env (FlatAppExp fixitems) =
        union (map ((fv_exp env) o #item) fixitems)
    | fv_exp env (AppExp {function, argument}) =
	union [fv_exp env function, fv_exp env argument]
    | fv_exp env (CcallExp (function, arguments)) =
	union (map (fv_exp env) (function :: arguments))
    | fv_exp env (CaseExp {expr, rules}) =
	union ((fv_exp env expr) :: (map (fv_rule env) rules))
    | fv_exp env (LetExp {dec, expr}) = union [fv_dec env dec, fv_exp env expr]
    | fv_exp env (PletExp {dec, expr}) = union [fv_dec env dec, fv_exp env expr]
    | fv_exp env (SeqExp exprs) = []
    | fv_exp env (IntExp literal) = []
    | fv_exp env (WordExp literal) = []
    | fv_exp env (RealExp string) = []
    | fv_exp env (StringExp string) = []
    | fv_exp env (CharExp string) = []
    | fv_exp env (RecordExp symexps) =
	union (map ((fv_exp env) o #2) symexps)
    | fv_exp env (ListExp exprs) = union (map (fv_exp env) exprs)
    | fv_exp env (TupleExp exprs) = union (map (fv_exp env) exprs)
    | fv_exp env (SelectorExp symbol) = []
    | fv_exp env (ConstraintExp {expr, constraint}) =
	union [fv_exp env expr, fv_ty env constraint]
    | fv_exp env (HandleExp {expr, rules}) =
	union (fv_exp env expr :: map (fv_rule env) rules)
    | fv_exp env (RaiseExp expr) = fv_exp env expr
    | fv_exp env (IfExp {test, thenCase, elseCase}) =
	union (map (fv_exp env) [test, thenCase, elseCase])
    | fv_exp env (AndalsoExp (expr0, expr1)) =
	union [fv_exp env expr0, fv_exp env expr1]
    | fv_exp env (OrelseExp (expr0, expr1)) =
	union [fv_exp env expr0, fv_exp env expr1]
    | fv_exp env (VectorExp exprs) = union (map (fv_exp env) exprs)
    | fv_exp env (WhileExp {test, expr}) =
	union [fv_exp env test, fv_exp env expr]
    | fv_exp env (MarkExp (exp, region)) = fv_exp env exp
    | fv_exp env (Delay expr) = fv_exp env expr

  and fv_rule env (Rule {pat, exp}) = union [fv_pat env pat, fv_exp env exp]

  and fv_pat env WildPat = []
    | fv_pat env (VarPat path) = []
    | fv_pat env (IntPat literal) = []
    | fv_pat env (WordPat literal) = []
    | fv_pat env (StringPat string) = []
    | fv_pat env (CharPat string) = []
    | fv_pat env (RecordPat {def, flexibility}) =
        union (map ((fv_pat env) o #2) def)
    | fv_pat env (ListPat pats) = union (map (fv_pat env) pats)
    | fv_pat env (TuplePat pats) = union (map (fv_pat env) pats)
    | fv_pat env (FlatAppPat fixitems) =
	union (map ((fv_pat env) o # item) fixitems)
    | fv_pat env (AppPat {constr, argument}) =
	union [fv_pat env constr, fv_pat env argument]
    | fv_pat env (ConstraintPat {pattern, constraint}) =
	union [fv_pat env pattern, fv_ty env constraint]
    | fv_pat env (LayeredPat {varPat, expPat}) =
	union [fv_pat env varPat, fv_pat env expPat]
    | fv_pat env (MarkPat (pat, region)) = fv_pat env pat
    | fv_pat env (VectorPat pats) = union (map (fv_pat env) pats)
    | fv_pat env (OrPat pats) = union (map (fv_pat env) pats)
    | fv_pat env (DelayPat pat) = fv_pat env pat

  and fv_strexp env (VarStr path) = []
    | fv_strexp env (StructStr dec) = fv_dec env dec
    | fv_strexp env (AppStr (path, strexpbools)) =
        union (map ((fv_strexp env) o #1) strexpbools)
    | fv_strexp env (LetStr (dec, strexp)) =
	union [fv_dec env dec, fv_strexp env strexp]
    | fv_strexp env (MarkStr (strexp, region)) = fv_strexp env strexp

  and fv_fctexp env (VarFct (path, NoSig)) = []
    | fv_fctexp env (VarFct (pat, Transparent fs)) = fv_fsigexp env fs
    | fv_fctexp env (VarFct (pat, Opaque fs)) = fv_fsigexp env fs
    | fv_fctexp env (FctFct {params, body, constraint}) =
        union [union (map ((fv_sigexp env) o #2) params),
	       fv_strexp env body,
	       case constraint of
		 NoSig => []
	       | Transparent sigexp => fv_sigexp env sigexp
	       | Opaque sigexp => fv_sigexp env sigexp]
    | fv_fctexp env (LetFct (dec, fctexp)) =
	union [fv_dec env dec, fv_fctexp env fctexp]
    | fv_fctexp env (AppFct (path, strexpbools, fsigconst)) =
	union [union (map ((fv_strexp env) o #1) strexpbools),
	       case fsigconst of
		 NoSig => []
	       | Transparent fsigexp => fv_fsigexp env fsigexp
	       | Opaque fsigexp => fv_fsigexp env fsigexp]
    | fv_fctexp env (MarkFct (fctexp, region)) = fv_fctexp env fctexp

  and fv_sigexp env (VarSig symbol) = []
    | fv_sigexp env (AugSig (sigexp, syms, tyvars, ty)) =
        union [fv_sigexp env sigexp,
	       union (map (fv_tyvar env) tyvars),
	       fv_ty env ty]
    | fv_sigexp env (SigSig specs) = union (map (fv_spec env) specs)
    | fv_sigexp env (MarkSig (sigexp, region)) = fv_sigexp env sigexp

  and fv_fsigexp env (VarFsig sym) = []
    | fv_fsigexp env (FsigFsig {param, def}) =
        union [union (map ((fv_sigexp env) o #2) param),
	       fv_sigexp env def]
    | fv_fsigexp env (MarkFsig (fsigexp, region)) = fv_fsigexp env fsigexp

  and fv_spec env (StrSpec symsigexps) =
        union (map ((fv_sigexp env) o #2) symsigexps)
    | fv_spec env (TycSpec (specs, bool)) =
	let
	  fun do_spec (symbol, tyvars, NONE) =
	        union (map (fv_tyvar env) tyvars)
	    | do_spec (symbol, tyvars, SOME ty) =
		union [union (map (fv_tyvar env) tyvars), fv_ty env ty]
	in
	  union (map do_spec specs)
	end
    | fv_spec env (FctSpec symfsigexps) =
	union (map ((fv_fsigexp env) o #2) symfsigexps)
    | fv_spec env (ValSpec symtys) = union (map ((fv_ty env) o #2) symtys)
    | fv_spec env (DataSpec {datatycs, withtycs}) =
	union [union(map (fv_db env) datatycs),
	       union(map (fv_tb env) withtycs)]
    | fv_spec env (ExceSpec symtyopts) =
	union (map (fn (_,SOME ty) => fv_ty env ty | (_,NONE) => []) symtyopts)
    | fv_spec env (FixSpec  {fixity, ops}) = []
    | fv_spec env (ShareSpec paths) = []
    | fv_spec env (ShatycSpec paths) = []
    | fv_spec env (IncludeSpec symbol) = []
    | fv_spec env (MarkSpec  (spec, region)) = fv_spec env spec

  and fv_dec env (ValDec (vbs, ref tyvars)) =
        union (map (fv_vb (addtyvars (env, tyvars))) vbs)
    | fv_dec env (ValrecDec (rvbs, ref tyvars)) =
	union (map (fv_rvb (addtyvars (env, tyvars))) rvbs)
    | fv_dec env (FunDec (fbs, ref tyvars)) =
	union (map (fv_fb (addtyvars (env, tyvars))) fbs)
    | fv_dec env (TypeDec tbs) = union (map (fv_tb env) tbs)
    | fv_dec env (DatatypeDec {datatycs, withtycs}) =
        union [union (map (fv_db env) datatycs),
	       union (map (fv_tb env) withtycs)]
    | fv_dec env (AbstypeDec {abstycs, withtycs, body}) =
	union [union (map (fv_db env) abstycs),
	       union (map (fv_tb env) withtycs),
	       fv_dec env body]
    | fv_dec env (ExceptionDec ebs) = union (map (fv_eb env) ebs)
    | fv_dec env (StrDec strbs) = union (map (fv_strb env) strbs)
    | fv_dec env (AbsDec strbs) = union (map (fv_strb env) strbs)
    | fv_dec env (FctDec fctbs) = union (map (fv_fctb env) fctbs)
    | fv_dec env (SigDec sigbs) = union (map (fv_sigb env) sigbs)
    | fv_dec env (FsigDec fsigbs) = union (map (fv_fsigb env) fsigbs)
    | fv_dec env (LocalDec (dec0, dec1)) =
        union [fv_dec env dec0, fv_dec env dec1]
    | fv_dec env (SeqDec decs) = union (map (fv_dec env) decs)
    | fv_dec env (OpenDec paths) = []
    | fv_dec env (OvldDec _) = []
    | fv_dec env (FixDec {fixity: fixity, ops: symbol list}) = []
    | fv_dec env (ImportDec _) = []
    | fv_dec env (ExternDec _) = []
    | fv_dec env (MarkDec (dec, region)) = fv_dec env dec

  and fv_vb env (Vb {pat, exp}) = union [fv_pat env pat, fv_exp env exp]
    | fv_vb env (MarkVb (vb, region)) = fv_vb env vb

  and fv_rvb env (Rvb {var, fixity, exp, resultty}) =
        (case resultty of
	   SOME ty => union [fv_exp env exp, fv_ty env ty]
	 | NONE => fv_exp env exp)
    | fv_rvb env (MarkRvb (rvb, region)) = fv_rvb env rvb

  and fv_fb env (Fb clauses) = union (map (fv_clause env) clauses)
    | fv_fb env (MarkFb (fb, region)) = fv_fb env fb

  and fv_clause env (Clause {pats, resultty, exp}) =
        union [union (map ((fv_pat env) o #item) pats),
	       case resultty of SOME ty => fv_ty env ty | NONE => [],
	       fv_exp env exp]

  and fv_tb env (Tb {tyc, def, tyvars}) =
        union ((fv_ty env def) :: (map (fv_tyvar env) tyvars))
    | fv_tb env (MarkTb (tb, region)) = fv_tb env tb

  and fv_db env (Db {tyc, tyvars, def}) =
        union [union (map (fv_tyvar env) tyvars),
	       union (map (fn (_, SOME ty) => fv_ty env ty | _ => []) def)]
    | fv_db env (MarkDb (db, region)) = fv_db env db

  and fv_eb env (EbGen {exn, etype}) =
        (case etype of SOME ty => fv_ty env ty | NONE => [])
    | fv_eb env (EbDef _) = []
    | fv_eb env (MarkEb (eb, region)) = fv_eb env eb

  and fv_strb env (Strb {name, def, constraint}) =
       union [fv_strexp env def,
	      case constraint of
		NoSig => []
	      | Transparent sigexp => fv_sigexp env sigexp
	      | Opaque sigexp => fv_sigexp env sigexp]
    | fv_strb env (MarkStrb (strb, region)) = fv_strb env strb

  and fv_fctb env (Fctb {name, def}) = fv_fctexp env def
    | fv_fctb env (MarkFctb (fctb, region)) = fv_fctb env fctb

  and fv_sigb env (Sigb {name, def}) = fv_sigexp env def
    | fv_sigb env (MarkSigb (sigb, region)) = fv_sigb env sigb

  and fv_fsigb env (Fsigb {name, def}) = fv_fsigexp env def
    | fv_fsigb env (MarkFsigb (fsigb, region)) = fv_fsigb env fsigb

  and fv_tyvar env (Tyv symbol) =
        let val n = Symbol.number symbol
	in if SortedList.member env n then [] else [n] end
    | fv_tyvar env (TempTyv symbol) =
        let val n = Symbol.number symbol
	in if SortedList.member env n then [] else [n] end
    | fv_tyvar env (MarkTyv (tyvar, region)) = fv_tyvar env tyvar

  and fv_ty env (VarTy tyvar) = fv_tyvar env tyvar
    | fv_ty env (ConTy (symbols, tys)) = union (map (fv_ty env) tys)
    | fv_ty env (RecordTy symtys) = union (map ((fv_ty env) o #2) symtys)
    | fv_ty env (TupleTy tys) = union (map (fv_ty env) tys)
    | fv_ty env (MarkTy (ty, region)) = fv_ty env ty

end
