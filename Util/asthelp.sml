(* Helper routines on the EL syntax tree AST *)
structure AstHelp : ASTHELP = 
  struct

    structure Formatter = Formatter

    open Ast Util Listops Formatter

    val error = fn s => error "asthelp.sml" s

    val true_path = [Symbol.varSymbol "true"]
    val false_path = [Symbol.varSymbol "false"]
    val nil_path = [Symbol.varSymbol "nil"]
    val cons_path = [Symbol.varSymbol "::"]  (* NOT fixSymbol evn though it's infix *)
    val true_exp = Ast.VarExp true_path
    val false_exp = Ast.VarExp false_path
    val true_pat = Ast.VarPat(true_path)
    val false_pat = Ast.VarPat(false_path)
    val nil_exp = Ast.VarExp nil_path
    val cons_exp = Ast.VarExp cons_path

    fun tyvar_strip (Ast.Tyv s) = s
      | tyvar_strip (Ast.MarkTyv (tv,r)) = tyvar_strip tv
    fun db_strip (Ast.Db {tyc,tyvars,def}) = (tyc,tyvars,def)
      | db_strip (Ast.MarkDb(db,r)) = db_strip db
    fun tb_strip (Ast.MarkTb(tb,r)) = tb_strip tb
      | tb_strip (Ast.Tb {tyc,tyvars,def}) = (tyc,tyvars,def)
    fun fctb_strip (Ast.MarkFctb (fctb,r)) = fctb_strip fctb
      | fctb_strip (Ast.Fctb{name,def}) = (name,def)
    fun strb_strip (Ast.MarkStrb (strb,r)) = strb_strip strb
      | strb_strip (Ast.Strb {name,def,constraint}) = (name,(def,constraint))
    fun vb_strip (Ast.MarkVb (vb,r)) = vb_strip vb
      | vb_strip (Ast.Vb {pat,exp}) = (pat,exp)
    fun fb_strip (Ast.Fb arg) = arg
      | fb_strip (Ast.MarkFb (fb,r)) = fb_strip fb


    local
      fun is_tyvar_bound (tyvar,symlist) = member(tyvar_strip tyvar,symlist)
      fun is_var_bound ([sym],symlist) = member(sym,symlist)
	| is_var_bound _ = false
      fun f_ty (state as (doconstr, constrbound : symbol list,
			  doty, tybound : symbol list,
			  dovar, varbound : symbol list)) (ty : Ast.ty) : Ast.ty = 
	(case ty of
	   Ast.VarTy tyvar => if (is_tyvar_bound(tyvar,tybound)) then ty else doty tyvar
	 | Ast.ConTy (symlist,tylist) => let val newsyms = map (fn s => if (member(s,constrbound))
									  then s else doconstr s) symlist
					 in Ast.ConTy(newsyms,map (f_ty state) tylist)
					 end
	 | Ast.RecordTy (symty_list) => Ast.RecordTy(map (fn (s,ty) => (s, f_ty state ty)) symty_list)
	 | Ast.TupleTy tylist => Ast.TupleTy(map (f_ty state) tylist)
	 | Ast.MarkTy (ty,r) => f_ty state ty)
      fun f_exp (state as (doconstr, constrbound,
			   doty, tybound,
			   dovar, varbound : symbol list)) (exp : Ast.exp) : Ast.exp = 
	let val self = f_exp state
	in
	  (case exp of
	     Ast.VarExp var => if (is_var_bound(var,varbound)) then exp else dovar var
	     | ( Ast.IntExp _ | Ast.WordExp _ |
		Ast.RealExp _ | Ast.StringExp _ | Ast.CharExp _ |
		Ast.SelectorExp _) => exp
	 | (Ast.ListExp elist) => Ast.ListExp(map self elist)
	 | (Ast.TupleExp elist) => Ast.TupleExp(map self elist)
	 | (Ast.VectorExp elist) => Ast.VectorExp(map self elist)
	 | (Ast.SeqExp elist) => Ast.SeqExp(map self elist)
	 | (Ast.AndalsoExp (e1,e2)) => Ast.AndalsoExp(f_exp state e1,f_exp state e2)
	 | (Ast.OrelseExp (e1,e2)) => Ast.OrelseExp(f_exp state e1,f_exp state e2)
	 | (Ast.FnExp rlist) => Ast.FnExp(map (f_rule state) rlist)
	 | (Ast.FlatAppExp exp_fix_list) => Ast.FlatAppExp 
	   (map (fn {item,fixity,region} => {item=self item,fixity=fixity,region=region}) exp_fix_list)
	 | (Ast.AppExp {function,argument}) => Ast.AppExp{function=self function,
							  argument=self argument}
	 | (Ast.CaseExp {expr,rules}) => Ast.CaseExp{expr=self expr,rules=map (f_rule state) rules}
	 | (Ast.HandleExp {expr,rules}) => Ast.HandleExp{expr=self expr,rules=map (f_rule state) rules}
	 | (Ast.LetExp {dec,expr}) => Ast.LetExp{dec=f_dec state dec, expr=self expr}
	 | (Ast.RecordExp symexp_list) => Ast.RecordExp(map (fn (s,e) => (s,self e)) symexp_list)
	 | (Ast.ConstraintExp {expr,constraint}) => Ast.ConstraintExp{expr=self expr,
								     constraint=f_ty state constraint}
	 | (Ast.RaiseExp e) => Ast.RaiseExp(self e)
	 | (Ast.IfExp {test,thenCase,elseCase}) => Ast.IfExp{test=self test,
							     thenCase=self thenCase,
							     elseCase=self elseCase}
	 | (Ast.WhileExp {test,expr}) => Ast.WhileExp{test=self test,
						      expr=self expr}
	 | (Ast.MarkExp (e,r)) => Ast.MarkExp(self e,r))
	end
      and f_rule state (Ast.Rule {pat,exp}) = Ast.Rule{pat=f_pat state pat, exp=f_exp state exp}
      and f_pat state pat = 
	(case pat of
	   (Ast.WildPat | Ast.VarPat _ | Ast.IntPat _ 
                                | Ast.WordPat _ | Ast.RealPat _ | Ast.StringPat _ | Ast.CharPat _) => pat
         | (Ast.RecordPat{def,flexibility}) => Ast.RecordPat{def=(map (fn(s,p) => (s,f_pat state p)) def),
								 flexibility=flexibility}
 	 | (Ast.ListPat plist) => Ast.ListPat(map (f_pat state) plist)
	 | (Ast.VectorPat plist) => Ast.VectorPat(map (f_pat state) plist)
	 | (Ast.OrPat plist) => Ast.OrPat(map (f_pat state) plist)
	 | (Ast.TuplePat plist) => Ast.TuplePat(map (f_pat state) plist)
	 | (Ast.FlatAppPat pat_fixitem_list) =>
	         Ast.FlatAppPat(map (fn {item,fixity,region} => 
				     {item=f_pat state item,fixity=fixity,region=region}) pat_fixitem_list)
	 | (Ast.AppPat {constr,argument}) => Ast.AppPat{constr=f_pat state constr,
						    argument=f_pat state argument}
 	 | (Ast.LayeredPat {varPat,expPat}) => Ast.LayeredPat{varPat=f_pat state varPat,
								  expPat=f_pat state expPat}
	 | (Ast.ConstraintPat {pattern,constraint}) => Ast.ConstraintPat{pattern=f_pat state pattern,
									     constraint=f_ty state constraint}
	 | (Ast.MarkPat (p,r)) => Ast.MarkPat(f_pat state p,r))
      and f_dec (state as (doconstr, constrbound,
			   doty, tybound,
			   dovar, varbound : symbol list)) dec = 
	(case dec of
	  Ast.ValDec vb_list => Ast.ValDec (map (f_vb state) vb_list)
	| Ast.ValrecDec rvb_list => Ast.ValrecDec (map (f_rvb state) rvb_list)
	| Ast.FunDec fb_list => Ast.FunDec (map (f_fb state) fb_list)
	| Ast.TypeDec tb_list => Ast.TypeDec (map (f_tb state) tb_list)
	| Ast.DatatypeDec {datatycs,withtycs} => let val newconstr = map (fn db => let val (tyc,_,_) = db_strip db
										   in tyc
										   end) datatycs
						     val state = (doconstr, newconstr @ constrbound,
								  doty, tybound,
								  dovar,varbound)
						 in Ast.DatatypeDec{datatycs=map (f_db state) datatycs,
								    withtycs=map (f_tb state) withtycs}
						 end
	| Ast.AbstypeDec{abstycs,withtycs,body} => Ast.AbstypeDec{abstycs=map (f_db state) abstycs,
								  withtycs=map (f_tb state) withtycs,
								  body=f_dec state body}
	| Ast.ExceptionDec eb_list => Ast.ExceptionDec (map (f_eb state) eb_list)
	| Ast.StrDec strb_list => Ast.StrDec (map (f_strb state) strb_list)
	| Ast.AbsDec strb_list => error "Don't handle abstract structure"
	| Ast.FctDec fctb_list => Ast.FctDec (map (f_fctb state) fctb_list)
	| Ast.SigDec sigb_list => Ast.SigDec (map (f_sigb state) sigb_list)
	| Ast.FsigDec fsigb_list => Ast.FsigDec (map (f_fsigb state) fsigb_list)
	| Ast.LocalDec (d1,d2) => Ast.LocalDec(f_dec state d1, f_dec state d2)
	| Ast.SeqDec dec_list => Ast.SeqDec(map (f_dec state) dec_list)
	| Ast.OpenDec _ => dec
	| Ast.OvldDec _ => error "Don't handle overloading declaration"
	| Ast.FixDec _ => dec
	| Ast.ImportDec _ => dec
	| Ast.MarkDec (dec,r) => Ast.MarkDec(f_dec state dec,r))
      and f_vb state (Ast.Vb{pat,exp}) = Ast.Vb{pat=f_pat state pat,exp=f_exp state exp}
	| f_vb state (Ast.MarkVb(vb,r)) = Ast.MarkVb(f_vb state vb,r)
      and f_rvb state (Ast.Rvb{var,fixity,exp,resultty}) = 
	    Ast.Rvb{var=var,fixity=fixity,exp=f_exp state exp, 
		    resultty=case resultty of NONE => NONE | SOME ty => SOME(f_ty state ty)}
	| f_rvb state (Ast.MarkRvb(rvb,r)) = Ast.MarkRvb(f_rvb state rvb,r)
      and f_fb state (Ast.Fb clauses) = Ast.Fb(map (f_clause state) clauses)
	| f_fb state (Ast.MarkFb(fb,r)) = Ast.MarkFb(f_fb state fb,r)
      and f_clause state (Ast.Clause{pats,resultty,exp}) = 
		Ast.Clause{pats=map (fn {item,fixity,region} => 
				     {item=f_pat state item,fixity=fixity,region=region}) pats,
			   resultty=case resultty of NONE => NONE | SOME ty => SOME(f_ty state ty),
			   exp=f_exp state exp}
      and f_tb (state as (doconstr,constrbound,doty,tybound,dovar,varbound)) (Ast.Tb{tyc,def,tyvars}) = 
	let val newbound = tybound @ (map tyvar_strip tyvars)
	  val newstate = (doconstr,constrbound,doty,newbound,dovar,varbound)
	in Ast.Tb{tyc=tyc,def=f_ty newstate def, tyvars=tyvars}
	end
	| f_tb state (Ast.MarkTb(tb,r)) = Ast.MarkTb(f_tb state tb,r)
      and f_db (state as (doconstr,constrbound,
			  doty,tybound,
			  dovar, varbound : symbol list)) (Ast.Db{tyc,def,tyvars}) = 
	let val newbound = tybound @ (map tyvar_strip tyvars)
	  val newstate = (doconstr,constrbound,doty,newbound,dovar,varbound)
	in  Ast.Db{def=map (fn (s,SOME ty) => (s,SOME(f_ty newstate ty))
                             | (s,NONE) => (s,NONE)) def,
		   tyc=tyc,
		   tyvars=tyvars}
	end
	| f_db state (Ast.MarkDb(db,r)) = Ast.MarkDb(f_db state db,r)
      and f_eb state (Ast.EbGen{exn,etype=NONE}) = Ast.EbGen{exn=exn,etype=NONE}
	| f_eb state (Ast.EbGen{exn,etype=SOME ty}) = Ast.EbGen{exn=exn,etype=SOME (f_ty state ty)}
	| f_eb state (Ast.EbDef blob) = Ast.EbDef blob
	| f_eb state (Ast.MarkEb(eb,r)) = Ast.MarkEb(f_eb state eb,r)
      and f_strb state (Ast.Strb {name,def,constraint}) = Ast.Strb{name=name,def=f_strexp state def,
								     constraint=case constraint of
								     NONE => NONE
								   | SOME se => SOME(f_sigexp state se)}
	| f_strb state (Ast.MarkStrb(strb,r)) = Ast.MarkStrb(f_strb state strb,r)
      and f_fctb state (Ast.Fctb{name,def}) = Ast.Fctb{name=name,def=f_fctexp state def}
	| f_fctb state (Ast.MarkFctb(fctb,r)) = Ast.MarkFctb(f_fctb state fctb,r)
      and f_sigb state (Ast.Sigb{name,def}) = Ast.Sigb{name=name,def=f_sigexp state def}
	| f_sigb state (Ast.MarkSigb(sigb,r)) = Ast.MarkSigb(f_sigb state sigb,r)
      and f_fsigb state (Ast.Fsigb{name,def}) = Ast.Fsigb{name=name,def=f_fsigexp state def}
	| f_fsigb state (Ast.MarkFsigb(fsigb,r)) = Ast.MarkFsigb(f_fsigb state fsigb,r)
      and f_strexp state strexp = 
	(case strexp of
	   Ast.VarStr _ => strexp
	 | Ast.StructStr dec => Ast.StructStr (f_dec state dec)
	 | Ast.AppStr (p,strexp_bool_list) => Ast.AppStr(p, map 
							 (fn(s,b) => (f_strexp state s,b)) strexp_bool_list)
	 | Ast.LetStr (dec, strexp) => Ast.LetStr(f_dec state dec, f_strexp state strexp)
	 | Ast.MarkStr (s,r) => Ast.MarkStr(f_strexp state s,r))
      and f_fctexp state fctexp = 
	(case fctexp of
	   Ast.VarFct (p,NONE) => Ast.VarFct(p,NONE)
	 | Ast.VarFct (p,SOME fsigexp) => Ast.VarFct(p,SOME (f_fsigexp state fsigexp))
	 | Ast.FctFct {params,body,constraint} => 
	     Ast.FctFct{params=map (fn (so,sigexp) => (so,f_sigexp state sigexp)) params,
			body=f_strexp state body,
			constraint=case constraint of NONE => NONE | SOME se => SOME(f_sigexp state se)}
	 | Ast.LetFct (dec,fctexp) => Ast.LetFct(f_dec state dec, f_fctexp state fctexp)
	 | Ast.AppFct _ => error "don't handle (higher-order) functor application"
	 | Ast.MarkFct (f,r) => Ast.MarkFct(f_fctexp state f, r))
      and f_sigexp state (Ast.VarSig s) = Ast.VarSig s
	| f_sigexp state (Ast.SigSig speclist) = Ast.SigSig(map (f_spec state) speclist)
	| f_sigexp state (Ast.MarkSig (se,r)) = Ast.MarkSig(f_sigexp state se,r)
      and f_fsigexp state (Ast.VarFsig s) = Ast.VarFsig s  
	| f_fsigexp state (Ast.FsigFsig {param,def}) = 
	        Ast.FsigFsig{param=map (fn(so,se) => (so,f_sigexp state se)) param,
			     def=f_sigexp state def}
	| f_fsigexp state (Ast.MarkFsig (fse,r)) = Ast.MarkFsig(f_fsigexp state fse,r)
      and f_spec (state as (doconstr,constrbound,
			    doty,tybound,
			    dovar, varbound : symbol list)) spec = 
	(case spec of
	   Ast.StrSpec s_se_list => Ast.StrSpec (map (fn (s,se) => (s,f_sigexp state se)) s_se_list)
	 | Ast.TycSpec (s_tvs_tyop_list,b) => 
	     Ast.TycSpec(map (fn (s,tvs,tyopt) =>
			      let val newbound =  tybound @ (map tyvar_strip tvs)
				val newstate = (doconstr,constrbound,doty,newbound,dovar,varbound)
			      in (s,tvs,case tyopt of
				  NONE => NONE
				| SOME ty => SOME(f_ty newstate ty))
			      end) s_tvs_tyop_list, b)
	| Ast.FctSpec s_fse_list => Ast.FctSpec(map (fn (s,fse) => (s,f_fsigexp state fse)) s_fse_list)
	| Ast.ValSpec s_ty_list => Ast.ValSpec (map (fn (s,ty) => (s,f_ty state ty)) s_ty_list)
	| Ast.DataSpec{datatycs,withtycs} => Ast.DataSpec{datatycs=map (f_db state) datatycs,
							 withtycs=map (f_tb state) withtycs}
	| Ast.ExceSpec s_to_list => Ast.ExceSpec(map (fn (s,NONE) => (s,NONE)
	                                    | (s,SOME ty) => (s,SOME(f_ty state ty))) s_to_list)
	| (Ast.FixSpec _ | Ast.ShareSpec _ | Ast.ShatycSpec _ | Ast.IncludeSpec _ | Ast.OpenSpec _) => spec
	| Ast.LocalSpec (slist1,slist2) => Ast.LocalSpec(map (f_spec state) slist1,
							 map (f_spec state) slist2)
	| Ast.MarkSpec (s,r) => Ast.MarkSpec(f_spec state s, r))

    in

      fun subst_vars_exp (subst : (Symbol.symbol * Ast.path) list, e : Ast.exp) : Ast.exp = 
	let 
	  fun do_tyvar tyvar = Ast.VarTy tyvar
	  fun do_var var = 
	    let fun loop [] = Ast.VarExp var
		  | loop ((p,s)::rest) = if ([p] = var) then Ast.VarExp(s) else loop rest
	    in loop subst
	    end
	  val e = f_exp (fn s => s, [],do_tyvar,[],do_var,[]) e
	in e
	end

      fun free_tyvar_ty (ty : Ast.ty, bound_tyvars) : symbol list = 
	let 
	    val tyvars = ref ([] : Ast.tyvar list)
	    fun eq(a : Ast.tyvar, b) = a = b
	    fun do_tyvar tyvar = (if member_eq(eq,tyvar,!tyvars)
				      then () 
				  else tyvars := tyvar :: (!tyvars); 
				  Ast.VarTy tyvar)
	    val _ = f_ty (fn s => s, [],
			  do_tyvar, bound_tyvars,
			  fn v => Ast.VarExp v,[]) ty
	in map tyvar_strip (!tyvars)
	end

      fun free_tyvar_exp(e : Ast.exp, bound_tyvars : symbol list) : symbol list = 
	     let 
	       val tyvars = ref ([] : Ast.tyvar list)
	       fun do_tyvar tyvar = (tyvars := tyvar :: (!tyvars); Ast.VarTy tyvar)
	       val _ = f_exp (fn s => s, [],do_tyvar,bound_tyvars,fn v => Ast.VarExp v,[]) e
	       val free_tvs = map tyvar_strip (!tyvars)
	     in free_tvs
	     end

      (* finds all free type variables in (e : Ast.exp) not in context *)
      fun free_tyc_ty(ty : Ast.ty, bound_constrs : symbol list) : symbol list = 
	let 
	  val constrs = ref ([] : symbol list)
	  fun do_constr s = (constrs := s :: (!constrs); s)
	  fun do_tyvar tyvar = Ast.VarTy tyvar
	  val _ = f_ty (do_constr,bound_constrs,
			do_tyvar,[],
			fn v => Ast.VarExp v,[]) ty
	in !constrs
	end

    end


    open Ast
    fun pp_region s1 s2 fmt = HOVbox((String s1) :: (fmt @ [String s2]))
    fun pp_list doer objs (left,sep,right,break) = 
      let 
	fun loop [] = [String right]
	  | loop [a] = [doer a, String right]
	  | loop (a::rest) = (doer a) :: (String sep) :: Break :: (loop rest)
	val fmts = (String left) :: (loop objs)
      in (if break then Vbox0 else HOVbox0 1) (size left) 1 fmts
      end
    val pp_listid = pp_list (fn x => x)

    fun pp_sym (s : Symbol.symbol) = String (Symbol.name s)
    fun pp_tyvar (Tyv s) = pp_sym s
      | pp_tyvar (MarkTyv (tv,_)) = pp_tyvar tv
    fun pp_path p = pp_list pp_sym p ("",".","",false)
    fun pp_ty ty = 
	  (case ty of
	     VarTy tyvar => pp_tyvar tyvar
	   | ConTy (syms, tys) => (pp_region "ConTy(" ")"
				   [(case syms of
				       [s] => pp_sym s
				     | _ => pp_list pp_sym syms ("[",", ","]",false)),
				    String ", ",
				    (case tys of
				       [t] => pp_ty t
				     | _ => pp_list pp_ty tys ("[",", ","]",false))])
	   | RecordTy symty_list => (pp_list (fn (sym,ty) => (pp_sym sym; String " = "; pp_ty ty)) 
				     symty_list ("(",", ",")",false))
	   | TupleTy tys => pp_list pp_ty tys ("(",", ",")",false)
	   | MarkTy (t,r) => pp_ty t)

    fun pp_pat pat = 
      (case pat of
	 Ast.WildPat => String "_"
       | Ast.VarPat p => pp_path p
       | (Ast.IntPat s | Ast.WordPat s | Ast.RealPat s | StringPat s | CharPat s) => String s
       | Ast.RecordPat _ => String "RecordPatUNIMPED"
       | Ast.ListPat _ => String "ListPatUNIMPED"
       | Ast.TuplePat pats => pp_list pp_pat pats ("(",", ",")",false)
       | Ast.FlatAppPat _ => String "FlatAppPatUNIMPED"
       | Ast.AppPat {constr,argument} => pp_region "(" ")" [pp_pat constr, String " ", pp_pat argument]
       | Ast.ConstraintPat _ => String "ConstraintPatUNIMPED"
       | Ast.LayeredPat _ => String "LayeredPatUNIMPED"
       | Ast.VectorPat pats => String "VectorPatUNIMPED"
       | Ast.MarkPat (p,r) => pp_pat p
       | Ast.OrPat _ => String "OrPatUNIMPED")
	 
    fun pp_exp exp = 
      (case exp of
	 Ast.VarExp p => pp_path p
       | Ast.IntExp s => String s
       | Ast.MarkExp (e,r) => pp_exp e
       | _ => String "Asthelp.pp_exp UNIMPED")


    fun wrapper pp out obj = 
      let 
	val fmtstream = open_fmt out
	val fmt = pp obj
      in (output_fmt (fmtstream,fmt); 
	  close_fmt fmtstream;
	  fmt)
      end

    fun help' doer = doer
    fun help pp obj = (wrapper pp TextIO.stdOut obj; ())

    val pp_tyvar'  = help' pp_tyvar
    val pp_sym'    = help' pp_sym
    val pp_path'   = help' pp_path 
    val pp_ty'     = help' pp_ty 
    val pp_pat'    = help' pp_pat
    val pp_exp'    = help' pp_exp

    val pp_tyvar = help pp_tyvar
    val pp_sym   = help pp_sym
    val pp_path  = help pp_path 
    val pp_ty    = help pp_ty 
    val pp_pat   = help pp_pat
    val pp_exp   = help pp_exp

    fun eq_path([],[]) = true
      | eq_path(_,[]) = false
      | eq_path([],_) = false
      | eq_path(s1::rest1,s2::rest2) = Symbol.eq(s1,s2) andalso eq_path(rest1,rest2)

  end
