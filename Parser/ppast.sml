(* Copyright 1994 by AT&T Bell Laboratories *)
(* print/ppast.sml *)

signature PPAST =
sig
  val ppExp     : PrettyPrint.ppstream -> Ast.exp * bool * int -> unit
  val ppPat     : PrettyPrint.ppstream -> Ast.pat * int -> unit
  val ppRule    : PrettyPrint.ppstream -> Ast.rule * int   -> unit
  val ppVb      : PrettyPrint.ppstream -> Ast.vb * int -> unit
  val ppRvb     : PrettyPrint.ppstream -> Ast.rvb * int -> unit
  val ppDec     : PrettyPrint.ppstream -> Ast.dec * int -> unit
  val ppStrexp  : PrettyPrint.ppstream -> Ast.strexp * int -> unit
  val ppFctexp  : PrettyPrint.ppstream -> Ast.fctexp * int -> unit
  val ppSigexp  : PrettyPrint.ppstream -> Ast.sigexp * bool * int -> unit
  val ppFsigexp : PrettyPrint.ppstream -> Ast.fsigexp * Symbol.symbol option * int -> unit
  val ppSpec    : PrettyPrint.ppstream -> Ast.spec * int -> unit
  val ppTy      : PrettyPrint.ppstream -> Ast.ty * int -> unit

(* install error handler, environment, fixity etc *)
  val ppAstSetUp : {errorHandler : (string -> string) option,
		    env : StaticEnv.staticEnv option,
		    fixity : (Symbol.symbol -> Fixity.fixity) option} -> unit
end

structure PPAst : PPAST =
struct

open PrettyPrint PPUtil Ast

structure S = Symbol

(* symbolic path 
type path = S.symbol list
*)

fun C f x y = f y x

(* ERROR HANDLING *)
val errorHandlerRef = ref (NONE : (string -> string) option)
fun errorHandler s =
    (case !errorHandlerRef of
	 NONE => s
       | SOME f => f s)

(* FIXITY *)
val env_ref = ref(NONE : (StaticEnv.staticEnv option))
val fixity_map_ref = ref(NONE : (Symbol.symbol -> Fixity.fixity) option)

(* fixity map takes preference *)

fun resolveFixity s =
    (case !fixity_map_ref of
	 SOME f => f s
       | NONE => (case !env_ref of
		      SOME env => Lookup.lookFix(env, Env.Symbol.fixSymbol(Symbol.name s))
		    | NONE => Fixity.NONfix))

(* ASTSETUP *)

  fun ppAstSetUp {errorHandler, env, fixity} =
      (errorHandlerRef := errorHandler;
       env_ref := env;
       fixity_map_ref := fixity)       

        fun ppLit s = ppsay s

(* PATTERN *)
fun ppPat ppstrm =
    let val ppsay = add_string ppstrm


	fun ppPat' (_,0) = ppsay "<pat>"
	  | ppPat' (VarPat p,_) = ppSymPath ppstrm (SymPath.SPATH p)
	  | ppPat' (WildPat,_) = ppsay "_"
	  | ppPat' (IntPat i,_) = ppLit i
	  | ppPat' (WordPat w,_) = ppLit w
	  | ppPat' (StringPat s,_) = pp_mlstr ppstrm s
	  | ppPat' (CharPat s,_) = (ppsay "#"; pp_mlstr ppstrm s)
	  | ppPat' (LayeredPat{varPat=v,expPat=p},d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppPat'(v,d); ppsay " as "; ppPat'(p,d-1);
	       end_block ppstrm)
		    (* Handle 0 length case specially to avoid {,...}: *)
	  | ppPat' (RecordPat{def=[],flexibility},_) =
	      if flexibility then ppsay "{...}"
	      else ppsay "()"
	  | ppPat' (r as RecordPat{def,flexibility},d) =
	      ppClosedSequence ppstrm
		{front=(C add_string "{"),
		 sep=(fn ppstrm => (add_string ppstrm ",";
				    add_break ppstrm (0,0))),
		 back=(fn ppstrm => if flexibility then add_string ppstrm ",...}"
				    else add_string ppstrm "}"),
		 pr=(fn ppstrm => fn (sym,pat) =>
		     (ppSym ppstrm sym; add_string ppstrm "=";
		      ppPat'(pat,d-1))),
		 style=INCONSISTENT}
		def
	  | ppPat' (TuplePat fields,d) =
	      ppClosedSequence ppstrm
		{front=(C add_string "("),
		 sep=(fn ppstrm => (add_string ppstrm ",";
				    add_break ppstrm (0,0))),
		 back=(C add_string ")"),
		 pr=(fn _ => fn pat => ppPat'(pat,d-1)),
		 style=INCONSISTENT}
		fields
	  | ppPat' (ListPat nil, d) = ppsay "[]"
	  | ppPat' (ListPat pats, d) = 
	      let fun pr _ pat = ppPat'(pat, d-1)
	       in ppClosedSequence ppstrm
		    {front=(C add_string "["),
		     sep=(fn ppstrm => (add_string ppstrm ",";
					add_break ppstrm (0,0))),
		     back=(C add_string "]"),
		     pr=pr,
		     style=INCONSISTENT}
		    pats
	      end
	  | ppPat' (VectorPat nil, d) = ppsay "#[]"
	  | ppPat' (VectorPat pats, d) = 
	      let fun pr _ pat = ppPat'(pat, d-1)
	       in ppClosedSequence ppstrm
		    {front=(C add_string "#["),
		     sep=(fn ppstrm => (add_string ppstrm ",";
					add_break ppstrm (0,0))),
		     back=(C add_string "]"),
		     pr=pr,
		     style=INCONSISTENT}
		    pats
	      end
	  | ppPat' (OrPat pats, d) =
	      ppClosedSequence ppstrm
		{front = (C add_string "("),
		 sep = fn ppstrm =>
			(add_break ppstrm (1,0); add_string ppstrm "| "),
		 back = (C add_string ")"),
		 pr = (fn _ => fn pat => ppPat'(pat, d-1)),
		 style = INCONSISTENT} 
		pats
	  | ppPat' (exp as (AppPat{constr,argument}), d) =
		let fun nonfixfn() =
		    (begin_block ppstrm CONSISTENT 0;
		     ppsay "(";
		     ppPat'(constr,d-1);
		     add_break ppstrm (1,0);
		     ppPat'(argument,d-1);
		     ppsay ")";
		     end_block ppstrm)
		in case (constr,argument) of
		    (VarPat[s],TuplePat[patl, patr]) =>
			(case resolveFixity s of
			     Fixity.INfix _ => 
				 (begin_block ppstrm CONSISTENT 0;
				  ppsay "(";
				  ppPat'(patl,d-1);
				  add_break ppstrm (1,0);
				  ppPat'(constr,d-1);
				  add_break ppstrm (1,0);
				  ppPat'(patr,d-1);
				  ppsay ")";
				  end_block ppstrm)
			   | _ => nonfixfn())
		  | _ => nonfixfn()
		end
	  | ppPat' (FlatAppPat [{item, ...}], d) = ppPat ppstrm (item,d-1)
	  | ppPat' (FlatAppPat patfixs, d) = 
	      ppClosedSequence ppstrm
		{front=(C add_string "("),
		 sep=(C add_string " "),
		 back=(C add_string ")"),
		 pr= fn ppstrm => fn {item, ...} => ppPat ppstrm (item,d-1),
		 style=INCONSISTENT}
		patfixs
	  | ppPat' (ConstraintPat{pattern,constraint},d) =
	     (begin_block ppstrm INCONSISTENT 0;
	      ppPat'(pattern,d-1); ppsay " : ";
	      add_break ppstrm (1,2);
	      ppTy ppstrm (constraint,d);
	      end_block ppstrm)
	  | ppPat' (MarkPat(pat,_),d) = ppPat'(pat,d)
     in ppPat'
    end


(* EXPRESSIONS *)
and ppExp ppstrm =
    let val ppsay = add_string ppstrm
	fun ppExp'(_,_,0) = ppsay "<exp>"
	  | ppExp'(VarExp p,_,_) = ppSymPath ppstrm (SymPath.SPATH p)
	  | ppExp'(FnExp rules,_,d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("(fn ","  | ",
			       (fn ppstrm => fn r =>
				  ppRule ppstrm (r,d-1)),
			       rules);
	       ppsay ")";
	       end_block ppstrm)
	  | ppExp'(AppExp{function,argument},noparens,d) =
		let fun nonfixfn() =
		    (begin_block ppstrm CONSISTENT 0;
		     if noparens then () else ppsay "(";
		     ppExp'(function,false,d-1);
		     add_break ppstrm (1,0);
		     ppExp'(argument,false,d-1);
		     if noparens then () else ppsay ")";
		     end_block ppstrm)
		in case (function,argument) of
		    (VarExp[s],TupleExp[argl, argr]) =>
			(case resolveFixity s of
			     Fixity.INfix _ => 
				 (begin_block ppstrm CONSISTENT 0;
				  if noparens then () else ppsay "(";
				  ppExp'(argl,false,d-1);
				  add_break ppstrm (1,0);
				  ppExp'(function,false,d-1);
				  add_break ppstrm (1,0);
				  ppExp'(argr,false,d-1);
				  if noparens then () else ppsay ")";
				  end_block ppstrm)
			   | _ => nonfixfn())
		  | _ => nonfixfn()
		end
	  | ppExp' (FlatAppExp [{item, ...}],fl,d) = ppExp ppstrm (item,fl,d-1)
          | ppExp'(FlatAppExp expfixs,noparens,d) =
	      ppClosedSequence ppstrm
		{front=(C add_string (if noparens then "" else "(")),
		 sep=(C add_string " "),
		 back=(C add_string (if noparens then "" else ")")),
		 pr= fn ppstrm => fn {item, ...} => ppExp' (item,false,d-1),
		 style=INCONSISTENT}
		expfixs
	  | ppExp'(CaseExp{expr,rules},_,d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "(case "; ppExp'(expr,true,d-1); nl_indent ppstrm 2;
	       ppvlist ppstrm ("of ","   | ",
		 (fn ppstrm => fn r => ppRule ppstrm (r,d-1)), rules);
	       ppsay ")";
	       end_block ppstrm)
	  | ppExp'(LetExp{dec,expr},_,d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "let "; ppDec ppstrm (dec,d-1);
	       add_break ppstrm (1,0);
	       ppsay " in "; ppExp'(expr,true,d-1);
	       add_break ppstrm (1,0);
	       ppsay "end";
	       end_block ppstrm)
	  | ppExp'(PletExp{dec,expr},_,d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "plet "; ppDec ppstrm (dec,d-1);
	       add_break ppstrm (1,0);
	       ppsay " in "; ppExp'(expr,true,d-1);
	       add_break ppstrm (1,0);
	       ppsay "end";
	       end_block ppstrm)
	  | ppExp'(SeqExp [exp],fl,d) = ppExp'(exp,fl,d-1)
	  | ppExp'(SeqExp exps,noparens,d) =
	      ppClosedSequence ppstrm
	        {front=(C add_string "("),
		 sep=(fn ppstrm => (add_string ppstrm ";";
				    add_break ppstrm (1,0))),
		 back=(C add_string ")"),
		 pr=(fn _ => fn exp => ppExp'(exp,true,d-1)),
		 style=INCONSISTENT}
		exps
	  | ppExp'(IntExp i,_,_) = ppLit i
 	  | ppExp'(WordExp w,_,_) = ppLit w
	  | ppExp'(RealExp r,_,_) = ppsay r
	  | ppExp'(StringExp s,_,_) = pp_mlstr ppstrm s
	  | ppExp'(CharExp s,_,_) = (ppsay "#"; pp_mlstr ppstrm s)
	  | ppExp'(RecordExp fields,_,d) =
	      ppClosedSequence ppstrm
		{front=(C add_string "{"),
		 sep=(fn ppstrm => (add_string ppstrm ",";
				    add_break ppstrm (0,0))),
		 back=(C add_string "}"),
		 pr=(fn ppstrm => fn (label,exp) =>
		     (ppSym ppstrm label; ppsay "=";
		      ppExp'(exp,true,d-1))),
		 style=INCONSISTENT}
		fields
	  | ppExp'(TupleExp fields,noparens,d) =
	      ppClosedSequence ppstrm
		{front=(C add_string "("),
		 sep=(fn ppstrm => (add_string ppstrm ",";
				    add_break ppstrm (0,0))),
		 back=(C add_string ")"),
		 pr=(fn _ => fn exp => ppExp'(exp,true,d-1)),
		 style=INCONSISTENT}
		fields
	  | ppExp'(ListExp elements,_,d) =
	      ppClosedSequence ppstrm
		{front=(C add_string "["),
		 sep=(fn ppstrm => (add_string ppstrm ",";
				    add_break ppstrm (0,0))),
		 back=(C add_string "]"),
		 pr=(fn _ => fn exp => ppExp'(exp,true,d-1)),
		 style=INCONSISTENT}
		elements
	  | ppExp'(VectorExp elements,_,d) =
	      ppClosedSequence ppstrm
		{front=(C add_string "#["),
		 sep=(fn ppstrm => (add_string ppstrm ",";
				    add_break ppstrm (0,0))),
		 back=(C add_string "]"),
		 pr=(fn _ => fn exp => ppExp'(exp,true,d-1)),
		 style=INCONSISTENT}
		elements
	  | ppExp'(SelectorExp sym,_,_) =  (* selector of a record field *)
	      (ppsay "#"; ppSym ppstrm sym)
          | ppExp'(ConstraintExp{expr,constraint},_,d) = (* type constraint *)
	      (begin_block ppstrm INCONSISTENT 0;
	       ppsay "("; ppExp'(expr,false,d); ppsay ":";
	       add_break ppstrm (1,2);
	       ppTy ppstrm (constraint,d); ppsay ")";
	       end_block ppstrm)
          | ppExp'(HandleExp{expr, rules},_,d) = (* exception handler *)
	     (begin_block ppstrm CONSISTENT 0;
	      ppExp'(expr,false,d-1); add_newline ppstrm; ppsay "handle ";
	      nl_indent ppstrm 2;
	      ppvlist ppstrm ("  ","| ",
		  (fn ppstrm => fn r => ppRule ppstrm (r,d-1)), rules);
	      end_block ppstrm)
          | ppExp'(RaiseExp exp,_,d) =  (* raise an exception *)
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "raise "; ppExp'(exp,false,d-1);
	       end_block ppstrm)
          | ppExp'(IfExp{test, thenCase, elseCase},noparens,d) = (* if expression *)
	      (begin_block ppstrm CONSISTENT 0;
	       if noparens then () else ppsay "(";
	       ppsay "if "; ppExp'(test,true,d-1);
	       add_break ppstrm (1,0);
	       ppsay " then "; ppExp'(thenCase,true,d-1);
	       add_break ppstrm (1,0);
	       ppsay " else "; ppExp'(elseCase,true,d-1);
	       if noparens then () else ppsay ")";
	       end_block ppstrm)
          | ppExp'(AndalsoExp(exp1,exp2),_,d) =	(* andalso (derived form) *)
	      (begin_block ppstrm INCONSISTENT 0;
	       ppsay "("; ppExp'(exp1,false,d-1);
	       add_break ppstrm (1,0);
	       ppsay " andalso "; 
	       add_break ppstrm (1,0);
	       ppExp'(exp2,false,d-1); ppsay ")";
	       end_block ppstrm)
          | ppExp'(OrelseExp(exp1,exp2),_,d) =	(* orelse (derived form) *)
	      (begin_block ppstrm INCONSISTENT 0;
	       ppsay "("; ppExp'(exp1,false,d-1);
	       add_break ppstrm (1,0);
	       ppsay " orelse "; 
	       add_break ppstrm (1,0);
	       ppExp'(exp2,false,d-1); ppsay ")";
	       end_block ppstrm)
          | ppExp'(WhileExp{test,expr},_,d) = (* while (derived form) *)
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "while "; ppExp'(test,true,d-1);
	       add_break ppstrm (1,0); ppsay " do ";
	       ppExp'(expr,false,d-1);
	       end_block ppstrm)
          | ppExp'(MarkExp(expr,region),fl,d) = ppExp'(expr,fl,d)
     in ppExp'
    end

(* RULE for case functions and exception handler *)
and ppRule ppstrm (Rule{pat,exp},0) = add_string ppstrm "<rule>"
  | ppRule ppstrm (Rule{pat,exp},d) = (begin_block ppstrm CONSISTENT 0;
				       ppPat ppstrm (pat,d-1);
				       add_string ppstrm " =>"; add_break ppstrm (1,2);
					   ppExp ppstrm (exp,false,d-1);
					   end_block ppstrm)

(* STRUCTURE EXPRESSION *)

and ppStrexp ppstrm =
    let val ppsay = add_string ppstrm
	fun ppStrexp'(_,0) = ppsay "<strexp>"
	  | ppStrexp'(VarStr p,d) = ppSymPath ppstrm (SymPath.SPATH p)
	  | ppStrexp'(StructStr dec,d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "struct ";
	       nl_indent ppstrm 2;
	       begin_block ppstrm CONSISTENT 0;
	       ppDec ppstrm (dec,d-1);
	       end_block ppstrm;
	       add_newline ppstrm;
	       ppsay "end";
	       end_block ppstrm)
	  | ppStrexp'(AppStr(path, args),d) =
	      (ppSymPath ppstrm (SymPath.SPATH path);
	       begin_block ppstrm INCONSISTENT 2;
	       List.app (fn (str,_) => 
			  (ppsay"("; ppStrexp'(str,d-1); ppsay")"))
	         args;
	       end_block ppstrm)
	  | ppStrexp'(LetStr(dec,body),d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "let "; ppDec ppstrm (dec,d-1); add_newline ppstrm;
	       ppsay " in "; ppStrexp'(body,d-1); add_newline ppstrm;
	       ppsay "end";
	       end_block ppstrm)
	  | ppStrexp'(MarkStr(body,_),d) = ppStrexp'(body,d)
     in ppStrexp'
    end

(* FUNCTOR EXPRESSION *)
and ppFctexp ppstrm =
    let val ppsay = add_string ppstrm
	fun ppFctexp'(VarFct(p,constraint),d) =
	      (ppSymPath ppstrm (SymPath.SPATH p);
	       case constraint
		 of Transparent fsig =>
		     (ppsay " : "; ppFsigexp ppstrm (fsig,NONE,d-1))
		  | Opaque fsig =>
		     (ppsay " :> "; ppFsigexp ppstrm (fsig,NONE,d-1))
		  | NoSig => ())
	  | ppFctexp'(FctFct{params,body,constraint},d) =
	      (begin_block ppstrm CONSISTENT 2;
	       ppsay "functor ";
	       begin_block ppstrm INCONSISTENT 2;
	       List.app (fn (pname,sign) => 
			  (ppsay"(";
			   case pname of
			     NONE => ppSigexp ppstrm (sign,false,d-1)
			   | SOME sym => (ppSym ppstrm sym;
					  ppsay " : ";
					  ppSigexp ppstrm (sign,true,d-1));
			   ppsay")"))

	         params;
	       end_block ppstrm;
	       case constraint
		 of Transparent sign =>
		     (ppsay " : "; ppSigexp ppstrm (sign,true,d-1))
		  | Opaque sign =>
		     (ppsay " :> "; ppSigexp ppstrm (sign,true,d-1))
		  | NoSig => ();
	       ppsay " = ";
	       ppStrexp ppstrm (body,d-1);
	       end_block ppstrm)
	  | ppFctexp'(LetFct(dec,fctexp),d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "let "; ppDec ppstrm (dec,d-1); add_newline ppstrm;
	       ppsay " in "; ppFctexp'(fctexp,d-1); add_newline ppstrm;
	       ppsay "end";
	       end_block ppstrm)
	  | ppFctexp'(AppFct(p,args,constraint),d) =
	      (ppSymPath ppstrm (SymPath.SPATH p);
	       begin_block ppstrm INCONSISTENT 2;
	       List.app (fn (str,_) => 
			  (ppsay"("; ppStrexp ppstrm (str,d-1); ppsay")"))
	         args;
	       end_block ppstrm)
	  | ppFctexp'(MarkFct(fct,_),d) = ppFctexp'(fct,d)
     in ppFctexp'
    end

(* SIGNATURE EXPRESSION *)
and ppSigexp ppstrm =
    let val ppsay = add_string ppstrm
	fun ppSigexp'(_,_,0) = ppsay "<sigexp>"
	  | ppSigexp'(VarSig sym,_,d) = ppSym ppstrm sym
	  | ppSigexp'(AugSig _,_,_) = ppsay "<augsigexp>"
	  | ppSigexp'(SigSig specs,sigflag,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       if sigflag then (ppsay "sig";
				nl_indent ppstrm 2)
	       else ();
	       begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("","",
			       (fn ppstrm => fn spec => ppSpec ppstrm (spec,d-1)),
			       specs);
	       end_block ppstrm;
	       if sigflag then (add_newline ppstrm; ppsay "end") else ();
	       end_block ppstrm)
	  | ppSigexp'(MarkSig(sigexp,_),sigflag,d) = ppSigexp'(sigexp,sigflag,d)
     in ppSigexp'
    end

(* FUNCTOR SIGNATURE EXPRESSION *)
and ppFsigexp ppstrm =
    let val ppsay = add_string ppstrm
	fun ppFsigexp'(_,_,0) = ppsay "<fsigexp>"
	  | ppFsigexp'(VarFsig sym,_,d) = ppSym ppstrm sym
	  | ppFsigexp'(FsigFsig{param,def},SOME name,d) =
	      (begin_block ppstrm CONSISTENT 2;
	       begin_block ppstrm INCONSISTENT 2;
	       ppSym ppstrm name;
	       ppsay " ";
	       List.app (fn (pname,sign) => 
			  (ppsay"(";
			   case pname of
			     NONE => ppSigexp ppstrm (sign,false,d-1)
			   | SOME sym => (ppSym ppstrm sym;
					  ppsay " : ";
					  ppSigexp ppstrm (sign,true,d-1));
			   ppsay")"))
	         param;
	       end_block ppstrm;
	       ppsay " = "; ppSigexp ppstrm (def,true,d-1);
	       end_block ppstrm)
	  | ppFsigexp'(FsigFsig _,_,_) = ppsay (errorHandler "<fsigexp>")
	  | ppFsigexp'(MarkFsig(fsigexp,_),nameopt,d) = ppFsigexp'(fsigexp,nameopt,d)
     in ppFsigexp'
    end

(* SPECIFICATION FOR SIGNATURE DEFINITIONS *)
and ppSpec ppstrm =
    let val ppsay = add_string ppstrm
	fun ppSpec'(_,0) = ppsay "<spec>"
	  | ppSpec'(StrSpec strspecs,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("structure ","and ",
		(fn ppstrm => fn (s,sigexp) => (ppSym ppstrm s;
						ppsay " : ";
						ppSigexp ppstrm (sigexp,true,d-1))),
		strspecs);
	       end_block ppstrm)
	  | ppSpec'(TycSpec tycls,d) = ppsay "<TycSpec>"
	  | ppSpec'(FctSpec fctspec,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("functor ","and ",
		(fn ppstrm => fn (s,fsigexp) => (ppSym ppstrm s;
						 ppsay " : ";
						 ppFsigexp ppstrm (fsigexp,NONE,d-1))),
		fctspec);
	       end_block ppstrm)
	  | ppSpec'(ValSpec vss,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("val "," and ",
		(fn ppstrm => fn (s,ty) => (begin_block ppstrm CONSISTENT 0;
					    ppSym ppstrm s;
					    add_string ppstrm " : ";
					    ppTy ppstrm (ty,d-1);
					    end_block ppstrm)),
		 vss);
	       end_block ppstrm)
	  | ppSpec'(DataSpec ds,d) = ppDec ppstrm ((DatatypeDec ds), d)
	  | ppSpec'(ExceSpec ebs,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("exception "," and ",
			       (fn ppstrm => fn (sym, tyopt) => ppEb ppstrm (EbGen{exn=sym,etype=tyopt},d-1)),
			       ebs);
	       end_block ppstrm)
	  | ppSpec'(FixSpec x,d) = ppDec ppstrm (FixDec x, d)
	  | ppSpec'(ShareSpec pathl,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("sharing "," = ",
			       (fn ppstrm => fn p => ppSymPath ppstrm (SymPath.SPATH p)),
			       pathl);
	       end_block ppstrm)
	  | ppSpec'(ShatycSpec pathl,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("sharing type "," = ",
			       (fn ppstrm => fn p => ppSymPath ppstrm (SymPath.SPATH p)),
			       pathl);
	       end_block ppstrm)
(* not in SML96 -dbm
	  | ppSpec'(LocalSpec(s1, s2),d) = 
	      (ppsay "local ";
	       add_newline ppstrm;
	       begin_block ppstrm CONSISTENT 0;
	       nl_indent ppstrm 2;
	       ppvlist ppstrm ("","",
			       (fn ppstrm => fn spec => ppSpec ppstrm (spec,d-1)),
			       s1);
	       end_block ppstrm;
	       add_newline ppstrm;
	       ppsay "in ";
	       begin_block ppstrm CONSISTENT 0;
	       nl_indent ppstrm 2;
	       ppvlist ppstrm ("","",
			       (fn ppstrm => fn spec => ppSpec ppstrm (spec,d-1)),
			       s2);
	       end_block ppstrm;
	       ppsay "end")
*)
	  | ppSpec'(IncludeSpec includesym,d) = 
	      (ppsay "include ";
	       ppsay (Symbol.name includesym))
(* not in SML96 - dbm
	  | ppSpec'(OpenSpec openl,d) = 
	      (ppsay "open ";
	       ppSequence ppstrm
	       {sep=(fn ppstrm => add_string ppstrm " "),
		pr=(fn ppstrm => fn x => ppSymPath ppstrm (SymPath.SPATH x)),
		style=INCONSISTENT}
	       openl)
*)
	  | ppSpec'(MarkSpec(spec,_),d) = ppSpec'(spec,d)
     in ppSpec'
    end

(* DECLARATIONS (let and structure) *)
and ppDec ppstrm =
    let val ppsay = add_string ppstrm
        fun pptyvars [] = ()
	  | pptyvars tyvars =  ppTuple ppstrm ppTyvar tyvars
	fun ppDec'(_,0) = ppsay "<dec>"
	  | ppDec'(ValDec (vbs, ref tyvars),d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       pptyvars tyvars;
	       ppvlist ppstrm ("val "," and ",
		(fn ppstrm => fn vb => ppVb ppstrm (vb,d-1)),
		 vbs);
	       end_block ppstrm)
	  | ppDec'(ValrecDec (rvbs, ref tyvars),d) =   (* check *)
	      (begin_block ppstrm CONSISTENT 0;
	       pptyvars tyvars;
	       ppvlist ppstrm ("val rec "," and ",
		(fn ppstrm => fn vb => ppRvb ppstrm (vb,d-1)),
		 rvbs);
	       end_block ppstrm)
	  | ppDec'(FunDec (fbs, ref tyvars),d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       pptyvars tyvars;
	       ppvlist ppstrm ("fun "," and ",
		(fn ppstrm => fn tb => ppFb ppstrm (tb,d-1)),
		 fbs);
	       end_block ppstrm)
	  | ppDec'(TypeDec tbs,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("type "," and ",
		(fn ppstrm => fn tb => ppTb ppstrm (tb,d-1)),
		 tbs);
	       end_block ppstrm)
	  | ppDec'(DatatypeDec{datatycs,withtycs},d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("datatype ","and ", fn ppstrm => fn dtcs => ppDb ppstrm (dtcs, d-1), datatycs);
	       add_newline ppstrm;
	       ppvlist ppstrm ("withtype ","and ", fn ppstrm => fn tcs => ppTb ppstrm (tcs, d-1), withtycs);
	       end_block ppstrm)

	  | ppDec'(AbstypeDec{abstycs, body, withtycs},d) =  (* check *)
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("abstype ","and ", fn ppstrm => fn dtcs => ppDb ppstrm (dtcs, d-1), abstycs);
	       add_newline ppstrm;
	       ppvlist ppstrm ("withtype ","and ", fn ppstrm => fn tcs => ppTb ppstrm (tcs, d-1), withtycs);
	       add_newline ppstrm;
	       ppsay "with ";
	       begin_block ppstrm CONSISTENT 0;
	       nl_indent ppstrm 2;
	       ppDec'(body, d-1);
	       end_block ppstrm;
	       ppsay "end";
	       end_block ppstrm)
	  | ppDec'(ExceptionDec ebs,d) =  
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("exception "," and ",
		(fn ppstrm => fn eb => ppEb ppstrm (eb,d-1)),
		 ebs);
	       end_block ppstrm)
	  | ppDec'(StrDec strbs,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("structure ","and ",
		(fn ppstrm => fn strb => ppStrb ppstrm (strb,d-1)),
		 strbs);
	       end_block ppstrm)
	  | ppDec'(AbsDec strbs,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("abstraction ","and ",
		(fn ppstrm => fn strb => ppStrb ppstrm (strb,d-1)),
		strbs);
	       end_block ppstrm)
	  | ppDec'(FctDec fctbs,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("functor ","and ",
		(fn ppstrm => fn fctb => ppFctb ppstrm (fctb,d-1)),
		fctbs);
	       end_block ppstrm)
	  | ppDec'(SigDec sigbs,d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("signature ","and ",
		(fn ppstrm => fn sigb => ppSigb ppstrm (sigb,d-1)),
		 sigbs);
	       end_block ppstrm)
	  | ppDec'(FsigDec fsigbs,d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("funsig ","and ",
		(fn ppstrm => fn fsigb => ppFsigb ppstrm (fsigb,d-1)),
		 fsigbs);
	       end_block ppstrm)
	  | ppDec'(LocalDec(dec1,dec2),d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "local "; ppDec ppstrm (dec1,d-1);
	       add_break ppstrm (1,0);
	       ppsay " in "; ppDec ppstrm (dec2,d-1);
	       ppsay " ";
	       add_break ppstrm (1,0);
	       ppsay "end";
	       end_block ppstrm)
	  | ppDec'(SeqDec decs,d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("", "" ,(fn ppstrm => fn dec => ppDec'(dec,d-1)),decs);
	       end_block ppstrm)
	  | ppDec'(OpenDec paths,d) =
	      (ppsay "open ";
	       ppSequence ppstrm
	         {sep=(fn ppstrm => add_string ppstrm " "),
		  pr=(fn ppstrm => fn x =>
		          ppSymPath ppstrm (SymPath.SPATH x)),
		  style=INCONSISTENT}
		 paths)
	  | ppDec'(exp as (OvldDec _),d) = ppsay (errorHandler "<OvldDec>")
	  | ppDec'(FixDec{fixity,ops},d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay (Fixity.fixityToString fixity);
	       ppsay " ";
	       ppSymPath ppstrm (SymPath.SPATH ops);
	       end_block ppstrm)
	  | ppDec'(ImportDec importl,d) = 
	      (ppsay "import ";
	       ppSequence ppstrm
	       {sep=(fn ppstrm => add_string ppstrm " "),
		pr=(fn ppstrm => fn x => ppsay x),
		style=INCONSISTENT}
	       importl)
	  | ppDec'(MarkDec(dec, _), d) = ppDec'(dec, d)
     in ppDec'
    end

(* VALUE BINDINGS *)
and ppVb ppstrm = 
    fn (_,0) => add_string ppstrm "<vb>"
     | (Vb{pat,exp},d) =>
	 (begin_block ppstrm CONSISTENT 0;
	  ppPat ppstrm (pat,d-1); add_string ppstrm " =";
	  add_break ppstrm (1,2); ppExp ppstrm (exp,true,d-1);
	  end_block ppstrm)
     | (MarkVb(vb,_),d) => ppVb ppstrm (vb,d)

(* RECURSIVE VALUE BINDINGS *)
and ppRvb ppstrm = 
    fn (_,0) => add_string ppstrm "<rvb>"   (* check *)
     | (Rvb{var, fixity, exp, resultty},d) =>
	 (begin_block ppstrm CONSISTENT 0;
	  add_string ppstrm (Symbol.name var);
	  add_string ppstrm " =";
	  add_break ppstrm (1,2);
	  ppExp ppstrm (exp,true,d-1);
	  end_block ppstrm)
     | (MarkRvb(rvb,_),d) => ppRvb ppstrm (rvb,d)

(* DATA BINDINGS *)
and ppDb ppstrm = 
    fn (_, 0) => add_string ppstrm "<db>" 
     | (Db{tyc, tyvars, def}, d) =>
	    (case tyvars
		 of nil => ()
	       | tyvars => (ppTuple ppstrm ppTyvar tyvars);
	     add_string ppstrm " ";
	     ppSym ppstrm tyc;
	     add_string ppstrm " = ";
	     ppSequence ppstrm
		 {sep=(fn ppstrm => (add_string ppstrm " |";
				     add_break ppstrm (1,0))),
		  pr=(fn ppstrm => fn (s, t) => 
		      (ppSym ppstrm s;
		       case t
			   of NONE => ()
			 | SOME ty' => (add_string ppstrm " of "; ppTy ppstrm (ty', d-1)))),
		  style=INCONSISTENT}
		 def)
     | (MarkDb(db, _), d) => ppDb ppstrm (db, d)

(* EXCEPTION BINDINGS *)
and ppEb ppstrm =
    fn (_, 0) => add_string ppstrm "<fb>"
     | (EbGen{exn,etype,...}, d) => (ppSym ppstrm exn;
				     case etype
					 of NONE => ()
				       | SOME ty' =>
					     (add_string ppstrm " of "; ppTy ppstrm (ty', d-1)))
     | (EbDef{exn,edef}, d) => (ppSym ppstrm exn;
			     add_string ppstrm "=";
			     ppSymPath ppstrm (SymPath.SPATH edef))
     | (MarkEb(eb, _), d) => ppEb ppstrm (eb, d)

(* FUNCTION BINDINGS *)
and ppFb ppstrm = 
    fn (_,0) => add_string ppstrm "<fb>"
     | (Fb clauses,d) =>
	      ppClosedSequence ppstrm
		{front = fn _ => (),
		 sep = fn ppstrm =>
			(nl_indent ppstrm ~2; add_string ppstrm "| "),
		 back = fn _ => (),
		 pr = (fn _ => fn(Clause{pats, resultty, exp}) => 
		       (begin_block ppstrm CONSISTENT 0;
			ppClosedSequence ppstrm
			{front=fn _ => (),
			 sep=(C add_string " "),
			 back=fn _ => (),
			 pr= fn ppstrm => fn {item, ...} => ppPat ppstrm (item,d-1),
			 style=INCONSISTENT}
			pats;
			add_string ppstrm " =";
			add_break ppstrm (1,2); ppExp ppstrm (exp,true,d-1);
			end_block ppstrm)),
		 style = INCONSISTENT} 
		clauses
     | (MarkFb(fb,_),d) => ppFb ppstrm (fb,d)

(* TYPE BINDING *)
and ppTb ppstrm =
    fn (_,0) => add_string ppstrm "<tb>"
     | (Tb{tyc,def,tyvars},d) =>
	 (case tyvars
	    of nil => ()
	     | _ => (ppTuple ppstrm ppTyvar tyvars);
	  ppSym ppstrm tyc;
	  add_string ppstrm " ="; add_break ppstrm (1,0);
	  ppTy ppstrm (def,d-1))
     | (MarkTb(tb,_),d) => ppTb ppstrm (tb,d)


(* STRUCTURE BINDING *)
and ppStrb ppstrm =
  let val ppsay = add_string ppstrm in
    fn (Strb{name,def,constraint},d) =>
	 (ppSym ppstrm name;
	  case constraint
	    of NoSig => ()
	     | Transparent sigexp =>
		(ppsay " : ";
		 ppSigexp ppstrm (sigexp,true,d-1))
	     | Opaque sigexp =>
		(ppsay " :> ";
		 ppSigexp ppstrm (sigexp,true,d-1));
	  add_string ppstrm " =";
	  add_break ppstrm (1,2);
	  ppStrexp ppstrm (def,d-1))
     | (MarkStrb(strb,_),d) => ppStrb ppstrm (strb,d)
  end

(* FUNCTOR BINDING *)
and ppFctb ppstrm =
    let val ppsay = add_string ppstrm
	fun ppFctb'(_,0) = ppsay "<fctb>"
          | ppFctb'(Fctb{name,def=MarkFct(fctexp,_)},d) =
	      ppFctb'(Fctb{name=name,def=fctexp},d-1)
          | ppFctb'(Fctb{name,def=FctFct{params,body,constraint}},d) =
  	      (begin_block ppstrm CONSISTENT 2;
	       ppSym ppstrm name;
	       begin_block ppstrm INCONSISTENT 2;
	       List.app (fn (pname,sign) => 
			  (ppsay"(";
			   case pname of NONE => ppSigexp ppstrm (sign,false,d-1)
			      | SOME sym => (ppSym ppstrm sym;
					     ppsay " : ";
					     ppSigexp ppstrm (sign,true,d-1));
			   ppsay")"))
	         params;
	       end_block ppstrm;
	       case constraint
		 of Transparent fsig => (ppsay " : "; ppSigexp ppstrm (fsig,true,d-1))
	          | Opaque fsig =>  (ppsay " :> "; ppSigexp ppstrm (fsig,true,d-1))
		  | NoSig => ();
	       ppsay " = "; add_newline ppstrm;
	       ppStrexp ppstrm (body,d-1);
	       end_block ppstrm)
          | ppFctb'(Fctb{name,def},d) =
	      (begin_block ppstrm CONSISTENT 2;
	       ppSym ppstrm name;
	       ppsay " = ";
	       ppFctexp ppstrm (def,d-1);
	       end_block ppstrm)
          | ppFctb'(MarkFctb(fctb,_),d) = ppFctb'(fctb,d)
     in ppFctb'
    end

(* SIGNATURE BINDING *)
and ppSigb ppstrm =
    fn (_,0) => add_string ppstrm "<sigb>"
     | (Sigb{name,def},d) =>
	 (begin_block ppstrm CONSISTENT 0;
	  ppSym ppstrm name; add_string ppstrm " =";
	  add_break ppstrm (1,2); ppSigexp ppstrm (def,true,d-1);
	  end_block ppstrm)
     | (MarkSigb(sigb,_),d) => ppSigb ppstrm (sigb,d)

(* FUNSIG BINDING *)
and ppFsigb ppstrm =
    fn (_,0) => add_string ppstrm "<fsigb>"
     | (Fsigb{name,def},d) => (ppFsigexp ppstrm (def, SOME name, d-1))
     | (MarkFsigb(fsigb,_),d) => ppFsigb ppstrm (fsigb,d)

(* TYPE VARIABLE *)
and ppTyvar ppstrm =
    fn (Tyv name) => ppSym ppstrm name
     | (TempTyv name) => (add_string ppstrm "_"; ppSym ppstrm name)
     | (MarkTyv(tyvar,_)) => ppTyvar ppstrm tyvar

(* TYPES *)
and ppTy ppstrm =
    let val ppsay = add_string ppstrm
        fun ppTy'(VarTy tyvar,d) = ppTyvar ppstrm tyvar
          | ppTy'(ConTy(path,args),d) =			
	  let val arrowfl = (case (path, args) of
			      ([s], [_, _]) => if Symbol.name s = "->" then true else false
			    | _ => false)
	  in
	     (begin_block ppstrm INCONSISTENT 2;
	      case args
		of nil => ()
		 | _ =>
		   ppClosedSequence ppstrm
		     {front=(fn ppstrm => add_string ppstrm "("),
		      sep=(fn ppstrm => add_string ppstrm (if arrowfl then " -> " else ",")),
		      back=(fn ppstrm => add_string ppstrm ")"),
		      pr=(fn ppstrm => fn ty => ppTy ppstrm (ty,d-1)),
		      style=INCONSISTENT}
		     args;
	      if arrowfl then ()
		else ppSymPath ppstrm (SymPath.SPATH path);
	      end_block ppstrm)
	  end
	 | ppTy'(RecordTy fields,d) =
	      ppClosedSequence ppstrm
		{front=(C add_string "{"),
		 sep=(fn ppstrm => (add_string ppstrm ",";
				    add_break ppstrm (0,0))),
		 back=(fn ppstrm => add_string ppstrm "}"),
		 pr=(fn ppstrm => fn (sym,ty) =>
		     (ppSym ppstrm sym; add_string ppstrm " : ";
		      ppTy'(ty,d-1))),
		 style=INCONSISTENT}
		fields
	 | ppTy'(TupleTy fields,d) =
	      ppClosedSequence ppstrm
		{front=(C add_string "("),
		 sep=(fn ppstrm => (add_string ppstrm " * ";
				    add_break ppstrm (0,0))),
		 back=(C add_string ")"),
		 pr=(fn _ => fn ty => ppTy'(ty,d-1)),
		 style=INCONSISTENT}
 		fields
	 | ppTy'(MarkTy(ty,_),d) = ppTy ppstrm (ty,d)
     in ppTy'
    end
 
end (* structure Ast *)

(*
 * $Log$
# Revision 1.2  99/01/18  20:18:41  pscheng
# *** empty log message ***
# 
# Revision 1.1  1998/02/01  01:28:12  pscheng
# Changes to facilitate bootstrapping:
#   Added ascription in various places
#   Split up files into signature and code
#
 * Revision 1.1.1.1  1997/01/14  01:38:43  george
 *   Version 109.24
 *
 *)
