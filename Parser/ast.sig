(* Copyright 1992 by AT&T Bell Laboratories 
 *
 *)

signature AST =
sig
  type fixity
  type symbol  (* = Symbol.symbol *)
  val infixleft : int -> fixity
  val infixright : int -> fixity
  type literal = TilWord64.word

  (* to mark positions in files *)
  type srcpos  (* = int *)
  type region  (* = srcpos * srcpos *)
  (* symbolic path (SymPath.spath) *)
  type path
  type 'a fixitem (* = {item: 'a, fixity: symbol option, region: region} *)

  datatype 'a sigConst
    = NoSig
    | Transparent of 'a
    | Opaque of 'a

  (* EXPRESSIONS *)

  datatype top 
    = ImplTop of string list * dec (* top level implementation file *)
    | InterTop of string list * spec list (* top level interface file *)
    | MarkTop of top * region   (* mark a top level file *)

  and exp
    = VarExp of path		(* variable *)
    | FnExp of rule list		(* abstraction *)
    | FlatAppExp of exp fixitem list
                                  (* expressions prior to fixity parsing *)
    | AppExp of {function:exp,argument:exp}
				  (* application *)
    | CaseExp of{expr:exp,rules:rule list}
				  (* case expression *)
    | LetExp of {dec:dec,expr:exp} (* let expression *)
    | SeqExp of exp list		(* sequence of expressions *)
    | IntExp of literal		(* integer *)
    | WordExp of literal	(* word literal *)
    | RealExp of string		(* floating point coded by its string *)
    | StringExp of string	(* string *)
    | CharExp of string		(* char *)
    | RecordExp of (symbol * exp) list	(* record *)
    | ListExp of exp list	(*  [list,in,square,brackets] *)
    | TupleExp of exp list	(* tuple (derived form) *)
    | SelectorExp of symbol	(* selector of a record field *)
    | ConstraintExp of {expr:exp,constraint:ty}
				  (* type constraint *)
    | HandleExp of {expr:exp, rules:rule list}
				  (* exception handler *)
    | RaiseExp of exp		(* raise an exception *)
    | IfExp of {test:exp, thenCase:exp, elseCase:exp}
				  (* if expression (derived form) *)
    | AndalsoExp of exp * exp	(* andalso (derived form) *)
    | OrelseExp of exp * exp	(* orelse (derived form) *)
    | VectorExp of exp list       (* vector *)
    | WhileExp of {test:exp,expr:exp}
				  (* while (derived form) *)
    | MarkExp of exp * region	(* mark an expression *)
    | DelayExp of exp           (* create suspension *)
    | CcallExp of exp * exp list (* call a C function *)

  (* RULE for case functions and exception handler *)
  and rule = Rule of {pat:pat,exp:exp}

  (* PATTERN *)
  and pat = WildPat				(* empty pattern *)
	  | VarPat of path			(* variable pattern *)
	  | IntPat of literal			(* integer *)
	  | WordPat of literal			(* word literal *)
	  | StringPat of string			(* string *)
	  | CharPat of string			(* char *)
	  | RecordPat of {def:(symbol * pat) list, flexibility:bool}
						(* record *)
          | ListPat of pat list		       (*  [list,in,square,brackets] *)
	  | TuplePat of pat list		(* tuple *)
          | FlatAppPat of pat fixitem list
                                        (* patterns prior to fixity parsing *)
	  | AppPat of {constr:pat,argument:pat}(* application *)
	  | ConstraintPat of {pattern:pat,constraint:ty}
						  (* constraint *)
	  | LayeredPat of {varPat:pat,expPat:pat}	(* as expressions *)
          | VectorPat of pat list                 (* vector pattern *)
	  | MarkPat of pat * region	(* mark a pattern *)
	  | OrPat of pat list			(* or-pattern *)
          | DelayPat of pat                     (* match suspension *)

  (* STRUCTURE EXPRESSION *)
  and strexp = VarStr of path			(* variable structure *)
	     | StructStr of dec			(* defined structure *)
	     | AppStr of path * (strexp * bool) list (* application *)
	     | LetStr of dec * strexp		(* let in structure *)
	     | MarkStr of strexp * region (* mark *)

  (* FUNCTOR EXPRESSION *)
  and fctexp = VarFct of path * fsigexp sigConst	(* functor variable *)
	     | FctFct of  {			(* definition of a functor *)
		params	   : (symbol option * sigexp) list,
		body	   : strexp,
		constraint : sigexp sigConst}
	     | LetFct of dec * fctexp
	     | AppFct of path * (strexp * bool) list * fsigexp sigConst
						  (* application *)
	     | MarkFct of fctexp * region (* mark *)

  (* WHERE SPEC *)                                                
  and wherespec = WhType of symbol list * tyvar list * ty
                | WhStruct of symbol list * symbol list
                                                                
  (* SIGNATURE EXPRESSION *)
  and sigexp = VarSig of symbol			(* signature variable *)
             | AugSig of sigexp * wherespec list (* sig augmented with where spec *)
	     | SigSig of spec list		(* defined signature *)
	     | MarkSig of sigexp * region	(* mark *)

  (* FUNCTOR SIGNATURE EXPRESSION *)
  and fsigexp = VarFsig of symbol			(* funsig variable *)
	      | FsigFsig of {param: (symbol option * sigexp) list, def:sigexp}
						  (* defined funsig *)
	      | MarkFsig of fsigexp * region	(* mark a funsig *)

  (* SPECIFICATION FOR SIGNATURE DEFINITIONS *)
  and spec = StrSpec of (symbol * sigexp option * path option) list
                                                                (* structure *)
           | TycSpec of ((symbol * tyvar list * ty option) list * bool)
                                                                (* type *)
	   | FctSpec of (symbol * fsigexp) list			(* functor *)
	   | ValSpec of (symbol * ty) list			(* value *)
	   | DataSpec of {datatycs: db list, withtycs: tb list}	(* datatype *)
	   | ExceSpec of (symbol * ty option) list		(* exception *)
	   | FixSpec of  {fixity: fixity, ops: symbol list} 	(* fixity *)
	   | ShareStrSpec of path list			(* structure sharing *)
	   | ShareTycSpec of path list			(* type sharing *)
	   | IncludeSpec of symbol			(* include specif *)
           | SigSpec of sigb list                       (* signature *)
	   | MarkSpec of spec * region	(* mark a spec *)

  (* DECLARATIONS (let and structure) *)
  and dec = ValDec of vb list * tyvar list ref		(* values *)
	  | ValrecDec of rvb list * tyvar list ref	(* recursive values *)
	  | FunDec of fb list * tyvar list ref		(* recurs functions *)
	  | TypeDec of tb list				(* type dec *)
	  | DatatypeDec of {datatycs: db list, withtycs: tb list}
							  (* datatype dec *)
	  | AbstypeDec of {abstycs: db list, withtycs: tb list, body: dec}
							  (* abstract type *)
	  | ExceptionDec of eb list			(* exception *)
	  | StrDec of strb list				(* structure *)
	  | AbsDec of strb list				(* abstract struct *)
	  | FctDec of fctb list				(* functor *)
	  | SigDec of sigb list				(* signature *)
	  | FsigDec of fsigb list				(* funsig *)
	  | LocalDec of dec * dec				(* local dec *)
	  | SeqDec of dec list				(* sequence of dec *)
	  | OpenDec of path list			(* open structures *)
	  | OvldDec of symbol * ty * exp list	(* overloading (internal) *)
	  | FixDec of {fixity: fixity, ops: symbol list}  (* fixity *)
	  | ImportDec of string list		(* import (unused) *)
	  | ExternDec of symbol * ty      (* declare an external value *)
	  | MarkDec of dec * region		(* mark a dec *)

  (* VALUE BINDINGS *)
  and vb = Vb of {pat:pat, exp:exp}
	 | MarkVb of vb * region

  (* RECURSIVE VALUE BINDINGS *)
  and rvb = Rvb of {var:symbol, fixity: (symbol * region) option,
		    exp:exp, resultty: ty option}
	  | MarkRvb of rvb * region

  (* RECURSIVE FUNCTIONS BINDINGS *)
  and fb = Fb of clause list
	 | MarkFb of fb * region

  (* CLAUSE: a definition for a single pattern in a function binding *)
  and clause = Clause of {pats: pat fixitem list, resultty: ty option, exp:exp}

  (* TYPE BINDING *)
  and tb = Tb of {tyc : symbol, def : ty, tyvars : tyvar list}
	 | MarkTb of tb * region

  (* DATATYPE BINDING *)
  and db = Db of {tyc : symbol, tyvars : tyvar list, rhs : dbrhs}
         | MarkDb of db * region

  (* DATATYPE BINDING RIGHT HAND SIDE *)
  and dbrhs = Constrs of (symbol * ty option) list
            | Repl of symbol list

  (* EXCEPTION BINDING *)
  and eb = EbGen of {exn: symbol, etype: ty option} (* Exception definition *)
	 | EbDef of {exn: symbol, edef: path}	  (* defined by equality *)
	 | MarkEb of eb * region

  (* STRUCTURE BINDING *)
  and strb = Strb of {name: symbol,def: strexp,constraint: sigexp sigConst}
	   | MarkStrb of strb * region

  (* FUNCTOR BINDING *)
  and fctb = Fctb of {name: symbol,def: fctexp}
	   | MarkFctb of fctb * region

  (* SIGNATURE BINDING *)
  and sigb = Sigb of {name: symbol,def: sigexp}
	   | MarkSigb of sigb * region

  (* FUNSIG BINDING *)
  and fsigb = Fsigb of {name: symbol,def: fsigexp}
	    | MarkFsigb of fsigb * region

  (* TYPE VARIABLE *)
  and tyvar = Tyv of symbol
	    | TempTyv of symbol (* temporary used in tyvar binding inference *)
	    | MarkTyv of tyvar * region

  (* TYPES *)
  and ty 
      = VarTy of tyvar			(* type variable *)
      | ConTy of symbol list * ty list	(* type constructor *)
      | RecordTy of (symbol * ty) list 	(* record *)
      | TupleTy of ty list		(* tuple *)
      | MarkTy of ty * region	        (* mark type *)

end (* signature AST *)


(*
 * $Log$
# Revision 1.5  97/10/21  21:00:21  pscheng
# got rid of int inf
# 
# Revision 1.4  1997/09/03  20:10:13  pscheng
# added extern and ccall syntax
#
# Revision 1.3  97/07/02  22:03:02  jgmorris
# Modified syntax to allow for interfaces and implementations.
# 
 * Revision 1.2  1997/05/30 14:12:29  zdance
 * Added support for (*$import...*) and (*$include...*) directives.
 * Also changed the grammar to allow for (possibly) semicolon-separated sequences
 * at the top level declarations.
 *
 * Revision 1.1.1.1  1997/05/23 14:53:49  til
 * Imported Sources
 *
# Revision 1.2  97/04/10  15:04:52  cokasaki
# Added DelayExp and DelayPat to ast.  Added lexical token DOLLAR.
# 
# Revision 1.1  97/03/21  15:24:17  pscheng
# added our local version of the parser
# asthelp.* and name* migrated from Util directory
# 
 * Revision 1.2  1997/01/28  23:20:41  jhr
 * Integer and word literals are now represented by IntInf.int (instead of
 * as strings).
 *
 * Revision 1.1.1.1  1997/01/14  01:38:42  george
 *   Version 109.24
 *
 *)
