(* Copyright 1992 by AT&T Bell Laboratories
 *)

structure AstUtil:ASTUTIL = struct

  open Symbol Fixity Ast PrintUtil ErrorMsg

val unitPat = RecordPat{def=nil,flexibility=false}
val unitExp = RecordExp nil
val trueDcon = [varSymbol "true"]
val falseDcon = [varSymbol "false"]
val quoteDcon = [varSymbol "QUOTE"]
val antiquoteDcon = [varSymbol "ANTIQUOTE"]
val arrowTycon = tycSymbol "->"
val exnID = Symbol.tycSymbol "exn"
val bogusID = varSymbol "BOGUS"
val symArg = strSymbol "<Parameter>"
val itsym = [varSymbol "it"]

fun checkFix (i, err : ErrorMsg.complainer) =
      if (i < 0) orelse (9 < i)
	then (
	  err COMPLAIN "fixity precedence must be between 0 and 9" nullErrorBody;
	  9)
	else i

(* layered patterns *)

fun lay3 ((x as VarPat _), y, _) = LayeredPat{varPat=x,expPat=y}
  | lay3 (ConstraintPat{pattern,constraint}, y, err) =
	 (err COMPLAIN "illegal (multiple?) type constraints in AS pattern"
		       nullErrorBody;
          case lay3 (pattern,y,err)
           of LayeredPat{varPat,expPat} =>
	     LayeredPat{varPat=varPat,
			expPat=ConstraintPat{pattern=expPat,
					     constraint=constraint}}
            | pat => pat)
  | lay3 (MarkPat(x,_),y, err) = lay3 (x,y,err)
  | lay3 (FlatAppPat[x],y,err) = (err COMPLAIN "parentheses illegal around variable in AS pattern" nullErrorBody; y)
  | lay3 (x,y,err) = (err COMPLAIN "pattern to left of AS must be variable"
			    nullErrorBody; y)

fun lay2 (ConstraintPat{pattern,constraint}, y, err) =
	 (err COMPLAIN "illegal (multiple?) type constraints in AS pattern"
		       nullErrorBody;
          case lay2 (pattern,y,err)
           of LayeredPat{varPat,expPat} =>
	     LayeredPat{varPat=varPat,
			expPat=ConstraintPat{pattern=expPat,
					     constraint=constraint}}
            | pat => pat)
  | lay2 (MarkPat(x,_),y, err) = lay2 (x,y,err)
  | lay2 (FlatAppPat[{item,...}],y,err) = lay3(item,y,err)
  | lay2 p = lay3 p

fun lay (ConstraintPat{pattern,constraint}, y, err) =
         (case lay2 (pattern,y,err)
           of LayeredPat{varPat,expPat} =>
	     LayeredPat{varPat=varPat,
			expPat=ConstraintPat{pattern=expPat,
					     constraint=constraint}}
            | pat => pat)
  | lay (MarkPat(x,_),y, err) = lay (x,y,err)
  | lay p = lay2 p

val layered = lay

(* sequence of declarations *)
fun makeSEQdec (SeqDec a, SeqDec b) = SeqDec(a@b)
  | makeSEQdec (SeqDec a, b) = SeqDec(a@[b])
  | makeSEQdec (a, SeqDec b) = SeqDec(a::b)
  | makeSEQdec (a,b) = SeqDec[a,b]

(* sequence of interface components *)
fun makeSEQtopspec (SeqSpec a, SeqSpec b) = SeqSpec(a@b)
  | makeSEQtopspec (SeqSpec a, b) = SeqSpec(a@[b])
  | makeSEQtopspec (a, SeqSpec b) = SeqSpec(a::b)
  | makeSEQtopspec (a,b) = SeqSpec[a,b]

fun QuoteExp s = AppExp{function=VarExp quoteDcon,argument=StringExp s}
fun AntiquoteExp e = AppExp{function=VarExp antiquoteDcon,argument= e}

end (* structure *)


