(*$import Prelude TopLevel Fixity ErrorMsg Ast Util Listops Symbol Formatter ASTUTIL PrintUtil *)

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


fun QuoteExp s = AppExp{function=VarExp quoteDcon,argument=StringExp s}
fun AntiquoteExp e = AppExp{function=VarExp antiquoteDcon,argument= e}

end (* structure *)


(*
 * $Log$
# Revision 1.5  2000/09/12  18:56:46  swasey
# Changes for cutoff compilation
# 
# Revision 1.4  98/02/15  22:43:19  pscheng
# bootstrapping changes
# 
# Revision 1.3  1998/02/01  01:27:54  pscheng
# Changes to facilitate bootstrapping:
#   Added ascription in various places
#   Split up files into signature and code
#
# Revision 1.2  1998/01/21  20:40:09  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.1  97/03/26  14:12:23  pscheng
# added copy of SMLNJ parser files
# 
 * Revision 1.1.1.1  1997/01/14  01:38:43  george
 *   Version 109.24
 *
 *)
