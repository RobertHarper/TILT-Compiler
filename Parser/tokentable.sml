(*$import ML_TOKENS Symbol StrgHash IntStrMap *)
(* tokentable.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(***************************************************************************

  TOKEN.SML: hash table for token recognition

 ***************************************************************************)

functor TokenTable(Tokens:ML_TOKENS):
  sig val checkToken: (string * int) -> (Tokens.svalue,int)Tokens.token
      val checkTyvar: (string * int) -> (Tokens.svalue,int)Tokens.token
  end = 
struct

exception NotToken

(* The list of string with their corresponding token *)

val tokenList =
  [("*"		, fn yypos => Tokens.ASTERISK(yypos,yypos+1)),
   ("|"		, fn yypos => Tokens.BAR(yypos,yypos+1)),
   (":"		, fn yypos => Tokens.COLON(yypos,yypos+1)),
   (":>"	, fn yypos => Tokens.COLONGT(yypos,yypos+2)),
   ("="		, fn yypos => Tokens.EQUALOP(yypos,yypos+1)),
   ("#"		, fn yypos => Tokens.HASH(yypos,yypos+1)),
   ("and"	, fn yypos => Tokens.AND(yypos,yypos+3)),
   ("abstraction"	, fn yypos => Tokens.ABSTRACTION(yypos,yypos+11)),
   ("abstype"	, fn yypos => Tokens.ABSTYPE(yypos,yypos+7)),
   ("->"	, fn yypos => Tokens.ARROW(yypos,yypos+2)),
   ("as"	, fn yypos => Tokens.AS(yypos,yypos+2)),
   ("case"	, fn yypos => Tokens.CASE(yypos,yypos+4)),
   ("datatype"	, fn yypos => Tokens.DATATYPE(yypos,yypos+8)),
   ("else"	, fn yypos => Tokens.ELSE(yypos,yypos+4)),
   ("end"	, fn yypos => Tokens.END(yypos,yypos+3)),
   ("eqtype"	, fn yypos => Tokens.EQTYPE(yypos,yypos+6)),
   ("exception"	, fn yypos => Tokens.EXCEPTION(yypos,yypos+9)),
   ("do"	, fn yypos => Tokens.DO(yypos,yypos+2)),
   ("=>"	, fn yypos => Tokens.DARROW(yypos,yypos+2)),
   ("fn"	, fn yypos => Tokens.FN(yypos,yypos+2)),
   ("fun"	, fn yypos => Tokens.FUN(yypos,yypos+3)),
   ("functor"	, fn yypos => Tokens.FUNCTOR(yypos,yypos+7)),
   ("funsig"	, fn yypos => Tokens.FUNSIG(yypos,yypos+7)),
   ("handle"	, fn yypos => Tokens.HANDLE(yypos,yypos+6)),
   ("if"	, fn yypos => Tokens.IF(yypos,yypos+2)),
   ("in"	, fn yypos => Tokens.IN(yypos,yypos+2)),
   ("include"	, fn yypos => Tokens.INCLUDE(yypos,yypos+7)),
   ("infix"	, fn yypos => Tokens.INFIX(yypos,yypos+5)),
   ("infixr"	, fn yypos => Tokens.INFIXR(yypos,yypos+6)),
   ("let"	, fn yypos => Tokens.LET(yypos,yypos+3)),
   ("local"	, fn yypos => Tokens.LOCAL(yypos,yypos+5)),
   ("nonfix"	, fn yypos => Tokens.NONFIX(yypos,yypos+6)),
   ("of"	, fn yypos => Tokens.OF(yypos,yypos+2)),
   ("op"	, fn yypos => Tokens.OP(yypos,yypos+2)),
   ("open"	, fn yypos => Tokens.OPEN(yypos,yypos+4)),
   ("overload"	, fn yypos => Tokens.OVERLOAD(yypos,yypos+8)),
   ("raise"	, fn yypos => Tokens.RAISE(yypos,yypos+5)),
   ("rec"	, fn yypos => Tokens.REC(yypos,yypos+3)),
   ("sharing"	, fn yypos => Tokens.SHARING(yypos,yypos+7)),
   ("sig"	, fn yypos => Tokens.SIG(yypos,yypos+3)),
   ("signature"	, fn yypos => Tokens.SIGNATURE(yypos,yypos+9)),
   ("struct"	, fn yypos => Tokens.STRUCT(yypos,yypos+6)),
   ("structure"	, fn yypos => Tokens.STRUCTURE(yypos,yypos+9)),
   ("then"	, fn yypos => Tokens.THEN(yypos,yypos+4)),
   ("type"	, fn yypos => Tokens.TYPE(yypos,yypos+4)),
   ("val"	, fn yypos => Tokens.VAL(yypos,yypos+3)),
   ("pval"	, fn yypos => Tokens.PVAL(yypos,yypos+4)),
   ("where"	, fn yypos => Tokens.WHERE(yypos,yypos+5)),
   ("while"	, fn yypos => Tokens.WHILE(yypos,yypos+5)),
   ("with"	, fn yypos => Tokens.WITH(yypos,yypos+4)),
   ("withtype"	, fn yypos => Tokens.WITHTYPE(yypos,yypos+8)),
   ("orelse"	, fn yypos => Tokens.ORELSE(yypos,yypos+6)),
   ("andalso"	, fn yypos => Tokens.ANDALSO(yypos,yypos+7)),
   ("extern"    , fn yypos => Tokens.EXTERN(yypos,yypos+6)),
   ("Ccall"     , fn yypos => Tokens.CCALL(yypos,yypos+5)),
   ("plet"     , fn yypos => Tokens.PLET(yypos,yypos+4))
]

(* hash table obtained from the previous list *)
val tokenTable =
  let val t: (int -> (Tokens.svalue,int)Tokens.token) IntStrMap.intstrmap =
    IntStrMap.new(128,NotToken)
  in           
  app (fn (str,tok) => 
	 IntStrMap.add t (StrgHash.hashString str,str,tok)) 
      tokenList;
  t
  end

(* look-up function. If the symbol is found, the corresponding token is
   generated with the position of its begining. Otherwise it is a regular
   identifier. *)
   
fun checkToken (str,yypos) =
  let val hash = StrgHash.hashString str
  in
  IntStrMap.map tokenTable (hash,str) yypos
  handle NotToken => 
    Tokens.ID(FastSymbol.rawSymbol(hash,str),yypos,yypos+size (str))
  end
;

fun checkTyvar (str,yypos) =
  let val hash = StrgHash.hashString str
  in Tokens.TYVAR (FastSymbol.rawSymbol(hash,str),yypos,yypos+size (str)) end

end

(*
 * $Log$
# Revision 1.6  99/05/12  15:27:55  pscheng
# *** empty log message ***
# 
# Revision 1.5  1999/01/18  20:18:46  pscheng
# *** empty log message ***
#
# Revision 1.4  1998/02/15  22:44:08  pscheng
# bootstrapping changes
#
# Revision 1.3  1998/01/21  20:40:52  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.2  1997/09/03  20:10:41  pscheng
# added extern and ccall syntax
#
# Revision 1.1  97/03/26  14:12:38  pscheng
# added copy of SMLNJ parser files
# 
 * Revision 1.1.1.1  1997/01/14  01:38:38  george
 *   Version 109.24
 *
 *)
