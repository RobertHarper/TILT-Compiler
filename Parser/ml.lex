(* ml.lex
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *)

open ErrorMsg;

structure TokTable = TokenTable(Tokens);
type svalue = Tokens.svalue
type pos = int
type lexresult = (svalue,pos) Tokens.token
type lexarg = {comLevel : int ref,
	       sourceMap : SourceMap.sourcemap,
	       charlist : string list ref,
	       stringtype : bool ref,
	       stringstart : int ref, (* start of current string or comment*)
	       err : pos*pos -> ErrorMsg.complainer}
type arg = lexarg
type ('a,'b) token = ('a,'b) Tokens.token
val eof = fn ({comLevel,err,stringstart,stringtype,sourceMap,
               charlist, ...}:lexarg) =>
	   let val pos = Int.max(!stringstart+2, SourceMap.lastChange sourceMap)
	    in if !comLevel>0 then err (!stringstart,pos) COMPLAIN
					 "unclosed comment" nullErrorBody
		  	      else ();
	       Tokens.EOF(pos,pos)
	   end
fun addString (charlist,s:string) = charlist := s :: (!charlist)
fun addChar (charlist, c:char) = addString(charlist, String.str c)
fun makeString charlist = (concat(rev(!charlist)) before charlist := nil)

fun atoi(err,p,s,i) =
     let val s = String.substring(s,i,size s - i)
     in  TilWord64.fromDecimalString s
	 handle Overflow =>
	     (err(p,p+size s) COMPLAIN "decimal constant too large" nullErrorBody;
	      TilWord64.zero)
     end

fun xtoi(err,p,s,i) =
     let val s = String.substring(s,i,size s - i)
     in  TilWord64.fromHexString s
	 handle Overflow =>
	     (err(p,p+size s) COMPLAIN "hex constant too large" nullErrorBody;
	      TilWord64.zero)
     end

fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

%%
%reject
%s A S F;
%header (functor MLLexFun(structure Tokens : ML_TOKENS));
%arg ({comLevel,sourceMap,err,charlist,stringstart,stringtype});
idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
ws=("\012"|[\t\ ])*;
eol=("\013\010"|"\010"|"\013");
some_sym=[!%&$+/:<=>?@~`|#*]|\-|\^;
sym={some_sym}|"\\";
num=[0-9]+;
frac="."{num};
exp=[eE](~?){num};
real=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
hexnum=[0-9a-fA-F]+;
%%
<INITIAL>{ws}	=> (continue());
<INITIAL>{eol}	=> (SourceMap.newline sourceMap yypos; continue());
<INITIAL>"_"	=> (Tokens.WILD(yypos,yypos+1));
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>"{"	=> (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"	=> (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"["	=> (Tokens.LBRACKET(yypos,yypos+1));
<INITIAL>"]"	=> (Tokens.RBRACKET(yypos,yypos+1));
<INITIAL>";"	=> (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("	=> (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"	=> (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"."		=> (Tokens.DOT(yypos,yypos+1));
<INITIAL>"..."		=> (Tokens.DOTDOTDOT(yypos,yypos+3));
<INITIAL>"'"("'"?)("_"|{num}|"'")?{id}?
			=> (TokTable.checkTyvar(yytext,yypos));
<INITIAL>{id}	        => (TokTable.checkToken(yytext,yypos));
<INITIAL>{sym}+         => (TokTable.checkToken(yytext,yypos));
<INITIAL>{real}	=> (Tokens.REAL(yytext,yypos,yypos+size yytext));
<INITIAL>[0-9]	=> (Tokens.DIGIT(Char.ord(String.sub(yytext,0))-Char.ord #"0",yypos,yypos+1));
<INITIAL>[1-9][0-9]* => (Tokens.INT(atoi(err, yypos, yytext, 0),yypos,yypos+size yytext));
<INITIAL>{num}	=> (Tokens.INT0(atoi(err, yypos, yytext, 0),yypos,yypos+size yytext));
<INITIAL>~{num}	=> (Tokens.INT0(atoi(err, yypos, yytext, 0),yypos,yypos+size yytext));
<INITIAL>"0x"{hexnum} => (Tokens.INT0(xtoi(err, yypos, yytext, 2),yypos,yypos+size yytext));
<INITIAL>"~0x"{hexnum} => (Tokens.INT0(TilWord64.snegate(xtoi(err, yypos, yytext, 3)),yypos,yypos+size yytext));
<INITIAL>"0w"{num} => (Tokens.WORD(atoi(err, yypos, yytext, 2),yypos,yypos+size yytext));
<INITIAL>"0wx"{hexnum} => (Tokens.WORD(xtoi(err, yypos, yytext, 3),yypos,yypos+size yytext));
<INITIAL>\"	=> (charlist := [""]; stringstart := yypos;
                    stringtype := true; YYBEGIN S; continue());
<INITIAL>\#\"	=> (charlist := [""]; stringstart := yypos;
                    stringtype := false; YYBEGIN S; continue());
<INITIAL>"(*"	=> (YYBEGIN A; stringstart := yypos; comLevel := 1; continue());
<INITIAL>\h	=> (err (yypos,yypos) COMPLAIN "non-Ascii character"
		        nullErrorBody;
		    continue());
<INITIAL>.	=> (err (yypos,yypos) COMPLAIN "illegal token" nullErrorBody;
		    continue());
<A>"(*"		=> (inc comLevel; continue());
<A>{eol}	=> (SourceMap.newline sourceMap yypos; continue());
<A>"*)" => (dec comLevel; if !comLevel=0 then YYBEGIN INITIAL else (); continue());
<A>.		=> (continue());
<S>\"	        => (let val s = makeString charlist
                        val s = if size s <> 1 andalso not(!stringtype)
                                 then (err(!stringstart,yypos) COMPLAIN
                                      "character constant not length 1"
                                       nullErrorBody;
                                       substring(s^"x",0,1))
                                 else s
                        val t = (s,!stringstart,yypos+1)
                    in YYBEGIN INITIAL;
                       if !stringtype then Tokens.STRING t else Tokens.CHAR t
                    end);
<S>{eol}	=> (err (!stringstart,yypos) COMPLAIN "unclosed string"
		        nullErrorBody;
		    SourceMap.newline sourceMap yypos;
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos));
<S>\\{eol}     	=> (SourceMap.newline sourceMap (yypos+1);
		    YYBEGIN F; continue());
<S>\\{ws}   	=> (YYBEGIN F; continue());
<S>\\a		=> (addString(charlist, "\007"); continue());
<S>\\b		=> (addString(charlist, "\008"); continue());
<S>\\f		=> (addString(charlist, "\012"); continue());
<S>\\n		=> (addString(charlist, "\010"); continue());
<S>\\r		=> (addString(charlist, "\013"); continue());
<S>\\t		=> (addString(charlist, "\009"); continue());
<S>\\v		=> (addString(charlist, "\011"); continue());
<S>\\\\		=> (addString(charlist, "\\"); continue());
<S>\\\"		=> (addString(charlist, "\""); continue());
<S>\\\^[@-_]	=> (addChar(charlist,
			Char.chr(Char.ord(String.sub(yytext,2))-Char.ord #"@"));
		    continue());
<S>\\\^.	=>
	(err(yypos,yypos+2) COMPLAIN "illegal control escape; must be one of \
	  \@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_" nullErrorBody;
	 continue());
<S>\\[0-9]{3}	=>
 (let val x = Char.ord(String.sub(yytext,1))*100
	     +Char.ord(String.sub(yytext,2))*10
	     +Char.ord(String.sub(yytext,3))
	     -((Char.ord #"0")*111)
  in (if x>255
      then err (yypos,yypos+4) COMPLAIN "illegal ascii escape" nullErrorBody
      else addChar(charlist, Char.chr x);
      continue())
  end);
<S>\\u[0-9a-fA-F]{4}	=>
 (let fun digit i =
	  let val n = Char.ord(String.sub(yytext,i+1))
	  in  if n < 65 then n - 48
	      else if n < 97 then n - 55
	      else n - 87
	  end
      val x = digit 1*4096+digit 2*256+digit 3*16+digit 4
  in (if x>255
      then err (yypos,yypos+6) COMPLAIN "illegal ascii escape" nullErrorBody
      else addChar(charlist, Char.chr x);
      continue())
  end);
<S>\\		=> (err (yypos,yypos+1) COMPLAIN "illegal string escape"
		        nullErrorBody;
		    continue());


<S>[\000-\031]  => (err (yypos,yypos+1) COMPLAIN "illegal non-printing character in string" nullErrorBody;
                    continue());
<S>({idchars}|{some_sym}|\[|\]|\(|\)|[,.;^{}])+|.  => (addString(charlist,yytext); continue());
<F>{eol}	=> (SourceMap.newline sourceMap yypos; continue());
<F>{ws}		=> (continue());
<F>\\		=> (YYBEGIN S; stringstart := yypos; continue());
<F>.		=> (err (!stringstart,yypos) COMPLAIN "unclosed string"
		        nullErrorBody;
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos+1));
