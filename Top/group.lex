(* Lexer for group files. *)

structure T = Tokens

type ('a,'b) token = ('a,'b) T.token
type pos = int
type svalue = T.svalue
type arg = ExtSyn.lexarg

type lexresult = (svalue,pos) token

fun eof ({start, parseError, ...} : ExtSyn.lexarg) =
    let val _ = (case start()
		   of NONE => ()
		    | SOME (what,pos) =>
		       parseError("unterminated " ^ what,pos,pos))
    in  T.EOF(0,0)
    end

fun uconv (base : int, digit : char -> int) (cs : char list) : int =
    #2 (foldr (fn (c,(m,s)) => (m*base,s + m*(digit c))) (1,0) cs)

val dconv : char list -> int = uconv(10,fn c => ord c - 48)
val hconv : char list -> int =
    uconv(16,
	  fn c =>
	  let val n = ord c
	  in  if n < 65 then n - 48
	      else if n < 97 then n - 55
	      else n - 87
	  end)

fun literal (skip : int, conv : char list -> int,
	     parseError : string * int * int -> unit, yytext : string,
	     yypos : int) : lexresult =
    let val cs = explode yytext
	val (negate,cs) = if hd cs = #"~" then (true,tl cs) else (false,cs)
	val cs = List.drop(cs,skip)
	val u = conv cs
	val n = if negate then ~u else u
    in  T.INT(n, yypos, yypos+(size yytext)-1)
    end handle Overflow =>
	   (parseError ("integer too big", yypos, yypos+(size yytext)-1);
	    T.INT(~1,yypos,yypos))

fun escape (skip : int, conv : char list -> int,
	    parseError : string * int * int -> unit,
	    addToStr : string -> unit,
	    yytext : string, yypos : int) : unit =
    let val cs = List.drop(explode yytext,skip)
	val n = conv cs
    in  if n >= 0 andalso n <= Char.maxOrd then
	    addToStr (str(chr n))
	else
	    parseError ("character escape too big", yypos,
			yypos+(size yytext)-1)
    end

%%
%header (functor GroupLexFun (structure Tokens : Group_TOKENS));
%arg ({newline,startCom,nestCom,endCom,startStr,addToStr,finishStr,parseError,...} : ExtSyn.lexarg);
%full

lws = (\032 | \t | \013 | \012)+;

a = [A-Za-z_'];
d = [0-9];
h = [0-9A-Fa-f];
id = {a} ({a} | {d})*;

q = \034;
e = \092;
qtext = (\032 | \033 | [\035-\091] | [\093-\126])+;

%s C Q E;
%%
<INITIAL>\n	=> (newline yypos; continue());
<INITIAL>"(*"	=> (YYBEGIN C; startCom yypos; continue());
<C>\n		=> (newline yypos; continue());
<C>"(*"		=> (nestCom(); continue());
<C>"*)"		=> (if endCom() then YYBEGIN INITIAL else (); continue());
<C>[^*(\n]+	=> (continue());
<C>.		=> (continue());
<INITIAL>{lws}	=> (continue());
<INITIAL>"$"	=> (T.DOLLAR (yypos,yypos));
<INITIAL>"."	=> (T.DOT (yypos,yypos));
<INITIAL>"^"	=> (T.CARAT (yypos,yypos));
<INITIAL>"("	=> (T.LPAREN (yypos,yypos));
<INITIAL>")"	=> (T.RPAREN (yypos,yypos));
<INITIAL>"{"	=> (T.LBRACE (yypos,yypos));
<INITIAL>"}"	=> (T.RBRACE (yypos,yypos));
<INITIAL>"="	=> (T.EQ (yypos,yypos));
<INITIAL>"S="	=> (T.SEQ (yypos,yypos+1));
<INITIAL>"B="	=> (T.BEQ (yypos,yypos+1));
<INITIAL>"<"	=> (T.LT (yypos,yypos));
<INITIAL>"<="	=> (T.LE (yypos,yypos+1));
<INITIAL>">"	=> (T.GT (yypos,yypos));
<INITIAL>">="	=> (T.GE (yypos,yypos+1));
<INITIAL>:	=> (T.COLON (yypos,yypos));
<INITIAL>env	=> (T.ENV (yypos,yypos+2));
<INITIAL>true	=> (T.BOOL (true,yypos,yypos+3));
<INITIAL>false	=> (T.BOOL (false,yypos,yypos+4));
<INITIAL>if	=> (T.IF (yypos,yypos+1));
<INITIAL>then	=> (T.THEN (yypos,yypos+3));
<INITIAL>else	=> (T.ELSE (yypos,yypos+3));
<INITIAL>not	=> (T.NOT (yypos,yypos+2));
<INITIAL>andalso => (T.ANDALSO (yypos,yypos+6));
<INITIAL>orelse	=> (T.ORELSE (yypos,yypos+5));
<INITIAL>defined => (T.DEFINED (yypos,yypos+6));
<INITIAL>interface => (T.INTERFACE (yypos,yypos+8));
<INITIAL>val	=> (T.VAL (yypos,yypos+2));
<INITIAL>source	=> (T.SOURCE (yypos,yypos+5));
<INITIAL>compiled => (T.COMPILED (yypos,yypos+7));
<INITIAL>and	=> (T.AND (yypos,yypos+2));
<INITIAL>unit	=> (T.UNIT (yypos,yypos+3));
<INITIAL>primitive => (T.PRIMITIVE (yypos,yypos+8));
<INITIAL>import => (T.IMPORT (yypos,yypos+5));
<INITIAL>group	=> (T.GROUP (yypos,yypos+4));
<INITIAL>include => (T.INCLUDE (yypos,yypos+6));
<INITIAL>make	=> (T.MAKE (yypos,yypos+3));
<INITIAL>executable => (T.EXECUTABLE (yypos,yypos+9));
<INITIAL>library => (T.LIBRARY (yypos,yypos+6));
<INITIAL>#if	=> (T.IF' (yypos,yypos+2));
<INITIAL>#elif	=> (T.ELIF (yypos,yypos+4));
<INITIAL>#else	=> (T.ELSE' (yypos,yypos+4));
<INITIAL>#endif	=> (T.ENDIF (yypos,yypos+5));
<INITIAL>#error => (T.ERROR (yypos,yypos+5));
<INITIAL>{id}	=> (T.ID (yytext,yypos,yypos+(size yytext)-1));
<INITIAL>~?{d}+	=> (literal (0,dconv,parseError,yytext,yypos));
<INITIAL>~?0x{h}+ => (literal (2,hconv,parseError,yytext,yypos));
<INITIAL>{q}	=> (YYBEGIN Q; startStr yypos; continue());
<Q>{q}		=> (YYBEGIN INITIAL; T.STRING (finishStr yypos));
<Q>{e}a		=> (addToStr "\a"; continue());
<Q>{e}b		=> (addToStr "\b"; continue());
<Q>{e}t		=> (addToStr "\t"; continue());
<Q>{e}n		=> (addToStr "\n"; continue());
<Q>{e}v		=> (addToStr "\v"; continue());
<Q>{e}f		=> (addToStr "\f"; continue());
<Q>{e}r		=> (addToStr "\r"; continue());
<Q>{e}"^"\064-\095 =>
	(let val c = String.sub (yytext,2)
	     val ctl = str(chr(ord c - 64))
	 in  addToStr ctl; continue()
	 end);
<Q>{e}{d}{d}{d} =>
	(escape(1,dconv,parseError,addToStr,yytext,yypos);
	 continue());
<Q>{e}u{h}{h}{h}{h} =>
	(escape(2,hconv,parseError,addToStr,yytext,yypos);
	 continue());
<Q>{e}{q}	=> (addToStr "\""; continue());
<Q>{e}{e}	=> (addToStr "\\"; continue());
<Q>{e}{lws}	=> (YYBEGIN E; continue());
<Q>{e}\n	=> (YYBEGIN E; newline (yypos+1); continue());
<Q>{qtext}+	=> (addToStr yytext; continue());
<E>{lws}	=> (continue());
<E>\n		=> (newline yypos; continue());
<E>{e}		=> (YYBEGIN Q; continue());
.		=> (parseError ("illegal character " ^
				Char.toString (String.sub(yytext,0)),
				yypos,yypos);
		    T.EOF(yypos,yypos));
