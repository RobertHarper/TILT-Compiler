(* Lexer for project description files. *)

structure T = Tokens

type ('a,'b) token = ('a,'b) T.token
type pos = Pos.pos
type svalue = T.svalue
type arg = ExtSyn.lexarg

type lexresult = (svalue,pos) token

fun eof ({start, parseError, curpos, ...} : ExtSyn.lexarg) =
    let val _ = (case start()
		   of NONE => ()
		    | SOME (what,pos) =>
		       parseError("unterminated " ^ what,pos,pos))
    in  T.EOF(curpos())
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
	     parseError : string * pos * pos -> unit, yytext : string,
	     pos : pos) : lexresult =
    let val cs = explode yytext
	val (negate,cs) = if hd cs = #"~" then (true,tl cs) else (false,cs)
	val cs = List.drop(cs,skip)
	val u = conv cs
	val n = if negate then ~u else u
    in  T.INT(n,pos,pos)
    end handle Overflow =>
	   (parseError ("integer too big",pos,pos);
	    T.INT(~1,pos,pos))

fun escape (skip : int, conv : char list -> int,
	    parseError : string * pos * pos -> unit,
	    addToStr : string -> unit,
	    yytext : string, pos : pos) : unit =
    let val cs = List.drop(explode yytext,skip)
	val n = conv cs
    in  if n >= 0 andalso n <= Char.maxOrd then
	    addToStr (str(chr n))
	else
	    parseError ("character escape too big", pos, pos)
    end

%%
%header (functor ProjectLexFun (structure Tokens : Project_TOKENS));
%arg ({curpos,curpos',newline,startCom,nestCom,endCom,startStr,addToStr,finishStr,parseError,...} : ExtSyn.lexarg);
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
<INITIAL>\n	=> (newline (); continue());
<INITIAL>"(*"	=> (YYBEGIN C; startCom (); continue());
<C>\n		=> (newline (); continue());
<C>"(*"		=> (nestCom(); continue());
<C>"*)"		=> (if endCom() then YYBEGIN INITIAL else (); continue());
<C>[^*(\n]+	=> (continue());
<C>.		=> (continue());
<INITIAL>{lws}	=> (continue());
<INITIAL>"$"	=> (T.DOLLAR (curpos()));
<INITIAL>"^"	=> (T.CARAT (curpos()));
<INITIAL>"("	=> (T.LPAREN (curpos()));
<INITIAL>")"	=> (T.RPAREN (curpos()));
<INITIAL>"{"	=> (T.LBRACE (curpos()));
<INITIAL>"}"	=> (T.RBRACE (curpos()));
<INITIAL>"="	=> (T.EQ (curpos()));
<INITIAL>"S="	=> (T.SEQ (curpos()));
<INITIAL>"B="	=> (T.BEQ (curpos()));
<INITIAL>"<"	=> (T.LT (curpos()));
<INITIAL>"<="	=> (T.LE (curpos()));
<INITIAL>">"	=> (T.GT (curpos()));
<INITIAL>">="	=> (T.GE (curpos()));
<INITIAL>:	=> (T.COLON (curpos()));
<INITIAL>::	=> (T.COLONCOLON (curpos()));
<INITIAL>env	=> (T.ENV (curpos()));
<INITIAL>true	=> (T.BOOL (true,curpos'(),curpos'()));
<INITIAL>false	=> (T.BOOL (false,curpos'(),curpos'()));
<INITIAL>if	=> (T.IF (curpos()));
<INITIAL>then	=> (T.THEN (curpos()));
<INITIAL>else	=> (T.ELSE (curpos()));
<INITIAL>not	=> (T.NOT (curpos()));
<INITIAL>andalso
		=> (T.ANDALSO (curpos()));
<INITIAL>orelse	=> (T.ORELSE (curpos()));
<INITIAL>defined
		=> (T.DEFINED (curpos()));
<INITIAL>unit	=> (T.UNIT (curpos()));
<INITIAL>interface
		=> (T.INTERFACE (curpos()));
<INITIAL>compiled
		=> (T.COMPILED (curpos()));
<INITIAL>primitive
		=> (T.PRIMITIVE (curpos()));
<INITIAL>val	=> (T.VAL (curpos()));
<INITIAL>include
		=> (T.INCLUDE (curpos()));
<INITIAL>local	=> (T.LOCAL (curpos()));
<INITIAL>#if	=> (T.IF' (curpos()));
<INITIAL>#elif	=> (T.ELIF (curpos()));
<INITIAL>#else	=> (T.ELSE' (curpos()));
<INITIAL>#endif	=> (T.ENDIF (curpos()));
<INITIAL>#error	=> (T.ERROR (curpos()));
<INITIAL>{id}	=> (T.ID (yytext,curpos'(),curpos'()));
<INITIAL>~?{d}+	=> (literal (0,dconv,parseError,yytext,curpos'()));
<INITIAL>~?0x{h}+
		=> (literal (2,hconv,parseError,yytext,curpos'()));
<INITIAL>{q}	=> (YYBEGIN Q; startStr (); continue());
<Q>{q}		=> (YYBEGIN INITIAL; T.STRING (finishStr ()));
<Q>{e}a		=> (addToStr "\a"; continue());
<Q>{e}b		=> (addToStr "\b"; continue());
<Q>{e}t		=> (addToStr "\t"; continue());
<Q>{e}n		=> (addToStr "\n"; continue());
<Q>{e}v		=> (addToStr "\v"; continue());
<Q>{e}f		=> (addToStr "\f"; continue());
<Q>{e}r		=> (addToStr "\r"; continue());
<Q>{e}"^"\064-\095
		=>
	(let val c = String.sub (yytext,2)
	     val ctl = str(chr(ord c - 64))
	 in  addToStr ctl; continue()
	 end);
<Q>{e}{d}{d}{d}
		=>
	(escape(1,dconv,parseError,addToStr,yytext,curpos'());
	 continue());
<Q>{e}u{h}{h}{h}{h}
		=>
	(escape(2,hconv,parseError,addToStr,yytext,curpos'());
	 continue());
<Q>{e}{q}	=> (addToStr "\""; continue());
<Q>{e}{e}	=> (addToStr "\\"; continue());
<Q>{e}{lws}	=> (YYBEGIN E; continue());
<Q>{e}\n	=> (YYBEGIN E; newline (); continue());
<Q>{qtext}+	=> (addToStr yytext; continue());
<E>{lws}	=> (continue());
<E>\n		=> (newline (); continue());
<E>{e}		=> (YYBEGIN Q; continue());
.		=>
	(parseError ("illegal character " ^
		     Char.toString (String.sub(yytext,0)), curpos'(),curpos'());
	 T.EOF(curpos()));
