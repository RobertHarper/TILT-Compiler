(* ml.lex
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *)

(* ml.lex.sml imports Prelude TopLevel StrgHash Control SourceMap Int String Char ML_TOKENS Symbol ErrorMsg TilWord64 TokenTable Vector *)

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
               brack_stack : int ref list ref, (* for frags *)
	       err : pos*pos -> ErrorMsg.complainer}
type arg = lexarg
type ('a,'b) token = ('a,'b) Tokens.token
val eof = fn ({comLevel,err,stringstart,stringtype,sourceMap,
               charlist, brack_stack}:lexarg) => 
	   let val pos = Int.max(!stringstart+2, SourceMap.lastChange sourceMap)
	    in if !comLevel>0 then err (!stringstart,pos) COMPLAIN
					 "unclosed comment" nullErrorBody
		  	      else ();
	       Tokens.EOF(pos,pos)
	   end	
fun addString (charlist,s:string) = charlist := s :: (!charlist)
fun addChar (charlist, c:char) = addString(charlist, String.str c)
fun makeString charlist = (concat(rev(!charlist)) before charlist := nil)

fun atoi(s,i) = 
     let val s = String.substring(s,i,size s - i)
     in  TilWord64.fromDecimalString s
     end

fun xtoi(s,i) = 
     let val s = String.substring(s,i,size s - i)
     in  TilWord64.fromHexString s
     end


fun mysynch (src, pos, parts) =
  let fun digit d = Char.ord d - Char.ord #"0"
      fun cvt digits = foldl (fn(d, n) => 10*n + digit d) 0 (explode digits)
      val r = SourceMap.resynch src
  in  case parts 
        of [col, line] => 
              r (pos, {fileName=NONE,      line=cvt line, column=SOME(cvt col)})
         | [file, col, line] => 
              r (pos, {fileName=SOME file, line=cvt line, column=SOME(cvt col)})
         | _ => impossible "text in (*#line...*)"
  end

fun has_quote s = let
      fun loop i = ((String.sub(s,i) = #"`") orelse loop (i+1))
	    handle _ => false
      in
	loop 0
      end

fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)
%% 
%reject
%s A S F Q AQ L LL LLC LLCQ IMP;
%header (functor MLLexFun(structure Tokens : ML_TOKENS));
%arg ({comLevel,sourceMap,err,charlist,stringstart,stringtype,brack_stack});
idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
ws=("\012"|[\t\ ])*;
nrws=("\012"|[\t\ ])+;
eol=("\013\010"|"\010"|"\013");
some_sym=[!%&$+/:<=>?@~|#*]|\-|\^;
sym={some_sym}|"\\";
quote="`";
full_sym={sym}|{quote};
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
<INITIAL>"#["	=> (Tokens.VECTORSTART(yypos,yypos+1));
<INITIAL>"]"	=> (Tokens.RBRACKET(yypos,yypos+1));
<INITIAL>";"	=> (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("	=> (if (null(!brack_stack))
                    then ()
                    else inc (hd (!brack_stack));
                    Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"	=> (if (null(!brack_stack))
                    then ()
                    else if (!(hd (!brack_stack)) = 1)
                         then ( brack_stack := tl (!brack_stack);
                                charlist := [];
                                YYBEGIN Q)
                         else dec (hd (!brack_stack));
                    Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"."		=> (Tokens.DOT(yypos,yypos+1));
<INITIAL>"..."		=> (Tokens.DOTDOTDOT(yypos,yypos+3));
<INITIAL>"'"("'"?)("_"|{num})?{id}
			=> (TokTable.checkTyvar(yytext,yypos));
<INITIAL>{id}	        => (TokTable.checkToken(yytext,yypos));
<INITIAL>{full_sym}+    => (if (!Control.quotation)
                            then if (has_quote yytext)
                                 then REJECT()
                                 else TokTable.checkToken(yytext,yypos)
                            else TokTable.checkToken(yytext,yypos));
<INITIAL>{sym}+         => (TokTable.checkToken(yytext,yypos));
<INITIAL>{quote}        => (if (!Control.quotation)
                            then (YYBEGIN Q;
                                  charlist := [];
                                  Tokens.BEGINQ(yypos,yypos+1))
                            else (err(yypos, yypos+1)
                                     COMPLAIN "quotation implementation error"
				     nullErrorBody;
                                  Tokens.BEGINQ(yypos,yypos+1)));
<INITIAL>{real}	=> (Tokens.REAL(yytext,yypos,yypos+size yytext));
<INITIAL>[1-9][0-9]* => (Tokens.INT(atoi(yytext, 0),yypos,yypos+size yytext));
<INITIAL>{num}	=> (Tokens.INT0(atoi(yytext, 0),yypos,yypos+size yytext));
<INITIAL>~{num}	=> (Tokens.INT0(atoi(yytext, 0),yypos,yypos+size yytext));
<INITIAL>"0x"{hexnum} => (Tokens.INT0(xtoi(yytext, 2),yypos,yypos+size yytext));
<INITIAL>"~0x"{hexnum} => (Tokens.INT0(TilWord64.snegate(xtoi(yytext, 3)),yypos,yypos+size yytext));
<INITIAL>"0w"{num} => (Tokens.WORD(atoi(yytext, 2),yypos,yypos+size yytext));
<INITIAL>"0wx"{hexnum} => (Tokens.WORD(xtoi(yytext, 3),yypos,yypos+size yytext));
<INITIAL>\"	=> (charlist := [""]; stringstart := yypos;
                    stringtype := true; YYBEGIN S; continue());
<INITIAL>\#\"	=> (charlist := [""]; stringstart := yypos;
                    stringtype := false; YYBEGIN S; continue());
<INITIAL>"(*#line"{nrws}  => 
                   (YYBEGIN L; stringstart := yypos; comLevel := 1; continue());
<INITIAL>"(*$import"{nrws} =>
		   (YYBEGIN IMP; comLevel := 1; Tokens.IMPORT(yypos, yypos+9));
<INITIAL>"(*"	=> (YYBEGIN A; stringstart := yypos; comLevel := 1; continue());
<INITIAL>"*)"	=> (err (yypos,yypos+1) COMPLAIN "unmatched close comment"
		        nullErrorBody;
		    continue());
<INITIAL>\h	=> (err (yypos,yypos) COMPLAIN "non-Ascii character"
		        nullErrorBody;
		    continue());
<INITIAL>.	=> (err (yypos,yypos) COMPLAIN "illegal token" nullErrorBody;
		    continue());
<IMP>{id}                 => (Tokens.STRING(yytext, yypos, yypos+size yytext));
<IMP>{ws}                 => (continue());
<IMP>{eol}                 => (SourceMap.newline sourceMap yypos; continue());
<IMP>"*)"                 => (YYBEGIN INITIAL; comLevel := 0; continue());
<IMP>.			  => (err (yypos, yypos+1) COMPLAIN
			      "ill-formed import directive" nullErrorBody
		              ; YYBEGIN INITIAL; comLevel := 0; continue());
<L>[0-9]+                 => (YYBEGIN LL; charlist := [yytext]; continue());
<LL>\.                    => ((* cheat: take n > 0 dots *) continue());
<LL>[0-9]+                => (YYBEGIN LLC; addString(charlist, yytext); continue());
<LL>0*               	  => (YYBEGIN LLC; addString(charlist, "1");    continue()
		(* note hack, since ml-lex chokes on the empty string for 0* *));
<LLC>"*)"                 => (YYBEGIN INITIAL; mysynch(sourceMap, yypos+2, !charlist); 
		              comLevel := 0; charlist := []; continue());
<LLC>{ws}\"		  => (YYBEGIN LLCQ; continue());
<LLCQ>[^\"]*              => (addString(charlist, yytext); continue());
<LLCQ>\""*)"              => (YYBEGIN INITIAL; mysynch(sourceMap, yypos+3, !charlist); 
		              comLevel := 0; charlist := []; continue());
<L,LLC,LLCQ>"*)" => (err (!stringstart, yypos+1) WARN 
                       "ill-formed (*#line...*) taken as comment" nullErrorBody;
                     YYBEGIN INITIAL; comLevel := 0; charlist := []; continue());
<L,LLC,LLCQ>.    => (err (!stringstart, yypos+1) WARN 
                       "ill-formed (*#line...*) taken as comment" nullErrorBody;
                     YYBEGIN A; continue());
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
<S>\\		=> (err (yypos,yypos+1) COMPLAIN "illegal string escape"
		        nullErrorBody; 
		    continue());


<S>[\000-\031]  => (err (yypos,yypos+1) COMPLAIN "illegal non-printing character in string" nullErrorBody;
                    continue());
<S>({idchars}|{some_sym}|\[|\]|\(|\)|{quote}|[,.;^{}])+|.  => (addString(charlist,yytext); continue());
<F>{eol}	=> (SourceMap.newline sourceMap yypos; continue());
<F>{ws}		=> (continue());
<F>\\		=> (YYBEGIN S; stringstart := yypos; continue());
<F>.		=> (err (!stringstart,yypos) COMPLAIN "unclosed string"
		        nullErrorBody; 
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos+1));
<Q>"^`"	=> (addString(charlist, "`"); continue());
<Q>"^^"	=> (addString(charlist, "^"); continue());
<Q>"^"          => (YYBEGIN AQ;
                    let val x = makeString charlist
                    in
                    Tokens.OBJL(x,yypos,yypos+(size x))
                    end);
<Q>"`"          => ((* a closing quote *)
                    YYBEGIN INITIAL;
                    let val x = makeString charlist
                    in
                    Tokens.ENDQ(x,yypos,yypos+(size x))
                    end);
<Q>{eol}        => (SourceMap.newline sourceMap yypos; addString(charlist,"\n"); continue());
<Q>.            => (addString(charlist,yytext); continue());

<AQ>{eol}       => (SourceMap.newline sourceMap yypos; continue());
<AQ>{ws}        => (continue());
<AQ>{id}        => (YYBEGIN Q; 
                    let val hash = StrgHash.hashString yytext
                    in
                    Tokens.AQID(FastSymbol.rawSymbol(hash,yytext),
				yypos,yypos+(size yytext))
                    end);
<AQ>{sym}+      => (YYBEGIN Q; 
                    let val hash = StrgHash.hashString yytext
                    in
                    Tokens.AQID(FastSymbol.rawSymbol(hash,yytext),
				yypos,yypos+(size yytext))
                    end);
<AQ>"("         => (YYBEGIN INITIAL;
                    brack_stack := ((ref 1)::(!brack_stack));
                    Tokens.LPAREN(yypos,yypos+1));
<AQ>.           => (err (yypos,yypos+1) COMPLAIN
		       ("ml lexer: bad character after antiquote "^yytext)
		       nullErrorBody;
                    Tokens.AQID(FastSymbol.rawSymbol(0,""),yypos,yypos));

