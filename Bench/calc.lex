structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
val error = fn (e,l : int,_) =>
              (output(std_out,"line " ^ (makestring l) ^
                               ": " ^ e ^ "\n"))

%%
%header (functor CalcLexFun(structure Tokens: Calc_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.NUM
                (revfold (fn (a,r) => ord(a)-ord("0")+10*r)
                         (explode yytext) 0,
                  !pos,!pos));
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));
{alpha}+ => (if yytext="print"
                 then Tokens.PRINT(!pos,!pos)
                 else Tokens.ID(yytext,!pos,!pos)
            );
"-"      => (Tokens.SUB(!pos,!pos));
"^"      => (Tokens.CARAT(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());


