%%
%%
"+"      => (Tokens.PLUS(!pos,!pos));
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());


