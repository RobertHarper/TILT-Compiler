(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* ARG_LEXER: the %arg option of ML-Lex allows users to produce lexers which
   also take an argument before yielding a function from unit to a token
*)

signature ARG_LEXER =
   sig
       structure UserDeclarations :
	   sig
	        type ('a,'b) token
		type pos
		type svalue
		type arg
	   end
	val makeLexer : (int -> string) -> UserDeclarations.arg -> unit ->
         (UserDeclarations.svalue,UserDeclarations.pos) UserDeclarations.token
   end
