(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* signature ARG_PARSER is the signature that will be matched by parsers whose
    lexer takes an additional argument.
*)

signature ARG_PARSER =
    sig
        structure Token : TOKEN
	structure Stream : STREAM
	exception ParseError

	type arg
	type lexarg
	type pos
	type result
	type svalue

	val makeLexer : (int -> string) -> lexarg ->
			 (svalue,pos) Token.token Stream.stream
	val parse : int * ((svalue,pos) Token.token Stream.stream) *
		    (string * pos * pos -> unit) * arg ->
				result * (svalue,pos) Token.token Stream.stream

	val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
				bool
     end
