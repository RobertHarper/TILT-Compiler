(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* signature PARSER is the signature that most user parsers created by
   SML-Yacc will match.
*)

signature PARSER =
    sig
        structure Token : TOKEN
	structure Stream : STREAM
	exception ParseError

	(* type pos is the type of line numbers *)

	type pos

	(* type result is the type of the result from the parser *)

	type result

         (* the type of the user-supplied argument to the parser *)
 	type arg

	(* type svalue is the type of semantic values for the semantic value
	   stack
	 *)

	type svalue

	(* val makeLexer is used to create a stream of tokens for the parser *)

	val makeLexer : (int -> string) ->
			 (svalue,pos) Token.token Stream.stream

	(* val parse takes a stream of tokens and a function to print
	   errors and returns a value of type result and a stream containing
	   the unused tokens
	 *)

	val parse : int * ((svalue,pos) Token.token Stream.stream) *
		    (string * pos * pos -> unit) * arg ->
				result * (svalue,pos) Token.token Stream.stream

	val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
				bool
     end
