
(* (Parsing.parse Parse.exp (Parsing.transform Tokens.token (Pos.markstream (Stostream.stostream "hello")))) *)

fun parse s = 
    case ParseString.parse s of
	NONE => print "Sorry, no parse!\n"
      | SOME e => print ((ScurvyPrint.tostring_e e) ^ "\n")

val tokenize = (map (fn (a,b) => a)) o ParseString.tokenize
