structure TopLevel :> TOPLEVEL =
struct

    exception Impossible
    
    open Scurvy
    open Parse
    open Parsing

    val infixes = 
	[infix_primop ("+", LeftAssoc, 1),
	 infix_primop ("-", LeftAssoc, 1),
	 infix_primop ("*", LeftAssoc, 2),
	 infix_primop ("div", LeftAssoc, 2),
	 infix_primop ("mod", LeftAssoc, 2)] :
	(string * (Scurvy.exp Parsing.Opr,Tokens.token) Parsing.T) list


    fun getint (Int x) = x
      | getint _ = raise Impossible


    val vals = [(Internalfun (Int o ~ o getint), ARROW(TUPLE[INT,INT],
						       INT), "~")
		]

end
