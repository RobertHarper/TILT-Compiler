signature BASIC_PARSING =
    sig

	(* Parser with token type 't, result type 'a *)
	type ('a,'t) T

	(* succeed with given value *)
	val succeed : 'a -> ('a,'t) T
	(* fail immediately *)
	val fail : ('a,'t) T

	(* check for end of input *)
	val done : 'a -> ('a,'t) T
	(* admit anything, provided there's something on the input *)
	val any : ('t,'t) T

	(* sequential successful composition of parsers *)
	val -- : ('a,'t) T * ('a -> ('b,'t) T) -> ('b,'t) T
	(* sequential failing composition of parsers *)
	val ## : ('a,'t) T * (Pos.T -> ('a,'t) T) -> ('a,'t) T

	(* grab position *)
	val !! : ('a,'t) T  -> ('a * Pos.T,'t) T

	(* to handle mutually-recursive parsers *)
	val $ : (unit -> ('a,'t) T) -> ('a,'t) T

        (* to construct a recursive parser *)
        val fix : (('a,'t) T -> ('a,'t) T) -> ('a,'t) T

	(* re-parse same input, given result of first parse *)
	val lookahead : ('a,'t) T -> ('a -> ('b,'t) T) -> ('b,'t) T

	(* parse a stream *)
	val parse : ('a,'t) T -> ('t * Pos.T) Stream.T -> 'a option

	(* transform a stream by repeatedly parsing it *)
	val transform : ('a,'t) T -> ('t * Pos.T) Stream.T -> 'a Stream.T

    end ;
