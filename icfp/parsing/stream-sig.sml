signature STREAM =
    sig

	type 'a T

	exception Empty

	val empty : 'a T

	val cons : 'a * 'a T -> 'a T
	val lcons : 'a * (unit -> 'a T) -> 'a T
	val delay : (unit -> 'a T) -> 'a T

	val is_empty : 'a T -> bool

	val uncons : 'a T -> 'a * 'a T
	val hd : 'a T -> 'a
	val tl : 'a T -> 'a T
	val ltl : 'a T -> 'a T

        val map : ('a -> 'b) -> ('a T -> 'b T)
        val app : ('a -> unit) -> ('a T -> unit)

        val foldr : ('a * 'b -> 'b) -> 'b -> 'a T -> 'b
        val foldl : ('a * 'b -> 'b) -> 'b -> 'a T -> 'b

        val toList : 'a T -> 'a list

    end
