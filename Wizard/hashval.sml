(* These have to be made smarter. *)

structure HashVal (* : HASH_VAL *) =
    struct

	exception NYI

	type hashval = word

	fun nullary (i:word) = i

	fun unary (i:word, s:word) =
	    i + s

	fun binary (i:word, s1:word, s2:word) =
	    i + s1 + s2

	fun ternary (i:word, s1:word, s2:word, s3:word) =
	    i + s1 + s2 + s3

	fun quaternary (i:word, s1:word, s2:word, s3:word, s4:word) =
	    i + s1 + s2 + s3 + s4

	fun n_ary (i:word, sl:word list) =
	    raise NYI

    end
