structure Stamp (* : STAMP *) =
    struct

	type stamp = word

	val compare = Word.compare
	fun eq (w1, w2) =
	    case compare (w1, w2)
	      of EQUAL => true
	       | _ => false

	(* simple counter *)

	val counter = ref 0w0

	fun new_stamp () =
	    let
		val n = !counter
	    in
		counter := n + 0w1;
		n
	    end

	(* randomized version; danger, will robinson! what if the generator repeats? *)
	val new_stamp = Rand.mkRandom 0w0

    end
