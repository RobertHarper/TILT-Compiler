
structure Stostream =
struct

    (* converts a string to a stream *)
    fun stostream s =
	foldr Stream.cons Stream.empty (String.explode s)

end