(*
	Print characters c such that Char.fromString(Char.toString c) !=
	SOME c.
*)
fun c2s (c:char) : string =
	let	val num = Int.toString(ord c)
		val rep = Char.toString c
	in	num ^ "(" ^ rep ^ ")"
	end

fun check (c:char) : unit =
	(case (Char.fromString(Char.toString c)) of
		SOME c' =>
			if c = c' then ()
			else print (c2s c ^ " -> " ^ c2s c' ^ "\n")
	|	NONE =>
			print (c2s c ^ " ?\n"))

val _ = app check (List.tabulate(256,chr))
