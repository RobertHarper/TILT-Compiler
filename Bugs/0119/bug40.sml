(*
	Print characters c such that Char.fromCString(Char.toCString c) !=
	SOME c.
*)
fun c2s (c:char) : string =
	let	val num = Int.toString(ord c)
		val rep = Char.toString c
		val rep' = Char.toCString c
	in	concat[num,"(",rep,",",rep',")"]
	end

fun check (c:char) : unit =
	(case (Char.fromCString(Char.toCString c)) of
		SOME c' =>
			if c = c' then ()
			else print (c2s c ^ " -> " ^ c2s c' ^ "\n")
	|	NONE =>
			print (c2s c ^ " ?\n"))
	handle e =>
		print (c2s c ^ " -> " ^ exnName e ^ "\n")

val _ = app check (List.tabulate(256,chr))
