(*
	Check a few cases of Char.fromString with and without leading
	whitespace.
*)
fun c2s (c:char) : string =
	let	val num = Int.toString(ord c)
		val rep = Char.toString c
	in	num ^ "(" ^ rep ^ ")"
	end

fun check (s:string, c:char) : unit =
	(case (Char.fromString s) of
		SOME c' =>
			if c = c' then ()
			else print (s ^ " -> SOME " ^ c2s c' ^ ", expected " ^ c2s c ^ "\n")
	|	NONE =>
			print (s ^ " -> NONE, expected " ^ c2s c ^ "\n"))

val _ = app check
	[("A", #"A"),
	("z", #"z"),
	("@", #"@"),
	("~", #"~"),
	("\\a", #"\007"),
	("\\b", #"\008"),
	("\\t", #"\009"),
	("\\n", #"\010"),
	("\\v", #"\011"),
	("\\f", #"\012"),
	("\\r", #"\013"),
	("\\\\", #"\\"),
	("\\\"", #"\""),
	("\\^@", #"\000"),
	("\\^A", #"\001"),
	("\\^Z", #"\026"),
	("\\^_", #"\031"), 
	("\\000", #"\000"),
	("\\097", #"a"),
	("\\255", #"\255"),
	("\\   \t\n\n \\A", #"A"),
	("\\   \t\n\n \\z", #"z"),
	("\\   \t\n\n \\@", #"@"),
	("\\   \t\n\n \\~", #"~"),
	("\\   \t\n\n \\\\n", #"\n"),
	("\\   \t\n\n \\\\t", #"\t"),
	("\\   \t\n\n \\\\\\", #"\\"),
	("\\   \t\n\n \\\\\"", #"\""),
	("\\   \t\n\n \\\\^@", #"\000"),
	("\\   \t\n\n \\\\^A", #"\001"),
	("\\   \t\n\n \\\\^Z", #"\026"),
	("\\   \t\n\n \\\\^_", #"\031"), 
	("\\   \t\n\n \\\\000", #"\000"),
	("\\   \t\n\n \\\\097", #"a"),
	("\\   \t\n\n \\\\255", #"\255")]
