(*
	Check a few cases where Char.fromString should return NONE.
*)
fun c2s (c:char) : string =
	let	val num = Int.toString(ord c)
		val rep = Char.toString c
	in	num ^ "(" ^ rep ^ ")"
	end

fun check (s:string) : unit =
	(case (Char.fromString s) of
		SOME c =>
			print (s ^ " -> " ^ c2s c ^ "\n")
	|	NONE => ())
	handle e =>
		print (s ^ " -> " ^ exnName e ^ "\n")

val _ = app check
	["\\",
	"\\c",
	"\\F",
	"\\e",
	"\\g",
	"\\N",
	"\\T",
	"\\1",
	"\\11",
	"\\256",
	"\\-65",
	"\\~65",
	"\\?",
	"\\^`",
	"\\^a",
	"\\^z",
	"\\   a",
	"\\   a\\B",
	"\\   \\"]
