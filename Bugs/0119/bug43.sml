(*
	Check a few cases where Char.fromCString should return NONE.
*)
fun c2s (c:char) : string =
	let	val num = Int.toString(ord c)
		val rep = Char.toString c
	in	num ^ "(" ^ rep ^ ")"
	end

fun check (s:string) : unit =
	(case (Char.fromCString s) of
		SOME c =>
			print (s ^ " -> " ^ c2s c ^ "\n")
	|	NONE => ())
	handle e =>
		print (s ^ " -> " ^ exnName e ^ "\n")

val _ = app check
	["\\",
	"\\X",
	"\\=",
	"\\400",
	"\\777",
	"\\8",
	"\\9",
	"\\c",
	"\\d",
	"\\x",
	"\\x100",
	"\\xG"]
