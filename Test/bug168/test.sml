
fun printl s = print (s^"\n")

val _ = printl (case Real.fromString "nan"
		  of SOME r => "SOME("^(Real.toString r)^")"
		   | NONE => "NONE");

val _ = printl (case Real.fromString "inf"
		  of SOME r => "SOME("^(Real.toString r)^")"
		   | NONE => "NONE");

