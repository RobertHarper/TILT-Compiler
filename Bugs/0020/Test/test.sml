(*$import TextIO TopLevel *)

(* extern Breakpoint : (unit, unit) --> *)

fun printLines (lineno, stream) =
    let val line = TextIO.inputLine stream
(* 	val _ = if lineno = 172 then Ccall(Breakpoint,()) else () *)
	val _ = print (Int.toString lineno ^ " <<" ^ String.toString line ^ ">>\n")
    in
	if line = "" then ()
	else printLines (lineno+1, stream)
    end

val stream = TextIO.openIn "sparc.sig.sml.info"
val _ = printLines (1, stream)
val _ = TextIO.closeIn stream

val _ = print "done\n"
