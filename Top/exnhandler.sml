structure ExnHandler :> EXN_HANDLER =
struct
    (*
	Not a Stats.bool because we want each process to
	to have its own value; for example, when using
	a command-line master to run interactive slaves.
    *)
    val Interactive : bool ref = ref true

    fun bomb (e:exn) : bool =
	(case e
	   of UtilError.Reject _ => false
	    | _ => true)

    fun errorMsg (e : exn) : string =
	(case e
	   of UtilError.BUG {file,msg} => file ^ ": " ^ msg
	    | UtilError.Reject {msg} => msg
	    | e => "uncaught exception: " ^ exnMessage e)

    val bug : Word8.word = 0w1
    val reject : Word8.word = 0w10	(* See ../Test/runtest.sml *)

    fun exitStatus (e : exn) : Word8.word =
	(case e
	   of UtilError.Reject _ => reject
	    | _ => bug)

    fun eprint (s : string) : unit = TextIO.output(TextIO.stdErr, s)

    fun print (e : exn) : unit =
	let val _ = eprint "tilt: "
	    val _ = if bomb e then eprint "BUG: " else ()
	    val _ = eprint (errorMsg e)
	    val _ = eprint "\n"
	in  ()
	end

    fun exit (e : exn) : 'a =
	Posix.Process.exit (exitStatus e)

    fun printAndExit (e : exn) : 'a =
	(print e; exit e)
end
