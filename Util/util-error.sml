structure UtilError :> UTIL_ERROR =
struct
    (*
	Not a Stats.bool because we want each process to
	to have its own value; for example, when using
	a command-line master to run interactive slaves.
    *)
    val Interactive : bool ref = ref true

    exception BUG of {file : string, msg : string}
    exception Reject of {msg : string}
    exception BadMagicNumber of string

    fun error (file : string) (msg : string) : 'a =
	raise BUG {file=file,msg=msg}

    fun reject (msg : string) : 'a =
	raise Reject {msg=msg}

    fun bomb (e:exn) : bool =
	(case e of
	    Reject _ => false
	    (* Assume the user goofed and handed us a bad file. *)
	|   BadMagicNumber _ => false
	|   _ => true)

    fun errormsg (e : exn) : string =
	(case e of
	    BUG {file,msg} => file ^ ": " ^ msg
	|   Reject {msg} => msg
	|   BadMagicNumber file => "bad magic number in " ^ file
	|   e => "uncaught exception: " ^ exnMessage e)

    fun exitstatus (e : exn) : Word8.word =
	if bomb e then 0w1 else 0w10	(* See ../Test/runtest.sml *)

    fun eprint (s : string) : unit = TextIO.output(TextIO.stdErr, s)

    fun print (e : exn) : unit =
	let val msg = concat
		["tilt: ", if bomb e then "BUG: " else "",
		 errormsg e, "\n"]
	    val _ = eprint msg
	in  ()
	end

    fun exit (e : exn) : 'a =
	Posix.Process.exit (exitstatus e)

    fun print_and_exit (e : exn) : 'a =
	let val _ = (print e handle _ => ())
	in  exit e
	end

end
