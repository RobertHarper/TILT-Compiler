(*
	Command line parsing.
*)
signature ARG =
sig

    type 'a argument =
	{argc : char,
	 acc : 'a,
	 argf : unit -> string option,
	 eargf : (unit -> string) -> string}

    val arguments : ('a argument -> 'a) -> string list * 'a -> string list * 'a

    type arg =
	{argc : char,
	 argf : unit -> string option,
	 eargf : (unit -> string) -> string}

    val args : (arg -> unit) -> string list -> string list

end

(*
	Attribution: Based on the interface described in the arg(2)
	manual page from Plan 9; the following example is taken from
	that manual page.

	Arg.arguments is given a function F, a list of command line
	arguments, an an initial value.  It calls F for each command
	line option, stopping before - or after --, and returns any
	additional command line arguments.  F should reject the option
	character argc by not terminating.  If the option takes an
	optional argument, then F can obtain it with argf().  If the
	option requires an argument, then F can use (eargf fail) where
	fail does not terminate.  Argf and eargf should be called
	before F terminates as their side effects determine how the
	rest of the command line arguments are processed.

	Arg.args is a simpler interface; F can use effects to record
	information about the options it sees.

	EXAMPLE

	This SML program can take option b and option f, which
	requires an argument.

		val _ = print (CommandLine.name())

		fun process_option ({argc,argf,...} : Arg.arg) : unit =
		    (case argc
		       of #"b" => print " -b"
			| #"f" =>
			    (case argf()
			       of NONE => print " -f(no arg)"
				| SOME s => print (" -f(" ^ s ^ ")"))
			| _ => print (" badflag(" ^ str argc ^ ")"))

		val args = Arg.args process_option (CommandLine.arguments())

		val _ =
		    (print (" " ^ Int.toString (length args) ^ " args: ");
		     app (fn arg => print (" (" ^ arg ^ ")")) args;
		     print "\n")

	Here is the output from running the command prog -bffile1 -r
	-f file2 arg1 arg2

		prog -b -f(file1) badflag(r) -f(file2) 2 args:  (arg1) (arg2)
*)
