structure Main :> MAIN =
struct

    structure M = Manager

    val reject = Util.reject

    fun usage () : 'a =
	let val msg =
		"usage: tilt project ...\n\
		\       tilt -o exe project ...\n\
		\       tilt -l lib project ...\n\
		\       tilt -p project ...\n\
		\       tilt -s [[num/]host ...]\n\
		\general options:\n\
		\       -v       increase diagnostics level\n\
		\       -f F     set flag F to true\n\
		\       -r F     set F to false\n\
		\compiler options:\n\
		\       -m       master only\n\
		\       -c U     operate on unit U\n\
		\       -C I     operate on interface I\n\
		\       -t T     compile for target T (sparc, alpha, talx86)\n"
	in  TextIO.output (TextIO.stdErr, msg);
	    OS.Process.exit (OS.Process.failure)
	end

    datatype action =
	MAKE
      | MAKE_EXE of string
      | MAKE_LIB of string
      | PURGE
      | PURGE_ALL
      | RUN_SLAVES

    type options =
	{action : action,
	 master : bool,
	 targets : M.targets}

    fun check (options : options) : unit =
	let val {action,master,targets} = options

	    fun check_purge () : unit =
		if master then usage() else ()

	    fun check_slave () : unit =
		if master orelse not (null targets) then usage() else ()

	in  (case action
	       of MAKE => ()
		| MAKE_EXE _ => ()
		| MAKE_LIB _ => ()
		| PURGE => check_purge()
		| PURGE_ALL => check_purge()
		| RUN_SLAVES => check_slave())
	end

    fun set_action (options : options, newaction : action) : options =
	let val {action,master,targets} = options
	in  (case action
	       of MAKE => {action=newaction,master=master,targets=targets}
		| _ => usage())
	end

    fun set_purge_action (options : options) : options =
	let val {action,master,targets} = options
	    val newaction =
		(case action
		   of MAKE => PURGE
		    | PURGE => PURGE_ALL
		    | PURGE_ALL => PURGE_ALL
		    | _ => usage())
	in  {action=newaction,master=master,targets=targets}
	end

    fun process_arg (arg : options Arg.argument) : options =
	let val {acc=options, argc, eargf, ...} = arg
	    val {action,master,targets} = options
	in
	    (case argc
	       of #"o" => set_action (options, MAKE_EXE (eargf usage))
		| #"l" => set_action (options, MAKE_LIB (eargf usage))
		| #"p" => set_purge_action options
		| #"s" => set_action (options, RUN_SLAVES)
		| #"v" =>
		    (M.DiagLevel := !M.DiagLevel + 1;
		     options)
		| #"f" => (Stats.bool (eargf usage) := true; options)
		| #"r" => (Stats.bool (eargf usage) := false; options)
		| #"m" => {action=action,master=true,targets=targets}
		| #"c" =>
		    let val target = M.unit (eargf usage)
			val targets = target :: targets
		    in  {action=action,master=master,targets=targets}
		    end
		| #"C" =>
		    let val target = M.interface (eargf usage)
			val targets = target :: targets
		    in  {action=action,master=master,targets=targets}
		    end
		| #"t" =>
		    (case Platform.fromString (eargf usage)
		       of SOME objtype => Target.setTarget objtype
			| NONE => usage();
		     options)
		| _ => usage())
	end

    fun slaveSpec (arg : string) : int * string =
	let fun isslash c = c = #"/"
	    fun error() = reject ("argument must have form [num/]host -- " ^ arg)
	    fun nonempty "" = error()
	      | nonempty s = s
	    fun nat num = (case Int.fromString num
			     of NONE => error()
			      | SOME n => if n > 0 then n
					  else error())
	in  (case String.fields isslash arg
	       of [host] => (1, nonempty host)
		| [num, host] => (nat num, nonempty host)
		| _ => error())
	end

    fun run (args : string list, options : options) : unit =
	let val _ = check options
	    val {action,master,targets} = options
	    fun projects () : string list =
		(case args
		   of nil => usage()
		    | _ => args)
	    val with_slave = not master
	in
	    (case action
	       of MAKE => M.make' with_slave (projects(), targets)
		| MAKE_EXE exe =>
		    M.make_exe' with_slave (projects(), exe, targets)
		| MAKE_LIB lib =>
		    M.make_lib' with_slave (projects(), lib, targets)
		| PURGE => M.purge (projects(), targets)
		| PURGE_ALL => M.purgeAll (projects(), targets)
		| RUN_SLAVES =>
		    (case args
		       of [] => M.slave()
			| slaves => M.slaves (map slaveSpec slaves)))
	end

    fun main (_ : string, args : string list) : OS.Process.status =
	let val _ = ExnHandler.Interactive := false
	    val options : options =
		{action=MAKE, master=false, targets=nil}
	    val (args,options) = Arg.arguments process_arg (args,options)
	in  run (args,options);
	    OS.Process.success
	end handle e => ExnHandler.printAndExit e

end
