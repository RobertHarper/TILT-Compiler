structure Main :> MAIN =
struct

    fun reject s = raise Compiler.Reject s

    fun usage () : 'a =
	let val msg = concat
		["usage: tilt [-bmpv] [-a arch] [-fr flag] groupfile ...\n\
		 \       tilt [-v] -s [[num/]host ...]\n\
		 \       tilt [-bv] [-a arch] -c groupfile ...\n"]
	in  TextIO.output (TextIO.stdErr, msg);
	    OS.Process.exit (OS.Process.failure)
	end

    fun setdiaglevel (boot : bool, level : int) : unit =
	let val diaglevel = if boot then Boot.DiagLevel else Manager.DiagLevel
	in  diaglevel := level
	end

    fun setarch (arch : Target.platform option) : unit =
	(case arch
	   of NONE => ()
	    | SOME p => Target.setTargetPlatform p)

    fun make (boot : bool, master : bool, stats : bool, verbose : int,
	      arch : Target.platform option, flags : (bool ref * bool) list,
	      groupfiles : string list) : unit =
	let val _ = setdiaglevel (boot, verbose)
	    val _ = setarch arch
	    val _ = app (fn (r,v) => r := v) flags
	    val f : string -> unit =
		(case (boot,master)
		   of (false,false) => Manager.make
		    | (true,false) => Boot.make
		    | (false,true) => Manager.master
		    | (true,true) => Boot.master)
	    val f = if stats then (fn s => (f s; Stats.print_stats())) else f
	in  app f groupfiles
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

    fun slave (verbose : int, args : string list) : unit =
	let val _ = Manager.DiagLevel := verbose
	in  (case args
	       of nil => Manager.slave()
		| _ => Manager.slaves (map slaveSpec args))
	end

    fun purge (boot : bool, verbose : int, arch : Target.platform option,
	       purgeall : bool, groupfiles : string list) : unit =
	let val _ = setdiaglevel (boot, verbose)
	    val _ = setarch arch
	    val f : string -> unit =
		(case (boot,purgeall)
		   of (false,false) => Manager.purge
		    | (true,false) => Boot.purge
		    | (false,true) => Manager.purgeAll
		    | (true,true) => Boot.purgeAll)
	in  app f groupfiles
	end

    datatype options =
	MAKE of {boot:bool ref,
		 master:bool ref,
		 stats:bool ref,
		 verbose:int ref,
		 arch:Target.platform option ref,
		 flags:(bool ref * bool) list ref}
      | SLAVE of {verbose : int ref}
      | PURGE of {boot:bool ref,
		  verbose:int ref,
		  arch:Target.platform option ref,
		  purgeall:bool ref}

    fun run (opts : options, args : string list) : unit =
	(case opts
	   of MAKE {boot,master,stats,verbose,arch,flags} =>
		make (!boot,!master,!stats,!verbose,!arch,!flags,args)
	    | SLAVE {verbose} => slave (!verbose,args)
	    | PURGE {boot,verbose,arch,purgeall} =>
		purge (!boot,!verbose,!arch,!purgeall,args))

    fun bootstrap (opts : options) : unit =
	(case opts
	   of MAKE {boot,...} => boot
	    | SLAVE _ => usage()
	    | PURGE {boot,...} => boot) := true

    fun master (opts : options) : unit =
	(case opts
	   of MAKE {master,...} => master
	    | _ => usage()) := true

    fun printstats (opts : options) : unit =
	(case opts
	   of MAKE {stats,...} => stats
	    | _ => usage()) := true

    fun verbose (opts : options) : unit =
	let val counter =
		(case opts
		   of MAKE {verbose,...} => verbose
		    | SLAVE {verbose,...} => verbose
		    | PURGE {verbose,...} => verbose)
	    val n = !counter + 1
	    val _ = counter := n
	    val _ =
		if n = 1 then
		    print (concat ["TILT version ", Version.version, "\n\
				   \(Using basis from ", Dirs.libDir(), ")\n"])
		else ()
	in  ()
	end

    fun setarch (opts : options, arch : string) : unit =
	let val r =
		(case opts
		   of MAKE {arch,...} => arch
		    | SLAVE _ => usage()
		    | PURGE {arch,...} => arch)
	    val p =
		(case Target.platformFromName arch
		   of SOME p => p
		    | NONE => reject ("invalid architecture: "  ^ arch))
	in  r := SOME p
	end

    fun setflag (opts : options, flag : string, value : bool) : unit =
	(case opts
	   of MAKE {flags,...} =>
		let val flag = (Stats.bool flag
				handle _ => reject ("invalid flag: " ^ flag))
		in  flags := (flag,value) :: !flags
		end
	    | _ => usage())

    fun slave_mode (opts : options) : options =
	(case opts
	   of MAKE {boot=ref false, master=ref false, stats=ref false,
		    verbose=ref n, arch=ref NONE, flags=ref nil} =>
		SLAVE {verbose=ref n}
	    | _ => usage())

    fun purge_mode (opts : options) : options =
	(case opts
	   of MAKE {boot=ref b, master=ref false, stats=ref false,
		    verbose=ref n, arch=ref a, flags=ref nil} =>
		PURGE {boot=ref b, verbose=ref n, arch=ref a, purgeall=ref false}
	    | PURGE {purgeall,...} => (purgeall := true; opts)
	    | _ => usage())

    fun option (r : options ref) (arg : Arg.arg) : unit =
	let val opts = !r
	    val {argc, eargf, ...} = arg
	in
	    (case argc
	       of #"b" => bootstrap opts
		| #"m" => master opts
		| #"p" => printstats opts
		| #"v" => verbose opts
		| #"a" => setarch (opts, eargf usage)
		| #"f" => setflag (opts,eargf usage,true)
		| #"r" => setflag (opts,eargf usage,false)
		| #"s" => r := slave_mode opts
		| #"c" => r := purge_mode opts
		| _ => usage())
	end

    fun main (_ : string, args : string list) : OS.Process.status =
	let val _ = ExnHandler.Interactive := false
	    val opts : options =
		MAKE {boot=ref false,master=ref false,stats=ref false,
		      verbose=ref 0,arch=ref NONE,flags=ref nil}
	    val r = ref opts
	    val args = Arg.args (option r) args
	in  run (!r, args);
	    OS.Process.success
	end handle e => ExnHandler.printAndExit e

end
