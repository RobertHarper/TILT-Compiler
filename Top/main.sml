structure Main : MAIN =
struct

    fun reject s = raise Compiler.Reject s

    val usage = "usage: tilt [-vVps] [-t platform] [-fr flag] [-cCmMbB groupfile] [-S [num/]host]"

    datatype cmd =
        Make of string			(* -m groupfile *)
      | Boot of string			(* -b groupfile *)
      | SetTarget of Target.platform	(* -t platform *)
      | SetFlag of string		(* -f flag *)
      | ResetFlag of string		(* -r flag *)
      | Clean of string			(* -c groupfile *)
      | CleanAll of string		(* -C groupfile *)
      | Master of string		(* -M groupfile *)
      | BootMaster of string		(* -B groupfile *)
      | Slave				(* -s *)
      | Slaves of int * string		(* -S [num/]host *)
      | Verbose				(* -v *)
      | PrintUsage			(* -? *)
      | PrintVersion			(* -V *)
      | PrintStats			(* -p *)

    (* isSlaves, isSlave : cmd -> bool *)
    fun isSlaves (Slaves _) = true
      | isSlaves _ = false
    fun isSlave (Slave) = true
      | isSlave _ = false

    fun printVersion () : unit =
	(print "TILT version "; print Version.version; print "\n";
	 print "(Using basis from ";
	 print (Dirs.libDir());
	 print ")\n")

    (* runCmd : cmd -> unit *)
    fun runCmd (Make groupfile) = Manager.make groupfile
      | runCmd (Boot groupfile) = Boot.make groupfile
      | runCmd (SetTarget target) = Target.setTargetPlatform target
      | runCmd (SetFlag flag) = Stats.bool flag := true
      | runCmd (ResetFlag flag) = Stats.bool flag := false
      | runCmd (Clean groupfile) = Manager.purge groupfile
      | runCmd (CleanAll groupfile) = Manager.purgeAll groupfile
      | runCmd (Master groupfile) = Manager.master groupfile
      | runCmd (BootMaster groupfile) = Boot.master groupfile
      | runCmd (Slave) = Manager.slave ()
      | runCmd (Slaves arg) = Manager.slaves [arg]
      | runCmd (Verbose) = (Manager.DiagLevel := !Manager.DiagLevel + 1;
			    Boot.DiagLevel := !Boot.DiagLevel + 1;
			    if !Manager.DiagLevel = 1 then
				printVersion()
			    else ())
      | runCmd (PrintUsage) = (print usage; print "\n")
      | runCmd (PrintVersion) = printVersion()
      | runCmd (PrintStats) = Stats.print_stats()

    (* run : cmd list -> unit.
     * As a hack, we launch any remote slaves first since they operate in the
     * background and any local slave last since it won't terminate.
     *)
    fun run cmds =
	let
	    val (remoteSlaves, cmds) = List.partition isSlaves cmds
	    val remoteSlaves = List.map (fn (Slaves arg) => arg) remoteSlaves
	    val (localSlave, cmds) = List.partition isSlave cmds
	in
	    if List.null remoteSlaves then () else Manager.slaves remoteSlaves;
	    List.app runCmd cmds;
	    if List.null localSlave then () else Manager.slave()
	end

    (* slavesArg : string -> int * string *)
    fun slavesArg arg =
	let fun isslash c = c = #"/"
	    val args = String.fields isslash arg
	    fun error() = reject ("argument must have form [num/]host -- " ^ arg)
	    fun nonempty "" = error()
	      | nonempty s = s
	    fun nat num = (case Int.fromString num
			     of NONE => error()
			      | SOME n => if n > 0 then n
					  else error())
	in
	    case args
	      of [host] => (1, nonempty host)
	       | [num, host] => (nat num, nonempty host)
	       | _ => error()
	end

    fun platform (target : string) : Target.platform =
	(case Target.platformFromName target
	   of SOME platform => platform
	    | NONE => reject ("invalid target platform: " ^ target))

    fun cmdline (args : string list) : cmd list =
	let
	    val options = [Getopt.Arg   (#"t", SetTarget o platform),
			   Getopt.Arg   (#"f", SetFlag),
			   Getopt.Arg   (#"r", ResetFlag),
			   Getopt.Arg   (#"c", Clean),
			   Getopt.Arg   (#"C", CleanAll),
			   Getopt.Arg   (#"m", Make),
			   Getopt.Arg   (#"b", Boot),
			   Getopt.Arg   (#"M", Master),
			   Getopt.Arg   (#"B", BootMaster),
			   Getopt.Noarg (#"s", Slave),
			   Getopt.Arg   (#"S", Slaves o slavesArg),
			   Getopt.Noarg (#"v", Verbose),
			   Getopt.Noarg (#"?", PrintUsage),
			   Getopt.Noarg (#"V", PrintVersion),
			   Getopt.Noarg (#"p", PrintStats)]
	in
	    case Getopt.getopt (options, args)
	      of Getopt.Error msg => reject (msg ^ "\n" ^ usage)
	       | Getopt.Success (cmds, args) =>
		  let
		      val _ = if List.null cmds orelse not (List.null args)
				  then reject usage
			      else ()
		  in  cmds
		  end
	end

    fun main (_ : string, args : string list) : OS.Process.status =
	(ExnHandler.Interactive := false;
	 run (cmdline args);
	 OS.Process.success) handle e => ExnHandler.printAndExit e

end
