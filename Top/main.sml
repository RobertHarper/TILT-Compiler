(*$import Prelude TopLevel List String Int OS MAIN Manager Stats Getopt UtilError Dirs Target *)

structure Main : MAIN =
struct

    val usage = "usage: tilt [-?vs] [-t platform] [-fr flag] [-cCm mapfile] [-S [num/]host] [mapfile ...]"
    val version = "TILT version 0.1 (alpha8)\n"

    datatype cmd =
        Make of string list		(* [mapfile ...] *)
      | SetTarget of Target.platform	(* -t platform *)
      | SetFlag of string		(* -f flag *)
      | ResetFlag of string		(* -r flag *)
      | Clean of string			(* -c mapfile *)
      | CleanAll of string		(* -C mapfile *)
      | Master of string		(* -m mapfile *)
      | Slave				(* -s *)
      | Slaves of int * string		(* -S [num/]host *)
      | PrintUsage			(* -? *)
      | PrintVersion			(* -v *)

    (* isSlaves, isSlave : cmd -> bool *)
    fun isSlaves (Slaves _) = true
      | isSlaves _ = false
    fun isSlave (Slave) = true
      | isSlave _ = false
	
    (* runCmd : cmd -> unit *)
    fun runCmd (Make mapfiles) = List.app Manager.make mapfiles
      | runCmd (SetTarget target) = Target.setTargetPlatform target
      | runCmd (SetFlag flag) = Stats.bool flag := true
      | runCmd (ResetFlag flag) = Stats.bool flag := false
      | runCmd (Clean mapfile) = Manager.purge mapfile
      | runCmd (CleanAll mapfile) = Manager.purgeAll mapfile
      | runCmd (Master mapfile) = Manager.master mapfile
      | runCmd (Slave) = Manager.slave ()
      | runCmd (Slaves arg) = Manager.slaves [arg]
      | runCmd (PrintUsage) = (print usage; print "\n")
      | runCmd (PrintVersion) = (print version;
				 print "(Using basis from ";
				 print (Dirs.getLibDir (Dirs.getDirs ()));
				 print ")\n")

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
    
    exception ArgErr of string

    (* slavesArg : string -> int * string *)
    fun slavesArg arg =
	let fun isslash c = c = #"/"
	    val args = String.fields isslash arg
	    fun error() = raise ArgErr ("argument must have form [num/]host -- " ^ arg)
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

    (* cmdline : string list -> cmd list.  May raise ArgErr *)
    fun cmdline args =
	let
	    val options = [Getopt.Arg   (#"t", SetTarget o Target.platformFromName),
			   Getopt.Arg   (#"f", SetFlag),
			   Getopt.Arg   (#"r", ResetFlag),
			   Getopt.Arg   (#"c", Clean),
			   Getopt.Arg   (#"C", CleanAll),
			   Getopt.Arg   (#"m", Master),
			   Getopt.Noarg (#"s", Slave),
			   Getopt.Arg   (#"S", Slaves o slavesArg),
			   Getopt.Noarg (#"?", PrintUsage),
			   Getopt.Noarg (#"v", PrintVersion)]
	in
	    case Getopt.getopt (options, args)
	      of Getopt.Error msg => raise ArgErr (msg ^ "\n" ^ usage)
	       | Getopt.Success (cmds, mapfiles) =>
		  let 
		      val _ = if List.null cmds andalso List.null mapfiles
				  then raise ArgErr usage
			      else ()
		  in  cmds @ [Make mapfiles]
		  end
	end

    (* errorMsg : exn -> string *)
    fun errorMsg (UtilError.BUG msg) = (* Not yet true that BUG implies a compiler error.
				         "internal error: " ^ *) msg
      | errorMsg (ArgErr msg) = msg
      | errorMsg (e) = exnMessage e

    (* main : string * string list -> OS.Process.status *)
    fun main (_, args) =
	(let val _ = UtilError.showErrors := false
	     val cmds = cmdline args
	 in
	     run cmds; OS.Process.success
	 end)
	     handle e => (print "tilt: ";
			  print (errorMsg e);
			  print "\n";
			  OS.Process.failure)
end
