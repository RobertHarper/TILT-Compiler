(*$import MANAGER Master Slave LinkParse LinkIl Compiler Linker OS List SplayMapFn SplaySetFn Platform Dirs Delay *)

structure Manager :> MANAGER = 
struct

  val error = fn s => Util.error "manager.sml" s

  open Help

  val slave = Slave.run
  val stat_final = Stats.ff("TimeFinal")

  fun helper runner (mapfile : string, cs : bool) =
	let val _ = if !(Stats.tt "Reset stats between calls") then Stats.clear_stats() else ()
	in  runner(mapfile, cs);
	    if (!chat_ref andalso !stat_final)
		then Stats.print_stats()
	    else ()
	end

  fun master mapfile = 
      let val _ = Til.checkNative()
	  val _ = startTime "Starting compilation"
	  val _ = helper Master.run (mapfile, true)
	  val _ = showTime (true,"Finished compilation")
      in  if (!chat_ref) then reshowTimes() else ()
      end
  fun slaves (slaveList : (int * string) list) =
      let val dirs = Dirs.getDirs()
	  val commDir = Dirs.getCommDir dirs
	  val script = Dirs.relative (Dirs.getBinDir dirs, "til_slave")
	  fun cmdline (num, count, machine) =
	      String.concat [script, " ", Int.toString num, " ", Int.toString count, " ",
			     machine, " ", commDir, "&"]
	  fun startSlave ncm =
	      (OS.Process.system (cmdline ncm); ())
	  fun loop _ [] = ()
	    | loop cur ((count,machine)::rest) = (startSlave(cur,count,machine); loop (cur+count) rest)
      in  loop 0 slaveList;
	  chat "Started slaves.\n"
      end

  fun tilc arg =
      let val _ = Til.checkNative()
	  fun runner args = 
	  let val {setup,step,complete} = Master.once args
	      val _ = Slave.setup()
	      fun loop state = 
		  (case (step state) of
		       Master.COMPLETE t => complete()
		     | Master.PROCESSING (t,state) => (Slave.step(); loop state)
		     | Master.IDLE (t,state, _) => (Slave.step(); loop state))
	  in  loop (setup())
	  end
	  val _ = startTime "Starting compilation"
	  val _ = helper runner arg
	  val _ = showTime (true,"Finished compilation")
      in  reshowTimes()
      end
  fun make mapfile = tilc (mapfile, true)

  fun makeNoLink mapfile = tilc (mapfile,false)

  fun purge mapfile = Master.purge mapfile

  fun buildRuntime rebuild = 
      let val _ = Til.checkNative()
	  val command = if rebuild then "cd Runtime; gmake purge; gmake runtime"
			else "cd Runtime; gmake runtime"
      in  if Util.system command then () else error "Error in building runtime"
      end

  (*
  (* getArgs:
     Takes a list of string arguments and returns a 4-tuple.
     The 1st is the mapfile.
     The 2nd indicates whether the -c flag is present.
     The 3rd carries the name of the -o filename (final executable), if present.
     The 4th component is a list of the source files to process. *)

  val flags = (ref false, ref false, ref false, ref false) (* c, r ,o, all *)

  fun resetFlags () = (#1(flags) := false;
		       #2(flags) := false;
		       #3(flags) := false;
		       #4(flags) := false)

  fun getArgs (args : string list) :  string option * bool * string option * bool * string list = 
      let
	  fun loop args (acc as (mapFile, hasC, oFile, hasAll, srcs)) = 
	      case args of
		  [] => acc
		| ("-c"::rest) => if hasC 
				      then  error ("Two -c switches not allowed.")
				  else loop rest (mapFile, true, oFile, hasAll, srcs)
		| ("-all"::rest) => if hasAll
				      then error ("Two -all switches not allowed.")
				    else if (null srcs)
					     then loop rest (mapFile, hasC, oFile, true, srcs)
					 else error "-all switch given but also units"
		| ["-o"] => error "No output file specified for -o switch." 
		| ("-o" :: file :: rest) => 
				      (case oFile of
					   NONE => loop rest (mapFile, hasC, SOME file, hasAll, srcs)
					 | SOME _ => error "Output file already specified")
		| ["-m"] => error "No output file specified for -m switch." 
		| ("-m" :: file :: rest) => 
				      (case mapFile of
					   NONE => loop rest (SOME file, hasC, oFile, hasAll, srcs)
					 | SOME _ => error "Output file already specified")
		| (src::rest) => if hasAll
				     then error "-all switch given and also unts"
				 else loop rest (mapFile, hasC, oFile, hasAll, src::srcs)
      in  (resetFlags(); loop args (NONE, false, NONE, false, []))
      end

  fun help() = print "This is TILT - no help available.\n"
  fun command(env : string, args : string list) : int =
    case args of 
      [] => (print ("No arguments specified.\n"); 1)
    | ["-h"] => (help(); 0)
    | ["-?"] => (help(); 0)
    | ("-h"::_) =>
	  (print  ("Invalid arguments: -h or -? must occur by itself.\n"); 1)
    | ("-?"::_) =>
	  (print  ("Invalid arguments: -h or -? must occur by itself.\n"); 1)
    | args => 
	let val (mapFile, hasC, oFile, hasAll, srcs) = getArgs args
	    val mapFile = (case mapFile of
			       NONE => "mapfile"
			     | SOME mfile => mfile)
	    val srcs = if hasAll then [] else srcs
	in (tilc(mapFile, hasC, oFile, srcs); 0)
	end
*)
end

