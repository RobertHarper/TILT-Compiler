(*$import Prelude TopLevel Util Stats TopHelp Dirs String Int MANAGER Master Slave Compiler OS *)

functor Manager (val bootMapfile : unit -> string option
		 structure Master : MASTER)
    :> MANAGER = 
struct

  val error = fn s => Util.error "manager.sml" s

  val statFinal = Stats.ff "TimeFinal"
  val resetStats = Stats.tt "ResetStats"
      
  val slave = Slave.run
      
  fun helper runner mapfile =
	let val _ = if !resetStats then Stats.clear_stats() else ()
	in  runner mapfile;
	    if (!Help.chat_ref andalso !statFinal)
		then Stats.print_stats()
	    else ()
	end

  fun master mapfile = 
      let val _ = Help.startTime "Starting compilation"
	  val _ = helper Master.run mapfile
	  val _ = Help.showTime (true,"Finished compilation")
      in  if (!Help.chat_ref) then Help.reshowTimes() else ()
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
	  Help.chat "Started slaves.\n"
      end

  fun tilc arg =
      let fun runner args = 
	  let val {setup,step,complete} = Master.once args
	      val {setup=setup', step=step', complete=complete'} = Slave.slave NONE
	      fun slaveStep state' = #1 (step' state')
	      fun loop (state,state') = 
		  (case (step state) of
		       Master.COMPLETE t => (complete(); complete' state')
		     | Master.PROCESSING (t,state) => loop (state, slaveStep state')
		     | Master.IDLE (t,state, _) => loop (state, slaveStep state'))
	  in  loop (setup(), setup'())
	  end
	  val _ = Help.startTime "Starting compilation"
	  val _ = helper runner arg
	  val _ = Help.showTime (true,"Finished compilation")
      in  Help.reshowTimes()
      end
  fun make mapfile = tilc mapfile

  fun purgeAll mapfile = Master.purgeAll mapfile
  fun purge mapfile = Master.purge mapfile

  fun boot () = (case bootMapfile()
		   of NONE => ()
		    | SOME mapfile => make mapfile)
end

local
    fun noMapfile () = NONE
    fun basisMapfile () =
	Option.map
	(fn dir => OS.Path.joinDirFile {dir=dir,file="mapfile-basis"})
	(OS.Process.getEnv "TILT_LIBDIR")

    structure BootMaster =
	Master (val bootstrap  = true
		val basisMapfile = noMapfile
		val basisImports = nil)
	
    structure Master =
	Master (val bootstrap = false
		val basisMapfile = basisMapfile
		val basisImports = ["Firstlude", "TiltPrim", "Prelude", "TopLevel"])
in
    structure Boot =
	Manager (val bootMapfile = basisMapfile
		 structure Master = BootMaster)

    structure Manager =
	Manager (val bootMapfile = noMapfile
		 structure Master = Master)
end
