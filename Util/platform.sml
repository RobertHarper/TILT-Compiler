(*$import PLATFORM Util String OS *)

structure Platform :> PLATFORM = 

struct

    val error = fn s => Util.error "platform.sml" s
    datatype platform = NT | DUNIX | SOLARIS | LINUX
	
    val platform = 
	let val sysOpt = (case OS.Process.getEnv "SYS" of
			  NONE => OS.Process.getEnv "OS"
			| some => some)
	in  (case sysOpt of
		 NONE => error "SYS and OS envoronment variable both unset"
	       | SOME sys => 
		   if (sys = "Windows_NT") then NT
		   else if (String.substring(sys,0,3) = "sun")
			  then SOLARIS
		   else if (String.substring(sys,0,5) = "alpha")
			  then DUNIX
		   else if (String.substring(sys,0,10) = "i386_linux")
			  then LINUX
		   else error ("Unrecognized system/OS type: " ^ sys))
	end

    fun spinMilli 0 = ()
      | spinMilli n = 
	let fun spin 0 = ()
	      | spin n = spin (n-1)
	in spin 50000; spinMilli (n-1)
	end

    fun sleep duration = 
	(case platform of
	     DUNIX   => (OS.IO.poll([], SOME (Time.fromReal duration)); ())
	   | SOLARIS => (OS.IO.poll([], SOME (Time.fromReal duration)); ())
	   | LINUX   => (OS.IO.poll([], SOME (Time.fromReal duration)); ())
	   | NT      => spinMilli (1 + Real.floor(duration * 1000.0)))

    fun pid() = 
	(case platform of
	     DUNIX   => Posix.Process.pidToWord(Posix.ProcEnv.getpid())
	   | SOLARIS => Posix.Process.pidToWord(Posix.ProcEnv.getpid())
	   | LINUX   => Posix.Process.pidToWord(Posix.ProcEnv.getpid())
	   | NT      => let val time = Time.toReal(Time.now())
			    val floor = Real.realFloor time
			    val pid = Word32.fromInt(Real.floor((time - floor) * 1000.0))
			in  (print "Cannot obtain pid on NT so we use first 3 decimal digits of time.";
			     print "Chose "; print (Int.toString (Word32.toInt pid)); print ".\n";
			     pid)
			end)

end
