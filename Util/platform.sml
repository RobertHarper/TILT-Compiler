(*$import PLATFORM Util String OS Posix *)

structure Platform :> PLATFORM = 

struct

    val error = fn s => Util.error "platform.sml" s
    datatype platform = NT | DUNIX | SOLARIS | LINUX

    (* get : string -> string *)
    fun get name =
	let fun match (n,_) = n = name
	in
	    case List.find match (Posix.ProcEnv.uname ())
	      of NONE => error ("uname doesn't provide " ^ name)
	       | SOME (_,value) => value
	end

    (* platform : unit -> platform. *)
    fun platform () =
	(case get "sysname"
	   of "SunOS" => SOLARIS
	    | "OSF1" => DUNIX
	    | "Linux" => LINUX
	    | sysname => error ("unrecognized sysname " ^ sysname))

    (* hostname : unit -> string *)
    fun hostname () = get "nodename"
	      
    fun spinMilli start 0 = ()
      | spinMilli start n = 
	let fun spin 0 = ()
	      | spin n = spin (n-1)
	in spin start; spinMilli start (n-1)
	end
    val spinMilliNT = spinMilli 50000      (* timed for baked *)
    val spinMilliDUNIX = spinMilli 35000   (* timed for tcl *)

    (*  Posix.Process.sleep does not work for times under 1.0 seconds
          since it seems to round down to the nearest second.
	OS.IO.poll (as a way to sleep) works on the Sun but not the Alpha.
    *)
    fun sleep duration = 
	(case platform() of
	     DUNIX   => spinMilliDUNIX (1 + Real.floor(duration * 1000.0))
	   | SOLARIS => (OS.IO.poll([], SOME (Time.fromReal duration)); ())
	   | LINUX   => (OS.IO.poll([], SOME (Time.fromReal duration)); ())
	   | NT      => spinMilliNT (1 + Real.floor(duration * 1000.0)))

    fun pid() = 
	(case platform() of
	     DUNIX   => Posix.Process.pidToWord(Posix.ProcEnv.getpid())
	   | SOLARIS => Posix.Process.pidToWord(Posix.ProcEnv.getpid())
	   | LINUX   => Posix.Process.pidToWord(Posix.ProcEnv.getpid())
	   | NT      => let val time = Time.toReal(Time.now())
			    val floor = Real.realFloor time
			    val pid = Word32.fromInt(Real.floor((time - floor) * 10000.0))
			in  (print "Cannot obtain pid on NT so we use first 4 decimal digits of time.";
			     print "Chose "; print (Int.toString (Word32.toInt pid)); print ".\n";
			     pid)
			end)

end
