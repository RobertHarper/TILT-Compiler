structure Platform :> PLATFORM =

struct

    val error = fn s => UtilError.error "platform.sml" s
    datatype platform = DUNIX | SOLARIS | LINUX | GENERIC

    fun platformName (p : platform) : string =
	(case p
	   of DUNIX   => "alpha"
	    | SOLARIS => "sparc"
	    | LINUX   => "linux"
	    | GENERIC => "generic")

    fun get (name : string) : string =
	let fun match (n,_) = n = name
	in
	    case List.find match (Posix.ProcEnv.uname ())
	      of NONE => error ("uname doesn't provide " ^ name)
	       | SOME (_,value) => value
	end

    (* platform : unit -> platform. *)
    fun platform () : platform =
	(case get "sysname"
	   of "SunOS" => SOLARIS
	    | "OSF1" => DUNIX
	    | "Linux" => LINUX
	    | _ => GENERIC)

    (* hostname : unit -> string *)
    fun hostname () = get "nodename"

    (*
	This is lame.
    *)
    fun spinMilli start 0 = ()
      | spinMilli start n =
	let fun spin 0 = ()
	      | spin n = spin (n-1)
	in spin start; spinMilli start (n-1)
	end
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
	   | GENERIC => (OS.IO.poll([], SOME (Time.fromReal duration)); ()))

    fun pid() =
	Posix.Process.pidToWord(Posix.ProcEnv.getpid())

end
