structure Platform :> PLATFORM =
struct

    structure B = Blaster

    val error = fn s => UtilError.error "platform.sml" s

    datatype objtype = SPARC | ALPHA | TALx86

    fun littleEndian SPARC = false
      | littleEndian ALPHA = true
      | littleEndian TALx86 = true

    fun toString SPARC = "sparc"
      | toString ALPHA = "alpha"
      | toString TALx86 = "talx86"

    fun fromString "sparc" = SOME SPARC
      | fromString "alpha" = SOME ALPHA
      | fromString "talx86" = SOME TALx86
      | fromString _ = NONE

    fun blastOutObjtype (os : B.outstream) (ot : objtype) : unit =
	(case ot
	   of ALPHA => B.blastOutInt os 0
	    | SPARC => B.blastOutInt os 1
	    | TALx86 => B.blastOutInt os 2)
    fun blastInObjtype (is : B.instream) : objtype =
	(case B.blastInInt is
	   of 0 => ALPHA
	    | 1 => SPARC
	    | 2 => TALx86
	    | _ => error "bad objtype")
    val (blastOutObjtype,blastInObjtype) =
	B.magic (blastOutObjtype,blastInObjtype,"objtype $Revision$")

    datatype cputype = UNSUPPORTED | SUPPORTED of objtype

    fun get (name : string) : string =
	let fun match (n,_) = n = name
	in
	    case List.find match (Posix.ProcEnv.uname ())
	      of NONE => error ("uname doesn't provide " ^ name)
	       | SOME (_,value) => value
	end

    fun cputype () : cputype =
	(case get "sysname"
	   of "SunOS" => SUPPORTED SPARC
	    | "OSF1" => SUPPORTED ALPHA
(*	    | "Linux" => SUPPORTED TALx86 *)
	    | _ => UNSUPPORTED)

    fun cputypeToString (ct : cputype) : string =
	(case ct
	   of UNSUPPORTED => "unsupported"
	    | SUPPORTED objtype => toString objtype)

    fun blastOutCputype (os : B.outstream) (ct : cputype) : unit =
	(case ct
	   of SUPPORTED objtype => (B.blastOutInt os 0; blastOutObjtype os objtype)
	    | UNSUPPORTED => B.blastOutInt os 1)
    fun blastInCputype (is : B.instream) : cputype =
	(case B.blastInInt is
	   of 0 => SUPPORTED (blastInObjtype is)
	    | 1 => UNSUPPORTED
	    | _ => error "bad cputype")
    val (blastOutCputype,blastInCputype) =
	B.magic (blastOutCputype,blastInCputype,"cputype $Revision$")

    fun hostname () : string = get "nodename"

    (*
	OS.IO.poll (in contrast to Posix.Process.sleep) can be used for
	millisecond timeouts.
    *)
    fun sleep (duration : real) : unit =
	(OS.IO.poll([], SOME (Time.fromReal duration)); ())

    fun pid() =
	Posix.Process.pidToWord(Posix.ProcEnv.getpid())

end
