(*$import MAIN TopLevel Manager Stats Getopt *)

(* was $import MAIN Prelude List TextIO TopLevel Manager Stats Getopt *)

structure Main : MAIN =
struct

    val usage = "usage: tilt [-?vs] [-fr flag] [-cm mapfile] [-S [num/]host] [mapfile ...]\n"
    val version = "TILT version 0.1 (alpha4)\n"

    datatype cmd =
        Make of string list		(* [mapfile ...] *)
      | SetFlag of string		(* -f flag *)
      | ResetFlag of string		(* -r flag *)
      | Clean of string			(* -c mapfile *)
      | Master of string		(* -m mapfile *)
      | Slave				(* -s *)
      | Slaves of int * string		(* -S [num/]host *)
      | Slaves' of (int * string) list
      | PrintUsage			(* -? *)
      | PrintVersion			(* -v *)

    (* runCmd : cmd -> unit *)
    fun runCmd (Make mapfiles) = List.app Manager.make mapfiles
      | runCmd (SetFlag flag) = Stats.bool flag := true
      | runCmd (ResetFlag flag) = Stats.bool flag := false
      | runCmd (Clean mapfile) = Manager.purge mapfile
      | runCmd (Master mapfile) = Manager.master mapfile
      | runCmd (Slave) = Manager.slave ()
      | runCmd (Slaves arg) = Manager.slaves [arg]
      | runCmd (Slaves' arg) = Manager.slaves arg
      | runCmd (PrintUsage) = print usage
      | runCmd (PrintVersion) = print version

    (* mergeSlaves : cmd list -> cmd list. *)
    fun mergeSlaves cmds =
	let fun isSlaves (Slaves _) = true
	      | isSlaves _ = false
	    fun strip slaves = List.map (fn (Slaves arg) => arg) slaves
	    val (slaves, others) = List.partition isSlaves cmds
	in
	    if List.null slaves	then others
	    else (Slaves' (strip slaves)) :: others
	end
	
    (* run : cmd list -> unit *)
    fun run cmds = List.app runCmd (mergeSlaves cmds)

    (*** Scanning   [num/]host    Ug ***)
	
    (* intScan : int -> (char,'a) StringCvt.reader -> (int,'a) StringCvt.reader
     * (intScan n cs) is a reader for integers >= n.
     *)
    fun intScan min reader source =
	case Int.scan StringCvt.DEC reader source
	  of NONE => NONE
	   | SOME (n, source') => if n < min then NONE else SOME (n, source')
    fun natScan reader source = intScan 1 reader source
	
    (* stringScan : int -> (char,'a) StringCvt.reader -> (string,'a) StringCvt.reader
     * (stringScan n cs) is a reader for strings of length at least n.
     *)
    fun stringScan minLen reader source =
	let val (s, empty) = StringCvt.splitl (fn _ => true) reader source
	in  if size s < minLen then NONE
	    else SOME (s, empty)
	end
    fun nonEmpty reader source = stringScan 1 reader source

    (* numHostScan : (char,'a) StringCvt.reader -> (int*string,'a) StringCvt.reader
     * (numHostScan cs) is a reader for n/host pairs.
     *)
    fun numHostScan reader source =
	case natScan reader source
	  of NONE => NONE
	   | SOME (n, source') =>
	      case reader source'
		of SOME (#"/", source'') =>
		    (case nonEmpty reader source''
		       of NONE => NONE
			| SOME (s, empty) => SOME ((n, s), empty))
		 | _ => NONE

    (* all : (char -> bool) -> string -> bool *)
    fun all p s =
	let exception False
	    fun check c = if p c then ()
			  else raise False
	in
	    (CharVector.app check s; true)
	    handle False => false
	end

    (* hostScan : (char,'a) StringCvt.reader -> (int*string,'a) StringCvt.reader
     * (hostScan cs) is a reader for strings of the form n/host or host.
     *)
    fun hostScan reader source =
	case numHostScan reader source
	  of NONE =>
	      (case nonEmpty reader source
		 of NONE => NONE
		  | SOME (s, empty) => (* s must not contain / *)
		     let fun notslash #"/" = false
			   | notslash _ = true
		     in
			 if all notslash s
			     then SOME ((1, s), empty)
			 else NONE
			 handle Slash => NONE
		     end)
	   | r => r

    (* parseHost : string -> (int * string) option *)
    val parseHost = StringCvt.scanString hostScan

    (*** Command line processing ***)
	
    exception Error of string
	
    (* slavesArg : string -> int * string *)
    fun slavesArg arg =
	(case parseHost arg
	   of NONE => raise Error ("argument must have form [num/]host -- " ^ arg ^ "\n")
	    | SOME num_host => num_host)
	      
    (* cmdline : string list -> cmd list.  May raise Error *)
    fun cmdline args =
	let
	    val options = [Getopt.Arg   (#"f", SetFlag),
			   Getopt.Arg   (#"r", ResetFlag),
			   Getopt.Arg   (#"c", Clean),
			   Getopt.Arg   (#"m", Master),
			   Getopt.Noarg (#"s", Slave),
			   Getopt.Arg   (#"S", Slaves o slavesArg),
			   Getopt.Noarg (#"?", PrintUsage),
			   Getopt.Noarg (#"v", PrintVersion)]
	in
	    case Getopt.getopt (options, args)
	      of Getopt.Error msg => raise Error (msg ^ "\n" ^ usage)
	       | Getopt.Success (cmds, mapfiles) =>
		  let 
		      val _ = if List.null cmds andalso List.null mapfiles
				  then raise Error usage
			      else ()
		  in  cmds @ [Make mapfiles]
		  end
	end

    (* We could warn user if any commands follow Slave (which doesn't terminate) *)

    (* errorMsg : exn -> string *)
    fun errorMsg (Util.BUG msg) = "internal error: " ^ msg ^ "\n"
      | errorMsg (Error msg) = msg
      | errorMsg (e) = (exnMessage e) ^ "\n"

    (* main : string * string list -> OS.Process.status *)
    fun main (_, args) =
	((run (cmdline args); OS.Process.success)
	 handle e => (print "tilt: "; print (errorMsg e);
		      OS.Process.failure))
	
    (* main' : unit -> OS.Process.status.  The old interactive loop. *)
    fun main' () =
	let val _ = print "TILT Compiler.  Please type one of the following: \n"
	    val _ = print "(1) make <mapfile>\n"
	    val _ = print "(2) master <mapfile>\n"
	    val _ = print "(3) slave\n"
	    val _ = print "(4) set <flag>\n"
	    val _ = print "(5) clear <flag>\n"
	    val _ = TextIO.flushOut TextIO.stdOut
	    val line = TextIO.inputLine TextIO.stdIn
	in
	    if size line = 0
		then OS.Process.success
	    else
		let val line = String.substring(line, 0, (size line) - 1)  (* drop the return *)
		    val words = String.fields Char.isSpace line
		    val _ = (case words of 
				 ["make",mapfile] => Manager.make mapfile
			       | ["master",mapfile] => Manager.master mapfile
			       | ["slave"] => Manager.slave()
			       | ["set",flag] => Stats.bool(flag) := true
			       | ["clear",flag] => Stats.bool(flag) := false
			       | _ => print "Command not understood.\n\n")
		in
		    main'()
		end
	end
end
