(*$import Prelude TopLevel BACKGROUND Posix Word8 SysWord UtilError TextIO Option Int OS Util Stats *)

structure Background :> BACKGROUND =
struct

    val error = fn s => Util.error "background.sml" s

    val noFork = Stats.ff "NoFork"
	
    open Posix.Process

    (* There is a bug either in SML/NJ's runtime or in the digital
     * unix 4.0D implementation of waitpid().  The runtime sometimes
     * raises an exception because it doesn't understand the status
     * value returned by waitpid().
     *)
    fun my_waitpid_nh arg =
	(waitpid_nh arg handle OS.SysErr ("unknown child status", _) =>
	    (print "Warning: caught and ignored an unknown child status\n";
	     NONE))
	    
    structure Signal = Posix.Signal

    val zero = Word8.fromInt 0
    val one = Word8.fromInt 1

    val statusToString : Word8.word -> string =
	Int.toString o Word8.toInt
	
    val pidToString : pid -> string =
	Int.toString o SysWord.toInt o pidToWord
	
    val signalToString : Signal.signal -> string =
	Int.toString o SysWord.toInt o Signal.toWord

    (* printError : exn -> unit *)
    fun printError (UtilError.BUG msg) = print ("tilt: " ^ msg ^ "\n")
      | printError e = print ("tilt: " ^ exnMessage e ^ "\n")

    (* background : (unit -> unit) -> unit -> bool *)
    fun background f =
	let
	    (* Prevent both processes writing same (buffered) text. *)
	    val _ = (TextIO.flushOut (TextIO.stdOut);
		     TextIO.flushOut (TextIO.stdErr))
	in
	    case fork()
	      of NONE =>		(* child process *)
		  let val status = (f(); zero) handle e => (printError e; one)
		  in  exit status
		  end
		| SOME pid =>		(* parent process *)
		  let
		      local
			  val exit_status : exit_status option ref
			      = ref NONE
		      in
			  fun get_status () =
			      if isSome (!exit_status) then !exit_status
			      else (exit_status := Option.map #2 (my_waitpid_nh (W_CHILD pid, []));
				    !exit_status)
		      end
		  
		      fun isdone () =
			  case get_status()
			    of NONE => false
			     | SOME W_EXITED => true
			     | SOME (W_EXITSTATUS s) => if s = zero then true
							else error ("child " ^ pidToString pid ^
								    " exitted with status " ^ statusToString s)
			     | SOME (W_SIGNALED s) => error ("child " ^ pidToString pid ^
							     " killed with signal " ^ signalToString s)
			     | SOME (W_STOPPED _) => error ("W_STOPPED without WUNTRACED")
		  in
		      isdone
		  end
	end
    
    (* Handle noFork *)
    val background = (fn f =>
		      if (!noFork) then
			  (f(); fn () => true)
		      else background f)
	
end
