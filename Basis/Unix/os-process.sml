(* os-process.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The Posix-based implementation of the generic process control
 * interface (OS.Process).
 *
 *)

structure OS_Process :> OS_PROCESS =
  struct

    structure P_Proc = Posix.Process

    type status = PreOS.Process.status (* int *)

    val success = 0
    val failure = 1

    fun system cmd = (case P_Proc.fork()
	   of NONE => (
		P_Proc.exec ("/bin/sh", ["sh", "-c", cmd])
		P_Proc.exit 0w127)
	    | (SOME pid) => let
(*  xxxxxxxxxx
		fun savSig s = Signals.setHandler (s, Signals.IGNORE)
		val savSigInt = savSig UnixSignals.sigINT
		val savSigQuit = savSig UnixSignals.sigQUIT
		fun restore () = (
		      Signals.setHandler (UnixSignals.sigINT, savSigInt);
		      Signals.setHandler (UnixSignals.sigQUIT, savSigQuit))
*)
		fun restore() = ()
		fun wait () = (case #2(P_Proc.waitpid(P_Proc.W_CHILD pid, []))
		       of P_Proc.W_EXITED => success
			| (P_Proc.W_EXITSTATUS w) => Word8.toInt w
			| (P_Proc.W_SIGNALED s) => failure (* ?? *)
			| (P_Proc.W_STOPPED s) => failure (* this shouldn't happen *)
		      (* end case *))
		in
		  (wait() before restore())
		    handle ex => (restore(); raise ex)
		end
	  (* end case *))

    val atExit : (unit -> unit) -> unit =
	ignore o TiltCleanUp.atExit

    fun terminate x = P_Proc.exit(Word8.fromInt x)
    fun exit sts = (TiltCleanUp.exitting(); terminate sts)

    val getEnv = Posix.ProcEnv.getenv

  end
