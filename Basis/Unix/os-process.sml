(*$import OS_PROCESS Posix CleanUp *)
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
    structure CU = CleanUp

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

    local
      val hooks = ref ([] : (unit -> unit) list)
      val _ = CU.addCleaner (
	    "OS.Process",
	    [CU.AtExit],
	    fn _ => List.app (fn f => (f ()) handle _ => ()) (! hooks))
    in
    fun atExit hook = hooks := hook :: !hooks
    end

    fun terminate x = P_Proc.exit(Word8.fromInt x)
    fun exit sts = (CU.clean CU.AtExit; terminate sts)

    val getEnv = Posix.ProcEnv.getenv

  end

(*
 * $Log$
# Revision 1.1  98/03/09  19:54:20  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
