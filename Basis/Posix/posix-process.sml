(*$import Prelude List Int POSIX_Signal Word32 Time Word8 POSIX_extern POSIX_PROCESS *)
(* posix-process.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX 1003.1 process submodule
 *
 *)

structure POSIX_Process :> POSIX_PROCESS where type signal = POSIX_Signal.signal =
  struct

    structure Sig = POSIX_Signal

    val ++ = SysWord.orb
    val & = SysWord.andb
    infix ++ &

    type word = SysWord.word
    type s_int = SysInt.int

    type signal = Sig.signal
    datatype pid = PID of s_int
    fun pidToWord (PID i) = SysWord.fromInt i
    fun wordToPid w = PID (SysWord.toInt w)
    
    fun osval (s : string) : s_int = Ccall(posix_process_num,s)
    val w_osval = SysWord.fromInt o osval

    fun sysconf (s : string) : SysWord.word = Ccall(posix_process_sysconf,s)

    fun fork () =
          case Ccall(posix_process_fork,()) of
            0 => NONE
          | child_pid => SOME(PID child_pid)
    
    fun exec (x: string, y : string list) : 'a = (Ccall(posix_process_exec, x, y);
						  raise LibFail "exec cannot return")
    fun exece (x: string, y : string list, z : string list) : 'a = (Ccall(posix_process_exece, x, y, z);
								    raise LibFail "exece cannot return")
    fun execp (x: string, y : string list) : 'a = (Ccall(posix_process_execp, x, y);
						  raise LibFail "execp cannot return")

    datatype waitpid_arg
      = W_ANY_CHILD 
      | W_CHILD of pid 
      | W_SAME_GROUP
      | W_GROUP of pid
    
    datatype killpid_arg
      = K_PROC of pid
      | K_SAME_GROUP
      | K_GROUP of pid

    datatype exit_status
      = W_EXITED
      | W_EXITSTATUS of Word8.word
      | W_SIGNALED of signal
      | W_STOPPED of signal
    
      (* (pid',status,status_val) = waitpid' (pid,options)  *)
    fun waitpid' (pid : s_int, opts : word) : s_int * s_int * s_int = Ccall(posix_process_waitpid, pid, opts)

    fun argToInt W_ANY_CHILD = ~1
      | argToInt (W_CHILD (PID pid)) = pid
      | argToInt (W_SAME_GROUP) = 0
      | argToInt (W_GROUP (PID pid)) = ~pid

      (* The exit status from wait is encoded as a pair of integers.
       * If the first integer is 0, the child exited normally, and
       * the second integer gives its exit value.
       * If the first integer is 1, the child exited due to an uncaught
       * signal, and the second integer gives the signal value.
       * Otherwise, the child is stopped and the second integer 
       * gives the signal value that caused the child to stop.
       *)
    fun mkExitStatus (0,0) = W_EXITED
      | mkExitStatus (0,v) = W_EXITSTATUS(Word8.fromInt v)
      | mkExitStatus (1,s) = W_SIGNALED (Sig.fromWord(SysWord.fromInt s))
      | mkExitStatus (_,s) = W_STOPPED (Sig.fromWord(SysWord.fromInt s))


    val wnohang = w_osval "WNOHANG"
    structure W =
      struct
        datatype flags = WF of word

        fun wordTo w = WF w
        fun toWord (WF w) = w

        fun flags ms = WF(List.foldl (fn (WF m,acc) => m ++ acc) 0w0 ms)
        fun anySet (WF m, WF m') = (m & m') <> 0w0
        fun allSet (WF m, WF m') = (m & m') = m

        fun orF (WF f,acc) = f ++ acc

        val untraced =
          WF(sysconf "JOB_CONTROL"; w_osval "WUNTRACED") handle _ => WF 0w0
      end

    fun waitpid (arg,flags) = let
          val (pid,status,sv) = waitpid'(argToInt arg, List.foldl W.orF 0w0 flags)
          in
            (PID pid, mkExitStatus(status,sv))
          end

    fun waitpid_nh (arg,flags) =
          case waitpid'(argToInt arg, List.foldl W.orF wnohang flags) of
            (0,_,_) => NONE
          | (pid,status,sv) => SOME(PID pid, mkExitStatus(status,sv))

    fun wait () = waitpid(W_ANY_CHILD,[])
    
    fun exit (x: Word8.word) : 'a = (Ccall(posix_process_exit, x);
				     raise LibFail "execp cannot return")
    
    fun kill' (x : s_int, y : s_int) : unit = Ccall(posix_process_kill, x, y)
    fun kill (K_PROC (PID pid), s) = kill'(pid, SysWord.toInt(Sig.toWord s))
      | kill (K_SAME_GROUP, s) = kill'(~1, SysWord.toInt(Sig.toWord s))
      | kill (K_GROUP (PID pid), s) = kill'(~pid, SysWord.toInt(Sig.toWord s))
    
    fun alarm' (x : int) : int = Ccall(posix_process_alarm, x)
    val alarm = Time.fromSeconds o alarm' o Time.toSeconds

    fun pause () : unit = Ccall(posix_process_pause,())

    fun sleep' (x : int) : int = Ccall(posix_process_sleep, x)
    val sleep = Time.fromSeconds o sleep' o Time.toSeconds

  end (* structure POSIX_Process *)

(*
 * $Log$
# Revision 1.3  2000/09/12  18:54:40  swasey
# Changes for cutoff compilation
# 
 * Revision 1.2  1999/09/22 15:45:13  pscheng
 * *** empty log message ***
 *
# Revision 1.1  1998/03/09  19:53:34  pscheng
# added basis
#
 * Revision 1.1.1.1  1997/01/14  01:38:23  george
 *   Version 109.24
 *
 *)
