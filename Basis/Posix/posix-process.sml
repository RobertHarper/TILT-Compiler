(* posix-process.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX 1003.1 process submodule
 *
 *)

structure POSIX_Process :> POSIX_PROCESS where type signal = POSIX_Signal.signal =
struct

    val osval : string -> int = ccall1 posix_process_num
    val posix_process_fork : unit -> int = ccall0 posix_process_fork
    val posix_process_exec : string * string list -> int = fn (c,args) => Ccall(posix_process_exec,c,args)
    val posix_process_exece : string * string list * string list -> int = fn (c,args,es) => Ccall(posix_process_exece,c,args,es)
    val posix_process_execp : string * string list -> int = fn (c,args) => Ccall(posix_process_execp,c,args)
    val waitpid' : int * word -> waitpidrep = ccall2 posix_process_waitpid
    val posix_process_exit : TiltPrim.uint8 -> int = fn v => Ccall(posix_process_exit,v)
    val kill' : int * int -> unit = ccall2 posix_process_kill
    val alarm' : int -> int = fn t => Ccall(posix_process_alarm,t)
    val pause : unit -> unit = fn () => Ccall(posix_process_pause,())
    val sleep' : int -> int = fn t => Ccall(posix_process_sleep,t)

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

    val w_osval = SysWord.fromInt o osval

    fun fork () =
          case posix_process_fork() of
            0 => NONE
          | child_pid => SOME(PID child_pid)

    fun syserr (e:int) : 'a =
	let val msg = Ccall(syserror_msg,e)
	    val exn = TiltExn.SysErr(msg, SOME e)
	in  raise exn
	end

    fun exec (arg : string * string list) : 'a =
	syserr (posix_process_exec arg)
    fun exece (arg : string * string list * string list) : 'a =
	syserr (posix_process_exece arg)
    fun execp (arg : string * string list) : 'a =
	syserr (posix_process_execp arg)

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
          WF(PrePosix.sysconf "JOB_CONTROL"; w_osval "WUNTRACED") handle _ => WF 0w0
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

    fun exit (x: Word8.word) : 'a = syserr(posix_process_exit x)

    fun kill (K_PROC (PID pid), s) = kill'(pid, SysWord.toInt(Sig.toWord s))
      | kill (K_SAME_GROUP, s) = kill'(~1, SysWord.toInt(Sig.toWord s))
      | kill (K_GROUP (PID pid), s) = kill'(~pid, SysWord.toInt(Sig.toWord s))

    val alarm = Time.fromSeconds o alarm' o Time.toSeconds

    val sleep = Time.fromSeconds o sleep' o Time.toSeconds

end
