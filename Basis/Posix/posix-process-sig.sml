(*$import SysWord Prelude POSIX_FLAGS Word8 Time *)
(* posix-process-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Signature for POSIX 1003.1 process submodule
 *
 *)

signature POSIX_PROCESS =
  sig
    eqtype signal
    eqtype pid

    val wordToPid     : SysWord.word -> pid
    val pidToWord     : pid -> SysWord.word

    val fork : unit -> pid option
    
    val exec  : string * string list -> 'a
    val exece : string * string list * string list -> 'a
    val execp : string * string list -> 'a
    
    datatype waitpid_arg
      = W_ANY_CHILD
      | W_CHILD of pid
      | W_SAME_GROUP
      | W_GROUP of pid
    
    datatype exit_status
      = W_EXITED
      | W_EXITSTATUS of Word8.word
      | W_SIGNALED of signal
      | W_STOPPED of signal
    
    structure W :
      sig
        include POSIX_FLAGS

        val untraced : flags
      end

    val wait : unit -> pid * exit_status
    val waitpid : waitpid_arg * W.flags list -> pid * exit_status
    val waitpid_nh : waitpid_arg * W.flags list -> (pid * exit_status) option
    
    val exit : Word8.word -> 'a
    
    datatype killpid_arg
      = K_PROC of pid
      | K_SAME_GROUP
      | K_GROUP of pid

    val kill : killpid_arg * signal -> unit
    
    val alarm : Time.time -> Time.time
    val pause : unit -> unit
    val sleep : Time.time -> Time.time

  end (* signature POSIX_PROCESS *)

(*
 * $Log$
# Revision 1.2  2000/11/27  22:36:40  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:53:33  pscheng
 * added basis
 *
 * Revision 1.1.1.1  1997/01/14  01:38:23  george
 *   Version 109.24
 *
 *)
