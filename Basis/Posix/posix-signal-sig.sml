(*$import Word32 *)
(* posix-signal-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Signature for POSIX 1003.1 signals.
 *
 *)

signature POSIX_SIGNAL =
  sig
    eqtype signal

    val toWord   : signal -> SysWord.word
    val fromWord : SysWord.word -> signal

    val abrt : signal
    val alrm : signal
    val fpe  : signal
    val hup  : signal
    val ill  : signal
    val int  : signal
    val kill : signal
    val pipe : signal
    val quit : signal
    val segv : signal
    val term : signal
    val usr1 : signal
    val usr2 : signal
    val chld : signal
    val cont : signal
    val stop : signal
    val tstp : signal
    val ttin : signal
    val ttou : signal
    val bus  : signal

  end (* signature POSIX_SIGNAL *)

(*
 * $Log$
# Revision 1.1  98/03/09  19:53:36  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:23  george
 *   Version 109.24
 *
 *)
