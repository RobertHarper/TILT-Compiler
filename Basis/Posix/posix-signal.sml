(*$import Word32 SysInt SysWord POSIX_extern POSIX_SIGNAL *)
(* posix-signal.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX 1003.1 signals.
 *
 *)

structure POSIX_Signal :> POSIX_SIGNAL =
  struct

    datatype signal = SIG of SysInt.int

    fun toWord (SIG i) = SysWord.fromInt i
    fun fromWord w = SIG (SysWord.toInt w)

    fun osval str = Ccall(posix_signal_num,str)

    val abrt = SIG(osval "abrt")
    val alrm = SIG(osval "alrm")
    val fpe  = SIG(osval "fpe")
    val hup  = SIG(osval "hup")
    val ill  = SIG(osval "ill")
    val int  = SIG(osval "int")
    val kill = SIG(osval "kill")
    val pipe = SIG(osval "pipe")
    val quit = SIG(osval "quit")
    val segv = SIG(osval "segv")
    val term = SIG(osval "term")
    val usr1 = SIG(osval "usr1")
    val usr2 = SIG(osval "usr2")
    val chld = SIG(osval "chld")
    val cont = SIG(osval "cont")
    val stop = SIG(osval "stop")
    val tstp = SIG(osval "tstp")
    val ttin = SIG(osval "ttin")
    val ttou = SIG(osval "ttou")
    val bus  = SIG(osval "bus")

  end (* structure POSIX_Signal *)

(*
 * $Log$
# Revision 1.5  2001/12/13  16:31:30  swasey
# *** empty log message ***
# 
# Revision 1.4  2000/11/27  22:36:40  swasey
# *** empty log message ***
# 
 * Revision 1.3  2000/09/12 18:54:41  swasey
 * Changes for cutoff compilation
 *
 * Revision 1.2  1999/09/22 15:45:13  pscheng
 * *** empty log message ***
 *
# Revision 1.1  1998/03/09  19:53:38  pscheng
# added basis
#
 * Revision 1.1.1.1  1997/01/14  01:38:23  george
 *   Version 109.24
 *
 *)
