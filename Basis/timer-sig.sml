(*$import Prelude Time *)

(* timer-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature TIMER =
  sig

    type cpu_timer
    type real_timer

    val startCPUTimer : unit -> cpu_timer
    val checkCPUTimer : cpu_timer -> {
	    usr : Time.time, sys : Time.time, gc : Time.time
	  }
    val totalCPUTimer : unit -> cpu_timer

    val startRealTimer : unit -> real_timer
    val checkRealTimer : real_timer -> Time.time
    val totalRealTimer : unit -> real_timer

  end (* TIMER *)


(*
 * $Log$
# Revision 1.2  2000/11/27  22:36:19  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 15:45:47  pscheng
 * adding the basis
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
