signature HPPA_MILLICODE = sig
  structure I : HPPAINSTR

  val divu : {rs:int, rt:int, rd:int} -> I.instruction list
  val mulo : {rs:int, rt:int, rd:int} -> I.instruction list
  val divo : {rs:int, rt:int, rd:int} -> I.instruction list
  val mulu : {rs:int, rt:int, rd:int} -> I.instruction list
  val cvti2d : {rs:int, fd:int} -> I.instruction list
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:16:28  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:08  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1999/01/04 21:56:28  george
 *   Version 110.12
 *
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
