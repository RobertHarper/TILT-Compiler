(*$import Prelude PRIM_IO PrimIOFn Position CharVector CharArray *)
(* text-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure TextPrimIO = PrimIOFn (structure A = CharArray
				 val someElem = #"\000"
				 type pos = Position.int
				 val compare = Position.compare);
    

(*
 * $Log$
# Revision 1.4  2000/11/27  22:36:26  swasey
# *** empty log message ***
# 
 * Revision 1.3  2000/09/12 18:54:17  swasey
 * Changes for cutoff compilation
 *
 * Revision 1.2  1999/09/22 15:45:01  pscheng
 * *** empty log message ***
 *
# Revision 1.1  1998/03/09  19:50:53  pscheng
# added basis
#
 * Revision 1.1.1.1  1997/01/14  01:38:19  george
 *   Version 109.24
 *
 *)
