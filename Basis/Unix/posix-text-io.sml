(*$import TextIOFn TEXT_IO PosixTextPrimIO TextPrimIO *)
(* text-io.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * The implementation of the TextIO stack on Posix systems.
 *
 *)

structure TextIO = TextIOFn (structure OSPrimIO = PosixTextPrimIO);


(*
 * $Log$
# Revision 1.2  2000/11/27  22:36:45  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:54:29  pscheng
 * added basis
 *
 * Revision 1.2  1997/01/31  20:39:51  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
