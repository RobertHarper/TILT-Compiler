(*$import TextIOFn TEXT_IO PosixTextPrimIO TextPrimIO *)
(* text-io.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * The implementation of the TextIO stack on Posix systems.
 *
 *)

structure TextIO :> TEXT_IO
    where type StreamIO.reader = TextPrimIO.reader
    where type StreamIO.writer = TextPrimIO.writer
    where type StreamIO.pos = TextPrimIO.pos
  = TextIOFn (structure OSPrimIO = PosixTextPrimIO);


(*
 * $Log$
# Revision 1.1  98/03/09  19:54:29  pscheng
# added basis
# 
 * Revision 1.2  1997/01/31  20:39:51  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
