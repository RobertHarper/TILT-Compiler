(*$import BinIOFn PosixBinPrimIO BinPrimIO TextPrimIO *)

(* bin-io.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * The implementation of the BinIO stack on Posix systems.
 *
 *)

val x = "abcde" : BinPrimIO.vector
val x = "abcde" : TextPrimIO.vector
val x = "abcde" : PosixBinPrimIO.PrimIO.vector


structure BinIO 
    :> BIN_IO  
    where type StreamIO.reader = BinPrimIO.reader
    where type StreamIO.writer = BinPrimIO.writer
(*    where type StreamIO.pos = BinPrimIO.pos  - redundant *)
  = BinIOFn (structure OSPrimIO = PosixBinPrimIO);



(*
 * $Log$
# Revision 1.1  98/03/09  19:54:24  pscheng
# added basis
# 
 * Revision 1.3  1997/07/15  15:54:07  dbm
 *   Removed redundant where type definition.
 *
 * Revision 1.2  1997/01/31  20:39:50  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
