(*$import PrimIOFn Word8Vector Word8Array Word8 Int *)
(* bin-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)


structure BinPrimIO = PrimIOFn (
    structure Vector = Word8Vector
    structure Array = Word8Array
    val someElem = (#"\000" : Word8.word)
    type pos = Position.int
    val compare = Position.compare);


(*
 * $Log$
# Revision 1.2  99/09/22  15:45:00  pscheng
# *** empty log message ***
# 
# Revision 1.1  1998/03/09  19:50:39  pscheng
# added basis
#
 * Revision 1.1.1.1  1997/01/14  01:38:18  george
 *   Version 109.24
 *
 *)
