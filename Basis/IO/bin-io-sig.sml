(*$import IMPERATIVE_IO Word8Vector Word8 Int Prelude *)
(* bin-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature BIN_IO =
  sig
    include IMPERATIVE_IO

    val openIn     : string -> instream
    val openOut    : string -> outstream
    val openAppend : string -> outstream
  end
  where 
(* type vector = Word8Vector.vector redundant and wrong*)
      type StreamIO.vector = Word8Vector.vector
  and type StreamIO.elem = Word8.word
  and type StreamIO.pos = Position.int;

(*
 * $Log$
# Revision 1.2  99/09/22  15:45:00  pscheng
# *** empty log message ***
# 
# Revision 1.1  1998/03/09  19:50:38  pscheng
# added basis
#
 * Revision 1.2  1997/05/20  12:12:43  dbm
 *   SML '97 sharing, where structure.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:18  george
 *   Version 109.24
 *
 *)
