(*$import Prelude Word8 Word8Vector Word8Array Substring *)
(* byte-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature BYTE =
  sig

    val byteToChar : Word8.word -> char
    val charToByte : char -> Word8.word

    val bytesToString : Word8Vector.vector -> string
    val stringToBytes : string -> Word8Vector.vector

    val unpackStringVec : Word8Vector.vector * int * int option -> string
    val unpackString    : Word8Array.array * int * int option -> string
    val packString      : Word8Array.array * int * Substring.substring -> unit

  end

(*
 * $Log$
# Revision 1.4  2001/12/13  16:31:22  swasey
# *** empty log message ***
# 
# Revision 1.3  2000/11/27  22:36:29  swasey
# *** empty log message ***
# 
 * Revision 1.2  2000/09/12 18:54:28  swasey
 * Changes for cutoff compilation
 *
# Revision 1.1  98/03/09  19:52:31  pscheng
# added basis
# 
 * Revision 1.2  1997/05/29  14:44:18  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:13  george
 *   Version 109.24
 *
 *)
