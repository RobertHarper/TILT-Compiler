(*$import MONO_VECTOR String Word8Vector *)

(* char-vector.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Vectors of characters (aka strings).
 *
 *)

structure CharVector :> MONO_VECTOR where type elem = char and type vector = string =
  struct

    type elem = char
    type vector = string

    val maxLen = String.maxSize

    val fromList = Word8Vector.fromList
    val length = Word8Vector.length
    val sub = Word8Vector.sub
    val tabulate = Word8Vector.tabulate

    val extract  = Word8Vector.extract
    val concat   = Word8Vector.concat


    val app = Word8Vector.app
    val map = Word8Vector.map
    val appi = Word8Vector.appi
    val mapi = Word8Vector.mapi

    val foldl = Word8Vector.foldl
    val foldr = Word8Vector.foldr
    val foldli = Word8Vector.foldli
    val foldri = Word8Vector.foldri


  end (* CharVector *)


(*
 * $Log$
# Revision 1.1  98/03/09  19:50:12  pscheng
# added basis
# 
 * Revision 1.5  1997/07/07  18:06:33  jhr
 *   Added extract function to String (moved the implementation from CharVector).
 *
 * Revision 1.4  1997/05/29  14:44:20  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.3  1997/03/19  20:08:48  george
 *   bugfix for 1108 -- String.maxSize missnamed as String.maxLen
 *
 * Revision 1.2  1997/02/11  15:15:38  george
 * got rid of structure rebinding, since inlining is now preserved
 *
 * Revision 1.1.1.1  1997/01/14  01:38:13  george
 *   Version 109.24
 *
 *)
