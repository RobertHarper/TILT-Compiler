(*$import Prelude Word8 Word8Array Word8Vector Substring Array BYTE PreString *)
(* byte.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Byte : BYTE =
  struct
    fun chr (b : Word8.word) : char = b
    fun ord (c : char) : Word8.word = c
    fun vectorToString (v,pos,len) : string = Word8Vector.extract(v,pos,SOME len)
    fun arrayToString (a,pos,len) : string = Word8Array.extract(a,pos,SOME len)

    val byteToChar = chr
    val charToByte = ord

    fun bytesToString (cv : Word8Vector.vector) : string = cv
    fun stringToBytes (str : string) : Word8Vector.vector = str

    val unpackStringVec : (Word8Vector.vector * int * int option) -> string
	 = Word8Vector.extract
    val unpackString  : (Word8Array.array * int * int option) -> string
	 = Word8Array.extract


    fun packString (arr : char array, i : int, ss : Substring.substring) : unit = 
	let
	    val PreString.SS(src, srcStart, srcLen) = ss
	    val dstLen = Array.length arr
	    fun cpy (_, _, 0w0) = ()
	      | cpy (srcIndx, dstIndx, n) = 
		(unsafe_update (arr, dstIndx, unsafe_vsub(src, srcIndx));
		 cpy (uplus(srcIndx,0w1), uplus(dstIndx,0w1), uminus(n,0w1)))
	in
	    if (i < 0) orelse (i > dstLen-srcLen) then raise Subscript else ();
		cpy (int32touint32 srcStart, int32touint32 i, int32touint32 srcLen)
	end

  end


(*
 * $Log$
# Revision 1.2  2000/09/12  18:54:28  swasey
# Changes for cutoff compilation
# 
# Revision 1.1  98/03/09  19:52:32  pscheng
# added basis
# 
 * Revision 1.3  1997/05/29  14:44:19  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.2  1997/02/11  15:15:35  george
 * got rid of structure rebinding, since inlining is now preserved
 *
 * Revision 1.1.1.1  1997/01/14  01:38:13  george
 *   Version 109.24
 *
 *)
