(*$import Firstlude TiltPrim Prelude Word8 Word8Array Word8Vector Substring Array BYTE PreString *)
(* byte.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Byte :> BYTE =
  struct
    val int32touint32 = TiltPrim.int32touint32
	
    val unsafe_update = TiltPrim.unsafe_update
    val unsafe_vsub = TiltPrim.unsafe_vsub
	
    val uplus = TiltPrim.uplus
    val uminus = TiltPrim.uminus
	
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


