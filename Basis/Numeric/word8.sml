(*$import WORD Prelude General NumFormat NumScan *)
(* word8.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Word8 :> WORD where type word = word8 =
  struct
(*
    structure W8 = InlineT.Word8
    structure W31 = InlineT.Word31
    structure LW = Word32
*)
    type word = word8  (* 31 bits *)

    val wordMask = 0w255
    val wordSize = 8
    val wordSizeW = 0w8
    val wordShift = uminus(0w31, wordSizeW)
    val wordShifti = 31 - wordSize


    val toi32 = uint32toint32
    val tow32 = uint8touint32
    val tow8 = uint32touint8
    fun adapt arg = tow8(arg && 0wxFF)

    fun sextend (w8 : word8) : word32 = 
	let val w32 = uint8touint32 w8
	    val neg = (w32 && 0w128)
	    val mask = int32touint32(uint32toint32((neg << 24)) ~>> 23)
	in  w32 || mask
	end

    val toInt   : word -> int = uint8toint32
    fun toIntX  (x : word) : int = uint32toint32(sextend x)
    fun fromInt (x : int) : word = uint32touint8(wordMask && (int32touint32 x))

    val toLargeInt   : word -> int = toInt
    val toLargeIntX  : word -> int = toIntX
    val fromLargeInt : int -> word = fromInt

    val toLargeWord : word -> PreLargeWord.word = uint8touint32
    fun toLargeWordX (x : word) : PreLargeWord.word = sextend x
    fun fromLargeWord (x : PreLargeWord.word) : word = uint32touint8(x && wordMask)

(*
    val toInt   : word -> int = W8.toInt
    val toIntX  : word -> int = W8.toIntX
    val fromInt : int -> word = W8.fromInt

    val toLargeWord : word -> PreLargeWord.word = W8.toLargeWord
    val toLargeWordX = W8.toLargeWordX
    val fromLargeWord = W8.fromLargeWord

    val toLargeInt  : word -> LargeInt.int = LW.toLargeInt o toLargeWord
    val toLargeIntX : word -> LargeInt.int = W8.toLargeIntX
    val fromLargeInt: LargeInt.int -> word = W8.fromLargeInt
*)



  (** These should be inline functions **)
    fun lshift (w : word, k) = adapt(if ulte(wordSizeW, k)
					 then 0w0
				     else ((tow32 w) << (toi32 k)))
    fun rshiftl (w : word, k) = adapt(if ulte(wordSizeW, k)
					  then 0w0
				      else ((tow32 w) >> (toi32 k)))
    fun rshifta (w : word, k) = adapt(int32touint32
				      (if ulte(wordSizeW, k)
					   then toi32((tow32 w) << wordShifti) ~>> 31
				       else (toi32((tow32 w) << wordShifti)) ~>> 
					   (toi32 (uplus(wordShift, k)))))
    nonfix << >> ~>> + - * div mod <= = >= < >
    val << = lshift
    val >> = rshiftl
    val ~>> = rshifta

    val orb  : word * word -> word = orbyte
    val andb : word * word -> word = andbyte
    fun xorb (x : word, y : word) : word = adapt(^^(tow32 x, tow32 y))
    fun notb (x : word) : word = adapt (!! (tow32 x))

    fun + (x : word, y : word) : word = adapt(uplus(tow32 x, tow32 y))
    fun - (x : word, y : word) : word = adapt(uminus(tow32 x, tow32 y))
    fun * (x : word, y : word) : word = adapt(uplus(tow32 x, tow32 y))
    fun div (x : word, y : word) : word = tow8(udiv(tow32 x, tow32 y))
    fun mod (x : word, y : word) : word = tow8(umod(tow32 x, tow32 y))

    fun compare (w1, w2) =
      let val w1 = tow32 w1
	  val w2 = tow32 w2
      in if (ult(w1, w2)) then LESS
	 else if (ugt(w1, w2)) then GREATER
	      else EQUAL
      end

    fun > (x : word, y: word) = ugt(tow32 x, tow32 y)
    fun >= (x : word, y: word) = ugte(tow32 x, tow32 y)
    fun < (x : word, y: word) = ult(tow32 x, tow32 y)
    fun <= (x : word, y: word) = ulte(tow32 x, tow32 y)

    fun min (w1, w2) = if <(w1,w2) then w1 else w2
    fun max (w1, w2) = if >(w1,w2) then w1 else w2

    fun fmt radix = (NumFormat.fmtWord radix) o toLargeWord 
    val toString = fmt StringCvt.HEX

    fun scan radix = let
	  val scanLarge = NumScan.scanWord radix
	  fun scan getc cs = (case (scanLarge getc cs)
		 of NONE => NONE
		  | (SOME(w, cs')) => if ugt(w, 0w255)
		      then raise Overflow
		      else SOME(fromLargeWord w, cs')
		(* end case *))
	  in
	    scan
	  end
    val fromString = StringCvt.scanString (scan StringCvt.HEX)

  end  (* structure Word8 *)


(*
 * $Log$
# Revision 1.1  98/03/09  19:52:58  pscheng
# added basis
# 
 * Revision 1.2  1997/02/11  15:15:56  george
 * got rid of structure rebinding, since inlining is now preserved
 *
 * Revision 1.1.1.1  1997/01/14  01:38:18  george
 *   Version 109.24
 *
 *)
