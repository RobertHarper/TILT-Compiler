(*$import WORD Prelude PreWord StringCvt NumFormat NumScan *)
(* word32.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)


structure Word32 :> WORD where type word = word32 =
  struct

    type word = word32

    val wordSize = 32
    val wordSizeW = 0w32

    (* the X versions treat the word as signed *)
    fun toInt   (x : word) : int = if (ult(x,0wx7fffffff)) 
				       then uint32toint32 x else raise Overflow
    fun toIntX  (x : word) : int = uint32toint32 x
    fun fromInt (x : int) : word = int32touint32 x

    val toLargeInt   : word -> int = toInt
    val toLargeIntX  : word -> int = toIntX
    val fromLargeInt : int -> word = fromInt

    fun toLargeWord (x : word) : PreLargeWord.word = x
    fun toLargeWordX (x : word) : PreLargeWord.word = x
    fun fromLargeWord (x : PreLargeWord.word) : word = x

    val toi32 = uint32toint32



  (** These should be inline functions **)
    fun lshift (w : word, k) = if ulte(wordSizeW, k)
					 then 0w0
				     else (w << (toi32 k))
    fun rshiftl (w : word, k) = if ulte(wordSizeW, k)
					  then 0w0
				      else (w >> (toi32 k))
    fun rshifta (w : word, k) = int32touint32
				      (if ulte(wordSizeW, k)
					   then (toi32 w) ~>> 31
				       else (toi32 w) ~>> (toi32 k))

    nonfix << >> ~>> + - * div mod <= = >= < > || && ^^
    val << = lshift
    val >> = rshiftl
    val ~>> = rshifta

    val orb  : word * word -> word = ||
    val xorb  : word * word -> word = ^^
    val andb : word * word -> word = &&
    val notb : word -> word = !! 

    val +  : (word * word -> word) = uplus
    val -  : (word * word -> word) = uminus
    val *  : (word * word -> word) = umult
    val div  : (word * word -> word) = udiv
    val mod  : (word * word -> word) = umod


    fun compare (w1, w2) =
      if (ult(w1, w2)) then LESS
	 else if (ugt(w1, w2)) then GREATER
	      else EQUAL

    val >  : (word * word -> bool) = ugt
    val >=  : (word * word -> bool) = ugte
    val <  : (word * word -> bool) = ult
    val <=  : (word * word -> bool) = ulte

    fun min (w1, w2) = if <(w1,w2) then w1 else w2
    fun max (w1, w2) = if >(w1,w2) then w1 else w2

    fun fmt radix = (NumFormat.fmtWord radix)
    val toString = fmt StringCvt.HEX

    val scan = NumScan.scanWord
    val fromString = StringCvt.scanString (NumScan.scanWord StringCvt.HEX)

  end  (* structure Word32 *)

structure Word = Word32
structure LargeWord = Word32
structure SysWord = Word32
