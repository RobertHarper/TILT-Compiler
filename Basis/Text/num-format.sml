(*$import Prelude StringCvt PreString General *)
(* num-format.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The word to string conversion for the largest word and int types.
 * All of the other fmt functions can be implemented in terms of them.
 *
 *)

structure NumFormat : sig

    val fmtWord : StringCvt.radix -> word -> string
    val fmtInt  : StringCvt.radix -> int -> string

  end = struct

    val int32touint32 = TiltPrim.int32touint32
    val uint32toint32 = TiltPrim.uint32toint32

    val && = TiltPrim.&&
    val >> = TiltPrim.>>

    val udiv = TiltPrim.udiv
    val umult = TiltPrim.umult
    val uminus = TiltPrim.uminus

    val ult = TiltPrim.ult

    val unsafe_vsub = TiltPrim.unsafe_vsub
(*
    structure W = InlineT.Word32
    structure I = InlineT.Int31
    structure I32 = InlineT.Int32

    val op < = W.<
    val op - = W.-
    val op * = W.*
    val op div = W.div
*)

    val plus = (op +) : int * int -> int
    val less = (op <) : int * int -> bool
    val negate = ~ : int -> int

    val op < = ult
    val op - = uminus
    val op * = umult
    val op div = udiv
    val w2i = uint32toint32
    val i2w = int32touint32


    fun mkDigit (w : word) = unsafe_vsub("0123456789abcdef", w)

    fun wordToBin w = let
	  fun mkBit w = if (&&(w, 0w1) = 0w0) then #"0" else #"1"
	  fun f (0w0, n, l) = (plus(n : int, 1), #"0" :: l)
	    | f (0w1, n, l) = (plus(n : int, 1), #"1" :: l)
	    | f (w, n, l) = f(>>(w, 1), plus(n, 1), (mkBit w) :: l)
	  in
	    f (w, 0, [])
	  end
    fun wordToOct w = let
	  fun f (w, n, l) = if (w < 0w8)
		then (plus(n, 1), (mkDigit w) :: l)
		else f(>>(w, 3), plus(n : int, 1), mkDigit(&&(w, 0wx7)) :: l)
	  in
	    f (w, 0, [])
	  end
    fun wordToDec w = let
	  fun f (w, n, l) = if (w < 0w10)
		then (plus(n, 1), (mkDigit w) :: l)
		else let val j = w div 0w10
		  in
		    f (j,  plus(n, 1), mkDigit(w - 0w10*j) :: l)
		  end
	  in
	    f (w, 0, [])
	  end
    fun wordToHex w = let
	  fun f (w, n, l) = if (w < 0w16)
		then (plus(n, 1), (mkDigit w) :: l)
		else f(>>(w, 4), plus(n, 1), mkDigit(&&(w, 0wxf)) :: l)
	  in
	    f (w, 0, [])
	  end

    fun fmtW StringCvt.BIN = wordToBin
      | fmtW StringCvt.OCT = wordToOct
      | fmtW StringCvt.DEC = wordToDec
      | fmtW StringCvt.HEX = wordToHex

    fun fmtWord radix = PreString.implode o (fmtW radix)

    fun fmtInt radix i = 
      if i2w i = 0wx80000000 then "~2147483648"
      else let
	  val w32 = i2w(if less(i, 0) then negate(i) else i)
          val (n, digits) = fmtW radix w32
	in
	  if less(i, 0) then PreString.implode(plus(n,1), #"~"::digits)
	  else PreString.implode(n, digits)
	end
  end;

