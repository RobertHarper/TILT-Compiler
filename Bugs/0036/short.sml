(*$import Prelude *)

structure Format : sig

    val wordToHex : word -> int * char list

  end = struct

    fun mkDigit (w : word) = TiltPrim.unsafe_vsub("0123456789abcdef", w)

    fun wordToOct w = let
	  fun f1 (w, n, l) = if TiltPrim.ult(w, 0w8)
		then (TiltPrim.iplus(n, 1), (mkDigit w) :: l)
		else f1(TiltPrim.>>(w, 3),
			TiltPrim.iplus(n : int, 1),
			mkDigit(TiltPrim.&&(w, 0wx7)) :: l)
	  in
	    f1 (w, 0, [])
	  end
    fun wordToDec w = let
	  fun f2 (w, n, l) = if TiltPrim.ult(w,0w10)
		then (TiltPrim.iplus(n, 1), (mkDigit w) :: l)
		else let val j = TiltPrim.udiv(w, 0w10)
		  in
		    f2 (j,
			TiltPrim.iplus(n, 1),
			mkDigit(TiltPrim.uminus(w,TiltPrim.umult(0w10,j))) :: l)
		  end
	  in
	    f2 (w, 0, [])
	  end
    fun wordToHex w = let
	  fun f3 (w, n, l) = if TiltPrim.ult(w, 0w16)
		then (TiltPrim.iplus(n, 1), (mkDigit w) :: l)
		else f3(TiltPrim.>>(w, 4),
			TiltPrim.iplus(n, 1),
			mkDigit(TiltPrim.&&(w, 0wxf)) :: l)
	  in
	    f3 (w, 0, [])
	  end

    fun fmtW 8 = wordToOct
      | fmtW 10 = wordToDec
      | fmtW 16 = wordToHex

    fun fmtWord radix = fmtW radix
  end;
val _ = Format.wordToHex 0wxFFFFFFFF
