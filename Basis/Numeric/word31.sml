(* Word31.word is kept abstract.  It would be bad if
 *    val x : Word31.word = 0wxFFFFFFFF;
 * could typecheck.
 *)

structure Word31 :> WORD =
struct

    structure W = Word32

    type word = W.word                  (* invariant: bit 31 is 0 *)

    val wordSize = 31

    fun bit30 (w : word) = W.andb (0wx40000000, w)
    fun bit31 (w : word) = W.andb (0wx80000000, w)

    fun extend w = W.orb(w, W.<<(bit30 w, 0w1))
    fun drop w = W.andb(0wx7fffffff, w)

    fun trapOverflow (w : word) = if bit31 w = 0w0 then w
				  else raise Overflow

    val toLargeWord = W.toLargeWord
    val toLargeWordX = W.toLargeWordX o extend
    val fromLargeWord = drop o W.fromLargeWord

    val toLargeInt = W.toLargeInt
    val toLargeIntX = W.toLargeIntX o extend
    val fromLargeInt = drop o W.fromLargeInt

    val toInt = W.toInt
    val toIntX = W.toInt o extend
    val fromInt = drop o W.fromInt

    val orb = W.orb
    val xorb = W.xorb
    val andb = W.andb
    val notb = drop o W.notb

    val << = drop o W.<<
    val >> = W.>>
    fun ~>> (i, n) = drop (W.~>> (extend i, n))

    val op+ = drop o W.+
    val op- = drop o W.-
    val op* = drop o W.*
    val op div = W.div
    val op mod = W.mod

    val compare = W.compare
    val op>  = W.>
    val op<  = W.<
    val op>= = W.>=
    val op<= = W.<=

    val max = W.max
    val min = W.min

    val fmt = W.fmt
    val toString = W.toString

    fun fromString s = (case W.fromString s
			  of NONE => NONE
			   | SOME r => SOME (trapOverflow r))

    fun scan radix getc src = (case W.scan radix getc src
				 of NONE => NONE
				  | SOME (r, rest) => SOME (trapOverflow r, rest))
end
