(*$import Prelude CharVector Char Word32 *)
(* hash-string.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *)

structure HashString : sig

    val hashString : string -> word

  end = struct

    fun charToWord c = Word.fromInt(Char.ord c)

  (* A function to hash a character.  The computation is:
   *
   *   h = 33 * h + 720 + c
   *)
    fun hashChar (c, h) = Word.<<(h, 0w5) + h + 0w720 + (charToWord c)

    fun hashString s = CharVector.foldl hashChar 0w0 s
	  
  end (* HashString *)
