(* pack-word-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature PACK_WORD =
  sig

    val bytesPerElem : int

    val isBigEndian : bool

    val subVec : Word8Vector.vector * int -> LargeWord.word
    val subVecX : Word8Vector.vector * int -> LargeWord.word

    val subArr : Word8Array.array * int -> LargeWord.word
    val subArrX : Word8Array.array * int -> LargeWord.word

    val update : Word8Array.array * int * LargeWord.word -> unit

  end;

