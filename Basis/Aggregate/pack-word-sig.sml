(*$import Prelude Word8Vector Word8Array Word32 *)
(* pack-word-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature PACK_WORD =
  sig

    val bytesPerElem : int

    val isBigEndian : bool

    val subVec : (Word8Vector.vector * int) -> Word32.word
    val subVecX : (Word8Vector.vector * int) -> Word32.word

    val subArr : (Word8Array.array * int) -> Word32.word
    val subArrX : (Word8Array.array * int) -> Word32.word

    val update : (Word8Array.array * int * Word32.word) -> unit

  end;

