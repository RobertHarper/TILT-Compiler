(*$import Firstlude TiltPrim Prelude IMPERATIVE_IO Word8Vector Word8 *)
(* bin-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature BIN_IO =
  sig
    include IMPERATIVE_IO
      where type StreamIO.vector = Word8Vector.vector
        and type StreamIO.elem = Word8.word

    val openIn     : string -> instream
    val openOut    : string -> outstream
    val openAppend : string -> outstream
  end

