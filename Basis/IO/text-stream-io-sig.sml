(*$import Prelude Substring STREAM_IO *)
(* text-stream-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature TEXT_STREAM_IO =
  sig
    include STREAM_IO
      where type vector = string
        and type elem = char
    val inputLine    : instream -> string * instream
    val outputSubstr : outstream * Substring.substring -> unit
  end

