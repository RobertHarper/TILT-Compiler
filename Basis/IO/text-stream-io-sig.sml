(*$import Prelude Substring STREAM_IO *)
(* text-stream-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature TEXT_STREAM_IO =
  sig
    include STREAM_IO
    val inputLine    : instream -> (string * instream)
    val outputSubstr : (outstream * Substring.substring) -> unit
  end

(*
 * $Log$
# Revision 1.2  2000/09/12  18:54:18  swasey
# Changes for cutoff compilation
# 
# Revision 1.1  98/03/09  19:50:54  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:19  george
 *   Version 109.24
 *
 *)
