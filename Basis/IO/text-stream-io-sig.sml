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

(*
 * $Log$
# Revision 1.4  2001/12/13  16:31:20  swasey
# *** empty log message ***
# 
# Revision 1.3  2000/11/27  22:36:26  swasey
# *** empty log message ***
# 
 * Revision 1.2  2000/09/12 18:54:18  swasey
 * Changes for cutoff compilation
 *
# Revision 1.1  98/03/09  19:50:54  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:19  george
 *   Version 109.24
 *
 *)
