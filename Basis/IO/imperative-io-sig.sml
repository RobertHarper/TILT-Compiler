(*$import Prelude STREAM_IO *)
(* imperative-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature IMPERATIVE_IO =
  sig
    structure StreamIO : STREAM_IO

    type vector = StreamIO.vector
    type elem = StreamIO.elem

    type instream
    type outstream

    val input    : instream -> vector
    val input1   : instream -> elem option
    val inputN   : instream * int -> vector
    val inputAll : instream -> vector
    val canInput : instream * int -> int option
	
    val lookahead : instream -> elem option
    val closeIn : instream -> unit
    val endOfStream : instream -> bool

    val output   : outstream * vector -> unit
    val output1  : outstream * elem -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit

    val getPosIn    : instream -> StreamIO.in_pos
    val setPosIn    : instream * StreamIO.in_pos -> unit
    val mkInstream  : StreamIO.instream -> instream
    val getInstream : instream -> StreamIO.instream
    val setInstream : instream * StreamIO.instream -> unit

    val getPosOut    : outstream -> StreamIO.out_pos
    val setPosOut    : outstream * StreamIO.out_pos -> unit
    val mkOutstream  : StreamIO.outstream -> outstream
    val getOutstream : outstream -> StreamIO.outstream
    val setOutstream : outstream * StreamIO.outstream -> unit

  end;

(*
 * $Log$
# Revision 1.3  2001/12/13  16:31:19  swasey
# *** empty log message ***
# 
# Revision 1.2  2000/11/27  22:36:25  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:50:41  pscheng
 * added basis
 *
 * Revision 1.1.1.1  1997/01/14  01:38:18  george
 *   Version 109.24
 *
 *)
