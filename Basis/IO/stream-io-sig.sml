(*$import Prelude IO *)
(* stream-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature STREAM_IO =
  sig
    type elem
    type vector
    type reader
    type writer

    type instream
    type outstream

    type in_pos
    type out_pos
    type pos

    val input       : instream -> vector * instream
    val input1      : instream -> (elem * instream) option
    val inputN      : instream * int -> vector * instream
    val inputAll    : instream -> vector * instream
    val canInput    : instream * int -> int option
    val closeIn     : instream -> unit
    val endOfStream : instream -> bool
    val mkInstream  : reader * vector option -> instream
    val getReader   : instream -> reader * vector
    val getPosIn    : instream -> in_pos
    val setPosIn    : in_pos -> instream
    val filePosIn   : in_pos -> pos

    val output        : outstream * vector -> unit
    val output1       : outstream * elem -> unit
    val flushOut      : outstream -> unit
    val closeOut      : outstream -> unit
    val setBufferMode : outstream * IO.buffer_mode -> unit
    val getBufferMode : outstream -> IO.buffer_mode
    val mkOutstream   : writer * IO.buffer_mode -> outstream
    val getWriter     : outstream -> writer * IO.buffer_mode
    val getPosOut     : outstream -> out_pos
    val setPosOut     : out_pos -> unit
    val filePosOut    : out_pos -> pos

  end

(*
 * $Log$
# Revision 1.3  2001/12/13  16:31:19  swasey
# *** empty log message ***
# 
# Revision 1.2  2000/11/27  22:36:26  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:50:47  pscheng
 * added basis
 *
 * Revision 1.1.1.1  1997/01/14  01:38:19  george
 *   Version 109.24
 *
 *)
