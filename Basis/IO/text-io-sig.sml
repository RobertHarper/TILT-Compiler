(*$import Firstlude TiltPrim Prelude PreString StringCvt TEXT_STREAM_IO *)
(* text-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature TEXT_IO =
sig

    structure StreamIO : TEXT_STREAM_IO

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

    val inputLine    : instream -> string
    val outputSubstr : outstream * PreString.substring -> unit

    val openIn     : string -> instream
    val openOut    : string -> outstream
    val openAppend : string -> outstream
    val openString : string -> instream

    val stdIn  : instream
    val stdOut : outstream
    val stdErr : outstream

    val print : string -> unit

    val scanStream : ((elem, StreamIO.instream) StringCvt.reader
		      -> ('a, StreamIO.instream) StringCvt.reader)
		     -> instream -> 'a option

  end;

