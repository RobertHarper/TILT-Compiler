(*$import IO_SIG *)
(* io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure IO : IO =
  struct

    exception Io of {
	name : string,
	function : string,
	cause : exn
      }

    exception BlockingNotSupported
    exception NonblockingNotSupported
    exception RandomAccessNotSupported
    exception TerminatedStream
    exception ClosedStream

    datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF

  end


(*
 * $Log$
# Revision 1.1  98/03/09  19:50:43  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:18  george
 *   Version 109.24
 *
 *)
