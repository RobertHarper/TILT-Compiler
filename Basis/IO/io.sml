(*$import Prelude IO_SIG *)
(* io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure IO :> IO =
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
# Revision 1.4  2001/12/13  16:31:19  swasey
# *** empty log message ***
# 
# Revision 1.3  2000/11/27  22:36:25  swasey
# *** empty log message ***
# 
 * Revision 1.2  2000/09/12 18:54:15  swasey
 * Changes for cutoff compilation
 *
# Revision 1.1  98/03/09  19:50:43  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:18  george
 *   Version 109.24
 *
 *)
