(*$import Prelude PRIM_IO *)
(* os-prim-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is an interface to a PrimIO structure augmented with OS specific
 * functions to create readers and writers.
 *
 *)

signature OS_PRIM_IO =
  sig
    structure PrimIO : PRIM_IO

    type file_desc

    val openRd  : string -> PrimIO.reader
    val openWr  : string -> PrimIO.writer
    val openApp : string -> PrimIO.writer

    val mkReader : {
	    fd : file_desc,
	    name : string,
  	    initBlkMode : bool
	  } -> PrimIO.reader
    val mkWriter: {
	    fd : file_desc,
	    name : string,
	    appendMode : bool,
	    initBlkMode : bool, 
	    chunkSize : int
	  } -> PrimIO.writer

  end


(*
 * $Log$
# Revision 1.3  2001/12/13  16:31:19  swasey
# *** empty log message ***
# 
# Revision 1.2  2000/09/12  18:54:16  swasey
# Changes for cutoff compilation
# 
# Revision 1.1  98/03/09  19:50:45  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:19  george
 *   Version 109.24
 *
 *)
