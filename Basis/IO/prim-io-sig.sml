(*$import Prelude PreOS *)
(* prim-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature PRIM_IO =
  sig
    type array
    type vector
    type elem
    eqtype pos

    val compare : pos * pos -> order

    datatype reader = RD of {
	name      : string, 
	chunkSize : int,
	readVec   : (int -> vector) option,
        readArr   : ({buf : array, i : int, sz : int option} -> int) option,
	readVecNB : (int -> vector option) option,
	readArrNB : ({buf : array, i : int, sz : int option} -> int option) option,
	block     : (unit -> unit) option,
	canInput  : (unit -> bool) option,
	avail     : unit -> int option,
	getPos    : (unit -> pos) option,
	setPos    : (pos -> unit) option,
        endPos    : (unit -> pos) option,
	verifyPos : (unit -> pos) option,
	close     : unit -> unit,
	ioDesc    : PreOS.IO.iodesc option
      }

    datatype writer = WR of {
	name       : string,
	chunkSize  : int,
	writeVec   : ({buf : vector, i : int, sz : int option} -> int) option,
	writeArr   : ({buf : array, i : int, sz : int option} -> int) option,
	writeVecNB : ({buf : vector, i : int, sz : int option} -> int option) option,
	writeArrNB : ({buf : array, i : int, sz : int option} -> int option) option,
	block      : (unit -> unit) option,
	canOutput  : (unit -> bool) option,
	getPos     : (unit -> pos) option,
	setPos     : (pos -> unit) option,
        endPos     : (unit -> pos) option,
	verifyPos  : (unit -> pos) option,
	close      : unit -> unit,
	ioDesc     : PreOS.IO.iodesc option
      }

    val augmentReader : reader -> reader
    val augmentWriter : writer -> writer

  end


(*
 * $Log$
# Revision 1.2  2000/11/27  22:36:26  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:50:46  pscheng
 * added basis
 *
 * Revision 1.1.1.1  1997/01/14  01:38:19  george
 *   Version 109.24
 *
 *)
