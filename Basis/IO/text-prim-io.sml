(*$import PrimIOFn CharVector CharArray Int32 *)
(* text-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure TextPrimIO :> PRIM_IO where type array = Word8Array.array
				  and type vector = string
				  and type elem =  char
				  and type pos = Position.int
    = PrimIOFn (structure Vector = CharVector
		structure Array = CharArray
		val someElem = #"\000"
		type pos = Position.int
		val compare = Position.compare);
    

(*
 * $Log$
# Revision 1.1  98/03/09  19:50:53  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:19  george
 *   Version 109.24
 *
 *)
