(* text-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure TextPrimIO = PrimIOFn (structure A = CharArray
				 val someElem = #"\000"
				 type pos = Position.int
				 val compare = Position.compare);


