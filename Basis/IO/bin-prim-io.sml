(*$import PrimIOFn Word8Vector Word8Array Word8 Position *)
(* bin-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)


structure BinPrimIO = PrimIOFn (structure A = Word8Array
				val someElem = (#"\000" : Word8.word)
				type pos = Position.int
				val compare = Position.compare)


