(*$import PRIM_IO PosixPrimIOFn BinPrimIO OS_PRIM_IO *)
(* posix-bin-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This implements the UNIX version of the OS specific binary primitive
 * IO structure.  The Text IO version is implemented by a trivial translation
 * of these operations (see posix-text-prim-io.sml).
 *
 *)

structure PosixBinPrimIO : OS_PRIM_IO = PosixPrimIO(structure PrimIO = BinPrimIO)
