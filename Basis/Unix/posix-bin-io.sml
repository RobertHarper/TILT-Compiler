(*$import BinIOFn PosixBinPrimIO BinPrimIO TextPrimIO *)

(* bin-io.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * The implementation of the BinIO stack on Posix systems.
 *
 *)

structure BinIO = BinIOFn (structure OSPrimIO = PosixBinPrimIO);

(*
 * $Log$
# Revision 1.3  2000/11/27  22:36:44  swasey
# *** empty log message ***
# 
 * Revision 1.2  1998/03/21 23:59:43  pscheng
 * remove debugging bindings
 *
# Revision 1.1  1998/03/09  19:54:24  pscheng
# added basis
#
 * Revision 1.3  1997/07/15  15:54:07  dbm
 *   Removed redundant where type definition.
 *
 * Revision 1.2  1997/01/31  20:39:50  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
