(* int-inf-sig.sml
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This package is derived from Andrzej Filinski's bignum package.
 *
 *)

signature INT_INF =
  sig
    include INTEGER

    val divmod  : (int * int) -> (int * int)
    val quotrem : (int * int) -> (int * int)
    val pow : (int * Int.int) -> int
    val log2 : int -> Int.int

  end (* signature INT_INF *)


(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:22  swasey
# *** empty log message ***
# 
# Revision 1.1  98/03/09  19:52:35  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)
