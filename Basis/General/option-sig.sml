(*$import Prelude *)
(* option-sig.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *)

signature OPTION =
  sig

    datatype 'a option = NONE | SOME of 'a
    exception Option

    val getOpt         : 'a option * 'a -> 'a
    val isSome         : 'a option -> bool
    val valOf          : 'a option -> 'a
    val filter         : ('a -> bool) -> 'a -> 'a option
    val join           : 'a option option -> 'a option
    val map            : ('a -> 'b) -> 'a option -> 'b option
    val mapPartial     : ('a -> 'b option) -> 'a option -> 'b option
    val compose        : ('a -> 'b) * ('c -> 'a option) -> 'c -> 'b option
    val composePartial : ('a -> 'b option) * ('c -> 'a option) -> 'c -> 'b option

  end;

