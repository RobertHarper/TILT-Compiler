(*$import Prelude *)
(* listsort-sig.sml
 *
 * COPYRIGHT (c) 1989 by AT&T Bell Laboratories
 *
 * The generic list sorting interface.  Taken from the SML/NJ compiler.
 *)

signature LIST_SORT =
  sig

     val sort : ('a * 'a -> bool) -> 'a list -> 'a list  
	(* (sort gt l) sorts the list l in ascending order using the
	 * ``greater-than'' relationship defined by gt.
	 *)

     val uniqueSort : ('a * 'a -> order) -> 'a list -> 'a list
       (* uniquesort produces an increasing list, removing equal 
        * elements
        *)

     val sorted : ('a * 'a -> bool) -> 'a list -> bool  
	(* (sorted gt l) returns true if the list is sorted in ascending
	 * order under the ``greater-than'' predicate gt.
	 *)

  end; (* LIST_SORT *)
