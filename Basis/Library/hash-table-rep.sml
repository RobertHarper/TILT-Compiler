(* hash-table-rep.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This is the internal representation of hash tables, along with some
 * utility functions.  It is used in both the polymorphic and functor
 * hash table implementations.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

structure HashTableRep : sig

    datatype ('a, 'b) bucket
      = NIL
      | B of (word * 'a * 'b * ('a, 'b) bucket)

    type ('a, 'b) table = ('a, 'b) bucket array

    val alloc : int -> ('a, 'b) table
	(* allocate a table of at least the given size *)

    val growTable : (('a, 'b) table * int) -> ('a, 'b) table
	(* grow a table to the specified size *)

    val growTableIfNeeded : (('a, 'b) table ref * int) -> bool
	(* conditionally grow a table; the second argument is the number
	 * of items currently in the table.
	 *)

    val listItems  : (('a, 'b) table * int ref) -> 'b list
    val listItemsi : (('a, 'b) table * int ref) -> ('a * 'b) list


    val appi : ('a * 'b -> 'c) -> ('a, 'b) table -> unit
    val app : ('a -> 'b) -> ('c, 'a) table -> unit

    val mapi : ('a * 'b -> 'c) -> ('a, 'b) table -> ('a, 'c) table
    val map : ('a -> 'b) -> ('c, 'a) table -> ('c, 'b) table

    val foldi : ('a * 'b * 'c -> 'c) -> 'c -> ('a, 'b) table -> 'c
    val fold : ('a * 'b -> 'b) -> 'b -> ('c, 'a) table -> 'b

    val filteri : ('a * 'b -> bool) -> ('a, 'b) table -> unit
    val filter : ('a -> bool) -> ('b,'a) table -> unit

    val copy : ('a, 'b) table -> ('a, 'b) table

    val bucketSizes : ('a, 'b) table -> int list

  end = struct

    datatype ('a, 'b) bucket
      = NIL
      | B of (word * 'a * 'b * ('a, 'b) bucket)

    type ('a, 'b) table = ('a, 'b) bucket array

    fun index (i, sz) = Word.toIntX(Word.andb(i, Word.fromInt sz - 0w1))

  (* find smallest power of 2 (>= 32) that is >= n *)
    fun roundUp n = let
	  fun f i = if (i >= n) then i else f(i * 2)
	  in
	    f 32
	  end

  (* Create a new table; the int is a size hint and the exception
   * is to be raised by find.
   *)
    fun alloc sizeHint = Array.array(roundUp sizeHint, NIL)

  (* grow a table to the specified size *)
    fun growTable (table, newSz) = let
	  val newArr = Array.array (newSz, NIL)
	  fun copy NIL = ()
	    | copy (B(h, key, v, rest)) = let
		val indx = index (h, newSz)
		in
		  Array.update (newArr, indx,
		    B(h, key, v, Array.sub(newArr, indx)));
		  copy rest
		end
	  in
	    Array.app copy table;
	    newArr
	  end

  (* conditionally grow a table; return true if it grew. *)
    fun growTableIfNeeded (table, nItems) = let
	    val arr = !table
	    val sz = Array.length arr
	    in
	      if (nItems >= sz)
		then (table := growTable (arr, sz+sz); true)
		else false
	    end

  (* return a list of the items in the table *)
    fun listItems (table, nItems) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, k, v, r), l, n) = g(r, v::l, n-1)
		in
		  g (Array.sub(table, i), l, n)
		end
	  in
	    f ((Array.length table) - 1, [], !nItems)
	  end (* listItems *)
    fun listItemsi (table, nItems) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, k, v, r), l, n) = g(r, (k, v)::l, n-1)
		in
		  g (Array.sub(table, i), l, n)
		end
	  in
	    f ((Array.length table) - 1, [], !nItems)
	  end (* listItems *)

  (* Apply a function to the entries of the table *)
    fun appi f table = let
	  fun appF NIL = ()
	    | appF (B(_, key, item, rest)) = (f (key, item); appF rest)
	  in
	    Array.app appF table
	  end (* appi *)
    fun app f table = let
	  fun appF NIL = ()
	    | appF (B(_, key, item, rest)) = (f item; appF rest)
	  in
	    Array.app appF table
	  end (* app *)

  (* Map a table to a new table that has the same keys *)
    fun mapi f table = let
	  fun mapF NIL = NIL
	    | mapF (B(hash, key, item, rest)) =
		B(hash, key, f (key, item), mapF rest)
	  val newTbl = Array.tabulate (
		Array.length table,
		fn i => mapF (Array.sub(table, i)))
	  in
	    newTbl
	  end (* transform *)

  (* Map a table to a new table that has the same keys *)
    fun map f table = let
	  fun mapF NIL = NIL
	    | mapF (B(hash, key, item, rest)) = B(hash, key, f item, mapF rest)
	  val newTbl = Array.tabulate (
		Array.length table,
		fn i => mapF (Array.sub(table, i)))
	  in
	    newTbl
	  end (* map *)

    fun foldi f init table = let
	  fun foldF (NIL, accum) = accum
	    | foldF (B(hash, key, item, rest), accum) =
		foldF(rest, f(key, item, accum))
	  in
	    Array.foldl foldF init table
	  end
    fun fold f init table = let
	  fun foldF (NIL, accum) = accum
	    | foldF (B(hash, key, item, rest), accum) =
		foldF(rest, f(item, accum))
	  in
	    Array.foldl foldF init table
	  end

  (* remove any hash table items that do not satisfy the given
   * predicate.
   *)
    fun filteri pred table = let
	  fun filterP NIL = NIL
	    | filterP (B(hash, key, item, rest)) = if (pred(key, item))
		then B(hash, key, item, filterP rest)
		else filterP rest
	  in
	    Array.modify filterP table
	  end (* filteri *)
    fun filter pred table = let
	  fun filterP NIL = NIL
	    | filterP (B(hash, key, item, rest)) = if (pred item)
		then B(hash, key, item, filterP rest)
		else filterP rest
	  in
	    Array.modify filterP table
	  end (* filter *)

  (* Create a copy of a hash table *)
    fun copy table =
	  Array.tabulate (Array.length table, fn i => Array.sub(table, i));

  (* returns a list of the sizes of the various buckets.  This is to
   * allow users to gauge the quality of their hashing function.
   *)
    fun bucketSizes table = let
	  fun len (NIL, n) = n
	    | len (B(_, _, _, r), n) = len(r, n+1)
	  in
	    Array.foldr (fn (b, l) => len(b, 0) :: l) [] table
	  end

  end (* HashTableRep *)
