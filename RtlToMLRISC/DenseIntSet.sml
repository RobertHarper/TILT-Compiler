
(* =========================================================================
 * DenseIntSet.sml
 * ========================================================================= *)

functor DenseIntSet(
	  structure IntMap: ORD_MAP where type Key.ord_key = int
          structure WordN:  WORD
	) :> ORD_SET
               where type Key.ord_key = int
	  = struct

  (* -- structures --------------------------------------------------------- *)

  structure Key = IntMap.Key

  (* -- types -------------------------------------------------------------- *)

  type item = int

  (*
   * A integer set is represented as a mapping from word indices to words.
   * Each word is a bitmap of the integers that are in the set over its index.
   * Only nonzero words are allowed in the word map.
   *)
  type set = WordN.word IntMap.map

  (* -- exceptions --------------------------------------------------------- *)

  exception NotFound = LibBase.NotFound

  exception Unimplemented

  (* -- word values -------------------------------------------------------- *)

  val wordSize = WordN.wordSize

  val zero = WordN.fromInt 0
  val one  = WordN.fromInt 1

  val notb = WordN.notb

  fun op<<(word, n) = WordN.<<(word, Word.fromInt n)
  fun op>>(word, n) = WordN.>>(word, Word.fromInt n)

  val andb = WordN.andb
  val orb  = WordN.orb
  val xorb = WordN.xorb

  infix 5 << >>
  infix 4 andb orb xorb

  (* -- values ------------------------------------------------------------- *)

  val empty = IntMap.empty

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return the word index and mask of a given integer.
   * item -> the integer to get the word index and mask of
   * <- the word index of item
   * <- the word mask of item
   * use shift+mask to speed this up (need to get signs right) ???
   *)
  fun split item = (item div wordSize, one<<item mod wordSize)

  fun add(map, item) =
	let
	  val (index, mask) = split item
	in
	  case IntMap.find(map, index) of
	    SOME word => IntMap.insert(map, index, word orb mask)
	  | NONE      => IntMap.insert(map, index, mask)
	end

  fun delete(map, item) =
	let
	  val (index, mask) = split item
	in
	  case IntMap.find(map, index) of
	    SOME word => if word andb mask<>zero then
	                   let
			     val word' = word xorb mask
			   in
			     if word'<>zero then
			       IntMap.insert(map, index, word')
			     else
			       let
				 val (map', _) = IntMap.remove(map, index)
			       in
				 map'
			       end
			   end
			 else
			   raise NotFound
	  | NONE      => raise NotFound
	end

  fun member(map, item) =
	let
	  val (index, mask) = split item
	in
	  case IntMap.find(map, index) of
	    SOME word => word andb mask<>zero
	  | NONE      => false
	end

  fun isEmpty map = IntMap.numItems map=0

  fun compare(map1, map2) = raise Unimplemented

  fun isSubset(map1, map2) =
        let
	  fun isSubset1(index, word, flag) =
	    case IntMap.find(map2, index) of
	      SOME word' => flag andalso word andb word'=word
	    | NONE       => false
	in
	  IntMap.foldli isSubset1 true map1
	end

  local
    fun union1(index, word, map) =
          case IntMap.find(map, index) of
	    SOME word' => IntMap.insert(map, index, word orb word')
	  | NONE       => IntMap.insert(map, index, word)
  in
    fun union(map1, map2) = IntMap.foldli union1 map2 map1
  end

  fun intersection(map1, map2) =
        let
	  fun intersection1(index, word, map) =
	    case IntMap.find(map2, index) of
	      SOME word' => let
			      val word'' = word andb word'
			    in
			      if word''<>zero then
				IntMap.insert(map, index, word'')
			      else
				map
			    end
	    | NONE       => map
	in
	  IntMap.foldli intersection1 IntMap.empty map1
	end

  fun difference(map1, map2) =
        let
	  fun difference1(index, word, map) =
	    case IntMap.find(map2, index) of
	      SOME word' => let
			      val word'' = word andb notb word'
			    in
			      if word''<>zero then
				IntMap.insert(map, index, word'')
			      else
				map
			    end
	    | NONE       => IntMap.insert(map, index, word)
	in
	  IntMap.foldli difference1 IntMap.empty map1
	end

  fun foldl f value map =
        let
	  fun foldlWord value (item, word) =
	        if word<>zero then
		  let
		    val value' = if word andb one<>zero then
		                   f(item, value)
				 else
				   value
		  in
		    foldlWord value' (item+1, word>>1)
		  end
		else
		  value

	  fun foldl1(index, word, value) =
	        foldlWord value (index*wordSize, word)
	in
	  IntMap.foldli foldl1 value map
	end

  fun foldr f value map =
        let
	  fun foldrWord value (item, word) =
	        if word<>zero then
		  let
		    val value' = foldrWord value (item+1, word>>1)
		  in
		    if word andb one<>zero then
		      f(item, value')
		    else
		      value'
		  end
		else
		  value

	  fun foldr1(index, word, value) =
	        foldrWord value (index*wordSize, word)
	in
	  IntMap.foldri foldr1 value map
	end

  (* -- layered functions -------------------------------------------------- *)

  fun singleton item = add(empty, item)

  fun addList(set, list) =
        List.foldl (fn(item, set) => add(set, item)) set list

  fun equal(map1, map2) = isSubset(map1, map2) andalso isSubset(map2, map1)

  fun numItems set = foldl (fn(_, count) => count+1) 0 set

  fun listItems set = foldr op:: [] set

  fun map f set = foldl (fn(item, set) => add(set, f item)) empty set

  fun app f set = foldr (fn(item, ()) => f item) () set

  fun filter f set =
	foldl (fn(item, set) => if f item then add(set, item) else set)
	      empty
	      set

  fun exists f set = foldl (fn(item, flag) => flag orelse f item) false set

  fun find f set =
        let
	  fun find1(_, result as SOME _) = result
	    | find1(item, NONE)          = if f item then SOME item else NONE
	in
	  foldl find1 NONE set
	end

end

