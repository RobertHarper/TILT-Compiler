(*$import TopLevel ORD_SET LibBase IntMap Word32 IntBinaryMap Array *)


(* =========================================================================
 * DenseIntSet.sml
 * ========================================================================= *)

structure DenseIntSet
	    :> ORD_SET
		 where type Key.ord_key = int
	    = struct

  (* -- structures --------------------------------------------------------- *)

  structure IntMap = IntBinaryMap
  structure WordN  = Word32

  structure Key = IntMap.Key

  (* -- types -------------------------------------------------------------- *)

  type item = int

  (*
   * A integer set is represented as a mapping from word indices to words long
   * with a count of the number of integers in the set.
   * Each word is a bitmap of the integers that are in the set over its index.
   * Only nonzero words are allowed in the word map.
   *)
  type set = WordN.word IntMap.map * int

  (* -- exceptions --------------------------------------------------------- *)

  exception NotFound = LibBase.NotFound

  exception Unimplemented

  (* -- word values -------------------------------------------------------- *)

  val wordSize = WordN.wordSize

  val zero = WordN.fromInt 0
  val one  = WordN.fromInt 1
  val one' = Word.fromInt 1

  val notb = WordN.notb

  val op<< = WordN.<<
  val op>> = WordN.>>

  val andb = WordN.andb
  val orb  = WordN.orb
  val xorb = WordN.xorb

  infix 5 << >>
  infix 4 andb orb xorb

  (* -- private functions -------------------------------------------------- *)

  fun IntMap_remove(map, int) =
	let
	  val (map', _) = IntMap.remove(map, int)
	in
	  map'
	end

  (*
   * Return the word index and mask of a given integer.
   * item -> the integer to get the word index and mask of
   * <- the word index of item
   * <- the word mask of item
   *)
  local
    val w5  = Word.fromInt 5
    val w6  = Word.fromInt 6
    val w31 = Word.fromInt 31
    val w63 = Word.fromInt 63

    fun split32 item =
	  let
	    val word = Word.fromInt item
	  in
	    (Word.toIntX(Word.~>>(word, w5)), one<<Word.andb(word, w31))
	  end

    fun split64 item =
	  let
	    val word = Word.fromInt item
	  in
	    (Word.toIntX(Word.~>>(word, w6)), one<<Word.andb(word, w63))
	  end

    fun splitN item = (item div wordSize, one<<Word.fromInt(item mod wordSize))
  in
    val split = case wordSize of
		  32 => split32
		| 64 => split64
		| _  => splitN
  end

  (*
   * Return the number of bits set in a given word.
   * word -> the word to count the on bits of
   * <- the number of one bits in word
   *)
  local
    fun countInt 0 = 0
      | countInt n = (n mod 2)+countInt(n div 2)

    val countInt8 = Array.tabulate(256, countInt)

    val w8   = Word.fromInt 8
    val w255 = WordN.fromInt 255
  in
    fun countWord(count, word) =
	  if word<>zero then
	    countWord(count+Array.sub(countInt8, WordN.toIntX(word andb w255)),
		      word>>w8)
	  else
	    count
  end

  (* -- values ------------------------------------------------------------- *)

  val empty = (IntMap.empty, 0)

  (* -- functions ---------------------------------------------------------- *)

  fun add(set as (map, count), item) =
	let
	  val (index, mask) = split item
	in
	  case IntMap.find(map, index) of
	    SOME word => if word andb mask<>zero then
			   set
			 else
			   (IntMap.insert(map, index, word orb mask), count+1)
	  | NONE      => (IntMap.insert(map, index, mask), count+1)
	end

  fun delete((map, count), item) =
	let
	  val (index, mask) = split item
	in
	  case IntMap.find(map, index) of
	    SOME word => if word andb mask<>zero then
			   let
			     val word' = word xorb mask
			   in
			     if word'<>zero then
			       (IntMap.insert(map, index, word'), count-1)
			     else
			       (IntMap_remove(map, index), count-1)
			   end
			 else
			   raise NotFound
	  | NONE      => raise NotFound
	end

  fun member((map, _), item) =
	let
	  val (index, mask) = split item
	in
	  case IntMap.find(map, index) of
	    SOME word => word andb mask<>zero
	  | NONE      => false
	end

  fun isEmpty(_, 0) = true
    | isEmpty _	    = false

  fun compare _ = raise Unimplemented

  fun isSubset((map1, count1), (map2, count2)) =
	let
	  fun isSubset1(index, word, flag) =
	    case IntMap.find(map2, index) of
	      SOME word' => flag andalso word andb word'=word
	    | NONE	 => false
	in
	  count1<=count2 andalso IntMap.foldli isSubset1 true map1
	end

  fun numItems(_, count) = count

  local
    fun union1(index, word, set as (map, count)) =
	  case IntMap.find(map, index) of
	    SOME word' => let
			    val word'' = word orb word'
			  in
			    if word''<>word then
			      (IntMap.insert(map, index, word''),
			       countWord(count, word'' xorb word'))
			    else
			      set
			  end
	  | NONE       => (IntMap.insert(map, index, word),
			   countWord(count, word))
  in
    fun union(set1, (_, 0)) =
	  set1
      | union((_, 0), set2) =
	  set2
      | union(set1 as (_, count1), set2 as (map2, count2)) =
	 if count1>=count2 then
	   IntMap.foldli union1 set1 map2
	 else
	   union(set2, set1)
  end

  fun intersection((_, 0), _) =
	empty
    | intersection(_, (_, 0)) =
	empty
    | intersection(set1 as (map1, count1), set2 as (map2, count2)) =
	if count1>=count2 then
	  let
	    fun intersection1(index, word, set as (map, count)) =
	      case IntMap.find(map1, index) of
		SOME word' => let
				val word'' = word andb word'
			      in
				if word''<>zero then
				  (IntMap.insert(map, index, word''),
				   countWord(count, word''))
				else
				  set
			      end
	      | NONE	   => set
	  in
	    IntMap.foldli intersection1 empty map2
	  end
	else
	  intersection(set2, set1)

  fun difference(set1, (_, 0)) =
	set1
    | difference((_, 0), set2) =
	empty
    | difference(set1 as (map1, count1), (map2, count2)) =
	let
	  fun remove1(index, word2, set as (map, count)) =
		case IntMap.find(map1, index) of
		  SOME word1 => let
				  val word = word1 andb notb word2
				in
				  if word<>zero then
				    if word<>word1 then
				      (IntMap.insert(map, index, word),
				       count-countWord(0, word xorb word1))
				    else
				      set
				  else
				    (IntMap_remove(map, index),
				     count-countWord(0, word1))
				end
		| NONE	     => set

	  fun add1(index, word1, set as (map, count)) =
		case IntMap.find(map2, index) of
		  SOME word2 => let
				  val word = word1 andb notb word2
				in
				  if word<>zero then
				    (IntMap.insert(map, index, word),
				     countWord(count, word))
				  else
				    set
				end
		| NONE	     => (IntMap.insert(map, index, word1),
				 countWord(count, word1))
	in
	  if count1>=count2 then
	    IntMap.foldli remove1 set1 map2
	  else
	    IntMap.foldli add1 empty map1
	end

  fun foldl f value (map, _) =
	let
	  fun foldlWord(item, word, value) =
		if word<>zero then
		  let
		    val value' = if word andb one<>zero then
				   f(item, value)
				 else
				   value
		  in
		    foldlWord(item+1, word>>one', value')
		  end
		else
		  value

	  fun foldl1(index, word, value) =
		foldlWord(index*wordSize, word, value)
	in
	  IntMap.foldli foldl1 value map
	end

  fun foldr f value (map, _) =
	let
	  fun foldrWord(item, word, value) =
		if word<>zero then
		  let
		    val value' = foldrWord(item+1, word>>one', value)
		  in
		    if word andb one<>zero then
		      f(item, value')
		    else
		      value'
		  end
		else
		  value

	  fun foldr1(index, word, value) =
		foldrWord(index*wordSize, word, value)
	in
	  IntMap.foldri foldr1 value map
	end

  (* -- layered functions -------------------------------------------------- *)

  fun singleton item = add(empty, item)

  fun add'(item, set) = add(set, item)

  fun addList(set, list) =
	List.foldl (fn(item, set) => add(set, item)) set list

  fun equal(set1, set2) =
	numItems set1=numItems set2 andalso
	isSubset(set1, set2) andalso
	isSubset(set2, set1)

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
	    | find1(item, NONE)		 = if f item then SOME item else NONE
	in
	  foldl find1 NONE set
	end

end

