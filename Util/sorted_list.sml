functor SListFn (structure Key : ORD_KEY) :> SLIST where type key = Key.ord_key =
  struct
    structure Key = Key

    val compare = Key.compare
    type key = Key.ord_key

    type 'a slist = (key*'a) list

    fun gt ((key1,_),(key2,_)) =
      case compare (key1,key2)
	of GREATER => true
	 | _ => false

    fun add list (elt as (key,value)) =
      let
	fun loop ([],acc) = List.revAppend (acc,[elt])
	  | loop (list as ((fst as (key',_))::rest),acc) =
	  (case compare (key,key')
	     of GREATER => loop (rest,fst::acc)
	      | LESS => List.revAppend (acc,elt::list)
	      | EQUAL => List.revAppend(acc,elt::list))
      in
	loop (list,[])
      end

    fun find list key =
      let
	fun loop [] = NONE
	  | loop ((fst as (key',value))::rest) =
	  (case compare (key,key')
	     of GREATER => loop rest
	      | LESS => NONE
	      | EQUAL => SOME value)
      in
	loop list
      end

    datatype merge = LEFT | RIGHT | BOTH

    fun shuffle merge (l1,l2) =
      let
	fun shuffle' (([],any,acc) | (any,[],acc)) = any@acc
	  | shuffle' (cmap1 as ((key1,value1)::rest1),cmap2 as ((key2,value2)::rest2),acc) =
	  (case compare (key1,key2)
	     of LESS => shuffle' (rest1,cmap2,(key1,value1)::acc)
	      | GREATER => shuffle' (cmap1,rest2,(key2,value2)::acc)
	      | EQUAL =>
	       let
		 val (left,right,acc) =
		   case merge
		     of LEFT => (cmap1,rest2,acc)
		      | RIGHT => (rest1,cmap2,acc)
		      | BOTH => (rest1,rest2,(key1,value1)::(key2,value2)::acc)
	       in
		 shuffle' (left,right,acc)
	       end)
      in
	shuffle' (l1,l2,[])
      end

    fun merge lists = shuffle BOTH lists
    fun merge_left lists = shuffle LEFT lists
    fun merge_right lists = shuffle RIGHT lists

    val map : ('a -> 'b) -> 'a slist -> 'b slist = Listops.map_second

    fun empty () = []

    fun fromList (l1 : (key * 'a) list) : 'a slist = ListMergeSort.sort gt l1

    fun toList (l : 'a slist) : (key * 'a) list = l

    fun null [] = true
      | null _ = false
  end
