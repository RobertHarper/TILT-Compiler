(*$import TopLevel IREGISTER_MAP intBinaryMap Array Word32 *)


(* =========================================================================
 * DenseRegisterMap.sml
 * ========================================================================= *)

structure DenseRegisterMap
	    :> REGISTER_MAP
		 where type id = int
	    = struct

  (* -- structures --------------------------------------------------------- *)

  structure IntMap = IntBinaryMap

  (* -- types -------------------------------------------------------------- *)

  type id = int

  (*
   * A register map is represented as a mapping from register id indices to
   * value blocks and a default value function.
   * A value block is an arrary of values for a contiguous sequence of
   * register ids.
   *)
  type 'a map = 'a option Array.array IntMap.map ref * (id -> 'a)

  (* -- values ------------------------------------------------------------- *)

  (*
   * The size of a value block.
   *)
  val blockSize = 32

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return the block index and offset of a given register id.
   * id -> the id to get the block index and offset of
   * <- the block index of id
   * <- the block offset of id
   *)
  local
    val w3  = Word.fromInt 3
    val w4  = Word.fromInt 4
    val w5  = Word.fromInt 5
    val w6  = Word.fromInt 6
    val w7  = Word.fromInt 7
    val w15 = Word.fromInt 15
    val w31 = Word.fromInt 31
    val w63 = Word.fromInt 63

    fun split8 id =
	  let
	    val word = Word.fromInt id
	  in
	    (Word.toIntX(Word.>>(word, w3)), Word.toIntX(Word.andb(word, w7)))
	  end

    fun split16 id =
	  let
	    val word = Word.fromInt id
	  in
	    (Word.toIntX(Word.>>(word, w4)), Word.toIntX(Word.andb(word, w15)))
	  end

    fun split32 id =
	  let
	    val word = Word.fromInt id
	  in
	    (Word.toIntX(Word.>>(word, w5)), Word.toIntX(Word.andb(word, w31)))
	  end

    fun split64 id =
	  let
	    val word = Word.fromInt id
	  in
	    (Word.toIntX(Word.>>(word, w6)), Word.toIntX(Word.andb(word, w63)))
	  end

    fun splitN id = (id div blockSize, id mod blockSize)
  in
    val split = case blockSize of
		  8  => split8
		| 16 => split16
		| 32 => split32
		| 64 => split64
		| _  => splitN
  end

  (*
   * Return a block map with a new value block for a given index containing
   * a given value at a given offset.
   * map    -> the block map to add the block to
   * index  -> the index of the block to add
   * offset -> the offset of the initial value
   * value  -> the initial value
   * <- the new block map
   *)
  fun insertBlock(map, index, offset, value) =
	let
	  val block = Array.array(blockSize, NONE)
	in
	  Array.update(block, offset, SOME value);
	  IntMap.insert(map, index, block)
	end

  fun map default = (ref IntMap.empty, default)

  fun lookup (mapRef as ref map, default) id =
	let
	  val (index, offset) = split id
	in
	  case IntMap.find(map, index) of
	    SOME block =>
	      (case Array.sub(block, offset) of
		 SOME value => value
	       | NONE	    => let
				 val value = default id
			       in
				 Array.update(block, offset, SOME value);
				 value
			       end)
	  | NONE =>
	      let
		val value = default id
	      in
		mapRef := insertBlock(map, index, offset, value);
		value
	      end
	end

  fun test (ref map, _) id =
	let
	  val (index, offset) = split id
	in
	  case IntMap.find(map, index) of
	    SOME block => Array.sub(block, offset)
	  | NONE       => NONE
	end

  fun insert (mapRef as ref map, _) (id, value) =
	let
	  val (index, offset) = split id
	in
	  case IntMap.find(map, index) of
	    SOME block => Array.update(block, offset, SOME value)
	  | NONE       => mapRef := insertBlock(map, index, offset, value)
	end

  fun reset(mapRef, _) = mapRef := IntMap.empty

end

