
(* =========================================================================
 * DenseRegisterMap.sml
 * ========================================================================= *)

functor DenseRegisterMap(
	  structure IntMap: ORD_MAP where type Key.ord_key = int
	) :> REGISTER_MAP
	       where type id = int
	  = struct

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
   * use shift+mask to speed this up ???
   *)
  fun split id = (id div blockSize, id mod blockSize)

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

