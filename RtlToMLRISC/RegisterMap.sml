(* =========================================================================
 * RegisterMap.sml
 * ========================================================================= *)

functor RegisterMap(
          structure HashTable: MONO_HASH_TABLE
        ) :> REGISTER_MAP
	       where type id = HashTable.Key.hash_key
	  = struct

  (* -- types -------------------------------------------------------------- *)

  type id = HashTable.Key.hash_key

  (*
   * A register map is represented as a reference to a hash table, along with
   * a default value function.
   * The tables might also be represented as arrays if we know that the range
   * of register ids is contiguous and we can guess the base id.
   *)
  type 'a map = 'a HashTable.hash_table ref * (id -> 'a)

  (* -- exceptions --------------------------------------------------------- *)

  exception Impossible

  (* -- functions ---------------------------------------------------------- *)

  fun table() = HashTable.mkTable(31, Impossible)

  fun map default = (ref(table()), default)

  fun lookup (ref table, default) id =
	case HashTable.find table id of
	  NONE =>
	    let
	      val value = default id
	    in
	      HashTable.insert table (id, value); value
	    end
	| SOME value =>
	    value

  fun test (ref table, _) id = HashTable.find table id

  fun insert (ref table, _) mapping = HashTable.insert table mapping

  fun reset(tableRef, _) = tableRef := table()

end

