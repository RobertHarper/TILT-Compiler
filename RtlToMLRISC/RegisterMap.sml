(*$import TopLevel REGISTER_MAP IntHashTable *)


(* =========================================================================
 * RegisterMap.sml
 * ========================================================================= *)

structure RegisterMap
	    :> REGISTER_MAP
		 where type id = int
	    = struct

  (* -- types -------------------------------------------------------------- *)

  type id = int

  (*
   * A register map is represented as a reference to a hash table, along with
   * a default value function.
   * The tables might also be represented as arrays if we know that the range
   * of register ids is contiguous and we can guess the base id.
   *)
  type 'a map = 'a IntHashTable.hash_table ref * (id -> 'a)

  (* -- exceptions --------------------------------------------------------- *)

  exception Impossible

  (* -- functions ---------------------------------------------------------- *)

  fun table() = IntHashTable.mkTable(31, Impossible)

  fun map default = (ref(table()), default)

  fun lookup (ref table, default) id =
	case IntHashTable.find table id of
	  NONE =>
	    let
	      val value = default id
	    in
	      IntHashTable.insert table (id, value); value
	    end
	| SOME value =>
	    value

  fun test (ref table, _) id = IntHashTable.find table id

  fun insert (ref table, _) mapping = IntHashTable.insert table mapping

  fun reset(tableRef, _) = tableRef := table()

end

