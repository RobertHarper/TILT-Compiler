(* Hash cons table built using SML/NJ hash tables.  Should re-implement,
   making each entry weak, and providing a find_or_insert operation.  We
   should make the data items weak here.  We should also try ternary trees as
   an alternative. *)

functor HCTableFn (structure Key : HASHABLE) (* : HASH_CONS_TABLE *) =
    struct

	structure Key = Key

	exception Impossible

	structure HashKey : HASH_KEY =
	    struct
		type hash_key = Key.key
		val hashVal = Key.hash
		val sameKey = Key.eq
	    end

	structure HashTable : MONO_HASH_TABLE = HashTableFn (HashKey)

	type 'a hc_table = 'a HashTable.hash_table

	fun make n = HashTable.mkTable (n, Impossible)

	fun find_or_insert t (k, v) =
	    case HashTable.find t k
	      of SOME v => v
	       | NONE => (HashTable.insert t (k, v); v)

    end
