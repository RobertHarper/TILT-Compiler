(*$import TopLevel HashTableFn HashString *)

(* =========================================================================
 * StringHashTable.sml
 * ========================================================================= *)

structure StringHashTable =
  HashTableFn(
    struct
      type hash_key = string
      val  hashVal  = HashString.hashString
      val  sameKey  = op=
    end
  )

