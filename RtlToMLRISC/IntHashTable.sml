
(* =========================================================================
 * IntHashTable.sml
 * ========================================================================= *)

structure IntHashTable =
  HashTableFn(
    struct
      type hash_key = int
      val  hashVal  = Word.fromInt
      val  sameKey  = op=
    end
  )

