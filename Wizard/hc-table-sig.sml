(* Minimal hash table interface to support hash-cons'ing. *)
signature HASH_CONS_TABLE =
    sig

	structure Key : HASHABLE

	type 'a hc_table

	val make : int -> 'a hc_table

	val find_or_insert : 'a hc_table -> (Key.key * 'a) -> 'a

    end
