
signature HASH_KEY =
   sig
       type key
       type hashcode = word

       val hash : key -> hashcode
       val eq : key * key -> bool
   end

signature HASH_TABLE =
    sig
	structure Key : HASH_KEY

	type 'a hashtbl

	(* hash tables sizes should be primes not too close to powers of 2
	 * if the size - 2 is also prime, that's even better *)
	val make : int -> 'a hashtbl
	val find_or_insert : 'a hashtbl -> Key.key -> (unit -> 'a) -> 'a
    end
