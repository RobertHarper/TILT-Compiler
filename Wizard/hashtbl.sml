
functor WeakHashTableFun (structure Key : HASH_KEY) (* :> HASH_TABLE where type Key.key = Key.key *) =
    struct
	open Array
	open SMLofNJ.Weak
	
	structure Key = Key

	type 'a entry = (Key.key weak * 'a weak) option

	type 'a hashtbl = 'a entry array

	fun make n = array (n, NONE)

	fun find_or_insert table key f =
	    let val len = Word.fromInt (length table)
		val hash = Key.hash key
		val firstpos = hash mod len
		val incr = (hash mod (len - 0w2)) + 0w1  (* relatively prime to len, if len is prime *)

		fun emplace pos =
		    let val data = f ()
			val entry = SOME (weak key, weak data)
		    in
			update (table, Word.toInt pos, entry);
			data
		    end

		fun loop_w_open (openpos, pos) =
		    if pos = firstpos then
			(* hash table is full, or we have a bad secondary hash *)
			f ()
		    else
			(case sub (table, Word.toInt pos) of
			     NONE =>
				 emplace openpos
			   | SOME (wkey, wdata) =>
				 (case strong wkey of
				      NONE =>
					  loop_w_open (openpos, (pos + incr) mod len)
				    | SOME key' =>
					  if Key.eq (key, key') then
					      (case strong wdata of
						   NONE =>
						       (* somehow the data has been garbage collected without the key, put it back *)
						       emplace pos
						 | SOME data => data)
					  else
					      loop_w_open (openpos, (pos + incr) mod len)))

		fun loop pos =
		    if pos = firstpos then
			(* hash table is full, or the table length isn't prime *)
			f ()
		    else
			loop' pos

		and loop' pos =
		    (case sub (table, Word.toInt pos) of
			 NONE =>
			     emplace pos
		       | SOME (wkey, wdata) =>
			     (case strong wkey of
				  NONE =>
					  loop_w_open (pos, (pos + incr) mod len)
				| SOME key' =>
					  if Key.eq (key, key') then
					      (case strong wdata of
						   NONE =>
						       (* somehow the data has been garbage collected without the key, put it back *)
						       emplace pos
						 | SOME data => data)
					  else
					      loop ((pos + incr) mod len)))
	    in
		loop' firstpos
	    end
    end