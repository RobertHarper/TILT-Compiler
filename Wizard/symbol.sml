structure Symbol (* :> SYMBOL *) =
    struct

	exception NYI

	type symbol = string
	val compare = String.compare

	fun eq (s, s') = s = s'

	structure SymbolKey : ORD_KEY =
	    struct
		type ord_key = symbol
		val compare = compare
	    end

	structure SymbolMap : ORD_MAP = SplayMapFn (SymbolKey)
	structure SymbolSet : ORD_SET = SplaySetFn (SymbolKey)

	fun hash s = HashString.hashString s

	fun intern s = "'" ^ s

	(* gensym a new symbol *)
	local
	    val idx = ref 0
	in
	    fun new_sym () =
		let
		    val n = !idx
		    val _ = idx := n+1
		    val s = Int.toString n
		in
		    "#" ^ s
		end
	end

    end
