signature SYMBOL =
    sig

	type symbol
	val compare : symbol * symbol -> order

	val hash : symbol -> Word.word
	val eq : symbol * symbol -> bool

	structure SymbolMap : ORD_MAP where type Key.ord_key = symbol
	structure SymbolSet : ORD_SET where type Key.ord_key = symbol

	val intern : string -> symbol
        val new_sym : unit -> symbol

    end
