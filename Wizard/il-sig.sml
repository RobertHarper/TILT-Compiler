signature IL =
    sig

	type tipe

	datatype tipe_ =
	    Unit
	  | Prod of tipe * tipe
	  | Arrow of tipe * tipe

	val expose_tipe : tipe -> tipe_
	val hide_tipe : tipe_ -> tipe

	val eq_tipe : tipe * tipe -> bool

	type term

	datatype term_ =
	    Var of Symbol.symbol
	  | Triv
	  | Pair of tipe * tipe * term * term
	  | ProjL of tipe * tipe * term
	  | ProjR of tipe * tipe * term
	  | Lam of tipe * tipe * Symbol.symbol * term
	  | App of tipe * tipe * term * term
	    
	val expose_term : term -> term_
	val hide_term : term_ -> term

	val eq_term : term * term -> bool

	val free_vars : term -> Symbol.SymbolSet.set

	type subst = term Symbol.SymbolMap.map
	val subst : subst * term -> term

	val format_tipe : tipe -> string
	val format_term : term -> string

    end
