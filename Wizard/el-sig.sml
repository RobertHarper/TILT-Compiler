signature EL =
    sig

	datatype tipe =
	    Unit
	  | Prod of tipe * tipe
	  | Arrow of tipe * tipe

	datatype term =
	    Var of Symbol.symbol
	  | Triv
	  | Pair of term * term
	  | ProjL of term
	  | ProjR of term
	  | Lam of Symbol.symbol * tipe * term
	  | App of term * term
	  | Ascription of term * tipe

	val parse_tipe : string -> tipe
	val format_tipe : tipe -> string

	val parse_term : string -> term
	val format_term : term -> string

    end
