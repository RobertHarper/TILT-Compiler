structure EL (* :> EL *) =
    struct

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

	
	exception NYI

	fun parse_tipe s = raise NYI
	fun parse_term s = raise NYI

	fun format_tipe t = raise NYI
	fun format_term t = raise NYI

    end
