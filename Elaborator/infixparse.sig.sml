(* Handles infix parsing of pattern and expressions. *)
signature INFIXPARSE = 
  sig


    (* parse_exp takes a fixity_table encoding infix information
                  and a list of expressions that may contain infix operators
       and returns a single exp in which these infix operators are no
	longer infix.  It does NOT recursively parse the subexpressions since
	expressions may ultimately contain declarations which could change
	the infix information. 

	parse_pat takes a fixity_table encoding infix information
	                a predicate that indicates whether a symbol is a constructor
		         a list of patterns
       and returns a single pattern in which all infix operators are no
	 longer infix.  It DOES recursively traverse all subpatterns as
	 patterns contain no declarations that could change infix information *)

    structure Il : IL
    val debug : bool ref
    val parse_exp : Il.fixity_table * Ast.exp -> Ast.exp option
    val parse_pat : Il.fixity_table * (Ast.symbol list -> bool) 
                                    * Ast.pat list -> Ast.pat list option
    val parse_datbind : Ast.db list * Ast.tb list -> (Ast.db list * Ast.tb list) option

  end
