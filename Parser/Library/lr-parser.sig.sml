(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* LR_PARSER: signature for a polymorphic LR parser *)

signature LR_PARSER =
    sig
	structure Stream: STREAM
	structure LrTable : LR_TABLE
	structure Token : TOKEN

	sharing LrTable = Token.LrTable

	exception ParseError

	val parse : {table : LrTable.table,
		     lexer : ('_b,'_c) Token.token Stream.stream,
		     arg: 'arg,
		     saction : int *
			       '_c *
				(LrTable.state * ('_b * '_c * '_c)) list *
				'arg ->
				     LrTable.nonterm *
				     ('_b * '_c * '_c) *
				     ((LrTable.state *('_b * '_c * '_c)) list),
		     void : '_b,
		     ec : { is_keyword : LrTable.term -> bool,
			    noShift : LrTable.term -> bool,
			    preferred_change : (LrTable.term list * LrTable.term list) list,
			    errtermvalue : LrTable.term -> '_b,
			    showTerminal : LrTable.term -> string,
			    terms: LrTable.term list,
			    error : string * '_c * '_c -> unit
			   },
		     lookahead : int  (* max amount of lookahead used in *)
				      (* error correction *)
			} -> '_b *
			     (('_b,'_c) Token.token Stream.stream)
    end
