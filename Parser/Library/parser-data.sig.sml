(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* PARSER_DATA: the signature of ParserData structures in {parser name}LrValsFun
   produced by  SML-Yacc.  All such structures match this signature.

   The {parser name}LrValsFun produces a structure which contains all the values
   except for the lexer needed to call the polymorphic parser mentioned
   before.

*)

signature PARSER_DATA =
   sig
        (* the type of line numbers *)

	type pos

	(* the type of semantic values *)

	type svalue

         (* the type of the user-supplied argument to the parser *)
 	type arg

	(* the intended type of the result of the parser.  This value is
	   produced by applying extract from the structure Actions to the
	   final semantic value resultiing from a parse.
	 *)

	type result

	structure LrTable : LR_TABLE
	structure Token : TOKEN
	sharing Token.LrTable = LrTable

	(* structure Actions contains the functions which mantain the
	   semantic values stack in the parser.  Void is used to provide
	   a default value for the semantic stack.
	 *)

	structure Actions :
	  sig
	      val actions : int * pos *
		   (LrTable.state * (svalue * pos * pos)) list * arg->
		         LrTable.nonterm * (svalue * pos * pos) *
			 ((LrTable.state *(svalue * pos * pos)) list)
	      val void : svalue
	      val extract : svalue -> result
	  end

	(* structure EC contains information used to improve error
	   recovery in an error-correcting parser *)

	structure EC :
	   sig
	     val is_keyword : LrTable.term -> bool
	     val noShift : LrTable.term -> bool
 	     val preferred_change : (LrTable.term list * LrTable.term list) list
	     val errtermvalue : LrTable.term -> svalue
	     val showTerminal : LrTable.term -> string
	     val terms: LrTable.term list
	   end

	(* table is the LR table for the parser *)

	val table : LrTable.table
    end
