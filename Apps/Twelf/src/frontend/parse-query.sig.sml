(*$import PARSING TP_RECON *)
(* Parsing Queries *) 
(* Author: Frank Pfenning *)

signature PARSE_QUERY =
sig

  structure Parsing : PARSING
  structure ExtSyn : EXTSYN

  val parseQuery' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ExtSyn.query * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front(*ExtSyn.query Parsing.parser*)

end;  (* signature PARSE_QUERY *)
