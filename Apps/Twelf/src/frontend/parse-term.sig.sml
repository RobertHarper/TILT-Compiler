(*$import PARSING TP_RECON *)
(* Parsing Terms and Declarations *)
(* Author: Frank Pfenning *)

signature PARSE_TERM =
sig

  structure Parsing : PARSING
  structure ExtSyn : EXTSYN

  val parseTerm' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ExtSyn.term * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front(*ExtSyn.term Parsing.parser*)
  val parseDec'  : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ExtSyn.dec * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front(*ExtSyn.dec Parsing.parser*)

end;  (* signature PARSE_TERM *)
