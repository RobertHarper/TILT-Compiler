(*$import PARSING TP_RECON *)
(* Parsing Signature Entries *) 
(* Author: Frank Pfenning *)

signature PARSE_CONDEC =
sig

  structure Parsing : PARSING
  structure ExtSyn : EXTSYN

  val parseConDec' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ExtSyn.condec * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front (*ExtSyn.condec Parsing.parser*)

end;  (* signature PARSE_CONDEC *)
