(*$import Prelude PARSING NAMES *)
(* Parsing Fixity Declarations *)
(* Author: Frank Pfenning *)

signature PARSE_FIXITY =
sig

  structure Parsing : PARSING
  structure Names : NAMES

  val parseFixity' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ((string * Parsing.Lexer.Paths.region) * Names.Fixity.fixity) * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front  (*((string * Parsing.Lexer.Paths.region) * Names.Fixity.fixity) Parsing.parser*)
      
  val parseNamePref' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ((string * Parsing.Lexer.Paths.region) * (string * string option)) * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front (*((string * Parsing.Lexer.Paths.region)
			* (string * string option)) Parsing.parser*)

end;  (* signature PARSE_FIXITY *)
