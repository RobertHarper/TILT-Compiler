(*$import PARSING MODE_RECON *)
(* Parsing Mode Declarations *)
(* Author: Carsten Schuermann *)

signature PARSE_MODE =
sig

  structure Parsing : PARSING
  structure ExtModes: EXTMODES

  val parseMode' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ExtModes.modedec * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front (*ExtModes.modedec Parsing.parser*)

end;  (* signature PARSE_MODE *)
