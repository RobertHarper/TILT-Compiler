(*$import PARSING THM_RECON *)
(* Parsing Theorems *)
(* Author: Carsten Schuermann *)

signature PARSE_THM =
sig

  structure Parsing : PARSING
  structure ThmExtSyn: THMEXTSYN

  val parseTerminates' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ThmExtSyn.tdecl * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front(*ThmExtSyn.tdecl Parsing.parser*)
  val parseTheorem' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ThmExtSyn.theorem * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front(*ThmExtSyn.theorem Parsing.parser*)
  val parseTheoremDec' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ThmExtSyn.theoremdec * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front(*ThmExtSyn.theoremdec Parsing.parser*)
  val parseProve' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ThmExtSyn.prove * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front(*ThmExtSyn.prove Parsing.parser*)
  val parseEstablish' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ThmExtSyn.establish * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front(*ThmExtSyn.establish Parsing.parser*)
  val parseAssert' : (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front -> ThmExtSyn.assert * (Parsing.Lexer.Token * Parsing.Lexer.Paths.region) Parsing.Stream.front(*ThmExtSyn.assert Parsing.parser*)

end;  (* signature PARSE_THM *)
