(* Parsing Theorems *)
(* Author: Carsten Schuermann *)

signature PARSE_THM =
sig

  structure Parsing : PARSING
  structure ThmExtSyn: THMEXTSYN

  val parseTerminates' : ThmExtSyn.tdecl Parsing.parser
  val parseTheorem' : ThmExtSyn.theorem Parsing.parser
  val parseTheoremDec' : ThmExtSyn.theoremdec Parsing.parser
  val parseProve' : ThmExtSyn.prove Parsing.parser
  val parseEstablish' : ThmExtSyn.establish Parsing.parser
  val parseAssert' : ThmExtSyn.assert Parsing.parser

end;  (* signature PARSE_THM *)
