(*$import StreamStructs LEXER *)
(* General basis for parsing modules *)
(* Author: Frank Pfenning *)

signature PARSING =
sig
  structure Stream : STREAM
  structure Lexer : LEXER
    sharing Lexer.Stream = Stream

  (*type lexResult = Lexer.Token * Lexer.Paths.region*)

  (*type 'a parser = (Lexer.Token * Lexer.Paths.region (*lexResult*)) Stream.front -> 'a * (Lexer.Token * Lexer.Paths.region (*lexResult*)) Stream.front*)

  exception Error of string
  val error : Lexer.Paths.region * string -> 'a	(* always raises Error *)
end;  (* signature PARSING *)
