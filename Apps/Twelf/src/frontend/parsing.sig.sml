(* General basis for parsing modules *)
(* Author: Frank Pfenning *)

signature PARSING =
sig
  structure Stream : STREAM
  structure Lexer : LEXER
    sharing Lexer.Stream = Stream

  type lexResult = Lexer.Token * Lexer.Paths.region

  type 'a parser = lexResult Stream.front -> 'a * lexResult Stream.front

  exception Error of string
  val error : Lexer.Paths.region * string -> 'a	(* always raises Error *)
end;  (* signature PARSING *)
