signature TOKENS =
sig

  datatype token =
      Wordtok of string
    | Numbertok of int
    | Stringtok of string
    | Floattok of real

  (* parse a char stream into tokens separated by whitespace or comments *)
  val token      : (token * Pos.T, char) Parsing.T

  (* a parser that recognizes a specific word *)
  val litWord    : string -> (string, token) Parsing.T
  (* ... a specific number *)
  val litNumber  : int -> (int, token) Parsing.T

  (* ... any word *)
  val anyWord    : (string, token) Parsing.T
  (* ... any number *)
  val anyNumber  : (int, token) Parsing.T
  (* ... "string literal" *)
  val anyString  : (string, token) Parsing.T
  (* floatingpoint.constant *)
  val anyFloat   : (real, token) Parsing.T

end
