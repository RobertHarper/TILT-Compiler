(* Non-blocking binary IO utility functions;
 * to deal with strings and chars.  *)

signature BIN_IO_UTIL =
  sig
    type instream = BinIO.instream
    type outstream = BinIO.outstream
    val input_string : instream * int -> string option
    val lookahead : instream -> char option
    val input_char : instream -> char option
    val output_string : outstream * string -> unit
    val copy : instream * outstream -> unit
 end
