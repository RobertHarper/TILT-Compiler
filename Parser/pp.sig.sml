(*$import Prelude *)

signature PRETTYPRINT =
sig
  type ppstream
  type ppconsumer = {consumer : string -> unit,
		    linewidth : int,
		    flush : unit -> unit} 
  datatype break_style
    = CONSISTENT
    | INCONSISTENT

  exception PP_FAIL of string

  val mk_ppstream    : ppconsumer -> ppstream
  val dest_ppstream  : ppstream -> ppconsumer
  val add_break      : ppstream -> int * int -> unit
  val add_newline    : ppstream -> unit
  val add_string     : ppstream -> string -> unit
  val begin_block    : ppstream -> break_style -> int -> unit
  val end_block      : ppstream -> unit
  val clear_ppstream : ppstream -> unit
  val flush_ppstream : ppstream -> unit
  val with_pp : ppconsumer -> (ppstream -> unit) -> unit
  val pp_to_string : int -> (ppstream -> 'a -> unit) -> 'a -> string
end