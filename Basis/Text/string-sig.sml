(*$import Prelude CHAR *)
(* string-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature STRING =
  sig
    eqtype string
    structure Char : CHAR
	(* sharing type string = Char.string -- Holds for TILT but not part of this signature *)

    val maxSize : int

    val size      : string -> int
    val sub       : string * int -> Char.char
    val extract   : string * int * int option -> string
    val substring : string * int * int -> string
    val concat    : string list -> string
    val ^         : string * string -> string
    val str       : Char.char -> string
    val implode   : Char.char list -> string
    val explode   : string -> Char.char list

    val map       : (Char.char -> Char.char) -> string -> string
    val translate : (Char.char -> string) -> string -> string
    val tokens    : (Char.char -> bool) -> string -> string list
    val fields    : (Char.char -> bool) -> string -> string list

    val isPrefix : string -> string -> bool
    val compare  : string * string -> order
    val collate  : (Char.char * Char.char -> order) -> string * string -> order

    val <  : string * string -> bool
    val <= : string * string -> bool
    val >  : string * string -> bool
    val >= : string * string -> bool

    val fromString  : string -> string option
    val toString    : string -> string
    val fromCString : string -> string option
    val toCString   : string -> string

  end

