(*$import Firstlude TiltPrim Prelude StringCvt *)
(* bool-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature BOOL =
  sig

    datatype bool = false | true
    val not : bool -> bool

    val fromString : string -> bool option
    val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
    val toString   : bool -> string

  end

