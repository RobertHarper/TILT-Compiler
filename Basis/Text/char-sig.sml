(*$import Prelude StringCvt *)
(* char-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature CHAR =
  sig

    eqtype char

    val chr : int -> char
    val ord : char -> int

    val minChar : char
    val maxChar : char
    val maxOrd  : int

    val pred : char -> char
    val succ : char -> char

    val <  : (char * char) -> bool
    val <= : (char * char) -> bool
    val >  : (char * char) -> bool
    val >= : (char * char) -> bool

    val compare : (char * char) -> order

    val scan : (char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader
    val fromString  : string -> char option
    val toString    : char -> string
    val fromCString : string -> char option
    val toCString   : char -> string

    val contains : string -> char -> bool
    val notContains : string -> char -> bool

    val isLower    : char -> bool   (* contains "abcdefghijklmnopqrstuvwxyz" *)
    val isUpper    : char -> bool   (* contains "ABCDEFGHIJKLMNOPQRSTUVWXYZ" *)
    val isDigit    : char -> bool   (* contains "0123456789" *)
    val isAlpha    : char -> bool   (* isUpper orelse isLower *)
    val isHexDigit : char -> bool   (* isDigit orelse contains "abcdefABCDEF" *)
    val isAlphaNum : char -> bool   (* isAlpha orelse isDigit *)
    val isPrint    : char -> bool   (* any printable character (incl. #" ") *)
    val isSpace    : char -> bool   (* contains " \t\r\n\v\f" *)
    val isPunct    : char -> bool
    val isGraph    : char -> bool   (* (not isSpace) andalso isPrint *)
    val isCntrl    : char -> bool
    val isAscii    : char -> bool   (* ord c < 128 *)

    val toUpper : char -> char
    val toLower : char -> char

  end; (* CHAR *)

