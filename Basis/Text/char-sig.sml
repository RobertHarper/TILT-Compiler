(*$import Prelude StringCvt *)
(* char-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature CHAR =
  sig

    eqtype char
    eqtype string

    val minChar : char
    val maxChar : char
    val maxOrd  : int

    val ord : char -> int
    val chr : int -> char

    val succ : char -> char
    val pred : char -> char

    val <  : char * char -> bool
    val <= : char * char -> bool
    val >  : char * char -> bool
    val >= : char * char -> bool

    val compare : char * char -> order

    val contains : string -> char -> bool
    val notContains : string -> char -> bool
	
    val toLower : char -> char
    val toUpper : char -> char

    val isAlpha    : char -> bool   (* isUpper orelse isLower *)
    val isAlphaNum : char -> bool   (* isAlpha orelse isDigit *)
    val isAscii    : char -> bool   (* ord c < 128 *)
    val isCntrl    : char -> bool
    val isDigit    : char -> bool   (* contains "0123456789" *)
    val isGraph    : char -> bool   (* (not isSpace) andalso isPrint *)
    val isHexDigit : char -> bool   (* isDigit orelse contains "abcdefABCDEF" *)
    val isLower    : char -> bool   (* contains "abcdefghijklmnopqrstuvwxyz" *)
    val isPrint    : char -> bool   (* any printable character (incl. #" ") *)
    val isSpace    : char -> bool   (* contains " \t\r\n\v\f" *)
    val isPunct    : char -> bool
    val isUpper    : char -> bool   (* contains "ABCDEFGHIJKLMNOPQRSTUVWXYZ" *)

    val fromString  : string -> char option
    val scan : (char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader
    val toString    : char -> string
    val fromCString : string -> char option
    val toCString   : char -> string

  end; (* CHAR *)

