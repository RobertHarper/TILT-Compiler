(* char-map-sig.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Fast, read-only, maps from characters to values.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature CHAR_MAP =
  sig

    type 'a char_map
	(* a finite map from characters to 'a *)

    val mkCharMap : {default : 'a, bindings : (string * 'a) list} -> 'a char_map
	(* make a character map which maps the bound characters to their
	 * bindings and maps everything else to the default value.
	 *)

    val mapChr : 'a char_map -> char -> 'a
	(* map the given character *)
    val mapStrChr : 'a char_map -> (string * int) -> 'a
	(* (mapStrChr c (s, i)) is equivalent to (mapChr c (String.sub(s, i))) *)

  end (* CHAR_MAP *)

