(* list-format-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 *)

signature LIST_FORMAT =
  sig

    val formatList : {
	    init : string,
	    sep : string,
	    final : string,
	    fmt : 'a -> string
	  } -> 'a list -> string
	(* given an initial string (init), a separator (sep), a terminating
	 * string (final), and an item formating function (fmt), return a list
	 * formatting function.  The list ``[a, b, ..., c]'' gets formated as
	 * ``init ^ (fmt a) ^ sep ^ (fmt b) ^ sep ^ ... ^ sep ^ (fmt c) ^ final.''
	 *)

    val scanList : {
	    init : string,
	    sep : string,
	    final : string,
	    scan : (char, 'b) StringCvt.reader -> ('a, 'b) StringCvt.reader
	  } -> (char, 'b) StringCvt.reader -> ('a list, 'b) StringCvt.reader
	(* given an expected initial string, a separator, a terminating
	 * string, and an item scanning function, return a function that
	 * scans a string for a list of items.  Whitespace is ignored.
	 * If the input string has the incorrect syntax, then NONE is returned.
	 *)

  end; (* LIST_FORMAT *)
