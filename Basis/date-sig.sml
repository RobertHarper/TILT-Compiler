(*$import Prelude StringCvt Time *)
(* date-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature DATE =
  sig

    datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

    datatype month
      = Jan | Feb | Mar | Apr | May | Jun
      | Jul | Aug | Sep | Oct | Nov | Dec

    datatype date = DATE of {
	year   : int,			(* e.g. 1995 *)
	month  : month,
	day    : int,       		(* 1-31  *)
	hour   : int,       		(* 0-23  *)
	minute : int,       		(* 0-59  *)
	second : int,       		(* 0-61 (allowing for leap seconds) *)
	wday   : weekday option,
	yday   : int option,		(* 0-365 *)
	isDst  : bool option		(* daylight savings time in force *)
      }

    exception Date

    val fromTimeLocal : Time.time -> date
    val fromTimeUniv  : Time.time -> date
    val toTime   : date -> Time.time

    val toString   : date -> string
    val fmt        : string -> date -> string
(** not yet implemented **
    val fromString : string -> date option
    val scan       : (char, 'a) StringCvt.reader -> (date, 'a) StringCvt.reader
**)

    val compare : (date * date) -> order

  end;


(*
 * $Log$
# Revision 1.1  98/03/09  15:45:44  pscheng
# adding the basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)
