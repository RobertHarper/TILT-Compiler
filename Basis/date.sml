(*$import PreTime DATE Time POSIX_extern Vector *)
(* date.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Date :> DATE =
  struct
(*
    val op + = InlineT.DfltInt.+
    val op - = InlineT.DfltInt.-
    val op < = InlineT.DfltInt.<
    val op <= = InlineT.DfltInt.<=
    val op > = InlineT.DfltInt.>
    val op >= = InlineT.DfltInt.>=
    val op = = InlineT.=
*)

  (* the run-time system indexes the year off this *)
    val baseYear = 1900

    exception Date

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

  (* tables for mapping integers to days/months *)
    val dayTbl = #[Sun, Mon, Tue, Wed, Thu, Fri, Sat]
    val monthTbl = #[Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]

    fun dayToInt NONE = 0
      | dayToInt (SOME d) = (case d
	   of Sun => 0 | Mon => 1 | Tue => 2 | Wed => 3
	    | Thu => 4 | Fri => 5 | Sat => 6
	  (* end case *))
    fun monthToInt m = (case m
	   of Jan => 0 | Feb => 1 | Mar => 2 | Apr => 3 | May => 4 | Jun => 5
	    | Jul => 6 | Aug => 7 | Sep => 8 | Oct => 9 | Nov => 10 | Dec => 11
	  (* end case *))

  (* the tuple type used to communicate with C; this 9-tuple has the fields:
   * tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday, tm_yday,
   * and tm_isdst.
   *)
    type tm = (int * int * int * int * int * int * int * int * int)

  (* wrap a C function call with a handler that maps SysErr exception into Date
   * exceptions.
   *)
    fun wrap f x = (f x) handle _ => raise Date

    val ascTime : tm -> string = wrap (fn arg => posix_ascTime arg)
    val localTime : (int * int) -> tm = wrap (fn arg => posix_localTime arg)
    val gmTime : (int * int) -> tm = wrap (fn arg => posix_gmTime arg)
    val mkTime : tm -> (int * int) = wrap (fn arg => posix_mkTime arg)
    val strfTime : (string * tm) -> string = wrap (fn arg => posix_strfTime arg)


    fun mkDate (
	  tm_sec, tm_min, tm_hour, tm_mday, tm_mon,
	  tm_year, tm_wday, tm_yday, tm_isdst
	) = DATE{
		year = baseYear + tm_year,
		month = Vector.sub(monthTbl, tm_mon),
		day = tm_mday,
		hour = tm_hour,
		minute = tm_min,
		second = tm_sec,
		wday = SOME(Vector.sub(dayTbl, tm_wday)),
		yday = SOME(tm_yday),
		isDst = if (tm_isdst < 0) then NONE else SOME(tm_isdst <> 0)
	      }

    fun dateToTM (DATE d) = (
	    #second d,			(* tm_sec *)
	    #minute d,			(* tm_min *)
	    #hour d,			(* tm_hour *)
	    #day d,			(* tm_mday *)
	    monthToInt(#month d),	(* tm_mon *)
	    #year d - baseYear,		(* tm_year *)
	    dayToInt(#wday d),		(* tm_wday *)
	    0,				(* tm_yday *)
	    case (#isDst d)		(* tm_isdst *)
	     of NONE => ~1
	      | (SOME false) => 0
	      | (SOME true) => 1
	    (* end case *)
	  )

    fun fromTimeLocal (TIME{sec, usec}) = mkDate(localTime(sec, usec))
    fun fromTimeUniv (TIME{sec, usec}) = mkDate(gmTime(sec, usec))
    fun toTime d = let
	  val (sec, usec) = mkTime (dateToTM d)
	  in
	    TIME{sec=sec, usec=usec}
	  end

  (* check a date for consistency (stolen from Peter Sestoft) *)
    fun chkDate (DATE{year, month, day, hour, minute, second, yday, ...}) = let
	  val isLeap = (Int.rem(year, 4) = 0) andalso (Int.rem(year, 100) <> 0) 
	               orelse (Int.rem(year, 400) = 0)
          val mthdays = (case month
		 of Jan => 31 | Feb => if isLeap then 29 else 28
                  | Mar => 31 | Apr => 30 | May => 31 | Jun => 30
                  | Jul => 31 | Aug => 31 | Sep => 30 | Oct => 31
                  | Nov => 30 | Dec => 31
		(* end case *))
	  val yeardays = if isLeap then 366 else 365
	  in 
             baseYear <= year 
            andalso 1 <= day    andalso day    <= mthdays
            andalso 0 <= hour   andalso hour   <= 24
            andalso 0 <= minute andalso minute <= 59
            andalso 0 <= second andalso second <= 61
            andalso (case yday
	       of NONE    => true
                | SOME yd => 0 <= yd andalso yd < yeardays
	      (* end case *))
	  end;

    fun toString (DATE{wday=NONE, ...}) = raise Date
      | toString d = ascTime (dateToTM d)
    fun fmt fmtStr d = strfTime (fmtStr, dateToTM d)

(**
    val fromString : string -> date option
    val scan       : (getc : (char, 'a) StringCvt.reader) -> 'a -> (date * 'a) option
**)

    fun compare (DATE d1, DATE d2) = let
	  fun cmp (i1::r1, i2::r2) =
		if (i1 < i2) then LESS
		else if (i1 = i2) then cmp (r1, r2)
		else GREATER
	    | cmp _ = EQUAL
	  in
	    cmp (
	      [#year d1, monthToInt(#month d1), #day d1, #hour d1, #minute d1, #second d1],
	      [#year d2, monthToInt(#month d2), #day d2, #hour d2, #minute d2, #second d2])
	  end

  end;


(*
 * $Log$
# Revision 1.1  98/03/09  15:45:45  pscheng
# adding the basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)
