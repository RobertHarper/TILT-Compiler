(*$import Prelude List PreTime DATE Time POSIX_extern Vector StringCvt Int *)
(* date.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Date :> DATE =
  struct


  (* the run-time system indexes the year off this *)
    val baseYear = 1900

    exception Date

    datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

    datatype month
      = Jan | Feb | Mar | Apr | May | Jun
      | Jul | Aug | Sep | Oct | Nov | Dec

    datatype date = DATE of {
        year : int,
	month  : month,
	day : int,
	hour : int,
	minute : int,
	second : int,
	offset : Time.time option,
	wday   : weekday,
	yday : int,
	isDst : bool option
      }

  (* tables for mapping integers to days/months *)
    val dayTbl = #[Sun, Mon, Tue, Wed, Thu, Fri, Sat]
    val monthTbl = #[Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]

    fun dayToInt (d) = (case d
			  of Sun => 0 | Mon => 1 | Tue => 2 | Wed => 3
			   | Thu => 4 | Fri => 5 | Sat => 6
	                (* end case *))

    (* careful about this: the month numbers are 0-11 *)
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

    (* note: mkTime assumes the tm structure passed to it reflects
     * the local time zone
     *)
    val ascTime : tm -> string = wrap (fn arg => Ccall(posix_ascTime, arg))
    val localTime : int -> tm = wrap (fn arg => Ccall(posix_localTime, arg))
    val gmTime : int -> tm = wrap (fn arg => Ccall(posix_gmTime, arg))
    val mkTime : tm -> int = wrap (fn arg => Ccall(posix_mkTime, arg))
    val strfTime : (string * tm) -> string = wrap (fn arg => Ccall(posix_strfTime, arg))

    fun year (DATE{year, ...}) = year
    fun month (DATE{month, ...}) = month
    fun day (DATE{day, ...}) = day
    fun hour (DATE{hour, ...}) = hour
    fun minute (DATE{minute, ...}) = minute
    fun second (DATE{second, ...}) = second
    fun weekDay (DATE{wday, ...}) = wday
    fun yearDay (DATE{yday, ...}) = yday
    fun isDst (DATE{isDst, ...}) = isDst
    fun offset (DATE{offset,...}) = offset

    fun localOffset () = raise TiltExn.LibFail "Date.localOffset not implemented"
 
    (* 
     * This code is taken from Reingold's paper
     *)
    local 
(*	val quot = Int.quot
	val not = Bool.not *)
	fun sum (f,k,p) = 
	    let fun loop (f,i,p,acc) = if (not(p(i))) then acc
				       else loop(f,i+1,p,acc+f(i))
	    in
		loop (f,k,p,0)
	    end
	fun lastDayOfGregorianMonth (month,year) =
	    if ((month=1) andalso 
		(Int.mod (year,4) = 0) andalso 
		not (Int.mod (year,400) = 100) andalso
		not (Int.mod (year,400) = 200) andalso
		not (Int.mod (year,400) = 300))
		then 29
	    else List.nth ([31,28,31,30,31,30,31,31,30,31,30,31],month)
    in
	fun toAbsolute (month, day, year) =
	    day  
	    + sum (fn (m) => lastDayOfGregorianMonth(m,year),0,
		   fn (m) => (m<month)) 
	    + 365 * (year -1)
	    + Int.quot (year-1,4)
	    - Int.quot (year-1,100)
	    + Int.quot (year-1,400)
	fun fromAbsolute (abs) =
	    let val approx = Int.quot (abs,366)
		val year = (approx + sum(fn(_)=>1, approx, 
					 fn(y)=> (abs >= toAbsolute(0,1,y+1))))
		val month = (sum (fn(_)=>1, 0,
				  fn(m)=> (abs > toAbsolute(m,lastDayOfGregorianMonth(m,year),year))))
		val day = (abs - toAbsolute(month,1,year) + 1)
	    in
		(month, day, year)
	    end
	fun wday (month,day,year) =
	    let val abs = toAbsolute (month,day,year)
	    in
		Vector.sub (dayTbl, Int.mod(abs,7))
	    end
	fun yday (month, day, year) = 
	    let val abs = toAbsolute (month, day, year)
		val daysPrior = 
		    365 * (year -1)
		    + Int.quot (year-1,4)
		    - Int.quot (year-1,100)
		    + Int.quot (year-1,400)
	    in 
		abs - daysPrior - 1    (* to conform to ISO standard *)
	    end
    end

    (*
     * this function should also canonicalize the time (hours, etc...)
     *)
    fun canonicalizeDate (DATE d) = 
	let val args = (monthToInt(#month d), #day d, #year d)
	    val (monthC,dayC,yearC) = fromAbsolute (toAbsolute (args))
	    val yday = yday (args)
	    val wday = wday (args)
	in
	    DATE {year = yearC,
		  month = Vector.sub (monthTbl,monthC),
		  day = dayC,
		  hour = #hour d,
		  minute = #minute d,
		  second = #second d,
		  offset = #offset d,
		  isDst = NONE,
		  yday = yday,
		  wday = wday}
	end

    fun toTM (DATE d) = (
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

	fun fromTM (
		    tm_sec, tm_min, tm_hour, tm_mday, tm_mon,
		    tm_year, tm_wday, tm_yday, tm_isdst
		    ) offset = DATE{
				    year = baseYear + tm_year,
				    month = Vector.sub(monthTbl, tm_mon),
				    day = tm_mday,
				    hour = tm_hour,
				    minute = tm_min,
				    second = tm_sec,
				    wday = Vector.sub(dayTbl, tm_wday),
				    yday = tm_yday,
				    isDst = if (tm_isdst < 0) then NONE else SOME(tm_isdst <> 0),
				    offset = offset
				    }

	(* takes two tm's and returns the second tm with 
	 * its dst flag set to the first one's.
	 * Used to compute local offsets 
	 *)
	fun toSameDstTM ((tm_sec, tm_min, tm_hour, tm_mday, tm_mon,
			  tm_year, tm_wday, tm_yday, tm_isdst),
			 (tm_sec', tm_min', tm_hour', tm_mday', tm_mon',
			  tm_year', tm_wday', tm_yday', tm_isdst')) = 
	    (tm_sec', tm_min', tm_hour', tm_mday', tm_mon',
	     tm_year', tm_wday', tm_yday', tm_isdst)

	(* a diff is +/- seconds between local time and gmt
	 * what to add to local time to get gmt
	 *)

	val secInDay = Int.fromInt(60 * 60 * 24)
	val secInHDay = Int.fromInt(30 * 30 * 24)
(*
	fun diffToOffset (d) =
	    if (d<0) then Time.fromSeconds (secInDay+d)
	    else Time.fromSeconds(d)
*)
	fun offsetToDiff (off) =
	    let val s = Time.toSeconds (off)
	    in
		if (s>secInHDay) then secInHDay-s else s
	    end

	(* 
	 * this function is meant as an analogue to 
	 * mkTime, but constructs UTC time instead of localtime
	 * idea:  mkTime (localtime(t))= t
	 *        mkGMTime (gmtime(t))= t
	 *)

	fun localDiff (tm) = 
	    let val t = mkTime (tm)
		val loc = localTime (t)
		val gmt = gmTime (t)
	    in
		mkTime (toSameDstTM(loc,gmt)) - mkTime(loc)
	    end

	fun mkGMTime (tm) = mkTime (toSameDstTM (localTime(mkTime(tm)),tm)) -
	    localDiff (tm) 

	fun toSeconds (d) = 
	    let val tm = toTM (d)
	    in
		case (offset d) of
		    NONE => mkTime (tm)
		  | SOME (offsetV) => mkGMTime (tm) + offsetToDiff (offsetV)
	    end

	val toTime = Time.fromSeconds o toSeconds

	fun fromTimeLocal (t) =
	    fromTM (localTime (Time.toSeconds (t))) NONE

	fun fromTimeOffset (t, offset) =
	    fromTM (gmTime (Time.toSeconds (t) - Time.toSeconds(offset)))
	    (SOME offset)

	fun fromTimeUniv (t) = fromTimeOffset (t,Time.zeroTime)

	fun date {year,month,day,hour,minute,second,offset} = 
	    let val d = DATE {second = second,
			      minute = minute,
			      hour = hour,
			      year = year,
			      month = month, 
			      day = day,
			      offset = offset,
			      isDst = NONE,
			      yday = 0,
			      wday = Mon}
		val canonicalDate = canonicalizeDate (d)
		fun internalDate () = 
		    (case (offset) of
			 NONE => fromTimeLocal (toTime canonicalDate)
		       | SOME (offsetV) => fromTimeOffset (toTime canonicalDate,
							   offsetV))
	  in 
		internalDate () handle Date => d
	    end

	fun toString d = ascTime (toTM d)
	    
	fun fmt fmtStr d = strfTime (fmtStr, toTM d)

	fun fromString (_ : string) : date option = raise TiltExn.LibFail "Date.fromString unimplemented"

	fun scan (_ : (char, 'a) StringCvt.reader) (_ : 'a) : (date * 'a) option =
	    raise TiltExn.LibFail "Date.scan unimplemented"
		
	(* comparison does not take into account the offset
	 * thus, it does not compare dates in different time zones
	 *)
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
# Revision 1.5  2000/11/27  22:36:17  swasey
# *** empty log message ***
# 
 * Revision 1.4  2000/09/12 18:54:03  swasey
 * Changes for cutoff compilation
 *
 * Revision 1.3  2000/08/21 20:29:19  swasey
 * Changes for the new Manager.
 *
# Revision 1.2  98/04/06  21:17:34  pscheng
# update: Typeof_c, dependent arrow/record types
# 
# Revision 1.1  1998/03/09  15:45:45  pscheng
# adding the basis
#
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)
