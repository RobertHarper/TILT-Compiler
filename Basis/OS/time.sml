(*$import TIME PreTime Int32 Real64 Bool NumFormat Char *)
(* time.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Time : TIME =
  struct

(*    structure PB = PreBasis *)

  (* get time type from type-only structure *)
(*    open Time *)

    datatype time = datatype time
    exception Time

    val zeroTime = TIME{sec=0, usec=0}

    fun toSeconds (TIME{sec, ...}) = sec  (* should we round? *)
    fun fromSeconds sec =
	  if (sec < 0)
	    then raise Time
	    else TIME{sec=sec, usec=0}

    fun toMilliseconds (TIME{sec, usec}) =
	  (sec * 1000) + Int.quot(usec, 1000)
    fun fromMilliseconds msec =
	  if (msec < 0)
	    then raise Time
	  else if (msec >= 1000)
	    then TIME{sec= Int.quot(msec, 1000), usec= 1000*(Int.rem(msec, 1000))}
	    else TIME{sec= 0, usec= 1000*msec}

    fun toMicroseconds (TIME{sec, usec}) =
	  (sec * 1000000) + usec
    fun fromMicroseconds usec =
	  if (usec < 0)
	    then raise Time
	  else if (usec >= 1000000)
	    then TIME{sec= Int.quot(usec, 1000000), usec= Int.rem(usec,  1000000)}
	    else TIME{sec=0, usec=usec}

    fun fromReal rt = if (rt < 0.0)
	  then raise Time
	  else let
	    val sec = Real.floor rt
	    in
	      TIME{sec=sec, usec=Real.floor((rt - Real.fromInt sec) * 1000000.0)}
	    end

    fun toReal (TIME{sec, usec}) =
	  (Real.fromInt sec) + ((Real.fromInt usec) * 0.000001)

    fun add (TIME{sec=s1, usec=u1}, TIME{sec=s2, usec=u2}) = let
	  val s = s1 + s2
	  val u = u1+u2
	  in
	    if (u >= 1000000)
	      then TIME{sec=s+1, usec=u-1000000}
	      else TIME{sec=s, usec=u}
	  end
    fun sub (TIME{sec=s1, usec=u1}, TIME{sec=s2, usec=u2}) = let
	  val s = s1 - s2
	  val u = u1 - u2
	  val (s, u) = if (u < 0) then (s-1, u+1000000) else (s, u)
	  in
	    if (s < 0)
	      then raise Time
	      else TIME{sec=s, usec=u}
	  end

    fun compare (TIME{sec=s1, usec=u1}, TIME{sec=s2, usec=u2}) =
	  if (s1 < s2) then LESS
	  else if (s1 = s2)
	    then if (u1 < u2) then LESS
	    else if (u1 = u2) then EQUAL
	    else GREATER
	  else GREATER

    fun less (TIME{sec=s1, usec=u1}, TIME{sec=s2, usec=u2}) =
	  (s1 < s2) orelse ((s1 = s2) andalso (u1 < u2))
    fun lessEq (TIME{sec=s1, usec=u1}, TIME{sec=s2, usec=u2}) =
	  (s1 < s2) orelse ((s1 = s2) andalso (u1 <= u2))


    fun now () = let val (ts, tu) = Ccall(ml_timeofday,())
	  in
	    TIME{sec=ts, usec=tu}
	  end


    local
      val zeros = "0000000000"
      val numZeros = String.size zeros
      fun pad 0 = []
	| pad n = if (n <= numZeros)
	    then [substring(zeros, 0, n)]
	    else zeros :: pad(n - numZeros)
      val fmtInt = (NumFormat.fmtInt StringCvt.DEC) o Int32.fromInt
    in
    fun fmt prec (TIME{sec, usec}) = let
	  val sec' = fmtInt sec
	  in
	    if (prec <= 0)
	      then sec'
	      else let
		val usec' = fmtInt usec
		val frac = String.substring(zeros, 0, 6 - String.size usec') ^ usec'
		in
		  if (prec < 6)
		    then String.concat [
			sec', ".", String.substring(frac, 0, prec)
		      ]
		    else String.concat (sec' :: "." :: frac :: pad(prec-6))
		end
	  end
    end (* local *)

  (* scan a time value; this has the syntax:
   *
   *  [0-9]+(.[0-9]+)? | .[0-9]+
   *)
    fun scan getc charStrm = let
	  fun chrLE (x : char, y : char) : bool = ulte(uint8touint32 x, uint8touint32 y)
	  fun isDigit c = (chrLE(#"0", c) andalso chrLE(c, #"9"))
	  fun incByDigit (n, c) = 10*n + (Char.ord c - Char.ord #"0")
	  fun scanSec (secs, cs) = (case (getc cs)
		 of NONE => SOME(TIME{sec=secs, usec=0}, cs)
		  | (SOME(#".", cs')) => (case (getc cs')
		       of NONE => SOME(TIME{sec=secs, usec=0}, cs)
			| (SOME(d, cs'')) => if (isDigit d)
			    then scanUSec (secs, cs')
			    else SOME(TIME{sec=secs, usec=0}, cs)
		      (* end case *))
		  | (SOME(d, cs')) => if (isDigit d)
		      then scanSec(incByDigit(secs, d), cs')
		      else SOME(TIME{sec=secs, usec=0}, cs)
		(* end case *))
	  and scanUSec (secs, cs) = let
		fun normalize (usecs, 6) = usecs
		  | normalize (usecs, n) = normalize(10*usecs, n+1)
		fun scan' (usecs, 6, cs) = (case (getc cs)
		       of NONE => (usecs, cs)
			| (SOME(d, cs')) => if (isDigit d)
			    then scan' (usecs, 6, cs')
			    else (usecs, cs)
		      (* end case *))
		  | scan' (usecs, ndigits, cs) = (case (getc cs)
		       of NONE => (normalize(usecs, ndigits), cs)
			| (SOME(d, cs')) => if (isDigit d)
			    then scan' (incByDigit(usecs, d), ndigits+1, cs')
			    else (normalize(usecs, ndigits), cs)
		      (* end case *))
		val (usecs, cs) = scan' (0, 0, cs)
		in
		  SOME(TIME{sec=secs, usec=usecs}, cs)
		end
	  val cs = StringCvt.skipWS getc charStrm
	  in
	    case (getc cs)
	     of NONE => NONE
	      | (SOME(#".", cs')) => (case (getc cs')
		   of NONE => NONE
		    | (SOME(d, _)) =>
			if (isDigit d) then scanUSec (0, cs') else NONE
		  (* end case *))
	      | (SOME(d, _)) => if (isDigit d) then scanSec(0, cs) else NONE
	    (* end case *)
	  end

    val toString   = fmt 3
    val fromString = StringCvt.scanString scan

    val (op +) = add
    val (op -) = sub

    val (op <)  = less
    val (op <=) = lessEq
    val (op >)  = Bool.not o lessEq
    val (op >=) = Bool.not o less

  end (* TIME *)

(*
 * $Log$
# Revision 1.2  98/04/06  21:17:42  pscheng
# update: Typeof_c, dependent arrow/record types
# 
# Revision 1.1  1998/03/09  19:53:11  pscheng
# added basis
#
 * Revision 1.2  1997/05/29  14:44:29  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
