(* char.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Char :> CHAR
    where type char = char
    where type string = string =
struct

    val int32touint32 = TiltPrim.int32touint32
    val && = TiltPrim.&&
    val uplus = TiltPrim.uplus
    val ult = TiltPrim.ult
    val ilt = TiltPrim.ilt
    val igt = TiltPrim.igt
    val unsafe_sub8 = TiltPrim.unsafe_sub8
    val unsafe_update8 = TiltPrim.unsafe_update8
    val unsafe_vsub = TiltPrim.unsafe_vsub
    val unsafe_vsub8 = TiltPrim.unsafe_vsub8
    val unsafe_array8 = TiltPrim.unsafe_array8

    val itoc = TiltPrim.int32touint8
    val ctoi = ord

    type char = char
    type string = string

    val minChar : char	= itoc 0
    val maxOrd		= 255
    val maxChar	: char	= itoc maxOrd

    fun chr (i : int) : char = if 0 <= i andalso i <= maxOrd then itoc i
			       else raise Chr
    val ord = ord

    fun pred (c : char) : char = let
	  val c' = (ctoi c - 1)
	  in
	    if (c' < 0) then raise Chr else (itoc c')
	  end
    fun succ (c : char) : char = let
	  val c' = (ctoi c + 1)
	  in
	    if (maxOrd < c') then raise Chr else (itoc c')
	  end


    val (op <)  = (op <  : char * char -> bool)
    val (op <=) = (op <= : char * char -> bool)
    val (op >)  = (op >  : char * char -> bool)
    val (op >=) = (op >= : char * char -> bool)


    fun compare (c1 : char, c2 : char) =
	if (c1 = c2) then EQUAL
	else if (c1 < c2) then LESS
	     else GREATER

  (* testing character membership *)
    local
      fun ord' c = int32touint32(ord c)
      fun mkArray (s, sLen) =
	  let
	      val sLen = int32touint32 sLen
	      val ca = unsafe_array8 (int32touint32 (maxOrd+1),#"\000")
	      fun ins i = if ult(i,sLen)
			      then (unsafe_update8 (ca, ord'(unsafe_vsub8(s, i)), #"\001");
				    ins(uplus(i,0w1)))
			  else ()
	      val _ = ins 0w0
	  in  ca
	  end
    in
	fun contains "" = (fn c => false)
	  | contains s =
	    let val sLen = PreString.size s
	    in
		if (sLen = 1)
		    then let val c' = unsafe_vsub8(s, 0w0)
			 in fn c => (c = c') end
		else let val cv = mkArray (s, sLen)
		     in fn c => (unsafe_sub8(cv, ord' c) <> #"\000")
		     end
	    end
	fun notContains "" = (fn c => true)
	  | notContains s =
	    let val sLen = PreString.size s
	    in
		if (sLen = 1)
		    then let val c' = unsafe_vsub8(s,0w0)
			 in fn c => (c <> c') end
		else let val cv = mkArray (s, sLen)
		     in fn c => (unsafe_sub8(cv, ord' c) = #"\000")
		     end
	    end
    end (* local *)

  (* For each character code we have an 8-bit vector, which is interpreted
   * as follows:
   *   0x01  ==  set for upper-case letters
   *   0x02  ==  set for lower-case letters
   *   0x04  ==  set for digits
   *   0x08  ==  set for white space characters
   *   0x10  ==  set for punctuation characters
   *   0x20  ==  set for control characters
   *   0x40  ==  set for hexadecimal characters
   *   0x80  ==  set for SPACE
   *)
    val ctypeTbl = "\
	    \\032\032\032\032\032\032\032\032\032\040\040\040\040\040\032\032\
	    \\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\
	    \\136\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
	    \\068\068\068\068\068\068\068\068\068\068\016\016\016\016\016\016\
	    \\016\065\065\065\065\065\065\001\001\001\001\001\001\001\001\001\
	    \\001\001\001\001\001\001\001\001\001\001\001\016\016\016\016\016\
	    \\016\066\066\066\066\066\066\002\002\002\002\002\002\002\002\002\
	    \\002\002\002\002\002\002\002\002\002\002\002\016\016\016\016\032\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	  \"
    fun inSet (c : char, s : word) = let
	  val m = int32touint32(ord(unsafe_vsub8(ctypeTbl, int32touint32 (ord c))))
	  in  &&(m, s) <> 0w0
	  end

  (* predicates on integer coding of Ascii values *)
    fun isAlpha c	= inSet(c, 0wx03)
    fun isUpper c	= inSet(c, 0wx01)
    fun isLower c	= inSet(c, 0wx02)
    fun isDigit c	= inSet(c, 0wx04)
    fun isHexDigit c	= inSet(c, 0wx40)
    fun isAlphaNum c	= inSet(c, 0wx07)
    fun isSpace c	= inSet(c, 0wx08)
    fun isPunct c	= inSet(c, 0wx10)
    fun isGraph c	= inSet(c, 0wx17)
    fun isPrint c	= inSet(c, 0wx97)
    fun isCntrl c	= inSet(c, 0wx20)
    fun isAscii c    	= c < chr 128

    val offset = ctoi #"a" - ctoi #"A"
    fun toUpper c = if (isLower c) then itoc(ctoi c - offset) else c
    fun toLower c = if (isUpper c) then itoc(ctoi c + offset) else c

  (* conversions between characters and printable representations *)
    fun digit (c:char) : int = ord c - 48

    fun hdigit (c:char) : int =
	let val n = ord c
	in  if ilt(n,65) then n - 48
	    else if ilt(n,97) then n - 55
	    else n - 87
	end

    fun numtochar (rep:'a, base:int, revdigits:int list) : (char * 'a) option =
	let fun loop (ds:int list, m:int, n:int) : (char * 'a) option =
		(case ds of
		    nil => if igt(n,maxOrd) then NONE else SOME(chr n,rep)
		|   d::ds => loop(ds,m*base,n+d*m))
	in  loop(revdigits,1,0)
	end handle Overflow => NONE

    (*
	Removes a prefix of zeros so that scanC can handle escapes like
	\x00000000000001 without causing numtochar to overflow when it
	computes base^i for digit i.
    *)
    fun dropzeros (revdigits:int list) : int list =
	let val digits = rev revdigits
	    fun skip (ds:int list) : int list =
		(case ds of
		    0 :: ds => skip ds
		|   _ => rev ds)
	in  skip digits
	end
	    
    fun scan getc rep = let
	  fun get2 rep = (case (getc rep)
		 of (SOME(c1, rep')) => (case (getc rep')
		       of (SOME(c2, rep'')) => SOME(c1, c2, rep'')
			| _ => NONE
		      (* end case *))
		  | _ => NONE
		(* end case *))
	  fun get4 rep =
		(case (get2 rep) of
		    SOME (c1,c2,rep) =>
			(case (get2 rep) of
			    SOME (c3,c4,rep) => SOME(c1,c2,c3,c4,rep)
			|   NONE => NONE)
		|   NONE => NONE)
	  in
	    case (getc rep)
	     of NONE => NONE
	      | (SOME(#"\\", rep')) => (case (getc rep')
		   of NONE => NONE
		    | (SOME(#"a", rep'')) => (SOME(#"\a", rep''))
		    | (SOME(#"b", rep'')) => (SOME(#"\b", rep''))
		    | (SOME(#"t", rep'')) => (SOME(#"\t", rep''))
		    | (SOME(#"n", rep'')) => (SOME(#"\n", rep''))
		    | (SOME(#"v", rep'')) => (SOME(#"\v", rep''))
		    | (SOME(#"f", rep'')) => (SOME(#"\f", rep''))
		    | (SOME(#"r", rep'')) => (SOME(#"\r", rep''))
		    | (SOME(#"\\", rep'')) => (SOME(#"\\", rep''))
		    | (SOME(#"\"", rep'')) => (SOME(#"\"", rep''))
		    | (SOME(#"^", rep'')) => (case (getc rep'')
			 of NONE => NONE
	    		  | (SOME(c, rep''')) =>
			      if ((#"@" <= c) andalso (c <= #"_"))
			        then SOME(chr(ord c - 64), rep''')
			        else NONE
			(* end case *))
		    | (SOME(#"u", rep)) =>
			(case (get4 rep) of
			    NONE => NONE
			|   SOME (d1,d2,d3,d4,rep) =>
				if isHexDigit d1 andalso isHexDigit d2 andalso
				    isHexDigit d3 andalso isHexDigit d4
				then numtochar(rep,16,[hdigit d4,hdigit d3,hdigit d2,hdigit d1])
				else NONE)
		    | (SOME(d1, rep'')) => if (isDigit d1)
			then (case (get2 rep'')
			   of SOME(d2, d3, rep''') =>
				if isDigit d2 andalso isDigit d3
				then numtochar(rep''',10,[digit d3,digit d2,digit d1])
				else NONE
			    | NONE => NONE
			  (* end case *))
			else if (isSpace d1) then
			    let fun loop rep =
				    (case (getc rep) of
					SOME (#"\\",rep) => scan getc rep
				    |	SOME (c,rep) =>
					    if isSpace c then loop rep else NONE
				    |	NONE => NONE)
			    in  loop rep''
			    end
			else NONE
		  (* end case *))
	      | (SOME(#"\"", rep')) => NONE	(* " *)
	      | (SOME(c, rep')) => if (isPrint c) then (SOME(c, rep')) else NONE
	    (* end case *)
	  end

    val fromString = StringCvt.scanString scan

    val itoa = (NumFormat.fmtInt StringCvt.DEC) (* o Int.fromInt *)

    fun toString #"\a" = "\\a"
      | toString #"\b" = "\\b"
      | toString #"\t" = "\\t"
      | toString #"\n" = "\\n"
      | toString #"\v" = "\\v"
      | toString #"\f" = "\\f"
      | toString #"\r" = "\\r"
      | toString #"\"" = "\\\""
      | toString #"\\" = "\\\\"
      | toString c =
	  if (isPrint c)
	    then unsafe_vsub (PreString.chars, int32touint32(ord c))
	    else let
	      val c' = ord c
	      in
		if (c > chr 32)
		  then PreString.concat2("\\", itoa c')
		  else PreString.concat2("\\^",
		    unsafe_vsub (PreString.chars, int32touint32(c'+64)))
	      end

    fun scanC getc rep =
	(case (getc rep) of
	    NONE => NONE
	|   SOME (#"\\",rep) =>
		(case (getc rep) of
		    NONE => NONE
		|   SOME (#"a",rep) => SOME(#"\a",rep)
		|   SOME (#"b",rep) => SOME(#"\b",rep)
		|   SOME (#"t",rep) => SOME(#"\t",rep)
		|   SOME (#"n",rep) => SOME(#"\n",rep)
		|   SOME (#"v",rep) => SOME(#"\v",rep)
		|   SOME (#"f",rep) => SOME(#"\f",rep)
		|   SOME (#"r",rep) => SOME(#"\r",rep)
		|   SOME (#"?",rep) => SOME(#"?",rep)
		|   r as SOME (#"\\",rep) => r
		|   r as SOME (#"\"",rep) => r
		|   r as SOME (#"'",rep) => r
		|   SOME (#"^",rep) =>
			(case (getc rep) of
			    NONE => NONE
			|   SOME (c,rep) =>
				let val n = ord c
				in  if ilt(63,n) andalso ilt(n,96) then
					SOME(chr(n - 64),rep)
				    else NONE
				end)
		|   SOME (#"x",rep) =>
			let fun cvt (rep,revdigits:int list) =
				(case revdigits of
				    nil => NONE
				|   _ => numtochar(rep,16,dropzeros revdigits))
			    fun scan (rep,acc) =
				(case (getc rep) of
				    NONE => cvt(rep,acc)
				|   SOME(c,rep') =>
					if isHexDigit c then
					    scan(rep',hdigit c::acc)
					else cvt(rep,acc))
			in  scan(rep,nil)
			end
		|   SOME (d1,rep) =>
			let fun cvt (rep,revdigits:int list) =
				numtochar(rep,8,revdigits)
			    fun isOctal (c:char) : bool =
				let val n = ord c
				in  ilt(47,n) andalso ilt(n,56)
				end
			    fun scan (0,rep,acc) = cvt(rep,acc)
			      | scan (n,rep,acc) =
				(case (getc rep) of
				    NONE => cvt(rep,acc)
				|   SOME(c,rep') =>
					if isOctal c then
					    scan(n-1,rep',digit c::acc)
					else cvt(rep,acc))
			in  if isOctal d1 then
				scan(2,rep,[digit d1])
			    else NONE
			end)
	|   SOME (#"\"",rep) => NONE
	|   r as SOME (c,rep) => if isPrint c then r else NONE)

    val fromCString = StringCvt.scanString scanC

    fun toCString #"\a" = "\\a"
      | toCString #"\b" = "\\b"
      | toCString #"\t" = "\\t"
      | toCString #"\n" = "\\n"
      | toCString #"\v" = "\\v"
      | toCString #"\f" = "\\f"
      | toCString #"\r" = "\\r"
      | toCString #"\"" = "\\\""
      | toCString #"\\" = "\\\\"
      | toCString #"?" = "\\?"
      | toCString #"'" = "\\'"
      | toCString c =
	let val n = ord c
	in  if (isPrint c) then
		unsafe_vsub (PreString.chars, int32touint32 n)
	    else
		let val s = NumFormat.fmtInt StringCvt.OCT n
		    val s = StringCvt.padLeft #"0" 3 s
		in  PreString.concat2("\\",s)
		end
	end

end
