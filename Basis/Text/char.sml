(*$import Prelude CHAR General PreString NumFormat StringCvt *)
(* char.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Char :> CHAR where type char = char = 
  struct

(*
    structure C = InlineT.Char

    val op + = InlineT.DfltInt.+
    val op - = InlineT.DfltInt.-
    val op * = InlineT.DfltInt.*

    val itoc : int -> char = InlineT.cast
    val ctoi : char -> int = InlineT.cast
*)
    val itoc = chr
    val ctoi = ord

    type char = char

    val chr = chr
    val ord = ord
    val minChar : char	= chr 0
    val maxOrd		= 255
    val maxChar	: char	= chr maxOrd


    fun pred (c : char) : char = let
	  val c' = (ctoi c - 1)
	  in
	    if (c' < 0) then raise General.Chr else (itoc c')
	  end
    fun succ (c : char) : char = let
	  val c' = (ctoi c + 1)
	  in
	    if (maxOrd < c') then raise General.Chr else (itoc c')
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
	      val ca = array(maxOrd+1,#"\000")
	      fun ins i = if ult(i,sLen)
			      then (unsafe_update (ca, ord'(unsafe_vsub(s, i)), #"\001");
				    ins(uplus(i,0w1)))
			  else ()
	      val _ = ins 0w0
	  in  ca
	  end
    in
	fun contains "" = (fn c => false)
	  | contains s = 
	    let val sLen = size s
	    in
		if (sLen = 1)
		    then let val c' = unsafe_vsub(s, 0w0)
			 in fn c => (c = c') end
		else let val cv = mkArray (s, sLen)
		     in fn c => (unsafe_sub(cv, ord' c) <> #"\000") 
		     end
	    end
	fun notContains "" = (fn c => true)
	  | notContains s = 
	    let val sLen = size s
	    in
		if (sLen = 1)
		    then let val c' = unsafe_vsub(s,0w0)
			 in fn c => (c <> c') end
		else let val cv = mkArray (s, sLen)
		     in fn c => (unsafe_sub(cv, ord' c) = #"\000") 
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
	  val m = int32touint32(ord(vsub(ctypeTbl, ord c)))
	  in  (m && s) <> 0w0
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
    fun scan getc rep = let
	  fun get2 rep = (case (getc rep)
		 of (SOME(c1, rep')) => (case (getc rep')
		       of (SOME(c2, rep'')) => SOME(c1, c2, rep'')
			| _ => NONE
		      (* end case *))
		  | _ => NONE
		(* end case *))
	  in
	    case (getc rep)
	     of NONE => NONE
	      | (SOME(#"\\", rep')) => (case (getc rep')
		   of NONE => NONE
		    | (SOME(#"\\", rep'')) => (SOME(#"\\", rep''))
		    | (SOME(#"\"", rep'')) => (SOME(#"\"", rep''))
		    | (SOME(#"n", rep'')) => (SOME(#"\n", rep''))
		    | (SOME(#"t", rep'')) => (SOME(#"\t", rep''))
		    | (SOME(#"^", rep'')) => (case (getc rep'')
			 of NONE => NONE
	    		  | (SOME(c, rep''')) =>
			      if ((#"@" <= c) andalso (c <= #"_"))
			        then SOME(chr(ord c - ord #"@"), rep''')
			        else NONE
			(* end case *))
		    | (SOME(d1, rep'')) => if (isDigit d1)
			then (case (get2 rep'')
			   of SOME(d2, d3, rep''') => let
				fun cvt d = (ord d - ord #"0")
				in
				  if (isDigit d2 andalso isDigit d3)
				    then SOME(
				      chr(100*(cvt d1)+10*(cvt d2)+(cvt d3)),
				      rep''')
				    else NONE
			        end
			    | NONE => NONE
			  (* end case *))
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
(** NOTE: we should probably recognize the control characters **)
	    else let
	      val c' = ord c
	      in
		if (c > chr 32)
		  then PreString.concat2("\\", itoa c')
		  else PreString.concat2("\\^",
		    unsafe_vsub (PreString.chars, int32touint32(c'+64)))
	      end

    fun fromCString s = raise LibFail "Char.fromCString not implemented"

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
      | toCString c = if (isPrint c)
	  then unsafe_vsub (PreString.chars, int32touint32(ord c))
	  else PreString.concat2("\\", itoa (ord c))


  end (* Char *)
