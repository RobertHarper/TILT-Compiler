(*
	The SML/NJ library IntInf is not the same as the basis library
	IntInf.  We do not provide a standard IntInf.  -dave
*)
(*
 * NJ rejects this as well.  I believe that they and we are both
 * incorrect to reject ~ and +, etc. at type IntInf.int. The Basis 
 * documentation specifies that these should be overloaded to include 
 * IntInf.int.
 *
 *   val + : num * num -> num int * int -> int
 *   val ~ : realint -> realint int -> int
 *
 * where
 *   int := {Int.int, IntN.int, IntInf.int, Position.int} 
 *   word := {Word.word, Word8.word, WordN.word, SysWord.word} 
 *   real := {Real.real, RealN.real}
 *   realint := real union int 
 *   num := word union int union real
 *
 *  I'm not sure what the overloaded status of integer constants should be.
 *
 * -leaf
 *)

val big : IntInf.int = 0x80000000

fun try (barg: IntInf.int): unit =
       let val small = SOME (IntInf.toInt barg)
		  handle Overflow => NONE
	   val bstr = IntInf.toString barg
	   fun fail msg = print ("Fail " ^ msg ^ ": " ^ bstr ^ "\n")
	   val isSmall = ~ big <= barg andalso barg < big
       in case small of
	     NONE => if isSmall
			then fail "1"
			else ()
	     | SOME sarg => if isSmall
			       then let val sstr = Int.toString sarg
				    in if bstr = sstr
				       andalso barg = IntInf.fromInt sarg
					  then ()
					  else fail "2"
				    end
			       else fail "3"
       end

fun spin (low: IntInf.int, limit: IntInf.int): unit =
       let fun loop (arg: IntInf.int): unit =
		  if arg = limit
		     then ()
		     else (
			try arg;
			try (~ arg);
			loop (arg + 1)
		     )
       in loop low
       end

val _ = spin (0, 1000)
val _ = spin (big - 1000, big + 1000)

val _ = print "All ok\n"
