(*$import Prelude YaccBase *)

(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
# Revision 1.4  2000/11/27  22:37:09  swasey
# *** empty log message ***
# 
# Revision 1.3  98/01/21  20:40:50  pscheng
# moved the .sig files to .sig.sml file
# 
# Revision 1.2  1997/10/24  21:36:30  cstone
# update to 109.32 & NT
#
# Revision 1.2  1997/08/26  19:18:55  jhr
#   Replaced used of "abstraction" with ":>".
#
# Revision 1.1.1.1  1997/01/14  01:38:04  george
#   Version 109.24
#
 * Revision 1.1.1.1  1996/01/31  16:01:43  george
 * Version 109
 * 
 *)

(* Stream: a structure implementing a lazy stream.  The signature STREAM
   is found in base.sig *)

structure Stream :> STREAM =
struct
   datatype 'a str = EVAL of 'a * 'a str ref | UNEVAL of (unit->'a)

   type 'a stream = 'a str ref

   fun get(ref(EVAL t)) = t
     | get(s as ref(UNEVAL f)) = 
	    let val t = (f(), ref(UNEVAL f)) in s := EVAL t; t end

   fun streamify f = ref(UNEVAL f)
   fun cons(a,s) = ref(EVAL(a,s))

end;
