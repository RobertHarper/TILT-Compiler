signature SUSPENSION =
sig
   type 'a susp
   val $$ : (unit -> 'a) -> 'a susp
   val !! : 'a susp -> 'a
end

structure Suspension :> SUSPENSION =
struct
   datatype 'a thunk = VALUE of 'a | CLOSURE of unit -> 'a
   type 'a susp = 'a thunk ref 

   fun $$ e = ref(CLOSURE e)
   fun !! (ref (VALUE v)) = v
     | !! (r as ref(CLOSURE e)) = 
       let val v = e()
       in  r := VALUE v; v end  
end

(* 
 * $Log$
# Revision 1.1  99/02/17  21:17:13  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:06  pscheng
# *** empty log message ***
#
 *)
