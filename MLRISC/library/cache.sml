signature CACHE_REF =
sig

   type 'a cache 

   val cache : ('a -> 'b) -> 'a -> 'b cache
   val flush : 'a cache -> unit
   val !!    : 'a cache -> 'a

end

structure CacheRef :> CACHE_REF =
struct

   type 'a cache = 'a option ref * (unit -> 'a)

   fun cache f x = (ref NONE, fn _ => f x)

   fun flush (x as ref(SOME _),_) = x := NONE
     | flush (x,_) = ()

   fun !! (r as ref NONE,f)    = let val x = f() in r := SOME x; x end
     | !! (r as ref(SOME x),f) = x

end 

(*
 * $Log$
# Revision 1.1  99/02/17  21:16:54  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:37  pscheng
# *** empty log message ***
#
 *)
