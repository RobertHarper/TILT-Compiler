signature HASH_MAP =
sig

   type ('a,'b) map

   val create : { order : 'a * 'a -> order,
                  hash  : 'a -> int,
                  exn   : exn
                } -> int -> ('a,'b) map 

   val size         : ('a,'b) map -> int
   val bucketSize   : ('a,'b) map -> int
   val isEmpty      : ('a,'b) map -> bool
   val insert       : ('a,'b) map -> ('a * 'b) -> unit
   val update       : ('a,'b) map -> (('a * 'b) * ('b -> 'b)) -> unit
   val remove       : ('a,'b) map -> 'a -> unit
   val lookup       : ('a,'b) map -> 'a -> 'b
   val lookupOrElse : ('a,'b) map -> 'b -> 'a -> 'b
   val toList       : ('a,'b) map -> ('a * 'b) list
   val clear        : ('a,'b) map -> unit
   val contains     : ('a,'b) map -> 'a -> bool
   val app          : ('a * 'b -> unit) -> ('a,'b) map -> unit
   val fold         : (('a * 'b) * 'c -> 'c) -> 'c -> ('a,'b) map -> 'c
   val toString     : (('a -> string) * ('b -> string)) -> ('a,'b) map 
                      -> string

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:19  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:17:02  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:52  pscheng
# *** empty log message ***
#
 *)
