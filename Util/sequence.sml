(*$import Util SEQUENCE Listops List *)

structure Sequence :> SEQUENCE =
struct

    val error = fn s => Util.error "sequence.sml" s
 
    type ('a,'b) sequence = ('a*'b) list
    val length = length
    fun fromList x = x
    fun toList x = x
    val foldl = foldl
    val foldr = foldr
    val foldl_acc = Listops.foldl_acc
    val map = map
    val maptolist = map
    val map2 = Listops.map2
    val map_second = Listops.map_second
    val mapcount = Listops.mapcount
    val app = app
    val app_second = fn f => app (fn (a,b) => f b)
    val all = List.all
    val all2 = Listops.all2
    val no_dups = Listops.no_dups

    fun lookup pred set key = 
	let fun loop [] = NONE
	      | loop ((a,b)::rest) = if (pred(a,key)) then SOME b else loop rest
	in loop set
	end

    fun find pred seq  = 
      let 
	fun loop [] = NONE
	  | loop ((a,b)::rest) = if (pred a) then SOME b else loop rest
      in loop seq
      end
    
end
