(*$import TopLevel Util *)

structure Sequence : SEQUENCE =
struct

    val error = fn s => Util.error "sequence.sml" s
 
    type ('a,'b) sequence = ('a*'b) list
    val length = length
    fun fromList x = x
    fun toList x = x
    val foldl = foldl
    val foldl_acc = Listops.foldl_acc
    val map = map
    val map2 = Listops.map2
    val app = app
    val all = List.all

    fun lookup pred set key = 
	let fun loop [] = NONE
	      | loop ((a,b)::rest) = if (pred(a,key)) then SOME b else loop rest
	in loop set
	end

end
