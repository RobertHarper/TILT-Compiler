(*$import *)

(* ------ Looping constructs ------ *)
fun stride(start,stop,stride,f) = 
    let fun loop cur = if (cur <= stop) then (f cur; loop (cur+stride : int)) else ()
    in  loop start
    end

fun for(start,stop,f) = stride(start,stop,1,f)

(* Parallel constructs *)

extern threadID : (unit, int) -->
extern Spawn : (unit -> unit, unit) -->
extern Yield : (unit, unit) -->


(*
val strideP = stride
val forP = for
val mapP = map
val filterP = filter
*)

(* PARALLEL VERSION *)

fun strideP(start,stop,stride,f) =
    let fun loop cur = (if (cur <= stop) 
			   then let 
			            pval _ = (f cur) : unit
                                    and _ = loop (cur+stride : int)
				in  ()
				end
		       else ())
    in  loop start
    end

fun forP(start,stop,f) =
    let 
	val total = stop - start + 1
	val stride = 1 + (total div 10)
	fun loop start' = 
	    let 
		val stop' = start' + (stride - 1)
		val stop' = if (stop' > stop) then stop else stop'
	    in  for(start',stop',f)
	    end
    in	strideP(start,stop,stride,loop)
    end

local
    fun nextblock 0 ls = ls
      | nextblock _ [] = []
      | nextblock n (_::rest) = nextblock (n-1) rest
in
    fun mapP f ls = 
	let val len = length ls
	    val block = 1 + (len div 10)
	    fun mapblock 0 _ = []
	      | mapblock _ [] = []
	      | mapblock n (a::rest) = (f a) :: (mapblock (n-1) rest)
	    fun loop [] = []
	      | loop ls = 
		let pval first = mapblock block ls
		    and rest = loop (nextblock block ls)
		in  first::rest
		end
	in  List.concat (loop ls)
	end

    fun filterP pred ls = 
	let val len = length ls
	    val block = 1 + (len div 20)
	    fun filterblock 0 _ = []
	      | filterblock _ [] = []
	      | filterblock n (a::rest) = let val rest = (filterblock (n-1) rest)
					  in  if (pred a) then a :: rest else rest
					  end
	    fun loop [] = []
	      | loop ls = 
		let pval first = filterblock block ls
		    and rest = loop (nextblock block ls)
		in  first::rest
		end
	in  List.concat (loop ls)
	end
end (* local *)

