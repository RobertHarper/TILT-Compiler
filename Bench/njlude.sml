(* ------ Looping constructs ------ *)
fun stride(start,stop,stride,f) = 
    let fun loop cur = if (cur <= stop) then (f cur; loop (cur+stride : int)) else ()
    in  loop start
    end

fun for(start,stop,f) = stride(start,stop,1,f)
val strideP = stride
val forP = for
val mapP = map
val filterP = List.filter


fun time f arg = ....
