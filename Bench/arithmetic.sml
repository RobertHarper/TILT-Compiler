(*$import Prelude *)
(*  Compute the 100th Fibonacci number.
    Compute an approximation (2000th iterate)
       using the arc-tangent expansion. (McLauren series)
*)

fun fib 0 = 1
  | fib 1 = 1
  | fib n = (fib(n-1)) + (fib(n-2))

fun computePi steps = 
    let 
	fun loop (0,acc,_) = acc
	  | loop (steps,acc,n) = 
	    let val acc' = acc + (1.0 / n) - (1.0 / (n+2.0))
	    in  loop (steps-1,acc',n+4.0)
	    end
    in  4.0 * loop (steps,0.0,1.0)
    end

val x = fib 10
val _ = print (Int.toString x)
val _ = print "\n"
val y = computePi 10000
val _ = print (Real.toString y)
val _ = print "\n"
