(*  Compute the 100th Fibonacci number.
*)

structure Arithmetic :> RUN = 
  struct
  fun fib 0 = 1
    | fib 1 = 1
    | fib n = (fib(n-1)) + (fib(n-2))

  fun fastfib x = 
    let
      fun loop x x0 x1 = 
	if x = 0 then x0
	else loop (x-1) (x1) (x0 + x1)
    in loop x 1 1
    end

  fun fact x = if x = 0
		 then 1 
	       else x * fact (x -1)
    
  fun run () = 
    let
      val x = fib 40
      val _ = print "This should be 63245986: "
      val _ = print (Int.toString x)
      val _ = print "\n"
      val x = fastfib 40
      val _ = print "This should be 63245986: "
      val _ = print (Int.toString x)
      val _ = print "\n"
      val x = fact 12
      val _ = print "This should be 479001600: "
      val _ = print (Int.toString x)
      val _ = print "\n"
    in ()
    end
end
