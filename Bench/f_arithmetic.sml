(*  
     Compute an approximation (2000th iterate)
       using the arc-tangent expansion. (McLauren series)
*)

structure F_Arithmetic :> RUN = 
  struct
    
   fun computePi steps = 
    let 
      fun loop (0,acc,_) = acc
	| loop (steps,acc,n) = 
	let val acc' = acc + (1.0 / n) - (1.0 / (n+2.0))
	in  loop (steps-1,acc',n+4.0)
	end
    in  4.0 * loop (steps,0.0,1.0)
    end
  

  fun run () = 
    let
      val y = computePi 100000000
      val _ = print "This should be an approximation of pi: "
      val _ = print (Real.toString y)
      val _ = print "\n"
    in ()
    end
end
