structure PreInt =
    struct
	fun imod(a : int, b : int) =
	    let val temp = TiltPrim.irem(a,b)
	    in if ((b>0 andalso temp>=0) orelse
		   (b<0 andalso temp<=0))
		   then temp
	       else temp+b
	    end

	fun idiv(a : int, b : int) =
	    let val temp = TiltPrim.iquot(a,b)
	    in  (* same if sign of a and b agree *)
		if ((a>=0 andalso b>0) orelse (a<=0 andalso b<0))
		    then temp
		else
		    if (b * temp = a)   (* same if exact div *)
			then temp
		    else temp - 1       (* here's where they differ *)
	    end

	(* Note ineg includes overflow check. *)
	fun iabs (a : int) : int = if TiltPrim.igt (a, 0) then a else TiltPrim.ineg a

    end

structure PreLargeInt =
    struct
	type int = int
    end
