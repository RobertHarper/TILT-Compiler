(*$import Stats Time Timer Real Int *)

structure Timestamp :
    sig
	val show : bool ref
	val timestamp : unit -> unit
    end =
struct

    val show = Stats.ff("ShowTimestamps")

    fun cpuTime () : Time.time =
	let val timer = Timer.totalCPUTimer()
	    val {usr, sys, ...} = Timer.checkCPUTimer timer
	in  Time.+(usr, sys)
	end
(*
    fun realTime () : Time.time = 
	let val timer = Timer.totalRealTimer()
	in  Timer.checkRealTimer timer
	end
*)	
    fun showTime (what, f) =
	let val time = f()
	    val time = Time.toReal time
	    val time = Real.floor time
	    val time = Int.toString time
	in  print what; print " "; print time; print " "
	end
	
    fun timestamp () = if (!show)
			   then showTime("CPUTIME",cpuTime)
		       else ()

end
