(*$import PLATFORM Util String OS *)

structure Platform :> PLATFORM = 

struct

    val error = fn s => Util.error "platform.sml" s
    datatype platform = NT | DUNIX | SOLARIS | LINUX
	
    val platform = NT

    fun spinMilli start 0 = ()
      | spinMilli start n = 
	let fun spin 0 = ()
	      | spin n = spin (n-1)
	in spin start; spinMilli start (n-1)
	end
    val spinMilliNT = spinMilli 50000      (* timed for baked *)

    fun sleep duration = spinMilliNT (1 + Real.floor(duration * 1000.0))

    fun pid() = 
	let val time = Time.toReal(Time.now())
	    val floor = Real.realFloor time
	    val pid = Word32.fromInt(Real.floor((time - floor) * 10000.0))
	in  (print "Cannot obtain pid on NT so we use first 4 decimal digits of time.";
	     print "Chose "; 
	     print (Int.toString (Word32.toInt pid)); 
	     print ".\n";
	     pid)
	end
end
