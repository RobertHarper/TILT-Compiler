(*$import UTIL_ERROR TextIO *)

(*Pulled out of Util so that Platform can use Error while Util uses Platform.*)
structure UtilError :> UTIL_ERROR = 
struct
    exception BUG of string

    val showErrors = ref true
	
    (* raise_error : string -> 'a *)
    fun raise_error s = let in
	                   if !showErrors
			       then (print "Error: "; print s; print "\n")
			   else ();
			   raise BUG s
			end
	
    (* error : string -> string -> 'a *)
    fun error filename str = raise_error (filename ^ ": " ^ str)
end
