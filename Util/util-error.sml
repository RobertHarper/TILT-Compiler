(*
	I think we should do away with showErrors and dontShow and
	move the printing out of raise_error and into the top-level
	exception handler in main.
*)
structure UtilError :> UTIL_ERROR =
struct
    exception BUG of string

    val showErrors = ref true

    fun dontShow (f : 'a -> 'b) (x : 'a) : 'b =
	let val old = !showErrors
	    val _ = showErrors := false
	in  (f x before (showErrors := old)
	     handle e => (showErrors := old; raise e))
	end

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
