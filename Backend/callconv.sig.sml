(*$import  MACHINE *)

signature CALLCONV =
sig
   structure Machine : MACHINE


   (* standard calling conventions for
	   * C --- but limited to 6 args
	   * unknown ML functions
	   * known ML function functions
    *)

   datatype actuals = ACTUALS of {args : Machine.assign list,
				  results : Machine.assign list}

   datatype formals = FORMALS of {args : Machine.register list,
				  results : Machine.register list}

   (* bool = true if this is the calling convention
      for a function definition   bool = false if this
      is the calling convention for a call site.
 
      The two are not the same because the stack location
      of arguments differs.   For a function definition,
      arguments which are on the stack are found on the
      caller's frame.   For a function call, arguments
      to be placed on the stack are placed in the current
      frame.*)
 
   val std_c : bool -> formals -> actuals
   val unknown_ml : bool -> formals -> actuals
end

