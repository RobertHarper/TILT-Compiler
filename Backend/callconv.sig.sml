signature CALLCONV =
sig
   structure Machine : MACHINE


   (* calling conventions for
	   * C --- standard but limited to 6 args
	   * unknown ML functions --- standard
	   * known ML function functions -- not necessarily standard
    *)

   datatype formals = FORMALS of {args : Machine.register list,
				  results : Machine.register list}

   val std_c : formals -> Machine.linkage
   val unknown_ml : formals -> Machine.linkage
end

