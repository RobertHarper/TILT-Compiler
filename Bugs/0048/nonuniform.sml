(*$import *)

(* The compiler should reject this gracefully with an error message
   about non-uniform recursive types. *)

datatype 'a foo = A of bar 
     and bar = B of int foo

(* Currently the compiler fails with confusing error messages:
	nonuniform.sml:3.24-3.27 Error:           type constructor bar wants 1 arguments, given 0
	tilt: equal.sml: elaborator impossibility: unresolved type does not permit equailty
*)
