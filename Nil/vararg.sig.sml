(* Converting functions with either statically and dynamically known parameter types such that they take a single record
 * argument to use multiple arguments, as well as generating code to apply vararg/onearg at runtime
 *)

signature VARARG =
  sig
      val debug : bool ref
      (* Print debugging information *)

      val flattenThreshold : int
      (* Maximum number of components in a function's single record argument such that the function should still be flattened *)

      val optimize : Nil.module -> Nil.module
      (* Flatten functions and change applications where appropriate *)

      val generate : unit -> {vararg : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      onearg : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      bnds : Nil.bnd list}
      (* Generate import information and bindings with code for runtime vararg/onearg application *)

      val reduce_vararg : NilContext.context * Nil.openness * Nil.effect * Nil.con * Nil.con * Nil.exp -> Nil.exp option
      val reduce_onearg : NilContext.context * Nil.openness * Nil.effect * Nil.con * Nil.con * Nil.exp -> Nil.exp option

  end
