(* General optimizations and generation of polymorphic aggregate handling functions *)

signature OPTIMIZE =
  sig
      val debug : bool ref
      (* Print debug information *)

      val optimize :  {doDead : bool,
		       doProjection : int option,
		       doUncurry : bool,
		       doCse : bool} -> Nil.module -> Nil.module
      (* Perform optimizations listed in the comment at the top of optimize.sml as always being performed, as well
       * as performing dead code elimination (doDead), replacement of known projections for small values (doProjection),
       * uncurrying (doUncurry), or common subexpression elimination (doCse) based on input parameters.
       *)

      val generate : unit -> {sub : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      update : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      array : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      len : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      vsub : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      vector : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      vlen : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      bnds : Nil.bnd list}
      (* Generate import information and bindings with code for polymorphic aggregate handling functions *)
  end
