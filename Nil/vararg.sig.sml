(*$import Nil *)

signature VARARG = 
  sig
      val flattenThreshold : int
      val optimize : Nil.module -> Nil.module
      val generate : unit -> {vararg : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      onearg : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      bnds : Nil.bnd list}
  end
