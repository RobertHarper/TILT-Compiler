(*$import Prelude Nil *)
signature OPTIMIZE = 
  sig
      val debug : bool ref
      val optimize :  {doDead : bool,
		       doProjection : int option,  
		       doUncurry : bool,
		       doCse : bool} -> Nil.module -> Nil.module
      val generate : unit -> {sub : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      update : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      array : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      len : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      vsub : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      vector : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      vlen : Nil.label * Nil.var * Nil.niltrace * Nil.con,
			      bnds : Nil.bnd list}
  end