signature TONIL =
sig
    structure Il : IL
    structure Nil : NIL

    val xcompunit : Il.context -> 
                    (Name.var * Name.var) Name.VarMap.map ->
                    Il.sbnds ->
                    {cu_bnds : Nil.bnd list,
		     vmap : (Name.var * Name.var) Name.VarMap.map}

    val debug : bool ref
    val elaborator_specific_optimizations : bool ref

end
