signature TONIL =
sig
    structure Il : IL
    structure Nil : NIL

    val xcompunit : Il.context -> 
                    (Name.var, Name.var * Name.var) Name.varmap ->
                    Il.sbnds ->
                    {cu_bnds : Nil.bnd list,
		     vmap : (Name.var, Name.var * Name.var) Name.varmap}

end
