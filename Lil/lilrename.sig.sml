(*$import Name Lil Alpha *)
signature LILRENAME = 
  sig
    type exp = Lil.exp
    type sv32 = Lil.sv32
    type sv64 = Lil.sv64
    type con = Lil.con
    type kind = Lil.kind
    type bnd = Lil.bnd

    type var = Lil.var

    type 'a map = 'a Name.VarMap.map

    (*Just rename the expression variable bindings*)
    val renameEVarsExp : exp -> exp
      
    (*Just rename the constructor level bindings*)
    val renameCVarsCon : con -> con

    (*Just rename the constructor level bindings*)
    val renameKVarsKind : kind -> kind
      
    (*Rename all the bindings*)
    val renameExp : exp -> exp
    val renameSv32 : sv32 -> sv32
    val renameSv64 : sv64 -> sv64
    val renameCon : con -> con
    val renameKind : kind -> kind
    val renameBnd : bnd -> (bnd * (var map * var map * var map))


    (* These functions check whether or not the given item is
     * renamed in the sense that all binding occurrences in the
     * item are distinct
     *)
    val isRenamedExp  : exp -> bool
    val isRenamedCon  : con -> bool
    val isRenamedKind : kind -> bool

    (* These functions check as above, but also check that 
     * no bound binding occurrence fails the predicate.  So
     * for example, by passing in functions that look in a context
     * for an occurrence of a variable, it is possible to check whether or 
     * not an item is renamed in the sense that all binding occurrences are distinct,
     * and no variable is bound which also occurs in the context
     *
     * isRenamedXXXWRT (exp_var_pred,con_var_pred,kind_var_pred) item
     *)
    val isRenamedExpWRT  : ((var -> bool) * (var -> bool) * (var -> bool)) -> exp -> bool
    val isRenamedConWRT  : ((var -> bool) * (var -> bool) * (var -> bool)) -> con -> bool
    val isRenamedKindWRT : ((var -> bool) * (var -> bool) * (var -> bool)) -> kind -> bool

  end
