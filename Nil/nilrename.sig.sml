(*$import Prelude Name Nil Alpha *)
signature NILRENAME = 
  sig
    type exp = Nil.exp
    type con = Nil.con
    type kind = Nil.kind
    type bnd = Nil.bnd
    type conbnd = Nil.conbnd
    type var = Nil.var
    type module = Nil.module
    type 'a map = 'a Name.VarMap.map

    (*Just rename the expression variable bindings*)
    val renameEVarsExp : exp -> exp
    val renameEVarsCon : con -> con
    val renameEVarsKind : kind -> kind
      
    (*Just rename the constructor level bindings*)
    val renameCVarsExp : exp -> exp
    val renameCVarsCon : con -> con
    val renameCVarsKind : kind -> kind
      
    (*Rename all the bindings*)
    val renameExp : exp -> exp
    val renameCon : con -> con
    val renameKind : kind -> kind
    val renameMod : module -> module
    val renameBnd : bnd -> (bnd * (var map* var map))
    val renameCBnd : conbnd -> (conbnd * var map)
    val renameFunction : Nil.function * Nil.con -> Nil.function

    (*These functions check whether or not the given item is
     * renamed in the sense that all binding occurrences in the
     * item are distinct
     *)
    val isRenamedExp  : exp -> bool
    val isRenamedCon  : con -> bool
    val isRenamedKind : kind -> bool
    val isRenamedMod  : module -> bool

    (*These functions return true if there is no shadowing, otherwise false
     *)
    val noShadowsExp  : exp -> bool
    val noShadowsCon  : con -> bool
    val noShadowsKind : kind -> bool
    val noShadowsMod  : module -> bool

    (* These functions check as above, but also check that 
     * no bound binding occurrence fails the predicate.  So
     * for example, by passing in functions that look in a context
     * for an occurrence of a variable, it is possible to check whether or 
     * not an item is renamed in the sense that all binding occurrences are distinct,
     * and no variable is bound which also occurs in the context
     *
     * isRenamedXXXWRT (exp_var_pred,con_var_pred) item
     *)
    val isRenamedExpWRT  : ((var -> bool) * (var -> bool)) -> exp -> bool
    val isRenamedConWRT  : ((var -> bool) * (var -> bool)) -> con -> bool
    val isRenamedKindWRT : ((var -> bool) * (var -> bool)) -> kind -> bool

    val renameExpWRT   : ((var -> bool) * (var -> bool)) -> exp -> exp
    val renameConWRT   : ((var -> bool) * (var -> bool)) -> con -> con
    val renameKindWRT  : ((var -> bool) * (var -> bool)) -> kind -> kind

    val alphaCRenameExp   : Alpha.alpha_context -> exp -> exp
    val alphaCRenameCon   : Alpha.alpha_context -> con -> con
    val alphaCRenameKind  : Alpha.alpha_context -> kind -> kind
    val alphaECRenameCon  : Alpha.alpha_context * Alpha.alpha_context -> con -> con
    val alphaECRenameKind : Alpha.alpha_context * Alpha.alpha_context -> kind -> kind

  end
