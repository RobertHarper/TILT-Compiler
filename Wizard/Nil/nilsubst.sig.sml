(*Code that is specific to the Nil substitutions *)
signature NILSUBST = 
  sig
    type exp = Nil.exp
    type con = Nil.con
    type niltrace = Nil.niltrace
    type kind = Nil.kind
    type bnd = Nil.bnd
    type conbnd = Nil.conbnd
    type var = Nil.var
    type module = Nil.module

    type con_subst
    type exp_subst

    structure C : SUBST where type item = con
			  and type item_subst = con_subst
    structure E : SUBST where type item = exp
			  and type item_subst = exp_subst
      

    val substConInExp : con_subst -> exp -> exp
    val substConInCon : con_subst -> con -> con
    val substConInKind : con_subst -> kind -> kind
    val substConInBnd : con_subst -> bnd -> bnd
    val substConInCBnd : con_subst -> conbnd -> conbnd
    val substConInTrace : con_subst -> niltrace -> niltrace
    val substExpInExp : exp_subst -> exp -> exp
    val substExpInCon : exp_subst -> con -> con
    val substExpInKind : exp_subst -> kind -> kind
    val substExpConInExp : (exp_subst * con_subst) -> exp -> exp
    val substExpConInCon : (exp_subst * con_subst) -> con -> con
    val substExpConInKind : (exp_subst * con_subst) -> kind -> kind

    val varConKindSubst : var -> con -> kind -> kind
    val varConConSubst : var -> con -> con -> con
    val varConExpSubst : var -> con -> exp -> exp

    val varExpExpSubst : var -> exp -> exp -> exp
    val varExpConSubst : var -> exp -> con -> con
    val varExpKindSubst : var -> exp -> kind -> kind

  end
