(*$import Nil *)

signature NILREWRITE = 
  sig
    datatype 'a changeopt = NOCHANGE | NORECURSE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a
      
    datatype 'state handler =
      HANDLER of {
		  bndhandler : 'state * Nil.bnd -> ('state * Nil.bnd list) changeopt,
		  cbndhandler : 'state * Nil.conbnd -> ('state * Nil.conbnd list) changeopt,
		  (*For cbnds, a return of CHANGE_NORECURSE (state,cbnds)
		   * will result in cbnds being bound in state before being returned.
		   *)

		  conhandler : 'state * Nil.con -> ('state * Nil.con) changeopt,
		  exphandler : 'state * Nil.exp -> ('state * Nil.exp) changeopt,
		  kindhandler : 'state * Nil.kind -> ('state * Nil.kind) changeopt,
		  con_var_bind : 'state * Nil.var * Nil.kind -> ('state * Nil.var option),
		  con_var_define : 'state * Nil.var * Nil.con -> ('state * Nil.var option),
		  exp_var_bind : 'state * Nil.var * Nil.con -> ('state * Nil.var option),
		  exp_var_define : 'state * Nil.var * Nil.exp -> ('state * Nil.var option)
		  }

    val rewriters : 'state handler -> {
				       rewrite_kind : 'state -> Nil.kind -> Nil.kind,
				       rewrite_con :  'state -> Nil.con -> Nil.con,
				       rewrite_exp :  'state -> Nil.exp -> Nil.exp,
				       rewrite_bnd :  'state -> Nil.bnd -> (Nil.bnd list * 'state),
				       rewrite_cbnd :  'state -> Nil.conbnd -> (Nil.conbnd list * 'state),
				       rewrite_mod : 'state -> Nil.module -> Nil.module
				       }

    val default_handler : 'state handler
    val set_conhandler : 'state handler -> ('state * Nil.con -> ('state * Nil.con) changeopt) -> 'state handler
    val set_exphandler : 'state handler -> ('state * Nil.exp -> ('state * Nil.exp) changeopt) -> 'state handler

    val set_con_binder : 'state handler -> ('state * Nil.var * Nil.kind -> ('state * Nil.var option)) -> 'state handler
    val set_con_definer : 'state handler -> ('state * Nil.var * Nil.con -> ('state * Nil.var option)) -> 'state handler

    val set_exp_binder : 'state handler -> ('state * Nil.var * Nil.con -> ('state * Nil.var option)) -> 'state handler
    val set_exp_definer : 'state handler -> ('state * Nil.var * Nil.exp -> ('state * Nil.var option)) -> 'state handler
  end
