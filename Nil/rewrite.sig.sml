(*$import Prelude Nil *)

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

		  conhandler   : 'state * Nil.con -> ('state * Nil.con) changeopt,
		  exphandler   : 'state * Nil.exp -> ('state * Nil.exp) changeopt,
		  kindhandler  : 'state * Nil.kind -> ('state * Nil.kind) changeopt,
		  tracehandler : 'state * Nil.niltrace -> ('state * Nil.niltrace) changeopt,
		  con_var_bind   : 'state * Nil.var * Nil.kind -> ('state * Nil.var option),
		  con_var_define : 'state * Nil.var * Nil.con -> ('state * Nil.var option),
		  exp_var_bind   : 'state * Nil.var * Nil.con -> ('state * Nil.var option),
		  exp_var_define : 'state * Nil.var * Nil.exp -> ('state * Nil.var option),
		  sum_var_bind   : 'state * Nil.var * (Nil.con * Nil.w32) -> ('state * Nil.var option)
		  }

    val rewriters : 'state handler -> {
				       rewrite_kind : 'state -> Nil.kind -> Nil.kind,
				       rewrite_con :  'state -> Nil.con -> Nil.con,
				       rewrite_exp :  'state -> Nil.exp -> Nil.exp,
				       rewrite_bnd :  'state -> Nil.bnd -> (Nil.bnd list * 'state),
				       rewrite_cbnd :  'state -> Nil.conbnd -> (Nil.conbnd list * 'state),
				       rewrite_trace : 'state -> Nil.niltrace -> Nil.niltrace,
				       rewrite_mod : 'state -> Nil.module -> Nil.module
				       }
    val null_handler        : 'state * 'a -> ('state * 'b) changeopt

    val null_binder         : 'state * Nil.var * 'a -> ('state * Nil.var option)

    val default_handler     : 'state handler
    val set_kindhandler     : 'state handler -> ('state * Nil.kind -> ('state * Nil.kind) changeopt) -> 'state handler
    val set_conhandler      : 'state handler -> ('state * Nil.con  -> ('state * Nil.con) changeopt)  -> 'state handler
    val set_exphandler      : 'state handler -> ('state * Nil.exp  -> ('state * Nil.exp) changeopt)  -> 'state handler

    val set_con_binder      : 'state handler -> ('state * Nil.var * Nil.kind -> ('state * Nil.var option)) -> 'state handler
    val set_con_definer     : 'state handler -> ('state * Nil.var * Nil.con -> ('state * Nil.var option)) -> 'state handler

    val set_exp_binder      : 'state handler -> ('state * Nil.var * Nil.con -> ('state * Nil.var option)) -> 'state handler
    val set_exp_definer     : 'state handler -> ('state * Nil.var * Nil.exp -> ('state * Nil.var option)) -> 'state handler

    val set_sum_binder      : 'state handler -> ('state * Nil.var * (Nil.con * Nil.w32) -> ('state * Nil.var option)) -> 'state handler

  end
