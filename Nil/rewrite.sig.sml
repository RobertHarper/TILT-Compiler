(*$import Prelude Nil *)

signature NILREWRITE = 
  sig
    datatype 'a changeopt = NOCHANGE | NORECURSE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a 

    type ('state,'term) termhandler = 'state * 'term -> ('state * 'term) changeopt
    type ('state, 'bnd) bndhandler  = 'state * 'bnd -> ('state * 'bnd list) changeopt
    type 'state          binder     = 'state * Nil.var  -> ('state * Nil.var option)

    datatype 'state handler =
      HANDLER of {
		  bndhandler : ('state, Nil.bnd) bndhandler,
		  cbndhandler : ('state, Nil.conbnd) bndhandler,
		  (*For cbnds, a return of CHANGE_NORECURSE (state,cbnds)
		   * will result in cbnds being bound in state before being returned.
		   *)

		  conhandler   : ('state,Nil.con) termhandler,
		  exphandler   : ('state,Nil.exp) termhandler,
		  kindhandler  : ('state,Nil.kind) termhandler,
		  tracehandler : ('state,Nil.niltrace) termhandler,
		  con_var_bind : 'state binder,
		  exp_var_bind : 'state binder,
		  labelled_var : 'state * Nil.label * Nil.var -> 'state
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

    val null_binder         : 'state binder

    val null_label_binder   : 'state * Nil.label * Nil.var -> 'state

    val default_handler     : 'state handler

    val set_kindhandler     : 'state handler -> ('state, Nil.kind) termhandler -> 'state handler
    val set_conhandler      : 'state handler -> ('state, Nil.con) termhandler  -> 'state handler
    val set_exphandler      : 'state handler -> ('state, Nil.exp) termhandler  -> 'state handler

    val set_con_binder      : 'state handler -> 'state binder -> 'state handler

    val set_exp_binder      : 'state handler -> 'state binder  -> 'state handler

    val set_label_binder    : 'state handler -> ('state * Nil.label * Nil.var -> 'state) -> 'state handler
  end
