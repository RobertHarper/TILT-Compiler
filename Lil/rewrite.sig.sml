(*$import Lil *)

signature LILREWRITE = 
  sig
    datatype 'a changeopt = NOCHANGE | NORECURSE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a 

    type ('state,'term) termhandler = 'state * 'term -> ('state * 'term) changeopt
    type ('state, 'bnd) bndhandler  = 'state * 'bnd -> ('state * 'bnd list) changeopt
    type 'state          binder     = 'state * Lil.var  -> ('state * Lil.var option)

    (* Not currently a handler for ops, but trivial to add if needed.
     *)
    datatype 'state handler =
      HANDLER of {
		  bndhandler : ('state, Lil.bnd) bndhandler,

		  conhandler   : ('state,Lil.con) termhandler,
		  exphandler   : ('state,Lil.exp) termhandler,
		  sv32handler  : ('state,Lil.sv32) termhandler,
		  sv64handler  : ('state,Lil.sv64) termhandler,
		  kindhandler  : ('state,Lil.kind) termhandler,

		  kind_var_bind : 'state binder,
		  con_var_bind  : 'state binder,
		  exp_var32_bind  : 'state binder,
		  exp_var64_bind  : 'state binder
		  }
     
    val rewriters : 'state handler -> {
				       rewrite_kind  : 'state -> Lil.kind -> Lil.kind,
				       rewrite_con   :  'state -> Lil.con -> Lil.con,
				       rewrite_exp   :  'state -> Lil.exp -> Lil.exp,
				       rewrite_sv32  : 'state -> Lil.sv32 -> Lil.sv32,
				       rewrite_sv64  : 'state -> Lil.sv64 -> Lil.sv64,
				       rewrite_op32  : 'state -> Lil.op32 -> Lil.op32,
				       rewrite_op64  : 'state -> Lil.op64 -> Lil.op64,
				       rewrite_bnd   :  'state -> Lil.bnd -> ('state * Lil.bnd list)
				       }

    val null_handler        : 'state * 'a -> ('state * 'b) changeopt

    val null_binder         : 'state binder

    val default_handler     : 'state handler


  end
