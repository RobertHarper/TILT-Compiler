signature NORMALIZE = 
  sig
    type kind
    type con
    type exp
    type module
    type context 
    type 'a subst

    val debug : bool ref
    val show_calls : bool ref

    val get_shape : context -> con -> kind

    val kind_normalize : context -> kind -> kind
    val con_normalize : context -> con -> con
    val exp_normalize : context -> exp -> exp
    val module_normalize : context -> module -> module

    val kind_normalize' : (context * (con subst)) -> kind -> kind
    val con_normalize' : (context * (con subst)) -> con -> con
    val exp_normalize' : (context * (con subst)) -> exp -> exp

    val beta_conrecord : con -> con
    val beta_confun : context -> con -> con
    val eta_conrecord : context -> con -> con
    val eta_confun : con -> con
    val beta_typecase : context -> con -> con
  end
