signature NORMALIZE = 
  sig
    type kind
    type con
    type exp
    type module
    type context 
    type 'a subst

    val partial_get_kind : context -> con -> kind option
    val get_kind : context -> con -> kind

    val partial_kind_normalize : context -> kind -> kind
    val partial_con_normalize : context -> con -> con
    val partial_exp_normalize : context -> exp -> exp
    val partial_module_normalize : context -> module -> module

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
