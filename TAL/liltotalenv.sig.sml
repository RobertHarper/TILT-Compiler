signature LILTOTALENV = 
  sig
    type env

    val empty : unit -> env

    val get_ms_return : env -> Lil.con option
    val get_tvars : env -> Lil.var list

    val set_ms_return : env -> Lil.con -> env

    val enter_handler : env -> env
    val in_handler : env -> bool

    val typeof_op32 : env -> Lil.op32 -> Lil.con
    val typeof_sv32 : env -> Lil.sv32 -> Lil.con
    val typeof_code : env -> Lil.function -> Lil.con

    val find_cvar  : env * Lil.var -> Lil.kind
    val find_var32 : env * Lil.var -> Lil.con
    val find_var64 : env * Lil.var -> Lil.con

    val find_talvar : env * Lil.var -> Tal.con option
    val find_talvar32 : (env -> Lil.con -> Tal.con) -> env * Lil.var -> Tal.con
    val find_talvar64 : (env -> Lil.con -> Tal.con) -> env * Lil.var -> Tal.con
    val bind_talvar : env * (Lil.var * (env -> Tal.con)) -> env

    val bind_cvar : env * (Lil.var * Lil.kind) -> env
    val bind_cvars : env * (Lil.var * Lil.kind) list -> env
    val add_tvar  : env -> Lil.var -> env
    val add_tvars : env -> Lil.var list -> env
    val bind_kvar : env * Lil.var * LilContext.parity -> env
    val bind_bnd : env -> Lil.bnd -> env

    val bind_var32 : env * (Lil.var * Lil.con) -> env
    val bind_var64 : env * (Lil.var * Lil.con) -> env
    val bind_vars32 : env * (Lil.var * Lil.con) list -> env
    val bind_vars64 : env * (Lil.var * Lil.con) list -> env
    val bind_label : env * (Lil.label * Lil.con) -> env

    val kindof : env -> Lil.con -> Lil.kind

    val subst_con : env -> Lil.con -> Lil.con

    val vartrans : env -> Lil.var -> Tal.label
    val var2con : env -> Lil.var -> Tal.con
    val label_var : env -> Lil.var -> Tal.label -> env
  end