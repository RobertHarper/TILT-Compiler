signature LILTOTALENV = 
  sig
    type env

    val empty : unit -> env

    val get_ms_return : env -> Tal.con option
    val get_tvars : env -> Tal.identifier list

    val set_ms_return : env -> Tal.con -> env


    val typeof_op32 : env -> Lil.op32 -> Lil.con
    val typeof_sv32 : env -> Lil.sv32 -> Lil.con
    val typeof_code : env -> Lil.function -> Lil.con

    val find_var32 : env * Lil.var -> Lil.con
    val find_var64 : env * Lil.var -> Lil.con

    val bind_cvar : env * (Lil.var * Lil.kind) -> env
    val bind_cvars : env * (Lil.var * Lil.kind) list -> env
    val bind_kvar : env * Lil.var * LilContext.parity -> env
    val bind_bnd : env -> Lil.bnd -> env

    val bind_var32 : env * (Lil.var * Lil.con) -> env
    val bind_var64 : env * (Lil.var * Lil.con) -> env
    val bind_vars32 : env * (Lil.var * Lil.con) list -> env
    val bind_vars64 : env * (Lil.var * Lil.con) list -> env
    val bind_label : env * (Lil.label * Lil.con) -> env

    val kindof : env -> Lil.con -> Lil.kind

    val subst_con : env -> Lil.con -> Lil.con

  end