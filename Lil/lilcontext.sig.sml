(*$import Lil *)

signature LILCONTEXT = 
  sig
      
    type context

    exception Unbound of string
    exception Rebound of string

    datatype parity = Pos | Neg | Any

    val empty : unit -> context

    val bind_cvar  : context * (Lil.var * Lil.kind) -> context
    val bind_kvar  : context * Lil.var * parity -> context
    val bind_var32 : context * (Lil.var * Lil.con)  -> context
    val bind_var64 : context * (Lil.var * Lil.con)  -> context
    val bind_label : context * (Lil.label * Lil.con) -> context

    val bind_cvars  : context * (Lil.var * Lil.kind) list -> context
    val bind_kvars  : context * Lil.var list * parity -> context
    val bind_var32s : context * (Lil.var * Lil.con) list -> context
    val bind_var64s : context * (Lil.var * Lil.con) list  -> context
    val bind_labels : context * (Lil.label * Lil.con) list  -> context

    val unfold_cvar : (context * (Lil.var * Lil.var)) -> context * LilSubst.con_subst
    val split_cvar  : (context * (Lil.var * (Lil.var * Lil.var))) -> context * LilSubst.con_subst
    val vcase_cvar  : (context * (Lil.var * Lil.var)) -> (context * LilSubst.con_subst) list

    val find_cvar  : context * Lil.var -> Lil.kind
    val find_kvar  : context * Lil.var -> parity
    val find_var32 : context * Lil.var -> Lil.con
    val find_var64 : context * Lil.var -> Lil.con
    val find_label : context * Lil.label -> Lil.con

    val clear_vars : context -> context 
  end 
