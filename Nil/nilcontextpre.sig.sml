(*$import Nil *)
signature NILCONTEXTPRE = 
  sig
      
    type kind = Nil.kind
    type con = Nil.con
    type exp = Nil.exp
    type var = Nil.var
    type context

    val empty : unit -> context
    exception Unbound

    (*Insert a type into a context: the constructor is not necessarily normalized *)
    val insert_con : context * var * con -> context

    (*Given a function to synthesize the type, 
     * take a context and an expression and insert the corresponding type
     * (delayed and memoized)
     *)
    val insert_exp_pre : ((context * exp) -> con) -> context * var * exp -> context

    val find_con   : context * var -> con
    val find_std_con_pre : (context * con -> con) -> context * var -> con

    val insert_con_list : context * (var * con) list -> context

    (* insert_kind (context,var,kind) => context'
      * - inserts the previously unbound variable "var" into the context
      * with kind selfify(Var_c var,kind)
      * PRE: var is not bound in context
      *    : kind is normalized.
      * POST: var bound in context at kind selfify(Var_c var,kind).
      *
      *    If var is already bound, raises an exception
      *)
    val insert_kind : context * var * kind -> context

    val insert_kind_equation : context * var * con * kind -> context
    val insert_equation : context * var * con -> context
    val insert_kind_list : context* (var * kind) list -> context



    val find_kind          : context * var -> kind

    val find_kind_equation : context * con -> con option

    (* Return the standard kind of the variable
     *)
    val find_std_kind      : context * var -> kind

    (* Return the standard, selfified kind of the variable
     *)
    val find_max_kind      : context * var -> kind

    val kind_standardize : context * kind -> kind

    (* Return the most precise standard kind for the constructor
     *)
    val kind_of : context * con -> kind

    (* Useful debuggin routines *)
    val print_context : context -> unit
    val print_kinds : context -> unit
    val print_cons : context -> unit

    val is_well_formed : 
      (context * Nil.kind -> unit) 
      * (context * Nil.con -> Nil.kind)
      * (context * Nil.kind * Nil.kind -> bool) -> context -> bool

    (*These functions return true if all binding sites in the item are distinct,
     * and also are not already bound in the context.
     *)
    val isRenamedExp : context -> Nil.exp -> bool
    val isRenamedCon : context -> Nil.con -> bool
    val isRenamedKind : context -> Nil.kind -> bool

    val exp_error_context  : context * exp  -> context
    val con_error_context  : context * con  -> context
    val kind_error_context : context * kind -> context

    val exps_error_context  : context * exp list  -> context
    val cons_error_context  : context * con list  -> context
    val kinds_error_context : context * kind list -> context
  end 
