(*$import WNil *)

(*The signature of the true implementation of contexts.  In order to 
 * eliminate cyclic dependencies, the main implementation is paramaterized
 * over a few functions that are filled in later.  Most modules will use the
 * filled in version: structure NilContext :> NILCONTEXT
 *)

signature NILCONTEXTPRE = 
  sig
      
    type kind = Nil.kind
    type con  = Nil.con
    type exp  = Nil.exp
    type var  = Nil.var

    type context

    exception Unbound

    val empty : unit -> context

    (*Insert a type into a context: the constructor is not necessarily normalized *)
    val insert_con : context * var * con -> context

    (*Given a function to synthesize the type, 
     * take a context and an expression and insert the corresponding type
     * (delayed and memoized)
     *)
    val insert_exp_pre  : ((context * exp) -> con) -> context * var * exp -> context

    val insert_con_list : context * (var * con) list -> context

    val find_con        : context * var -> con


    (* insert_kind (context,var,kind) => context'
      * - inserts the previously unbound variable "var" into the context
      * at kind "kind". If var is already bound, raises an exception.
      * Use bound_con and bound_exp to query whether or not a variable 
      * is already bound.
      *)
    val insert_kind          : context * var * kind       -> context
    val insert_kind_equation : context * var * con * kind -> context
    val insert_equation      : context * var * con        -> context
    val insert_kind_list     : context* (var * kind) list -> context

    (*As above, except that the kind is guaranteed not to contain
     * any undecorated Singletons.  This gives substantial speedups,
     * but must be used with care!  
     *)
    val insert_stdkind          : context * var * kind       -> context
    val insert_stdkind_equation : context * var * con * kind -> context

    (* Check whether a variable is already bound.
     *)
    val bound_con : context * var -> bool
    val bound_exp : context * var -> bool

      
    val find_kind          : context * var -> kind
    val find_kind_equation : context * con -> con option

    (* Return the standard kind of the variable
     *)
    val find_std_kind      : context * var -> kind

    (* Return the standard, selfified kind of the variable.
     * Note that this kind is generally much smaller than the standard kind.
     *)
    val find_max_kind      : context * var -> kind

    (* Return an equivalent kind with no undecorated singletons
     *)
    val kind_standardize   : context * kind -> kind

    (* Return the most precise standard kind for the constructor.  
     * This kind will also have no dependent record kinds.
     *)
    val kind_of            : context * con -> kind

    (* Print functions*)
    val print_context : context -> unit
    val print_kinds : context -> unit
    val print_cons : context -> unit

    (* is_well_formed (kind_valid,con_valid,subkind) D 
     * Check whether or not a given context is well-formed.
     *)
    val is_well_formed : 
      (context * Nil.kind -> Nil.kind) 
      * (context * Nil.con -> Nil.kind)
      * (context * Nil.kind * Nil.kind -> bool) -> context -> bool

    (*These functions take an item or a list of items and 
     * return the minimal context necessary to cover all
     * the free variables in the item.  This can make error
     * printouts vastly shorter.
     *)
    val exp_error_context  : context * exp  -> context
    val con_error_context  : context * con  -> context
    val kind_error_context : context * kind -> context

    val exps_error_context  : context * exp list  -> context
    val cons_error_context  : context * con list  -> context
    val kinds_error_context : context * kind list -> context
  end 
