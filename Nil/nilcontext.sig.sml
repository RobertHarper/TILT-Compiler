(*$import Nil *)
signature NILCONTEXT = 
  sig
      
    type kind = Nil.kind
    type con = Nil.con
    type exp = Nil.exp
    type var = Nil.var
    type 'a subst
    type context

    val empty : unit -> context
    exception Unbound

    (*Insert a type into a context: the constructor is not necessarily normalized *)
    val insert_con : context * var * con -> context

    val find_con   : context * var -> con
    val insert_con_list : context * (var * con) list -> context

    (*NOTE: Use of bind_kind over insert_kind is strongly recommended.
     * Due to the dependencies in the context, and the fact that
     * entering code removes elements from the context, relying
     * variable overwriting to handle shadowing correctly is probably
     * not sound.  
     *)

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



    (*find_kind_project returns the kind, and the most precise constructor
     * that can be created based on the transparent information in the
     * kind.  Note that in the case of transparency, SOME ... will be returned.
     * A NONE denotes the path is at least partially opaque.
     *)
    val find_kind          : context * var -> kind

    val find_kind_equation : context * con -> con option

    (* Return the standard kind of the variable
     *)
    val find_std_kind      : context * var -> kind

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
  end 
