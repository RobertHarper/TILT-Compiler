(*$import NIL *)
signature NILCONTEXT = 
  sig
    structure Nil : NIL
      
    type kind = Nil.kind
    type con = Nil.con
    type var = Nil.var
    type 'a subst
    type context

    val empty : context
    exception Unbound

    (*Insert a type into a context: the constructor is not necessarily normalized *)
    val insert_con : context * var * con -> context
    val remove_con : context * var -> context
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
    val insert_kind_list : context* (var * kind) list -> context


    (* bind_kind (context,var,kind) => (context',var',subst)
      * - inserts the (possibly previously bound) variable "var" into the context
      * with kind selfify(Var_c var,kind)
      * PRE: kind is normalized
      * POST: substitute subst var => var'
      *       find_kind(context',var') => selfify(Var_c var',kind)
      *       find_kind(context',var') => con_normalize(pull(Var_c var',kind))
      *)
    val bind_kind : context * var * kind -> context * var * con subst
    val bind_kind_list : context * (var * kind) list -> context * (var * kind) list * con subst

    (*find_kind_project returns the kind, and the most precise constructor
     * that can be created based on the transparent information in the
     * kind.  Note that in the case of transparency, SOME ... will be returned.
     * A NONE denotes the path is at least partially opaque.
     *)
    val find_kind          : context * var -> kind
    val remove_kind        : context * var -> context
    val find_kind_equation : context * con -> con option


    (* Useful debuggin routines *)
    val print_context : context -> unit
    val print_kinds : context -> unit
    val print_cons : context -> unit

  end 
