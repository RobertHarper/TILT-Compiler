(*$import NIL *)
signature NILCONTEXT = 
  sig
      
    type kind = Nil.kind
    type con = Nil.con
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
    val insert_shape : context * var * kind -> context
    val insert_kind_shape : context * var * kind * kind -> context
    val insert_kind_shape_equation : context * var * con * kind * kind -> context
    val insert_kind_equation : context * var * con * kind -> context
    val insert_kind_list : context* (var * kind) list -> context

    val make_shape : context * Nil.kind -> Nil.kind

    (*Given a constructor, returns the most imprecise kind for that
     * con - i.e, merely the shape info
     *)
    val shape_of : context * Nil.con -> Nil.kind
    val kind_of : context * Nil.con -> Nil.kind
    val kind_of_bnds : context * Nil.conbnd list -> context

    (*find_kind_project returns the kind, and the most precise constructor
     * that can be created based on the transparent information in the
     * kind.  Note that in the case of transparency, SOME ... will be returned.
     * A NONE denotes the path is at least partially opaque.
     *)
    val find_shape         : context * var -> kind
    val find_kind          : context * var -> kind

    val find_kind_equation : context * con -> con option


    (* Useful debuggin routines *)
    val print_context : context -> unit
    val print_kinds : context -> unit
    val print_cons : context -> unit

  end 
