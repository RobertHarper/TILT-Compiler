(*
 * signatures NILCONTEXT and NILCONTEXT'
 * In order to break a cycle in the structure dependancies, 
 * NILCONTEXT' defines a structure in which functions which
 * require a normalizer are abstract WRT to the normalizing
 * function.
 * NILCONTEXT is NILCONTEXT' after tieing the normalizer loop
*)

signature NILCONTEXT' = 
  sig
    structure Nil : NIL
      
    type kind = Nil.kind
    type con = Nil.con
    type var = Nil.var
    type 'a subst
    type context

    val empty : unit -> context

    (* This function changes the context to an *internal*
     * context.  All subsequent variable that are simply bound
     * will be forgotten as soon as a code function is entered
     * This is used to enforce "codeness".  leave_top_level should be
     * always be called upon crossing an abstraction boundary
     *)
    val leave_top_level : context -> context

    (*Removes from the context all variables except those bound at the
     * top level, or explicitly bound as "code" variables
     *)
    val code_context : context -> context

    (*Insert a type into a context
     * PRE: Constructor is normalized
     *)
    val insert_con : context*var*con -> context

    (*Insert a type into a context
     * PRE: Constructor is normalized
     * The binding will remain visible even after crossing into a code
     * function
     *)
    val insert_code_con : context*var*con -> context
    val find_con : context*var -> con option
    val remove_con : context*var -> context

    val insert_con_list : context * (var * con) list -> context
    val insert_code_con_list : context * (var * con) list -> context


    (*NOTE: Use of bind_kind over insert_kind is strongly recommended.
     * Due to the dependencies in the context, and the fact that
     * entering code removes elements from the context, relying
     * variable overwriting to handle shadowing correctly is probably
     * not sound.  
     *)

    (* insert_kind normalizer (context,var,kind) => context'
      * - inserts the previously unbound variable "var" into the context
      * with kind selfify(Var_c var,kind)
      * PRE: var is not bound in context
      *    : kind is normalized.
      * POST: var bound in context at kind selfify(Var_c var,kind).
      *
      *    If var is already bound, raises an exception
      *)
    val insert_kind : (context -> con -> con) -> context * var * kind -> context
    val insert_kind_list : (context -> con -> con) -> context* (var * kind) list -> context
    val unpull_convar : context * var -> context

    (* insert_kind normalizer (context,var,kind) => (context',var',subst)
      * - inserts the (possibly previously bound) variable "var" into the context
      * with kind selfify(Var_c var,kind)
      * PRE: kind is normalized
      * POST: substitute subst var => var'
      *       find_kind(context',var') => selfify(Var_c var',kind)
      *       find_kind(context',var') => con_normalize(pull(Var_c var',kind))
      *)
    val bind_kind : (context -> con -> con) -> context * var * kind -> context * var * con subst
    val bind_kind_list : (context -> con -> con) -> context * (var * kind) list -> context * (var * kind) list * con subst

    val find_kind : context*var -> kind option

    (*find_kind' returns the kind, and the most precise constructor
     * that can be created based on the transparent information in the
     * kind.  Note that in the case of total opacity, this will simply
     * be the variable itself.
     *)
    val find_kind' : context*var -> (con*kind) option
    val remove_kind : context*var -> context

    val context_addkindmap : context * context -> context


    val foldli_kind : ((var * kind * 'a) -> 'a) -> 'a -> context -> 'a


    val print_context : context -> unit

    val print_kinds : context -> unit
      
    val print_cons : context -> unit

  end 


signature NILCONTEXT = 
  sig
    structure Nil : NIL
      
    type kind = Nil.kind
    type con = Nil.con
    type var = Nil.var
    type 'a subst
    type context

    val empty : unit -> context

    (* This function changes the context to an *internal*
     * context.  All subsequent variable that are simply bound
     * will be forgotten as soon as a code function is entered
     * This is used to enforce "codeness".  leave_top_level should be
     * always be called upon crossing an abstraction boundary
     *)
    val leave_top_level : context -> context

    (*Removes from the context all variables except those bound at the
     * top level, or explicitly bound as "code" variables
     *)
    val code_context : context -> context

    (*Insert a type into a context
     * PRE: Constructor is normalized
     *)
    val insert_con : context*var*con -> context

    (*Insert a type into a context
     * PRE: Constructor is normalized
     * The binding will remain visible even after crossing into a code
     * function
     *)
    val insert_code_con : context*var*con -> context
    val find_con : context*var -> con option
    val remove_con : context*var -> context

    val insert_con_list : context * (var * con) list -> context
    val insert_code_con_list : context * (var * con) list -> context


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
    val insert_kind_list : context* (var * kind) list -> context

    (* unpull_convar will modify the constructor that is returned to
          the unpulled form (i.e., the variable)  *)
    val unpull_convar : context * var -> context

    (* insert_kind (context,var,kind) => (context',var',subst)
      * - inserts the (possibly previously bound) variable "var" into the context
      * with kind selfify(Var_c var,kind)
      * PRE: kind is normalized
      * POST: substitute subst var => var'
      *       find_kind(context',var') => selfify(Var_c var',kind)
      *       find_kind(context',var') => con_normalize(pull(Var_c var',kind))
      *)
    val bind_kind : context * var * kind -> context * var * con subst
    val bind_kind_list : context * (var * kind) list -> context * (var * kind) list * con subst

    val find_kind : context*var -> kind option

    (*find_kind' returns the kind, and the most precise constructor
     * that can be created based on the transparent information in the
     * kind.  Note that in the case of total opacity, this will simply
     * be the variable itself.
     *)
    val find_kind' : context*var -> (con*kind) option
    val remove_kind : context*var -> context

    val context_addkindmap : context * context -> context


    val foldli_kind : ((var * kind * 'a) -> 'a) -> 'a -> context -> 'a

    val print_context : context -> unit

    val print_kinds : context -> unit
      
    val print_cons : context -> unit

  end 
