(*
 * signatures NILCONTEXT and NILCONTEXT'
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

    val leave_top_level : context -> context
    val code_context : context -> context

    val insert_con : context*var*con -> context
    val insert_code_con : context*var*con -> context
    val find_con : context*var -> con option
    val remove_con : context*var -> context

    val insert_con_list : context * (var * con) list -> context
    val insert_code_con_list : context * (var * con) list -> context

    (*Note that insertion of a kind automatically selfifies*)
    (* insert_kind (context,var,kind) => ()
      * PRE: var is not bound in context
      * POST: var is bound to selfify(Var_c var,kind) in the context
      *
      * If var is already bound, raises an exception
      *
      * Note that the context should only be used with alpha normalized
      * parse trees, since otherwise scoping will cause
      * an attempt to rebind a variable.
      *)
    val context_addkindmap : context * context -> context
    val insert_kind : (context -> con -> con) -> context * var * kind -> context
    val find_kind : context*var -> kind option
    val find_kind' : context*var -> (con*kind) option
    val remove_kind : context*var -> context
    val insert_kind_list : (context -> con -> con) -> context* (var * kind) list -> context

    val bind_kind : (context -> con -> con) -> context * var * kind -> context * var * con subst
    val bind_kind_list : (context -> con -> con) -> context * (var * kind) list -> context * (var * kind) list * con subst

    val foldli_kind : ((var * kind * 'a) -> 'a) -> 'a -> context -> 'a

    val c_insert_con : context*var*con*(context->'a) -> 'a

    val c_insert_kind : (context -> con -> con) -> context*var*kind*(context->'a) -> 'a

    val c_insert_con_list : context * (var*con) list * (context -> 'a) -> 'a
    val c_insert_kind_list : (context -> con -> con) -> context * (var*kind) list * (context -> 'a) -> 'a

    val print_context : context -> unit

    val print_kinds : context -> unit
      
    val print_cons : context -> unit

    val selfify : (Nil.con * Nil.kind) -> Nil.kind

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

    val leave_top_level : context -> context
    val code_context : context -> context

    val insert_con : context*var*con -> context
    val insert_code_con : context*var*con -> context
    val find_con : context*var -> con option
    val remove_con : context*var -> context

    val insert_con_list : context * (var * con) list -> context
    val insert_code_con_list : context * (var * con) list -> context

    (*Note that insertion of a kind automatically selfifies*)
    (* insert_kind (context,var,kind) => ()
      * PRE: var is not bound in context
      * POST: var is bound to selfify(Var_c var,kind) in the context
      *
      * If var is already bound, raises an exception
      *
      * Note that the context should only be used with alpha normalized
      * parse trees, since otherwise scoping will cause
      * an attempt to rebind a variable.
      *)
    val context_addkindmap : context * context -> context
    val insert_kind : context*var*kind -> context
    val find_kind : context*var -> kind option
    val find_kind' : context*var -> (con*kind) option
    val remove_kind : context*var -> context
    val insert_kind_list : context* (var * kind) list -> context

    val bind_kind : context * var * kind -> context * var * con subst
    val bind_kind_list : context * (var * kind) list -> context * (var * kind) list * con subst

    val foldli_kind : ((var * kind * 'a) -> 'a) -> 'a -> context -> 'a

    val c_insert_con : context*var*con*(context->'a) -> 'a

    val c_insert_kind : context*var*kind*(context->'a) -> 'a

    val c_insert_con_list : context * (var*con) list * (context -> 'a) -> 'a
    val c_insert_kind_list : context * (var*kind) list * (context -> 'a) -> 'a

    val print_context : context -> unit

    val print_kinds : context -> unit
      
    val print_cons : context -> unit

    val selfify : (Nil.con * Nil.kind) -> Nil.kind

  end 
