signature NILCONTEXT = 
  sig
    structure Nil : NIL

    type kind = Nil.kind
    type con = Nil.con
    type var = Nil.var

    exception NotFound

    type context

    val empty : unit -> context

    val insert_con : context*var*con -> context
    val find_con : context*var -> con option
    val remove_con : context*var -> context


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
    val insert_kind : context*var*kind -> context
    val find_kind : context*var -> kind option
    val remove_kind : context*var -> context
    val appi_kind : ((var * kind) -> unit) -> context -> unit

    val c_insert_con : context*var*con*(context->'a) -> 'a
    val c_remove_con : context*var*(context -> 'a) -> 'a

    val c_insert_kind : context*var*kind*(context->'a) -> 'a
    val c_remove_kind : context*var*(context -> 'a) -> 'a

    val c_insert_con_list : context * (var*con) list * (context -> 'a) -> 'a
    val c_insert_kind_list : context * (var*kind) list * (context -> 'a) -> 'a
      
    val c_foldl : ('state -> 'a) -> ('elt * 'state * ('state -> 'a) -> 'a) 
                                                     -> 'state -> 'elt list -> 'a
    val c_fold_con_acc : 
      ((context * 'acc_elt list) -> 'a)
      -> ((context * 'elt) -> ('acc_elt * (var * con)))
         -> context -> 'elt list -> 'a

    val c_fold_kind_acc : 
      ((context * 'acc_elt list) -> 'a)
      -> ((context * 'elt) -> ('acc_elt * (var * kind)))
         -> context -> 'elt list -> 'a

    val print_context : context -> unit

    val print_kinds : context -> unit
      
    val print_cons : context -> unit

    val selfify : (Nil.con * Nil.kind) -> Nil.kind

  end 
