signature NILCONTEXT = 
  sig
    structure Nil : NIL

    type kind = Nil.kind
    type con = Nil.con
    type var = Nil.var

    exception NotFound

    type context

    val empty : unit -> context


    val insert_con : context*var*con -> unit

    val find_con : context*var -> con option
      
    val remove_con : context*var -> unit


    val insert_kind : context*var*kind -> unit

    val find_kind : context*var -> kind option
      
    val remove_kind : context*var -> unit


    val c_insert_con : context*var*con*(context->'a) -> 'a

    val c_remove_con : context*var*(context -> 'a) -> 'a


    val c_insert_kind : context*var*kind*(context->'a) -> 'a

    val c_remove_kind : context*var*(context -> 'a) -> 'a

    val c_insert_con_list : context * (var*con) list * (context -> 'a) -> 'a
      
    val c_insert_kind_list : context * (var*kind) list * (context -> 'a) -> 'a

    val print_context : context -> unit

    val print_kinds : context -> unit
      
    val print_cons : context -> unit

  end 
