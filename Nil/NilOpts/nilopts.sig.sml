signature NILOPTS = 
sig
    val debug : bool ref 

    val do_anormalize2 : bool ref
    val do_cse : bool ref
    val do_flatten : bool ref
    val do_reduce : bool ref 
    val do_inline : bool ref
    val do_uncurry : bool ref
	val do_inline_once : bool ref
	val do_project_known : bool ref
	val do_dead : bool ref

    val do_fold_constants : bool ref

    val print_pre : bool ref
    val print_anormalize : bool ref
    val print_flatten : bool ref 
    val print_uncurry : bool  ref
    val print_inline : bool ref 
    val print_reduce : bool  ref 

    type click
    val make_click : string -> click
    val inc_click : click -> unit

    val init_clicks : unit -> unit
    val print_clicks : unit -> unit

    (* How many clicks have gone by since the last execution 
     of round_clicks ? *)
    val round_clicks : click list -> int
    val print_round_clicks : click list -> unit 

end 



