(* Pretty-printing routines for the primitives *)
signature PPPRIM =
  sig
    structure Prim : PRIM

    (* these don't do actual output *)
    val pp_prim'    : Prim.prim -> Formatter.format
    val pp_ilprim'  : Prim.ilprim -> Formatter.format
    val pp_value'   : ('exp -> Formatter.format) -> ('con -> Formatter.format) 
	               -> ('con,'exp) Prim.value -> Formatter.format
    val pp_is'      : Prim.intsize -> Formatter.format
    val pp_fs'      : Prim.floatsize -> Formatter.format

    (* these go to std_out *)
    val pp_prim    : Prim.prim -> unit
    val pp_ilprim  : Prim.ilprim -> unit
    val pp_value   : ('exp -> Formatter.format) -> ('con -> Formatter.format) 
	               -> ('con,'exp) Prim.value -> unit
    val pp_is      : Prim.intsize -> unit
    val pp_fs      : Prim.floatsize -> unit

  end
