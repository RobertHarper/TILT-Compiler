(*$import WNil Formatter *)
(* Pretty-printing routines for the NIL *)
signature PPNIL =
  sig

    val elide_prim : bool ref (* hide the type arguments to primitives *)

    (* these don't do actual output *)
    val pp_bnd'     : Nil.bnd -> Formatter.format
    val pp_conbnd'  : Nil.conbnd -> Formatter.format
    val pp_bnds'    : Nil.bnd list -> Formatter.format
    val pp_exp'     : Nil.exp -> Formatter.format
    val pp_con'     : Nil.con -> Formatter.format
    val pp_kind'    : Nil.kind -> Formatter.format
    val pp_prim'    : Nil.prim -> Formatter.format
    val pp_trace'   : Nil.niltrace -> Formatter.format
    val pp_var'     : Name.var -> Formatter.format
    val pp_label'   : Name.label -> Formatter.format
    val pp_list'    : ('a -> Formatter.format) -> 'a list -> 
                                  (string * string * string * bool) -> Formatter.format
    val pp_module'   : Nil.module -> Formatter.format

    (* these go to std_out *)
    val pp_bnd     : Nil.bnd -> unit
    val pp_conbnd  : Nil.conbnd -> unit
    val pp_bnds    : Nil.bnd list -> unit
    val pp_exp     : Nil.exp -> unit
    val pp_con     : Nil.con -> unit
    val pp_kind    : Nil.kind -> unit
    val pp_prim    : Nil.prim -> unit
    val pp_trace   : Nil.niltrace -> unit
    val pp_var     : Name.var -> unit
    val pp_label   : Name.label -> unit
    val pp_list    : ('a -> Formatter.format) -> 'a list -> 
                                  (string * string * string * bool) -> unit
    val pp_module   : {module: Nil.module,
                       name: string,
                       pass: string,
                       header: string} -> unit

  end
