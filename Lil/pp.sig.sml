(* Pretty-printing routines for the LIL *)
signature PPLIL =
  sig

    (* these don't do actual output *)
    val pp_bnd'     : Lil.bnd -> Formatter.format
    val pp_bnds'    : Lil.bnd list -> Formatter.format
    val pp_exp'     : Lil.exp -> Formatter.format
    val pp_sv32'    : Lil.sv32 -> Formatter.format
    val pp_sv64'    : Lil.sv64 -> Formatter.format
    val pp_op32'    : Lil.op32 -> Formatter.format
    val pp_op64'    : Lil.op64 -> Formatter.format
    val pp_con'     : Lil.con -> Formatter.format
    val pp_kind'    : Lil.kind -> Formatter.format
    val pp_prim'    : Lil.prim -> Formatter.format
    val pp_var'     : Name.var -> Formatter.format
    val pp_label'   : Name.label -> Formatter.format
    val pp_list'    : ('a -> Formatter.format) -> 'a list -> 
                                  (string * string * string * bool) -> Formatter.format
    val pp_module'   : Lil.module -> Formatter.format

    (* these go to std_out *)
    val pp_bnd     : Lil.bnd -> unit
    val pp_bnds    : Lil.bnd list -> unit
    val pp_exp     : Lil.exp -> unit
    val pp_sv32    : Lil.sv32 -> unit
    val pp_sv64    : Lil.sv64 -> unit
    val pp_op32    : Lil.op32 -> unit
    val pp_op64    : Lil.op64 -> unit
    val pp_con     : Lil.con -> unit
    val pp_kind    : Lil.kind -> unit
    val pp_prim    : Lil.prim -> unit
    val pp_var     : Name.var -> unit
    val pp_label   : Name.label -> unit
    val pp_list    : ('a -> Formatter.format) -> 'a list -> 
                                  (string * string * string * bool) -> unit
    val pp_module   : Lil.module -> unit
      
    val pp_pass     : {module: Lil.module,
                       name: string,
                       pass: string,
                       header: string} -> unit

  end
