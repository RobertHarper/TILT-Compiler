(* Pretty-printing routines for the internal language. *)
signature PPIL =
  sig

    structure Il : IL
    structure IlContext : ILCONTEXT 
    sharing IlContext.Il = Il


    (* type variables can be displayed in different ways *)
    datatype display = VAR_ONLY | VALUE_ONLY | VAR_VALUE
    val convar_display : display ref



    (* these don't do actual output *)
    val pp_bnd'     : Il.bnd -> Formatter.format
    val pp_bnds'    : Il.bnds -> Formatter.format
    val pp_sbnd'    : Il.sbnd -> Formatter.format
    val pp_sbnds'   : Il.sbnds -> Formatter.format
    val pp_dec'     : Il.dec -> Formatter.format
    val pp_decs'    : Il.decs -> Formatter.format
    val pp_sdec'    : Il.sdec -> Formatter.format
    val pp_sdecs'   : Il.sdecs -> Formatter.format
    val pp_exp'     : Il.exp -> Formatter.format
    val pp_con'     : Il.con -> Formatter.format
    val pp_kind'    : Il.kind -> Formatter.format
    val pp_value'   : Il.value -> Formatter.format
    val pp_prim'    : Il.prim -> Formatter.format
    val pp_context' : Il.context -> Formatter.format
    val pp_inline'  : Il.inline -> Formatter.format
    val pp_signat'  : Il.signat -> Formatter.format
    val pp_mod'     : Il.mod -> Formatter.format
    val pp_var'     : Name.var -> Formatter.format
    val pp_label'   : Name.label -> Formatter.format
    val pp_path'    : Il.path -> Formatter.format
    val pp_list'    : ('a -> Formatter.format) -> 'a list -> 
                                  (string * string * string * bool) -> Formatter.format
    val pp_pathlist' : ('a -> Formatter.format) -> 'a list -> Formatter.format
    val pp_commalist' : ('a -> Formatter.format) -> 'a list -> Formatter.format
    val pp_semicolonlist' : ('a -> Formatter.format) -> 'a list -> Formatter.format
    val pp_phrase_class' : IlContext.phrase_class -> Formatter.format

    (* these go to std_out *)
    val pp_bnd     : Il.bnd -> unit
    val pp_bnds    : Il.bnds -> unit
    val pp_sbnd    : Il.sbnd -> unit
    val pp_sbnds   : Il.sbnds -> unit
    val pp_dec     : Il.dec -> unit
    val pp_decs    : Il.decs -> unit
    val pp_sdec    : Il.sdec -> unit
    val pp_sdecs   : Il.sdecs -> unit
    val pp_exp     : Il.exp -> unit
    val pp_con     : Il.con -> unit
    val pp_kind    : Il.kind -> unit
    val pp_value   : Il.value -> unit
    val pp_prim    : Il.prim -> unit
    val pp_context : Il.context -> unit
    val pp_inline  : Il.inline -> unit
    val pp_signat  : Il.signat -> unit
    val pp_mod     : Il.mod -> unit
    val pp_var     : Name.var -> unit
    val pp_label   : Name.label -> unit
    val pp_path    : Il.path -> unit
    val pp_list    : ('a -> Formatter.format) -> 'a list -> 
                                  (string * string * string * bool) -> unit
    val pp_pathlist : ('a -> Formatter.format) -> 'a list -> unit
    val pp_commalist : ('a -> Formatter.format) -> 'a list -> unit
    val pp_semicolonlist : ('a -> Formatter.format) -> 'a list -> unit
    val pp_phrase_class : IlContext.phrase_class -> unit

  end
