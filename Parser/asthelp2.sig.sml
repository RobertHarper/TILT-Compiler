(* Helper routines for the external AST. *)
signature ASTHELP =
  sig

    (* Some pretty-printing routines for AST *)
    val pp_tyvar' : Ast.tyvar -> Formatter.format
    val pp_sym'   : Symbol.symbol -> Formatter.format
    val pp_path'  : Ast.path -> Formatter.format
    val pp_typath': Ast.typath -> Formatter.format
    val pp_ty'    : Ast.ty -> Formatter.format
    val pp_pat'   : Ast.pat -> Formatter.format
    val pp_exp'   : Ast.exp -> Formatter.format
    val pp_strexp'   : Ast.strexp -> Formatter.format

    (* These print to std_out *)
    val pp_tyvar : Ast.tyvar -> unit
    val pp_sym   : Symbol.symbol -> unit
    val pp_path  : Ast.path -> unit
    val pp_typath: Ast.typath -> unit
    val pp_ty    : Ast.ty -> unit
    val pp_pat   : Ast.pat -> unit
    val pp_exp   : Ast.exp -> unit
    val pp_strexp: Ast.strexp -> unit


    (* AST stripping routines to remove Mark information *)
    val tyvar_strip: Ast.tyvar -> Symbol.symbol
    val db_strip   : Ast.db    -> (Symbol.symbol * Ast.tyvar list * Ast.dbrhs)
    val tb_strip   : Ast.tb    -> (Symbol.symbol * Ast.tyvar list * Ast.ty)
    val strb_strip : Ast.strb  -> (Symbol.symbol * (Ast.strexp * Ast.sigexp Ast.sigConst))
    val vb_strip   : Ast.vb    -> (Ast.pat * Ast.exp)
    val fb_strip   : Ast.fb    -> Ast.clause list
    val exp_strip  : Ast.exp   -> Ast.exp

    (* Useful AST/EL expressions *)
    val true_exp  : Ast.exp
    val false_exp : Ast.exp
    val true_pat  : Ast.pat
    val false_pat : Ast.pat
    val nil_exp : Ast.exp
    val cons_exp : Ast.exp

    (* Misc equality, substitutions, and free variable computation on AST *)
    val eq_path        : Ast.path * Ast.path -> bool
    val free_tyvar_dec : Ast.dec * (Ast.symbol -> bool) -> Ast.symbol list
    val free_tyvar_exp : Ast.exp * (Ast.symbol -> bool) -> Ast.symbol list
    val free_tyvar_ty  : Ast.ty  * (Ast.symbol -> bool) -> Ast.symbol list
    val free_tyc_ty    : Ast.ty  * (Ast.symbol -> bool) -> Ast.symbol list
    val subst_vars_exp : (Symbol.symbol * Ast.path) list * Ast.exp -> Ast.exp
    val subst_vars_ty  : (Symbol.symbol * Ast.tyvar) list * Ast.ty -> Ast.ty

  end
