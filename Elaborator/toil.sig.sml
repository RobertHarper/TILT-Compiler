(*$import Il Ast *)
(* The translation from AST to IL. *)
signature TOIL = 
  sig

    val debug : bool ref


    (* ---------------------- Translations --------------------------- *)
    type decresult = (Il.sbnd option * Il.context_entry) list
    type filepos = Ast.srcpos -> string * int * int

    val xexp    : Il.context * filepos * Ast.exp -> (Il.exp * Il.con * bool) option
    val xdec    : Il.context * filepos * Ast.dec -> decresult option
    val xstrexp : Il.context * filepos * Ast.strexp
	                     * Ast.sigexp Ast.sigConst -> (decresult * Il.mod * Il.signat) option
    val xspec   : Il.context * filepos * Ast.spec list -> (Il.sdec list) option
    val xsigexp : Il.context * filepos * Ast.sigexp -> Il.signat option
    val xty     : Il.context * filepos * Ast.ty -> Il.con option
    val xtybind : Il.context * filepos * Ast.tb list -> decresult option
    val xeq     : Il.context * Il.con -> Il.exp option
    val xeq_mu  : Il.context * Il.con -> Il.exp option


  end;
