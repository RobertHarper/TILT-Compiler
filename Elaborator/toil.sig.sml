(* The translation from AST to IL. *)
signature TOIL = 
  sig
    structure Il : IL

    val debug : bool ref

    (* Translations *)
    type decresult = (Il.sbnd option * Il.context_entry) list
    val xexp    : Il.context * Ast.exp -> (Il.exp * Il.con)  
    val xdec    : Il.context * Ast.dec -> decresult
    val xstrexp : Il.context * Ast.strexp
	                     * Ast.sigexp Ast.sigConst -> decresult * Il.mod * Il.signat
    val xspec   : Il.context * Ast.spec list -> Il.sdec list
    val xsigexp : Il.context * Ast.sigexp -> Il.signat
    val xty     : Il.context * Ast.ty -> Il.con
    val xtybind : Il.context * Ast.tb list -> decresult
    val poly_inst : Il.context * Il.sdec list -> Il.sbnd list * Il.sdec list * Il.con list
    val xeq     : Il.context * Il.con -> Il.exp option

    (* Signature Patching *)
    val xsig_sharing_type      : Il.context * Il.sdecs * Ast.path list -> Il.sdecs
    val xsig_sharing_structure : Il.context * Il.sdecs * Ast.path list -> Il.sdecs
    val xsig_wheretype         : Il.sdecs * Il.label list * Il.con * Il.kind -> Il.sdecs

  end;
