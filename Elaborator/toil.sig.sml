(* The translation from AST to IL. *)
signature TOIL = 
  sig
    structure Il : IL

    val debug : bool ref

    (* Translations *)
    val xexp    : Il.context * Ast.exp -> (Il.exp * Il.con)  
    val xdec    : Il.context * Ast.dec -> (Il.sbnd option * Il.context_entry) list
    val xstrexp : Il.context * Ast.strexp * Ast.sigexp Ast.sigConst -> Il.mod * Il.signat
    val xspec   : Il.context * Ast.spec list -> Il.sdec list
    val xsigexp : Il.context * Ast.sigexp -> Il.signat
    val xty     : Il.context * Ast.ty -> Il.con
    val xtybind : Il.context * Ast.tb list -> (Il.sbnd option * Il.context_entry) list
    val poly_inst : Il.context * Il.sdec list -> Il.sbnd list * Il.sdec list * Il.con list
    val xeq     : Il.context * Il.con -> Il.exp

    (* Signature Patching *)
    val xsig_wheretype : Il.signat * Il.label list * Il.con * Il.kind -> Il.signat
    val xsig_sharing : Il.signat * Il.label list * Il.label list * Il.kind -> Il.signat

  end;
