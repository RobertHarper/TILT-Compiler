(* The translation from AST to IL. *)
signature TOIL = 
  sig
    structure Il : IL

    val debug : bool ref
    val s1 : Il.signat ref
    val s2 : Il.signat ref
    val decs1 : Il.decs ref

    (* Translations *)
    val xexp    : Il.context * Ast.exp -> (Il.exp * Il.con)  
    val xdec    : Il.context * Ast.dec -> (Il.mod * Il.signat)  
    val xstrexp : Il.context * Ast.strexp * Ast.sigexp option -> Il.mod * Il.signat
    val xspec   : Il.context * Ast.spec list -> Il.sdec list
    val xsigexp : Il.context * Ast.sigexp -> Il.signat
    val xty     : Il.context * Ast.ty -> Il.con
    val xtybind : Il.context * Ast.tb list -> Il.mod * Il.signat
    val poly_inst : Il.decs * Il.sdec list -> Il.sbnd list * Il.sdec list * Il.con list


    (* Signature Patching *)
    val xsig_wheretype : Il.signat * Il.label list * Il.con * Il.kind -> Il.signat
    val xsig_sharing : Il.signat * Il.label list * Il.label list * Il.kind -> Il.signat

  end;
