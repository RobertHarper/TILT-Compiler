(* The translation from AST to IL. *)
signature TOIL =
sig

    val debug : bool ref

    val add_eq_entry : (Il.context,Il.con,Il.exp) Tyvar.tyvar -> unit
    val xeq'         : Il.context * (Il.con * Il.exp * Il.exp * (Il.con -> Il.con)) option * Il.con -> (Il.exp * Il.con) option
    val xeq          : Il.context * Il.con -> (Il.exp * Il.con) option
    val polyinst     : Il.context * Il.sdecs -> Il.sbnd list * Il.sdecs * Il.con list

    val expcompile  : Il.context * Ast.exp -> Il.exp * Il.con * bool  (* valuability *)
    val typecompile : Il.context * Ast.ty -> Il.con

    type filepos = Ast.srcpos -> string * int * int

    val xdec : Il.context * filepos * Ast.dec -> Il.decresult option
    val xtopspec : Il.context * filepos * Ast.topspec -> Il.entries option
    val seal : Il.context * filepos * Il.mod * Il.signat * Il.signat -> Il.mod option

end
