(* Pattern compiler.
   Handles pattern bindings, case statements, and function declarations. *)
signature PAT =
  sig

    val debug : bool ref
    val do_result_type : bool ref

    (* We avoid mutual dependencies by installing functions at run-time. *)
    val installHelpers: {typecompile : Il.context * Ast.ty -> Il.con,
			 expcompile : Il.context * Ast.exp -> Il.exp * Il.con * bool,
			 polyinst : Il.context * Il.sdecs -> Il.sbnd list * Il.sdecs * Il.con list}
	                -> unit

    (* BindCompile creates bindings of all variables in the given pattern Ast.pat
        when the pattern is applied to the expression (Il.exp/Il.con).
	Bind is raised if the pattern does not match. *)
    val bindCompile  : {context : Il.context,
			bindpat :  Ast.pat,
			arg : (Il.var * Il.con)} -> (Il.sbnd * Il.sdec) list


    (* CaseCompile corresponds to a case statement. The third argument
        is a list of the arms.  The second argument is the argument of the
	case statement. Normally, a Match exn is raised if noting matches.
	If reraise is true and arg is of type exn, then the arg exn is raised instead. *)
    val caseCompile  : {context : Il.context,
			arg : (Il.var * Il.con),
			arms : (Ast.pat * Ast.exp) list,
			reraise : bool} -> Il.exp * Il.con


    (* funCompile corresponds to a function declaration. It is
       distingushed from the other compiles by the presence of curried
       patterns. The second argument is a list of the function arms,
       which are pairs of the patterns and bodies.

       The return value is a 2-field record denoting a function. The
       argument variable and its type is separated from the body so
       that packaging into the IL FIX construct is more convenient. *)

    val funCompile : {context : Il.context,
		      rules : (Ast.pat list * Ast.exp) list}
	             ->
		     {arglist : (Il.var * Il.con) list,
		      body : Il.exp * Il.con}
  end
