(* Pattern compiler.  
   Handles pattern bindings, case statements, and function declarations. *)
signature PAT = 
  sig
    structure Il : IL

    val debug : bool ref

    type patarg = {context : Il.context,
		   typecompile : Il.context * Ast.ty -> Il.con,
		   expcompile : Il.context * Ast.exp -> Il.exp * Il.con,
		   polyinst : Il.context * Il.sdecs -> Il.sbnd list * Il.sdecs * Il.con list }

    (* BindCompile creates bindings of all variables in the given pattern Ast.pat
        when the pattern is applied to the expression (Il.exp/Il.con). *)
    val bindCompile  : {patarg : patarg,
			bindpat :  Ast.pat,
			arg : (Il.exp * Il.con)} -> (Il.sbnd * Il.sdec) list


    (* CaseCompile corresponds to a case statement. The second argument
        is a list of the arms.  The third argument is the argument of the
	 case statement. *)
    val caseCompile  : {patarg : patarg,
			arg : (Il.exp * Il.con),
			arms : (Ast.pat * Ast.exp) list} -> Il.exp * Il.con



    (* funCompile correspond to a function declaration.  It is distingushed from
       the other compiles by the presence of curried patterns.  The second argument
       is a list of the function arms, which are pairs of the patterns and bodies.
       The third argument, if present, is the value of the function
       if the match fails.  If not present, a match exception is raised.

       The return value is a 2-field record denoting a function.
       The argument variable and its type is separated from the body
       so that packaging into the IL FIX construct is more convenient. *)
    val funCompile : {patarg : patarg,
		      rules : (Ast.pat list * Ast.exp) list,
		      reraise : bool} -> {arglist : (Il.var * Il.con) list,
					  body : Il.exp * Il.con}
  end
