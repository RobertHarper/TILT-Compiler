(*$import TopLevel Util Name Nil NilContext NilRewrite BOUNDCHECK *)

(*
 Verification that a module contains no unbound variables.
 Uses NilRewriting to do the bulk of the work.
*)

structure BoundCheck :> BOUNDCHECK = 
  struct
  
    open Nil
      
    val lprintl = Util.lprintl
    val printl = Util.printl

   (*Shadowing*)
    local 
      open NilRewrite

      structure VarSet = Name.VarSet

      val add = VarSet.add
      val member = VarSet.member
	
      type state = {error   : bool ref,
		    context : NilContext.context,
		    cbound  : VarSet.set,
		    ebound  : VarSet.set}
      (*
       !error = whether or not an unbound variable has been found
       context = Nil context for bound variables at a certain point in a Nil module
       cbound = bound constructor variables at the current point
       ebound = bound term variables at the current point
      *)

      (* Term variable binder *)
      fun exp_var_bind (state : state as {error,context,ebound,cbound},var) : (state * var option)= 
	(if member (ebound, var) orelse NilContext.bound_exp (context,var) then
	   (lprintl ("Warning! Expression variable " ^ (Name.var2string var) ^ " rebound");
	    error := true;
	    (state,NONE)
	    )
	 else
	   ({error = error,context = context,ebound = add (ebound,var),cbound = cbound},
	    NONE))

      (* Constructor variable binder *)
      fun con_var_bind (state :state as {error,context,cbound,ebound},var) : (state * var option)= 
	(if member (cbound, var) orelse NilContext.bound_con (context,var) then
	   (lprintl ("Warning! Constructor variable " ^ (Name.var2string var) ^ " rebound");
	    error := true;
	    (state,NONE)
	    )
	 else
	   ({error = error,context = context,cbound = add (cbound,var),ebound = ebound},
	    NONE))
	   
      (* Constructor handler *)
      fun conhandler (state as {error,context,cbound,...} : state,con : con) =
	(case con
	   of Var_c var => 
	     if NilContext.bound_con(context,var) orelse member (cbound,var) then NOCHANGE
	     else 
	       (lprintl ("Warning! Constructor variable " ^ (Name.var2string var) ^ " is unbound");
		error := true;
		NOCHANGE
		)
	    | _ => NOCHANGE)

      (* Term handler *)
      fun exphandler (state as {error,context,ebound,...} : state,exp : exp) =
	(case exp
	   of Var_e var => 
	     if NilContext.bound_exp(context,var) orelse member (ebound,var) then NOCHANGE
	     else 
	       (lprintl ("Warning! Expression variable " ^ (Name.var2string var) ^ " is unbound");
		error := true;
		NOCHANGE
		)
	    | _ => NOCHANGE)


      val all_handlers =  
	let
	  val h = set_con_binder default_handler con_var_bind
	  val h = set_exp_binder h exp_var_bind
	  val h = set_exphandler h exphandler
	  val h = set_conhandler h conhandler
	in
	  h
	end

      val {rewrite_exp = checkExp,
	   rewrite_con = checkCon,
	   rewrite_kind = checkKind,
	   rewrite_mod = checkMod,...} = rewriters all_handlers

      fun checkXXX checker (context,item) = 
	let
	  val error  = ref false
	  val cbound = VarSet.empty
	  val ebound = VarSet.empty
	in 
	  ignore (checker {error = error,context = context,ebound = ebound,cbound = cbound} item);
	  not (!error)
	end

    in
      val check_exp  = checkXXX checkExp
      val check_con  = checkXXX checkCon
      val check_kind = checkXXX checkKind
      val check_mod  = checkXXX checkMod
    end
  end