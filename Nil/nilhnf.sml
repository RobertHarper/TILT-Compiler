(*$import NilUtil NilContextPre NilSubst NORMALIZE *)

structure NilHNF :> NILHNF = 
  struct
    open Nil

    structure NilContext = NilContextPre

    type context = NilContext.context

    type 'a subst = 'a NilSubst.subst


    val timer = Stats.timer
    val subtimer = Stats.subtimer

    val find_kind_equation = (*subtimer ("HNF:find_kind_equation",*)NilContext.find_kind_equation(*)*)

    val type_of = subtimer ("HNF:typeof",Normalize.type_of)

    val substConInCon = fn subst => subtimer ("HNF:substConInCon",NilSubst.substConInCon subst)
    val add = fn subst => subtimer ("HNF:add",NilSubst.add subst)
    val substitute = fn subst => subtimer ("HNF:substitute",NilSubst.substitute subst)
    val empty = NilSubst.empty
    val fromList = NilSubst.fromList

    val zip = Listops.zip
    val unzip = Listops.unzip

    val strip_var = NilUtil.strip_var
    val strip_crecord = NilUtil.strip_crecord

    val eq_opt = Util.eq_opt

    val eq_var = Name.eq_var
    val eq_label = Name.eq_label

    val locate = NilError.locate "NilHNF"
    val perr_c = NilError.perr_c
      
    val find = fn f => subtimer ("HNF:List.find",List.find f)

    fun error s s' = Util.error s s'
      
    fun con_reduce (state : context * con subst) (constructor : con) : con * bool  = 
      let
	val (D,subst) = state 
	val res = 
	  (case constructor of
	     (Prim_c _) => (substConInCon subst constructor,false)
	   | (Mu_c _) => (substConInCon subst constructor,false)
	   | (AllArrow_c _) => (substConInCon subst constructor,false)
	   | (ExternArrow_c _) => (substConInCon subst constructor,false)
	   | (Crecord_c _) => (substConInCon subst constructor,false)
	   | (Proj_c (Mu_c _,lab)) => (substConInCon subst constructor,false)

	   | (Var_c var) => 
	     (case (substitute subst var) of
		SOME c => subtimer ("HNF:Reduce on subst",con_reduce (D,empty())) c 
	      | NONE => (constructor,true))

	   | (Let_c (sort,((cbnd as Open_cb (var,formals,body,body_kind))::rest),con)) =>
	     con_reduce_letfun state (sort,Open_cb,var,formals,body,body_kind,rest,con)

	   | (Let_c (sort,((cbnd as Code_cb (var,formals,body,body_kind))::rest),con)) =>
	     con_reduce_letfun state (sort,Code_cb,var,formals,body,body_kind,rest,con)

	   | (Let_c (sort,cbnd as (Con_cb(var,con)::rest),body)) =>
	     let 
	       val con = substConInCon subst con
	       val subst = add subst (var,con)
	     in  con_reduce (D,subst) (Let_c(sort,rest,body))
	     end
	   | (Let_c (sort,[],body)) => con_reduce state body
	   | (Closure_c (c1,c2)) => 
	     let 
	       val (c1,path) = con_reduce state c1 
	       val c2 = substConInCon subst c2
	     in  (Closure_c(c1,c2),false)
	     end
	   | Typeof_c e => con_reduce state (type_of(D,e))
	   | (Proj_c (con,label)) => 
	     let
	       (*Empty the substitution first, so that you don't substitute
		* into the large thing*)
	       val ((con,path),state) = 
		 (case con_reduce (D,empty()) con
		    of (con,true) => (con_reduce state con,(D,empty()))
		     | other => (other,state))
	     in
	       if path then
		 (Proj_c(con,label),true)
	       else
		 (case strip_crecord con
		    of SOME entries =>
		      (case (find (fn ((l,_)) => eq_label (l,label)) entries )
			 of SOME (_,con) => con_reduce state con  
			  | NONE => (error (locate "con_reduce") "Field not in record"))
		     | NONE => (perr_c con;
				error (locate "con_reduce") "Not a path, but not a record!"))
	     end
	   | (App_c (cfun,actuals)) => 
	     let

	       (*Empty the substitution first, so that you don't substitute
		* into the large thing*)
	       val ((cfun,path),actuals,state) = 
		 (case con_reduce (D,empty()) cfun
		    of (con,true) => 
		      (con_reduce state con,
		       map (#1 o (con_reduce state)) actuals,
		       (D,empty()))
		     | other => (other,map (#1 o (con_reduce (D,empty()))) actuals,state))

	       val res = 
		 let
		   
		   fun reduce actuals (formals,body,body_kind) = 
		     let
		       val (vars,_) = unzip formals
		       val subst = fromList (zip vars actuals)
		     in con_reduce (D,subst) body
		     end
		   
		   val app = App_c (cfun,actuals)
		 in
		   if path then
		     (app,true) 
		   else
		     (case cfun 
			of Let_c (_,[Open_cb (var,formals,body,body_kind)],Var_c v) =>
			  if eq_var(var,v)
			    then reduce actuals (formals,body,body_kind) 
			  else error (locate "con_reduce") "redex not in HNF"
			 | Let_c (_,[Code_cb (var,formals,body,body_kind)],Var_c v) =>
			  if eq_var(var,v)
			    then reduce actuals (formals,body,body_kind) 
			  else error (locate "con_reduce") "redex not in HNF"
			 | Let_c (_,[Code_cb (var,formals,body,body_kind)],Closure_c(Var_c v,env)) =>
			  if eq_var(var,v)
			    then reduce (actuals @ [env]) (formals,body,body_kind) 
			  else error (locate "con_reduce") "redex not in HNF"
			 | Closure_c(Let_c (_,[Code_cb (var,formals,body,body_kind)],Var_c v), env) =>
			  if eq_var(var,v)
			    then reduce (actuals @ [env]) (formals,body,body_kind) 
			  else error (locate "con_reduce") "redex not in HNF"
			 | _ => error (locate "con_reduce") "redex not in HNF")
		 end
	     in  res
	     end
	   | (Typecase_c {arg,arms,default,kind}) => error (locate "con_reduce") "typecase not done yet"
	   | (Annotate_c (annot,con)) => con_reduce state con)
      in
	res
      end
    and con_reduce_letfun state (sort,coder,var,formals,body,body_kind,rest,con) = 
      let
	val (D,subst) = state
	val lambda = (Let_c (sort,[coder (var,formals,body,body_kind)],Var_c var))
      in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var con) 
	   then (substConInCon subst lambda,false)
	 else
	   let 
	     val subst = add subst (var,substConInCon subst lambda)
	   in  con_reduce (D,subst) (Let_c(sort,rest,con))
	   end
      end

     
    val depth = ref 0
    fun ++ r = (r := !r + 1;!r)
    fun -- r = (r := !r - 1;!r)

    fun reduce_hnf (D,con) = 
      let
	val (con,path) = con_reduce (D,empty()) con
      in
	if path then
	  (case find_kind_equation(D,con)
	     of SOME con => (subtimer ("HNF:Reduce level "^(Int.toString (++depth)),reduce_hnf) (D,con)) before (ignore (--depth))
	      | NONE => con)
	else
	  con
      end
    
    val con_reduce = fn (D,con) => timer ("HNF:con_reduce",#1 o (con_reduce (D,empty()))) con

    val reduce_hnf = fn args => (depth := 0;timer ("HNF:reduce_hnf",reduce_hnf) args)

(*
    datatype state = STATE of {context:context,
			       subst:con subst,
			       path : bool,
			       con_reduce : state * con -> con}
      
    fun con_handler (state as STATE {context,subst,path,con_reduce}) : state * con changeopt = 
      let
	val res = 
	  (case constructor of
	     (Prim_c _) => CHANGE_NORECURSE (state,substConInCon subst constructor)
	   | (Mu_c _) => CHANGE_NORECURSE (state,substConInCon subst constructor)
	   | (AllArrow_c _) => CHANGE_NORECURSE (state,substConInCon subst constructor)
	   | (ExternArrow_c _) => CHANGE_NORECURSE (state,substConInCon subst constructor)
	   | (Crecord_c _) => CHANGE_NORECURSE (state,substConInCon subst constructor)
	   | (Proj_c (Mu_c _,lab)) => CHANGE_NORECURSE (state,substConInCon subst constructor)
	   | (Var_c var) => 
	     (case (substitute subst var) of
		SOME c => CHANGE_RECURSE (STATE{context=context,subst=empty(),path=path,con_reduce=con_reduce},c)
	      | NONE => CHANGE_NORECURSE (STATE{context=context,subst=subst,path=true,con_reduce=con_reduce},constructor))
	   | (Closure_c (c1,c2)) => 
	     let 
	       val (c1,path) = con_reduce state c1 
	       val c2 = substConInCon subst c2
	     in  CHANGE_NORECURSE(state,Closure_c(c1,c2))
	     end
	   | Typeof_c e => CHANGE_RECURSE (STATE{context=context,subst=empty(),path=path,con_reduce=con_reduce},type_of(D,e))
	   | (Proj_c (con,label)) => 
	     let
	       val (con,path) = con_reduce state con
	     in
	       if path then
		 (Proj_c(con,label),true)
	       else
		 (case strip_crecord con
		    of SOME entries =>
		      (case (List.find (fn ((l,_)) => eq_label (l,label)) entries )
			 of SOME (_,con) => con_reduce (D,empty()) con
			  | NONE => (error (locate "con_reduce") "Field not in record"))
		     | NONE => (perr_c con;
				error (locate "con_reduce") "Not a path, but not a record!"))
	     end
	   | (App_c (cfun,actuals)) => 
	     let
	       val (cfun,path) = con_reduce state cfun 

	       val res = 
		 let
		   
		   fun reduce actuals (formals,body,body_kind) = 
		     let
		       val (vars,_) = unzip formals
		       val subst = fromList (zip vars actuals)
		     in con_reduce (D,subst) body
		     end
		   
		   val actuals = map (#1 o (con_reduce state)) actuals

		   val app = App_c (cfun,actuals)
		 in
		   if path then
		     (app,true) 
		   else
		     (case cfun 
			of Let_c (_,[Open_cb (var,formals,body,body_kind)],Var_c v) =>
			  if eq_var(var,v)
			    then reduce actuals (formals,body,body_kind) 
			  else error (locate "con_reduce") "redex not in HNF"
			 | Let_c (_,[Code_cb (var,formals,body,body_kind)],Var_c v) =>
			  if eq_var(var,v)
			    then reduce actuals (formals,body,body_kind) 
			  else error (locate "con_reduce") "redex not in HNF"
			 | Let_c (_,[Code_cb (var,formals,body,body_kind)],Closure_c(Var_c v,env)) =>
			  if eq_var(var,v)
			    then reduce (actuals @ [env]) (formals,body,body_kind) 
			  else error (locate "con_reduce") "redex not in HNF"
			 | Closure_c(Let_c (_,[Code_cb (var,formals,body,body_kind)],Var_c v), env) =>
			  if eq_var(var,v)
			    then reduce (actuals @ [env]) (formals,body,body_kind) 
			  else error (locate "con_reduce") "redex not in HNF"
			 | _ => error (locate "con_reduce") "redex not in HNF")
		 end
	     in  res
	     end
	   | (Typecase_c {arg,arms,default,kind}) => error (locate "con_reduce") "typecase not done yet"
	   | (Annotate_c (annot,con)) => con_reduce state con)
      in
	res
      end
    and con_reduce_letfun state (sort,coder,var,formals,body,body_kind,rest,con) = 
      let
	val (D,subst) = state
	val lambda = (Let_c (sort,[coder (var,formals,body,body_kind)],Var_c var))
      in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var con) 
	   then (substConInCon subst lambda,false)
	 else
	   let 
	     val subst = add subst (var,substConInCon subst lambda)
	   in  con_reduce (D,subst) (Let_c(sort,rest,con))
	   end
      end

     
    val depth = ref 0
    fun ++ r = (r := !r + 1;!r)
    fun -- r = (r := !r - 1;!r)

    fun reduce_hnf (D,con) = 
      let
	val (con,path) = con_reduce (D,empty()) con
      in
	if path then
	  (case find_kind_equation(D,con)
	     of SOME con => (subtimer ("HNF:Reduce level "^(Int.toString (++depth)),reduce_hnf) (D,con)) before (ignore (--depth))
	      | NONE => con)
	else
	  con
      end
    
    val con_reduce = fn (D,con) => timer ("HNF:con_reduce",#1 o (con_reduce (D,empty()))) con

    val reduce_hnf = fn args => (depth := 0;timer ("HNF:reduce_hnf",reduce_hnf) args)

*)
  end