(*$import Ppnil Nil List Util Name Listops Stats NilUtil NilError NilContextPre NilSubst Normalize NILHNF *)

structure NilHNF :> NILHNF = 
  struct
    open Nil

    structure NilContext = NilContextPre

    type context = NilContext.context

    type con_subst = NilSubst.con_subst


    val timer = Stats.subtimer
    val subtimer = Stats.subtimer

    val find_kind_equation = (*subtimer ("HNF:find_kind_equation",*)NilContext.find_kind_equation(*)*)

    val type_of = subtimer ("HNF:typeof",Normalize.type_of)

    val substConInCon = fn subst => subtimer ("HNF:substConInCon",NilSubst.substConInCon subst)
    val addr = subtimer ("HNF:addr",NilSubst.C.addr)
    val sim_add = NilSubst.C.sim_add
    val substitute = fn subst => subtimer ("HNF:substitute",NilSubst.C.substitute subst)
    val empty = NilSubst.C.empty
    val fromList = NilSubst.C.simFromList
    val merge = NilSubst.C.merge

    val zip = Listops.zip
    val unzip = Listops.unzip

    val strip_var = NilUtil.strip_var
    val strip_crecord = NilUtil.strip_crecord
    val is_mu_c = NilUtil.is_mu_c

    val eq_opt = Util.eq_opt

    val eq_var = Name.eq_var
    val eq_label = Name.eq_label

    val locate = NilError.locate "NilHNF"
    val perr_c = NilError.perr_c
      
    val find = fn f => subtimer ("HNF:List.find",List.find f)

    fun error s s' = Util.error s s'

    fun is_path (Proj_c (Mu_c _,lab)) = true
      | is_path (Proj_c (c,l)) = is_path c
      | is_path (App_c (c,a)) = is_path c
      | is_path (Var_c v) = true
      | is_path (Annotate_c (_,c)) = is_path c
      | is_path _ = false

    local
      datatype entry =  CON of string * con * NilContext.context * con_subst

      val stack = ref ([] : entry list)
	
      fun push e = stack := (e :: (!stack))
    in
      fun clear_stack() = stack := []
      fun push_con name (c,(context,subst)) = push(CON(name,c,context,subst))
      fun pop() = stack := (tl (!stack))
      fun show_stack() = let val st = !stack
			     val _ = clear_stack()
			     fun show (CON(name,c,context,subst)) =
			       (print (name^" called with constructor =\n");
				Ppnil.pp_con c;
				print "\nand context"; NilContext.print_context context;
				print "\n and subst";  NilSubst.C.print subst;
				print "\n\n")
			 in  app show (rev st)
			 end
      fun wrap str f = 
	(clear_stack(); f())
	handle e => (print "\n ------ ERROR in "; print str; print " ---------\n";
		     show_stack (); 
		     print "\n ------ END STACK for "; print str; print " ---------\n";
		     raise e)
      fun wrap1 str f arg1 = wrap str (fn () => f arg1)
      fun wrap2 str f arg1 arg2 = wrap str (fn () => f arg1 arg2)
    end
	
    fun con_reduce (state : context * con_subst) (constructor : con) : con * bool  = 
(*      (push_con "con_reduce" (constructor,state);
       (con_reduce' state constructor) before pop())
    and con_reduce' (state : context * con_subst) (constructor : con) : con * bool  = *)
      let
	val (D,subst) = state 
	val res = 
	  (case constructor of
	     (Prim_c _) => (substConInCon subst constructor,false)
	   | (Mu_c _) => (substConInCon subst constructor,false)
	   | (AllArrow_c _) => (substConInCon subst constructor,false)
	   | (ExternArrow_c _) => (substConInCon subst constructor,false)
	   | (Crecord_c _) => (substConInCon subst constructor,false)

	   | (Var_c var) => 
	     (case (substitute subst var) of
		SOME c => (c,is_path c)
	      | NONE => (constructor,true))

	   | (Let_c (sort,((cbnd as Open_cb (var,formals,body,body_kind))::rest),con)) =>
	     con_reduce_letfun state (sort,fn x => Open_cb x,var,formals,body,body_kind,rest,con)

	   | (Let_c (sort,((cbnd as Code_cb (var,formals,body,body_kind))::rest),con)) =>
	     con_reduce_letfun state (sort,fn x => Code_cb x,var,formals,body,body_kind,rest,con)

	   | (Let_c (sort,cbnd as (Con_cb(var,con)::rest),body)) =>
	     let 
	       val (con,_) = con_reduce state con
	       val subst = sim_add subst (var,con)
	     in  con_reduce (D,subst) (Let_c(sort,rest,body))
	     end
	   | (Let_c (sort,[],body)) => con_reduce state body
	   | (Closure_c (c1,c2)) => 
	     let 
	       val (c1,path) = con_reduce state c1 
	       val c2 = substConInCon subst c2
	     in  (Closure_c(c1,c2),path)
	     end
	   | Typeof_c e => con_reduce state (type_of(D,e))
	   | (Proj_c (con,label)) => 
	     if is_mu_c con then  (substConInCon subst constructor,true) (*Watch out for annotations!*)
	     else
	       let
		 (*Empty the substitution first, so that you don't substitute
		  * into the large thing*)
		 val ((con,path),state) = 
		   (case con_reduce (D,empty()) con
		      of (con,true) => (con_reduce state con,(D,empty()))
		       | other => (other,state))
	       in
		 if path orelse (is_mu_c con) then
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
	       val ((cfun,path),subst2) = 
		 (case con_reduce (D,empty()) cfun
		    of (con,true) => 
		      (con_reduce state con,
		       empty())
		     | other => (other,subst))


	       val actuals = map (#1 o (con_reduce state)) actuals

	       val res = 
		 if path then
		   (*Substitution must have been carried out, so can stop*)
		   (App_c (cfun,actuals),true) 
		 else
		   (*subst2 remains to be carried out on the body (and potentially the env)
		    * The original subst remains to be carried out on the actuals.
		    *)
		   let
		    
		     (*Note actuals = actuals @ [env] may be true
		      *)
		     fun reduce actuals (formals,body,body_kind) = 
		       let
			 val (vars,_) = unzip formals
			 val subst3 = fromList (zip vars actuals)
		       in con_reduce (D,merge (subst2,subst3)) body
		       end
		   

		   in
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
			    then reduce (actuals @ [#1(con_reduce (D,subst2) env)]) (formals,body,body_kind) 
			  else error (locate "con_reduce") "redex not in HNF"
			 | Closure_c(Let_c (_,[Code_cb (var,formals,body,body_kind)],Var_c v), env) =>
			  if eq_var(var,v)
			    then reduce (actuals @ [#1(con_reduce (D,subst2) env)]) (formals,body,body_kind) 
			  else error (locate "con_reduce") "redex not in HNF"
			 | _ => (perr_c cfun;
				 error (locate "con_reduce") "redex not in HNF"))
		 end
	     in  res
	     end
	   | (Typecase_c {arg,arms,default,kind}) => error (locate "con_reduce") "typecase not done yet"
	   | (Annotate_c (annot,con)) => con_reduce state con)
      in
	res
      end
    and con_reduce_letfun 
      (state : (context * con_subst)) 
      (sort : letsort,
       coder : var * (var * kind) list * con * kind -> conbnd,
       var : var,
       formals : (var * kind) list,
       body : con,
       body_kind : kind,
       rest : conbnd list,
       con : con) : (con * bool) = 
      let
	val (D,subst) = state
	val lambda = (Let_c (sort,[coder (var,formals,body,body_kind)],Var_c var))
      in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var con) 
	   then (substConInCon subst lambda,false)
	 else
	   let 
	     val subst = addr (subst,var,lambda)
	   in  con_reduce (D,subst) (Let_c(sort,rest,con))
	   end
      end

     
    val depth = ref 0
    fun ++ r = (r := !r + 1;!r)
    fun -- r = (r := !r - 1;!r)


    fun reduce_hnf (D,con) = 
(*      (push_con "reduce_hnf" (con,(D,empty()));
       (reduce_hnf' (D,con)) before pop())
    and reduce_hnf' (D,con) = *)
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

(*    val con_reduce = wrap2 "con_reduce" con_reduce*)
    val con_reduce = fn (D,con) => timer ("HNF:con_reduce",#1 o (con_reduce (D,empty()))) con

(*    val reduce_hnf = wrap1 "reduce_hnf" reduce_hnf*)
    val reduce_hnf = fn args => (depth := 0;timer ("HNF:reduce_hnf",reduce_hnf) args)


  end