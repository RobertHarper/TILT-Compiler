(*$import Ppnil NilUtil NilContextPre NilSubst NORMALIZE Alpha *)

structure Normalize :> NORMALIZE =
struct	

  structure NilContext = NilContextPre


  val number_flatten = Nil.flattenThreshold

  val profile       = Stats.ff "nil_profile"
  val local_profile = Stats.ff "normalize_profile"

  val subtimer = fn args => fn args2 => if !profile orelse !local_profile then Stats.subtimer args args2 else #2 args args2
    
  open Curtain
  open Nil 
  open Prim

  type con_subst = NilSubst.con_subst

  val substConInKind= fn s => subtimer("Norm:substConInKind",NilSubst.substConInKind s)
  val substConInExp = fn s => subtimer("Norm:substConInExp",NilSubst.substConInExp s)
  val substConInCon = fn s => subtimer("Norm:substConInCon",NilSubst.substConInCon s)
  val substExpInCon = fn s => subtimer("Norm:substExpInCon",NilSubst.substExpInCon s)

  val empty         = NilSubst.C.empty
  val add           = NilSubst.C.sim_add
  val addr          = NilSubst.C.addr
  val substitute    = NilSubst.C.substitute
  val fromList      = NilSubst.C.simFromList
  val printConSubst = NilSubst.C.printf Ppnil.pp_con

  val makeLetC             = NilUtil.makeLetC
  val is_var_c             = NilUtil.is_var_c
  val strip_var            = NilUtil.strip_var
  val strip_crecord        = NilUtil.strip_crecord
  val strip_proj           = NilUtil.strip_proj
  val strip_prim           = NilUtil.strip_prim
  val strip_app            = NilUtil.strip_app
  val con_free_convar      = NilUtil.con_free_convar
  val generate_tuple_label = NilUtil.generate_tuple_label
  val primequiv            = NilUtil.primequiv
  val singletonize         = NilUtil.singletonize 

  (*From NilRename*)
  val alphaCRenameExp   = NilRename.alphaCRenameExp
  val alphaCRenameCon   = NilRename.alphaCRenameCon
  val alphaCRenameKind  = NilRename.alphaCRenameKind
  val alphaECRenameCon  = NilRename.alphaECRenameCon
  val alphaECRenameKind = NilRename.alphaECRenameKind

  (*From Name*)
  val eq_var          = Name.eq_var
  val eq_var2         = Name.eq_var2
  val eq_label        = Name.eq_label
  val fresh_named_var = Name.fresh_named_var
  fun fresh_var ()    = fresh_named_var "normalize"
  val derived_var     = Name.derived_var
  val label2string    = Name.label2string
  val var2string      = Name.var2string 

  (*From Listops*)
  val map_second = Listops.map_second
  val foldl_acc  = Listops.foldl_acc
  val foldl2     = Listops.foldl2
  val map        = Listops.map
  val map2       = Listops.map2
  val zip        = Listops.zip
  val unzip      = Listops.unzip
  val all        = Listops.all
  val all2       = Listops.all2

  (*From Util *)
  val eq_opt  = Util.eq_opt
  val map_opt = Util.mapopt
  val printl  = Util.printl
  val lprintl = Util.lprintl

  (* NilContext *)
  type context   = NilContext.context
  val find_kind  = NilContext.find_kind   
  val kind_of    = NilContext.kind_of
  val find_con   = NilContext.find_con
  val insert_con = NilContext.insert_con

  val insert_kind          = NilContext.insert_kind
  val insert_kind_equation = NilContext.insert_kind_equation
  val insert_equation = NilContext.insert_equation

  val find_kind_equation = NilContext.find_kind_equation
  val print_context      = NilContext.print_context

  fun error s = Util.error "normalize.sml" s

  val assert   = NilError.assert
  val locate   = NilError.locate "Normalize"
  val perr_k_k = NilError.perr_k_k



  val debug = ref false
  val show_calls = ref false
  val show_context = ref false

  val warnDepth = 1000
  val maxDepth = 10000


  local
      datatype entry = 
	EXP of exp * (NilContext.context * (con_subst))
      | CON of con * (NilContext.context  * (con_subst))
      | KIND of kind * (NilContext.context * (con_subst))
      | BND of bnd * (NilContext.context * (con_subst))
      | MODULE of module * (NilContext.context * (con_subst))
      val stack = ref ([] : entry list)

      val depth = ref 0
      fun push e = (depth := !depth + 1;
		    stack := (e :: (!stack));
		    if (!depth mod 20 = 0)
			then (print "****normalize.sml: stack depth = ";
			      print (Int.toString (!depth)))
		    else ();
		    if (!depth) > maxDepth
			then error "stack depth exceeded"
		    else ())
  in
    fun clear_stack() = (depth := 0; stack := [])
    fun push_exp (e,context) = push (EXP(e,context))
    fun push_con(c,context) = push(CON(c,context))
    fun push_kind(k,context) = push(KIND(k,context))
    fun push_bnd(b,context) = push(BND(b,context))
    fun push_mod(m,context) = push(MODULE(m,context))
    fun pop() = (depth := !depth - 1;
		 stack := (tl (!stack)))
    fun show_stack() = let val st = !stack
			   val _ = clear_stack()
			   fun show (EXP(e,(context,s))) = 
			     (print "exp_normalize called with expression =\n";
			      Ppnil.pp_exp e;
			      print "\nand context"; 
			      NilContext.print_context context;
			      print "\n and subst";
			      printConSubst s;
			      print "\n\n")
			     | show (CON(c,(context,s))) =
				     (print "con_normalize called with constructor =\n";
				      Ppnil.pp_con c;
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  printConSubst s;
				      print "\n\n")
			     | show (KIND(k,(context,s))) =
				     (print "kind_normalize called with kind =\n";
				      Ppnil.pp_kind k;
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  printConSubst s;
				      print "\n\n")
			     | show (BND(b,(context,s))) =
				     (print "bnd_normalize called with bound =\n";
				      Ppnil.pp_bnd b;
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  printConSubst s;
				      print "\n\n")
			     | show (MODULE(m,(context,s))) =
				     (print "module_normalize called with module =\n";
				      Ppnil.pp_module 
                                        {module = m,
                                         header = "",
                                         name = "",
                                         pass = ""};
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  printConSubst s;
				      print "\n\n")
		       in  app show (rev st)
		       end
    fun wrap str f = 
	(clear_stack(); f())
	handle e => (print "\n ------ ERROR in "; print str; print " ---------\n";
		     show_stack(); raise e)
    fun wrap1 str f arg1 = wrap str (fn () => f arg1)
    fun wrap2 str f arg1 arg2 = wrap str (fn () => f arg1 arg2)
  end
	 

  fun beta_conrecord' proj = 
    (case C.expose proj
       of Proj_c (con,label) =>
	 (case strip_crecord con
	    of SOME entries =>
	      (case (List.find (fn ((l,_)) => eq_label (l,label))
		     entries )
		 of SOME (l,c) => (true,c)
		  | NONE => (error "Field not in record" handle e => raise e))
	     | NONE => (false,proj))
	| _ =>
	    (Ppnil.pp_con proj;
	     (error "beta_conrecord called on non-projection" handle e => raise e)))

  fun beta_conrecord proj = #2(beta_conrecord' proj)

  val beta_conrecord = subtimer("Norm:beta_conrecord",beta_conrecord)

  fun eta_confun lambda = 
    let
      fun help(var, formals,body,con) =
	(case C.expose con
	   of Var_c var' => 
	     if eq_var(var,var') then 
	       (case strip_app body of
		  SOME (con,actuals) =>
		    let
		      val (vars,_) = unzip formals
		      fun eq (var,con) = eq_opt (eq_var,SOME var,strip_var con)
		    in
		      if (all2 eq (vars,actuals)) andalso
			(let
			   val fvs = con_free_convar con
			 in
			   all (fn v => all (not o (eq_var2 v)) fvs) vars
			 end)
			then con
		      else lambda
		    end
		| NONE => lambda)
	     else lambda
	  | _ => lambda)

      val res = 
	(case C.expose lambda
	   of Let_c (sort,[Open_cb (var,formals,body)],con) => help(var,formals,body,con)
	    | Let_c (sort,[Code_cb (var,formals,body)],con) => help(var,formals,body,con)
	    | _ => lambda)
    in res
    end


  and beta_confun app = #2(beta_confun' app)

  and beta_confun' app =
    (case C.expose app
       of App_c (con,actuals) =>
	 let  
	   
	   exception NOT_A_LAMBDA
	   
	   fun strip (Open_cb (var,formals,body)) = (var,formals,body)
	     | strip (Code_cb (var,formals,body)) = (var,formals,body)
	     | strip _ = raise NOT_A_LAMBDA
	     
	   fun get_lambda (lambda,name) = 
	     let
	       val (var,formals,body) = strip lambda
	     in
	       (case C.expose name
		  of Var_c var' => 	  
		    if eq_var (var,var') then
		      (formals,body)
		    else raise NOT_A_LAMBDA
		   | _ => raise NOT_A_LAMBDA)
	     end
	   
	   fun lambda_or_closure c = 
	     (case C.expose c
		of Let_c (_,[lambda],name) => (get_lambda (lambda,name),NONE)
		 | Closure_c(code,env) =>
		  let val (args,_) = lambda_or_closure code
		  in  (args,SOME env) end
		 | _ => raise NOT_A_LAMBDA)
	       
	       
	   fun open_lambda cfun = (SOME (lambda_or_closure cfun)) handle NOT_A_LAMBDA => NONE
	     
	   fun reduce actuals (formals,body) = 
	     (true,
	      let
		val (vars,_) = unzip formals
		val subst = fromList (zip vars actuals)
	      in substConInCon subst body
	      end) 
	     
	     
	 in
	   (case open_lambda con
	      of SOME(args,SOME env) =>
		reduce (env::actuals) args
	       | SOME(args,NONE)     => 
		reduce actuals args
	       | NONE => (false,app))
	 end
       | _ =>
	 (Ppnil.pp_con app;
	  (error "beta_confun called on non-application" handle e => raise e)))

  fun lab2int l ~1 = error "lab2int failed"
    | lab2int l n = if (eq_label(l,NilUtil.generate_tuple_label n))
		      then n else lab2int l (n-1)



  (* ------ Reduce one step if not in head-normal-form; return whether progress was made  ------ *)
  datatype progress = PROGRESS | HNF | IRREDUCIBLE
  datatype 'a ReduceResult = REDUCED of 'a | UNREDUCED of con

    fun is_hnf c : bool = 
        (case C.expose c of
             Prim_c(pc,clist) => true
           | AllArrow_c _ => true
           | ExternArrow_c _ => true
           | Var_c _ => false
           | Let_c _ => false
           | Mu_c _ => true
           | Proj_c (c,_) =>
	       (case C.expose c
		  of Mu_c _ => true
		   | _ => false)
	   | Typeof_c _ => false
           | App_c _ => false
           | Crecord_c _ => true
           | Closure_c _ => error "Closure_c not a type"
           | Typecase_c _ => false)


    fun expandMuType(D:context, mu_con:con) =
	let 
	       
	  fun extract mu_tuple_con (defs,which) =
	    let 
	      val var' = Name.fresh_named_var "mu_bnd"
	      val defs = Sequence.toList defs
	      fun mapper (n,(v,_)) = 
		if (length defs = 1) 
		  then Con_cb (v,C.Var_c var')
		else Con_cb (v,C.Proj_c(C.Var_c var',NilUtil.generate_tuple_label(n+1)))
	      val bnds = Listops.mapcount mapper defs
	      val bnds = (Con_cb (var',mu_tuple_con))::bnds
	      val (_,c) = List.nth(defs,which-1)
	      val con = makeLetC bnds c
	    in  con
	    end

	in 
	  case C.expose (#2(reduce_hnf(D,mu_con))) of  
	    (Mu_c (_,defs)) => extract mu_con (defs,1)
	  | (Proj_c (mu_tuple, l))  => 
	      (case C.expose mu_tuple
		 of Mu_c (_,defs) => extract mu_tuple (defs, lab2int l (Sequence.length defs))
		  | _ => error "expandMuType reduced to projection from non-mu type")
	  | _ => error "expandMuType reduced to non-mu type"
	end


  and con_reduce_letfun state (sort,coder,var,formals,body,rest,con) = 
	    let
	      val (D,subst) = state
	      val lambda = (C.Let_c (sort,[coder (var,formals,body)],C.Var_c var))
	    in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var con) 
		   then (HNF, subst, lambda)
	       else
		   let 
		       val _ = if (!debug) 
				   then (case (substitute subst var)  of
					SOME c => error "XXX var already in subst"
				      | _ => ())
				else ()
		       val subst = add subst (var,substConInCon subst lambda)
		   in  (PROGRESS,subst,C.Let_c(sort,rest,con))
		   end
	    end

  and con_reduce state (constructor : con) : progress * con_subst * con  = 
    (case C.expose constructor 
       of (Prim_c (Vararg_c (openness,effect),[argc,resc])) => 
	 let 
	   val irreducible = Prim_c(Vararg_c(openness,effect),[argc,resc])
	   val no_flatten = AllArrow_c{openness=openness,effect=effect,isDependent=false,
				       tFormals=[],eFormals=[(NONE,argc)],fFormals=0w0,
				       body_type=resc}
	   val (prg,subst,argc) = con_reduce state argc 
	 in
	   (case prg
	      of PROGRESS => (PROGRESS,subst,C.Prim_c (Vararg_c (openness,effect),[argc,resc]))
	       | HNF => 
		(HNF,#2 state,
		 (case C.expose argc
		    of Prim_c(Record_c (labs,_),cons) =>
		      if (length labs > !number_flatten) then C.hide no_flatten 
		      else C.AllArrow_c{openness=openness,effect=effect,isDependent=false,
					tFormals=[], eFormals=map (fn c => (NONE,c)) cons,
					fFormals=0w0, body_type=resc}
		     | _         => C.hide no_flatten))
	       | IRREDUCIBLE => 
		(case find_kind_equation(#1 state,argc)
		   of SOME argc => (PROGRESS,#2 state,C.Prim_c (Vararg_c (openness,effect),[argc,resc]))
		    | NONE      => (HNF,#2 state,C.hide irreducible)))
	 end
	| (Prim_c _) => (HNF, #2 state, constructor)
	| (Mu_c _) => (HNF, #2 state, constructor)
	| (AllArrow_c _) => (HNF, #2 state, constructor)
	| (ExternArrow_c _) => (HNF, #2 state, constructor)
	| (Var_c var) => 
	 let val (D,subst) = state
	 in  (case (substitute subst var) of
		   SOME c => (PROGRESS, subst, c)
		 | NONE => (IRREDUCIBLE, subst, constructor))
	 end

        | (Let_c (sort,((cbnd as Open_cb (var,formals,body))::rest),con)) =>
	 con_reduce_letfun state (sort,Open_cb,var,formals,body,rest,con)

        | (Let_c (sort,((cbnd as Code_cb (var,formals,body))::rest),con)) =>
	 con_reduce_letfun state (sort,Code_cb,var,formals,body,rest,con)

	| (Let_c (sort,cbnd as (Con_cb(var,con)::rest),body)) =>
	    let val (D,subst) = state
		val subst = addr (subst,var,con)
	    in  (PROGRESS,subst,C.Let_c(sort,rest,body))
	    end
	| (Let_c (sort,[],body)) => (PROGRESS,#2 state,body)
	| (Closure_c (c1,c2)) => 
	    let val (progress,subst,c1) = con_reduce state c1 
	    in  (progress,subst,C.Closure_c(c1,c2))
	    end
	| (Crecord_c _) => (HNF,#2 state,constructor)
	| Typeof_c e => (PROGRESS, #2 state, type_of(#1 state,e))
	| Proj_c (c,lab) =>
	    (case C.expose c
	       of Mu_c _ => (HNF,#2 state,constructor)
		| _ => 
		 (case con_reduce state c of
		    (PROGRESS,subst,c) => (PROGRESS,subst,C.Proj_c(c,lab))
		  | (_,_,c) => 
		      let val (D,subst) = state
			val (progress,con) = beta_conrecord' (C.Proj_c (c,lab))
		      in  if progress
			    then (PROGRESS,subst,con)
			  else (IRREDUCIBLE,subst,con)
		      end))
	| (App_c (cfun,actuals)) => 
	       (case con_reduce state cfun of
		    (PROGRESS,subst,c) => (PROGRESS,subst,C.App_c(c,actuals))
		  | (_,_,c) => 
			let val (D,subst) = state
			    val (progress,con) = beta_confun'(C.App_c(c,actuals))
			in  if progress
				then (PROGRESS,subst,con)
			    else (IRREDUCIBLE,subst,con)
			end)
	| (Typecase_c {arg,arms,default,kind}) => error "typecase not done yet")


    and reduce_once (D,con) = let val (progress,subst,c) = con_reduce(D,empty()) con
			      in  (case progress 
				     of IRREDUCIBLE => (case find_kind_equation (D,c) 
							  of SOME c => substConInCon subst c
							   | NONE => substConInCon subst c)
				      | _ => substConInCon subst c)
			      end
    and reduce_until (D,pred,con) = 
        let fun loop n (subst,c) = 
            let val _ = if (n>1000) then error "reduce_until exceeded 1000 reductions" else ()
	    in  case (pred c) of
                SOME info => REDUCED(valOf(pred (substConInCon subst c)))
	      | NONE => let val (progress,subst,c) = con_reduce(D,subst) c
			in  case progress of
				PROGRESS => loop (n+1) (subst,c) 
			      | HNF => (case pred (substConInCon subst c) of
		                    SOME _ => REDUCED(valOf(pred (substConInCon subst c)))
				  | NONE => UNREDUCED (substConInCon subst c))
			      | IRREDUCIBLE => 
				  (case find_kind_equation(D,c) 
				     of SOME c => loop (n+1) (subst,c)
				      | NONE => UNREDUCED (substConInCon subst c))
			end
	    end
        in  loop 0 (empty(),con)
        end
    and reduce_hnf (D,con) = 
        let fun loop n (subst,c) = 
            let val _ = if (n>maxDepth) 
			    then (print "reduce_hnf exceeded max reductions\n";
				  Ppnil.pp_con (substConInCon subst c); print "\n\n";
				  error "reduce_hnf exceeded max reductions")
			 else ()
		val _ = if (n > 0 andalso n mod warnDepth = 0) 
			    then (print "reduce_hnf at iteration #";
				  print (Int.toString n);
				  print "\n")
			else ()
	        val (progress,subst,c) = con_reduce(D,subst) c  
	    in  case progress of
			 PROGRESS => loop (n+1) (subst,c) 
		       | HNF => (true, substConInCon subst c)
		       | IRREDUCIBLE => 
			   (case find_kind_equation(D,c)
			      of SOME c => loop (n+1) (subst,c)
			       | NONE => (false, substConInCon subst c))
	    end
        in  loop 0 (empty(),con)
        end
    and reduce_hnf' (D,con,subst) = 
      let 
	fun loop (subst,c) = 
	  let 
	    val (progress,subst,c) = con_reduce(D,subst) c  
	  in  case progress of
	    PROGRESS => loop (subst,c) 
	  | HNF => (subst,c)
	  | IRREDUCIBLE => 
	      (case find_kind_equation(D,c) of
		 SOME c => loop (subst,c)
	       | NONE => (subst,c))
	  end
      in  loop (subst,con)
      end
      

    and projectTuple(D:context, c:con, l:label) = 
	(case (reduce_hnf(D,C.Proj_c(c,l))) of
	   (true,c)  => c
	 | (false,c) => c)

    and removeDependence vclist c = 
	let fun loop subst [] = substExpInCon subst c
	      | loop subst ((v,c)::rest) = 
	           let val e = E.hide (Raise_e(NilUtil.match_exn,c))
		   in  loop (NilSubst.E.addr (subst,v,e)) rest
		   end
	in  loop (NilSubst.E.empty()) vclist
	end

    and projectRecordType(D:context, c:con, l:label) = 
	(case C.expose (#2(reduce_hnf(D,c))) of
	   Prim_c(Record_c (labs,SOME vars), cons) =>
	     let fun loop _ [] = error "projectRecordType could not find field"
		   | loop rev_vclist ((ll,v,c)::rest) = 
	       if (eq_label(l,ll))
		 then removeDependence (rev rev_vclist) c
	       else loop ((v,c)::rev_vclist) rest
	     in  loop [] (Listops.zip3 labs vars cons)
	     end
	 | Prim_c(Record_c (labs,_), cons) =>
	     (case (Listops.assoc_eq(eq_label,l,Listops.zip labs cons)) of
		NONE => error "projectRecordType could not find field"
	      | SOME c => c)
	 | c => (print "projectRecordType reduced to non-record type = \n";
		 Ppnil.pp_con (C.hide c); print "\n";
		 error "projectRecordType reduced to non-record type"))

    and reduceToSumtype(D: context, c:con) = 
      let
	val con = #2(reduce_hnf(D,c))
      in
	case C.expose con of
	  Prim_c(Sum_c {tagcount,totalcount,known}, [carrier]) => 
	    if (TilWord32.equal(totalcount,TilWord32.uplus(tagcount,0w1))) 
	      then (tagcount,known,[carrier])
	    else (case C.expose (#2(reduce_hnf(D,carrier))) of
		    Crecord_c lcons => (tagcount,known,map #2 lcons)
		  | _ => error "reduceToSumtype failed to reduced carrier to a crecord")
	| _ => (print "reduceToSumtype failed to reduced argument to sumtype\n";
	       Ppnil.pp_con con; print "\n";
	       error "reduceToSumtype failed to reduced argument to sumtype")
      end

    and projectSumType(D:context, c:con, s:TilWord32.word) = 
      let val con = #2(reduce_hnf(D,c))
      in
	case C.expose con 
	  of Prim_c(Sum_c {tagcount,totalcount,known}, cons) =>
	    if (TilWord32.ult(s,tagcount))
	      then error "projectSumType: asking for tag fields"
	    else 
	      let val nontagcount = TilWord32.toInt(TilWord32.uminus(totalcount,tagcount))
		val which = TilWord32.toInt(TilWord32.uminus(s,tagcount))
	      in  case (nontagcount,which) of
		(0,_) => error "projectSumType: only tag fields"
	      | (1,0) => hd cons
	      | _ => (projectTuple(D,hd cons, NilUtil.generate_tuple_label (which + 1))
		      handle e => (print "projectSumtype - unable to reduce Tuple\n";
				   NilContext.print_context D;
				   raise e))
	      end
	   | _ => (print "projectSumType reduced to non-sum type = \n";
		   Ppnil.pp_con con; print "\n";
		   error "projectSumType reduced to non-sum type")
      end

   and reduce_vararg(D:context,openness,effect,argc,resc) = 
       let 
	 val irreducible = C.Prim_c(Vararg_c(openness,effect),[argc,resc])
	 val no_flatten = C.AllArrow_c{openness=openness,effect=effect,isDependent=false,
				       tFormals=[],eFormals=[(NONE,argc)],fFormals=0w0,
				       body_type=resc}
	 val (hnf,con) = reduce_hnf(D,argc)
       in  case (hnf,C.expose con) 
	     of (_,Prim_c(Record_c (labs,_),cons)) => 
	       if (length labs > !number_flatten) then no_flatten 
	       else C.AllArrow_c{openness=openness,effect=effect,isDependent=false,
				 tFormals=[], eFormals=map (fn c => (NONE,c)) cons,
				 fFormals=0w0, body_type=resc}
	      | (true,_) => no_flatten
	      | _ => irreducible
       end


   and type_of_switch (D:context,switch:switch):con  = 
     (case switch of
	   Intsw_e {result_type,...} => result_type
	 | Sumsw_e {result_type,...} => result_type
	 | Exncase_e {result_type,...} => result_type
         | Typecase_e {result_type,...} => result_type)

   and type_of_value (D,value) = 
     (case value 
	of int (intsize,_) => C.Prim_c (Int_c intsize,[])
	 | uint (intsize,_) => C.Prim_c (Int_c intsize,[])
	 | float (floatsize,string) => C.Prim_c (Float_c floatsize,[])
	 | array (con,arr) => C.Prim_c (Array_c,[con])
	 | vector (con,vec) => C.Prim_c (Vector_c,[con])
	 | refcell expref => C.Prim_c (Array_c,[type_of(D,!expref)])
	 | tag (atag,con) => C.Prim_c (Exntag_c,[con]))

   and type_of_fbnd (D,openness,constructor,defs) = 
     let
       val def_list = Sequence.toList defs
       val (vars,functions) = unzip def_list
       val declared_c = map (NilUtil.function_type openness) functions
       val bnd_types = zip vars declared_c
       val D = NilContext.insert_con_list (D,bnd_types)
     in
       ((bnd_types,[]),D)
     end
   and type_of_bnds (D,bnds) = 
     let
       fun folder (bnd,D) = 
	 (case bnd of
	    Con_b (phase, cbnd) => 
	      let val (v,c) = 
		(case cbnd of
		   Con_cb (v,c) => (v,c)
		 | Open_cb(v,vklist,c) => (v,C.Let_c(Sequential,[cbnd],C.Var_c v))
		 | Code_cb(v,vklist,c) => (v,C.Let_c(Sequential,[cbnd],C.Var_c v)))
		  val D = NilContext.insert_equation(D,v,c)
	      in  (([],[cbnd]),D)
	      end
	  | Exp_b (var, _, exp) =>
	      let
		val con = type_of (D,exp)
		val D = NilContext.insert_con(D,var,con)
	      in
		(([(var,con)],[]),D)
	      end
	  | (Fixopen_b defs) => (type_of_fbnd(D,Open,Fixopen_b,defs))
	  | (Fixcode_b defs) => (type_of_fbnd(D,Code,Fixcode_b,defs))
	  | Fixclosure_b (is_recur,defs) => 
	      let
		val defs_l = Sequence.toList defs
		val defs_l = Listops.map_second (fn cl => #tipe cl) defs_l
		val D = NilContext.insert_con_list (D,defs_l)
	      in 
		((defs_l,[]),D)
	      end)
       val (bnds,D) = foldl_acc folder D bnds
       val (etypes_l,cbnds_l) = unzip bnds
       val cbnds = List.concat cbnds_l
       val etypes = List.concat etypes_l
     in
       (D,etypes,cbnds)
     end

   and type_of_prim (D,prim,cons,exps) = 
        let
	   fun specialize_sumtype (known,sumcon) = 
	       let val (_,sumcon_hnf) = reduce_hnf(D,sumcon)
	       in  NilUtil.convert_sum_to_special(sumcon_hnf,known)
	       end
	in
	 (case prim of
	    record labs => C.Prim_c(Record_c (labs,NONE), map (fn e => type_of(D,e)) exps)
	  | select lab => projectRecordType(D,type_of(D,hd exps),lab)
	  | inject s => specialize_sumtype(s,hd cons)
	  | inject_known s => specialize_sumtype(s,hd cons)
	  | inject_known_record s => specialize_sumtype(s,hd cons)
	  | project s => projectSumType(D,hd cons, s)
	  | project_known s => projectSumType(D,hd cons, s)
	  | project_known_record (s,lab) => let val summandType = projectSumType(D,hd cons, s)
					  in  projectRecordType(D,summandType,lab)
					  end
	  | box_float fs => C.Prim_c(BoxFloat_c fs,[])
	  | unbox_float fs => C.Prim_c(Float_c fs,[])
	  | roll => hd cons
	  | unroll => expandMuType(D,hd cons)
	  | make_exntag => C.Prim_c(Exntag_c, cons)
	  | inj_exn _ => C.Prim_c(Exn_c, [])
	  | make_vararg (openness,effect) => 
	       let val [argc,resc] = cons
	       in  reduce_vararg(D,openness,effect,argc,resc)
	       end
	  | make_onearg (openness,effect) => 
	       let val [argc,resc] = cons
	       in  C.AllArrow_c{openness=openness,effect=effect,isDependent=false,
				tFormals=[],eFormals=[(NONE,argc)],fFormals=0w0,body_type=resc}
	       end
	  | peq => error "peq not done")
	end

   and type_of (D : context,exp : exp) : con = 
     let val _ = if (!debug)
		     then (print "XXX type_of on ";
			   Ppnil.pp_exp exp;
			   print "\n")
		 else ()
     in
       (case E.expose exp 
	  of Var_e var => 
	    (NilContext.find_con (D,var)
	     handle Unbound =>
	       error 
	       ("Encountered undefined variable " ^ (Name.var2string var) 
		^ "in type_of"))
	   | Const_e value => type_of_value (D,value)
	   | Let_e (letsort,bnds,exp) => 
	    let
	      val (D,etypes,cbnds) = type_of_bnds (D,bnds)
	      val c = type_of (D,exp)
	    in
	      removeDependence etypes (C.Let_c (Sequential,cbnds,c))
	    end
	   | Prim_e (NilPrimOp prim,cons,exps) => type_of_prim (D,prim,cons,exps)
	   | Prim_e (PrimOp prim,cons,exps) =>   
	    let 
	      val (total,arg_types,return_type) = NilPrimUtil.get_type prim cons
	    in return_type
	    end
	   | Switch_e switch => type_of_switch (D,switch)
	   | ExternApp_e (app,texps) =>
	    let val app_con : con = type_of (D,app)
	    in  (case C.expose (#2(reduce_hnf(D,app_con))) of
		   ExternArrow_c(_,c) => c
		 | c => (print "Ill Typed expression - not an arrow type. c = \n";
			 Ppnil.pp_con app_con;
			 print "\nreduce to = \n";
			 Ppnil.pp_con (C.hide c);
			 print "\nexp = \n";
			 Ppnil.pp_exp app;
			 print "\n";
			 error "Ill Typed expression - not an arrow"))
	    end	   
	   | (App_e (openness,app,cons,texps,fexps)) =>
	    let
	      val app_con : con = type_of (D,app)
	      val  (tformals,eformals,body) = 
		(case C.expose (#2(reduce_hnf(D,app_con))) of
		     AllArrow_c{tFormals,eFormals,body_type,...} => (tFormals,eFormals,body_type)
		   | Prim_c(Vararg_c _, [_,c]) => ([],[],c)
		   | c => (print "Ill Typed expression - not an arrow type.\n app_con = \n";
			      Ppnil.pp_con app_con;
			      print "\nreduce to = \n";
			      Ppnil.pp_con (C.hide c);
			      print "\nexp = \n";
			      Ppnil.pp_exp app;
			      print "\n";
			      error "Ill Typed expression - not an arrow"))

	      val subst = fromList (zip (#1 (unzip tformals)) cons)
	      val con = substConInCon subst body
		  
	    in  removeDependence 
		  (map (fn (SOME v,c) => (v,substConInCon subst c)
		         | (NONE, c) => (fresh_var(), c)) eformals)
		  con
	    end

	   | Raise_e (exp,con) => con
	   | Handle_e {result_type,...} => result_type
	    )
     end

  val type_of = fn args => type_of args


  local
    fun bind_kind_eqn'((D,alpha),var,con,kind) =
      if NilContext.bound_con (D,var) then
	let
	  val vnew = Name.derived_var var
	  val D = insert_kind_equation(D,vnew,con,kind)
	  val alpha = Alpha.rename(alpha,var,vnew)
	in (D,alpha)
	end
      else (insert_kind_equation(D,var,con,kind),alpha)
	
    fun bind_kind_eqn(state as (D,alpha),var,con,kind) = 
      bind_kind_eqn'(state,var,alphaCRenameCon alpha con,alphaCRenameKind alpha kind)
      
    fun bind_eqn(state as (D,alpha),var,con) = 
      let val con = alphaCRenameCon alpha con
      in
	if NilContext.bound_con (D,var) then
	  let
	    val vnew = Name.derived_var var
	    val D = insert_equation(D,vnew,con)
	    val alpha = Alpha.rename(alpha,var,vnew)
	  in (D,alpha)
	  end
	else (insert_equation(D,var,con),alpha)
      end
    
    fun instantiate_formals(state,vklist,actuals) = 
      let
	fun folder ((var,k),actual,state) = bind_kind_eqn(state,var,actual,k)
      in foldl2 folder state (vklist,actuals)
      end
  in
    fun context_beta_reduce_letfun (state,sort,coder,var,formals,body,rest,con) = 
      let
	val lambda = (C.Let_c (sort,[coder (var,formals,body)],C.Var_c var))
      in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var con) 
	   then (state,lambda,false)
	 else
	   context_beta_reduce(bind_eqn(state,var,lambda),C.Let_c(sort,rest,con))
      end
    
    and context_beta_reduce (state : (context * Alpha.alpha_context),constructor : con) 
      : ((context * Alpha.alpha_context) * con * bool)  = 
      (case C.expose constructor of
	 (Prim_c (Vararg_c (p,e),[argc,resc])) =>       
	   let 
	     val (state,argc,path) = context_reduce_hnf''(state,argc)
	       
	     val irreducible = Prim_c(Vararg_c(p,e),[argc,resc])
	     val no_flatten  = AllArrow_c{openness=p,effect=e,isDependent=false,
					  tFormals=[],eFormals=[(NONE,argc)],fFormals=0w0,
					  body_type=resc}
	     val res = 
	       (case C.expose argc of
		  Prim_c(Record_c (labs,_),cons) => 
		    if (length labs > !number_flatten) then C.hide no_flatten 
		    else C.AllArrow_c{openness=p,effect=e,isDependent=false,
				      tFormals=[], eFormals=map (fn c => (NONE,c)) cons,
				      fFormals=0w0, body_type=resc}
		| _ => if path then C.hide irreducible
		       else C.hide no_flatten)
	   in (state,res,false)
	   end
       | (Prim_c _)            => (state,constructor,false)
       | (Mu_c _)              => (state,constructor,false)
       | (AllArrow_c _)        => (state,constructor,false)
       | (ExternArrow_c _)     => (state,constructor,false)
       | (Crecord_c _)         => (state,constructor,false)
       | (Var_c var)           => (state,constructor,true)
       | (Proj_c (c,lab))      => 
	   (case C.expose c
	      of (Mu_c _)      => (state,constructor,false)
	       | _             => 
		(case context_beta_reduce (state,c) of
		   (state,constructor,true)  => (state,C.Proj_c(constructor,lab),true)
		 | (state,constructor,false) => 
		     let val field = 
		       (case strip_crecord constructor
			  of SOME entries =>
			    (case (List.find (fn ((l,_)) => eq_label (l,lab))
				   entries )
			       of SOME (l,c) => c
				| NONE => error (locate "context_beta_reduce") "Field not in record")
			   | NONE => error (locate "context_beta_reduce") "Proj from non-record")
		     in context_beta_reduce(state,field)
		     end))
	      
	      
       | (Let_c (sort,((cbnd as Open_cb (var,formals,body))::rest),con)) =>
	  context_beta_reduce_letfun (state,sort,Open_cb,var,formals,body,rest,con)
	      
       | (Let_c (sort,((cbnd as Code_cb (var,formals,body))::rest),con)) =>
	  context_beta_reduce_letfun (state,sort,Code_cb,var,formals,body,rest,con)
	      
       | (Let_c (sort,cbnd as (Con_cb(var,con)::rest),body))             =>
	  context_beta_reduce(bind_eqn(state,var,con),C.Let_c(sort,rest,body))
	      
       | (Let_c (sort,[],body)) => context_beta_reduce(state,body)
	      
       | (Closure_c (c1,c2)) => 
	  let val (state,c1,path) = context_beta_reduce (state,c1)
	  in  (state,C.Closure_c(c1,c2),path)
	  end
	
       | Typeof_c e => 
	  let val (D,alpha) = state
	  in context_beta_reduce((D,Alpha.empty_context()),type_of(D,alphaCRenameExp alpha e))
	  end
       | (App_c (cfun,actuals)) => 
	  (case context_beta_reduce (state,cfun) of
	     (state,constructor,true)  => (state,C.App_c(constructor,actuals),true)
	   | (state,constructor,false) => 
	       let
		 exception NOT_A_LAMBDA
		 
		 fun strip (Open_cb (var,formals,body)) = (var,formals,body)
		   | strip (Code_cb (var,formals,body)) = (var,formals,body)
		   | strip _ = raise NOT_A_LAMBDA
		   
		 fun get_lambda (lambda,name) = 
		   let
		     val (var,formals,body) = strip lambda
		   in
		     (case C.expose name
			of Var_c var' => 	  
			  if eq_var (var,var') then
			    (formals,body)
			  else raise NOT_A_LAMBDA
			 | _ => raise NOT_A_LAMBDA)
		   end
		 
		 fun lambda_or_closure c = 
		   (case C.expose c
		      of Let_c (_,[lambda],name) => (get_lambda (lambda,name),NONE)
		       | Closure_c(code,env) =>
			let val (args,_) = lambda_or_closure code
			in  (args,SOME env) end
		       | _ => raise NOT_A_LAMBDA)			    
		      
		 fun open_lambda cfun = (SOME (lambda_or_closure cfun)) handle NOT_A_LAMBDA => NONE
		   
	       in
		 (case open_lambda constructor
		    of SOME((formals,body),SOME env) =>
		      context_beta_reduce(instantiate_formals(state,formals,env::actuals),body)
		     | SOME((formals,body),NONE)     => 
		      context_beta_reduce(instantiate_formals(state,formals,actuals),body)
		     | NONE => (error (locate "context_beta_reduce") "redex not in HNF"))
	       end)
	     
       | (Typecase_c {arg,arms,default,kind}) => 
	  let
	    val (state,arg)   = context_reduce_hnf'(state,arg)
	  in
	    (case strip_prim arg
	       of SOME (pcon,args) =>
		 (case List.find (fn (pcon',formals,body) => primequiv (pcon,pcon')) arms
		    of SOME (_,formals,body) => context_beta_reduce(instantiate_formals(state,formals,args),body)
		     | NONE => (state,default,false))
		| _ => (state,C.hide (Typecase_c{arg=arg,arms=arms,default=default,kind=kind}),false))
	  end)
    and context_reduce_hnf'' (state,constructor) =
      let val ((D,alpha),con,path) = context_beta_reduce (state,constructor)
      in
	if path then
	  let val con = alphaCRenameCon alpha con
	  in
	    (case find_kind_equation (D,con)
	       of SOME con => context_reduce_hnf'' ((D,Alpha.empty_context()),con)
		| NONE => ((D,Alpha.empty_context()),con,true))
	  end
	else
	  ((D,alpha),con,false)
      end
    
    and context_reduce_hnf' args = 
      let val (state,con,path) = context_reduce_hnf'' args
      in (state,con)
      end

    fun context_reduce_hnf(D,constructor) = 
      let val ((D,alpha),con) = context_reduce_hnf'((D,Alpha.empty_context()),constructor)
      in (D,alphaCRenameCon alpha con)
      end
    
  end

  val reduce_hnf = wrap1 "reduce_hnf" reduce_hnf
  val reduce_once = wrap1 "reduce_once" reduce_once
  val reduce_until = fn arg => wrap "reduce_until" (fn () => reduce_until arg)

  val beta_conrecord = wrap1 "beta_conrecord" beta_conrecord
  val beta_confun = wrap1 "beta_confun" beta_confun
  val eta_confun = wrap1 "eta_confun" eta_confun

  val reduceToSumtype = wrap1 "reduceToSumType" reduceToSumtype
  val type_of = fn arg => (wrap1 "type_of" type_of arg
			   handle e => (print "type_of failed on ";
					Ppnil.pp_exp (#2 arg); raise e))

  fun nilprim_uses_carg np =
	(case np of
	     record _ => false
	   | select _ => false
	   | roll => false
	   | unroll => false
	   | project_known_record _ => false
	   | project_known _ => false
	   | project _ => true
	   | inject_known_record _ => false
	   | inject_known _ => false
	   | inject _ => true
           | box_float _ => false
           | unbox_float _ => false
           | make_exntag => false
           | inj_exn _ => false
           | make_vararg _ => true
           | make_onearg _ => true
           | peq => true)

  fun aggregate_uses_carg (Prim.OtherArray false) = true
    | aggregate_uses_carg (Prim.OtherVector false) = true
    | aggregate_uses_carg _ = false

  fun prim_uses_carg p =
	(case p of
	     array2vector t => aggregate_uses_carg t
	   | vector2array t => aggregate_uses_carg t
	   |  create_table t => aggregate_uses_carg t
	   |  create_empty_table t => aggregate_uses_carg t
	   |  sub t => aggregate_uses_carg t
	   |  update t => aggregate_uses_carg t
	   |  length_table t => aggregate_uses_carg t
	   |  equal_table t => aggregate_uses_carg t
	   | _ => true)

  fun allprim_uses_carg (NilPrimOp np) = nilprim_uses_carg np
    | allprim_uses_carg (PrimOp p) = prim_uses_carg p

end