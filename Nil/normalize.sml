(*$import Prelude TopLevel Name List Sequence Listops Int Util NilError NilRename Stats Prim Array TilWord32 Alpha Nil Ppnil NilUtil NilContextPre NilSubst NORMALIZE *)

structure Normalize :> NORMALIZE =
struct	

  structure NilContext = NilContextPre


  val number_flatten = Nil.flattenThreshold

  val profile       = Stats.ff "nil_profile"
  val local_profile = Stats.ff "normalize_profile"

  val subtimer = fn args => fn args2 => if !profile orelse !local_profile then Stats.subtimer args args2 else #2 args args2
    
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
  val printConSubst = NilSubst.C.print

  val makeLetC             = NilUtil.makeLetC
  val map_annotate         = NilUtil.map_annotate
  val strip_annotate       = NilUtil.strip_annotate
  val is_var_c             = NilUtil.is_var_c
  val strip_var            = NilUtil.strip_var
  val strip_crecord        = NilUtil.strip_crecord
  val strip_proj           = NilUtil.strip_proj
  val strip_prim           = NilUtil.strip_prim
  val strip_app            = NilUtil.strip_app
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

  val find_kind_equation = NilContext.find_kind_equation
  val print_context      = NilContext.print_context

  val insert_kind          = NilContext.insert_kind
  val insert_kind_equation = NilContext.insert_kind_equation
  val insert_equation      = NilContext.insert_equation

  fun error s = Util.error "normalize.sml" s

  val assert   = NilError.assert
  val locate   = NilError.locate "Normalize"
  val perr_k_k = NilError.perr_k_k



  val debug = ref false
  val show_calls = ref false
  val show_context = ref false

  fun pull (c,kind) = 
    let open Nil NilUtil Name Util
    in  (case kind of
          Type_k => c
	| SingleType_k c2 => c2
	| Single_k c2 => c2
	| Record_k elts => 
	 let
	   fun folder (((label,var),kind),subst) = 
	     let
	       val kind = substConInKind subst kind
	       val con = pull (Proj_c (c,label),kind)
	       val subst = add subst (var,con)
	     in
	       ((label,con),subst)
	     end
	   val (entries,subst) = Listops.foldl_acc folder (empty()) (Sequence.toList elts)
	 in
	   (Crecord_c entries)
	 end
	| Arrow_k (openness, formals, return) => 
	 let
	   val vars = map (fn (v,_) => (Var_c v)) formals
	   val c = pull (App_c (c,vars),return)
	   val var = fresh_named_var "pull_arrow"
	 in
	   (*Closures?  *)
	   case openness of
	        Open => Let_c (Sequential,[Open_cb (var,formals,c)],Var_c var)
	      | Code => Let_c (Sequential,[Code_cb (var,formals,c)],Var_c var)
	      | Closure => let val cenv = (fresh_named_var "pull_closure", 
					   Record_k (Sequence.fromList []))
			   in  Let_c (Sequential,[Code_cb (var,cenv::formals ,c)],
				      Closure_c(Var_c var, Crecord_c []))
			   end
	 end)
    end

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
	 

  fun beta_conrecord' (proj as Proj_c (con,label)) = 
	(case strip_crecord con
	   of SOME entries =>
	     (case (List.find (fn ((l,_)) => eq_label (l,label))
		    entries )
		of SOME (l,c) => (true,c)
		 | NONE => (error "Field not in record" handle e => raise e))
	    | NONE => (false,proj))
    | beta_conrecord' proj = 
	   (Ppnil.pp_con proj;
	    (error "beta_conrecord called on non-projection" handle e => raise e))

  fun beta_conrecord proj = #2(beta_conrecord' proj)

  val beta_conrecord = subtimer("Norm:beta_conrecord",beta_conrecord)

  fun eta_confun lambda = 
    let
	fun help(var, formals,body) =
	    (case strip_app body of
		 SOME (con,actuals) =>
		     let
			 val (vars,_) = unzip formals
			 fun eq (var,con) = eq_opt (eq_var,SOME var,strip_var con)
		     in
			 if (all2 eq (vars,actuals)) andalso
			     (let
				  val fvs = NilUtil.freeConVarInCon(true,0,con)
			      in  all (fn v => not(Name.VarSet.member(fvs,v))) vars
			      end)
			     then
				 con
			 else
			     lambda
		     end
	       | NONE => lambda)
      fun eta_confun' (Let_c (sort,[Open_cb (var,formals,body)],Var_c var')) = 
	  if (eq_var(var,var')) then help(var,formals,body) else lambda
        | eta_confun' (Let_c (sort,[Code_cb (var,formals,body)],Var_c var')) = 
	  if (eq_var(var,var')) then help(var,formals,body) else lambda
	| eta_confun' _ = lambda
    in
      map_annotate eta_confun' lambda
    end

  and beta_typecase D typecase = 
    let 
      fun beta_typecase' 
	(Typecase_c {arg,arms,default,kind}) = 
	(case strip_prim arg
	   of SOME (pcon,args) =>
	     (case List.find (fn (pcon',formals,body) => primequiv (pcon,pcon')) arms
		of SOME (_,formals,body) => 
		  let
		    val (vars,_) = unzip formals
		    val subst = fromList (zip vars args)
		  in con_normalize' (D,subst) body
		  end
		 | NONE => default)
	    | _ => typecase)
	| beta_typecase' _ = 
	   (Ppnil.pp_con typecase;
	    (error "beta_typecase called on non type case" handle e => raise e))
    in
      map_annotate beta_typecase' typecase
    end


  and beta_confun once D app = #2(beta_confun' once D app)

  and beta_confun' once D (app as (App_c (con,actuals))) =
    let  

      exception NOT_A_LAMBDA

      fun strip (Open_cb (var,formals,body)) = (var,formals,body)
	| strip (Code_cb (var,formals,body)) = (var,formals,body)
	| strip _ = raise NOT_A_LAMBDA

      fun get_lambda (lambda,name) = 
	let
	  val (var,formals,body) = strip lambda
	in
	  (case strip_annotate name
	     of Var_c var' => 	  
	       if eq_var (var,var') then
		 (formals,body)
	       else raise NOT_A_LAMBDA
	      | _ => raise NOT_A_LAMBDA)
	end
      
      fun lambda_or_closure (Let_c (_,[lambda],name)) = (get_lambda (lambda,name),NONE)
	| lambda_or_closure (Closure_c(code,env)) = 
	let val (args,_) = lambda_or_closure code
	in  (args,SOME env) end
	| lambda_or_closure (Annotate_c(_,con)) = lambda_or_closure (con)
	| lambda_or_closure _ = raise NOT_A_LAMBDA


      fun open_lambda cfun = (SOME (lambda_or_closure cfun)) handle NOT_A_LAMBDA => NONE
	
      fun reduce actuals (formals,body) = 
	(true,
	 let
	   val (vars,_) = unzip formals
	   val subst = fromList (zip vars actuals)
	 in if once
	      then substConInCon subst body
	    else (con_normalize' (D,subst) body)
	 end) 
	
	
    in
      (case open_lambda con
	 of SOME(args,SOME env) =>
	       reduce (env::actuals) args
	  | SOME(args,NONE)     => 
	       reduce actuals args
	  | NONE => (false,app))
    end
    | beta_confun' _ _ con =
    (Ppnil.pp_con con;
     (error "beta_confun called on non-application" handle e => raise e))


  and insert_kind (D,var,kind) = NilContext.insert_kind (D,var,kind)
  and bind_at_kind ((var,kind),(D,subst)) =
    let
      val kind = kind_normalize' (D,subst) kind
      val var' = if ((find_kind(D,var); true)
			handle NilContext.Unbound => false)
		     then derived_var var 
		 else var
      val D = insert_kind (D,var',kind)
      val subst = add subst (var,Var_c var')
    in ((var',kind),(D,subst))
    end
  
  and bind_at_kinds state vklist = foldl_acc bind_at_kind state vklist

  and bind_at_con ((var,con),(D,subst)) =
    let
      val con = con_normalize' (D,subst) con
      val var' = if ((find_con(D,var); true)
			handle NilContext.Unbound => false)
		     then derived_var var 
		 else var
      val D = insert_con (D,var',con)
      val subst = add subst (var,Var_c var')
    in ((var',con),(D,subst))
    end

  and bind_at_tracecon ((var,trace,con),state) = 
      let val ((v,c),state) = bind_at_con ((var,con),state)
      in ((v,trace,c),state)
      end
  
  and bind_at_cons state vclist = foldl_acc bind_at_con state vclist
  and bind_at_tracecons state vtclist = foldl_acc bind_at_tracecon state vtclist
 
  and kind_normalize' (state as (D,subst)) (kind : kind) : kind = 
    if !debug then
      let 
	val _ = push_kind(kind,state)
	val _ = if (!show_calls)
		  then (print "kind_normalize called with kind =\n";
			Ppnil.pp_kind kind; 
			(if (!show_context)
			   then (print "\nand context"; NilContext.print_context D;
				 print "\n and subst";  printConSubst subst)
			 else ());
			 print "\n\n")
		else ()
	val res = kind_normalize state kind
	val _ = pop()
      in  res
      end
    else
      kind_normalize state kind
  and kind_normalize state (kind : kind) : kind = 
    (case kind of
          Type_k => kind
	| SingleType_k con => SingleType_k(con_normalize' state con)
	| Single_k con => Single_k(con_normalize' state con)
	| Record_k elts => 
	 let
	   val elt_list = Sequence.toList elts
	   val (labels,vars_and_kinds) = unzip (map (fn ((l,v),k) => (l,(v,k))) elt_list)
	   val (vars_and_kinds,state) =  bind_at_kinds state vars_and_kinds
	   val elts = map2 (fn (l,(v,k)) => ((l,v),k)) (labels,vars_and_kinds)
	 in  
	   Record_k (Sequence.fromList elts)
	 end
	| Arrow_k (openness, formals, return) => 
	 let
	   val (formals,state) = bind_at_kinds state formals
	   val return = kind_normalize' state return
	 in
	   (Arrow_k (openness, formals,return))
	 end)

  and con_normalize' (state as (D,subst)) (con : con) : con = 
    if !debug then
      let 
	val _ = push_con(con,state)
	val _ = if (!show_calls)
		  then (print "con_normalize called with con =\n";
			Ppnil.pp_con con; 
			if (!show_context)
			    then (print "\nand context"; NilContext.print_context D)
			else ();
			print "\n and subst";  printConSubst subst;
			print "\n\n")
		else ()
	val res = con_normalize state con
	val _ = pop()
      in  res
      end
    else
      con_normalize state con

  and con_normalize_letfun state (sort,constructor,var,formals,body,rest,letbody) = 
	    let
	      val old_state = state


	    in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var letbody) 
		   then
		       let val (formals,state) = bind_at_kinds state formals
			   val body = con_normalize' state body
			   val lambda = (Let_c (sort,[constructor (var,formals,body)],
						Var_c var))
			   val lambda = eta_confun lambda
		       in  lambda
		       end
	       else
		   let 

		       val lambda = (Let_c (sort,[constructor (var,formals,body)],
					    Var_c var))
		       val _ = 
			   (case (substitute (#2 old_state) var)  of
				SOME c => error "XXX var already in subst"
			      | _ => ())
		       val lambda = substConInCon (#2 old_state) lambda 
		       val state = 
			   let val (D,subst) = old_state
			   in (D,add subst (var,lambda))
			   end
		       val res = con_normalize' state (Let_c (sort,rest,letbody))

		   in   res
		   end
	    end



  and con_normalize state (constructor : con) : con  = 
    (case constructor of
          Prim_c (Record_c(labs,SOME vars),cons) =>
	      let val (vc,_) = bind_at_cons state (Listops.zip vars cons)
		  val vars = map #1 vc
		  val cons = map #2 vc
	      in Prim_c (Record_c(labs,SOME vars),cons)
	      end
	| (Prim_c (pcon,args)) =>
	      let val args = map (con_normalize' state) args
	      in (Prim_c (pcon,args))
	      end
	| (Mu_c (recur,defs)) =>
	 let
	   val def_list = Sequence.toList defs
	   val (vars,cons) = unzip def_list
	   val (vars_kinds,state') = 
	     bind_at_kinds state (map (fn v => (v,Type_k)) vars)
	   val (vars,_) = unzip vars_kinds
	   val cons = if recur then map (con_normalize' state') cons
		      else map (con_normalize' state) cons
	   val defs = Sequence.fromList (zip vars cons)
	 in Mu_c (recur,defs)
	 end
	| (AllArrow_c {openness,effect,isDependent,
		       tFormals,eFormals,fFormals,body_type}) =>
	 let
	   val (tFormals,state) = bind_at_kinds state tFormals
	   fun folder ((vopt,c),state) = 
	       (case vopt of
		    NONE => ((vopt, con_normalize' state c), state)
		  | SOME v => 
			let val ((v,c),state) = bind_at_con ((v,c),state)
			in  ((SOME v, c), state)
			end)
	   val (eFormals,state) = foldl_acc folder state eFormals
	   val body_type = con_normalize' state body_type
	 in AllArrow_c{openness = openness, effect = effect, isDependent = isDependent,
		       tFormals = tFormals, eFormals = eFormals, 
		       fFormals = fFormals, body_type = body_type}
	 end
	| ExternArrow_c (args,body) => 
	 let 
	   val args = map (con_normalize' state) args
	   val body = con_normalize' state body
	 in
	   ExternArrow_c (args,body)
	 end
	| (Var_c var) => 
	 let
	   val (D,subst) = state
	   val con = 
	     (case (substitute subst var) of
		   SOME c => c
		 | NONE =>
	           (case NilContext.find_kind_equation  (D,Var_c var) of
			SOME c => con_normalize' state c
		      | NONE => Var_c var))
	 in con
	 end
        | (Let_c (sort,((cbnd as Open_cb (var,formals,body))::rest),letbody)) =>
	 con_normalize_letfun state (sort,Open_cb,var,formals,body,rest,letbody)
        | (Let_c (sort,((cbnd as Code_cb (var,formals,body))::rest),letbody)) =>
	 con_normalize_letfun state (sort,Code_cb,var,formals,body,rest,letbody)

	| (Let_c (sort,cbnd as (Con_cb(var,con)::rest),body)) =>
	    let
	      val con = con_normalize' state con
	      val state = 
		let val (D,subst) = state
		in (D,add subst (var,con))
		end
	    in con_normalize' state (Let_c (sort,rest,body))
	    end
	| (Let_c (sort,[],body)) => con_normalize' state body
	| (Closure_c (code,env)) => 
	    let
	      val env = con_normalize' state env 
	      val code =  con_normalize' state code
	    in Closure_c (code,env)
	    end
	| (Crecord_c (entries as orig_entries)) => 
	    let
	      val (labels,cons) = unzip entries
	      val cons = map (con_normalize' state) cons
	      val entries = zip labels cons
	      val con = Crecord_c entries
	    in con
	    end
        | Typeof_c e => error "typeof encountered in con_normalize"
	| (Proj_c (rvals,label)) => 
	    let
	      val rvals = con_normalize' state rvals
	      val con = Proj_c (rvals,label)
	      val con = beta_conrecord con
	    in con
	    end
	| (App_c (cfun,actuals)) => 
	    let
		val (D,subst) = state
	      val cfun = con_normalize' state cfun
	      val actuals = map (con_normalize' state) actuals
	      val con = App_c (cfun,actuals)
	      val con = beta_confun false D con
	    in con
	    end
	| (Typecase_c {arg,arms,default,kind}) => 
	    let
	      val kind = kind_normalize' state kind
	      fun doarm (pcon,args,body) = 
		let
		  val (args,state) = bind_at_kinds state args
		  val body = con_normalize' state body
		in (pcon,args,body)
		end
	      val arg = con_normalize' state arg
	      val default = con_normalize' state default
	      val arms = map doarm arms
	      val con = Typecase_c {arg=arg,arms=arms,
				    default=default,kind=kind}
	      val con = beta_typecase (#1 state) con
	    in con
	    end
	| (Annotate_c (annot,con)) => Annotate_c (annot,con_normalize' state con))

  



  fun value_normalize' state value = 
    (case value of
          (int _) => value
	| (uint _) => value
	| (float _) => value
	| array (con,arr) => 
	 let
	   val con = con_normalize' state con
	   val _ = Array.modify (exp_normalize' state) arr
	 in array (con,arr)
	 end
	| vector (con,vec) => 
	 let
	   val con = con_normalize' state con
	   val _ = Array.modify (exp_normalize' state) vec
	 in vector (con,vec)
	 end
	| refcell expref => 
	 (expref := exp_normalize' state (!expref);
	  refcell expref)
	| tag (atag,con) => 
	 let
	   val con = con_normalize' state con
	 in tag (atag,con)
	 end)
  and switch_normalize' state switch = error "switch_normalize not handled"
(*
    (case switch of
          Intsw_e {size,result_type,arg,arms,default} =>
	 let
	   val arg = exp_normalize' state arg
	   val arms = map_second (function_normalize' state) arms
	   val default = map_opt (exp_normalize' state) default
	 in
	   Intsw_e {size=size,result_type=result_type,
		    arg=arg,arms=arms,default=default}
	 end
	| Sumsw_e {info,arg,arms,default} => 
	 let
	   val arg = exp_normalize' state arg
	   val arms = map_second (function_normalize' state) arms
	   val info = con_normalize' state info
	   val default = map_opt (exp_normalize' state) default
	 in
	   Sumsw_e {info=info,arg=arg,
		    arms=arms,default=default}
	 end
	| Exncase_e {info=_,arg,arms,default} =>
	 let
	   val arg = exp_normalize' state arg
	   val (vars,arm_fns) = unzip arms
	   val arm_fns = map (function_normalize' state) arm_fns
	   val vars = map (exp_normalize' state) vars
	   val arms = zip vars arm_fns
	   val default = map_opt (exp_normalize' state) default
	 in
	   Exncase_e {info=(),arg=arg,
		      arms=arms,default=default}
	 end
	| Typecase_e {info,arg,arms,default} =>
	 let
	   val arg = con_normalize' state arg
	   val arms = map_second (function_normalize' state) arms
	   val default = map_opt (exp_normalize' state) default
	 in
	   Typecase_e {info=(),arg=arg,
		       arms=arms,default=default}
	 end)
*)

  and function_normalize' state (Function {effect,recursive,isDependent,
					   tFormals,eFormals,fFormals,
					   body,body_type}) =
    let
      val (tFormals,state) = bind_at_kinds state tFormals
      val (eFormals,state) = bind_at_tracecons state eFormals
      val body = exp_normalize' state body
      val body_type = con_normalize' state body_type
    in Function{effect = effect , recursive = recursive, isDependent = isDependent,
		tFormals = tFormals, eFormals = eFormals, fFormals = fFormals,
		body = body, body_type = body_type}
    end

  and cfunction_normalize' state (tformals,body) = 
    let
      val (tformals,state) = bind_at_kinds state tformals
      val body = con_normalize' state body
    in (tformals,body)
    end

  and bnds_normalize' state bnds = 
    let
      fun norm_bnd (bnd,state) = bnd_normalize' state bnd
    in
      foldl_acc norm_bnd state bnds
    end

  and bnd_normalize' state (bnd : bnd) =
    (case bnd of
          Con_b (p, cbnd) =>
	 let	
	   val (cbnd,var,kind) =
	   (case cbnd of
		Con_cb (v,c) => 
		let val con = con_normalize' state c
		in  (Con_cb(v,con), v, Single_k con)
		end
	      | Open_cb(v,vklist,c) => 
		let val (vklist,c) = cfunction_normalize' state (vklist,c)
		    val cbnd = Open_cb(v,vklist,c)
		in  (cbnd, v, Single_k(Let_c(Sequential,[cbnd],Var_c v)))
		end
	      | Code_cb(v,vklist,c) =>
		let val (vklist,c) = cfunction_normalize' state (vklist,c)
		    val cbnd = Code_cb(v,vklist,c)
		in  (cbnd, v, Single_k(Let_c(Sequential,[cbnd],Var_c v)))
		end)
	   val bnd = Con_b (p,cbnd)
	   val ((var,kind),state) = bind_at_kind ((var,kind), state)
	 in (bnd,state)
	 end
	| Exp_b (var, tinfo, exp) =>
	 let
	   val exp = exp_normalize' state exp
	   val bnd = Exp_b (var,tinfo,exp)
	 in (bnd,state)
	 end
	| (Fixopen_b defs) =>
	 let val defs = Sequence.map (fn (v,f) => (v,function_normalize' state f)) defs
	 in  (Fixopen_b defs,state)
	 end
	| (Fixcode_b defs) =>
	 let val defs = Sequence.map (fn (v,f) => (v,function_normalize' state f)) defs
	 in  (Fixcode_b defs,state)
	 end
	| Fixclosure_b (is_recur,defs) => 
	 let
	   fun do_closure {code,cenv,venv,tipe} = 
	     let
	       val cenv = con_normalize' state cenv
	       val venv = exp_normalize' state venv
	       val tipe = con_normalize' state tipe
	     in {code=code,cenv=cenv,venv=venv,tipe=tipe}
	     end
	   val defs = Sequence.map (fn (v,c) => (v,do_closure c)) defs
	   val bnd = Fixclosure_b (is_recur,defs)
	 in (bnd,state)
	 end)
  and exp_normalize' (state as (D,subst)) (exp : exp) : exp = 
    if !debug then
      let 
	val _ = push_exp(exp,state)
	val _ = if (!show_calls)
		  then (print "exp_normalize called with expression =\n";
			Ppnil.pp_exp exp; 
			print "\nand context"; NilContext.print_context D;
			print "\n and subst";  printConSubst subst;
			print "\n\n")
		else ()
	val res = exp_normalize state exp
	val _ = pop()
      in  res
      end
    else
      exp_normalize state exp
  and exp_normalize state (exp : exp) : exp = 
    (case exp 
       of Var_e var => exp
	| Const_e value => Const_e (value_normalize' state value)
	| Let_e (letsort,bnds,exp) => 
	 let
	   val (bnds,state) = bnds_normalize' state bnds
	   val exp = exp_normalize' state exp
	 in
	   Let_e (letsort,bnds,exp)
	 end
	| Prim_e (prim,cons,exps) =>   
	 let
	   val cons = map (con_normalize' state) cons
	   val exps = map (exp_normalize' state) exps
	 in Prim_e (prim,cons,exps)
	 end
	| Switch_e switch => Switch_e (switch_normalize' state switch)
	| (App_e (openness,app,cons,texps,fexps)) =>
	 let
	   val app = exp_normalize' state app
	   val cons = map (con_normalize' state) cons
	   val texps = map (exp_normalize' state) texps
	   val fexps = map (exp_normalize' state) fexps
	 in App_e (openness,app,cons,texps,fexps)
	 end
	| ExternApp_e (app,args) => 
	 let
	   val app = exp_normalize' state app
	   val args = map (exp_normalize' state) args
	 in ExternApp_e (app,args)
	 end

	| Raise_e (exp,con) => 
	 let
	   val con = con_normalize' state con
	   val exp = exp_normalize' state exp
	 in Raise_e (exp,con)
	 end
	| Handle_e {body,bound,handler,result_type} =>
	 let
	   val body = exp_normalize' state body
	       (* XXX need to bind bound *)
	   val handler = exp_normalize' state handler
	   val result_type = con_normalize' state result_type
	 in Handle_e {body = body, bound = bound,
		      handler = handler, result_type = result_type}
	 end)
  fun import_normalize' state (ImportValue (label,var,tr,con)) =
    let
      val con = con_normalize' state con
    in
      (ImportValue (label,var,tr,con),state)
    end
    | import_normalize' state (ImportType (label,var,kind)) = 
    let
      val ((var,kind),state) = bind_at_kind ((var,kind),state)
    in
      (ImportType (label,var,kind),state)
    end
  
  fun export_normalize' state (ExportValue (label,v)) = ExportValue (label,v)
    | export_normalize' state (ExportType (label,v)) = ExportType (label,v)

  
  fun module_normalize' state (MODULE {bnds,imports,exports}) = 
    let
      fun folder (import,state) = import_normalize' state import
      val (imports,state) = foldl_acc folder state imports
      val (bnds,state) = bnds_normalize' state bnds
      val exports = map (export_normalize' state) exports
    in
      MODULE {bnds=bnds,imports=imports,exports=exports}
    end
  
  
  fun kind_normalize D = kind_normalize' (D,empty())
  fun con_normalize D = con_normalize' (D,empty())
  fun exp_normalize D = exp_normalize' (D,empty())

  fun module_normalize D = module_normalize' (D,empty())

  val beta_confun = beta_confun false

(*  val kind_normalize = wrap "kind_normalize" kind_normalize
  val con_normalize = wrap "con_normalize"  con_normalize
  val con_reduce_once = wrap "con_reduce_once" con_reduce
  val exp_normalize = wrap "exp_normalize" exp_normalize
  val module_normalize = wrap "mod_normalize" module_normalize

  val kind_normalize' = wrap "kind_normalize'" kind_normalize'
  val con_normalize' = wrap "con_normalize'"  con_normalize'
  val exp_normalize' = wrap "exp_normalize'" exp_normalize'
*)
    fun lab2int l ~1 = error "lab2int failed"
      | lab2int l n = if (eq_label(l,NilUtil.generate_tuple_label n))
			  then n else lab2int l (n-1)



  (* ------ Reduce one step if not in head-normal-form; return whether progress was made  ------ *)
  datatype progress = PROGRESS | HNF | IRREDUCIBLE
  datatype 'a ReduceResult = REDUCED of 'a | UNREDUCED of con

    fun is_hnf c : bool = 
        (case c of
             Prim_c(pc,clist) => true
           | AllArrow_c _ => true
           | ExternArrow_c _ => true
           | Var_c _ => false
           | Let_c _ => false
           | Mu_c _ => true
           | Proj_c (Mu_c _,_) => true
           | Proj_c _ => false
	   | Typeof_c _ => false
           | App_c _ => false
           | Crecord_c _ => true
           | Closure_c _ => error "Closure_c not a type"
           | Typecase_c _ => false
           | Annotate_c (_,c) => false)


    fun expandMuType(D:context, mu_con:con) =
	let 
	       
	  fun extract mu_tuple_con (defs,which) =
	    let 
	      val var' = Name.fresh_named_var "mu_bnd"
	      val defs = Sequence.toList defs
	      fun mapper (n,(v,_)) = 
		  Con_cb (v,Proj_c(Var_c var',NilUtil.generate_tuple_label(n+1)))
	      val bnds = Listops.mapcount mapper defs
	      val bnds = (Con_cb (var',mu_tuple_con))::bnds
	      val (_,c) = List.nth(defs,which-1)
	      val con = makeLetC bnds c
	    in  con
	    end

	in 
	  case #2(reduce_hnf(D,mu_con)) of  
	   Proj_c (mu_tuple as Mu_c (_,defs), l)  => 
	       extract mu_tuple (defs, lab2int l (Sequence.length defs))
	  | c => (print "expandMuType given type not reducible to projection from mu:\n";
		  Ppnil.pp_con c;
		  error "expandMuType does not reduce to projection from a mu type")
	end


  and con_reduce_letfun state (sort,coder,var,formals,body,rest,con) = 
	    let
	      val (D,subst) = state
	      val lambda = (Let_c (sort,[coder (var,formals,body)],Var_c var))
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
		   in  (PROGRESS,subst,Let_c(sort,rest,con))
		   end
	    end

  and con_reduce state (constructor : con) : progress * con_subst * con  = 
    (case constructor of
	 (Prim_c (Vararg_c (openness,effect),[argc,resc])) => 
	 let 
	   val irreducible = Prim_c(Vararg_c(openness,effect),[argc,resc])
	   val no_flatten = AllArrow_c{openness=openness,effect=effect,isDependent=false,
				       tFormals=[],eFormals=[(NONE,argc)],fFormals=0w0,
				       body_type=resc}
	 in
	   (case con_reduce state argc of
	      (PROGRESS,subst,argc) => (PROGRESS,subst,Prim_c (Vararg_c (openness,effect),[argc,resc]))
	    | (HNF,_,Prim_c(Record_c (labs,_),cons)) =>
		(HNF,#2 state,
		 if (length labs > !number_flatten) then no_flatten 
		 else AllArrow_c{openness=openness,effect=effect,isDependent=false,
				 tFormals=[], eFormals=map (fn c => (NONE,c)) cons,
				 fFormals=0w0, body_type=resc})
	    | (HNF,_,c)         => (HNF,#2 state,no_flatten)
	    | (IRREDUCIBLE,_,c) => 
		(case find_kind_equation(#1 state,argc)
		   of SOME argc => (PROGRESS,#2 state,Prim_c (Vararg_c (openness,effect),[argc,resc]))
		    | NONE      => (HNF,#2 state,irreducible)))
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

	| Let_c (sort,[],body) => (PROGRESS,#2 state,body)
        | Let_c (sort,cbnds,letbody) =>
	 let val cbnd::rest = NilUtil.flattenCbnds cbnds
	 in  (case cbnd of
		  Open_cb (var,formals,funbody) => con_reduce_letfun state (sort,Open_cb,var,formals,funbody,rest,letbody)
		| Code_cb (var,formals,funbody) => con_reduce_letfun state (sort,Code_cb,var,formals,funbody,rest,letbody)
		| Con_cb(var,con) => let val (D,subst) = state
					 val subst = addr (subst,var,con)
				     in  (PROGRESS,subst,Let_c(sort,rest,letbody))
				     end)
	 end
	| (Closure_c (c1,c2)) => 
	    let val (progress,subst,c1) = con_reduce state c1 
	    in  (progress,subst,Closure_c(c1,c2))
	    end
	| (Crecord_c _) => (HNF,#2 state,constructor)
	| Typeof_c e => (PROGRESS, #2 state, type_of(#1 state,e))
	| (Proj_c (Mu_c _,lab)) => (HNF,#2 state,constructor)
	| (Proj_c (c,lab)) => 
	      (case con_reduce state c of
		   (PROGRESS,subst,c) => (PROGRESS,subst,Proj_c(c,lab))
		 | (_,_,c) => 
		       let val (D,subst) = state
			   val (progress,con) = beta_conrecord' (Proj_c (c,lab))
		       in  if progress
			       then (PROGRESS,subst,con)
			   else (IRREDUCIBLE,subst,con)
		       end)
	| (App_c (cfun,actuals)) => 
	       (case con_reduce state cfun of
		    (PROGRESS,subst,c) => (PROGRESS,subst,App_c(c,actuals))
		  | (_,_,c) => 
			let val (D,subst) = state
			    val (progress,con) = beta_confun' true D (App_c(c,actuals))
			in  if progress
				then (PROGRESS,subst,con)
			    else (IRREDUCIBLE,subst,con)
			end)
	| (Typecase_c {arg,arms,default,kind}) => error "typecase not done yet"
	| (Annotate_c (annot,con)) => con_reduce state con)


    and reduce_once (D,con) = let val (progress,subst,c) = con_reduce(D,empty()) con
			      in  (case progress 
				     of IRREDUCIBLE => (case find_kind_equation (D,c) 
							  of SOME c => substConInCon subst c
							   | NONE => substConInCon subst c)
				      | _ => substConInCon subst c)
			      end
    and reduce_until (D,pred,con) = 
        let fun loop n (subst,c : con) = 
            let val _ = if (n=warnDepth) 
			    then (print "Warning: reduce_until exceeded "; 
				  print (Int.toString warnDepth); print "iterations\n")
			else ()
		val _ = if (n = maxDepth)
			    then (print "Warning: reduce_until exceeded "; 
				  print (Int.toString maxDepth); print "iterations.  Showing 10 iterations.\n")
			else ()
		val _ = if (n >= maxDepth)
			    then (print "Error: reduce_until iteration #";
				  print "#"; print (Int.toString n); print ": "; Ppnil.pp_con c; print "\n")
			else ()
		val _ = if (n >= maxDepth + 10) then error "reduce_until too many iterations" else ()
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
		       | HNF => (true, strip_annotate(substConInCon subst c))
		       | IRREDUCIBLE => 
			   (case find_kind_equation(D,c)
			      of SOME c => loop (n+1) (subst,c)
			       | NONE => (false, strip_annotate(substConInCon subst c)))
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
      

    and reduce(D,con) = con_normalize D con


    and projectTuple(D:context, c:con, l:label) = 
	(case (reduce_hnf(D,Proj_c(c,l))) of
	   (true,c)  => c
	 | (false,c) => c)

    and removeDependence vclist c = 
	let fun loop subst [] = substExpInCon subst c
	      | loop subst ((v,c)::rest) = 
	           let val e = Raise_e(NilUtil.match_exn,c)
		   in  loop (NilSubst.E.addr (subst,v,e)) rest
		   end
	in  loop (NilSubst.E.empty()) vclist
	end

    and projectRecordType(D:context, c:con, l:label) = 
	(case #2(reduce_hnf(D,c)) of
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
		   Ppnil.pp_con c; print "\n";
		   error "projectRecordType reduced to non-record type"))

    and reduceToSumtype(D: context, c:con) = 
	(case #2(reduce_hnf(D,c)) of
	     Prim_c(Sum_c {tagcount,totalcount,known}, [carrier]) => 
	       if (TilWord32.equal(totalcount,TilWord32.uplus(tagcount,0w1))) 
		  then (tagcount,known,[carrier])
	       else (case #2(reduce_hnf(D,carrier)) of
		      Crecord_c lcons => (tagcount,known,map #2 lcons)
		    | _ => error "reduceToSumtype failed to reduced carrier to a crecord")
           | c => (print "reduceToSumtype failed to reduced argument to sumtype\n";
		   Ppnil.pp_con c; print "\n";
		   error "reduceToSumtype failed to reduced argument to sumtype"))

    and projectSumType(D:context, c:con, s:TilWord32.word) = 
	(case #2(reduce_hnf(D,c)) of
	     Prim_c(Sum_c {tagcount,totalcount,known}, cons) =>
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
	   | c => (print "projectSumType reduced to non-sum type = \n";
		   Ppnil.pp_con c; print "\n";
		   error "projectSumType reduced to non-sum type"))

   and reduce_vararg(D:context,openness,effect,argc,resc) = 
       let val irreducible = Prim_c(Vararg_c(openness,effect),[argc,resc])
	   val no_flatten = AllArrow_c{openness=openness,effect=effect,isDependent=false,
				       tFormals=[],eFormals=[(NONE,argc)],fFormals=0w0,
				       body_type=resc}
       in  case (reduce_hnf(D,argc)) of
	   (_,Prim_c(Record_c (labs,_),cons)) => 
	       if (length labs > !number_flatten) then no_flatten 
	       else AllArrow_c{openness=openness,effect=effect,isDependent=false,
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

(**
           Intsw_e {default=SOME def,...} => type_of(D,def)
	 | Intsw_e {arms,...} => type_of(D,#2(hd arms))
	 | Sumsw_e {default=SOME def,...} => type_of(D,def)
	 | Sumsw_e {arms,bound,sumtype,...} => 
	       let val (tagcount,_,carriers) = reduceToSumtype(D,sumtype)
		   val ssumcon = Prim_c(Sum_c {tagcount=tagcount,
					       totalcount=TilWord32.uplus(tagcount,
									  TilWord32.fromInt
									  (length carriers)),
					       known=SOME 0w0}, 
					case carriers of
					    [_] => carriers
					  | _ => [NilUtil.con_tuple_inject carriers])
		   val D = NilContext.insert_con(D,bound,ssumcon)
	       in  type_of(D,#2(hd arms))
	       end
	 | Exncase_e {default=SOME def,...} => type_of(D,def)
	 | Exncase_e {arms,bound,...} => 
	       let val (tage,body) = hd arms
		   val tagcon = type_of(D,tage)
		   val Prim_c(Exntag_c, [con]) = #2(reduce_hnf(D,tagcon))
		   val D = NilContext.insert_con(D,bound,con)
	       in  type_of(D,#2(hd arms))
	       end
	 | Typecase_e _ => error "typecase_e not done")
**)

   and type_of_value (D,value) = 
     (case value 
	of int (intsize,_) => Prim_c (Int_c intsize,[])
	 | uint (intsize,_) => Prim_c (Int_c intsize,[])
	 | float (floatsize,string) => Prim_c (Float_c floatsize,[])
	 | array (con,arr) => Prim_c (Array_c,[con])
	 | vector (con,vec) => Prim_c (Vector_c,[con])
	 | refcell expref => Prim_c (Array_c,[type_of(D,!expref)])
	 | tag (atag,con) => Prim_c (Exntag_c,[con]))

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
		 | Open_cb(v,vklist,c) => (v,Let_c(Sequential,[cbnd],Var_c v))
		 | Code_cb(v,vklist,c) => (v,Let_c(Sequential,[cbnd],Var_c v)))
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
	    record labs => Prim_c(Record_c (labs,NONE), map (fn e => type_of(D,e)) exps)
	  | select lab => projectRecordType(D,type_of(D,hd exps),lab)
	  | inject s => specialize_sumtype(s,hd cons)
	  | inject_known s => specialize_sumtype(s,hd cons)
	  | inject_known_record s => specialize_sumtype(s,hd cons)
	  | project s => projectSumType(D,hd cons, s)
	  | project_known s => projectSumType(D,hd cons, s)
	  | project_known_record (s,lab) => let val summandType = projectSumType(D,hd cons, s)
					  in  projectRecordType(D,summandType,lab)
					  end
	  | box_float fs => Prim_c(BoxFloat_c fs,[])
	  | unbox_float fs => Prim_c(Float_c fs,[])
	  | roll => hd cons
	  | unroll => expandMuType(D,hd cons)
	  | make_exntag => Prim_c(Exntag_c, cons)
	  | inj_exn _ => Prim_c(Exn_c, [])
	  | make_vararg (openness,effect) => 
	       let val [argc,resc] = cons
	       in  reduce_vararg(D,openness,effect,argc,resc)
	       end
	  | make_onearg (openness,effect) => 
	       let val [argc,resc] = cons
	       in  AllArrow_c{openness=openness,effect=effect,isDependent=false,
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
       (case exp 
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
	      removeDependence etypes (Let_c (Sequential,cbnds,c))
	    end
	   | Prim_e (NilPrimOp prim,cons,exps) => type_of_prim (D,prim,cons,exps)
	   | Prim_e (PrimOp prim,cons,exps) =>   
	    let 
	      val (total,arg_types,return_type) = NilPrimUtil.get_type prim cons
	    in
	      return_type
	    end
	   | Switch_e switch => type_of_switch (D,switch)
	   | ExternApp_e (app,texps) =>
	    let
	      val app_con : con = type_of (D,app)
	    in  (case #2(reduce_hnf(D,app_con)) of
		     ExternArrow_c(_,c) => c
		   | c => (print "Ill Typed expression - not an arrow type. c = \n";
			      Ppnil.pp_con app_con;
			      print "\nreduce to = \n";
			      Ppnil.pp_con c;
			      print "\nexp = \n";
			      Ppnil.pp_exp app;
			      print "\n";
			      error "Ill Typed expression - not an arrow"))
	    end	   
	   | (App_e (openness,app,cons,texps,fexps)) =>
	    let
	      val app_con : con = type_of (D,app)
	      val  (tformals,eformals,body) = 
		(case #2(reduce_hnf(D,app_con)) of
		     AllArrow_c{tFormals,eFormals,body_type,...} => (tFormals,eFormals,body_type)
		   | Prim_c(Vararg_c _, [_,c]) => ([],[],c)
		   | c => (print "Ill Typed expression - not an arrow type.\n app_con = \n";
			      Ppnil.pp_con app_con;
			      print "\nreduce to = \n";
			      Ppnil.pp_con c;
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
	  val lambda = (Let_c (sort,[coder (var,formals,body)],Var_c var))
	in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var con) 
	     then (state,lambda,false)
	   else
	     context_beta_reduce(bind_eqn(state,var,lambda),Let_c(sort,rest,con))
	end
      
      and context_beta_reduce (state : (context * Alpha.alpha_context),constructor : con) 
	: ((context * Alpha.alpha_context) * con * bool)  = 
	(case constructor of 
	   (Prim_c (Vararg_c (openness,effect),[argc,resc])) =>       
	     let 
	       val (state,argc,path) = context_reduce_hnf''(state,argc)
		 
	       val irreducible = Prim_c(Vararg_c(openness,effect),[argc,resc])
	       val no_flatten  = AllArrow_c{openness=openness,effect=effect,isDependent=false,
					    tFormals=[],eFormals=[(NONE,argc)],fFormals=0w0,
					    body_type=resc}
	       val res = 
		 (case argc of
			Prim_c(Record_c (labs,_),cons) => 
			  if (length labs > !number_flatten) then no_flatten 
			  else AllArrow_c{openness=openness,effect=effect,isDependent=false,
					  tFormals=[], eFormals=map (fn c => (NONE,c)) cons,
					  fFormals=0w0, body_type=resc}
		      | _ => if path then irreducible
			     else no_flatten)
	     in (state,res,false)
	     end
	 | (Prim_c _)            => (state,constructor,false)
	 | (Mu_c _)              => (state,constructor,false)
	 | (AllArrow_c _)        => (state,constructor,false)
	 | (ExternArrow_c _)     => (state,constructor,false)
	 | (Crecord_c _)         => (state,constructor,false)
	 | (Proj_c (Mu_c _,lab)) => (state,constructor,false)
	 | (Var_c var)           => (state,constructor,true)
	     
	 | (Let_c (sort,((cbnd as Open_cb (var,formals,body))::rest),con)) =>
	     context_beta_reduce_letfun (state,sort,Open_cb,var,formals,body,rest,con)
	     
	 | (Let_c (sort,((cbnd as Code_cb (var,formals,body))::rest),con)) =>
	     context_beta_reduce_letfun (state,sort,Code_cb,var,formals,body,rest,con)
	     
	 | (Let_c (sort,cbnd as (Con_cb(var,con)::rest),body))             =>
	     context_beta_reduce(bind_eqn(state,var,con),Let_c(sort,rest,body))
	     
	 | (Let_c (sort,[],body)) => context_beta_reduce(state,body)
	     
	 | (Closure_c (c1,c2)) => 
	     let val (state,c1,path) = context_beta_reduce (state,c1)
	     in  (state,Closure_c(c1,c2),path)
	     end

	 | Typeof_c e => 
	     let val (D,alpha) = state
	     in context_beta_reduce((D,Alpha.empty_context()),type_of(D,alphaCRenameExp alpha e))
	     end
	 | (Proj_c (c,lab)) => 
	     (case context_beta_reduce (state,c) of
		(state,constructor,true)  => (state,Proj_c(constructor,lab),true)
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
		  end)
	 | (App_c (cfun,actuals)) => 
	      (case context_beta_reduce (state,cfun) of
		 (state,constructor,true)  => (state,App_c(constructor,actuals),true)
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
			 (case strip_annotate name
			    of Var_c var' => 	  
			      if eq_var (var,var') then
				(formals,body)
			      else raise NOT_A_LAMBDA
			     | _ => raise NOT_A_LAMBDA)
		       end
		     
		     fun lambda_or_closure (Let_c (_,[lambda],name)) = (get_lambda (lambda,name),NONE)
		       | lambda_or_closure (Closure_c(code,env)) = 
		       let val (args,_) = lambda_or_closure code
		       in  (args,SOME env) end
		       | lambda_or_closure (Annotate_c(_,con)) = lambda_or_closure (con)
		       | lambda_or_closure _ = raise NOT_A_LAMBDA
			 
			 
		     fun open_lambda cfun = (SOME (lambda_or_closure cfun)) handle NOT_A_LAMBDA => NONE
		       
		   in
		     (case open_lambda constructor
			of SOME((formals,body),SOME env) =>
			  context_beta_reduce(instantiate_formals(state,formals,env::actuals),body)
			 | SOME((formals,body),NONE)     => 
			  context_beta_reduce(instantiate_formals(state,formals,actuals),body)
			 | NONE => (Ppnil.pp_con constructor;
				    error (locate "context_beta_reduce") "redex not in HNF"))
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
		       | _ => (state,Typecase_c{arg=arg,arms=arms,default=default,kind=kind},false))
		 end
	     | (Annotate_c (annot,con)) => context_beta_reduce (state,con))
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

  val type_of = fn args => strip_annotate (type_of args)

  val kind_normalize = wrap2 "kind_normalize" kind_normalize
  val con_normalize = wrap2 "con_normalize"  con_normalize
  val exp_normalize = wrap2 "exp_normalize" exp_normalize
  val module_normalize = wrap2 "mod_normalize" module_normalize

  val kind_normalize' = wrap2 "kind_normalize'" kind_normalize'
  val con_normalize' = wrap2 "con_normalize'"  con_normalize'
  val exp_normalize' = wrap2 "exp_normalize'" exp_normalize'

  val reduce_hnf = wrap1 "reduce_hnf" reduce_hnf
  val reduce_once = wrap1 "reduce_once" reduce_once
  val reduce_until = fn arg => wrap "reduce_until" (fn () => reduce_until arg)

  val beta_conrecord = wrap1 "beta_conrecord" beta_conrecord
  val beta_confun = wrap2 "beta_confun" beta_confun
  val eta_confun = wrap1 "eta_confun" eta_confun
  val beta_typecase = wrap2 "beta_typecase" beta_typecase

  val reduceToSumtype = wrap1 "reduceToSumType" reduceToSumtype
  val type_of = fn arg => (wrap1 "type_of" type_of arg
			   handle e => (print "type_of failed on ";
					Ppnil.pp_exp (#2 arg); raise e))



end
