functor NormalizeFn(structure Nil : NIL
		    structure PpNil : PPNIL
		    structure NilUtil : NILUTIL 
		    structure NilContext : NILCONTEXT'
		    structure Subst : NILSUBST
		         sharing NilUtil.Nil = PpNil.Nil = NilContext.Nil = Nil
			 and type Subst.con = Nil.con
		         and type Subst.exp = Nil.exp
			 and type Subst.kind = Nil.kind
			 and type Subst.bnd = Nil.bnd
			 and type Subst.subst = NilContext.subst) :(*>*) NORMALIZE 
  where type exp = Nil.exp 
  and type con = Nil.con 
  and type kind = Nil.kind 
  and type context = NilContext.context 
  and type 'a subst = 'a Subst.subst = 
struct	
  open Nil 
  open Prim

  type 'a subst = 'a Subst.subst
  val substConInKind= Subst.substConInKind
  val substConInExp = Subst.substConInExp
  val substConInCon = Subst.substConInCon
  val empty = Subst.empty
  val con_subst_compose = Subst.con_subst_compose
  val add = Subst.add
  val substitute = Subst.substitute
  val fromList = Subst.fromList

  val map_annotate = NilUtil.map_annotate
  val is_var_c = NilUtil.is_var_c
  val strip_var = NilUtil.strip_var
  val strip_crecord = NilUtil.strip_crecord
  val strip_proj = NilUtil.strip_proj
  val strip_prim = NilUtil.strip_prim
  val strip_app = NilUtil.strip_app
  val strip_singleton = NilUtil.strip_singleton
  val con_free_convar = NilUtil.con_free_convar
  val alpha_equiv_con = NilUtil.alpha_equiv_con
  val primequiv = NilUtil.primequiv
  val pull = NilUtil.pull
  val singletonize = NilUtil.singletonize 

  val mapsequence = Util.mapsequence

  (*From Name*)
  val eq_var = Name.eq_var
  val eq_var2 = Name.eq_var2
  val eq_label = Name.eq_label
  val fresh_named_var = Name.fresh_named_var
  fun fresh_var () = fresh_named_var "normalize"
  val derived_var = Name.derived_var
  val label2string = Name.label2string
  val var2string = Name.var2string 

  (*From Listops*)
  val map_second = Listops.map_second
  val foldl_acc = Listops.foldl_acc
  val map = Listops.map
  val map2 = Listops.map2
  val zip = Listops.zip
  val unzip = Listops.unzip
  val all = Listops.all
  val all2 = Listops.all2
  val split = Listops.split

  (*From Util *)
  val set2list = Util.set2list
  val list2set = Util.list2set
  val mapsequence = Util.mapsequence
  val sequence2list = Util.sequence2list
  val list2sequence = Util.list2sequence
  val eq_opt = Util.eq_opt
  val map_opt = Util.mapopt
  val printl = Util.printl
  val lprintl = Util.lprintl

  (* Local helpers *)
  type context = NilContext.context
  val find_kind = NilContext.find_kind   
  val find_kind' = NilContext.find_kind'

  fun error s = Util.error "normalize.sml" s

  val debug = ref false
  val show_calls = ref false

  local
      datatype entry = 
	EXP of exp * (NilContext.context * (con subst))
      | CON of con * (NilContext.context  * (con subst))
      | KIND of kind * (NilContext.context * (con subst))
      | BND of bnd * (NilContext.context * (con subst))
      | MODULE of module * (NilContext.context * (con subst))
      val stack = ref ([] : entry list)
      fun push e = stack := (e :: (!stack))
  in
    fun push_exp (e,context) = push (EXP(e,context))
    fun push_con(c,context) = push(CON(c,context))
    fun push_kind(k,context) = push(KIND(k,context))
    fun push_bnd(b,context) = push(BND(b,context))
    fun push_mod(m,context) = push(MODULE(m,context))
    fun pop() = stack := (tl (!stack))
    fun show_stack() = let val st = !stack
			   val _ = stack := []
			   fun show (EXP(e,(context,s))) = 
			     (print "exp_normalize called with expression =\n";
			      PpNil.pp_exp e;
			      print "\nand context"; 
			      NilContext.print_context context;
			      print "\n and subst";
			      Subst.printConSubst s;
			      print "\n\n")
			     | show (CON(c,(context,s))) =
				     (print "con_normalize called with constructor =\n";
				      PpNil.pp_con c;
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  Subst.printConSubst s;
				      print "\n\n")
			     | show (KIND(k,(context,s))) =
				     (print "kind_normalize called with kind =\n";
				      PpNil.pp_kind k;
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  Subst.printConSubst s;
				      print "\n\n")
			     | show (BND(b,(context,s))) =
				     (print "bnd_normalize called with bound =\n";
				      PpNil.pp_bnd b;
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  Subst.printConSubst s;
				      print "\n\n")
			     | show (MODULE(m,(context,s))) =
				     (print "module_normalize called with module =\n";
				      PpNil.pp_module m;
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  Subst.printConSubst s;
				      print "\n\n")
		       in  app show (rev st)
		       end
    fun wrap f arg arg2 = (f arg arg2) 
      handle e => (show_stack(); raise e)
  end
	 
  fun count_fields k = 
    (case strip_singleton k 
       of (Record_k entries) => List.length (sequence2list entries)
	| _ => error "Expected record_kind")

  fun beta_conrecord proj = 
    let
      fun beta_conrecord' (Proj_c (con,label)) = 
	(case strip_crecord con
	   of SOME entries =>
	     (case (List.find (fn ((l,_)) => eq_label (l,label))
		    entries )
		of SOME (l,c) => c
		 | NONE => (error "Field not in record" handle e => raise e))
	    | NONE => proj)
	| beta_conrecord' _ = 
	   (PpNil.pp_con proj;
	    (error "beta_conrecord called on non-projection" handle e => raise e))
    in
      map_annotate beta_conrecord' proj
    end

  fun make_shape kind = 
    (case kind 
       of Type_k p => (Type_k p)
	| Word_k p => (Word_k p)
	| Singleton_k (p,kind,con) => make_shape kind
	| Record_k elts => 
	 Record_k (mapsequence (fn ((l,v),k) => ((l,v),make_shape k)) elts)
	| Arrow_k (openness, formals, return) => 
	 let
	   val formals = map_second make_shape formals
	   val return = make_shape return
	 in
	   (Arrow_k (openness, formals,return))
	 end)

  fun get_shape' (D : context) (constructor : con) : kind = 
    (case constructor 
       of (Prim_c (pcon,args)) =>  
	 let
	   val kind = 
	     (case pcon
		of ((Int_c W64) | 
		    (Float_c F32) |
		    (Float_c F64)) => (Type_k Runtime)
		  | _ => (Word_k Runtime))
	 in kind
	 end
	| (Mu_c (recur,defs,var)) => (singletonize (Word_k Runtime,constructor))
	| (AllArrow_c (openness,effect,tformals,formals,numfloats,body)) =>Word_k Runtime
	| (v as (Var_c var)) => 
	 (case find_kind (D,var) 
	    of SOME k => make_shape k
	     | NONE => 
	      (NilContext.print_context D;
	       error ("variable "^(var2string var)^" not in context")))
        | (Let_c (sort,(([cbnd as Open_cb (var,formals,body,body_kind)]) | 
			([cbnd as Code_cb (var,formals,body,body_kind)])),con)) => 
	 (if is_var_c con then
	    let
	      val openness = 
		(case cbnd 
		   of Open_cb _ => Open
		    | _ => Code)
	      val bndkind = Arrow_k(openness,formals,body_kind)
	    in bndkind
	    end
	  else
	    error "get_shape' called on un-normalized constructor")
	| (Let_c _) => error "get_shape' called on un-normalized constructor"
	| (Closure_c (code,env)) => 
	    (case (strip_singleton (get_shape' D code))
	       of Arrow_k ((Code | ExternCode),vklist,body_kind) => 
		 let 
		   val (first,(v,klast)) = split vklist
		   val kind = Arrow_k(Closure,first,body_kind)
		 in kind
		 end
		| _ => (error "Invalid closure: code component does not have code kind" 
			handle e => raise e))
	| (Crecord_c entries) => 
	 let
	   val (labels,cons) = unzip entries
	   val kinds = (map (get_shape' D) cons)
	   val k_entries = 
	     map2 (fn (l,k) => ((l,fresh_named_var "crec_norm"),k)) (labels,kinds)
	   val entries = zip labels cons
	 in Record_k (list2sequence k_entries)
	 end
	| (Proj_c (rvals,label)) => 
	 let
	   val record_kind = get_shape' D rvals
	   val entry_kinds = 
	     (case (strip_singleton record_kind) of
		Record_k kinds => sequence2list kinds
	      | other => 
		    (print "Non-record kind returned from get_shape' in projection:\n";
		     PpNil.pp_kind other; print "\n";
		     error "Non-record kind returned from get_shape' in projection" handle e => raise e))
	   fun find ((l,v),k) = eq_label(l,label)
	   (*No dependencies, since shape is all we get*)
	   val (_,kind) = valOf (List.find find entry_kinds)
	 in kind
	 end
	| (App_c (cfun,actuals)) => 
	    let
	      val cfun_kind = get_shape' D cfun
	      val (formals,body_kind) = 
		case (strip_singleton cfun_kind) of
		  (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		| _ => (print "Invalid kind for constructor application\n";
			PpNil.pp_kind cfun_kind; print "\n";
			(error "Invalid kind for constructor application" handle e => raise e))
	    in body_kind
	    end
	| (Typecase_c {arg,arms,default,kind}) => kind
	| (Annotate_c (annot,con)) => get_shape' D con)

  fun eta_confun lambda = 
    let
      fun eta_confun' 
	(Let_c (sort,(([Open_cb (var,formals,body,body_kind)]) |
		      ([Code_cb (var,formals,body,body_kind)])),con)) = 
	(case strip_app body
	   of SOME (con,actuals) =>
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
		 then
		   con
	       else
		 lambda
	     end
	    | NONE => lambda)
	| eta_confun' _ = lambda
    in
      map_annotate eta_confun' lambda
    end

  fun eta_conrecord D record_c = 
    let
      fun eta_conrecord' (Crecord_c []) = record_c
	| eta_conrecord' (Crecord_c (fields as (label,con)::rest)) = 
	(case strip_proj con
	   of SOME (c,l) => 
	     let
	       fun etable repcon (label,con) = 
		 (case strip_proj con
		    of SOME (con2,label2) => 
		      (eq_label (label,label2)) andalso 
		      (alpha_equiv_con (repcon,con2))
		     | NONE => false)
	       val kind = get_shape' D c
	       val kind' = get_shape' D record_c
	     in
	       if alpha_equiv_kind (kind,kind') andalso (all (etable c) fields) then
		 c
	       else 
		 record_c
	     end
	    | NONE => record_c)
	| eta_conrecord' _ = 
	   (PpNil.pp_con record_c;
	    (error "eta_conrecord passed non record" handle e => raise e))
    in
      map_annotate eta_conrecord' record_c
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
	   (PpNil.pp_con typecase;
	    (error "beta_typecase called on non type case" handle e => raise e))
    in
      map_annotate beta_typecase' typecase
    end

  and beta_confun D app = 
    let
      fun beta_confun' (app as (App_c (con,actuals))) = 
	let
	  fun beta_confun'' actuals
	    (Let_c (sort,(([Open_cb (var,formals,body,body_kind)]) |
			  ([Code_cb (var,formals,body,body_kind)])),con)) = 
	    (if eq_opt(eq_var,SOME var,strip_var con) then
	       let
		 val (vars,_) = unzip formals
		 val subst = fromList (zip vars actuals)
	       in con_normalize' (D,subst) body
	       end
	     else app)
	    | beta_confun'' actuals (Closure_c (code,env)) = 
	       beta_confun'' (actuals @ [env]) code
	    | beta_confun'' _ _ = app
	in    
	  map_annotate (beta_confun'' actuals) con
	end
	| beta_confun' con = 
	(PpNil.pp_con con;
	 (error "beta_confun called on non-application" handle e => raise e))
    in
      map_annotate beta_confun' app
    end

  and insert_kind (D,var,kind) = NilContext.insert_kind (fn D => con_normalize' (D,empty())) (D,var,kind)
  and bind_at_kind (D,subst) (var,kind) = 
    let
      val kind = kind_normalize' (D,subst) kind
      val var' = derived_var var 
      val D = insert_kind (D,var',kind)
      val subst = add subst (var,Var_c var')
    in
      ((D,subst),var',kind)
    end
  
  and bind_at_kinds state kinds = 
    let
      fun folder ((v,k),state) = 
	let
	  val (state,v,k) = bind_at_kind state (v,k)
	in
	  ((v,k),state)
	end
      val (kinds,state) = foldl_acc folder state kinds
    in
      (state,kinds)
    end
 
  and kind_normalize' (state as (D,subst)) (kind : kind) : kind = 
    if !debug then
      let 
	val _ = push_kind(kind,state)
	val _ = if (!show_calls)
		  then (print "kind_normalize called with kind =\n";
			PpNil.pp_kind kind; 
			print "\nand context"; NilContext.print_context D;
			print "\n and subst";  Subst.printConSubst subst;
			print "\n\n")
		else ()
	val res = kind_normalize state kind
	val _ = pop()
      in  res
      end
    else
      kind_normalize state kind
  and kind_normalize state (kind : kind) : kind = 
    (case kind 
       of Type_k p => (Type_k p)
	| Word_k p => (Word_k p)
	| Singleton_k (p,kind,con) => 
	 let
	   val con = con_normalize' state con
	   val kind = kind_normalize' state kind
	 in
	   singletonize(kind,con)
	 end
	| Record_k elts => 
	 let
	   val elt_list = sequence2list elts
	   val (labels,vars_and_kinds) = unzip (map (fn ((l,v),k) => (l,(v,k))) elt_list)
	   val (state,vars_and_kinds) =  bind_at_kinds state vars_and_kinds
	   val elts = 
	     map2 (fn (l,(v,k)) => ((l,v),k)) (labels,vars_and_kinds)
	 in  
	   Record_k elts
	 end
	| Arrow_k (openness, formals, return) => 
	 let
	   val (state,formals) = bind_at_kinds state formals
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
			PpNil.pp_con con; 
			print "\nand context"; NilContext.print_context D;
			print "\n and subst";  Subst.printConSubst subst;
			print "\n\n")
		else ()
	val res = con_normalize state con
	val _ = pop()
      in  res
      end
    else
      con_normalize state con
  and con_normalize state (constructor : con) : con  = 
    (case constructor 
       of (Prim_c (pcon,args)) =>
	 let
	   val args = map (con_normalize' state) args
	 in (Prim_c (pcon,args))
	 end
	| (Mu_c (recur,defs,var)) =>
	 let
	   val def_list = sequence2list defs
	   val (vars,cons) = unzip def_list
	   val (state',vars_kinds) = 
	     bind_at_kinds state (map (fn v => (v,Word_k Runtime)) vars)
	   val (vars,_) = unzip vars_kinds
	   val var = 
	     case substitute (#2 state') var
	       of SOME(Var_c var) => var
		| _ => var
	   val cons = if recur then map (con_normalize' state') cons
		      else map (con_normalize' state) cons
	   val defs = list2sequence (zip vars cons)
	 in Mu_c (recur,defs,var)
	 end
	| (AllArrow_c (openness,effect,tformals,formals,numfloats,body)) =>
	 let
	   val (state,tformals) = bind_at_kinds state tformals
	   val formals = map (con_normalize' state) formals
	   val body = con_normalize' state body
	 in AllArrow_c (openness,effect,tformals,formals,numfloats,body)
	 end
	| (Var_c var) => 
	 let
	   val (D,subst) = state
	   val con = 
	     (case (substitute subst var)  
		of SOME c => c
		 | _ => constructor)
	 in
	   (case strip_var con
	      of SOME v => 
		(case find_kind' (D,v)
		   of SOME (c,_) => c
		    | _ => 
		     (NilContext.print_context D;
		      error ("Variable not found in context: "^(var2string v))))
	       | _ => con)
	 end
        | (Let_c (sort,(((cbnd as Open_cb (var,formals,body,body_kind))::rest) | 
			((cbnd as Code_cb (var,formals,body,body_kind))::rest)),con)) => 
	    let
	      val old_state = state
	      val constructor = 
		case cbnd 
		  of Open_cb _ => Open_cb
		   | _ => Code_cb
	      val (state,formals) = bind_at_kinds state formals
	      val body = con_normalize' state body
	      val body_kind = kind_normalize' state body_kind
	      val lambda = (Let_c (sort,[constructor (var,formals,body,body_kind)],Var_c var))
	      val lambda = eta_confun lambda
	      val state = 
		let val (D,subst) = old_state
		in (D,add subst (var,lambda))
		end
	    in
	      if (null rest) andalso eq_opt (eq_var,SOME var,strip_var con) then
		lambda
	      else
		con_normalize' state (Let_c (sort,rest,con))
	    end
	| (Let_c (sort,cbnd as (Con_cb(var,kind,con)::rest),body)) =>
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
	| (Crecord_c entries) => 
	    let
	      val (labels,cons) = unzip entries
	      val cons = map (con_normalize' state) cons
	      val entries = zip labels cons
	      val con = Crecord_c entries
	      val con = eta_conrecord (#1 state) con
	    in con
	    end
	| (Proj_c (rvals,label)) => 
	    let
	      val rvals = con_normalize' state rvals
	      val con = Proj_c (rvals,label)
	      val con = beta_conrecord con
	    in con
	    end
	| (App_c (cfun,actuals)) => 
	    let
	      val cfun = con_normalize' state cfun
	      val actuals = map (con_normalize' state) actuals
	      val con = App_c (cfun,actuals)
	      val con = beta_confun (#1 state) con
	    in con
	    end
	| (Typecase_c {arg,arms,default,kind}) => 
	    let
	      val kind = kind_normalize' state kind
	      fun doarm (pcon,args,body) = 
		let
		  val (state,args) = bind_at_kinds state args
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
    (case value
       of ((int _) | (uint _) | (float _)) => value
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
  and switch_normalize' state switch = 
    (case switch
       of Intsw_e {info,arg,arms,default} =>
	 let
	   val arg = exp_normalize' state arg
	   val arms = map_second (function_normalize' state) arms
	   val default = map_opt (exp_normalize' state) default
	 in
	   Intsw_e {info=info,arg=arg,
		    arms=arms,default=default}
	 end
	| Sumsw_e {info=(non_val,val_cons),arg,arms,default} => 
	 let
	   val arg = exp_normalize' state arg
	   val arms = map_second (function_normalize' state) arms
	   val val_cons = map (con_normalize' state) val_cons
	   val default = map_opt (exp_normalize' state) default
	 in
	   Sumsw_e {info=(non_val,val_cons),arg=arg,
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
  and function_normalize' state (Function (effect,recursive,tformals,
						 formals,fformals,body,return)) = 
    let
      val (state,tformals) = bind_at_kinds state tformals
      val formals = map_second (con_normalize' state) formals
      val body = exp_normalize' state body
      val return = con_normalize' state return
    in Function (effect,recursive,tformals,formals,fformals,body,return)
    end
  and bnds_normalize' state bnds = 
    let
      fun norm_bnd (bnd,state) = bnd_normalize' state bnd
    in
      foldl_acc norm_bnd state bnds
    end
  and bnd_normalize' state (bnd : bnd) =
    (case bnd
       of Con_b (var, kind, con) =>
	 let
	   val con = con_normalize' state con
	   val (state,var,kind) = bind_at_kind state (var,kind)
	   val bnd = Con_b (var,kind,con)
	 in (bnd,state)
	 end
	| Exp_b (var, con, exp) =>
	 let
	   val con = con_normalize' state con
	   val exp = exp_normalize' state exp
	   val bnd = Exp_b (var,con,exp)
	 in (bnd,state)
	 end
	| ((Fixopen_b defs) | (Fixcode_b defs)) =>
	 let
	   val constructor = 
	     (case bnd 
		of Fixopen_b _ => Fixopen_b
		 | _ => Fixcode_b)
	   val def_list = set2list defs
	   val def_list = map_second (function_normalize' state) def_list
	   val defs = list2set def_list
	   val bnd = constructor defs
	 in
	   (bnd,state)
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
	   val defs = list2set (map_second do_closure (set2list defs))
	   val bnd = Fixclosure_b (is_recur,defs)
	 in (bnd,state)
	 end)
  and exp_normalize' (state as (D,subst)) (exp : exp) : exp = 
    if !debug then
      let 
	val _ = push_exp(exp,state)
	val _ = if (!show_calls)
		  then (print "exp_normalize called with expression =\n";
			PpNil.pp_exp exp; 
			print "\nand context"; NilContext.print_context D;
			print "\n and subst";  Subst.printConSubst subst;
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
	| Raise_e (exp,con) => 
	 let
	   val con = con_normalize' state con
	   val exp = exp_normalize' state exp
	 in Raise_e (exp,con)
	 end
	| Handle_e (exp,function) =>
	 let
	   val exp = exp_normalize' state exp
	   val function = function_normalize' state function
	 in Handle_e (exp,function)
	 end)
  fun import_normalize' state (ImportValue (label,var,con)) =
    let
      val con = con_normalize' state con
    in
      (ImportValue (label,var,con),state)
    end
    | import_normalize' state (ImportType (label,var,kind)) = 
    let
      val (state,var,kind) = bind_at_kind state (var,kind)
    in
      (ImportType (label,var,kind),state)
    end
  
  fun export_normalize' state (ExportValue (label,exp,con)) = 
    let
      val exp = exp_normalize' state exp
      val con = con_normalize' state con
    in ExportValue (label,exp,con)
    end
    | export_normalize' state (ExportType (label,con,kind)) = 
    let
      val con = con_normalize' state con
      val kind = kind_normalize' state kind
    in ExportType (label,con,kind)
    end
  
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

  val get_shape = get_shape'

  val kind_normalize = wrap kind_normalize
  val con_normalize = wrap con_normalize
  val exp_normalize = wrap exp_normalize

end
