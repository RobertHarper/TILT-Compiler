(*$import NIL PPNIL NILUTIL NILCONTEXT NILSUBST NORMALIZE PRIMUTIL *)
functor NormalizeFn(structure Nil : NIL
		    structure PpNil : PPNIL
		    structure NilUtil : NILUTIL 
		    structure PrimUtil : PRIMUTIL 
		    structure NilContext : NILCONTEXT
		    structure Subst : NILSUBST
			 sharing PrimUtil.Prim = Nil.Prim
		         sharing NilUtil.Nil = PpNil.Nil = NilContext.Nil = Nil
			 and type Subst.con = Nil.con = PrimUtil.con
		         and type Subst.exp = Nil.exp = PrimUtil.exp
			 and type Subst.kind = Nil.kind
			 and type Subst.bnd = Nil.bnd
			 and type Subst.subst = NilContext.subst) 
    :> NORMALIZE 
  where Nil = Nil
  and type context = NilContext.context 
  and type 'a subst = 'a Subst.subst = 
struct	

  structure Nil = Nil
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
  val con_free_convar = NilUtil.con_free_convar
  val alpha_equiv_con = NilUtil.alpha_equiv_con
  val alpha_equiv_kind = NilUtil.alpha_equiv_kind
  val primequiv = NilUtil.primequiv

  val singletonize = NilUtil.singletonize 


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
  val eq_opt = Util.eq_opt
  val map_opt = Util.mapopt
  val printl = Util.printl
  val lprintl = Util.lprintl

  (* Local helpers *)
  type context = NilContext.context
  val find_kind = NilContext.find_kind   



  fun error s = Util.error "normalize.sml" s

val closed_check = ref false
  val debug = ref false
  val show_calls = ref false
val show_context = ref false


  fun pull (c,kind) = 
    let open Nil NilUtil Name Util
    in  (case kind of
          Type_k => c
	| Singleton_k c2 => c2
	| Record_k elts => 
	 let
	   fun folder (((label,var),kind),subst) = 
	     let
	       val kind = Subst.substConInKind subst kind
	       val con = pull (Proj_c (c,label),kind)
	       val subst = Subst.add subst (var,con)
	     in
	       ((label,con),subst)
	     end
	   val (entries,subst) = Listops.foldl_acc folder (Subst.empty()) (Sequence.toList elts)
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
	        Open => Let_c (Sequential,[Open_cb (var,formals,c,return)],Var_c var)
	      | Code => Let_c (Sequential,[Code_cb (var,formals,c,return)],Var_c var)
	      | ExternCode => Let_c (Sequential,[Code_cb (var,formals,c,return)],Var_c var)
	      | Closure => let val cenv = (fresh_named_var "pull_closure", 
					   Record_k (Sequence.fromList []))
			   in  Let_c (Sequential,[Code_cb (var,formals @ [cenv] ,c,return)],
				      Closure_c(Var_c var, Crecord_c []))
			   end
	 end)
    end


  local
      datatype entry = 
	EXP of exp * (NilContext.context * (con subst))
      | CON of con * (NilContext.context  * (con subst))
      | KIND of kind * (NilContext.context * (con subst))
      | BND of bnd * (NilContext.context * (con subst))
      | MODULE of module * (NilContext.context * (con subst))
      val stack = ref ([] : entry list)
      val maxdepth = 10000
      val depth = ref 0
      fun clear_stack() = (depth := 0; stack := [])
      fun push e = (depth := !depth + 1;
		    stack := (e :: (!stack));
		    if (!depth mod 20 = 0)
			then (print "****normalize.sml: stack depth = ";
			      print (Int.toString (!depth)))
		    else ();
		    if (!depth) > maxdepth
			then error "stack depth exceeded"
		    else ())
  in
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
    fun wrap str f arg arg2 = (f arg arg2) 
      handle e => (print "\n ------ ERROR in "; print str; print " ---------\n";
		   show_stack(); raise e)
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
	   (PpNil.pp_con proj;
	    (error "beta_conrecord called on non-projection" handle e => raise e))

  fun beta_conrecord proj = #2(beta_conrecord' proj)

  fun make_shape D kind = 
      let fun lfolder(((l,v),k),D) = let val k = make_shape D k
				     in  (((l,v),k),insert_kind(D,v,k))
				     end
	  fun folder((v,k),D) = let val k = make_shape D k
				in  ((v,k),insert_kind(D,v,k))
				end
      in    (case kind of
		 Type_k => Type_k
	       | Singleton_k con => get_shape' D con
	       | Record_k elts => Record_k (#1(Sequence.foldl_acc lfolder D elts))
	       | Arrow_k (openness, formals, return) => 
		     let val (formals,D) = foldl_acc folder D formals
			 val return = make_shape D return
		     in  (Arrow_k (openness, formals,return))
		     end)
      end

  and get_shape' (D : context) (constructor : con) : kind = 
      let 	   
      in
      (case constructor of
	 Prim_c (Int_c W64,_) => Type_k
       | Prim_c (Float_c F32,_) => Type_k (* error *)
       | Prim_c (Float_c F64,_) => Type_k (* error *)
       | Prim_c _ => Type_k 

	| (Mu_c (recur,defs)) => (case (Sequence.length defs) of
				      1 => Type_k
				    | len => NilUtil.kind_tuple(Listops.copy(len,Type_k)))
	| (AllArrow_c (openness,effect,tformals,formals,numfloats,body)) => Type_k
	| (Var_c var) => NilContext.find_shape (D,var)
        | Let_c (sort,[Open_cb (var,formals,body,body_kind)],Var_c v) =>
	   if (eq_var(var,v)) then Arrow_k(Open,formals,body_kind) else get_shape' D (Var_c v)
	| Let_c(sort,[Code_cb (var,formals,body,body_kind)],Var_c v) =>
	   if (eq_var(var,v)) then Arrow_k(Code,formals,body_kind) else get_shape' D (Var_c v)
        | Let_c (sort,[Code_cb (var,formals,body,body_kind)],Closure_c(Var_c v,_)) =>
	   if (eq_var(var,v)) then Arrow_k(Closure,Listops.butlast formals,body_kind) 
	   else get_shape' D (Var_c v)
        | Let_c (sort,[],body) => get_shape' D body
        | Let_c (sort,cbnd::rest,body) => 
	       let val D = (case cbnd of
				Con_cb(v,c) => insert_kind(D,v,Singleton_k c)
			      | Open_cb(v,formals,_,body_kind) => insert_kind(D,v,Arrow_k(Open,formals,body_kind))
			      | Code_cb(v,formals,_,body_kind) => insert_kind(D,v,Arrow_k(Code,formals,body_kind)))
	       in  get_shape' D (Let_c(sort,rest,body))
	       end
	| (Closure_c (code,env)) => 
	    let val (vklist,body_kind) = 
		(case get_shape' D code of
	          Arrow_k (Code,vklist,body_kind) => (vklist,body_kind)
		| Arrow_k (ExternCode,vklist,body_kind) => (vklist,body_kind)
		| k => (print "Invalid closure: code component does not have code kind\n";
			PpNil.pp_kind k; print "\n";
			error "Invalid closure: code component does not have code kind" 
			handle e => raise e))		      
		val (first,(v,klast)) = split vklist
		val kind = Arrow_k(Closure,first,body_kind)
	    in kind
	    end

	| (Crecord_c entries) => 
	 let
	   val (labels,cons) = unzip entries
	   val kinds = (map (get_shape' D) cons)
	   val k_entries = 
	     map2 (fn (l,k) => ((l,fresh_named_var "crec_norm"),k)) (labels,kinds)
	   val entries = zip labels cons
	 in Record_k (Sequence.fromList k_entries)
	 end
	| (Proj_c (rvals,label)) => 
	 let
	   val record_kind = get_shape' D rvals
	   fun find [] = error "could not find field in record kind"
	     | find (((l,v),k)::rest) = 
	       if (eq_label(l,label))
		   then k (* there should be no dependencies *)
	       else find rest
	 in  (case record_kind of
		Record_k kinds => find (Sequence.toList kinds)
	      | other => 
		    (print "Non-record kind returned from get_shape' in projection:\n";
		     PpNil.pp_kind other; print "\n";
		     error "Non-record kind returned from get_shape' in projection" 
		     handle e => raise e))
	 end
	| (App_c (cfun,actuals)) => 
	    let
	      val cfun_kind = get_shape' D cfun
	      val (formals,body_kind) = 
		case cfun_kind of
		  (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		| _ => (print "Invalid (non-arrow) kind for constructor application\n";
			PpNil.pp_kind cfun_kind; print "\n";
			(error "Invalid kind for constructor application" handle e => raise e))
	      fun folder (((v,k),c),D) = NilContext.insert_kind_equation(D,v,c,k)
	      val D = foldl folder D (zip formals actuals)
	    in body_kind
	    end
	| (Typecase_c {arg,arms,default,kind}) => kind
	| (Annotate_c (annot,con)) => get_shape' D con)
      end

  and eta_confun lambda = 
    let
	fun help(var, formals,body,body_kind) = 
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
			     then
				 con
			 else
			     lambda
		     end
	       | NONE => lambda)
      fun eta_confun' (Let_c (sort,[Open_cb (var,formals,body,body_kind)],Var_c var')) = 
	  if (eq_var(var,var')) then help(var,formals,body,body_kind) else lambda
        | eta_confun' (Let_c (sort,[Code_cb (var,formals,body,body_kind)],Var_c var')) = 
	  if (eq_var(var,var')) then help(var,formals,body,body_kind) else lambda
	| eta_confun' _ = lambda
    in
      map_annotate eta_confun' lambda
    end

  and eta_conrecord D record_c = 
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
	       if NilUtil.alpha_equiv_kind (kind,kind') andalso (all (etable c) fields) then
		 c
	       else 
		 record_c
	     end
	    | NONE => record_c)
	| eta_conrecord' _ = 
	   (PpNil.pp_con record_c;
	    (error "eta_conrecord passed non record" handle e => raise e))
    in  (map_annotate eta_conrecord' record_c)
	handle e => (print "eta_conrecord called on record_c = \n";
		     PpNil.pp_con record_c; print "\n"; raise e)
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


  and beta_confun once D app = #2(beta_confun' once D app)

  and beta_confun' once D (app as (App_c (con,actuals))) =
    let
	  fun reduce actuals (formals,body,body_kind) = 
	       (true,
		let
		 val (vars,_) = unzip formals
		 val subst = fromList (zip vars actuals)
	       in if once
		      then substConInCon subst body
		  else (con_normalize' (D,subst) body)
	       end)
	  fun beta_confun'' actuals confun = 
	      (case confun of
		   Let_c (_,[Open_cb (var,formals,body,body_kind)],Var_c v) =>
		     if eq_var(var,v)
			 then reduce actuals (formals,body,body_kind) 
		     else (false,app)
		 | Let_c (_,[Code_cb (var,formals,body,body_kind)],Var_c v) =>
		     if eq_var(var,v)
			 then reduce actuals (formals,body,body_kind) 
		     else (false,app)
	         | Let_c (_,[Code_cb (var,formals,body,body_kind)],Closure_c(Var_c v,env)) =>
		   if eq_var(var,v)
		       then reduce (actuals @ [env]) (formals,body,body_kind) 
		   else (false,app)
		 | Closure_c(Let_c (_,[Code_cb (var,formals,body,body_kind)],Var_c v), env) =>
		       if eq_var(var,v)
			   then reduce (actuals @ [env]) (formals,body,body_kind) 
		       else (false,app)
		 | _ => (false,app))
	in  beta_confun'' actuals con
	end
      | beta_confun' _ _ con =
	(PpNil.pp_con con;
	 (error "beta_confun called on non-application" handle e => raise e))


  and insert_kind (D,var,kind) = NilContext.insert_kind (D,var,kind)
  and bind_at_kind (D,subst) (var,kind) = 
    let
      val kind = kind_normalize' (D,subst) kind
      val var' = if ((find_kind(D,var); true)
			handle NilContext.Unbound => false)
		     then derived_var var 
		 else var
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
			if (!show_context)
			    then (print "\nand context"; NilContext.print_context D)
			else ();
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
    (case kind of
          Type_k => kind
	| Singleton_k con => Singleton_k(con_normalize' state con)
	| Record_k elts => 
	 let
	   val elt_list = Sequence.toList elts
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
			if (!show_context)
			    then (print "\nand context"; NilContext.print_context D)
			else ();
			print "\n and subst";  Subst.printConSubst subst;
			print "\n\n")
		else ()
	val res = con_normalize state con
	val _ = pop()
      in  res
      end
    else
      con_normalize state con

  and con_normalize_letfun state (sort,constructor,var,formals,body,body_kind,rest,letbody) = 
	    let
	      val old_state = state


	    in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var letbody) 
		   then
		       let val (state,formals) = bind_at_kinds state formals
			   val body = con_normalize' state body
			   val body_kind = kind_normalize' state body_kind
			   val lambda = (Let_c (sort,[constructor (var,formals,body,body_kind)],
						Var_c var))
			   val lambda = eta_confun lambda
		       in  lambda
		       end
	       else
		   let 

		       val lambda = (Let_c (sort,[constructor (var,formals,body,body_kind)],
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
          (Prim_c (pcon,args)) =>
	 let
	   val args = map (con_normalize' state) args
	 in (Prim_c (pcon,args))
	 end
	| (Mu_c (recur,defs)) =>
	 let
	   val def_list = Sequence.toList defs
	   val (vars,cons) = unzip def_list
	   val (state',vars_kinds) = 
	     bind_at_kinds state (map (fn v => (v,Type_k)) vars)
	   val (vars,_) = unzip vars_kinds
	   val cons = if recur then map (con_normalize' state') cons
		      else map (con_normalize' state) cons
	   val defs = Sequence.fromList (zip vars cons)
	 in Mu_c (recur,defs)
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
	     (case (substitute subst var) of
		   SOME c => c
		 | NONE =>
	           (case NilContext.find_kind_equation(D,Var_c var) of
			SOME c => con_normalize' state c
		      | NONE => Var_c var))
	 in con
	 end
        | (Let_c (sort,((cbnd as Open_cb (var,formals,body,body_kind))::rest),letbody)) =>
	 con_normalize_letfun state (sort,Open_cb,var,formals,body,body_kind,rest,letbody)
        | (Let_c (sort,((cbnd as Code_cb (var,formals,body,body_kind))::rest),letbody)) =>
	 con_normalize_letfun state (sort,Code_cb,var,formals,body,body_kind,rest,letbody)

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
(*
	      val con = eta_conrecord (#1 state) con 
		  handle e => (print "eta_conrecord in con_normalize failed.\noriginal con = \n";
			       PpNil.pp_con (Crecord_c orig_entries);
			       print "\n eta_conrecord in con_normalized reduced to \n";
			       PpNil.pp_con con; print "\n"; raise e)
*)
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

  (* ------ Reduce one step if not in head-normal-form; return whether progress was made  ------ *)
  fun con_reduce_letfun state (sort,coder,var,formals,body,body_kind,rest,con) = 
	    let
	      val (D,subst) = state
	      val lambda = (Let_c (sort,[coder (var,formals,body,body_kind)],Var_c var))


	    in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var con) 
		   then (false, subst, lambda)
	       else
		   let 
		       val _ = 
			   (case (substitute subst var)  of
				SOME c => error "XXX var already in subst"
			      | _ => ())
		       val subst = add subst (var,lambda)
		   in  (true,subst,Let_c(sort,rest,con))
		   end
	    end

  fun con_reduce state (constructor : con) : bool * con subst * con  = 
    (case constructor of
          (Prim_c (pcon,args)) => (false,#2 state, constructor)
	| (Mu_c (recur,defs)) => (false,#2 state, constructor)
	| (AllArrow_c (openness,effect,tformals,formals,numfloats,body)) => (false,#2 state, constructor)
	| (Var_c var) => 
	 let val (D,subst) = state
	 in  (case (substitute subst var) of
		   SOME c => (true, subst, c)
		 | NONE =>
	           (case NilContext.find_kind_equation(D,Var_c var) of
			SOME c => (true, subst, c)
		      | NONE => (false, subst, Var_c var)))
	 end
        | (Let_c (sort,((cbnd as Open_cb (var,formals,body,body_kind))::rest),con)) =>
	 con_reduce_letfun state (sort,Open_cb,var,formals,body,body_kind,rest,con)
        | (Let_c (sort,((cbnd as Code_cb (var,formals,body,body_kind))::rest),con)) =>
	 con_reduce_letfun state (sort,Code_cb,var,formals,body,body_kind,rest,con)

	| (Let_c (sort,cbnd as (Con_cb(var,con)::rest),body)) =>
	    let val (D,subst) = state
		val con = Subst.substConInCon subst con
		val subst = add subst (var,con)
	    in  (true,subst,Let_c(sort,rest,body))
	    end
	| (Let_c (sort,[],body)) => (true,#2 state,body)
	| (Closure_c (c1,c2)) => (case con_reduce state c1 of
				      (true,subst,c) => (true,subst,Closure_c(c,c2))
				    | _ => (false,#2 state,constructor))
	| (Crecord_c _) => (false,#2 state,constructor)
	| (Proj_c (c,lab)) => 
	      (case con_reduce state c of
		   (true,subst,c) => (true,subst,Proj_c(c,lab))
		 | (false,_,c) => 
		       let val (D,subst) = state
			   val (progress,con) = beta_conrecord' (Proj_c (c,lab))
		       in  if progress
			       then (true,subst,con)
			   else case (NilContext.find_kind_equation(D,con)) of
			       NONE => (false,subst,con)
			     | SOME c => (true,subst,c)
		       end)
	| (App_c (cfun,actuals)) => 
	       (case con_reduce state cfun of
		    (true,subst,c) => (true,subst,App_c(c,actuals))
		  | (false,_,c) => 
			let val (D,subst) = state
			    val (progress,con) = beta_confun' true D (App_c(c,actuals))
			in  if progress
				then (true,subst,con)
			    else case (NilContext.find_kind_equation(D,con)) of
				NONE => (false,subst,con)
			      | SOME c => (true,subst,c)
			end)
	| (Typecase_c {arg,arms,default,kind}) => error "typecase not done yet"
	| (Annotate_c (annot,con)) => con_reduce state con)


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
    (case bnd of
          Con_b (p, cb) => error "sorry not handled"
	| Exp_b (var, con, exp) =>
	 let
	   val con = con_normalize' state con
	   val exp = exp_normalize' state exp
	   val bnd = Exp_b (var,con,exp)
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
	| Handle_e (exp,v,handler,con) =>
	 let
	   val exp = exp_normalize' state exp
	   val con = con_normalize' state con
	       (* XXX need to bind v *)
	   val handler = exp_normalize' state handler
	 in Handle_e (exp,v,handler,con)
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
  val beta_confun = beta_confun false

  val get_shape = wrap "get_shape" get_shape
  val kind_normalize = wrap "kind_normalize" kind_normalize
  val con_normalize = wrap "con_normalize"  con_normalize
  val con_reduce_once = wrap "con_reduce_once" con_reduce
  val exp_normalize = wrap "exp_normalize" exp_normalize
  val module_normalize = wrap "mod_normalize" module_normalize

  val kind_normalize' = wrap "kind_normalize'" kind_normalize'
  val con_normalize' = wrap "con_normalize'"  con_normalize'
  val exp_normalize' = wrap "exp_normalize'" exp_normalize'


    fun is_hnf c : bool = 
        (case c of
             Prim_c(pc,clist) => true
           | AllArrow_c _ => true
           | Var_c _ => false
           | Let_c _ => false
           | Mu_c _ => true
           | Proj_c (Mu_c _,_) => true
           | Proj_c _ => false
           | App_c _ => false
           | Crecord_c _ => true
           | Closure_c _ => error "Closure_c not a type"
           | Typecase_c _ => false
           | Annotate_c (_,c) => false)

    fun reduce_until_hnf(env,c) : con = 
        let fun diagnose n [] = error "reduce_until_hnf: diagnose"
              | diagnose n (c::rest) = (print "reduce_until_hnf(";
                                        print (Int.toString n);
                                        print ") =\n"; PpNil.pp_con c; print "\n";
                                        diagnose (n+1) rest)
            fun loop (n,past) (subst,c) = 
            if (n>1000) then diagnose 0 (rev past)
                else 
            if (is_hnf c)
                then (subst,c )
            else let val next = (n+1,c::past)
                     val (progress,subst,c) = con_reduce_once(env,subst) c
                 in  if progress then loop next (subst,c) else (subst,c)
                 end
            val (subst,c) = loop (0,[]) (Subst.empty(),c)
        in  Subst.substConInCon subst c
        end

    fun lab2int l ~1 = error "lab2int failed"
      | lab2int l n = if (eq_label(l,NilUtil.generate_tuple_label n))
			  then n else lab2int l (n-1)

    fun expandMuType(D:context, mu_con:con) =
	let fun extract (defs,which) =
	    let val defs = Sequence.toList defs
		fun mapper (n,(v,_)) = 
		    if (length defs = 1) 
			then (v,mu_con)
		    else (v,Proj_c(mu_con,NilUtil.generate_tuple_label(n+1)))
		val subst = Subst.fromList(Listops.mapcount mapper defs)
		val (_,c) = List.nth(defs,which-1)
	    in  Subst.substConInCon subst c
	    end
	in  (case (reduce_until_hnf(D,mu_con)) of
		 Mu_c (_,defs) => extract(defs,1)
	       | Proj_c(Mu_c (_,defs), l) => extract(defs,lab2int l (Sequence.length defs))
	       | _ => error "expandMuType reduced to non-mu type")
	end

    fun projectTuple(D:context, c:con, l:label) = 
	(case (reduce_until_hnf(D,c)) of
	     c as (Crecord_c _) => beta_conrecord(Proj_c(c,l))
	   | c => (print "projectTuple reduced to non-crecord type = \n";
		   PpNil.pp_con c; print "\n";
		   error "projectTuple reduced to non-crecord type"))

    fun projectRecordType(D:context, c:con, l:label) = 
	(case (reduce_until_hnf(D,c)) of
	     Prim_c(Record_c labs, cons) =>
		 (case (Listops.assoc_eq(eq_label,l,Listops.zip labs cons)) of
		      NONE => error "projectRecordType could not find field"
		    | SOME c => c)
	   | c => (print "projectRecordType reduced to non-record type = \n";
		   PpNil.pp_con c; print "\n";
		   error "projectRecordType reduced to non-record type"))

    fun projectSumType(D:context, c:con, s:TilWord32.word) = 
	(case (reduce_until_hnf(D,c)) of
	     Prim_c(Sum_c {tagcount,totalcount,known}, cons) =>
		 if (TilWord32.ult(s,tagcount))
		     then error "projectSumType: asking for tag fields"
		 else 
		     let val nontagcount = TilWord32.toInt(TilWord32.uminus(totalcount,tagcount))
			 val which = TilWord32.toInt(TilWord32.uminus(s,tagcount))
		     in  case (nontagcount,which) of
			 (0,_) => error "projectSumType: only tag fields"
		       | (1,0) => hd cons
		       | _ => projectTuple(D,hd cons, NilUtil.generate_tuple_label (which + 1))
		     end
	   | c => (print "projectSumType reduced to non-sum type = \n";
		   PpNil.pp_con c; print "\n";
		   error "projectSumType reduced to non-sum type"))

   fun type_of_switch (D:context,switch:switch):con  = 
     (case switch
	of Intsw_e {result_type,...} => result_type
	 | Sumsw_e {result_type,...} => result_type
	 | Exncase_e {result_type,...} => result_type
	 | Typecase_e {result_type,...} => result_type)

   fun type_of_value (D,value) = 
     (case value 
	of int (intsize,_) => Prim_c (Int_c intsize,[])
	 | uint (intsize,_) => Prim_c (Int_c intsize,[])
	 | float (floatsize,string) => Prim_c (Float_c floatsize,[])
	 | array (con,arr) => Prim_c (Array_c,[con])
	 | vector (con,vec) => Prim_c (Vector_c,[con])
	 | refcell expref => Prim_c (Ref_c,[type_of(D,!expref)])
	 | tag (atag,con) => Prim_c (Exntag_c,[con]))

   and type_of_fbnd (D,openness,constructor,defs) = 
     let
       fun ftype (Function (effect,recursive,tformals,formals,fformals,body,return)) = 
	 let
	   val num_floats = Word32.fromInt (List.length fformals)
	   val con = AllArrow_c (openness,effect,tformals,#2 (unzip formals),num_floats,return)
	 in
	   con
	 end
       val def_list = Sequence.toList defs
       val (vars,functions) = unzip def_list
       val declared_c = map ftype functions
       val bnd_types = zip vars declared_c
       val D = NilContext.insert_con_list (D,bnd_types)
     in
       D
     end
   and type_of_bnds (D,bnds) = 
     let
       fun folder (bnd,(D,subst)) = 
	 (case bnd of
	       Con_b (phase, cbnd) => 
		   let val D = NilContext.kind_of_bnds (D,[cbnd])
		       val (v,c) = 
			   (case cbnd of
				Con_cb (v,c) => (v,c)
			      | Open_cb(v,vklist,c,k) => (v,Let_c(Sequential,[cbnd],Var_c v))
			      | Code_cb(v,vklist,c,k) => (v,Let_c(Sequential,[cbnd],Var_c v)))
		       val subst = Subst.add subst (v,Subst.substConInCon subst c)
		   in  (D,subst)
		   end
	     | Exp_b (var, _, exp) =>
	      let
		val con = type_of (D,exp)
		val D = NilContext.insert_con(D,var,con)
	      in
		(D,subst)
	      end
	     | (Fixopen_b defs) => (type_of_fbnd(D,Open,Fixopen_b,defs),subst)
	     | (Fixcode_b defs) => (type_of_fbnd(D,Code,Fixcode_b,defs),subst)
	     | Fixclosure_b (is_recur,defs) => 
	      let
		val defs_l = Sequence.toList defs
		val defs_l = Listops.map_second (fn cl => #tipe cl) defs_l
		val D = NilContext.insert_con_list (D,defs_l)
	      in 
		(D,subst)
	      end)
     in
       List.foldl folder (D,Subst.empty()) bnds
     end
   and type_of_prim (D,prim,cons,exps) = 
       (case prim of
	    record labs => Prim_c(Record_c labs, map (fn e => type_of(D,e)) exps)
	  | select lab => projectRecordType(D,type_of(D,hd exps),lab)
	  | inject s => hd cons
	  | inject_record s => hd cons
	  | project_sum s => projectSumType(D,hd cons, s)
	  | project_sum_record (s,lab) => let val summandType = projectSumType(D,hd cons, s)
					  in  projectRecordType(D,summandType,lab)
					  end
	  | box_float fs => Prim_c(BoxFloat_c fs,[])
	  | unbox_float fs => Prim_c(Float_c fs,[])
	  | roll => hd cons
	  | unroll => expandMuType(D,hd cons)
	  | make_exntag => Prim_c(Exntag_c, cons)
	  | inj_exn _ => Prim_c(Exn_c, [])
	  | make_vararg (openness,effect) => error "type_of vararg not done"
	  | make_onearg (openness,effect) => error "type_of onearg not done"
	  | peq => error "peq not done")

   and type_of (D : context,exp : exp) : con = 
     let
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
	      val (D,subst) = type_of_bnds (D,bnds)
	      val c = type_of (D,exp)
	    in
	      Subst.substConInCon subst c
	    end
	   | Prim_e (NilPrimOp prim,cons,exps) => type_of_prim (D,prim,cons,exps)
	   | Prim_e (PrimOp prim,cons,exps) =>   
	    let 
	      val (total,arg_types,return_type) = PrimUtil.get_type prim cons
	    in
	      return_type
	    end
	   | Switch_e switch => type_of_switch (D,switch)
	   | (App_e (openness,app,cons,texps,fexps)) =>
	    let
	      val app_con : con = type_of (D,app)
	      val  (_,_,tformals,_,_,body) = 
		(case (NilUtil.strip_arrow(reduce_until_hnf(D,app_con))) of
		      SOME c => c
		    | NONE => (print "Ill Typed expression - not an arrow type. c = \n";
			       PpNil.pp_con app_con;
			       print "\nexp = \n";
			       PpNil.pp_exp app;
			       print "\n";
			       error "Ill Typed expression - not an arrow"))

	      val subst = Subst.fromList (zip (#1 (unzip tformals)) cons)

	      val con = Subst.substConInCon subst body
	    in
	      con
	    end

	   | Raise_e (exp,con) => con
	   | Handle_e (exp,v,handler,con) => type_of (D,exp)
	    )
     end


end
