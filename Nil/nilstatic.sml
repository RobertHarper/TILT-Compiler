functor NilStaticFn(structure Annotation : ANNOTATION
		    structure PrimUtil : PRIMUTIL
		    structure ArgNil : NIL
		    structure PpNil : PPNIL
		    structure Alpha : ALPHA
		    structure NilUtil : NILUTIL 
		    structure NilContext : NILCONTEXT
		    structure NilError : NILERROR 
		    structure Normalize : NORMALIZE
		    structure Subst : NILSUBST
		         sharing NilUtil.Nil = NilContext.Nil = Alpha.Nil 
			       = PpNil.Nil = NilError.Nil = ArgNil
			 and Annotation = ArgNil.Annotation
			 and ArgNil.Prim = PrimUtil.Prim
			 and type NilUtil.alpha_context = Alpha.alpha_context
			 and type PrimUtil.con = Subst.con = Normalize.con = ArgNil.con 
		         and type PrimUtil.exp = Subst.exp = Normalize.exp = ArgNil.exp
			 and type Subst.kind = Normalize.kind = ArgNil.kind
			 and type Subst.bnd = ArgNil.bnd
			 and type Subst.subst = NilContext.subst = Normalize.subst
			 and type Normalize.context = NilContext.context) :(*>*) NILSTATIC 
        where Nil = ArgNil 
	and type context = NilContext.context = 
struct	
  
  structure Annotation = Annotation
  structure Nil = ArgNil
  open Nil 
  open Prim

  val closed_check = ref false
  val debug = ref false
  val show_calls = ref false
  val select_carries_types = Stats.bool "select_carries_types"
  val bnds_made_precise = Stats.bool "bnds_made_precise"

  fun error s = Util.error "nilstatic.sml" s
  local
      datatype entry = 
	EXP of exp * NilContext.context 
      | CON of con * NilContext.context 
      | KIND of kind * NilContext.context
      | SUBKIND of kind * kind * NilContext.context
      | BND of bnd * NilContext.context
      | MODULE of module * NilContext.context
      val stack = ref ([] : entry list)
      val maxdepth = 10000
      val depth = ref 0
      fun clear_stack() = (depth := 0; stack := [])
      fun push e = (depth := !depth + 1;
		    stack := (e :: (!stack));
		    if (!debug andalso (!depth mod 20 = 0))
			then (print "****nilstatic.sml: stack depth = ";
			      print (Int.toString (!depth));
			      print "\n")
		    else ();
		    if (!depth) > maxdepth
			then error "stack depth exceeded"
		    else ())
  in
    fun push_exp (e,context) = push (EXP(e,context))
    fun push_con(c,context) = push(CON(c,context))
    fun push_kind(k,context) = push(KIND(k,context))
    fun push_subkind(k1,k2,context) = push(SUBKIND(k1,k2,context))
    fun push_bnd(b,context) = push(BND(b,context))
    fun push_mod(m,context) = push(MODULE(m,context))
    fun pop() = (depth := !depth - 1;
		 stack := (tl (!stack)))
    fun show_stack() = let val st = !stack
			   val _ = clear_stack()
			   fun show (EXP(e,context)) = 
				     (print "exp_valid called with expression =\n";
				      PpNil.pp_exp e;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (CON(c,context)) =
				     (print "con_valid called with constructor =\n";
				      PpNil.pp_con c;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (KIND(k,context)) =
				     (print "kind_valid called with kind =\n";
				      PpNil.pp_kind k;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (SUBKIND(k1,k2,context)) =
				     (print "sub_kind called with kind1 =\n";
				      PpNil.pp_kind k1;
				      print "\n                 and kind2 =\n";
				      PpNil.pp_kind k2;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (BND(b,context)) =
				     (print "bnd_valid called with bound =\n";
				      PpNil.pp_bnd b;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (MODULE(m,context)) =
				     (print "module_valid called with module =\n";
				      PpNil.pp_module m;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
		       in  app show (rev st)
		       end
    fun wrap str f arg = (f arg) 
      handle e => (print "Error while calling "; print str; print "\n"; show_stack(); raise e)
  end

  (* Local rebindings from imported structures *)

  (*From Normalize*)
  val con_normalize = Normalize.con_normalize
  val kind_normalize = Normalize.kind_normalize
  val exp_normalize = Normalize.exp_normalize
  val con_normalize' = Normalize.con_normalize'
  val kind_normalize' = Normalize.kind_normalize'
  val exp_normalize' = Normalize.exp_normalize'
  val beta_conrecord = Normalize.beta_conrecord
  val beta_confun = Normalize. beta_confun
  val eta_conrecord = Normalize.eta_conrecord
  val eta_confun = Normalize.eta_confun
  val beta_typecase  = Normalize.beta_typecase

  (*From NilContext*)
  type context = NilContext.context
  val empty = NilContext.empty
  val insert_con = NilContext.insert_con
  val insert_con_list = NilContext.insert_con_list
  val insert_code_con = NilContext.insert_code_con
  val insert_code_con_list = NilContext.insert_code_con_list
  val find_con = NilContext.find_con
  val bind_kind = NilContext.bind_kind
  val bind_kind_list = NilContext.bind_kind_list
  val insert_kind = NilContext.insert_kind
  val unpull_convar = NilContext.unpull_convar
  val find_kind' = NilContext.find_kind'

(*  val leave_top_level = NilContext.leave_top_level *)
(*  val code_context = NilContext.code_context *)
  fun leave_top_level D = if (!closed_check) 
			      then NilContext.leave_top_level D
			  else D
  fun code_context D = if (!closed_check) 
			      then NilContext.code_context D
			  else D

  (*From Alpha*)
  type alpha_context = Alpha.alpha_context

  (*From NilUtil*)
  val substConInExp = Subst.substConInExp
  val substConInCon = Subst.substConInCon
  val substConInKind = Subst.substConInKind
  val substExpInExp = Subst.substExpInExp
  val substConInBnd = Subst.substConInBnd
  val varConConSubst = Subst.varConConSubst
  val varConKindSubst = Subst.varConKindSubst
  val empty_subst = Subst.empty
  val con_subst_compose = Subst.con_subst_compose

  val exp_tuple = NilUtil.exp_tuple
  val con_tuple = NilUtil.con_tuple
  val convar_occurs_free = NilUtil.convar_occurs_free
  val con_free_convar = NilUtil.con_free_convar
  val same_openness = NilUtil.same_openness
  val same_effect = NilUtil.same_effect
  val primequiv = NilUtil.primequiv
  val sub_phase = NilUtil.sub_phase
  val get_phase = NilUtil.get_phase
  val alpha_equiv_con = NilUtil.alpha_equiv_con
  val alpha_equiv_kind = NilUtil.alpha_equiv_kind
  val alpha_sub_kind = NilUtil.alpha_sub_kind
  val alpha_normalize_con = NilUtil.alpha_normalize_con
  val alpha_normalize_kind = NilUtil.alpha_normalize_kind

  val is_var_e = NilUtil.is_var_e

  val map_annotate = NilUtil.map_annotate
(*   val singletonize = NilUtil.singletonize *)
  val get_arrow_return = NilUtil.get_arrow_return

  val strip_var = NilUtil.strip_var
  val strip_exntag = NilUtil.strip_exntag
  val strip_recursive = NilUtil.strip_recursive
  val strip_boxfloat = NilUtil.strip_boxfloat
  val strip_float = NilUtil.strip_float
  val strip_int = NilUtil.strip_int
  val strip_sum = NilUtil.strip_sum
  val strip_arrow = NilUtil.strip_arrow
  val strip_record = NilUtil.strip_record
  val strip_crecord = NilUtil.strip_crecord
  val strip_proj = NilUtil.strip_proj
  val strip_prim = NilUtil.strip_prim
  val strip_app = NilUtil.strip_app
  val is_exn_con = NilUtil.is_exn_con
  val is_var_c = NilUtil.is_var_c
  val is_float_c = NilUtil.is_float_c 
  val strip_singleton = NilUtil.strip_singleton

  (*From Name*)
  val eq_var = Name.eq_var
  val eq_var2 = Name.eq_var2
  val eq_label = Name.eq_label
  val var2string = Name.var2string
  val label2string = Name.label2string
  val fresh_named_var = Name.fresh_named_var
  fun fresh_var () = fresh_named_var "nilstatic"
  val derived_var = Name.derived_var

  (*From Listops*)
  val assoc_eq = Listops.assoc_eq
  val eq_len = Listops.eq_len
  val eq_len3 = Listops.eq_len3
  val map_second = Listops.map_second
  val foldl_acc = Listops.foldl_acc
  val foldl2 = Listops.foldl2
  val map = Listops.map
  val map2 = Listops.map2
  val map3 = Listops.map3
  val map0count = Listops.map0count
  val app2 = Listops.app2
  val app3 = Listops.app3
  val zip = Listops.zip
  val zip3 = Listops.zip3
  val unzip = Listops.unzip
  val unzip3 = Listops.unzip3
  val unzip4 = Listops.unzip4
  val all = Listops.all
  val all2 = Listops.all2
  val all3 = Listops.all3
  val split = Listops.split
  val opt_cons = Listops.opt_cons
  val find2 = Listops.find2
  val labels_distinct = Listops.no_dups Name.compare_label

  (*From PrimUtil*)
  val same_intsize = PrimUtil.same_intsize
  val same_floatsize = PrimUtil.same_floatsize

  (*From Util *)
  val set2list = Util.set2list
  val list2set = Util.list2set
  val mapsequence = Util.mapsequence
  val sequence2list = Util.sequence2list
  val list2sequence = Util.list2sequence
  val eq_opt = Util.eq_opt
  val map_opt = Util.mapopt
  val split_opt = Util.split_opt
  val printl = Util.printl
  val lprintl = Util.lprintl
  val curry2 = Util.curry2
  val curry3 = Util.curry3

  (*From NilError*)
  val c_all = NilError.c_all
  val c_all1 = NilError.c_all1
  val c_all2 = NilError.c_all2
  val c_all3 = NilError.c_all3

  val perr_e = NilError.perr_e
  val perr_c = NilError.perr_c
  val perr_k = NilError.perr_k
  val perr_e_c = NilError.perr_e_c
  val perr_c_c = NilError.perr_c_c
  val perr_c_k = NilError.perr_c_k
  val perr_k_k = NilError.perr_k_k
  val perr_c_k_k = NilError.perr_c_k_k
  val perr_e_c_c = NilError.perr_e_c_c

  val b_perr_k = NilError.b_perr_k

  val o_perr = NilError.o_perr
    
  val o_perr_e = NilError.o_perr_e
  val o_perr_c = NilError.o_perr_c
  val o_perr_k = NilError.o_perr_k
  val o_perr_e_c = NilError.o_perr_e_c
  val o_perr_c_c = NilError.o_perr_c_c
  val o_perr_k_k = NilError.o_perr_k_k
  val o_perr_c_k_k = NilError.o_perr_c_k_k
  val o_perr_e_c_c = NilError.o_perr_e_c_c
  (* Local helpers *)



    
  fun mark_as_checked (con,kind) = con(*
    (case con
       of (Annotate_c (TYPECHECKED _,_)) => con
	| (Annotate_c (annote,con)) =>
	 let
	   val con = mark_as_checked (con,kind)
	 in
	   Annotate_c (annote,con)
	 end
	| _ => Annotate_c (TYPECHECKED kind,con))
 *)


  fun foldl_all2 ffun init (l1,l2) = 
    let 
      fun loop ([],[],state) = (true,state)
	| loop (a::arest,b::brest,state) =
	let
	  val (flag,state) = ffun (a,b,state)
	in
	  if flag then
	    loop (arest,brest,state)
	  else
	    (false,state)
	end
	| loop _ = error "foldl_all2 passed lists of unequal length"
    in
      loop (l1,l2,init)
    end
    
  fun bind_at_kind' ((D,subst),(var,kind)) = 
    let
      val kind = substConInKind subst kind
      val kind = kind_valid (D,kind)
      val var' = if (!closed_check orelse (case find_kind'(D,var) of
					       SOME _ => true
					     | _ => false))
		     then derived_var var 
		 else var
      val D = insert_kind (D,var',kind)
      val subst = Subst.add subst (var,Var_c var')
    in
      ((D,subst),var',kind)
    end
 
  and bind_at_kind (D,var,kind) = bind_at_kind' ((D,Subst.empty()),(var,kind))

  and bind_at_kinds D kinds = 
    let
      fun folder ((v,k),state) = 
	let
	  val (state,v,k) = bind_at_kind' (state,(v,k))
	in
	  ((v,k),state)
	end
      val (kinds,state) = foldl_acc folder (D,Subst.empty()) kinds
    in
      (state,kinds)
    end
 
 
  and kind_valid (D,kind) = 
      let val _ = push_kind(kind,D)
	  val _ = if (!show_calls)
		      then (print "kind_valid called with kind =\n";
			    PpNil.pp_kind kind;
			    print "\nand context"; NilContext.print_context D;
			    print "\n\n")
		  else ()
        val res = kind_valid'(D,kind)
	  val _ = pop()
      in  res
      end

  and kind_reduce(D,kind) = kind_normalize D kind

  and kind_valid' (D : context, kind : kind) : kind = 
    (case kind 
       of Type_k p => (Type_k p)
	| Word_k p => (Word_k p)
	| Singleton_k (p,kind,con) => 
	 let
	   val (con,kind') = (con_valid (D,con))
	   val kind = NilUtil.kill_singleton(kind_valid (D,kind))
	   val phase = get_phase kind
	 in
	   if sub_phase (p,phase) andalso sub_kind' (D,kind',kind) then
	     (singletonize D (kind,con))
	   else
	     (perr_c_k_k (con,kind,kind');
	      error "Invalid singleton kind" handle e => raise e)
	 end
	| Record_k elts => 
	 let
	   val elt_list = sequence2list elts
	   val (labels,vars_and_kinds) = unzip (map (fn ((l,v),k) => (l,(v,k))) elt_list)
	   val ((D,subst),vars_and_kinds) = bind_at_kinds D vars_and_kinds
	   val entries = 
	     map2 (fn (l,(v,k)) => ((l,v),k)) (labels,vars_and_kinds)
	 in  
	   if labels_distinct labels then
	     (Record_k (list2sequence entries))
	   else 
	     (perr_k kind;
	      error "Labels in record kind not distinct")
	 end
	| Arrow_k (openness, formals, return) => 
	 let
	   val ((D,subst),formals) = bind_at_kinds D formals
	   val return = kind_normalize' (D,subst) return
	 in
	   (Arrow_k (openness, formals,return))
	 end)


  and con_valid (D : context, constructor : con) : con * kind = 
      let val _ = push_con(constructor,D)
	  val _ = if (!show_calls)
		      then (print "con_valid called with constructor =\n";
			    PpNil.pp_con constructor; 
			    print "\nand context"; NilContext.print_context D;
			    print "\n\n")
		  else ()
	  val res as (c,k) = con_valid'(D,constructor)
	  val _ = pop()
      in  res
      end

  and pcon_valid (D : context, pcon : primcon, args : con list ) 
    : primcon * kind * (con list) * kind list = 
    let
      val (args,kinds) = 
	unzip (map (curry2 con_valid D) args)
    in
      (case pcon
	 of ((Int_c W64) | 
	     (Float_c F32) |
	     (Float_c F64)) => (pcon,Type_k Runtime,args,kinds)
	 | ((Int_c W32) | (Int_c W16) | (Int_c W8) | 
	    (BoxFloat_c F64) | (BoxFloat_c F32) |
	    (Exn_c) | (Array_c) | (Vector_c) | (Ref_c) | (Exntag_c)) 
	   => (pcon,Word_k Runtime,args,kinds)
	 | (Record_c labels) => 
	     (if labels_distinct labels then
		(if c_all (is_word_kind D) b_perr_k kinds 
		   then (Record_c labels,Word_k Runtime,args,kinds)
		 else
		   (error "Record contains field of non-word kind" handle e => raise e))
	      else
		(error "Record contains duplicate field labels" handle e => raise e))
	 | (Sum_c {known,tagcount}) => 
	      (if c_all (is_word_kind D) b_perr_k kinds then
		 let
		   val valid =  
		     (case known 
			of SOME i => 
			  (Word32.<=(Word32.fromInt 0,i) andalso 
			   Word32.<(i,tagcount+Word32.fromInt(length args)))
			 | NONE => true) 
		 in
		   if valid then
		     (pcon,Word_k Runtime,args,kinds)
		   else
		     (error "Illegal index to sum constructor" handle e => raise e) 
		 end
	       else
		 (error "Sum contains non-word component" handle e => raise e))
	 | (Vararg_c _) => 
		 (if c_all (is_word_kind D) b_perr_k kinds then
		    (pcon,Word_k Runtime,args,kinds)
		  else 
		    (error "Vararg has non-word component" handle e => raise e)))
    end

  (* obtain all information from con; kind only gives shape *)
  and singletonize D (kind,con) = 
    (case kind 
       of Type_k phase => Singleton_k(phase,Type_k phase,con)
	| Word_k phase => Singleton_k(phase,Word_k phase,con)
	| Singleton_k(_,k,_) => singletonize D (k,con)
	| Record_k entries => 
	   Record_k (mapsequence (fn ((l,v),k) => ((l,v),singletonize D
						   (k,Normalize.beta_conrecord(Proj_c (con,l))))) entries)
	| Arrow_k (openness,args,return) => 
	 let
	   val (D,var_kinds,subst) = bind_kind_list(D,args)
	   val (formal_vars,_) = ListPair.unzip args
	   val actuals = List.map Var_c formal_vars
	   val return = Subst.substConInKind subst return
	 in
	   Arrow_k (openness,args,singletonize D (return,Normalize.beta_confun D (App_c (con,actuals))))
	 end)


  and con_valid' (D : context, constructor : con) : con * kind = 
    (case constructor 
       of (Prim_c (pcon,args)) =>
	 let
	   val (pcon,kind,args,kinds) = pcon_valid (D,pcon,args)
	   val con = (Prim_c (pcon,args))
	 in
	     (con,singletonize D (kind,con))
	 end
	| (Mu_c (is_recur,defs,var)) =>
	 let
	   val def_list = sequence2list defs
	     
	   val (vars,cons) = unzip def_list

	   val var_kinds = map (fn var => (var,Word_k Runtime)) vars

	   (* leave top level? *)
	   val origD = D
	   val (D,var_kinds,subst) = bind_kind_list (D,var_kinds)

	   val (vars,_) = unzip var_kinds
	   val var = 
	     case Subst.substitute subst var
	       of SOME(Var_c var) => var
		| _ => var

	   val cons = map (substConInCon subst) cons

	   val checkD = if is_recur then D else origD
	   val (cons,kinds) = unzip (map (curry2 con_valid checkD) cons)
	   val defs = list2sequence (zip vars cons)
	   val con = Mu_c (is_recur,defs,var)
	   val kind = singletonize D (Word_k Runtime,con)
	 in
	   if c_all (is_word_kind D) b_perr_k kinds then
	     (con,kind)
	   else
	     (error "Invalid kind for recursive constructor" handle e => raise e)
	 end
	| (AllArrow_c (openness,effect,tformals,formals,numfloats,body)) =>
	 let
(*	   val D = leave_top_level D *)
	   val ((D,subst),tformals) = bind_at_kinds D tformals
	   val formals = map (substConInCon subst) formals
	   val body = substConInCon subst body
	   val (body,body_kind) = con_valid (D,body)
	   val (formals,formal_kinds) = 
	     unzip (map (curry2 con_valid D) formals)
	   val con = AllArrow_c (openness,effect,tformals,formals,numfloats,body)
	   val kind = Singleton_k(Runtime,Word_k Runtime,con)
	 in
	   (*ASSERT*)
	   if (c_all (is_type_or_word_kind D) b_perr_k formal_kinds) andalso 
	     (is_type_or_word_kind D body_kind) then
	     (mark_as_checked (con,kind),kind)
	   else
	     (error "Invalid arrow constructor" handle e => raise e)
	 end
	| (v as (Var_c var)) => 
	 (case find_kind' (D,var) 
	    of SOME (c,k) => (c,k)  (*already singeltonized*)
	     | NONE => 
	      (error ("Encountered undefined variable " ^ (Name.var2string var) 
		      ^" in con_valid") handle e => raise e))
	    
	| (Let_c (sort,(((cbnd as Open_cb (var,formals,body,body_kind))::rest) | 
			((cbnd as Code_cb (var,formals,body,body_kind))::rest)),con)) => 
	 let
	   val origD = D
	   val is_code = 
	     case cbnd 
	       of Open_cb _ => false
		| _ => true
	   val D = if is_code then code_context D else leave_top_level D
	   val ((D,subst),formals) = bind_at_kinds D formals
	   val _ = if (!debug)
		     then (print "formals1 are ";
			   app (fn (v,k) => (PpNil.pp_var v; print " :: "; 
					     PpNil.pp_kind k; print "\n")) formals;
			   print "\n")
		   else ()
	   val body = substConInCon subst body
	   val body_kind = substConInKind subst body_kind
	   val body_kind = kind_valid(D,body_kind)
	   val (body,body_kind') = con_valid (D,body)
	   val return_kind = if !bnds_made_precise then body_kind' else body_kind
	   val _ = if (sub_kind' (D,body_kind',body_kind)) then ()
		   else (perr_c_k_k (body,body_kind,body_kind');
			 (error "invalid return kind for constructor function" handle e => raise e))
	   val (constructor,openness) = if is_code then (Code_cb,Code) else (Open_cb,Open)
	   val lambda = (Let_c (sort,[constructor (var,formals,body,return_kind)],Var_c var))
	   val lambda = eta_confun lambda
	   val bndkind = Arrow_k(openness,formals,return_kind)
	   val bndkind = singletonize D (bndkind,lambda)
	   val lambda = mark_as_checked (lambda,bndkind)
	 in
	   if (null rest) andalso (is_var_c con) andalso 
	     eq_opt (eq_var,SOME var,strip_var con) then
	     (lambda,bndkind)
	   else
	     con_valid (origD,varConConSubst var lambda (Let_c (sort,rest,con)))
	 end
        | (Let_c (sort,cbnd as (Con_cb(var,kind,con)::rest),body)) =>
	   let
	     val kind = kind_valid(D,kind) 
	     val (con,kind') = con_valid (D,con)
	     val con = varConConSubst var (mark_as_checked (con,kind')) (Let_c (sort,rest,body))
	   in
	     if sub_kind' (D,kind',kind) then
	       con_valid (D,con)
	     else
	       (printl "Kind (error in constructor declaration.";
		perr_k_k (kind,kind');
		lprintl "in context = ";
		NilContext.print_context D; print "\n";
		(error "Kind error in constructor declaration" handle e => raise e))
	   end
	| (Let_c (sort,[],body)) => con_valid (D,body)
	| (Closure_c (code,env)) => 
	   let
(*	     val D = leave_top_level D *)
	     val (env,env_kind) = con_valid (D,env)
	     val (code,code_kind) =  con_valid (D,code)
	   in
	     case (strip_singleton code_kind)
	       of Arrow_k ((Code | ExternCode),vklist,body_kind) => 
		 let 
		   val (first,(v,klast)) = split vklist
		   val con = Closure_c (code,env)
		   val body_kind = varConKindSubst v env body_kind
		   val kind = Arrow_k(Closure,first,body_kind)
		   val kind = singletonize D (kind,con)
		 in
		   if sub_kind' (D,env_kind,klast) then
		     (con,kind)
		   else
		     (print "Invalid kind for closure environment:";
		      print " env_kind < klast failed\n";
		      print "env_kind is "; PpNil.pp_kind env_kind; print "\n";
		      print "klast is "; PpNil.pp_kind klast; print "\n";
		      print "code_kind is "; PpNil.pp_kind code_kind; print "\n";
		      (error "Invalid kind for closure environment" handle e => raise e))
		 end
		| _ => (error "Invalid closure: code component does not have code kind" handle e => raise e)
	   end
	| (Crecord_c entries) => 
	   let
	     val (labels,cons) = unzip entries
	     val distinct = labels_distinct labels
	     val (cons,kinds) = unzip (map (curry2 con_valid D) cons)
	     val k_entries = map2 (fn (l,k) => ((l,fresh_named_var "crec_static"),k)) (labels,kinds)
	     val entries = zip labels cons
	     val con = Crecord_c entries
	     val eta_con = eta_conrecord D con
	     val kind = singletonize D (Record_k (list2sequence k_entries),eta_con)
	   in 
	     if distinct then
	       (con,kind)
	     else
	       (PpNil.pp_list PpNil.pp_label' labels 
		("labels are: ",",",";",true);
	       (error "Labels in record of constructors not distinct" handle e => raise e))
	   end
	| (Proj_c (rvals,label)) => 
	 let
	   val (rvals,record_kind) = con_valid (D,rvals)

	   val entry_kinds = 
	     (case (strip_singleton record_kind) of
		 Record_k kinds => sequence2list kinds
	       | other => 
		   (perr_c_k (constructor,other);
		    lprintl "and context is";
		    NilContext.print_context D;
		    (error "Non-record kind returned from con_valid in projection" handle e => raise e)))

	   fun proj_kind (((l,v),k)::rest,subst) = 
	     if eq_label (l,label) then
	       kind_normalize' (D,subst) k
	     else
	       proj_kind (rest,Subst.add subst (v,Proj_c (rvals,l)))
	     | proj_kind ([],subst) = 
	       (perr_c_k (rvals,record_kind);
		error ("Label "^(label2string label)^" not found in record"))
	       
	   val kind = proj_kind (entry_kinds,Subst.empty())

	   val con = Proj_c (rvals,label)
	   val con = beta_conrecord con
	   val kind = singletonize D (kind,con)
	   val con = mark_as_checked (con,kind)
	 in
	   (con,kind)
	 end
	| (App_c (cfun_orig,actuals)) => 
	 let
	   val (cfun,cfun_kind) = con_valid (D,cfun_orig)
	   val (formals,body_kind) = 
	     case (strip_singleton cfun_kind) of
	         (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		| _ => (print "Invalid kind for constructor application\n";
			PpNil.pp_kind cfun_kind; print "\n";
			(error "Invalid kind for constructor application" handle e => raise e))

	   val (actuals,actual_kinds) = 
	     unzip (map (curry2 con_valid D) actuals)
	   val actuals = map2 mark_as_checked (actuals,actual_kinds)
	   val (formal_vars,formal_kinds) = unzip formals

	   val body_kind = kind_normalize' (D,(Subst.fromList (zip formal_vars actuals))) body_kind

	   val _ = 
	     if c_all2 (fn (k1,k2) => sub_kind' (D,k1,k2))
	       (o_perr_k_k "Constructor function applied to wrong number of arguments") 
	       (actual_kinds,formal_kinds) 
	       then ()
	     else
	       (error "Constructor function failed: argument not subkind of expected kind" handle e => raise e)
		 
	   val con = App_c (cfun,actuals)
	   val D = (case cfun_orig of
			Var_c v => unpull_convar(D,v)
		      | _ => D)
	   val con = beta_confun D con
	   val kind = singletonize D (body_kind,con)
	 in
	   (con,kind)
	 end
	| (Typecase_c {arg,arms,default,kind=given_kind}) => 
	 let
	   val given_kind = kind_valid (D,given_kind)
	   fun doarm (pcon,args,body) = 
	     let
	       val (vars,kinds) = unzip args
	       val argcons = map Var_c vars
	       val args = zip vars kinds
	       val ((D,subst),args) = bind_at_kinds D args
	       val argcons = map (substConInCon subst) argcons
	       val body = substConInCon subst body
	       val (pcon,pkind,argcons,kinds) = pcon_valid (D,pcon,argcons)
	       val (body,body_kind) = con_valid(D,body)
	     in
	       if sub_kind' (D,body_kind,given_kind) then
		 (pcon,args,body)
	       else
		 (perr_k_k (given_kind,body_kind);
		  (error "Illegal kind in typecase" handle e => raise e))
	     end
	   val (arg,arg_kind) = con_valid (D,arg)
	   val (default,def_kind) = con_valid (D,default)
	   val arms = map doarm arms
	   val con = Typecase_c {arg=arg,arms=arms,
				 default=default,kind=given_kind}
	   val con = beta_typecase D con
	   val kind = singletonize D (given_kind,con)
	 in
	   if sub_kind' (D,def_kind,kind) andalso
	     is_type_or_word_kind D arg_kind then
	     (con,kind)
	   else
	     (error "Error in type case" handle e => raise e)
	 end
	| (Annotate_c (TYPECHECKED kind,con)) => (constructor,kind)
	| (Annotate_c (annot,con)) => 
	 let
	   val (con,kind) = con_valid (D,con)
	 in
	   (Annotate_c (annot,con),kind)
	 end)

  (*PRE: kind1 and kind2 are normalized - 
   * i.e. no singletons at higher kinds, all constructors are normalized
   *)
  and sub_kind' (D,kind1,kind2) = 
    let
	val _ = push_subkind(kind1,kind2,D)
      fun sub_one ((var1,kind1),(var2,kind2),(D,subst1,subst2)) = 
	let
	  val kind1 = kind_normalize' (D,subst1) kind1
	  val kind2 = kind_normalize' (D,subst2) kind2
	  val var' = derived_var var1
	  val D' = insert_kind (D,var',kind1)
	  val subst1 = Subst.add subst1 (var1,Var_c var')
	  val subst2 = Subst.add subst2 (var2,Var_c var')
	in
	  (sub_kind' (D,kind1,kind2),(D',subst1,subst2))
	end

      fun sub_all D (vks1,vks2) = 
	foldl_all2 sub_one (D,empty_subst(),empty_subst()) (vks1,vks2)
      val res = 
      (case (kind1,kind2)
	 of (Word_k p1, Word_k p2) => sub_phase(p1,p2)
	  | (Type_k p1, Type_k p2) => sub_phase(p1,p2)
	  | (Word_k p1, Type_k p2) => sub_phase(p1,p2)
	  | (Singleton_k (p1,_,_) ,Type_k p2) => sub_phase(p1,p2)
	  | (Singleton_k (p1,k,c),Word_k p2) => sub_phase(p1,p2) andalso is_word_kind D k
	  | (Singleton_k (p1,k1,c1),Singleton_k (p2,k2,c2)) => 
	   sub_phase(p1,p2) andalso alpha_equiv_con (c1,c2)
	  | (Arrow_k (openness1, formals1, return1), Arrow_k (openness2, formals2, return2)) => 
	   (if eq_len (formals1,formals2) then
	      let
		val (formals_ok,(D,subst2,subst1)) = sub_all D (formals2,formals1)
		(* Notice reversal of order!!!*)
		  
		val return1 = kind_normalize' (D,subst1) return1
		val return2 = kind_normalize' (D,subst2) return2
	      in
		formals_ok andalso 
		same_openness (openness1,openness2) andalso 
		 sub_kind' (D,return1,return2)
	      end
	    else
	      (lprintl "formals of different lengths!!";
	       false))
	  | (Record_k elts1,Record_k elts2) => 
	   let
	     val split_lbls = unzip o (map (fn ((l,v),k) => (l,(v,k))))
	     val (labels1,vks1) = split_lbls elts1
	     val (labels2,vks2) = split_lbls elts2
	   in
	     eq_len (elts1,elts2) andalso 
	     (#1 (sub_all D (vks1,vks2))) andalso
	     all2 eq_label (labels1,labels2)
	   end
	  | (_,_) => false)
	 orelse
	 (if !debug then
	    (lprintl "sub_kind failed!";
	     printl "Kind:";
	     PpNil.pp_kind kind1;
	     lprintl "Not equivalent to :";
	     PpNil.pp_kind kind2;
	     printl "")
	  else ();
	    false)
      val _ = pop()
    in res
    end
(*and is_word (D,con) = 
    (case con
       of (Prim_c (pcon,args)) =>
	 (case pcon
	    of ((Int_c W64) | 
		(Float_c F32) |
		(Float_c F64)) => false
	    | _ => true)
	| (Mu_c _) => true
	| (AllArrow_c _) => true
	| (v as (Var_c var)) => 
	    (case find_kind (D,var) 
	       of SOME k =>
		 (case strip_singleton k 
		    of Word_k _ => true
		     | _ => false)
		| NONE => 
		    (error ("Encountered undefined variable " ^ (Name.var2string var) 
			    ^" in is_word") handle e => raise e))
	| (Let_c (sort,((cbnd as [Open_cb (var,formals,body,body_kind)]) | 
			(cbnd as [Code_cb (var,formals,body,body_kind)])),Var_c var')) => false
	| (Let_c _) => error "is_word called on un-normalized let_c"
	| (Closure_c (code,env)) => false
	| (Crecord_c entries) => false
	| (Proj_c (rvals,label)) => 
	 let
	   val (rvals,record_kind) = con_valid (D,rvals)
	   val entry_kinds = 
	     (case (strip_singleton record_kind) of
		 Record_k kinds => sequence2list kinds
	       | other => 
		   (error "Non-record kind returned from con_valid in projection" handle e => raise e))
	   val kind = 
	     (case List.find (fn ((l,v),k) => eq_label (l,label)) entry_kinds
		of SOME (_,k) => k
		 | NONE => (perr_c_k (rvals,record_kind);
			    error ("Label "^(label2string label)^" not found in record")))
	 in
	   is_word_kind D kind
	 end
	| (App_c (cfun,actuals)) => 
	 let
	   val (cfun,cfun_kind) = con_valid (D,cfun)
	   val (formals,body_kind) = 
	     case (strip_singleton cfun_kind) of
	         (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		| _ => (print "Invalid kind for constructor application\n";
			PpNil.pp_kind cfun_kind; print "\n";
			(error "Invalid kind for constructor application" handle e => raise e))
	 in
	   is_word_kind D body_kind
	 end
	| (Typecase_c {kind,...}) => 
	 is_word_kind D kind
	| (Annotate_c (annot,con)) => is_word(D,con))*)
  and is_word_kind D kind = sub_kind' (D,kind,Word_k (get_phase kind))
  and is_type_or_word_kind D kind = sub_kind' (D,kind,Type_k (get_phase kind))

  fun sub_kind (D,k1,k2) = 
    let
      val k1 = kind_normalize D k1
      val k2 = kind_normalize D k2
    in
      sub_kind' (D,k1,k2)
    end

  fun kind_equiv (D,k1,k2) = 
    let
      val k1 = kind_normalize D k1
      val k2 = kind_normalize D k2
    in
      alpha_equiv_kind (k1,k2)
    end

  fun con_equiv (D,c1,c2) = 
    let
      val c1 = con_normalize D c1
      val c2 = con_normalize D c2
    in
      alpha_equiv_con (c1,c2)
    end

  fun con_reduce (D,con) = con_normalize D con

(* Term level type checking.  *)

  fun get_function_type (openness,Function (effect,recursive,tformals,
					      formals,fformals,body,return)) = 
    let
      val num_floats = Word32.fromInt (List.length fformals)
      val con = AllArrow_c (openness,effect,tformals,#2 (unzip formals),num_floats,return)
    in
      con
    end

  fun value_valid (D,value) = 
    (case value
       of (int (intsize,word) |
	   uint (intsize,word)) => 
	 let
	   val kind = case intsize 
			of W64 => Type_k Runtime
			 | _ => Word_k Runtime
	 in
	   (value,Prim_c (Int_c intsize,[]))
	 end
	| float (floatsize,string) => 
	 (value,Prim_c (Float_c floatsize,[]))
	| array (con,arr) => 
	 let
	   val (con,kind) = con_valid (D,con)
	   fun check exp = 
	     let
	       val (exp',con') = exp_valid (D,exp)
	     in
	       if alpha_equiv_con (con,con') then
		 exp'
	       else
		 (error "Array contains expression of incorrect type" handle e => raise e)
	     end
	 in
	   Array.modify check arr;
	   (array (con,arr),Prim_c (Array_c,[con]))
	 end
	| vector (con,vec) => 
	 let
	   val (con,kind) = con_valid (D,con)
	   fun check exp = 
	     let
	       val (exp',con') = exp_valid (D,exp)
	     in
	       if alpha_equiv_con (con,con') then
		 exp'
	       else
		 (error "Vector contains expression of incorrect type" handle e => raise e)
	     end
	 in
	   Array.modify check vec;
	   (vector (con,vec),Prim_c (Vector_c,[con]))
	 end
	| refcell expref =>
	 let
	   val (exp,con) = exp_valid (D,!expref)
	 in
	   expref := exp;
	   (refcell expref,Prim_c (Ref_c,[con]))
	 end
	| tag (atag,con) => 
	 let
	   val (con,kind) = con_valid (D,con)
	 in
	   (tag (atag,con),
	    Prim_c (Exntag_c,[con]))
	 end)
  and prim_valid (D,prim,cons,exps) = 
    (case (prim,cons,exps)
       of (record labels,cons,exps) =>
	 let
	   val fields = zip3 labels exps cons
	   fun check_one (label,exp,con) = 
	     let
	       val (con,kind) = con_valid (D,con)
	       val (exp,con') = exp_valid (D,exp)
	     in
	       if alpha_equiv_con (con,con') then
		 (label,exp,con)
	       else
		 (perr_e_c_c (exp,con,con');
		  (error ("Type mismatch in record at field "^(label2string label)) handle e => raise e))
	     end
	   val fields = map check_one fields
	   val (labels,exps,cons) = unzip3 fields
	   val exp = (record labels,cons,exps)
	   val con = Prim_c (Record_c labels,cons)
	   val (con,kind) = con_valid (D,con)
	 in
	   if labels_distinct labels then
	     (exp,con)
	   else
	     (error "Fields not distinct" handle e => raise e)
	 end
	| (select label,given_types,[exp]) =>
	 let
	   val (exp,con) = exp_valid (D,exp)
	   val (labels,found_types) = 
	     (case strip_record con 
		of SOME x => x
		 | NONE => 
		  (perr_e_c (exp,con);
		   (error "Projection from value of non record type" handle e => raise e)))
	   val type_args = 
	     if !select_carries_types then
	       let
		 val (given_types,_) = unzip (map (curry2 con_valid D) given_types)
	       in
		 if c_all2 alpha_equiv_con (o_perr_c_c "Length mismatch in record select") 
		   (given_types,found_types) then
		   if !bnds_made_precise then found_types else given_types
		 else
		   (perr_e (Prim_e (NilPrimOp (select label),given_types,[exp]));
		    print "Record has type: ";
		    perr_c con;
		    error ("Mismatch in field types for record select of label "^(label2string label)))
	       end
	     else
	       case given_types 
		 of [] => []
		  | _ => error "Select does not carry types"
	 in
	   case find2 (fn (l,c) => eq_label (l,label)) (labels,found_types)
	     of SOME (_,con) => 
	       ((select label,type_args,[exp]),con)
	      | NONE => 
	       (perr_e_c (exp,con);
		printl ("Label "^(label2string label)^" projected from expression");
		(error "No such label" handle e => raise e))
	 end
	| (inject {tagcount,sumtype},cons,exps as ([] | [_])) =>  
	 let
	   val (cons,kinds) = 
	     unzip (map (curry2 con_valid D) cons)
	   val con = Prim_c (Sum_c {tagcount=tagcount,known=NONE},cons) (*Can't propogate sumtype*)
	 in
	   case exps 
	     of [] => 
	       if (sumtype < tagcount) then
		 ((prim,cons,[]),con)
	       else
		 (perr_e (Prim_e (NilPrimOp prim,cons,[]));
		  (error "Illegal injection - sumtype out of range" handle e => raise e))
	      | argexp::_ =>    
		 if (tagcount <= sumtype) andalso 
		   ((Word32.toInt sumtype) < ((Word32.toInt tagcount) + (List.length cons))) then
		   let
		     val (argexp,argcon) = exp_valid (D,argexp)
		     val con_k = List.nth (cons,Word32.toInt (sumtype-tagcount))
		   in
		     if alpha_equiv_con (argcon,con_k) then 
		       ((prim,cons,[argexp]),con)
		     else
		       (perr_e_c_c (argexp,con_k,argcon);
			(error "Illegal injection - type mismatch in args" handle e => raise e))
		   end
		 else
		   (perr_e (Prim_e (NilPrimOp prim,cons,[argexp]));
		    (error "Illegal injection - field out of range" handle e => raise e))
	 end
	| (inject_record {tagcount,sumtype},argcons,argexps) => 
	 let
	   val (argcons,argkinds) = 
	     unzip (map (curry2 con_valid D) argcons)
	   val (argexps,expcons) = 
	     unzip (map (curry2 exp_valid D) exps)
	   val con = Prim_c (Sum_c {tagcount=tagcount,known=NONE},argcons) (*can't propogate field*)
	 in
	   if (tagcount <= sumtype) andalso 
	     ((Word32.toInt sumtype) < ((Word32.toInt tagcount) + (List.length argcons))) then
	     let
	       val con_k = List.nth (argcons,Word32.toInt (sumtype-tagcount))
	       val (labels,cons) = 
		 case strip_record con_k
		   of SOME (ls,cs) => (ls,cs)
		    | NONE => (printl "Field is not a record ";
			       PpNil.pp_con con_k;
			       (error "Record injection on illegal field" handle e => raise e))
	     in
	       if c_all2 alpha_equiv_con 
		 (o_perr_c_c "Length mismatch in record") (expcons,cons) then 
		 ((prim,argcons,argexps),con)
	       else
		  (error "Illegal record injection - type mismatch in args" handle e => raise e)
	     end
	   else
	     (printl "Expression ";
	      PpNil.pp_exp (Prim_e (NilPrimOp prim,cons,exps));
	      (error "Illegal injection - field out of range" handle e => raise e))
	 end
	| (project_sum {tagcount,sumtype},argcons,[argexp]) => 
	 let
	   val (argexp,argcon) = exp_valid (D,argexp)
	   val (argcons,argkinds) = unzip (map (curry2 con_valid D) argcons)
	 in
	   case strip_sum argcon
	     of SOME (tagcount',SOME field,cons) =>
	       if tagcount' = tagcount andalso
		 field = sumtype andalso
		 (field >= tagcount) andalso
		 (Word32.toInt field) < ((Word32.toInt tagcount) + List.length argcons) then
		 let
		   val con_i = List.nth (argcons,Word32.toInt (field - tagcount))
		 in
		   if c_all2 alpha_equiv_con (o_perr_c_c "Length mismatch in project") (cons,argcons) then
		     ((prim,argcons,[argexp]),con_i)
		   else
		      (error "Arguments to project_sum don't match" handle e => raise e)
		 end
	       else 
		 (perr_e_c ((Prim_e (NilPrimOp prim,cons,exps)),
			    argcon);
		  (error "Illegal projection - numbers don't match" handle e => raise e))
	      | _ => 
		 (perr_e_c ((Prim_e (NilPrimOp prim,cons,exps)),
			    argcon);
		  (error "Illegal projection - expression not of sum type" handle e => raise e))
	 end
	| (project_sum_record {tagcount,sumtype,field},argcons,[argexp]) => 
	 let
	   val (argexp,argcon) = exp_valid (D,argexp)
	   val (argcons,argkinds) = unzip (map (curry2 con_valid D) argcons)
	 in
	   case strip_sum argcon
	     of SOME (tagcount',SOME sumtype',cons) =>
	       (if tagcount' = tagcount andalso
		  sumtype = sumtype' andalso
		  tagcount <= sumtype andalso
		  (Word32.toInt sumtype) < ((Word32.toInt tagcount) + List.length argcons) then
		  if c_all2 alpha_equiv_con (o_perr_c_c "Length mismatch in project sum") (cons,argcons) 
		    then
		      let
			val con_i = List.nth (argcons,Word32.toInt (sumtype - tagcount))
		      in
			case strip_record con_i
			  of SOME (labels,cons) =>
			      (case Listops.assoc_eq(Name.eq_label,field,Listops.zip labels cons) of
				   SOME field_con => ((prim,argcons,[argexp]),field_con)
				 | NONE => (error "Project_sum_record field out of range" handle e => raise e))
			   | NONE => 
			      (perr_c con_i;
			       (error "Non recrod type in sum record projection" handle e => raise e))
		      end
		  else
		    (error "Arguments to project_sum don't match" handle e => raise e)
		else 
		  (perr_e (Prim_e (NilPrimOp prim,cons,exps));
		   (error "Illegal projection - numbers don't match" handle e => raise e)))
	      | _ => 
		  (perr_e (Prim_e (NilPrimOp prim,cons,exps));
		   (error "Illegal projection - expression not of sum type" handle e => raise e))
	 end
	| (box_float floatsize,[],[exp]) => 
	 let
	   val (exp,con) = exp_valid (D,exp)
	   val box_con = Prim_c (BoxFloat_c floatsize,[])
	 in
	   case strip_float con
	     of SOME floatsize' => 
	       if same_floatsize (floatsize,floatsize') then
		 ((box_float floatsize,[],[exp]),
		  box_con)
	       else
		 (error "Mismatched float size in box float" handle e => raise e)
	      | NONE => (error "Box float called on non-float" handle e => raise e)
	 end
	| (unbox_float floatsize,[],[exp]) => 
	 let
	   val (exp,con) = exp_valid (D,exp)
	   val unbox_con = Prim_c (Float_c floatsize,[])
	 in
	   case strip_boxfloat con
	     of SOME floatsize' => 
	       if same_floatsize (floatsize,floatsize') then
		 ((unbox_float floatsize,[],[exp]),unbox_con)
	       else
		 (error "Mismatched float size in box float" handle e => raise e)
	      | NONE => (error "Unbox float called on non-boxfloat" handle e => raise e)
	 end
	| (roll,[argcon],[exp]) => 
	 let
	   val (argcon,argkind) = con_valid (D,argcon)
	   val (exp,con) = exp_valid (D,exp)
	 in
	   case strip_recursive argcon 
	     of SOME (is_recur,set,var) =>
	       let
		 val def_list = set2list set
		 val (_,con') = valOf (List.find (fn (v,c) => eq_var (v,var)) def_list)
		 val cmap = Subst.fromList (map (fn (v,c) => (v,Mu_c (is_recur,set,v))) def_list)
		 val con' = con_normalize' (D,cmap) con'
	       in
		 if alpha_equiv_con (con,con') then
		   ((roll,[argcon],[exp]),argcon)
		 else
		   (perr_e_c_c (exp,con',con);
		    (error "Error in roll" handle e => raise e))
	       end
	      | NONE => 
	       (printl "Roll primitive given argument of type";
		PpNil.pp_con argcon;
		lprintl " not a recursive type";
		(error "Illegal constructor argument in roll" handle e => raise e))
	 end
	| (unroll,[con],[exp]) =>
	 let
	   val (argcon,argkind) = con_valid (D,con)
	   val (exp,con) = exp_valid (D,exp)
	 in
	   (case strip_recursive argcon 
	      of SOME (is_recur,set,var) =>
		(if alpha_equiv_con (argcon,con) then
		   let
		     val def_list = set2list set
		     val (_,con') = valOf (List.find (fn (v,c) => eq_var (v,var)) def_list)
		     val cmap = Subst.fromList (map (fn (v,c) => (v,Mu_c (is_recur,set,v))) def_list)
		     val con' = con_normalize' (D,cmap) con'
		   in
		     ((unroll,[argcon],[exp]),con')
		   end
		 else
		   (perr_e_c_c (exp,argcon,con);
		    (error "Error in unroll" handle e => raise e)))
	       | NONE => 
		   (printl "Urnoll primitive given argument of type";
		    PpNil.pp_con argcon;
		    lprintl " not a recursive type";
		    (error "Illegal constructor argument in unroll" handle e => raise e)))
	 end
	| (make_exntag,[argcon],[]) => 
	 let
	   val (argcon,argkind) = con_valid (D,argcon)
	   val exp = (make_exntag,[argcon],[])
	   val con = Prim_c (Exntag_c,[argcon])
	 in
	   (exp,con)
	 end
	| (inj_exn name,[],[exp1,exp2]) => 
	 let
	   val (exp1,con1) = exp_valid (D,exp1)
	   val (exp2,con2) = exp_valid (D,exp2)
	 in
	   case strip_exntag con1
	     of SOME con => 
	       if alpha_equiv_con (con2,con) then
		 let
		   val exp = (inj_exn name,[],[exp1,exp2])
		   val con = Prim_c (Exn_c,[])
		 in
		   (exp,con)
		 end
	       else
		 (error "Type mismatch in exception injection" handle e => raise e)
	      | NONE =>  
		 (perr_e_c (exp1,con1);
		  (error "Illegal argument to exception injection - not a tag" handle e => raise e))
	 end
	| (make_vararg (openness,effect),cons,exps) =>
	 (error "make_vararg unimplemented....punting" handle e => raise e)
	| (make_onearg (openness,effect),cons,exps) =>  
	 (error "make_onearg unimplemented....punting" handle e => raise e)
	| (peq,cons,exps) => 
	   (error "Polymorphic equality should not appear at this level" handle e => raise e)
	| (prim,cons,exps) => 
	 (perr_e (Prim_e (NilPrimOp prim,cons,exps));
	  lprintl "No matching case in prim_valid";
	  (error "Illegal primitive application" handle e => raise e)))

  and switch_valid (D,switch)  = 
    (case switch
       of Intsw_e {info=intsize,arg,arms,default} =>
	 let
	   val (arg,argcon) = exp_valid (D,arg)
	   val (ns,arm_fns) = unzip arms
	   val arm_fns = map (curry2 function_valid D) arm_fns
	   val arms = zip ns arm_fns
	   val _ = 
	     case strip_int argcon
	       of SOME intsize' => 
		 if same_intsize (intsize,intsize') then ()
		 else (error "Integer size mismatch in int switch" handle e => raise e)
		| NONE => 
		 (perr_e_c (arg,argcon);
		  (error "Branch argument not an int" handle e => raise e))

	   fun check_arms (NONE,[]) = error "Int case must be non-empty"
	     | check_arms (SOME rep_con,[]) = rep_con
	     | check_arms (NONE,(Function (_,_,[],[],[],_,arm_ret))::rest) = check_arms (SOME arm_ret,rest)
	     | check_arms (SOME rep_con,(Function (_,_,[],[],[],_,arm_ret))::rest) =  
	     if alpha_equiv_con (rep_con,arm_ret) then 
	       check_arms (SOME rep_con,rest)
	     else
	       (perr_c_c (rep_con,arm_ret);
		(error "Branch arm types don't match" handle e => raise e))
	     | check_arms _ = error "Illegal arm type for int switch"
		   
	   val (default,default_con) = split_opt (map_opt (curry2 exp_valid D) default)
	   val rep_con = check_arms (default_con,arm_fns)
	 in
	   (Intsw_e {info=intsize,arg=arg,
		     arms=arms,default=default},
	    rep_con)
	 end
	| Sumsw_e {info=(non_val,val_cons),arg,arms,default} => 
	 let
	   val (arg,argcon) = exp_valid (D,arg)
	   val (ns,arm_fns) = unzip arms
	   val arm_fns = map (curry2 function_valid  D) arm_fns
	   val (val_cons,_) = unzip (map (curry2 con_valid D) val_cons)

	   val arms = zip ns arm_fns

	   local
	     val (sum_decl,_) = con_valid(D,Prim_c (Sum_c {tagcount=non_val,known=NONE},val_cons))
	     val cons = 
	       if alpha_equiv_con(sum_decl,argcon) then
		 case strip_sum argcon
		   of SOME (_,_,cons) => cons
		    | _ => 
		     (perr_e_c (arg,argcon);
		      (error "Branch argument not of sum type" handle e => raise e))
	       else
		 (perr_e_c (arg,argcon);
		  print "given type:\n";
	  	  PpNil.pp_con sum_decl; print "\n";
		  error "Type given for sum switch argument does not match found type")

	     fun mk_sum field = 
	       Prim_c (Sum_c {tagcount=non_val,
			      known=SOME ((Word32.fromInt field)+non_val)},cons)

	     val known_sums = map0count mk_sum (List.length cons)

	     fun check_arms (rep_con,known_sums) = 
	       let
		 fun check_loop [] = ()
		   | check_loop ((index,Function (_,_,[],args as ([] | [_]),[],_,arm_ret))::arm_rets) = 
		   if index < non_val then
		     if alpha_equiv_con (rep_con,arm_ret) andalso null args then
		       check_loop arm_rets
		     else 
		       (perr_c_c (rep_con,arm_ret);
			(error "non val Sum Branch arm types don't match" handle e => raise e))
		   else
		     let 
		       val sum_con = List.nth (known_sums,Word32.toInt(index-non_val))
		       val arg_con = case args
				       of [(_,arg_con)] => arg_con
					| _ => error "Illegal argument list in sum switch arm"
		     in
		       if (alpha_equiv_con (rep_con,arm_ret)) andalso
			 alpha_equiv_con (arg_con,sum_con) then 
			 check_loop arm_rets
		       else
			 (printl "Argument type :";
			  perr_c_c (sum_con,argcon);
			  printl "Return type";
			  perr_c_c (rep_con,arm_ret);
			  (error "Sum Branch arm types don't match" handle e => raise e))
		     end
		   | check_loop _ = error "Illegal arm type for sum switch"
	       in
		 check_loop
	       end
	     val (default,default_con) = split_opt (map_opt (curry2 exp_valid D) default)
	     val rep_con = 
	       (case (default_con,arms)
		  of (SOME con,_) => con
		   | (NONE,(_,Function (_,_,_,_,_,_,con))::_) => con
		   | (NONE,[]) => error "Illegal sum switch - empty!")
	     val _ = check_arms (rep_con,known_sums) arms
	   in
	     val default = default
	     val rep_con = rep_con
	   end

	 in
	   (Sumsw_e {info=(non_val,val_cons),arg=arg,
		     arms=arms,default=default},
	    rep_con)
	 end
     | Exncase_e {info=_,arg,arms,default} =>
	 let
	   val (arg,argcon) = exp_valid (D,arg)
	   val (vars,arm_fns) = unzip arms
	   val arm_fns = map (curry2 function_valid D) arm_fns
	   val (vars,var_cons) = unzip (map (curry2 exp_valid D) vars)

	   val arms = zip vars arm_fns
	     
	   fun check_arms (NONE,[],_) = error "Exn case must be non-empty"
	     | check_arms (SOME rep_con,[],_) = rep_con
	     | check_arms (NONE,(Function (_,_,[],[(_,arg_con)],[],_,arm_ret))::rest,var_con::var_cons) = 
	     (case strip_exntag var_con
		of SOME exn_con =>
		  if alpha_equiv_con (arg_con,exn_con) then 
		    check_arms(SOME arm_ret,rest,var_cons)
		  else
		    (printl "Argument type :";
		     perr_c_c (exn_con,arg_con);
		     (error "Exn Branch arm types don't match" handle e => raise e))
		 | NONE => (error ("Variable has wrong type" handle e => raise e)))
	     | check_arms (SOME rep_con,
			   (Function (_,_,[],[(_,arg_con)],[],_,arm_ret))::arm_fns,var_con::var_cons) = 
		(case strip_exntag var_con
		   of SOME exn_con =>
		     if (alpha_equiv_con (rep_con,arm_ret)) andalso
		       alpha_equiv_con (arg_con,exn_con) then 
		       check_arms(SOME rep_con,arm_fns,var_cons)
		     else
		       (printl "Argument type :";
			perr_c_c (exn_con,arg_con);
			printl "Return type";
			perr_c_c (rep_con,arm_ret);
			(error "Exn Branch arm types don't match" handle e => raise e))
		    | NONE => (error ("Variable has wrong type" handle e => raise e)))
	     | check_arms _ = error "Illegal arm type in exn switch"

	   val (default,default_con) = split_opt (map_opt (curry2 exp_valid D) default)
	   val rep_con = check_arms (default_con,arm_fns,var_cons)
	 in
	   (Exncase_e {info=(),arg=arg,
		       arms=arms,default=default},
	    rep_con)
	 end
     | Typecase_e {info,arg=argcon,arms,default} =>
	 let
	   val (argcon,argkind) = con_valid (D,argcon)
	   val (pcons,arm_fns) = unzip arms
	   val arm_fns = map (curry2 function_valid D) arm_fns

	   fun check_arms (NONE,[],_) = error "Type case must be non-empty"
	     | check_arms (SOME rep_con,[],_) = rep_con
	     | check_arms (NONE,(Function (_,_,[],args,[],_,arm_ret))::rest,pcon::pcons) = 
	     let
	       val (_,arg_cons) = unzip args
	       val (pcon,pkind,arg_cons,argkinds) = pcon_valid(D,pcon,arg_cons)
	     in
	       if sub_kind' (D,argkind,pkind) then
		 check_arms(SOME arm_ret,rest,pcons)
	       else
		 (perr_k_k (pkind,argkind);
		  (error "Typecase expression arm has wrong type" handle e => raise e))
	     end
	     | check_arms (SOME rep_con,(Function (_,_,[],args,[],_,arm_ret))::arm_fns,pcon::pcons) = 
	     let
	       val (_,arg_cons) = unzip args
	       val (pcon,pkind,arg_cons,argkinds) = pcon_valid(D,pcon,arg_cons)
	     in
	       if alpha_equiv_con (arm_ret,rep_con) andalso 
		 sub_kind' (D,argkind,pkind) then
		 check_arms(SOME rep_con,arm_fns,pcons)
	       else
		 (perr_c_c (rep_con,arm_ret);
		  perr_k_k (pkind,argkind);
		  (error "Typecase expression arm has wrong type" handle e => raise e))
	     end
	     | check_arms _ = error "Illegal arm type in type case"

	   val (default,default_con) = split_opt (map_opt (curry2 exp_valid D) default)
	   val rep_con = check_arms (default_con,arm_fns,pcons)
	   val arms = zip pcons arm_fns
	 in
	   (Typecase_e {info=(),arg=argcon,
			arms=arms,default=default},
	    rep_con)
	 end)
       
  and function_valid (D,Function (effect,recursive,tformals,
				  formals,fformals,body,return)) = 
    let
      val origD = D
      val ((D,subst),tformals) = bind_at_kinds D tformals
      fun check_c ((var,con),D) = 
	let
	  val con = substConInCon subst con
	  val (con,_) = con_valid(D,con)
	in
	  ((var,con),insert_con (D,var,con))
	end
      val (formals,D) = foldl_acc check_c D formals
      val D = 
	foldl (fn (v,D) => 
	       insert_con (D,v,Prim_c (Float_c F64,[]))) D fformals
      val body = substConInExp subst body
      val (body,body_c) = exp_valid (D,body)
      val return = substConInCon subst return
      val (return,_) = con_valid (D,return)
      val return_con = if !bnds_made_precise then body_c else return
      val function = Function (effect,recursive,tformals,formals,fformals,body,return_con)
    in
      if alpha_equiv_con (body_c,return) then
	function
      else
	(perr_e_c_c (body,return,body_c);
	 (error "Return expression has wrong type" handle e => raise e))
    end
  and bnds_valid (D,bnds) = bnds_valid' (bnds,(D,Subst.empty()))
  and bnd_valid (D,bnd) = bnd_valid' (bnd,(D,Subst.empty()))
  and bnds_valid' (bnds,(D,subst)) = foldl_acc bnd_valid' (D,subst) bnds
  and bnd_valid'' (bnd,(D,subst)) = 
    let
      val (bnd,subst) = substConInBnd subst bnd 
    in
      (case bnd
	 of Con_b (var, given_kind, con) =>
	   let
	     val origD = D
	     val given_kind = kind_valid (D,given_kind)
	     val (con,found_kind) = con_valid (D,con)
	     val bnd_kind = if !bnds_made_precise then found_kind else given_kind
	     val ((D,subst'),var,bnd_kind) = bind_at_kind (D,var,bnd_kind)
	     val subst = Subst.con_subst_compose(subst',subst)
	     val bnd = Con_b (var,bnd_kind,con)
	   in
	     if sub_kind' (origD,found_kind,given_kind) then
	       (bnd,(D,subst))
	     else
	       if (!bnds_made_precise)
		 then (print "Warning: kind mismatch in constructor binding;  probably a result of\n";
			print "  dropping all singleton information including those in negative positions\n";
	 		(bnd,(D,subst)))
	       else 
		(perr_c_k_k (con,given_kind,found_kind);
		(error ("kind mismatch in constructor binding of "^(var2string var)) handle e => raise e))
	   end
	  | Exp_b (var, con, exp) =>
	   let
	     val (given_con,kind) = con_valid (D,con)
	     val (exp,found_con) = exp_valid (D,exp)
	     val bnd_con = if !bnds_made_precise then found_con else given_con
	     val D = insert_con (D,var,bnd_con)
	     val bnd = Exp_b (var,bnd_con,exp)
	   in
	     if alpha_equiv_con (given_con,found_con) then
	       (bnd,(D,subst))
	     else
	       (perr_e_c_c (exp,given_con,found_con);
		(error ("type mismatch in expression binding of "^(var2string var)) handle e => raise e))
	   end
	  | ((Fixopen_b defs) | (Fixcode_b defs)) =>
	   let
	     val origD = D
	     val is_code = 
	       (case bnd 
		  of Fixopen_b _ => false
		   | _ => true)
	     val def_list = set2list defs
	     val (vars,functions) = unzip def_list
	     val (openness,constructor) = 
	       if is_code then 
		 (Code,Fixcode_b) 
	       else (Open,Fixopen_b)

	     val D = leave_top_level D

	     val (declared_c) = map (curry2 get_function_type openness) functions
	     val (declared_c,_) = unzip (map (curry2 con_valid D) declared_c)  (*Must normalize!!*)
	     val bnd_types = zip vars declared_c

	     val D = 
	       if is_code then 
		 insert_code_con_list (code_context D, bnd_types)
	       else insert_con_list (D,bnd_types)
		       
	     val functions = map (curry2 function_valid D) functions

	     val D = 
	       if is_code then 
		 insert_code_con_list (origD, bnd_types)
	       else insert_con_list (origD,bnd_types)
		       
	     val defs = list2set (zip vars functions)
	     val bnd = constructor defs
	   in
	     (bnd,(D,subst))
	   end
	  | Fixclosure_b (is_recur,defs) => 
	   let
	     val origD = D
(*	     val D = leave_top_level D *)
	     val (vars,closures) = unzip (set2list defs)
	     val tipes = map (fn cl => #tipe cl) closures
	     val (tipes,_) = unzip (map (curry2 con_valid D) tipes)

	     val D = if is_recur 
		       then insert_con_list (D,zip vars tipes)
		     else D
	     fun do_closure ({code,cenv,venv,tipe=_},tipe) = 
	       let
		 val (cenv,ckind) = con_valid (D,cenv)
		 val (venv,vcon) = exp_valid (D,venv)
		 val (code_type) = 
		   (case find_con (D,code)
		      of SOME k => k
		       | NONE => (printl ("Code pointer "^(var2string code));
				  print " not defined in context";
				  (error "Invalid closure" handle e => raise e)))
		 val con = 
		   (case strip_arrow code_type
		      of SOME ((Code | ExternCode),effect,tformals,formals,numfloats,body_c) => 
			let
			  val (tformals,(v,last_k)) = split tformals
			  val tformals = map (fn (tv,k) => (tv,varConKindSubst v cenv k)) tformals
			  val formals = map (varConConSubst v cenv) formals
			  val (formals,last_c) = split formals
			  val last_c = con_normalize D last_c
			  val body_c = varConConSubst v cenv body_c
			  val closure_type = AllArrow_c (Closure,effect,tformals,formals,numfloats,body_c)
			  val closure_type = con_normalize D closure_type
			in
			  if sub_kind' (D,ckind,last_k) andalso
			    alpha_equiv_con (vcon,last_c) 
			    then
			      closure_type
			  else
			    (perr_k_k (last_k,ckind);
			     perr_c_c (last_c,vcon);
			     (error "Mismatch in closure" handle e => raise e))
			end
		       | _ => (perr_e_c (Var_e code,code_type);
			       (error "Code pointer in closure of illegal type" handle e => raise e)))
	       in
		 if alpha_equiv_con (con,tipe) then
		   {code=code,cenv=cenv,venv=venv,tipe=tipe}
		 else
		   (perr_c_c (tipe,con);
		    print "code_type is "; PpNil.pp_con code_type; print "\n";
		    print "con is "; PpNil.pp_con con; print "\n";
		    (error "Type error in closure" handle e => raise e))
	       end
	     val D = insert_con_list (origD,zip vars tipes)
	     val closures = map2 do_closure (closures,tipes)
	     val defs = list2set (zip vars closures)
	     val bnd = Fixclosure_b (is_recur, defs)
	   in
	     (bnd,(D,subst))
	   end)
    end
  and exp_valid (D : context, exp : exp) : exp * con = 
      let val _ = push_exp(exp,D)
	  val _ = if (!show_calls)
		      then (print "exp_valid called with expression =\n";
			    PpNil.pp_exp exp; 
			    print "\nand context"; NilContext.print_context D;
			    print "\n\n")
		  else ()
	  val res as (e,c) = exp_valid'(D,exp)
	  val _ = pop()
      in  res
      end

  and exp_valid' (D : context,exp : exp) : (exp * con) = 
    (case exp 
       of Var_e var => 
	 (case find_con (D,var)
	    of SOME con => (exp,con)
	     | NONE => 
	      (error ("Encountered undefined variable " ^ (Name.var2string var) 
		     ^ "in exp_valid") handle e => raise e))
	| Const_e value => 
	    let
	      val (value,con) = value_valid (D,value)
	    in
	      (Const_e value,con)
	    end
	| Let_e (letsort,bnds,exp) => 
	    let
	      val (bnds,(D,subst)) = bnds_valid (D,bnds)
	      val exp = substConInExp subst exp
	      val (exp,con) = exp_valid (D,exp)
	    in
	      (Let_e (letsort,bnds,exp),con)
	    end
	| Prim_e (NilPrimOp prim,cons,exps) =>   
	    let
	      val ((prim,cons,exps),con) = prim_valid (D,prim,cons,exps)
	    in
	      (Prim_e (NilPrimOp prim,cons,exps),con)
	    end
	| Prim_e (PrimOp prim,cons,exps) =>   
	    let 
	      val (cons,kinds) = unzip (map (curry2 con_valid D) cons)
	      val (total,arg_types,return_type) = PrimUtil.get_type prim cons
	      val (return_type,_) = con_valid(D,return_type)
	      val (arg_types,_) = unzip (map (curry2 con_valid D) arg_types)
	      val (exps,exp_cons) = unzip (map (curry2 exp_valid D) exps)
	      val con = 
		if c_all2 alpha_equiv_con (o_perr_c_c "Length mismatch in prim args") (arg_types,exp_cons) then
		  return_type
		else
		  (PpNil.pp_list PpNil.pp_con' arg_types ("\nExpected arguments of types: ",",","\n",false);
		   PpNil.pp_list PpNil.pp_exp' exps ("\nFound arguments: ",",","\n",false);
		   PpNil.pp_list PpNil.pp_con' exp_cons ("\nof types: ",",","\n",false);
		   error "Illegal type for Prim op")
	      val exp = Prim_e (PrimOp prim,cons,exps)
	    in
	      (exp,con)
	    end
	| Switch_e switch =>
	    let
	      val (switch,con) = switch_valid (D,switch)
	    in
	      (Switch_e switch,con)
	    end
	| ((App_e (openness as (Code | ExternCode),app as (Var_e _),cons,texps,fexps)) |  
	   (App_e (openness as (Closure | Open),app,cons,texps,fexps))) =>
	    let
	      val (cons,kinds) = unzip (map (curry2 con_valid D) cons)
	      val cons = map2 mark_as_checked (cons,kinds)
	      val actuals_t = map (curry2 exp_valid D) texps
	      val actuals_f = map (curry2 exp_valid D) fexps
	      val (app,con) = exp_valid (D,app)
	      val (openness',_,tformals,formals,numfloats,body) = 
		(case strip_arrow con
		   of SOME c => c
		    | NONE => (perr_e_c (app,con);
			       (error "Application of non-arrow expression" handle e => raise e)))
		   
	      fun check_one_kind ((var,formal_kind),actual_kind,(D,subst)) = 
		let
		  val origD = D
		  val formal_kind = kind_normalize' (D,subst) formal_kind
		  val ((D,subst),var,actual_kind) = bind_at_kind' ((D,subst),(var,actual_kind))
		in
		  if sub_kind' (origD,actual_kind,formal_kind) then
		    (D,subst)
		  else
		    (perr_k_k (formal_kind,actual_kind);
		     (error "Constructor parameter kind mismatch" handle e => raise e))
		end
	      val (D,subst) = 
		if eq_len (tformals,kinds) then
		  foldl2 check_one_kind (D,Subst.empty()) (tformals,kinds)
		else
		  (PpNil.pp_list (fn (v,k) => PpNil.pp_kind' k) tformals 
		   ("\nFormal param kinds are: ",",","\n",false);
		   PpNil.pp_list PpNil.pp_con' cons ("\nActuals are: ",",","\n",false);
		   error "Length mismatch between formal and actual constructor parameter lists" 
		   handle e => raise e)

	      fun check_one_con (actual_con,formal_con) = 
		alpha_equiv_con (actual_con,formal_con)

	      val (t_exps,t_cons) = unzip actuals_t
	      val (f_exps,f_cons) = unzip actuals_f

	      val err = o_perr_c_c "Length mismatch in exp actuals"

	      val formals = map (con_normalize' (D,subst)) formals
	      val params_match = 
		if c_all2 check_one_con err (t_cons,formals) then
		  if c_all is_float_c (fn c => (perr_c c;false)) f_cons then
		    true
		  else
		    (error "Expected float for float parameter" handle e => raise e)
		else
		  (PpNil.pp_list PpNil.pp_con' formals ("\nFormal Types: (",", ",")\n",false);
		   PpNil.pp_list PpNil.pp_con' t_cons ("\nActual Types: (",", ",")\n",false);
		   PpNil.pp_list PpNil.pp_exp' t_exps ("\nActuals: (",", ",")\n",false);
		   perr_e exp;
		   error "Formal/actual parameter type mismatch" handle e => raise e)

	      val exp = App_e (openness,app,cons,t_exps,f_exps)

	      (*The actuals must have more information than what is given in the kinds,
	       * since their kinds are sub_kinds
	       *)
	      val subst = Subst.fromList (zip (#1 (unzip tformals)) cons)
	      val con = con_normalize' (D,subst) body
	    in
	      if same_openness (openness,openness') andalso
		((Word32.toInt numfloats) = (List.length fexps))
		then
		  (exp,con)
	      else
		(error "Error in application - different openness" handle e => raise e)
	    end
	 | App_e _ => 
	    (printl "Application expression ";
	     PpNil.pp_exp exp;
	     (error "Illegal application.  Closure with non-var?" handle e => raise e))
	| Raise_e (exp,con) => 
	    let
	      val (con,kind) = con_valid (D,con)
	      val (exp,exn_con) = exp_valid (D,exp)
	    in
	      if is_exn_con (exn_con) then
		(Raise_e (exp,con),con)
	      else
		(perr_e_c (exp,con);
		 (error "Non exception raised - Ill formed expression" handle e => raise e))
	    end
	| Handle_e (exp,function) =>
	    (case function 
	       of Function (effect,recursive,[],[(var,c)],
			    [],body,con) =>
		 let
		   val _ = if is_exn_con c then () else error "Variable has wrong type in handle"
		   val (con',kind) = con_valid (D,con)
		   val (exp',con'') = exp_valid (D,exp)
		   val (body',con''') = exp_valid (insert_con (D,var,c),body)
		   val return_con = if !bnds_made_precise then con''' else con'
		   val function' = Function (effect,recursive,[],
					     [(var,c)],[],body',return_con)
		 in
		   if alpha_equiv_con (con',con'') andalso
		     alpha_equiv_con (con'',con''') then
		     (Handle_e (exp',function'),con'')
		   else
		     (print "Declared as : \n";
		      PpNil.pp_con con';
		      print "\nExpected : \n";
		      PpNil.pp_con con'';
		      print "\nFound : \n";
		      PpNil.pp_con con''';
		      (error "Handler body has incorrect type" handle e => raise e))
		 end
	       | _ => 
		 (print "Body is :\n";
		  PpNil.pp_exp (Handle_e (exp,function));
		  (error "Illegal body for handler" handle e => raise e)))
	       )  (*esac*)

      and bnd_valid' (bnd,(D,subst)) = 
	let 
	  val (bnd',_) = substConInBnd subst bnd
	  val _ = push_bnd(bnd',D)
	  val _ = if (!show_calls)
		    then (print "bnd_valid called with bnd =\n";
			  PpNil.pp_bnd bnd';
			  print "\nand context"; NilContext.print_context D;
			  print "\n\n")
		  else ()
	  val res = (bnd_valid''(bnd,(D,subst))
		     handle e => (show_stack(); raise e))
	  val _ = pop()
	in  res
      end

      val exp_valid = wrap "exp_valid" exp_valid
      val con_valid = wrap "con_valid" con_valid
      val kind_valid = wrap "kind_valid" kind_valid
      val con_reduce = wrap "con_reduce" con_reduce
      val kind_reduce = wrap "kind_reduce" kind_reduce

      fun import_valid' (ImportValue (label,var,con),(D,subst)) =
	let
	  val con = substConInCon subst con
	  val (con,kind) = con_valid(D,con)
	  val D = insert_con(D,var,con)
	in
	  (ImportValue (label,var,con),(D,subst))
	end
	| import_valid' (ImportType (label,var,kind),(D,subst)) = 
	let
	  val ((D,subst),var,kind) = bind_at_kind' ((D,subst),(var,kind))
	in
	  (ImportType (label,var,kind),(D,subst))
	end

      fun import_valid (D,import) = import_valid' (import,(D,Subst.empty()))

      fun export_valid' ((D,subst),ExportValue (label,exp,con)) = 
	let
	  val exp = substConInExp subst exp
	  val con = substConInCon subst con
	  val (exp,found_con) = exp_valid(D,exp)
	  val (con,kind) = con_valid(D,con)
	  val bnd_con = if !bnds_made_precise then found_con else con
	in
	  if alpha_equiv_con (found_con,con) then
	    ExportValue (label,exp,bnd_con)
	  else
	    (perr_e_c_c (exp,con,found_con);
	     (error "Type error in value exports of module" handle e => raise e))
	end
	| export_valid' ((D,subst),ExportType (label,con,kind)) = 
	let
	  val con = substConInCon subst con
	  val kind = substConInKind subst kind
	  val (con,found_kind) = con_valid(D,con)
	  val kind = kind_valid(D,kind)
	  val bnd_kind = if !bnds_made_precise then found_kind else kind
	in
	  if sub_kind' (D,found_kind,kind) then
	    ExportType (label,con,bnd_kind)
	  else
	    (perr_c_k_k (con,kind,found_kind);
	     (error "Type error in type exports of module" handle e => raise e))
	end

      fun export_valid (D,export) = export_valid' ((D,Subst.empty()),export)

      fun module_valid' (D,MODULE {bnds,imports,exports}) = 
	let
	  val (imports,(D,subst)) = foldl_acc import_valid' (D,Subst.empty()) imports
	  val (bnds,(D,subst)) = bnds_valid'(bnds,(D,subst))
	  val exports = map (curry2 export_valid' (D,subst)) exports
	in
	  MODULE {bnds=bnds,imports=imports,exports=exports}
	end

      fun module_valid (D,module) = 
	let val _ = push_mod(module,D)
	  val _ = if (!show_calls)
		    then (print "module_valid called with module =\n";
			  PpNil.pp_module module;
			  print "\nand context"; NilContext.print_context D;
			  print "\n\n")
		  else ()
	  val res = module_valid'(D,module)
	  val _ = pop()
	in  res
	end

      fun con_reduce_once (D,c) = Normalize.con_reduce_once D c
      val get_shape = Normalize.get_shape
      val module_valid = wrap "module_valid" module_valid

end
