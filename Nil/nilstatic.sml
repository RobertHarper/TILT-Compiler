(*$import ANNOTATION PRIMUTIL NIL PPNIL ALPHA NILUTIL NILCONTEXT NILERROR NORMALIZE NILSUBST Stats NILSTATIC *)
functor NilStaticFn(structure Annotation : ANNOTATION
		    structure PrimUtil : PRIMUTIL
			 where type con = Nil.con
		         where type exp = Nil.exp
		    structure PpNil : PPNIL
		    structure Alpha : ALPHA
		    structure NilUtil : NILUTIL 
		    structure NilContext : NILCONTEXT
		    structure NilError : NILERROR 
		    structure Normalize : NORMALIZE
		    structure Subst : NILSUBST
			 sharing type NilUtil.alpha_context = Alpha.alpha_context
			 and type Subst.subst = NilContext.subst = Normalize.subst
			 and type Normalize.context = NilContext.context) 
    :> NILSTATIC 
	where type context = NilContext.context = 
struct	
  
  structure Annotation = Annotation

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
      | EQCON of con * con * NilContext.context 
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
    fun push_eqcon(c1,c2,context) = push(EQCON(c1,c2,context))
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
			     | show (EQCON(c1,c2,context)) =
				     (print "con_equiv called with constructor =\n";
				      PpNil.pp_con c1; print "\nand\n";
				      PpNil.pp_con c2;
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
  val find_con = NilContext.find_con
  val insert_kind = NilContext.insert_kind
  val insert_kind_list = NilContext.insert_kind_list
  val find_kind = NilContext.find_kind



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
  val generate_tuple_label = NilUtil.generate_tuple_label

  val exp_tuple = NilUtil.exp_tuple
  val con_tuple = NilUtil.con_tuple
  val convar_occurs_free = NilUtil.convar_occurs_free
  val con_free_convar = NilUtil.con_free_convar
  val same_openness = NilUtil.same_openness
  val same_effect = NilUtil.same_effect
  val primequiv = NilUtil.primequiv
  val sub_phase = NilUtil.sub_phase
  val alpha_equiv_con = NilUtil.alpha_equiv_con
  val alpha_equiv_kind = NilUtil.alpha_equiv_kind
  val alpha_sub_kind = NilUtil.alpha_sub_kind
  val alpha_normalize_con = NilUtil.alpha_normalize_con
  val alpha_normalize_kind = NilUtil.alpha_normalize_kind

  val is_var_e = NilUtil.is_var_e

  val map_annotate = NilUtil.map_annotate
  val singletonize = NilUtil.singletonize 
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
  val labels_distinct = Listops.no_dups Name.compare_label_name

  (*From PrimUtil*)
  val same_intsize = PrimUtil.same_intsize
  val same_floatsize = PrimUtil.same_floatsize

  (*From Util *)
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
      val (subst,var') = (let val _ = find_kind(D,var)
			      val var' = derived_var var
					 in  (Subst.add subst (var,Var_c var'), var')
					 end)
			  handle NilContext.Unbound => (subst,var)
      val D = insert_kind (D,var',kind)
    in ((D,subst),var',kind)
    end
 
  and bind_at_kind (D,var,kind) = bind_at_kind' ((D,Subst.empty()),(var,kind))

  and bind_at_kinds D kinds = 
    let
      fun folder ((v,k),state) = 
	let val (state,v,k) = bind_at_kind' (state,(v,k))
	in  ((v,k),state)
	end
      val (kinds,state) = foldl_acc folder (D,Subst.empty()) kinds
    in (state,kinds)
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

  and kind_valid' (D : context, kind : kind) : kind = 
    (case kind of
          Type_k => kind
	| Singleton_k con => 
	 let
	   val (con,kind) = con_valid (D,con)
	 in
	     kind
	 end
	| Record_k elts => 
	 let
	   val elt_list = Sequence.toList elts
	   val (labels,vars_and_kinds) = unzip (map (fn ((l,v),k) => (l,(v,k))) elt_list)
	   val ((D,subst),vars_and_kinds) = bind_at_kinds D vars_and_kinds
	   val entries = 
	     map2 (fn (l,(v,k)) => ((l,v),k)) (labels,vars_and_kinds)
	 in  
	   if labels_distinct labels then
	     (Record_k (Sequence.fromList entries))
	   else 
	     (perr_k kind;
	      error "Labels in record kind not distinct")
	 end
	| Arrow_k (openness, formals, return) => 
	 let val ((D,subst),_) = bind_at_kinds D formals
	 in  (Arrow_k (openness, formals,return))
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
      (case pcon of
	   (Int_c W64) => (pcon,Type_k,args,kinds)
	 | (Float_c F32) => (pcon,Type_k,args,kinds)
	 | (Float_c F64) => (pcon,Type_k,args,kinds)
	 | (Int_c W32)  => (pcon,Type_k,args,kinds)
	 | (Int_c W16)  => (pcon,Type_k,args,kinds)
	 | (Int_c W8)  => (pcon,Type_k,args,kinds)
	 | (BoxFloat_c F64)  => (pcon,Type_k,args,kinds)
	 | (BoxFloat_c F32)  => (pcon,Type_k,args,kinds)
	 | (Exn_c)  => (pcon,Type_k,args,kinds)
	 | (Array_c)  => (pcon,Type_k,args,kinds)
	 | (Vector_c)  => (pcon,Type_k,args,kinds)
	 | (Ref_c)  => (pcon,Type_k,args,kinds)
	 | (Exntag_c) => (pcon,Type_k,args,kinds)
	 | (Record_c labels) => 
	     (if labels_distinct labels then
		(if c_all (is_type D) b_perr_k kinds 
		   then (Record_c labels,Type_k,args,kinds)
		 else
		   (error "Record contains field of non-word kind" handle e => raise e))
	      else
		(error "Record contains duplicate field labels" handle e => raise e))
	 | (Sum_c {known,totalcount,tagcount}) => 
	      (if c_all (is_type D) b_perr_k kinds then
		 let
		   val valid =  
		     (case known 
			of SOME i => 
			  (Word32.<=(Word32.fromInt 0,i) andalso 
			   Word32.<(i,tagcount+Word32.fromInt(length args)))
			 | NONE => true) 
		 in
		   if valid then
		     (pcon,Type_k,args,kinds)
		   else
		     (error "Illegal index to sum constructor" handle e => raise e) 
		 end
	       else
		 (error "Sum contains non-word component" handle e => raise e))
	 | (Vararg_c _) => 
		 (if c_all (is_type D) b_perr_k kinds then
		    (pcon,Type_k,args,kinds)
		  else 
		    (error "Vararg has non-word component" handle e => raise e)))
    end


  and con_valid_letfun' (D : context, sort, is_code, var, 
			 formals, body,body_kind, rest, con) : con * kind = 
	 let
	   val origD = D
	   val ((D,subst),formals) = bind_at_kinds D formals
	   val body = substConInCon subst body
	   val body_kind = substConInKind subst body_kind
	   val body_kind = kind_valid(D,body_kind)
	   val (body,body_kind') = con_valid (D,body)
	   val return_kind = if !bnds_made_precise then body_kind' else body_kind
	   val _ = if (sub_kind (D,body_kind',body_kind)) then ()
		   else (perr_c_k_k (body,body_kind,body_kind');
			 (error "invalid return kind for constructor function" handle e => raise e))
	   val (constructor,openness) = if is_code then (Code_cb,Code) else (Open_cb,Open)
	   val lambda = (Let_c (sort,[constructor (var,formals,body,return_kind)],Var_c var))
	   val lambda = eta_confun lambda
	   val bndkind = Arrow_k(openness,formals,return_kind)
	   val lambda = mark_as_checked (lambda,bndkind)
	 in
	   if (null rest) andalso (is_var_c con) andalso 
	     eq_opt (eq_var,SOME var,strip_var con) then
	     (lambda,bndkind)
	   else
	     con_valid (origD,varConConSubst var lambda (Let_c (sort,rest,con)))
	 end

  and con_valid' (D : context, constructor : con) : con * kind = 
    (case constructor 
       of (Prim_c (pcon,args)) =>
	 let
	   val (pcon,kind,args,kinds) = pcon_valid (D,pcon,args)
	   val con = (Prim_c (pcon,args))
	 in (con,kind)
	 end
	| (Mu_c (is_recur,defs)) =>
	 let
	   val def_list = Sequence.toList defs
	     
	   val (vars,cons) = unzip def_list

	   val var_kinds = map (fn var => (var,Type_k)) vars

	   val D' = insert_kind_list (D,var_kinds)
           val D = if is_recur then D' else D

	   val (vars,_) = unzip var_kinds


	   val (cons,kinds) = unzip (map (curry2 con_valid D) cons)
	   val defs = Sequence.fromList (zip vars cons)
	   val con = Mu_c (is_recur,defs)
	   val word_kind = Type_k
	   val kind = if (length def_list = 1)
			  then word_kind
		      else Record_k(Listops.mapcount 
				    (fn (n,_) => ((generate_tuple_label(n+1), 
						   fresh_named_var "mu"), word_kind)) def_list)
	 in
	   if c_all (is_type D) b_perr_k kinds then
	     (con,kind)
	   else
	     (error "Invalid kind for recursive constructor" handle e => raise e)
	 end
	| (AllArrow_c (openness,effect,tformals,formals,numfloats,body)) =>
	 let

	   val ((D,subst),tformals) = bind_at_kinds D tformals
	   val formals = map (substConInCon subst) formals
	   val body = substConInCon subst body
	   val (body,body_kind) = con_valid (D,body)
	   val (formals,formal_kinds) = 
	     unzip (map (curry2 con_valid D) formals)
	   val con = AllArrow_c (openness,effect,tformals,formals,numfloats,body)
	   val kind = Type_k
	 in
	   (*ASSERT*)
	   if (c_all (is_type D) b_perr_k formal_kinds) andalso 
	     (is_type D body_kind) then
	     (mark_as_checked (con,kind),kind)
	   else
	     (error "Invalid arrow constructor" handle e => raise e)
	 end
	| (v as (Var_c var)) => ((v,find_kind (D,var))
		handle NilContext.Unbound =>
		      (error ("Encountered undefined variable " ^ (Name.var2string var) 
		      ^" in con_valid") handle e => raise e))
	    
	| (Let_c (sort,((cbnd as Open_cb (var,formals,body,body_kind))::rest), con)) =>
	       con_valid_letfun'(D,sort,false,var,formals,body,body_kind,rest,con)

	| (Let_c (sort,((cbnd as Code_cb (var,formals,body,body_kind))::rest), con)) =>
	       con_valid_letfun'(D,sort,true,var,formals,body,body_kind,rest,con)

        | (Let_c (sort,cbnd as (Con_cb(var,con)::rest),body)) =>
	   let
	     val (con,kind') = con_valid (D,con)
	     val con = varConConSubst var (mark_as_checked (con,kind')) (Let_c (sort,rest,body))
	   in con_valid (D,con)
	   end
	| (Let_c (sort,[],body)) => con_valid (D,body)
	| (Closure_c (code,env)) => 
	   let
	     val (env,env_kind) = con_valid (D,env)
	     val (code,code_kind) =  con_valid (D,code)
	     val (vklist,body_kind) = 
		 case code_kind of
	          Arrow_k (Code ,vklist,body_kind) => (vklist,body_kind)
		| Arrow_k (ExternCode,vklist,body_kind) =>  (vklist,body_kind)
		| _ => (error "Invalid closure: code component does not have code kind" handle e => raise e)
	     val (first,(v,klast)) = split vklist
	     val con = Closure_c (code,env)
	     val body_kind = varConKindSubst v env body_kind
	     val kind = Arrow_k(Closure,first,body_kind)
	   in
	       if sub_kind (D,env_kind,klast) then
		   (con,kind)
	       else
		   (print "Invalid kind for closure environment:";
		    print " env_kind < klast failed\n";
		    print "env_kind is "; PpNil.pp_kind env_kind; print "\n";
		    print "klast is "; PpNil.pp_kind klast; print "\n";
		    print "code_kind is "; PpNil.pp_kind code_kind; print "\n";
		    (error "Invalid kind for closure environment" handle e => raise e))
	   end
	| (Crecord_c entries) => 
	   let
	     val (labels,cons) = unzip entries
	     val distinct = labels_distinct labels
	     val (cons,kinds) = unzip (map (curry2 con_valid D) cons)
	     val k_entries = map2 (fn (l,k) => ((l,fresh_named_var "crec_static"),k)) (labels,kinds)
	     val entries = zip labels cons
	     val con = Crecord_c entries
	     val con = eta_conrecord D con
	     val kind = Record_k (Sequence.fromList k_entries)
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
	     (case (record_kind) of
		 Record_k kinds => Sequence.toList kinds
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

	   val con = case beta_conrecord(rvals,label) of
			NONE => Proj_c (rvals,label)
		      | SOME c => c
	   val con = mark_as_checked (con,kind)
	 in
	   (con,kind)
	 end
	| (App_c (cfun_orig,actuals)) => 
	 let
	   val (cfun,cfun_kind) = con_valid (D,cfun_orig)
	   val (formals,body_kind) = 
	     case (cfun_kind) of
	         (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		| _ => (print "Invalid kind for constructor application\n";
			PpNil.pp_kind cfun_kind; print "\n";
			(error "Invalid kind for constructor application" handle e => raise e))

	   val (actuals,actual_kinds) = 
	     unzip (map (curry2 con_valid D) actuals)
	   val actuals = map2 mark_as_checked (actuals,actual_kinds)
	   val (formal_vars,formal_kinds) = unzip formals

	   val body_kind = Subst.substConInKind (Subst.fromList (zip formal_vars actuals)) body_kind

	   val _ = 
	     if c_all2 (fn (k1,k2) => sub_kind (D,k1,k2))
	       (o_perr_k_k "Constructor function applied to wrong number of arguments") 
	       (actual_kinds,formal_kinds) 
	       then ()
	     else
	       (error "Constructor function failed: argument not subkind of expected kind" handle e => raise e)
		 
	   val con = case beta_confun(cfun,actuals) of
			NONE => App_c (cfun,actuals)
		      | SOME c => c
	 in  (con,body_kind)
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
	       if sub_kind (D,body_kind,given_kind) then
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
	 in
	   if sub_kind (D,def_kind,given_kind) andalso
	     is_type D arg_kind then
	     (con,given_kind)
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

  (*PRE: kind1 and kind2 not necessarily normalized *)
  and sub_kind arg = subeq_kind false arg
  and kind_equiv arg = subeq_kind true arg

  and subeq_phase true (p1,p2) = p1 = p2
    | subeq_phase false (p1,p2) = sub_phase(p1,p2)

  and subeq_kind is_eq (D,kind1,kind2) = 
    let
      val _ = push_subkind(kind1,kind2,D)
      fun sub_one ((var1,kind1),(var2,kind2),(D,subst1,subst2)) = 
	let
	  val var' = derived_var var1
	  val D' = insert_kind (D,var',kind1)
	  val subst1 = Subst.add subst1 (var1,Var_c var')
	  val subst2 = Subst.add subst2 (var2,Var_c var')
	in
	  (subeq_kind is_eq (D,kind1,kind2),(D',subst1,subst2))
	end

      fun sub_all D (vks1,vks2) = 
	foldl_all2 sub_one (D,empty_subst(),empty_subst()) (vks1,vks2)
      val res = 
      (case (kind1,kind2) of
	    (Type_k, Type_k) => true
	  | (Singleton_k _ ,Type_k) => false
	  | (Singleton_k (c1),Singleton_k (c2)) => 
				let val k = Normalize.get_shape D c1
				in  con_equiv (D,c1,c2,k)
				end
	  | (Singleton_k (c1),k2) => (* k2 must be a higher kind *)
				let val k1 = Normalize.get_shape D c1
				    fun is_higher (Record_k _) = true
				      | is_higher (Arrow_k _) = true
				      | is_higher _ = false
				in  (is_higher k1) andalso
				    subeq_kind is_eq (D,singletonize(k1,c1),k2)
				end
	  | (Arrow_k (openness1, formals1, return1), Arrow_k (openness2, formals2, return2)) => 
	   (if eq_len (formals1,formals2) then
	      let
		val (formals_ok,(D,subst2,subst1)) = sub_all D (formals2,formals1)
		(* Notice reversal of order!!!*)
		val return1 = Subst.substConInKind subst1 return1
		val return2 = Subst.substConInKind subst2 return2
	      in
		formals_ok andalso 
		same_openness (openness1,openness2) andalso 
		 subeq_kind is_eq (D,return1,return2)
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
	  | (k1 as Arrow_k _,Singleton_k c2) => let val k2 = Normalize.get_shape D c2
						   in subeq_kind is_eq (D,k1,singletonize(k2,c2))
						   end
	  | (k1 as Record_k _,Singleton_k c2) => let val k2 = Normalize.get_shape D c2
						    in subeq_kind is_eq (D,k1,singletonize(k2,c2))
						    end
	  | (Arrow_k _, _) => false
	  | (Record_k _, _) => false)
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

  and is_type D kind = sub_kind (D,kind,Type_k)

  and beta_conrecord(c,label) = 
	(case strip_crecord c of
	   SOME entries =>
	     (case (List.find (fn ((l,_)) => eq_label (l,label)) entries) of
		   SOME (l,c) => SOME c
		 | NONE => (error "Field not in record" handle e => raise e))
	    | NONE => NONE)


  and beta_confun (confun,clist) = 
    let
	  fun reduce actuals (formals,body,body_kind) = 
	       let
		 val (vars,_) = unzip formals
		 val subst = Subst.fromList (zip vars actuals)
	       in SOME(substConInCon subst body)
	       end
    in
	      (case confun of
		   Let_c (_,(([Open_cb (var,formals,body,body_kind)])), Var_c v) =>
		   if eq_var(var,v)
		       then reduce clist (formals,body,body_kind) 
		   else NONE
		 | Let_c (_,(([Code_cb (var,formals,body,body_kind)])), Var_c v) =>
		   if eq_var(var,v)
		       then reduce clist (formals,body,body_kind) 
		   else NONE
	         | Let_c (_,[Code_cb (var,formals,body,body_kind)],Closure_c(Var_c v,env)) =>
		   if eq_var(var,v)
		       then reduce (clist @ [env]) (formals,body,body_kind) 
		   else NONE
		 | Closure_c(Let_c (_,[Code_cb (var,formals,body,body_kind)],Var_c v), env) =>
		       if eq_var(var,v)
			   then reduce (clist @ [env]) (formals,body,body_kind) 
		       else NONE
		 | _ => NONE)
	end

  (* puts con into head-normal form *)
  and con_reduce (D : context, con) : con = 
	(case con of
	   Prim_c _ => con
	 | Mu_c _ => con
	 | AllArrow_c _ => con
	 | Var_c v => (case NilContext.find_kind_equation(D,con) of
			 NONE => con
		       | SOME c => con_reduce (D,c))
	 | Crecord_c lclist => con
	 | Closure_c _ => con
	 | Typecase_c _ => error "typecase_c not handled yet"
	 | Annotate_c(_,c) => con_reduce(D,c)
	 | Let_c(letsort,[],c) => con_reduce(D,c)
	 | Let_c(letsort,[Open_cb(v,_,_,_)],Var_c v') =>
		 if (Name.eq_var(v,v')) then con else con_reduce(D,Var_c v')
	 | Let_c(letsort,[Code_cb(v,_,_,_)],Var_c v') =>
		 if (Name.eq_var(v,v')) then con else con_reduce(D,Var_c v')
	 | Let_c(letsort,cbnd::rest,c) => 
		let val subst = case cbnd of
		    		  Con_cb(v,c) => Subst.fromList[(v,c)]
				| Open_cb(v,_,_,_) => Subst.fromList[(v,Let_c(letsort,[cbnd],Var_c v))]
				| Code_cb(v,_,_,_) => Subst.fromList[(v,Let_c(letsort,[cbnd],Var_c v))]
		in  con_reduce(D,Subst.substConInCon subst (Let_c(letsort,rest,c)))
		end
	 | Proj_c (c,l) => let val c = con_reduce(D,c) 
			       val con = Proj_c(c,l)
			   in  case beta_conrecord(c,l) of
				 NONE => (case NilContext.find_kind_equation(D,con) of
					    NONE => Proj_c(c,l)
					  | SOME c => con_reduce(D,c))
			       | SOME c => con_reduce(D,c)
			   end
	 | App_c (c1,c2) => let val c1 = con_reduce(D,c1) 
			        val con = App_c(c1,c2)
			   in  case beta_confun(c1,c2) of
				 NONE => (case NilContext.find_kind_equation(D,con) of
					    NONE => App_c(c1,c2)
					  | SOME c => con_reduce(D,c))
			       | SOME c => con_reduce(D,c)
			   end)



  and cons_equiv (D,[]) = true
    | cons_equiv (D,((c1,c2,k)::rest)) = con_equiv(D,c1,c2,k) andalso cons_equiv(D,rest)

  and type_equiv (D,c1,c2) = con_equiv(D,c1,c2,Type_k)

  and con_equiv (D,c1,c2,k) : bool = 
    let val _ = push_eqcon(c1,c2,D)
	(* first, put c1 and c2 into weak head-normal form *)
	val c1 = con_reduce(D,c1)
	val c2 = con_reduce(D,c2)
	(* abstract at base kinds *)
	fun base() = alpha_equiv_con(c1,c2)
	(* abstract at any kind : need to expand *)
	fun expand k = 
	  (case k of
	     Type_k => base()
	   | Singleton_k c => expand (Normalize.get_shape D c)
	   | Record_k lvk_seq => 
		let val lvk_list = Sequence.toList lvk_seq
		in  cons_equiv(D, Listops.map (fn ((l,_),k) => (Proj_c(c1,l),Proj_c(c2,l),k)) lvk_list)
		end
	   | Arrow_k (openness,vklist,k) =>
		let val ((D,subst),vklist) = bind_at_kinds D vklist
		    val vars = map #1 vklist
		    val args = map Var_c vars
		    val k = Subst.substConInKind subst k
		    val c1 = App_c(c1,args)
		    val c2 = App_c(c2,args)
		in con_equiv(D,c1,c2,k)
		end)
        val res = (case (c1,c2) of
	  (Prim_c(pcon1,clist1), Prim_c(pcon2,clist2)) => 
		NilUtil.primequiv(pcon1,pcon2) andalso 
		(length clist1 = length clist2) andalso
		cons_equiv(D,map (fn (c1,c2) => (c1,c2,Type_k)) (Listops.zip clist1 clist2))
        | (Prim_c _, _) => false
	| (Mu_c(ir1,defs1), Mu_c(ir2,defs2)) => 
		let val vc1 = Sequence.toList defs1
		    val vc2 = Sequence.toList defs2
		    val v1 = map #1 vc1
		    val v2 = map #1 vc2
		in  (ir1 = ir2) andalso (length v1 = length v2) andalso
		    let fun mapper (v1,v2) = if (eq_var(v1,v2)) 
				       then NONE else SOME(v2,Var_c v1)
			val vc =  (Listops.zip v1 v2)
			val subst = Subst.fromList (List.mapPartial mapper vc)
			val D = foldl (fn ((v,_),D) => 
			NilContext.insert_kind(D,v,Type_k)) D vc
			fun pred ((_,c1),(_,c2)) = 
				con_equiv(D,Subst.substConInCon subst c1,
					    Subst.substConInCon subst c2,
					    Type_k)
		    in  Listops.andfold pred (Listops.zip vc1 vc2)
		    end
		end

	| (Mu_c _, _) => false
	| (AllArrow_c (openness1,effect1,vklist1,clist1,nf1,c1),
	   AllArrow_c (openness2,effect2,vklist2,clist2,nf2,c2)) =>
		openness1 = openness1 andalso effect1 = effect2 
		andalso nf1 = nf2 
		andalso (length vklist1 =length vklist2)
		andalso (length clist1 =length clist2)
		andalso 
		let fun folder(((v1,k1),(v2,k2)),(D,s1,s2,match)) = 
			let val match = match andalso kind_equiv(D,k1,k2)
			    val ((D,s),v',k1) = bind_at_kind (D,v1,k1)
			    val subst1 = if (eq_var(v1,v')) then s1
					else Subst.add s1 (v1,Var_c v')
			    val subst2 = if (eq_var(v2,v')) then s2
					else Subst.add s2 (v2,Var_c v')
			in  (D,subst1,subst2,match)
			end
		    val (D,subst1,subst2,match) = foldl folder 
					(D,Subst.empty(),Subst.empty(),true) 
					(Listops.zip vklist1 vklist2)
		    val clist1 = map (Subst.substConInCon subst1) (c1::clist1)
		    val clist2 = map (Subst.substConInCon subst2) (c2::clist2)
		in  match andalso
		   	(cons_equiv(D,Listops.map2 
				(fn (c1,c2) => (c1,c2,Type_k))
				(clist1, clist2)))
		end
	| (AllArrow_c _, _) => false
	(* check field by field *)
	| (Crecord_c lclist, _) => 
		let val kinds = (case k of
				   Record_k lvkseq => map #2 (Sequence.toList lvkseq)
				 | _ => error "bad (non-record) kind to con_equiv")
		in  cons_equiv(D, Listops.map2 (fn ((l,c),k) => (c,Proj_c(c2,l),k)) (lclist,kinds))
		end
	(* check the lambda by applying both sides to some new variables *)
	| (Let_c (_,[Open_cb(v,vklist,_,k)],_),_) =>
		let val ((D,subst),vklist) = bind_at_kinds D vklist
		    val vars = map #1 vklist
		    val args = map Var_c vars
		    val k = Subst.substConInKind subst k
		    val c1 = App_c(c1,args)
		    val c2 = App_c(c2,args)
		in con_equiv(D,c1,c2,k)
		end
	| (Let_c (_,[Code_cb(v,vklist,_,k)],_),_) => 
		let val ((D,subst),vklist) = bind_at_kinds D vklist
		    val vars = map #1 vklist
		    val args = map Var_c vars
		    val k = Subst.substConInKind subst k
		    val c1 = App_c(c1,args)
		    val c2 = App_c(c2,args)
		in con_equiv(D,c1,c2,k)
		end
	| (Let_c _, _) => error "Let_c (non-lambda) not WHNF"
	(* Use the kind to guide us in expanding the constructor down to base kinds *)
	| (Proj_c _, _) => expand k
	| (App_c _, _) => expand k
	| (Var_c _, _) => expand k
	| (Annotate_c _, _) => error "Annotate_c not WHNF"
	| (Typecase_c _, _) => error "Typecase_c not handled")
	val _ = pop()
    in  res
    end 

(* Term level type checking.  *)

  fun get_function_type (openness,Function (effect,recursive,tformals,
					      formals,fformals,body,return)) = 
    let
      val num_floats = Word32.fromInt (List.length fformals)
      val con = AllArrow_c (openness,effect,tformals,#2 (unzip formals),num_floats,return)
    in
      con
    end

  fun expand_mucon argcon : (con * con * (var * con) list) option = 
      (case argcon of 
	   Mu_c (is_recur,set) =>
	       (case (Sequence.toList set) of
		    [(v,c)] => let val con_open = c
				   val binds = [(v,argcon)]
				   val con_close = Subst.substConInCon (Subst.fromList binds) con_open
			       in SOME(con_open, con_close, binds)
			       end
		  | _ => NONE)
	 | Proj_c(mu_con as Mu_c (is_recur,set), lab) => 
		    let val def_list = Sequence.toList set
			fun mapper (n,(v,c)) = 
			    let val l = generate_tuple_label(n+1)
			    in  ((v,Proj_c(mu_con,l)),(l, c))
			    end
			val temp = Listops.mapcount mapper def_list
		    in  (case Listops.assoc_eq(eq_label,lab,map #2 temp) of
			     NONE => NONE
			   | SOME con_open => 
				 let val binds = map #1 temp
				     val con_close = Subst.substConInCon (Subst.fromList binds) con_open
				 in  SOME(con_open, con_close, binds)
				 end)
		    end
	 | _ => NONE)

  fun value_valid (D,value) = 
    (case value of
          int (intsize,_) => (value,Prim_c (Int_c intsize,[]))
        | uint (intsize,_) => (value,Prim_c (Int_c intsize,[]))
	| float (floatsize,string) => (value,Prim_c (Float_c floatsize,[]))
	| array (con,arr) => 
	 let
	   val (con,kind) = con_valid (D,con)
	   fun check exp = 
	     let
	       val (exp',con') = exp_valid (D,exp)
	     in
	       if type_equiv (D,con,con') then
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
	       if type_equiv (D,con,con') then
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
    (case (prim,cons,exps) of
          (record labels,_,exps) =>
	 let
	   val fields = zip labels exps
	   fun check_one (label,exp) =
	     let val (exp,con) = exp_valid (D,exp)
	     in	 (label,exp,con)
	     end
	   val fields = map check_one fields
	   val (labels,exps,cons) = unzip3 fields
	   val exp = (record labels,[],exps)
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
		 if c_all2 (fn (c1,c2) => type_equiv(D,c1,c2))
		    (o_perr_c_c "Length mismatch in record select") 
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
	| (inject sumtype,[sumcon],exps) =>
	 let
	   val (sumcon,_) = con_valid(D,sumcon)
	   val (tagcount,totalcount,sumtype,carrier) = 
		 (case strip_sum sumcon of
	           SOME (tc,total,SOME st,c) => (tc,total,st,c)
	         | _ => (error "inject type argument does not have special sum type"))
	   val con = Prim_c (Sum_c {tagcount=tagcount,
				    totalcount=totalcount,known=NONE},[carrier]) (*Can't propogate sumtype*)
	 in
	   case exps of
	        [] => 
	       if (sumtype < tagcount) then
		 ((prim,[carrier],[]),con)
	       else
		 (perr_e (Prim_e (NilPrimOp prim,[carrier],[]));
		  (error "Illegal injection - sumtype out of range" handle e => raise e))
	      | [argexp] =>    
		 let val cons = (* carrier should be normalized already *) 
		     (case carrier of
			  Crecord_c lcons => map #2 lcons
			| c => [c])
		 in
		 if (tagcount <= sumtype) andalso 
		   ((Word32.toInt sumtype) < ((Word32.toInt tagcount) + (List.length cons))) then
		   let
		     val (argexp,argcon) = exp_valid (D,argexp)
		     val con_k = List.nth (cons,Word32.toInt (sumtype-tagcount))
		   in
		     if type_equiv (D,argcon,con_k) then 
		       ((prim,cons,[argexp]),con)
		     else
		       (perr_e_c_c (argexp,con_k,argcon);
			(error "Illegal injection - type mismatch in args" handle e => raise e))
		   end
		 else
		   (perr_e (Prim_e (NilPrimOp prim,cons,[argexp]));
		    (error "Illegal injection - field out of range" handle e => raise e))
		end
	       | _ => (perr_e (Prim_e (NilPrimOp prim,cons,exps));
		       (error "Illegal injection - too many args"))
	 end
	| (inject_record sumtype, [sumcon], argexps) => 
	 let
	   val (argexps,expcons) = 
	     unzip (map (curry2 exp_valid D) exps)
	   val (sumcon,_) = con_valid(D,sumcon)
	   val (tagcount,totalcount,sumtype,carrier) = 
		 (case strip_sum sumcon of
	           SOME (tc,total,SOME st,c) => (tc,total,st,c)
	  	 | _ => error "inject_record not decorated with special sum type")
	   val con = Prim_c (Sum_c {tagcount=tagcount,totalcount=totalcount,known=NONE},[carrier]) (*can't propogate field*)
	   val cons = (* carrier should be normalized already *) 
	       (case carrier of
		    Crecord_c lcons => map #2 lcons
		  | c => [c])
	 in
	   if (tagcount <= sumtype) andalso 
	     ((Word32.toInt sumtype) < ((Word32.toInt tagcount) + (List.length cons))) then
	     let
	       val con_k = List.nth (cons,Word32.toInt (sumtype-tagcount))
	       val (labels,cons) = 
		 case strip_record con_k
		   of SOME (ls,cs) => (ls,cs)
		    | NONE => (printl "Field is not a record ";
			       PpNil.pp_con con_k;
			       (error "Record injection on illegal field" handle e => raise e))
	     in
	       if c_all2 (fn (c1,c2) => type_equiv(D,c1,c2))
		 (o_perr_c_c "Length mismatch in record") (expcons,cons) then 
		 ((prim,cons,argexps),con)
	       else
		  (error "Illegal record injection - type mismatch in args" handle e => raise e)
	     end
	   else
	     (printl "Expression ";
	      PpNil.pp_exp (Prim_e (NilPrimOp prim,cons,exps));
	      (error "Illegal injection - field out of range" handle e => raise e))
	 end
	| (project_sum k,[argcon],[argexp]) => 
	 let
	   val (argexp,argcon') = exp_valid (D,argexp)
	   val (argcon'',argkind) = con_valid(D,argcon)
	   val (tagcount,totalcount,field,carrier) = 
		 (case strip_sum argcon' of
	           SOME (tc,total,SOME f, c) => (tc,total,f,c)
	         | _ => (error "project_sum's term argument does not have special sum type"))
	 val cons = (* carrier should be normalized already *) 
		     (case carrier of
			  Crecord_c lcons => map #2 lcons
			| c => [c])
	   val which = if (field >= tagcount) then Word32.toInt (field - tagcount)
				else error "field < tagcount for project_sum"
           val con_i = List.nth (cons,which)
	 in
           if type_equiv(D,argcon',argcon'')
		then ((prim,[argcon],[argexp]),con_i)
	   else	
		(error "project_sum's decoration type and term argument type mismatch")
	 end
	| (project_sum_record (k,l),[argcon],[argexp]) => 
	 let
	   val (argexp,argcon') = exp_valid (D,argexp)
	   val (argcon'',argkind) = con_valid(D,argcon)
	   val (tagcount,totalcount,field,carrier) = 
		 (case strip_sum argcon' of
	           SOME (tc,total,SOME f, c) => (tc,total,f,c)
	         | _ => (error "project_sum's term argument does not have special sum type"))
	   val cons = (* carrier should be normalized already *) 
	       (case carrier of
		    Crecord_c lcons => map #2 lcons
		  | c => [c])
	   val which = if (field >= tagcount) then Word32.toInt (field - tagcount)
				else error "field < tagcount for project_sum"
           val con_i = List.nth (cons,which)
	   val _ = (case con_i of
		     Prim_c(Record_c labs, _) => 
			(if (Listops.member_eq(Name.eq_label,l,labs))
			  then ()
		        else error "project_sum_record summand type reduce to a record type without named label")
		   | _ => error "project_sum_record has summand type not equal to a record type")
	 in
           if type_equiv(D,argcon',argcon'')
		then ((prim,[argcon],[argexp]),con_i)
	   else	
		(error "project_sum's decoration type and term argument type mismatch")
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
	   val argcon = (case argcon of
			     Mu_c _ => argcon
			   | Proj_c(Mu_c _, _) => argcon
			   | _ => con_reduce(D,argcon))
	   val (exp,con) = exp_valid (D,exp)
	   val (expanded_con_open, expanded_con_close, vc_binds) = 
	       (case expand_mucon argcon of
		    SOME pair => pair
		  | NONE => (print "cannot expand reduced argcon of ROLL: ";
			     PpNil.pp_con argcon; print "\n";
			     error "cannot expand reduced argcon of ROLL"))

	   fun folder ((v,c),D) = NilContext.insert_kind_equation(D,v,c,Type_k)
	   val D = foldl folder D vc_binds

	 in
	     if type_equiv (D,con,expanded_con_open) then
		 ((roll,[argcon],[exp]),argcon)
	     else
		 (perr_e_c_c (exp,expanded_con_open,con);
		  (error "Error in roll" handle e => raise e))
	 end


	| (unroll,[con],[exp]) =>
	 let
	   val (argcon,argkind) = con_valid (D,con)
	   val (exp,con) = exp_valid (D,exp)
	   val (expanded_con_open, expanded_con_close, vc_binds) = 
	       (case expand_mucon argcon of
		    SOME pair => pair
		  | NONE => (print "cannot expand reduced argcon of UNROLL: ";
			     PpNil.pp_con argcon; print "\n";
			     error "cannot expand reduced argcon of UNROLL"))

	 in
	     (if type_equiv (D,argcon,con) then
		  ((unroll,[argcon],[exp]),expanded_con_close)
	      else
		  (perr_e_c_c (exp,argcon,con);
		   (error "Error in unroll" handle e => raise e)))
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
	       if type_equiv (D,con2,con) then
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

  and switch_valid (D,switch)  = error "sorry, need to do switch_valid"
(*
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
	     if type_equiv (D,rep_con,arm_ret) then 
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
	| Sumsw_e {info,arg,arms,default} => 
	 let
	   val (arg,argcon) = exp_valid (D,arg)
	   val (ns,arm_fns) = unzip arms
	   val arm_fns = map (curry2 function_valid  D) arm_fns
	   val (sum_decl,_) = con_valid(D,info)

	   val arms = zip ns arm_fns

	   local

	     val (non_val,totalcount,_,carrier) = 
	       if type_equiv(D,sum_decl,argcon) then
		 case strip_sum argcon of
		      SOME quad => quad
		    | _ => 
		     (perr_e_c (arg,argcon);
		      (error "Branch argument not of sum type" handle e => raise e))
	       else
		 (perr_e_c (arg,argcon);
		  print "given type:\n";
	  	  PpNil.pp_con sum_decl; print "\n";
		  error "Type given for sum switch argument does not match found type")

	     val cons = (* carrier should be normalized already *) 
		 (case carrier of
		      Crecord_c lcons => map #2 lcons
		    | c => [c])

	     fun mk_sum field = 
	       Prim_c (Sum_c {tagcount=non_val,
			      totalcount=totalcount,
			      known=SOME ((Word32.fromInt field)+non_val)},cons)

	     val known_sums = map0count mk_sum (List.length cons)

	     fun check_arms (rep_con,known_sums) = 
	       let
		 fun check_loop [] = ()
		   | check_loop ((index,Function (_,_,[],args as ([] | [_]),[],_,arm_ret))::arm_rets) = 
		   if index < non_val then
		     if type_equiv (D,rep_con,arm_ret) andalso null args then
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
		       if (type_equiv (D,rep_con,arm_ret)) andalso
			 type_equiv (D,arg_con,sum_con) then 
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
	   (Sumsw_e {info=sum_decl,arg=arg,
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
		  if type_equiv (D,arg_con,exn_con) then 
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
		     if (type_equiv (D,rep_con,arm_ret)) andalso
		         type_equiv (D,arg_con,exn_con) then 
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
	       if sub_kind (D,argkind,pkind) then
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
	       if type_equiv (D,arm_ret,rep_con) andalso 
		 sub_kind (D,argkind,pkind) then
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
*)

       
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
      if type_equiv (D,body_c,return) then
	function
      else
	(perr_e_c_c (body,return,body_c);
	 (error "Return expression has wrong type" handle e => raise e))
    end
  and bnds_valid (D,bnds) = bnds_valid' (bnds,(D,Subst.empty()))
  and bnd_valid (D,bnd) = bnd_valid' (bnd,(D,Subst.empty()))
  and bnds_valid' (bnds,(D,subst)) = foldl_acc bnd_valid' (D,subst) bnds
  and fbnd_valid'' (is_code,openness,constructor,defs,(D,subst)) = 
	   let
	     val def_list = Sequence.toList defs
	     val (vars,functions) = unzip def_list
	     val (declared_c) = map (curry2 get_function_type openness) functions
	     val (declared_c,_) = unzip (map (curry2 con_valid D) declared_c)  (*Must normalize!!*)
	     val bnd_types = zip vars declared_c

	     val D = insert_con_list (D,bnd_types)

		       
	     val functions = map (curry2 function_valid D) functions
		       
	     val defs = Sequence.fromList (zip vars functions)
	     val bnd = constructor defs
	   in (bnd,(D,subst))
	   end

  and bnd_valid'' (bnd,(D,subst)) = 
    let
      val (bnd,subst) = substConInBnd subst bnd 
    in
      (case bnd of
	   Con_b (phase, cbnd) => error "bnd_valid not done"
	  | Exp_b (var, exp) =>
	   let
	     val (exp,bnd_con) = exp_valid (D,exp)
	     val D = insert_con (D,var,bnd_con)
	     val bnd = Exp_b (var,exp)
	   in
	       (bnd,(D,subst))
	   end
	  | (Fixopen_b defs) => fbnd_valid''(false,Open,Fixopen_b,defs,(D,subst))
	  | (Fixcode_b defs) => fbnd_valid''(true,Code,Fixcode_b,defs,(D,subst))
	  | Fixclosure_b (is_recur,defs) => 
	   let
	     val origD = D
(*	     val D = leave_top_level D *)
	     val (vars,closures) = unzip (Sequence.toList defs)
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
		   (find_con (D,code)
			handle NilContext.Unbound =>
				(printl ("Code pointer "^(var2string code));
				  print " not defined in context";
				  (error "Invalid closure" handle e => raise e)))
		 val (effect,tformals,formals,numfloats,body_c) = 
		   (case strip_arrow code_type of
		       SOME (Code,effect,tformals,formals,numfloats,body_c) => 
			   (effect,tformals,formals,numfloats,body_c) 
		     | SOME (ExternCode,effect,tformals,formals,numfloats,body_c) => 
			   (effect,tformals,formals,numfloats,body_c) 
		     | _ => (perr_e_c (Var_e code,code_type);
			     (error "Code pointer in closure of illegal type" handle e => raise e)))
		 val (tformals,(v,last_k)) = split tformals
		 val tformals = map (fn (tv,k) => (tv,varConKindSubst v cenv k)) tformals
		 val formals = map (varConConSubst v cenv) formals
		 val (formals,last_c) = split formals
		 val last_c = con_normalize D last_c
		 val body_c = varConConSubst v cenv body_c
		 val closure_type = AllArrow_c (Closure,effect,tformals,formals,numfloats,body_c)
		 val closure_type = con_normalize D closure_type
		 val con = if (sub_kind (D,Singleton_k(cenv),last_k) andalso
			       type_equiv (D,vcon,last_c))
			   then
			      closure_type
			   else
			       (perr_k_k (last_k,ckind);
				perr_c_c (last_c,vcon);
				(error "Mismatch in closure" handle e => raise e))
	       in
		 if type_equiv (D,con,tipe) then
		   {code=code,cenv=cenv,venv=venv,tipe=tipe}
		 else
		   (perr_c_c (tipe,con);
		    print "code_type is "; PpNil.pp_con code_type; print "\n";
		    print "con is "; PpNil.pp_con con; print "\n";
		    (error "Type error in closure" handle e => raise e))
	       end
	     val D = insert_con_list (origD,zip vars tipes)
	     val closures = map2 do_closure (closures,tipes)
	     val defs = Sequence.fromList (zip vars closures)
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
    (case exp of
          Var_e var => 
	 ((exp,find_con (D,var))
	    handle NilContext.Unbound =>
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
	      val con = con_normalize D con
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
		if c_all2 (fn (c1,c2) => type_equiv(D,c1,c2))
		 (o_perr_c_c "Length mismatch in prim args") (arg_types,exp_cons) then
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
	| (App_e (openness,app,cons,texps,fexps)) =>
	    let

		val _ = (case (openness,app) of
			     (Code,Var_e _) => ()
			   | (ExternCode,Var_e _) => ()
			   | (Code,_) => error "code applied to non-variable"
			   | (ExternCode,_) => error "extern code applied to non-variable")

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
		  if sub_kind (origD,actual_kind,formal_kind) then
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
		type_equiv (D,actual_con,formal_con)

	      val (t_exps,t_cons) = unzip actuals_t
	      val (f_exps,f_cons) = unzip actuals_f
	      val subst = Subst.fromList (zip (#1 (unzip tformals)) cons)
	      val formals = map (Subst.substConInCon subst) formals

	      val err = o_perr_c_c "Length mismatch in exp actuals"

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
	      val con = Subst.substConInCon subst body
	    in
	      if same_openness (openness,openness') andalso
		((Word32.toInt numfloats) = (List.length fexps))
		then
		  (exp,con)
	      else
		(error "Error in application - different openness" handle e => raise e)
	    end

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
	| Handle_e (exp,v,handler,con) => error "need to do handle_e"
(*
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
		   if type_equiv (D,con',con'') andalso
		     type_equiv (D,con'',con''') then
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
	         (*esac*)
*)
	    )

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
	  if type_equiv (D,found_con,con) then
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
	  if sub_kind (D,found_kind,kind) then
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
    
      type con_subst = con Subst.subst
      val empty_subst = Subst.empty() : con_subst
      fun con_subst (subst,c) = Subst.substConInCon subst c
      fun con_reduce_once context_subst c = Normalize.con_reduce_once context_subst c
      val get_shape = Normalize.get_shape
      val make_shape = Normalize.make_shape
      val module_valid = wrap "module_valid" module_valid



end
