(*$import ANNOTATION PRIMUTIL NIL PPNIL ALPHA NILUTIL NILCONTEXT NILERROR NORMALIZE NILSUBST Stats NILSTATIC *)
structure NilStatic :> NILSTATIC where type context = NilContext.context = 
struct	
  
  structure Annotation = Annotation

  open Nil 
  open Prim

  val pp_kind = Ppnil.pp_kind
  val pp_con = Ppnil.pp_con
  val pp_exp = Ppnil.pp_exp
  val pp_kind' = Ppnil.pp_kind'
  val pp_con' = Ppnil.pp_con'
  val pp_exp' = Ppnil.pp_exp'

  val local_debug = Stats.ff "nilstatic_debug"
  val debug = Stats.ff "nil_debug"
  val show_calls = Stats.ff "nil_show_calls"
  val show_context = Stats.ff "nil_show_context"
  val warn_depth = ref 500

  val locate = NilError.locate "NilStatic"
  val assert = NilError.assert

  fun error s s' = Util.error s s'

  fun error' s = error "" s

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
			then error (locate "push") "stack depth exceeded"
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
				      pp_exp e;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (CON(c,context)) =
				     (print "con_valid called with constructor =\n";
				      pp_con c;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (EQCON(c1,c2,context)) =
				     (print "con_equiv called with constructor =\n";
				      pp_con c1; print "\nand\n";
				      pp_con c2;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (KIND(k,context)) =
				     (print "kind_valid called with kind =\n";
				      pp_kind k;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (SUBKIND(k1,k2,context)) =
				     (print "sub_kind called with kind1 =\n";
				      pp_kind k1;
				      print "\n                 and kind2 =\n";
				      pp_kind k2;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (BND(b,context)) =
				     (print "bnd_valid called with bound =\n";
				      Ppnil.pp_bnd b;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (MODULE(m,context)) =
				     (print "module_valid called with module =\n";
				      Ppnil.pp_module m;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
		       in  app show (rev st)
		       end
    fun wrap str f arg = 
      let val debug' = !debug;
      in 
	(debug := !local_debug;(f arg) before (debug := debug'))
	handle e => (debug := debug';print "Error while calling "; print str; print "\n"; show_stack(); raise e)
      end
  end

  (* Local rebindings from imported structures *)

  (*From Normalize*)
  val con_normalize = Normalize.con_normalize
  val kind_normalize = Normalize.kind_normalize
  val exp_normalize = Normalize.exp_normalize
  val con_normalize' = Normalize.con_normalize'
  val kind_normalize' = Normalize.kind_normalize'
  val exp_normalize' = Normalize.exp_normalize'

  val reduce_hnf = Normalize.reduce_hnf

  val expandMuType = Normalize.expandMuType
  val projectRecordType = Normalize.projectRecordType
  val projectSumType = Normalize.projectSumType
  val type_of = Normalize.type_of
  val strip_singleton = Normalize.strip_singleton
  val push_singleton = Normalize.push_singleton

  (*From NilContext*)
  type context = NilContext.context
  val empty = NilContext.empty
  val insert_con = NilContext.insert_con
  val insert_con_list = NilContext.insert_con_list
  val find_con = NilContext.find_con
  val insert_kind = NilContext.insert_kind
  val insert_kind_list = NilContext.insert_kind_list
  val find_kind = NilContext.find_kind
  val shape_of = NilContext.shape_of
  val make_shape = NilContext.make_shape
  val isRenamedExp = NilContext.isRenamedExp
  val isRenamedCon = NilContext.isRenamedCon
  val isRenamedKind = NilContext.isRenamedKind

  (*From Alpha*)
  type alpha_context = Alpha.alpha_context


  structure Subst = NilSubst
  type 'a subst = 'a Subst.subst
  val substConInExp = Subst.substConInExp
  val substConInCon = Subst.substConInCon
  val substConInKind = Subst.substConInKind
  val substConInBnd = Subst.substConInBnd
  val substConInCBnd = Subst.substConInCBnd
  val substExpInExp = Subst.substExpInExp
  val substExpInCon = Subst.substExpInCon
  val varConConSubst = Subst.varConConSubst
  val varConKindSubst = Subst.varConKindSubst
  val varExpConSubst = Subst.varExpConSubst
  val empty_subst = Subst.empty
  val con_subst_compose = Subst.con_subst_compose
  val generate_tuple_label = NilUtil.generate_tuple_label
  val renameCon = Subst.renameCon
  val renameKind = Subst.renameKind
  val renameMod = Subst.renameMod
  val isRenamedMod = Subst.isRenamedMod

  (*From NilUtil*)
  val function_type = NilUtil.function_type
  val exp_tuple = NilUtil.exp_tuple
  val con_tuple = NilUtil.con_tuple
  val convar_occurs_free = NilUtil.convar_occurs_free
  val con_free_convar = NilUtil.con_free_convar
  val same_openness = NilUtil.same_openness
  val same_effect = NilUtil.same_effect
  val primequiv = NilUtil.primequiv
  val sub_phase = NilUtil.sub_phase
  val project_from_kind = NilUtil.project_from_kind
(*  val alpha_equiv_con = NilUtil.alpha_equiv_con
  val alpha_equiv_kind = NilUtil.alpha_equiv_kind
  val alpha_sub_kind = NilUtil.alpha_sub_kind
  val alpha_normalize_con = NilUtil.alpha_normalize_con
  val alpha_normalize_kind = NilUtil.alpha_normalize_kind
*)
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
  val strip_externarrow = NilUtil.strip_externarrow
  val strip_record = NilUtil.strip_record
  val strip_crecord = NilUtil.strip_crecord
  val strip_proj = NilUtil.strip_proj
  val strip_prim = NilUtil.strip_prim
  val strip_app = NilUtil.strip_app
  val is_exn_con = NilUtil.is_exn_con
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
  val foldl3 = Listops.foldl3
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
  val same_intsize = NilPrimUtil.same_intsize
  val same_floatsize = NilPrimUtil.same_floatsize

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


(*  fun expand_mucon argcon : (con * con * (var * con) list) option = 
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
	| loop _ = error (locate "foldl_all2") "Passed lists of unequal length"
    in
      loop (l1,l2,init)
    end
    


  fun assertRenamed (isRenamed,printer,name) (context,item) = 
    (isRenamed context item, 
     fn () => (lprintl (name^" not properly renamed when passed to function");
	       printl (name^" is:");
	       printer item;
	       lprintl "Context is:";
	       NilContext.print_context context;
	       printl ""))
    
  val assertRenamedKind = assertRenamed (isRenamedKind,pp_kind,"Kind")
  val assertRenamedCon = assertRenamed (isRenamedCon,pp_con,"Con")
  val assertRenamedExp = assertRenamed (isRenamedExp,pp_exp,"Exp")

(*  val find_con = 
    (fn (D,v)=> 
     let 
       val _ = lprintl ("Looking up "^(var2string v))
       val c = find_con (D,v)
       val c' = renameCon c
     in 
       (lprintl "Find con got";
	pp_con c;
	lprintl "Renamed to"; 
	pp_con c';
	lprintl"";
	c')
     end)*)
  val find_con = renameCon o find_con
  val find_kind = renameKind o find_kind

  fun con_head_normalize (D,con) = 
    let
      val (is_hnf,con') = reduce_hnf(D,con)
    in
      con'
    end

  fun is_type D kind = 
    (case make_shape (D,kind)
       of Type_k => true
	| _ => false)

  fun assertWellFormed context = 
    let
      val debug' = !debug
      val _ = debug := false
      val res = 
	((NilContext.is_well_formed (kind_valid,con_valid,sub_kind) context, fn () => ())
	 handle any => (debug := debug';raise any))
      val _ = debug := debug'
    in
      res
    end
  and kind_valid (D,kind) = 
    let 
      val _ = push_kind(kind,D)
      val _ = if (!show_calls)
		then (print "kind_valid called with kind =\n";
		      pp_kind kind;
		      if !show_context then (print "\nand context"; NilContext.print_context D) else ();
		      print "\n\n")
	      else ()

      val _ = 
	if !debug then
	  assert (locate "kind_valid - PRE")
	  [
	   assertWellFormed D,
	   assertRenamedKind (D,kind)
	   ]
	else ()

      val _ = kind_valid'(D,kind)
      val _ = if (!show_calls)
		then (printl "kind_valid returned")
	      else ()

      val _ = pop()
    in  ()
    end

  and kind_valid' (D : context, kind : kind) : unit = 
    (case kind of
       Type_k => ()
     | Singleton_k con => 
	 let
	   val kind = con_valid (D,con)
	 in
	   ()
	 end
     | Record_k elts => 
	 let
	   fun folder (((l,v),k),D) = 
	     (kind_valid (D,k);
	      insert_kind (D,v,k))
	   val _ = Sequence.foldl folder D elts
	   fun compare (((l1,_),_),((l2,_),_)) = Name.compare_label_name (l1,l2)
	   val _ = 
	     (Sequence.no_dups compare elts) orelse
	     (perr_k kind;
	      error (locate "kind_valid") "Labels in record kind not distinct")
	 in ()
	 end
     | Arrow_k (openness, formals, return) => 
	 let 
	   fun folder ((v,k),D) = 
	     (kind_valid (D,k);
	      insert_kind (D,v,k))
	   val D = foldl folder D formals
	 in  
	   kind_valid (D,return)
	 end)

  and con_valid (D : context, constructor : con) : kind = 
    let 
      val _ = push_con(constructor,D)
      val _ = if (!show_calls)
		then (print "con_valid called with constructor =\n";
		      pp_con constructor; 
		      if !show_context then (print "\nand context"; NilContext.print_context D) else ();
		      print "\n\n")
	      else ()

      val _ = 
	if !debug then
	  assert (locate "con_valid PRE")
	  [
	   assertWellFormed D,
	   assertRenamedCon (D,constructor)
	   ]
	else ()
      val k = con_valid'(D,constructor)
      val _ = if (!show_calls)
		then (printl "con_valid returned")
	      else ()
      val _ = 
	if !debug then
	  assert (locate "con_valid POST")
	  [
	   assertRenamedKind (D,k)
	   ]
	else ()
      val _ = pop()
    in  k
    end


  and pcon_valid (D,pcon,args) = 
    let      
      val kinds = map (curry2 con_valid D) args
      val _ = 
	(case pcon of
	   (Record_c (labels,vars_opt)) => 
	     let
	       val _ = 
		 (if labels_distinct labels then
		    (c_all (is_type D) b_perr_k kinds) orelse
		    (error (locate "pcon_valid") "Record contains field of non-word kind")
		  else
		    (error (locate "pcon_valid") "Record contains duplicate field labels"))
	     in () 
	     end
	 | (Sum_c {known,totalcount,tagcount}) => 
	     let
	       val kinds = 
		 (case kinds
		    of [rkind] =>
		      (case strip_singleton(D,rkind)
			 of Record_k entries => Sequence.maptolist (fn (_,k) => k) entries
			  | other => [other])
		     | _ => error (locate "pcon_valid") "Wrong number of args to Sum_c")

	       val _ = 
		 (
		  ((length kinds) = (Word32.toInt (totalcount - tagcount))) orelse
		  (error (locate "pcon_valid") "Sum_c counts disagree with args");
		  
		  (List.all (is_type D) kinds) orelse
		  (error (locate "pcon_valid") "Sum contains non-word component" );

		  (case known 
		     of SOME i => 
		       (Word32.<=(Word32.fromInt 0,i) andalso 
			Word32.<(i,totalcount)) orelse
		       (perr_c (Prim_c (pcon,args));
			error (locate "pcon_valid") "Illegal index to sum constructor")
		      | NONE => true)
		     )

	     in ()
	     end
	 | (Vararg_c _) => 
	     let
	       val _ = 
		 (c_all (is_type D) b_perr_k kinds) orelse
		 (error (locate "pcon_valid") "Vararg has non-word component" )
	     in ()
	     end
	 | _ => ())
    in ()
    end
  
  and con_valid' (D : context, constructor : con) : kind = 
    let

      val _ = 
	(case constructor 
	   of (Prim_c (pcon,args)) => pcon_valid (D,pcon,args)
	    | (Mu_c (is_recur,defs)) =>
	     let
	       val word_kind = Type_k

	       val D =
		 if is_recur then
		   if Sequence.length defs = 1
		     then Sequence.foldl (fn ((v,c),D) => insert_kind(D,v,Type_k (*Singleton_k(constructor)*))) D defs
		   else 
		     let 
		       fun insert' ((v,k),D) = insert_kind (D,v,k)
		       fun mapper (i,(v,c)) = (v,Type_k)(*Singleton_k(Proj_c(constructor,generate_tuple_label(i+1))))*)
		       val vkinds = Sequence.mapcount mapper defs
		     in  Sequence.foldl insert' D vkinds
		     end
		 else D
		 
	       fun folder ((v,c),kinds) = 
		 let
		   val k = con_valid (D,c)
		 in k::kinds
		 end
	       
	       val kinds = Sequence.foldl folder [] defs

	       val _ = 
		 (c_all (is_type D) b_perr_k kinds) orelse
		 (error (locate "con_valid") "Invalid kind for recursive constructor" )
	     in
	       ()
	     end
	    | (AllArrow_c (openness,effect,tformals,vars_opt,formals,numfloats,body)) =>
	     let
	       fun folder ((v,k),D) = 
		 (kind_valid (D,k);
		  insert_kind (D,v,k))
	       val D = foldl folder D tformals
	       val (formal_kinds,D) = 
		 (case vars_opt
		    of SOME vars => 
		      let 
			fun folder (v,c,(kinds,D)) = ((con_valid (D,c))::kinds,insert_con (D,v,c))
		      in
			foldl2 folder ([],D) (vars,formals)
		      end
		     | NONE => (map (curry2 con_valid D) formals,D))
	       val body_kind = con_valid (D,body)
	       val _ = 
		 ((c_all (is_type D) b_perr_k formal_kinds) andalso (is_type D body_kind)) orelse
		 (error (locate "con_valid") "Invalid arrow constructor")
	     in
	       ()
	     end
	    | ExternArrow_c (args,body) => 
	     let
	       val _ = map (curry2 con_valid D) args
	       val _ = con_valid (D,body)
	     in
	       ()
	     end
	    | (v as (Var_c var)) => 
	     let
	       val _ = 
		 (find_kind (D,var)
		  handle NilContext.Unbound =>
		    (error (locate "con_valid")  ("Encountered undefined variable " ^ (Name.var2string var))))
	     in ()
	     end	       
	    | (Let_c (sort,cbnds,con)) =>
	     let  
	       val D = foldl cbnd_valid D cbnds
	       val _ = con_valid(D,con)
	     in ()
	     end
	    | (Typeof_c exp) => (ignore (exp_valid (D,exp)))
	    | (Closure_c (code,env)) => 
	     let
	       val env_kind = con_valid (D,env)
	       val code_kind =  con_valid (D,code)
	       val (vklist,body_kind) = 
		 case strip_singleton (D,code_kind) of
		   Arrow_k (Code ,vklist,body_kind) => (vklist,body_kind)
		 | Arrow_k (ExternCode,vklist,body_kind) =>  (vklist,body_kind)
		 | _ => (error (locate "con_valid") "Invalid closure: code component does not have code kind")
	       val (first,(v,klast)) = split vklist
(*	       val body_kind = varConKindSubst v env body_kind
	       val kind = Arrow_k(Closure,first,body_kind)*)
	       val _ = 
		 (sub_kind (D,env_kind,klast)) orelse
		 (print "Invalid kind for closure environment:";
		  print " env_kind < klast failed\n";
		  print "env_kind is "; pp_kind env_kind; print "\n";
		  print "klast is "; pp_kind klast; print "\n";
		  print "code_kind is "; pp_kind code_kind; print "\n";
		  (error (locate "con_valid") "Invalid kind for closure environment" ))
	     in
	       ()
	     end
	    | (Crecord_c entries) => 
	     let
	       val (labels,cons) = unzip entries
	       val kinds = map (curry2 con_valid D) cons
(*	       val k_entries = map2 (fn (l,k) => ((l,fresh_named_var "crec_static"),k)) (labels,kinds)
	       val kind = Record_k (Sequence.fromList k_entries)*)
	       val _ =  
		 (labels_distinct labels) orelse
		 (Ppnil.pp_list Ppnil.pp_label' labels 
		  ("labels are: ",",",";",true);
		  (error (locate "con_valid") "Labels in record of constructors not distinct" ))
	     in 
	       ()
	     end
	    | (Proj_c (rvals,label)) => 
	     let
	       val record_kind = con_valid (D,rvals)
		 
	       val entry_kinds = 
		 (case (strip_singleton (D,record_kind)) of
		    Record_k kinds => kinds
		  | other => 
		      (perr_c_k (constructor,record_kind);
		       lprintl "and context is";
		       NilContext.print_context D;
		       (error (locate "con_valid") 
			"Non-record kind returned from con_valid in projection")))
		
	     in
(*	       project_from_kind (entry_kinds,rvals,label)*)
	       ()
	     end
	    | (App_c (cfun_orig,actuals)) => 
	     let
	       val cfun_kind = con_valid (D,cfun_orig)
	       val (formals,body_kind) = 
		 case (strip_singleton (D,cfun_kind)) of
		   (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		 | _ => (print "Invalid kind for constructor application\n";
			 pp_kind cfun_kind; print "\n";
			 (error (locate "con_valid") "Invalid kind for constructor application" ))
		     
	       val actual_kinds = map (curry2 con_valid D) actuals
	       val (formal_vars,formal_kinds) = unzip formals
	       val _ = app (curry2 kind_valid D) formal_kinds
(*	       val body_kind = Subst.substConInKind (Subst.fromList (zip formal_vars actuals)) body_kind
	*)	 
	       val _ = 
		 (c_all2 (fn (k1,k2) => sub_kind (D,k1,k2))
		  (o_perr_k_k "Constructor function applied to wrong number of arguments") 
		  (actual_kinds,formal_kinds))
		 orelse
		 (error (locate "con_valid") "Constructor function failed: argument not subkind of expected kind")
		 
	     in  ()
	     end
	    | (Typecase_c {arg,arms,default,kind=given_kind}) => 
	     let
	       val _ = kind_valid (D,given_kind)
	       val origD = D
	       fun doarm (pcon,args,body) = 
		 let
		   fun folder ((v,k),D) =
		     let
		       val _ = kind_valid (origD,k)
		     in
		       (Var_c v,insert_kind(D,v,k))
		     end
		   val (argcons,D) = foldl_acc folder D args
		   val pkind = pcon_valid (D,pcon,argcons)
		   val body_kind = con_valid(D,body)
		   val _ = 
		     (sub_kind (D,body_kind,given_kind)) orelse
		     (perr_k_k (given_kind,body_kind);
		      (error (locate "con_valid") "Illegal kind in typecase"))
		 in ()
		 end
	       val _ = app doarm arms
	       val arg_kind = con_valid (D,arg)
	       val def_kind = con_valid (D,default)
	       val _ = 
		 ((sub_kind (D,def_kind,given_kind)) andalso (is_type D arg_kind)) orelse
		 (error (locate "con_Valid") "Error in type case" handle e => raise e)
	     in
	       ()
	     end
	    | (Annotate_c (annot,con)) => 
	     let
	       val kind = con_valid (D,con)
	     in
	       ()
	     end)
      val kind = Singleton_k(constructor)
    in
      kind
    end
  and cbnd_valid (cbnd,D) =
    let

      fun con_valid_letfun' (D : context, var, formals,body,body_kind) : unit = 
	let
	  fun folder ((v,k),D) = (kind_valid (D,k);insert_kind(D,v,k))
	  val D = foldl folder D formals
	  val _ = kind_valid(D,body_kind)
	  val found_body_kind = con_valid (D,body)
	  val _ = 
	    (sub_kind (D,found_body_kind,body_kind)) orelse
	    (perr_c_k_k (body,body_kind,found_body_kind);
	     (error (locate "con_valid_letfun'") "invalid return kind for constructor function" handle e => raise e))
	in ()
	end
      
      fun bnd_checker maker (var,formals,body,body_kind) = 
	let
	  val _ = con_valid_letfun'(D,var,formals,body,body_kind)
	  val var' = derived_var var
	  val bnd = maker (var',formals,body,body_kind)
	  val con = Let_c (Sequential,[bnd],Var_c var')
	  val D = insert_kind(D,var,Singleton_k con)
	in
	  D
	end
      
    in
      (case cbnd
	 of Open_cb args => bnd_checker Open_cb args
	  | Code_cb args => bnd_checker Code_cb args
	  | Con_cb(var,con) =>
	   let
	     val kind = con_valid (D,con)
	     val D = insert_kind(D,var,kind)
	   in D
	   end)
    end
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
	  val kind1 = substConInKind subst1 kind1
	  val kind2 = substConInKind subst2 kind2
	  val D' = insert_kind (D,var',kind1)
	  val subst1 = Subst.add subst1 (var1,Var_c var')
	  val subst2 = Subst.add subst2 (var2,Var_c var')
	in
	  (subeq_kind is_eq (D,kind1,kind2),(D',subst1,subst2))
	end

      fun sub_all D (vks1,vks2) = 
	foldl_all2 sub_one (D,empty_subst(),empty_subst()) (vks1,vks2)

      val res = 
      (case (kind1,kind2) 
	 of (Type_k, Type_k) => true
	  | (Type_k, Singleton_k _) => false
	  | (Type_k,_) => false
	  | (Singleton_k _ ,Type_k) => is_type D kind1
	  | (Singleton_k (c1),Singleton_k (c2)) => 
	   subeq_kind is_eq (D,push_singleton (D,c1),push_singleton (D,c2))
	  | (Singleton_k (c1),k2) => (* k2 must be a higher kind *)
	   subeq_kind is_eq (D,push_singleton (D,c1),k2)
	  | (k1,Singleton_k (c2)) => (* k1 must be a higher kind *)
	   subeq_kind is_eq (D,k1,push_singleton (D,c2))
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
	     val elts1 = Sequence.toList elts1
	     val elts2 = Sequence.toList elts2
	     val split_lbls = unzip o (map (fn ((l,v),k) => (l,(v,k))))
	     val (labels1,vks1) = split_lbls elts1
	     val (labels2,vks2) = split_lbls elts2
	   in
	     eq_len (elts1,elts2) andalso 
	     (#1 (sub_all D (vks1,vks2))) andalso
	     all2 eq_label (labels1,labels2)
	   end
	  | (Arrow_k _, _) => false
	  | (Record_k _, _) => false)
	 orelse
	 (if !debug then
	    (lprintl "sub_kind failed!";
	     printl "Kind:";
	     pp_kind kind1;
	     lprintl "Not equivalent to :";
	     pp_kind kind2;
	     printl "")
	  else ();
	    false)
      val _ = pop()
    in res
    end

  and cons_equiv (D,[]) = true
    | cons_equiv (D,((c1,c2,k)::rest)) = con_equiv(D,c1,c2,k) andalso cons_equiv(D,rest)

  and type_equiv (D,c1,c2) = con_equiv(D,c1,c2,Type_k)

  and con_equiv (D,c1,c2,k) : bool = 
    ((if !warn_depth = 500
	then (printl "Warning - con_equiv unimplemented - defaulting to true!";
	      warn_depth := 0)
      else warn_depth := !warn_depth +1);
     true)
(*    let val _ = push_eqcon(c1,c2,D)
	(* first, put c1 and c2 into weak head-normal form *)
	val c1 = con_reduce(D,c1)
	val c2 = con_reduce(D,c2)
	(* abstract at base kinds *)
	fun base() = alpha_equiv_con(c1,c2)
	(* abstract at any kind : need to expand *)
	fun expand k = 
	  (case k of
	     Type_k => base()
	   | Singleton_k c => expand (shape_of (D,c))
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
*)
(* Term level type checking.  *)


  and value_valid (D,value) = 
    (case value of
       int (intsize,_) => Prim_c (Int_c intsize,[])
     | uint (intsize,_) => Prim_c (Int_c intsize,[])
     | float (floatsize,string) => Prim_c (Float_c floatsize,[])
     | array (con,arr) => 
	 let
	   val kind = con_valid (D,con)
	   fun check exp = 
	     let
	       val con' = exp_valid (D,exp)
	       val _ = 
		 (type_equiv (D,con,con')) orelse
		 (error (locate "exp_valid") "Array contains expression of incorrect type")
	     in ()
	     end
	 in
	   (Array.app check arr;
	    Prim_c (Array_c,[con]))
	 end
	| vector (con,vec) => 
	 let
	   val kind = con_valid (D,con)
	   fun check exp = 
	     let
	       val con' = exp_valid (D,exp)
	       val _ = 
		 (type_equiv (D,con,con')) orelse
		 (error (locate "exp_valid") "Vector contains expression of incorrect type" handle e => raise e)
	     in()
	     end
	 in
	   (Array.app check vec;
	    Prim_c (Vector_c,[con]))
	 end
	| refcell expref =>
	 let
	   val con = exp_valid (D,!expref)
	 in
	   Prim_c (Ref_c,[con])
	 end
	| tag (atag,con) => 
	 let
	   val kind = con_valid (D,con)
	 in
	   Prim_c (Exntag_c,[con])
	 end)
  and prim_valid (D,prim,cons,exps) = 
    (case (prim,cons,exps) of
       (record labels,_,exps) =>
	 let
	   fun check_one exp = exp_valid (D,exp)
	   val cons = map check_one exps
	   val con = Prim_c (Record_c (labels,NONE),cons)
	   val kind = con_valid (D,con)
	   val _ = 
	     (labels_distinct labels)
	     orelse
	     (error (locate "prim_valid") "Fields not distinct" )
	 in
	   con
	 end
     | (select label,[],[exp]) =>
	 let
	   val con = exp_valid (D,exp)
	   val con = projectRecordType (D,con,label)
	 in
	   con
	 end
     | (inject sumtype,[sumcon],exps) =>
	 let
	   val _ = con_valid(D,sumcon)

	   val (tagcount,totalcount,carrier) = 
	     (case strip_sum (con_head_normalize(D,sumcon)) of
		SOME (tc,total,NONE,c) => (tc,total,c)
	      | SOME _ => (pp_con sumcon;
			   error (locate "prim_valid") "inject type argument has special sum type")
	      | NONE => (pp_con sumcon;
			 error (locate "prim_valid") "inject given invalid type argument"))

	   val _ = 
	     (case exps of
	        [] => 
		  (sumtype < tagcount) orelse
		  (perr_e (Prim_e (NilPrimOp prim,[carrier],[]));
		   (error (locate "prim_valid") "Illegal injection - sumtype out of range" ))
	      | [argexp] =>    
		  let 
		    val cons = 
		      (case con_head_normalize (D,carrier) of
			 Crecord_c lcons => map #2 lcons
		       | c => [c])
		  in
		    if (tagcount <= sumtype) andalso 
		      ((Word32.toInt sumtype) < ((Word32.toInt tagcount) + (List.length cons))) then
		      let
			val argcon = exp_valid (D,argexp)
			val con_k = List.nth (cons,Word32.toInt (sumtype-tagcount))
		      in
			(type_equiv (D,argcon,con_k)) orelse
			(perr_e_c_c (argexp,con_k,argcon);
			 (error (locate "prim_valid") "Illegal injection - type mismatch in args" ))
		      end
		    else
		      (perr_e (Prim_e (NilPrimOp prim,cons,[argexp]));
		       (error (locate "prim_valid") "Illegal injection - field out of range"))
		  end
	      | _ => (perr_e (Prim_e (NilPrimOp prim,cons,exps));
		      (error (locate "prim_valid") "Illegal injection - too many args")))
	 in
	   sumcon
	 end
	| (inject_nonrecord sumtype,[sumcon],argexps) =>
	 let

	   val _ = con_valid(D,sumcon)

	   val (tagcount,totalcount,carrier) = 
	     (case strip_sum (con_head_normalize(D,sumcon)) of
		SOME (tc,total,NONE,c) => (tc,total,c)
	      | SOME _ => (pp_con sumcon;
			   error (locate "prim_valid") "inject_nonrecord type argument has special sum type")
	      | NONE => (pp_con sumcon;
			 error (locate "prim_valid") "inject_nonrecord given invalid type argument"))

	   val _ = 
	     (case exps of
	        [] => 
		  (sumtype < tagcount) orelse
		  (perr_e (Prim_e (NilPrimOp prim,[carrier],[]));
		   (error (locate "prim_valid") "Illegal injection - sumtype out of range" ))
	      | [argexp] =>    
		  let 
		    val cons = 
		      (case con_head_normalize (D,carrier) of
			 Crecord_c lcons => map #2 lcons
		       | c => [c])
		  in
		    if (tagcount <= sumtype) andalso 
		      ((Word32.toInt sumtype) < ((Word32.toInt tagcount) + (List.length cons))) then
		      let
			val argcon = exp_valid (D,argexp)
			val con_k = List.nth (cons,Word32.toInt (sumtype-tagcount))
			val _ = 
			  (case strip_record (con_head_normalize(D,con_k))
			     of SOME _ => error (locate "prim_valid") "inject_nonrecord injects into record field"
			      | NONE => ())
		      in
			(type_equiv (D,argcon,con_k)) orelse
			(perr_e_c_c (argexp,con_k,argcon);
			 (error (locate "prim_valid") "Illegal injection - type mismatch in args" ))
		      end
		    else
		      (perr_e (Prim_e (NilPrimOp prim,cons,[argexp]));
		       (error (locate "prim_valid") "Illegal injection - field out of range"))
		  end
	      | _ => (perr_e (Prim_e (NilPrimOp prim,cons,exps));
		      (error (locate "prim_valid") "Illegal injection - too many args")))
	 in
	   sumcon
	 end

	| (inject_record sumtype, [sumcon], exps) => 
	 let

	   val _ = con_valid(D,sumcon)

	   val (tagcount,totalcount,carrier) = 
	     (case strip_sum (con_head_normalize (D,sumcon)) of
		SOME (tc,total,NONE,c) => (tc,total,c)
	      | SOME _ => (pp_con sumcon;
			   error (locate "prim_valid") "inject_record type argument has special sum type")
	      | NONE => (pp_con sumcon;
			 error (locate "prim_valid") "inject_record given invalid type argument"))

	   val expcons = map (curry2 exp_valid D) exps

	   val cons = 
	     (case con_head_normalize (D,carrier) of
		Crecord_c lcons => map #2 lcons
	      | c => [c])

	   val _ = 
	     if (tagcount <= sumtype) andalso 
	       ((Word32.toInt sumtype) < ((Word32.toInt tagcount) + (List.length cons))) then
	       let
		 val con_k = List.nth (cons,Word32.toInt (sumtype-tagcount))
		 val (labels,vars_opt,cons) = 
		   (case strip_record (con_head_normalize (D,con_k))
		      of SOME value => value
		       | NONE => (printl "Field is not a record ";
				  pp_con con_k;
				  (error (locate "prim_valid") "Record injection on illegal field" handle e => raise e)))
		 val D = 
		   (case vars_opt
		      of SOME vars => foldl2 (fn (v,c,D) => insert_con(D,v,c)) D (vars,cons)
		       | NONE => D)
	     in
	       (c_all2 (fn (c1,c2) => type_equiv(D,c1,c2)) (o_perr_c_c "Length mismatch in record") (expcons,cons)) orelse
	       (error (locate "prim_valid") "Illegal record injection - type mismatch in args" handle e => raise e)
	       end
	     else
	       (printl "Expression ";
		pp_exp (Prim_e (NilPrimOp prim,cons,exps));
		(error (locate "prim_valid") "Illegal injection - field out of range" handle e => raise e))
	       
	   val con = Prim_c (Sum_c {tagcount=tagcount,totalcount=totalcount,known=NONE},[carrier]) (*can't propogate field*)
	 in
	   con
	 end
	| (project_sum k,[argcon],[argexp]) => 
	 let
	   val argcon' = exp_valid (D,argexp)
	   val _ = 
	     (type_equiv(D,argcon,argcon')) orelse
	     (error (locate "prim_valid") "Type mismatch in project_sum")
	   val argkind = con_valid(D,argcon)
	   val con_k = projectSumType(D,argcon,k)
	 in
	   con_k
	 end
	| (project_sum_nonrecord k,[argcon],[argexp]) => 
	 let
	   val argcon' = exp_valid (D,argexp)
	   val _ = 
	     (type_equiv(D,argcon,argcon')) orelse
	     (error (locate "prim_valid") "Type mismatch in project_sum_nonrecord")
	   val argkind = con_valid(D,argcon)
	   val con_k = projectSumType(D,argcon,k)
	   val _ = 
	     (case strip_record (con_head_normalize(D,con_k))
		of SOME _ => error (locate "prim_valid") "inject_nonrecord injects into record field"
		 | NONE => ())
	 in
	   con_k
	 end
	| (project_sum_record (k,l),[argcon],[argexp]) => 
	 let
	   val argcon' = exp_valid (D,argexp)
	   val _ = 
	     (type_equiv(D,argcon,argcon')) orelse
	     (error (locate "prm_valid") "Type mismatch in project_sum_record")
	   val argkind = con_valid(D,argcon)
           val con_k = projectSumType(D,argcon,k)
	   val con = projectRecordType (D,con_k,l)
	 in
	   con
	 end
       
	| (box_float floatsize,[],[exp]) => 
	 let
	   val con = exp_valid (D,exp)
	   val box_con = Prim_c (BoxFloat_c floatsize,[])
	   val _ =
	     (case strip_float (con_head_normalize(D,con))
		of SOME floatsize' => 
		  (same_floatsize (floatsize,floatsize')) orelse
		  (error (locate "prim_valid") "Mismatched float size in box float" handle e => raise e)
		 | NONE => (error (locate "prim_valid") "Box float called on non-float" handle e => raise e))
	 in box_con
	 end
	| (unbox_float floatsize,[],[exp]) => 
	 let
	   val con = exp_valid (D,exp)
	   val unbox_con = Prim_c (Float_c floatsize,[])

	   val _ = 
	     (case strip_boxfloat (con_head_normalize(D,con))
		of SOME floatsize' => 
		  (same_floatsize (floatsize,floatsize')) orelse
		  (error (locate "prim_valid") "Mismatched float size in box float" handle e => raise e)
		 | NONE => (error (locate "prim_valid") "Unbox float called on non-boxfloat" handle e => raise e))
	 in
	   unbox_con
	 end
	| (roll,[argcon],[exp]) => 
	 let
	   val argkind = con_valid (D,argcon)
	   val con = exp_valid (D,exp)
	   val expanded_arg_con = expandMuType(D,argcon) 
	   val _ = 
	     (type_equiv (D,con,expanded_arg_con)) orelse
	     (perr_e_c_c (exp,expanded_arg_con,con);
	      (error (locate "prim_valid") "Error in roll" handle e => raise e))
	 in
	   argcon
	 end

	| (unroll,[con],[exp]) =>
	 let
	   val kind = con_valid (D,con)
	   val found_con = exp_valid (D,exp)
	   val expanded_con = expandMuType (D,con)
	   val _ = 
	     (type_equiv (D,found_con,con)) orelse
	     (perr_e_c_c (exp,con,found_con);
	      (error (locate "prim_valid") "Error in unroll" handle e => raise e))
	 in
	   expanded_con
	 end
	| (make_exntag,[argcon],[]) => 
	 let
	   val argkind = con_valid (D,argcon)
	   val con = Prim_c (Exntag_c,[argcon])
	 in
	   con
	 end
	| (inj_exn name,[],[exp1,exp2]) => 
	 let
	   val con1 = exp_valid (D,exp1)
	   val con2 = exp_valid (D,exp2)
	   val _ = 
	     (case strip_exntag (con_head_normalize(D,con1))
		of SOME con => 
		  (type_equiv (D,con2,con)) orelse
		  (error (locate "prim_valid") "Type mismatch in exception injection" handle e => raise e)
		 | NONE =>  
		    (perr_e_c (exp1,con1);
		     (error (locate "prim_valid") "Illegal argument to exception injection - not a tag" handle e => raise e)))
	   val con = Prim_c (Exn_c,[])
	 in con
	 end
	| (make_vararg (openness,effect),cons,exps) =>
	 (error (locate "prim_valid") "make_vararg unimplemented....punting" handle e => raise e)
	| (make_onearg (openness,effect),cons,exps) =>  
	 (error (locate "prim_valid") "make_onearg unimplemented....punting" handle e => raise e)
	| (peq,cons,exps) => 
	   (error (locate "prim_valid") "Polymorphic equality should not appear at this level" handle e => raise e)
	| (prim,cons,exps) => 
	 (perr_e (Prim_e (NilPrimOp prim,cons,exps));
	  lprintl "No matching case in prim_valid";
	  (error (locate "prim_valid") "Illegal primitive application" handle e => raise e)))

  and switch_valid (D,switch) =
    (case switch
       of Intsw_e {size,arg,arms,default} =>
	 let
	   val argcon = exp_valid (D,arg)
	   val (ns,armexps) = unzip arms
	   val armcons = map (curry2 exp_valid D) armexps
	   val _ = 
	     case strip_int (con_head_normalize(D,argcon))
	       of SOME intsize' => 
		 (same_intsize (size,intsize')) orelse
		 (error (locate "switch_valid") "Integer size mismatch in int switch" handle e => raise e)
		| NONE => 
		   (perr_e_c (arg,argcon);
		    (error (locate "switch_valid") "Branch argument not an int" handle e => raise e))
		   
	   fun check_arms (NONE,[]) = error (locate "switch_valid") "Int case must be non-empty"
	     | check_arms (SOME rep_con,[]) = rep_con
	     | check_arms (NONE,con::rest) = check_arms (SOME con,rest)
	     | check_arms (SOME rep_con,con::rest) =  
	     if type_equiv (D,rep_con,con) then 
	       check_arms (SOME rep_con,rest)
	     else
	       (perr_c_c (rep_con,con);
		(error (locate "switch_valid") "Branch arm types don't match" handle e => raise e))
		   
	   val default_con = map_opt (curry2 exp_valid D) default
	   val rep_con = check_arms (default_con,armcons)
	 in
	   rep_con
	 end
	| Sumsw_e {arg,sumtype,bound,arms,default} => 
	 let
	   val argcon = exp_valid (D,arg)
	   val _ = con_valid(D,sumtype)
	     
	   val _ =
	     (type_equiv(D,sumtype,argcon)) orelse
	     (perr_e_c (arg,argcon);
	      print "given type:\n";
	      pp_con sumtype; print "\n";
	      error (locate "switch_valid") "Type given for sum switch argument does not match found type")
	     
	   val (non_val,totalcount,_,carrier) = 
	     (case strip_sum (con_head_normalize(D,argcon)) of
		SOME quad => quad
	      | _ => 
		  (perr_e_c (arg,argcon);
		   (error (locate "switch_valid") "Branch argument not of sum type" handle e => raise e)))
		
	   fun mk_sum field = 
	     Prim_c (Sum_c {tagcount=non_val,
			    totalcount=totalcount,
			    known=SOME field},[carrier])


	   fun check_loop (NONE,[]) = error (locate "switch_valid") "Empty switch not allowed"
	     | check_loop (SOME con,[]) = con
	     | check_loop (con_opt,(index,exp)::arms) = 
	     let 
	       val sum_con = mk_sum index
	       val D = insert_con(D,bound,sum_con)	     
	       val con = exp_valid(D,exp)
	     in
	       (case con_opt
		  of SOME con' =>
		    if (type_equiv (D,con',con)) then 
		      check_loop (con_opt,arms)
		    else
		      (error (locate "switch_valid") "Sum Branch arm types don't match" handle e => raise e)
		   | NONE => check_loop(SOME con,arms))
	     end
	   
	   val default_con = map_opt (curry2 exp_valid D) default
	     
	   val rep_con = check_loop (default_con,arms)

	 in
	    rep_con
	 end
     | Exncase_e {arg,bound,arms,default} =>
	 let
	   val argcon = exp_valid (D,arg)
	   val _ = 
	     (is_exn_con (con_head_normalize (D,argcon))) orelse
	     (error (locate "switch_valid") "Argument to exncase not an exn")

	   val (indices,bodies) = unzip arms
	   val index_cons = map (curry2 exp_valid D) indices

	   fun check_arms (NONE,[],[]) = error (locate "switch_valid") "Exn case must be non-empty"
	     | check_arms (SOME rep_con,[],[]) = rep_con
	     | check_arms (con_opt,icon::icons,exp::exps) = 
	     let
	       val icon = con_head_normalize(D,icon)
	       val argtype = 
		 (case strip_exntag icon
		    of SOME exn_con => exn_con
		     | NONE => (perr_c icon;
				error (locate "switch_valid") ("Index has wrong type" handle e => raise e)))
	       val D = insert_con(D,bound,argtype)
	       val con = exp_valid(D,exp)
	       val con_opt =
		 (case con_opt
		    of SOME rep_con => 
		      let val _ = 
			(type_equiv (D,rep_con,con)) orelse
			(perr_c_c (rep_con,con);
			 (error (locate "switch_valid") "Exn Branch arm types don't match" handle e => raise e))
		      in con_opt
		      end
		     | NONE => SOME con)
	     in check_arms(con_opt,icons,exps)
	     end
	     | check_arms _ = error (locate "switch_valid") "Index/arm mismatch in exn switch"
	     
	   val default_con = map_opt (curry2 exp_valid D) default
	   val rep_con = check_arms (default_con,index_cons,bodies)
	 in
	   rep_con
	 end
     | Typecase_e {arg=argcon,arms,default} => error (locate "switch_valid") "Typecase_e not done")

       
       
  and bnds_valid (D,bnds) = (foldl bnd_valid' (D,Subst.empty()) bnds)
  and bnd_valid (state,bnd) = bnd_valid' (bnd,state)
  and bnd_valid'' (bnd,state) = 
    let
      val (D,subst) = state

      val bnd = substConInBnd subst bnd

      fun function_valid openness D (Function (effect,recursive,tformals,dependent,
					       formals,fformals,body,return)) = 
	let
	  val D' = foldl (fn ((v,k),D) => (kind_valid(D,k);insert_kind(D,v,k))) D tformals
	  val D = foldl (fn ((v,c),D) => (con_valid(D,c);insert_con(D,v,c))) D' formals
	  val D = 
	    (foldl (fn (v,D) => 
		    insert_con (D,v,Prim_c (Float_c F64,[]))) D fformals)
	  val body_c = exp_valid (D,body)
	  val D = if dependent then D else D'
	  val _ = con_valid (D,return)
	  val _ = 
	    (type_equiv (D,body_c,return)) orelse
	    (perr_e_c_c (body,return,body_c);
	     (error (locate "function_valid") "Return expression has wrong type" handle e => raise e))    

	  val (vars_opt,cons) = let val (vars,cons) = unzip formals
				in if dependent then (SOME vars,cons) else (NONE,cons) 
				end
	  val numfloats = Word32.fromInt (List.length fformals) 
	  val con = AllArrow_c(openness,effect,tformals,vars_opt,cons,numfloats,body_c)
	in
	  con
	end

      fun fbnd_valid (openness,defs) = 
	let
	  val origD = D
	  val bnd_types = Sequence.map_second (function_type openness) defs
	  val _ = Sequence.map_second (curry2 con_valid D) bnd_types
	  val D = Sequence.foldl (fn ((v,c),D) => insert_con(D,v,c)) D bnd_types
	  val found_types = Sequence.map_second (function_valid openness D) defs
	  fun checker ((_,c),(_,c')) = 
	    (type_equiv(origD,c,c')) orelse 
	    (error (locate "bnd_valid") "Declared function type does not match found type")
	  val _ = Sequence.all2 checker (bnd_types,found_types)
	in (D,subst)
	end
    in
      (case bnd 
	 of Con_b (phase, cbnd) => 
	   let
	     val _ = cbnd_valid (cbnd,D)
	     val (v,c) = 
	       (case cbnd of
		  Con_cb (v,c) => (v,c)
		| Open_cb(v,vklist,c,k) => (v,Let_c(Sequential,[cbnd],Var_c v))
		| Code_cb(v,vklist,c,k) => (v,Let_c(Sequential,[cbnd],Var_c v)))
	     val D = NilContext.insert_equation(D,v,c)
	     val subst = Subst.add subst (v,c)
	   in  (D,subst)
	   end
	  | Exp_b (var, tracinfo,exp) =>
	   let
	     val bnd_con = exp_valid (D,exp)
	     val D = insert_con (D,var,bnd_con)
	   in
	     (D,subst)
	   end
	  | (Fixopen_b defs) => fbnd_valid(Open,defs)
	  | (Fixcode_b defs) => fbnd_valid(Code,defs)
	  | Fixclosure_b (is_recur,defs) => 
	   let
	     val origD = D
	     val (vars,closures) = unzip (Sequence.toList defs)
	     val tipes = map (fn cl => #tipe cl) closures
	     val _ = map (curry2 con_valid D) tipes
	     val bnd_types = zip vars tipes
	     fun do_closure D ({code,cenv,venv,tipe}) = 
	       let
		 val ckind = con_valid (D,cenv)
		 val vcon = exp_valid (D,venv)
		 val (code_type) = 
		   (find_con (D,code)
		    handle NilContext.Unbound =>
		      (printl ("Code pointer "^(var2string code));
		       print " not defined in context";
		       (error (locate "bnd_valid") "Invalid closure" handle e => raise e)))
		 val (effect,tformals,vars_opt,formals,numfloats,body_c) = 
		   (case strip_arrow (con_head_normalize(D,code_type)) of
		       SOME (Code,effect,tformals,vars_opt,formals,numfloats,body_c) => 
			   (effect,tformals,vars_opt,formals,numfloats,body_c) 
		     | SOME (ExternCode,effect,tformals,vars_opt,formals,numfloats,body_c) => 
			   (effect,tformals,vars_opt,formals,numfloats,body_c) 
		     | _ => (perr_e_c (Var_e code,code_type);
			     (error (locate "bnd_valid") "Code pointer in closure of illegal type" handle e => raise e)))
		 val (tformals,(v,last_k)) = split tformals
		 val tformals = map (fn (tv,k) => (tv,varConKindSubst v cenv k)) tformals
		 val formals = map (varConConSubst v cenv) formals
		 val (formals,last_c) = split formals

		 val D = foldl (fn ((v,k),D) => insert_kind(D,v,k)) D tformals
		 val (D',body_c) = 
		   (case vars_opt
		      of SOME vars =>
			let
			  val (vars,last) = split vars
			in (foldl2 (fn (v,c,D) => insert_con (D,v,c)) D (vars,formals),varExpConSubst last venv body_c)
			end
		       | NONE => (D,body_c))

		 val body_c = varConConSubst v cenv body_c

		 val _ = 
		   (sub_kind (D,Singleton_k(cenv),last_k) andalso type_equiv (D',vcon,last_c)) orelse
		   (perr_k_k (last_k,ckind);
		    perr_c_c (last_c,vcon);
		    (error (locate "bnd_valid") "Mismatch in closure" handle e => raise e))

		 val closure_type = AllArrow_c (Closure,effect,tformals,vars_opt,formals,numfloats,body_c)

		 val _ = 
		   (type_equiv (D,closure_type,tipe)) orelse
		   (perr_c_c (tipe,closure_type);
		    print "code_type is "; pp_con code_type; print "\n";
		    print "con is "; pp_con closure_type; print "\n";
		    (error (locate "bnd_valid") "Type error in closure" handle e => raise e))
	       in ()
	       end
	     val D = insert_con_list (D,bnd_types)
	     val _ = if is_recur 
		       then app (do_closure D) closures
		     else app (do_closure origD) closures
	   in
	     (D,subst)
	   end)
    end

  and exp_valid (D : context, exp : exp) : con = 
      let 
	val _ = push_exp(exp,D)
	  val _ = if (!show_calls)
		      then (print "exp_valid called with expression =\n";
			    pp_exp exp; 
			    if !show_context then (print "\nand context"; NilContext.print_context D) else ();
			    print "\n\n")
		  else ()
	val _ = 
	  if !debug then
	    assert (locate "exp_valid PRE")
	    [
	     assertRenamedExp (D,exp),
	     assertWellFormed D
	     ]
	  else ()
	val res = exp_valid'(D,exp)
	val _ = if (!show_calls)
		  then (printl "exp_valid returned")
		else ()
	val _ = 
	  if !debug then
	    assert (locate "exp_valid POST")
	    [
	     assertRenamedCon (D,res)
	     ]
	  else ()
	  val _ = pop()
      in  res
      end

  and exp_valid' (D : context,exp : exp) : con = 
    (case exp of
       Var_e var => 
	 (find_con (D,var)
	  handle NilContext.Unbound =>
	    (error (locate "exp_valid") ("Encountered undefined variable " ^ (Name.var2string var))))
     | Const_e value => value_valid (D,value)
     | Let_e (letsort,bnds,exp) => 
	 let
	   val (D,subst) = bnds_valid (D,bnds)
	   val exp = substConInExp subst exp
	   val con = exp_valid (D,exp)
	 in
	   con
	 end
     | Prim_e (NilPrimOp prim,cons,exps) =>   prim_valid (D,prim,cons,exps)
     | Prim_e (PrimOp prim,cons,exps) =>   
	 let 
	   val kinds = map (curry2 con_valid D) cons
	   val (total,arg_types,return_type) = NilPrimUtil.get_type prim cons
	   val _ = con_valid(D,return_type)
	   val _ = app (ignore o (curry2 con_valid D)) arg_types
	   val exp_cons = map (curry2 exp_valid D) exps
	   val _ = 
	     (c_all2 (fn (c1,c2) => type_equiv(D,c1,c2))
	      (o_perr_c_c "Length mismatch in prim args") (arg_types,exp_cons)) orelse
	     (Ppnil.pp_list pp_con' arg_types ("\nExpected arguments of types: ",",","\n",false);
	      Ppnil.pp_list pp_exp' exps ("\nFound arguments: ",",","\n",false);
	      Ppnil.pp_list pp_con' exp_cons ("\nof types: ",",","\n",false);
	      error (locate "exp_valid") "Illegal type for Prim op")
	 in
	   return_type
	 end
     | Switch_e switch => switch_valid (D,switch)
     | (App_e (openness,app,cons,texps,fexps)) =>
	 let
	   
	   val con = exp_valid (D,app)

	   val (openness',_,tformals,vars_opt,formals,numfloats,body) = 
	     (case strip_arrow (con_head_normalize(D,con))
		of SOME c => c
		 | NONE => (perr_e_c (app,con);
			    (error (locate "exp_valid") "Application of non-arrow expression" handle e => raise e)))
	     
		
	   val _ = (case (openness,app) of
		      (Code,Var_e _) => ()
		    | (Code,_) => (perr_e app;
				   error (locate "exp_valid") "code applied to non-variable")
		    | _ => ())
	     
	   val _ = 
	     (same_openness (openness,openness')) orelse
	     (error (locate "exp_valid") "Error in application - different openness" handle e => raise e)

	   val kinds = map (curry2 con_valid D) cons

	   val _ = 
	     (eq_len (tformals,kinds)) orelse
	     (Ppnil.pp_list (fn (v,k) => pp_kind' k) tformals ("\nFormal param kinds are: ",",","\n",false);
	      Ppnil.pp_list pp_con' cons ("\nActuals are: ",",","\n",false);
	      error (locate "exp_valid") "Length mismatch between formal and actual constructor parameter lists")

	   fun check_one_kind ((var,formal_kind),actual_kind,D) = 
	     let
	       val _ = 
		 (sub_kind (D,actual_kind,formal_kind)) orelse
		 (perr_k_k (formal_kind,actual_kind);
		  (error (locate "exp_valid") "Constructor parameter kind mismatch" ))
	     in
	       insert_kind(D,var,actual_kind)
	     end

	   val D = foldl2 check_one_kind D (tformals,kinds)

	   val t_cons = map (curry2 exp_valid D) texps

	   fun print_error () =
	     (Ppnil.pp_list pp_con' formals ("\nFormal Types: (",", ",")\n",false);
	      Ppnil.pp_list pp_con' t_cons ("\nActual Types: (",", ",")\n",false);
	      Ppnil.pp_list pp_exp' texps ("\nActuals: (",", ",")\n",false);
	      perr_e exp;
	      error (locate "exp_valid") "Formal/actual parameter type mismatch")

	   val subst = Subst.fromList (zip (#1 (unzip tformals)) cons)
	   val formals = map (Subst.substConInCon subst) formals
	     
	   val D = 
	     (case vars_opt
		of SOME vars => 
		  let
		    fun check_one_dep (var,actual_con,formal_con,D) = 
		      if (type_equiv (D,actual_con,formal_con)) then insert_con (D,var,actual_con) 
		      else (print_error ())
		  in
		    foldl3 check_one_dep D (vars,t_cons,formals)
		  end
		 | NONE => 
		  let
		    fun check_one_con (actual_con,formal_con) = 
		      (type_equiv (D,actual_con,formal_con)) orelse (print_error ())
		    val _ = all2 check_one_con (t_cons,formals)
		  in
		    D
		  end)
		
	   val f_cons = map (curry2 exp_valid D) fexps
	   val f_cons = map (curry2 con_head_normalize D) f_cons
	   val err = o_perr_c_c "Length mismatch in exp actuals"

	   val _ = 
	     (c_all is_float_c (fn c => (perr_c c;false)) f_cons) orelse
	     (error (locate "exp_valid") "Expected float for float parameter" handle e => raise e)

	   val _ = 
	     ((Word32.toInt numfloats) = (List.length fexps)) orelse 
	     (error (locate "exp_valid") "Wrong number of float parameters")

(*	   val _ = (lprintl "STARTED WITH TYPE";
		    pp_con con;
		    lprintl "IN APP_E")

	   val _ = (lprintl "SUBSTITUTING";
		    Subst.printConSubst subst;
		    lprintl "IN APP_E")*)
	   val body = 
	     let
	       val body = Subst.substConInCon subst body
	     in
	       case vars_opt
		 of SOME vars => substExpInCon (Subst.fromList (zip vars texps)) body
		  | NONE => body
	     end

(*	   val _ = (lprintl "GENERATING TYPE";
		    pp_con con;
		    lprintl "IN APP_E")*)
	 in
	   body
	 end
     | ExternApp_e (exp,args) =>
	 let
	   val con = exp_valid(D,exp)
	   val (formals,return) =
	     (case strip_externarrow (con_head_normalize(D,con))
		of SOME value => value
		 | NONE => error (locate "exp_valid") "Extern application of non extern arrow value")
	   val argcons = map (curry2 exp_valid D) args 
	     
	   fun check_one_con (actual_con,formal_con) = 
	     (type_equiv (D,actual_con,formal_con)) orelse 
	     (error (locate "exp_valid") "Parameter type mismatch in External application")
	     
	   val _ = all2 check_one_con (argcons,formals)
	 in
	   return
	 end
       
       
     | Raise_e (exp,con) => 
	 let
	   val kind = con_valid (D,con)
	   val exn_con = exp_valid (D,exp)
	   val _ = 
	     (is_exn_con (con_head_normalize (D,exn_con))) orelse
	     (perr_e_c (exp,con);
	      (error (locate "exp_valid") "Non exception raised - Ill formed expression" handle e => raise e))
	 in
	   con
	 end
     | Handle_e (exp,v,handler) => 
	 let
	   val con = exp_valid(D,exp)
	   val D = insert_con(D,v,Prim_c(Exn_c,[]))
	   val handler_con = exp_valid(D,handler)
	   val _ = 
	     (type_equiv(D,con,handler_con)) orelse
	     error (locate "exp_valid") "Handler body has different type from handled expression"
	 in
	   con
	 end
       )

      and bnd_valid' (bnd,state) = 
	let 
	  val (D,subst) = state
	  val _ = push_bnd(bnd,D)
	  val _ = if (!show_calls)
		    then (print "bnd_valid called with bnd =\n";
			  Ppnil.pp_bnd bnd;
			  if !show_context then (print "\nand context"; NilContext.print_context D) else ();
			  print "\n\n")
		  else ()
	  val res = (bnd_valid''(bnd,state)
		     handle e => (show_stack(); raise e))
	  val _ = if (!show_calls)
		    then (printl "bnd_valid returned")
		  else ()
	  val _ = pop()
	in  res
      end

      val exp_valid = wrap "exp_valid" exp_valid
      val con_valid = wrap "con_valid" con_valid
      val kind_valid = wrap "kind_valid" kind_valid
      val con_reduce = wrap "con_reduce" con_head_normalize


      fun import_valid' (ImportValue (label,var,con),D) =
	let
	  val kind = con_valid(D,con)
	  val D = insert_con(D,var,con)
	in
	  D
	end
	| import_valid' (ImportType (label,var,kind),D) = 
	let
	  val _ = kind_valid (D,kind)
	  val D = insert_kind(D,var,kind)
	in
	  D
	end

      fun import_valid (D,import) = import_valid' (import,D)

      fun export_valid D (ExportValue (label,var)) = 
	let
	  val con = exp_valid(D,Var_e var)
	in  ()
	end
	| export_valid D (ExportType (label,var)) = 
	let
	  val kind = con_valid(D,Var_c var)
	in ()
	end

      fun module_valid' (D,MODULE {bnds,imports,exports}) = 
	let
	  val D = foldl import_valid' D imports
	  val D = #1(bnds_valid(D,bnds))
	  val _ = app (export_valid D) exports
	in
	  ()
	end

      fun module_valid (D,module) = 
	let 
	  val _ = push_mod(module,D)
	  val module = 
	    if !debug then
	      (assert (locate "module_valid:PRE")
	       [
		(isRenamedMod module,fn () => lprintl "Module not renamed")
		]
	       ;module) 
	      handle NilError.FailedAssert s => (lprintl "Renaming for you";renameMod module)
	    else module
	  val _ = if (!show_calls)
		    then (print "module_valid called with module =\n";
			  Ppnil.pp_module module;
			  if !show_context then (print "\nand context"; NilContext.print_context D) else ();
			  print "\n\n")
		  else ()
	  val res = module_valid'(D,module)

	  val _ = if (!show_calls)
		    then (printl "module_valid returned")
		  else ()
	  val _ =
	    if !debug then
	      assert (locate "module_valid:POST")
	      [
	       (isRenamedMod module,fn () => lprintl "Renaming not preserved")
	       ]
	    else ()
	  val _ = pop()
	in  res
	end
    
(*      type con_subst = con Subst.subst
      val empty_subst = Subst.empty() : con_subst
      fun con_subst (subst,c) = Subst.substConInCon subst c
      fun con_reduce_once context_subst c = Normalize.con_reduce_once context_subst c*)
      val module_valid = wrap "module_valid" module_valid



end
