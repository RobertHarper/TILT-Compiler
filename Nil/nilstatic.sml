(*$import NILSTATIC Nil Ppnil NilContext NilError NilSubst Stats Normalize NilUtil NilHNF *)
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
  val pp_var = Ppnil.pp_var
  val pp_label = Ppnil.pp_label
  val pp_label' = Ppnil.pp_label'
  val pp_list = Ppnil.pp_list

  val trace = Stats.ff "nilstatic_trace"
  val local_debug = Stats.ff "nilstatic_debug"
  val debug = Stats.ff "nil_debug"
  val show_calls = Stats.ff "nil_show_calls"
  val show_context = Stats.ff "nil_show_context"
  val short_circuit = Stats.tt "nilstatic_shortcircuit"
  val warn_depth = ref 500
  val type_equiv_count = Stats.counter "Type Equiv Calls"
  val alpha_equiv_checks = Stats.counter "Alpha Equiv Checks"
  val alpha_equiv_success = Stats.counter "Alpha Equiv Checks Succeeded"
  val kind_of_calls = Stats.counter "Kind of Calls"
  val kind_standardize_calls = Stats.counter "Kind Standardize Calls"

  val locate = NilError.locate "NilStatic"
  val assert = NilError.assert

  val equiv_depth = ref 0

  fun error s s' = Util.error s s'

  fun error' s = error "nilstatic.sml" s

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
				      Ppnil.pp_module 
                                        {module = m,
                                         header = "",
                                         name = "",
                                         pass = ""};
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

  val timer = Stats.timer
  val subtimer = Stats.subtimer
  (*From Normalize*)

  val expandMuType = Normalize.expandMuType
  val projectRecordType = Normalize.projectRecordType
  val projectSumType = Normalize.projectSumType
  val type_of = Normalize.type_of

  (*From NilContext*)
  type context = NilContext.context
  val empty = NilContext.empty
  val insert_con = subtimer ("Tchk:insert_con",NilContext.insert_con)
  val insert_con_list = subtimer ("Tchk:insert_con_list",NilContext.insert_con_list)
  val find_con = subtimer ("Tchk:find_con",NilContext.find_con)
  val find_std_con = subtimer ("Tchk:find_std_con",NilContext.find_std_con)
  val insert_kind = subtimer ("Tchk:insert_kind",NilContext.insert_kind)
  val insert_kind_equation = subtimer ("Tchk:insert_kind_equation",NilContext.insert_kind_equation)
  val insert_kind_list =subtimer ("Tchk:insert_kind_list", NilContext.insert_kind_list)
  val find_kind = subtimer ("Tchk:find_std_kind",NilContext.find_std_kind)             (*NOTE RENAMING!*)
  val kind_standardize = subtimer ("Tchk:kind_standardize",NilContext.kind_standardize)
  val kind_standardize = fn args => (kind_standardize_calls();
				     kind_standardize args)
  val kind_of = subtimer ("Tchk:kind_of",NilContext.kind_of)
  val kind_of = fn args => (kind_of_calls();
(*			    print ("Kind of called with argument:\n");
			    Ppnil.pp_con (#2 args);
			    print "\nReturned\n";*)
			    kind_of args)

  structure Subst = NilSubst
  type con_subst = Subst.con_subst
  type exp_subst = Subst.exp_subst
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
  val empty_subst = Subst.C.empty
  val con_subst_compose = Subst.C.compose

  (*From NilUtil*)
  val generate_tuple_label = NilUtil.generate_tuple_label
  val function_type = NilUtil.function_type
  val convert_sum_to_special = NilUtil.convert_sum_to_special
  val exp_tuple = NilUtil.exp_tuple
  val con_tuple = NilUtil.con_tuple
  val convar_occurs_free = NilUtil.convar_occurs_free
  val con_free_convar = NilUtil.con_free_convar
  val same_openness = NilUtil.same_openness
  val same_effect = NilUtil.same_effect
  val primequiv = NilUtil.primequiv
  val sub_phase = NilUtil.sub_phase
  val project_from_kind = subtimer ("Tchk:project_from_kind",NilUtil.project_from_kind)
  val selfify = subtimer ("Tchk:selfify",NilUtil.selfify)
  val alpha_equiv_con = subtimer ("Tchk:alpha_equiv", NilUtil.alpha_equiv_con)
  val alpha_equiv_con = (fn args => (alpha_equiv_checks();
				     (alpha_equiv_con args) andalso (alpha_equiv_success();true)))
    (*
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
  val count1 = Listops.count1
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
  (* XXX CS: detects conflicts between namespaces  that ought not occur! *)
  val labels_distinct = Listops.no_dups Name.compare_label

  (*From PrimUtil*)
  val same_intsize = NilPrimUtil.same_intsize
  val same_floatsize = NilPrimUtil.same_floatsize

  (*From Util *)
  val eq_opt = Util.eq_opt
  val map_opt = Util.mapopt
  val split_opt = Util.split_opt
  val printl = Util.printl
  val lprintl = Util.lprintl
  val lprint = Util.lprint
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
     end)
*)

  fun bound_kind arg = ((find_kind arg; true)
			handle NilContext.Unbound => false)
  val find_con = find_con
  val find_kind = find_kind

  val con_head_normalize = NilHNF.reduce_hnf

  val time_con_head_normalize = fn s => Stats.subtimer("Tchk:CHNF:"^s,con_head_normalize)
  val con_head_normalize = time_con_head_normalize "Rest"

  (*PRE: kind is standard
   *)
  fun is_type kind = 
    (case kind
       of Type_k => true
        | SingleType_k _ => true
	| _ => false)

  fun is_type' D kind = 
    (case kind_standardize(D,kind) 
       of Type_k => true
	| SingleType_k _ => true
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
	   assertWellFormed D
	   (* assertRenamedKind (D,kind) *)
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
     | SingleType_k con => con_analyze(D,con,Type_k)
     | Single_k con => ignore(con_valid(D,con))
     | Record_k elts => 
	 let
	   fun folder (((l,v),k),D) = 
	     (kind_valid (D,k);
	      insert_kind (D,v,k))
	   val _ = Sequence.foldl folder D elts
	   fun compare (((l1,_),_),((l2,_),_)) = Name.compare_label (l1,l2)
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

  and con_analyze(D,con,kind) =
    let
      val kind' = con_valid(D,con)
    in
      if sub_kind(D,kind',kind) then ()
      else error (locate "con_analyze") "Con cannot be given kind"
    end
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
	   assertWellFormed D
	   (* assertRenamedCon (D,constructor) *)
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
	   (* assertRenamedKind (D,k) *)
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
	   (Record_c (labels,NONE)) => 
	     let
	       val _ = 
		 (if labels_distinct labels then
		    (c_all is_type b_perr_k kinds) orelse
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
		      (case rkind
			 of Record_k entries => Sequence.maptolist (fn (_,k) => k) entries
			  | other => [other])
		     | _ => error (locate "pcon_valid") "Wrong number of args to Sum_c")

	       val _ = 
		 (
		  ((length kinds) = (Word32.toInt (totalcount - tagcount))) orelse
		  (error (locate "pcon_valid") "Sum_c counts disagree with args");
		  
		  (List.all is_type kinds) orelse
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
		 (c_all is_type b_perr_k kinds) orelse
		 (error (locate "pcon_valid") "Vararg has non-word component" )
	     in ()
	     end
	 | _ => ())
    in ()
    end
  
  and con_valid' (D : context, constructor : con) : kind = 
    let

      val kind = 
	(case constructor of
	   (Prim_c (Record_c (labels,SOME vars), args)) =>
	     let
	       fun folder (v,c,(kinds,D)) = ((con_valid (D,c))::kinds,insert_con (D,v,c))
	       val (kinds,D) = foldl2 folder ([],D) (vars,args)
	       val _ = 
		 (if labels_distinct labels then
		    (c_all is_type b_perr_k kinds) orelse
		    (error (locate "con_valid'") "DepRecord contains field of non-word kind")
		  else
		    (error (locate "con_valid'") "DepRecord contains duplicate field labels"))
	     in SingleType_k(constructor)
	     end
	 | (Prim_c (pcon,args)) => (pcon_valid (D,pcon,args);SingleType_k(constructor))
	 | (Mu_c (is_recur,defs)) =>
	     let
	       val D =
		 if is_recur then
		   let fun folder((v,_),D) = insert_kind (D,v,Type_k)
		   in  Sequence.foldl folder D defs
		   end
		 else D
		   
	       val kinds = map (fn (_,c) => con_valid(D,c)) (Sequence.toList defs)
		 
	       val _ = 
		 (c_all is_type b_perr_k kinds) orelse
		 (error (locate "con_valid'") "Invalid kind for recursive constructor" )
		 
	       val len = Sequence.length defs
	     in  
	       if len = 1
		 then SingleType_k(constructor)
	       else 
		 let 
		   fun mapper (i,(v,c)) = 
		     let val label = generate_tuple_label(i+1)
		     in((label,derived_var v),SingleType_k(Proj_c(constructor,label)))
		     end
		   val entries = Sequence.mapcount mapper defs
		 in  Record_k(entries)
		 end
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
		 ((c_all is_type b_perr_k formal_kinds) andalso (is_type body_kind)) orelse
		 (error (locate "con_valid'") "Invalid arrow constructor")
	     in
	       SingleType_k(constructor)
	     end
	    | ExternArrow_c (args,body) => 
	     let
	       val kinds = map (curry2 con_valid D) args
	       val kind = con_valid (D,body)
	       val _ = 
		 ((c_all is_type b_perr_k kinds) andalso (is_type kind)) orelse
		 (error (locate "con_valid") "Invalid extern arrow constructor")
	     in
	       SingleType_k(constructor)
	     end
	    | (v as (Var_c var)) => 
	      let
		val kind = (find_kind (D,var)
			    handle Unbound =>
			      (NilContext.print_context D;
			       error  (locate "con_valid") ("variable "^(var2string var)^" not in context")))
	      in
		kind
(*		selfify(v,kind) (*XXX SHould already be selfified?*)*)
	      end
	    | (Let_c (Parallel,cbnds,con)) => error' "Parallel bindings not supported yet"
	    | (Let_c (Sequential,cbnds,con)) =>
	      let  
		val D = foldl cbnd_valid D cbnds
		val kind = con_valid(D,con)
	      in kind   (*Since kind is principal, don't need to substitute*)
	      end
	    | (Typeof_c exp) => (SingleType_k(exp_valid (D,exp)))
	    | (Closure_c (code,env)) => 
	     let
	       val env_kind = con_valid (D,env)
	       val code_kind =  con_valid (D,code)
	       val (vklist,body_kind) = 
		 case code_kind of
		   Arrow_k (Code ,vklist,body_kind) => (vklist,body_kind)
		 | Arrow_k (ExternCode,vklist,body_kind) =>  (vklist,body_kind)
		 | _ => (error (locate "con_valid") "Invalid closure: code component does not have code kind")
	       val (first,(v,klast)) = split vklist
	       val _ = 
		 (sub_kind (D,env_kind,klast)) orelse
		 (print "Invalid kind for closure environment:";
		  print " env_kind < klast failed\n";
		  print "env_kind is "; pp_kind env_kind; print "\n";
		  print "klast is "; pp_kind klast; print "\n";
		  print "code_kind is "; pp_kind code_kind; print "\n";
		  (error (locate "con_valid'") "Invalid kind for closure environment" ))
	       val body_kind = NilSubst.varConKindSubst v env body_kind
	     in
	       Arrow_k(Closure,first,body_kind)
	     end
	    | (Crecord_c entries) => 
	     let
	       val _ = if (!trace)
			 then print "{con_valid processing Crecord_c\n"
		       else ()
	       val (labels,cons) = unzip entries
	       val kinds = map (curry2 con_valid D) cons
	       val _ =  
		 (labels_distinct labels) orelse
		 (Ppnil.pp_list Ppnil.pp_label' labels 
		  ("labels are: ",",",";",true);
                  Listops.no_dups (fn (x,y) => let val _ = (print "comparing: "; pp_label x; print " and "; pp_label y; print " : ")
                                                   val result = Name.compare_label (x,y)
                                                   val _ = if result = EQUAL then print "TRUE\n" else print "FALSE\n"
                                               in result end) labels;
		  (error (locate "con_valid'") "Labels in record of constructors not distinct" ))
	       val _ = if (!trace)
			   then print "con_valid done processing Crecord_c\n}"
		       else () 
	       val k_entries = 
		 map2 (fn (l,k) => ((l,fresh_named_var "con_valid_record"),k)) (labels,kinds)
	     in Record_k (Sequence.fromList k_entries)
	     end
	    | (Proj_c (rvals,label)) => 
	     let
	       (*		 val _ = (print "XXX con_valid on Proj_c\n")
*)
	       val record_kind = con_valid (D,rvals)
		 
	       val entry_kinds = 
		 (case record_kind of
		    Record_k kinds => kinds
		  | other => 
		      (perr_c_k (constructor,record_kind);
		       lprintl "and context is";
		       NilContext.print_context D;
		       (error (locate "con_valid'") 
			"Non-record kind returned from con_valid in projection")))
		val labs = map (#1 o #1) (Sequence.toList entry_kinds)
(*		val _ = (print "XXX DONE con_valid on Proj_c\n")
*)
		val _ = if (Listops.member_eq(eq_label,label,labs))
			  then () 
			else (print "attempted to project label ";
			      pp_label label;
			      print " from \n";
			      pp_con rvals;
			      print "\n which has labels";
			      pp_list pp_label' labs ("",", ","",false);
			      error' "Ill-formed projection")
	     in
	       project_from_kind (entry_kinds,rvals,label)
	     end
	    | (App_c (cfun_orig,actuals)) => 
	     let
	       (*		 val _ = (print "XXX con_valid on App_c";
		(case cfun_orig of
		   Var_c v => (print " var = "; pp_var v)
	    | _ => print " nonvar");
		   print "\n")
		*)
	       val cfun_kind = con_valid (D,cfun_orig)
	       val (formals,body_kind) = 
		 case cfun_kind of
		   (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		 | _ => (print "Invalid kind for constructor application\n";
			 pp_kind cfun_kind; print "\n";
			 (error (locate "con_valid'") "Invalid kind for constructor application" ))
	       val actual_kinds = map (curry2 con_valid D) actuals
	       val (formal_vars,formal_kinds) = unzip formals
	       val _ = 
		 (c_all2 (fn (k1,k2) => sub_kind (D,k1,k2))
		  (o_perr_k_k "Constructor function applied to wrong number of arguments") 
		  (actual_kinds,formal_kinds))
		 orelse
		 (error (locate "con_valid'") "Constructor function failed: argument not subkind of expected kind")
(*	       val _ = print "XXX con_valid on App_c 5 DONE\n" *)
	       fun folder (v,c,subst) = NilSubst.C.sim_add subst (v,c)
	       val subst = Listops.foldl2 folder (NilSubst.C.empty()) (formal_vars,actuals)
	     in  
	       substConInKind subst body_kind
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
		      (error (locate "con_valid'") "Illegal kind in typecase"))
		 in ()
		 end
	       val _ = app doarm arms
	       val arg_kind = con_valid (D,arg)
	       val def_kind = con_valid (D,default)
	       val _ = 
		 ((sub_kind (D,def_kind,given_kind)) andalso (is_type arg_kind)) orelse
		 (error (locate "con_valid'") "Error in type case" handle e => raise e)
	     in
	       given_kind
	     end
	    | (Annotate_c (annot,con)) => con_valid (D,con)
	     )
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
(*XXX CS
	  val found_body_kind = con_valid (D,body)
	  val _ = 
	    (sub_kind (D,found_body_kind,body_kind)) orelse
	    (perr_c_k_k (body,body_kind,found_body_kind);
	     (error (locate "con_valid_letfun'") "invalid return kind for constructor function" handle e => raise e))
*)
	in ()
	end
      
      fun bnd_checker maker (var,formals,body,body_kind) = 
	let
	    val _ = if (!trace)
			then (print "{Processing Open_cb/Code_cb with var = ";
			      pp_var var; print "\n")
		    else ()
	  val _ = con_valid_letfun'(D,var,formals,body,body_kind)
	  val var' = derived_var var
	  val bnd = maker (var',formals,body,body_kind)
	  val con = Let_c (Sequential,[bnd],Var_c var')
	  val D = insert_kind(D,var,Single_k con)
	    val _ = if (!trace)
			then (print "Done processing Open_cb/Code_cb with var = ";
			      pp_var var; print "}\n")
		    else ()
	in
	  D
	end
      
    in
      (case cbnd
	 of Open_cb args => bnd_checker Open_cb args
	  | Code_cb args => bnd_checker Code_cb args
	  | Con_cb(var,con) =>
	   let
	     val _ = if (!trace)
			 then (print "{Processing Con_cb with var = ";
			       pp_var var; print "\n")
		     else ()
	     val kind = con_valid (D,con)
	     val D = insert_kind_equation(D,var,con,kind)
	     val _ = if (!trace)
			 then (print "Done processing Con_cb with var = ";
			       pp_var var; print "}\n")
		     else ()
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
	  val subst1 = Subst.C.sim_add subst1 (var1,Var_c var')
	  val subst2 = Subst.C.sim_add subst2 (var2,Var_c var')
	in
	  (subeq_kind is_eq (D,kind1,kind2),(D',subst1,subst2))
	end

      fun sub_all D (vks1,vks2) = 
	foldl_all2 sub_one (D,empty_subst(),empty_subst()) (vks1,vks2)

      val res = 
      (case (kind1,kind2) 
	 of (Type_k, Type_k) => true
	  | (Type_k,_) => false
	  | (Single_k _,_) => subeq_kind is_eq (D, kind_standardize(D,kind1), kind2)
	  | (_,Single_k _) => subeq_kind is_eq (D, kind1, kind_standardize(D,kind2))
	  | (SingleType_k _ ,Type_k) => true
	  | (SingleType_k (c1),SingleType_k (c2)) => 
	     type_equiv(D,c1,c2)
	  | (SingleType_k (c1), _) => false
(*
	  | (SingleType_k (c1),Single_k (c2)) => 
	   subeq_kind is_eq (D,push_singleton (D,c1),push_singleton (D,c2))
	  | (Single_k _ ,Type_k) => is_type kind1
	  | (Single_k (c1),Single_k (c2)) => 
	   subeq_kind is_eq (D,push_singleton (D,c1),push_singleton (D,c2))
	  | (Single_k (c1),k2) => (* k2 must be a higher kind *)
	   subeq_kind is_eq (D,push_singleton (D,c1),k2)
	  | (k1,Single_k (c2)) => (* k1 must be a higher kind *)
	   subeq_kind is_eq (D,k1,push_singleton (D,c2))
*)
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
	    (lprintl "sub_kind failed!";
	     printl "Kind:";
	     pp_kind kind1;
	     lprintl "Not equivalent to :";
	     pp_kind kind2;
	     printl "";
             false)
      val _ = pop()
    in res
    end


  and type_equiv (D,c1,c2) = (
			      equiv_depth := !equiv_depth +1;
			      (if !equiv_depth = 1 then subtimer ("Tchk:type_equiv",con_equiv) (D,c1,c2,Type_k)
			       else (type_equiv_count();
				     (con_equiv (D,c1,c2,Type_k))))
				 before (equiv_depth := !equiv_depth -1)
				 )

  (* Given a well-formed context D and well-formed constructors c1 and c2 of kind k,
     return whether they are equivalent at well-formed kind k. *)
  and con_equiv (args as (D,c1,c2,k)) : bool = 
    (!short_circuit andalso (alpha_equiv_con(c1,c2))) orelse
(*    (print "\nCon equiv called with arg1 = \n";
     Ppnil.pp_con c1;
     print "\n arg 2 = \n";
     Ppnil.pp_con c2;
     print "\nkind =\n";
     Ppnil.pp_kind k;
     let val res =*) con_equiv' args(*
     in (print "\nCon Equiv Done\n";res) end)*)
    
  and con_equiv' (D,c1,c2,k) : bool = 
    let

      val con_head_normalize = (*fn args => (print "\nEntering con_head_normalize, arg is:\n";
					   Ppnil.pp_con (#2 args);
					   let val res = *)time_con_head_normalize "Equiv" (*args
					   in (print "\nReturning\n";res) end)*)
      val con_reduce = subtimer ("Equiv:con_reduce",NilHNF.con_reduce)

      val _ = push_eqcon(c1,c2,D)
        val res = 
        case k of
	  Type_k => 
(*	    let
	      val c1 = con_reduce (D,c1)
	      val c2 = con_reduce (D,c2)
	    in
	      (case con_structural_equiv (D,c1,c2)
		 of NONE => 
		   let
		     val c1 = 
		       (case NilContext.find_kind_equation (D,c1)
			  of SOME c1 => con_head_normalize (D,c1)
			   | NONE => c1)
		     val c2 = 
		       (case NilContext.find_kind_equation (D,c2)
			  of SOME c2 => con_head_normalize (D,c2)
			   | NONE => c2)
		   in
		     (case con_structural_equiv(D,c1,c2) of
			NONE => false
		      | SOME Type_k => true
		      | SOME _ => error' "con_structural_equiv returned bad kind")
		   end
		  | SOME _ => true)
	    end
*)	 
	    let

	      val c1 = con_head_normalize(D,c1)
	      val c2 = con_head_normalize(D,c2)
	    in  
	      (case con_structural_equiv(D,c1,c2) of
		 NONE => false
	       | SOME Type_k => true
	       | SOME _ => error' "con_structural_equiv returned bad kind")
	    end

	| SingleType_k c => true
	| Single_k c => true
	| Record_k lvk_seq => 
	   let fun folder (((l,v),k),(D,equal)) =
	     let val equal = equal andalso
	       (* if they weren't alpha-equivalent before, 
		they won't be now; bypass the check *)
	       con_equiv'(D,Proj_c(c1,l),Proj_c(c2,l),k)
		 val D = insert_kind(D,v,Single_k(Proj_c(c1,l)))
	     in  (D,equal)
	     end
	       val (D,equal) = Sequence.foldl folder (D,true) lvk_seq
	   in  equal
	   end
	| Arrow_k (openness,vklist,k) => 
	   let val D = insert_kind_list(D,vklist)
	     val args = map (Var_c o #1) vklist
	     val c1 = App_c(c1,args)
	     val c2 = App_c(c2,args)
	   in  
	     (* if they weren't alpha-equivalent before, 
	      they won't be now; bypass the check *)
	     con_equiv'(D,c1,c2,k)  
	   end
	val _ = pop()
	val _ = if res then ()
	    else (print "con_equiv' returning false\n";
		  print "c1 = "; Ppnil.pp_con c1; print "\n";
		  print "c2 = "; Ppnil.pp_con c2; print "\n";
		  print "k = "; Ppnil.pp_kind k; print "\n";
		  print "context = "; NilContext.print_context D; print "\n";
		  print "\n")
    in  res
    end

  (* The substitution returned maps variables in the second list to the first. *)
  and vklist_equiv (D, vklist1, vklist2) : context * con_subst * bool =
      let 
	fun folder((v1,k1),(v2,k2),(D,rename,match)) = 
	  let 
	    val k2 = subtimer("Tchk:Equiv:substConInKind",substConInKind rename) k2
	    val match = match andalso subtimer("Tchk:Equiv:kind_equiv",kind_equiv)(D,k1,k2)
	    val rename = if (eq_var(v1,v2)) then rename
			 else Subst.C.sim_add rename (v2,Var_c v1)
	    val D = insert_kind(D,v1,k1)
	  in  (D,rename,match)
	  end
      in  
	if (length vklist1 = length vklist2)
	  then Listops.foldl2 folder (D,empty_subst(),true) (vklist1, vklist2)
	else (D, empty_subst(), false)
      end
    
  and vlistopt_clist_equiv (D, cRename, NONE, clist1, NONE, clist2) : context * (exp_subst * con_subst) * bool =
    (D, (Subst.E.empty(),cRename),
     Listops.eq_list(fn (c1,c2) => con_equiv(D,c1,subtimer("Tchk:Equiv:substConInCon",Subst.substConInCon cRename) c2,
					     Type_k), clist1, clist2))
    | vlistopt_clist_equiv (D, rename, NONE, clist1, vlistopt2, clist2) =
    vlistopt_clist_equiv (D, rename, vlistopt2, clist2, NONE, clist1)
    | vlistopt_clist_equiv (D, cRename, SOME vlist1, clist1, vlistopt2, clist2) =
    let 
      fun folder(v1,c1,vopt2,c2,(D,rename as (eRename,cRename),match)) = 
	let 
	  val c2 = subtimer("Tchk:Equiv:substExpConInCon",Subst.substExpConInCon rename) c2
	  val match = match andalso con_equiv(D,c1,c2,Type_k)
	  val rename = (case vopt2 of
			  NONE => rename
			| SOME v2 => if (eq_var(v1,v2)) then rename
				     else (Subst.E.sim_add eRename (v2,Var_e v1),cRename))
	  val D = NilContext.insert_con(D,v1,c1)
	in  (D,rename,match)
	end
      val vlist2 = (case vlistopt2 of
		      NONE => map (fn _ => NONE) clist2
		    | SOME vars => map (fn v => SOME v) vars)
      val vl1 = length vlist1
      val vl2 = length vlist2
    in  if (vl1 = vl2 andalso vl1 = length clist1 andalso vl2 = length clist2)
	  then Listops.foldl4 folder (D,(Subst.E.empty(),cRename),true)
	    (vlist1,clist1,vlist2,clist2)
	else (D, (Subst.E.empty(),cRename), false)
    end


  and con_structural_equiv (D,c1,c2) : kind option =
   let fun base true = SOME Type_k
	 | base false = NONE
       fun kind_type_tuple 1 = Type_k
	 | kind_type_tuple len = NilUtil.kind_tuple(Listops.map0count (fn _ => Type_k) len)
       val res =   
	(case (c1,c2) of
	     (Prim_c(Record_c (labs1,vlistopt1),clist1), 
	      (Prim_c(Record_c (labs2,vlistopt2),clist2))) =>
	     base (Listops.eq_list(eq_label,labs1,labs2) andalso
		   #3(vlistopt_clist_equiv(D,empty_subst(),
					 vlistopt1,clist1,vlistopt2,clist2)))
	    | (Prim_c(pcon1 as Sum_c{tagcount,totalcount,known},clist1), 
	       Prim_c(pcon2,clist2)) =>
	     let val carriers = TilWord32.uminus(totalcount,tagcount)
		 val k = kind_type_tuple (TilWord32.toInt carriers)
	     in  base(NilUtil.primequiv(pcon1,pcon2) andalso 
		  Listops.eq_list(fn (c1,c2) => con_equiv(D,c1,c2,k),
				  clist1,clist2))
	     end
	    | (Prim_c(pcon1,clist1), Prim_c(pcon2,clist2)) => 
	     base(NilUtil.primequiv(pcon1,pcon2) andalso 
		  Listops.eq_list(fn (c1,c2) => con_equiv(D,c1,c2,Type_k),
				  clist1,clist2))
	    | (Mu_c(ir1,defs1), Mu_c(ir2,defs2)) => 
		let val vc1 = Sequence.toList defs1
		    val vc2 = Sequence.toList defs2
		    fun build (D,rename) ((v1,_)::rest1,(v2,_)::rest2) = 
			let val D = NilContext.insert_kind(D,v1,Type_k)
			    val rename = if (eq_var(v1,v2))
					     then rename
					 else Subst.C.sim_add rename (v2,Var_c v1)
			in  build (D,rename) (rest1,rest2)
			end
		      | build acc _ = acc
		    val (D,rename) = build (D,Subst.C.empty()) (vc1,vc2)
		    fun pred ((_,c1),(_,c2)) = 
			    con_equiv(D,c1,subtimer("Tchk:Equiv:substConInCon",Subst.substConInCon rename) c2, Type_k)
		in  if ((ir1 = ir2) andalso Listops.eq_list(pred,vc1,vc2))
			then SOME(kind_type_tuple (length vc1))
		    else NONE
		end
	  | (AllArrow_c (openness1,effect1,vklist1,vlistopt1,clist1,nf1,c1),
		AllArrow_c (openness2,effect2,vklist2,vlistopt2,clist2,nf2,c2)) =>
		base(openness1 = openness1 andalso effect1 = effect2 
		     andalso nf1 = nf2 
		     andalso 
		     let val (D,cRename,match) = vklist_equiv(D,vklist1,vklist2)
		     in  match andalso
			 let val (D,rename,match) = 
			   vlistopt_clist_equiv(D,cRename,vlistopt1,clist1,
						vlistopt2,clist2)
			 in  match andalso
			   type_equiv(D,c1,subtimer("Tchk:Equiv:substExpConInCon",Subst.substExpConInCon rename) c2)
			 end
		     end)

	      | (ExternArrow_c (clist1,c1), ExternArrow_c (clist2,c2)) => 
		let val (D,_,match) = 
		    vlistopt_clist_equiv(D,Subst.C.empty(),NONE,clist1,
					 NONE,clist2)
		in  base(type_equiv(D,c1,c2))
		end

	      | (Crecord_c lclist, _) => 
		error' "Crecord given to con_structural_equiv: not WHNF"

	      | (Let_c _, _) => error' "Let_c given to con_structural_equiv: not WHNF"

	      | (Proj_c (c1,l1), Proj_c(c2,l2)) => 
		(case (eq_label(l1,l2),con_structural_equiv(D,c1,c2)) of
		   (true,SOME(Record_k lvk_seq)) =>
		     let   
			 fun loop _ [] = error' "con_structural_equiv: field not present"
			   | loop subst (((l,v),k)::rest) = 
			     if (eq_label(l,l1))
				 then SOME(subtimer("Tchk:Equiv:substConInKind",Subst.substConInKind subst) k)
			     else loop (Subst.C.sim_add subst (v,Proj_c(c1,l))) rest
		     in  loop (Subst.C.empty()) (Sequence.toList lvk_seq)
		     end
		 | _ => NONE)
	      | (App_c (f1,clist1), App_c (f2,clist2)) =>
		 (case con_structural_equiv(D,f1,f2) of
		    SOME(Arrow_k(openness,vklist,k)) =>
		      let     
			fun folder ((v,k),c1,c2, (D,subst,equal)) =
			  if equal then 
			      let val equal = con_equiv(D,c1,c2,k)
				  val D = NilContext.insert_kind(D,v,Single_k c1)
				  val subst = Subst.C.sim_add subst (v,c1)
			      in  (D,subst,equal)
			      end
			  else (D,subst,equal)
			val (D,subst,equal) = Listops.foldl3 folder (D,Subst.C.empty(),true) 
			                          (vklist,clist1,clist2)
		      in if equal then SOME(subtimer("Tchk:Equiv:substConInKind",NilSubst.substConInKind subst) k) 
			 else NONE
		      end
		  | _ => NONE)
	      | (Var_c v, Var_c v') => if (eq_var(v,v')) 
					   then SOME(NilContext.find_kind(D,v))
				       else NONE
	      | (Annotate_c _, _) => error' "Annotate_c not WHNF"
	      | (Typecase_c _, _) => error' "Typecase_c not handled"
	      | _ => NONE)
       val _ = (case res of
		  NONE => (print "con_structural_equiv returning NONE\n";
			   print "c1 = "; Ppnil.pp_con c1; print "\n";
			   print "c2 = "; Ppnil.pp_con c2; print "\n";
			   print "\n")
		| _ => ())
   in res
   end 
 
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
	   Prim_c (Array_c,[con])
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
	   val _ = con_analyze(D,sumcon,Type_k)
	     
	   val (tagcount,totalcount,carrier) = 
	     (case strip_sum (con_head_normalize(D,sumcon)) of
		SOME (tc,total,NONE,c) => (tc,total,c)
	      | SOME _ => (pp_con sumcon;
			   error (locate "prim_valid") "inject type argument has special sum type")
	      | NONE => (pp_con sumcon;
			 error (locate "prim_valid") "inject given invalid type argument"))
		
	   val _ = 
	     (case exps
		of [] =>
		  if (sumtype < tagcount) then () else
		    (perr_e (Prim_e (NilPrimOp prim,[carrier],[]));
		     (error (locate "prim_valid") "Illegal injection - sumtype out of range" ))
		 | [argexp] => 
		    let
		      val nontagcount = TilWord32.toInt(TilWord32.uminus(totalcount,tagcount))
		      val which = TilWord32.toInt(TilWord32.uminus(sumtype,tagcount))
		      val con_k = 
			if (nontagcount < 1) then
			  (error (locate "prim_valid") "Illegal injection - no non value fields!")
			else if (which < 0) then
			  (error (locate "prim_valid") "Injecting value into non tag field")
			else if (nontagcount = 1) andalso (which = 0) then
			  carrier
			else 
			  Proj_c(carrier,NilUtil.generate_tuple_label (which + 1))
		    in
		      exp_analyze(D,argexp,con_k)
		    end
		 | _ => 
		    (perr_e (Prim_e (NilPrimOp prim,cons,exps));
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
		  ignore ((sumtype < tagcount) orelse
			  (perr_e (Prim_e (NilPrimOp prim,[carrier],[]));
			   (error (locate "prim_valid") "Illegal injection - sumtype out of range" )))
	      | [argexp] =>    
		  let 
		    val nontagcount = TilWord32.toInt(TilWord32.uminus(totalcount,tagcount))
		    val which = TilWord32.toInt(TilWord32.uminus(sumtype,tagcount))
		    val con_k = 
		      if (nontagcount < 1) then
			(error (locate "prim_valid") "Illegal injection - no non value fields!")
		      else if (which < 0) then
			(error (locate "prim_valid") "Injecting value into non tag field")
		      else if (nontagcount = 1) andalso (which = 0) then
			carrier
		      else 
			Proj_c(carrier,NilUtil.generate_tuple_label (which + 1))
		  in (
		      (case strip_record (con_head_normalize(D,con_k))
			 of SOME _ => error (locate "prim_valid") "inject_nonrecord injects into record field"
			  | NONE => ());
		      exp_analyze (D,argexp,con_k)
		      )
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

	   val nontagcount = TilWord32.toInt(TilWord32.uminus(totalcount,tagcount))
	   val which = TilWord32.toInt(TilWord32.uminus(sumtype,tagcount))

	   val expcons = map (curry2 exp_valid D) exps

	   val cons = 
	     if (nontagcount < 0) then
	       (perr_e (Prim_e (NilPrimOp prim,cons,exps));
		(error (locate "prim_valid") "Illegal injection - no non value fields!" handle e => raise e))
	     else if (which < 0) then
	       (error (locate "prim_valid") "Injecting value into non tag field")
	     else if (nontagcount = 1) andalso (which = 0) then
	       [carrier]
	     else 
	       map (fn i => Proj_c(carrier,NilUtil.generate_tuple_label i)) (count1 nontagcount)
	   
	   val con_k = List.nth (cons,which)

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
	   val _ = 
	     (c_all2 (fn (c1,c2) => type_equiv(D,c1,c2)) (o_perr_c_c "Length mismatch in record") (expcons,cons)) orelse
	     (error (locate "prim_valid") "Illegal record injection - type mismatch in args" handle e => raise e)
	       
	   val con = Prim_c (Sum_c {tagcount=tagcount,totalcount=totalcount,known=NONE},[carrier]) (*can't propogate field*)
	 in
	   con
	 end
	| (project_sum k,[argcon],[argexp]) => 
	 let
	   val argkind = con_valid(D,argcon)
	   val argcon = con_head_normalize(D,argcon)
	   val argcon' = convert_sum_to_special(argcon,k)
	   val argcon'' = exp_valid (D,argexp)
	   val _ = 
	     (type_equiv(D,argcon',argcon'')) orelse
	     (error (locate "prim_valid") "Type mismatch in project_sum")
	   val con_k = projectSumType(D,argcon,k)
	 in
	   con_k
	 end
	| (project_sum_nonrecord k,[argcon],[argexp]) => 
	 let
	   val argkind = con_valid(D,argcon)
	   val argcon = con_head_normalize(D,argcon)
	   val argcon' = convert_sum_to_special(argcon,k)
	   val argcon'' = exp_valid (D,argexp)
	   val _ = 
	     (type_equiv(D,argcon',argcon'')) orelse
	     (error (locate "prim_valid") "Type mismatch in project_sum_nonrecord")
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
	   val argkind = con_valid(D,argcon)
	   val argcon = con_head_normalize(D,argcon)
	   val argcon' = convert_sum_to_special(argcon,k)
	   val argcon'' = exp_valid (D,argexp)
	   val _ = 
	     (type_equiv(D,argcon',argcon'')) orelse
	     (error (locate "prm_valid") "Type mismatch in project_sum_record")
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

       
       
  and bnds_valid (D,bnds) = (foldl bnd_valid' (D,Subst.C.empty()) bnds)
  and bnd_valid (state,bnd) = bnd_valid' (bnd,state)
  and bnd_valid'' (bnd,state) = 
    let
      val (D,subst) = state
      val con_valid = subtimer ("Tchk:Bnd:con_valid",con_valid)
      val cbnd_valid = subtimer ("Tchk:Bnd:cbnd_valid",cbnd_valid)
      val substConInCon = fn subst => subtimer("Tchk:Bnd:substConInCon",substConInCon subst)
      val add = fn subst => subtimer("Tchk:Subst.add in bnd_valid",Subst.C.sim_add subst)
      val addr = subtimer("Tchk:Subst.addr in bnd_valid",Subst.C.addr)

(*      val bnd = substConInBnd subst bnd *)

      fun function_valid openness D (var,Function (effect,recursive,tformals,dependent,
						   formals,fformals,body,return)) = 
	let
	  val _ = if (!trace)
			then (print "{Processing function_valid with var = ";
			      pp_var var; print "\n")
		    else ()
	  val D' = foldl (fn ((v,k),D) => (kind_valid(D,k);insert_kind(D,v,k))) D tformals
	  val D = foldl (fn ((v,c),D) => (con_valid(D,c);insert_con(D,v,c))) D' formals
	  val D = 
	    (foldl (fn (v,D) => 
		    insert_con (D,v,Prim_c (Float_c F64,[]))) D fformals)
	  val _ = if (!trace)
			then (print "}{Processing body in function_valid with var = ";
			      pp_var var; print "\n")
		    else ()
	  val body_c = exp_valid (D,body)
	  val D = if dependent then D else D'
	  val _ = con_valid (D,return)
	  val _ = if (!trace)
			then (print "}{Processing body type against given type in function_valid with var = ";
			      pp_var var; print "\n")
		    else ()
	  val _ = 
	    (type_equiv (D,body_c,return)) orelse
	    (perr_e_c_c (body,return,body_c);
	     (error (locate "function_valid") "Return expression has wrong type" handle e => raise e))    

	  val (vars_opt,cons) = let val (vars,cons) = unzip formals
				in if dependent then (SOME vars,cons) else (NONE,cons) 
				end
	  val numfloats = Word32.fromInt (List.length fformals) 
	  val con = AllArrow_c(openness,effect,tformals,vars_opt,cons,numfloats,body_c)
	  val _ = if (!trace)
			then (print "Done processing function_valid with var = ";
			      pp_var var; print "}\n")
		    else ()
	in
	  (var,con)
	end

      fun fbnd_valid (openness,defs) = 
	let
	  val origD = D
	  val _ = if (!trace)
			then (print "{Processing fbnd_valid \n")
		    else ()
	  val bnd_types = Sequence.map_second (function_type openness) defs
(***
	  val _ = (print "bnd_types are: ";
		   Sequence.app (fn (_,c) => (Ppnil.pp_con c; print "\n")) bnd_types;
		   print "\n")
****)
	  val _ = Sequence.map_second (curry2 con_valid D) bnd_types
	  val D = Sequence.foldl (fn ((v,c),D) => insert_con(D,v,c)) D bnd_types
	  val _ = if (!trace)
		      then (print "Processing fbnd_valid done with making context}\n")
		    else ()
	  val found_types = Sequence.map (function_valid openness D) defs
	  fun checker ((_,c),(_,c')) = 
	    (type_equiv(origD,c,c')) orelse 
	    (error (locate "bnd_valid") "Declared function type does not match found type")
	  val _ = Sequence.all2 checker (bnd_types,found_types)
	in (D,subst)
	end

(*      val fbnd_valid = subtimer ("Tchk:Bnd:fbnd_valid",fbnd_valid)*)

    in
      (case bnd 
	 of Con_b (phase, cbnd) => 
	   let
	     val D = cbnd_valid (cbnd,D)
	     val (v,c) = 
	       (case cbnd of
		  Con_cb (v,c) => (v,c)
		| Open_cb(v,vklist,c,k) => (v,Let_c(Sequential,[cbnd],Var_c v))
		| Code_cb(v,vklist,c,k) => (v,Let_c(Sequential,[cbnd],Var_c v)))
	     val subst = addr (subst,v,c)
	   in  (D,subst)
	   end
	  | Exp_b (var, tracinfo,exp) =>
	   let
	     val _ = if (!trace)
			 then (print "{Processing Exp_b with var = ";
			       pp_var var; print "\n")
		     else ()
	     val bnd_con = exp_valid (D,exp)
	     val D = insert_con (D,var,bnd_con)
	     val _ = if (!trace)
			 then (print "Done processing Exp_b with var = ";
			       pp_var var; print "}\n")
		     else ()
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
		   (find_std_con (D,code)
		    handle NilContext.Unbound =>
		      (printl ("Code pointer "^(var2string code));
		       print " not defined in context";
		       (error (locate "bnd_valid") "Invalid closure" handle e => raise e)))
		 val (effect,tformals,vars_opt,formals,numfloats,body_c) = 
		   (case strip_arrow code_type of
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
		   (sub_kind (D,Single_k(cenv),last_k) andalso type_equiv (D',vcon,last_c)) orelse
		   (perr_k_k (last_k,ckind);
		    perr_c_c (last_c,vcon);
		    (error (locate "bnd_valid") "Mismatch in closure" handle e => raise e))

		 val closure_vars_opt = 
		     (case vars_opt of
			  NONE => NONE
			| SOME vars => SOME (Listops.butlast vars))
		 val closure_type = AllArrow_c (Closure,effect,tformals,
						closure_vars_opt,
						formals,numfloats,body_c)

		 val _ = con_valid(D,tipe)
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
  and exp_analyze(D : context, exp : exp, con : con) : unit = 
    let
      val con' = exp_valid(D,exp)
    in
       ignore (type_equiv(D,con,con') orelse 
	       (perr_c_c (con,con');
		error (locate "exp_analyze") "Expression cannot be given type"))
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
	     (* assertRenamedCon (D,res) *)
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
(*	   val exp = substConInExp subst exp *)
	   val con = exp_valid (D,exp)
	 in substConInCon subst con
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
	       let
		   val con' = con_head_normalize(D,con)

		   (* We rename the bound variables in the function type
		      because they may be added to the context below.  
		      If this is a recursive function call, they may
		      already be in the context
		    *)
		   val con'' = NilRename.renameCon con'
	       in
		   case strip_arrow con'' of
		       SOME c => c
		     | NONE => (perr_e_c (app,con);
				(error (locate "exp_valid") 
				 "Application of non-arrow expression" 
				 handle e => raise e))
	       end
		
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
	     (Ppnil.pp_list pp_con' formals ("\nFormal Types: (",", ",")",false);
	      Ppnil.pp_list pp_con' t_cons ("\nActual Types: (",", ",")",false);
	      Ppnil.pp_list pp_exp' texps ("\nActuals: (",", ",")\n",false);
	      perr_e exp;
	      error (locate "exp_valid") "Formal/actual parameter mismatch")

	   val subst = Subst.C.simFromList (zip (#1 (unzip tformals)) cons)
	   val formals = map (Subst.substConInCon subst) formals
	     

	   val D = 
	     (case vars_opt
		of SOME vars => 
		  let
		    fun check_one_dep (var,actual_con,formal_con,D) = 
		      if (type_equiv (D,actual_con,formal_con)) then insert_con (D,var,actual_con) 
		      else (print_error ())
		  in
		    (foldl3 check_one_dep D (vars,t_cons,formals))
		    handle e => (Ppnil.pp_list (pp_exp' o Var_e) vars ("\nVars: (",", ",")\n", false);
				 print_error())
		  end
		 | NONE => 
		  let
		    fun check_one_con (actual_con,formal_con) = 
		      (type_equiv (D,actual_con,formal_con)) orelse (print_error ())
		    val _ = if (all2 check_one_con (t_cons,formals)) then ()
			    else print_error()
			    
		  in
		    D
		  end)
		
	   val f_cons = map (curry2 exp_valid D) fexps
	   val f_cons = map (curry2 con_head_normalize D) f_cons

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
		 of SOME vars => substExpInCon (Subst.E.simFromList (zip vars texps)) body
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
	     
	   fun print_error () =
	     (Ppnil.pp_list pp_con' formals ("\nFormal Types: (",", ",")\n",false);
	      Ppnil.pp_list pp_con' argcons ("\nActual Types: (",", ",")\n",false);
	      Ppnil.pp_list pp_exp' args ("\nActuals: (",", ",")\n",false);
	      perr_e exp;
	      error (locate "exp_valid") "Formal/actual parameter mismatch in external app")

	   fun check_one_con (actual_con,formal_con) = 
	     (type_equiv (D,actual_con,formal_con)) orelse 
	     (print_error ())
	     
	   val _ = if (all2 check_one_con (argcons,formals)) then ()
		   else print_error ()
	 in
	   return
	 end
       
       
     | Raise_e (exp,con) => 
	 let
	   val _ = con_valid (D,con)
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
      val exp_valid = subtimer ("Tchk:exp_valid",exp_valid)
      val con_valid = wrap "con_valid" con_valid
      val con_valid = subtimer ("Tchk:con_valid",con_valid)
      val kind_valid = wrap "kind_valid" kind_valid
      val kind_valid = subtimer ("Tchk:kind_valid",kind_valid)
      val bnds_valid = subtimer ("Tchk:bnds_valid",bnds_valid)


      fun import_valid' (ImportValue (label,var,con),D) =
	let
	    val _ = if (!trace)
			then (print "{Processing ImportValue with var = ";
			      pp_var var; print "\n")
		    else ()
	  val kind = con_valid(D,con)
	  val D = insert_con(D,var,con)
	    val _ = if (!trace)
			then (print "Done processing ImportValue with var = ";
			      pp_var var; print "}\n")
		    else ()
	in
	  D
	end
	| import_valid' (ImportType (label,var,kind),D) = 
	let
	    val _ = if (!trace)
			then (print "{Processing ImportType with var = ";
			      pp_var var; print "\n")
		    else ()
	  val _ = kind_valid (D,kind)
	  val D = insert_kind(D,var,kind)
	    val _ = if (!trace)
			then (print "Done processing ImportType with var = ";
			      pp_var var; print "}\n")
		    else ()
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
	  val (D,_) = bnds_valid(D,bnds)
	  val _ = app (export_valid D) exports
	in
	  ()
	end

      fun module_valid (D,module) = 
	let 
	  val _ = push_mod(module,D)

	  val _ = if (!show_calls)
		    then (print "module_valid called with module =\n";
                          Ppnil.pp_module 
                                        {module = module,
                                         header = "",
                                         name = "",
                                         pass = ""};
			  if !show_context then (print "\nand context"; NilContext.print_context D) else ();
			  print "\n\n")
		  else ()
	  val res = module_valid'(D,module)

	  val _ = if (!show_calls)
		    then (printl "module_valid returned")
		  else ()

	  val _ = pop()
	in  res
	end
    
      val module_valid = wrap "module_valid" module_valid
      val module_valid = timer ("Module_valid", module_valid)

end
