functor NilStaticFn(structure Annotation : ANNOTATION
		    structure Prim : PRIM
		    structure Nil : NIL
		    structure NilUtil : NILUTIL
		    structure Cont : CONT
		    sharing NilUtil.Nil = Nil
		    and Annotation = Nil.Annotation
		    and Prim = Nil.Prim) :> 
  sig include NILSTATIC sharing Nil = Nil end = 
struct	
    
  structure Annotation = Annotation
     structure Prim = Prim
     structure Nil = Nil
     open Nil

     (* Local rebindings from imported structures *)

     val lookup = HashTable.lookup
     val find = HashTable.find
     val alpha_equiv_con = NilUtil.alpha_equiv_con
     val alpha_equiv_kind = NilUtil.alpha_equiv_kind
     val alpha_sub_kind = NilUtil.alpha_sub_kind
     val same_openness = NilUtil.same_openness
     val eq_var = Name.eq_var
     val eq_label = Name.eq_label
     val c_foldl = Cont.c_foldl
     val c_insert = Cont.c_insert
     val c_insert_list = Cont.c_insert_list
     val c_fold_acc = Cont.c_fold_acc

     (* Local helpers *)

     type context = (Name.var,kind) HashTable.hash_table
     exception undef_var
     fun empty_context () = Name.mk_var_hash_table (1000,undef_var)

     fun eq_var2 var1 var2 = eq_var (var1,var2)

     fun first (fst,_) = fst

     fun eq_var_pair var = (eq_var2 var) o first

     fun list2cmap list = 
       (fn var => 
	case List.find (eq_var_pair var) list
	  of SOME (v,c) => SOME c
	   | NONE => NONE)

     fun sorted_unique p ([] | [_]) = true
       | sorted_unique p (fst::snd::rest) = 
       (p (fst,snd)) andalso sorted_unique p (snd::rest)

     fun gt_label (l1,l2) = 
       (case Name.compare_label (l1,l2)
	  of GREATER => true
	   | _ => false)

     fun error s = Util.error "reduce.sml" s

     fun subst_fn var1 con = 
       (fn var2 => 
	(if eq_var (var1,var2) then 
	   SOME con
	 else
	   NONE))

     fun varConKindSubst var con = NilUtil.substConInKind (subst_fn var con)

     fun varConConSubst var con = NilUtil.substConInCon (subst_fn var con)

       

     fun primKind (Int_c Prim.W64) = Type_k
       | primKind (Int_c Prim.W32) = Word_k
       | primKind (Int_c Prim.W16) = Word_k
       | primKind (Int_c Prim.W8) = Word_k
       | primKind (Float_c Prim.F64) = Type_k
       | primKind (Float_c Prim.F32) = Word_k
       | primKind (BoxFloat_c Prim.F64) = Word_k
       | primKind (BoxFloat_c Prim.F32) = Word_k
       | primKind (Exn_c) = Word_k                 
       | primKind (Array_c) = Word_k
       | primKind (Vector_c) = Word_k
       | primKind (Ref_c) = Word_k
       | primKind (Exntag_c) = Word_k
       | primKind (Sum_c _) = Type_k
       | primKind (Record_c _) = Word_k
       | primKind (Vararg_c _) = Word_k


     fun do_beta_fun (Fun_c (openness,formals,body),actuals) = 
       let
	 fun subst_one ((var,_),actual,body) = 
	   varConConSubst var actual body
       in
	 SOME (ListPair.foldr subst_one body (formals,actuals))
       end
       | do_beta_fun (Annotate_c (an,con),actuals) = 
       (case do_beta_fun (con,actuals)
	  of SOME con => SOME (Annotate_c (an,con))
	   | NONE => NONE)
       | do_beta_fun cfun = NONE


     fun do_eta_fun (openness,formals,body as App_c(con,actuals)) = 
       let
	 val (formal_vars,_) = ListPair.unzip formals
	 fun eq (var1,(Var_c var2)) = eq_var(var1,var2)
	   | eq (formal,Annotate_c (an,con)) = eq (formal,con)
	   | eq _ = false
	 val rule_applies = 
	   (ListPair.all eq (formal_vars,actuals)) andalso 
	   not (List.exists (fn v => NilUtil.convar_occurs_free (v,con)) 
		formal_vars)
       in
	 if rule_applies then
	   SOME con
	 else
	   NONE
       end
       | do_eta_fun (openness,formals,Annotate_c (an,con)) = 
       (case do_eta_fun(openness,formals,con)
	  of SOME con => SOME (Annotate_c (an,con))
	   | NONE => NONE)
       | do_eta_fun (openness,formals,body) = NONE
	  
     (*PRE: elements are in head normal form*)
     fun do_eta_record [] = NONE
       | do_eta_record (entries as (label,con)::rest) = 
       let
	 fun strip_proj (Proj_c (con,label)) = SOME (con,label)
	   | strip_proj (Annotate_c (an,con)) = strip_proj con
	   | strip_proj _ = NONE
	   
	 (*Can get away with constructor alpha equivalence*)
	 (*If is in head normal form *)
	 fun eq con (label,Proj_c (con2,label2)) = 
	   (eq_label (label,label2)) andalso 
	   (alpha_equiv_con (con,con2))
	   | eq con (label,Annotate_c (an,con2)) = eq con (label,con2)
	   | eq _ _ = false
	   
	 val entry1 = strip_proj con
       in
	 case entry1 
	   of SOME (c,l) => 
	     if ((eq_label (l,label)) 
		 andalso	List.all (eq c) rest) then
	       SOME c
	     else 
	       NONE
	    | NONE => NONE
       end
     
     fun do_beta_record (c_rec as Crecord_c entries,label) =
       (case (List.find (fn ((l,_)) => eq_label (l,label))
	      entries )
	  of SOME (l,c) => SOME c
	   | NONE => error "Field not in record")
       | do_beta_record (Annotate_c (an,con),label) = 
	  (case do_beta_record (con,label)
	     of SOME con => SOME (Annotate_c (an,con))
	      | NONE => NONE)
       | do_beta_record (other,label) = NONE
	     
     fun fold_kinds (D,kinds,cont) = 
       let
	 fun step ((var,kind),(D,kmap),k) = 
	   let
	     val kind = kindSubstReduce (D,kmap,kind)
	   in
	     c_insert (D,(var,kind),fn C => k (D,(var,kind)::kmap))
	   end
       in
	 c_foldl cont step (D,[]) kinds
       end  

     and kind_valid (D,kind) = 
       (case kind 
	  of Type_k => Type_k
	   | Word_k => Word_k
	   | Singleton_k (kind,con) => 
	    let
	      val (con,kind) = (con_valid (D,con))
	    in
	      if alpha_sub_kind (kind,Type_k) then
		(Singleton_k (kind,con))
	      else
		error "Invalid singleton kind"
	    end
	   | Record_k elts => 
	    let
	      val elt_list = Util.sequence2list elts
	      val vars_and_kinds = List.map (fn ((l,v),k) => (v,k)) elt_list

	      fun base (D,elts) = 
		let
		  val entries = 
		    ListPair.map (fn (((l,_),_),(v,k)) => ((l,v),k)) 
		    (elt_list,vars_and_kinds)
		in
		  (Record_k (Util.list2sequence entries))
		end
	    in
	      fold_kinds (D,vars_and_kinds,base)
	    end
	   | Arrow_k (openness, formals, return) => 
	    let
	      fun base (D,kmap) = 
		let
		  val return' = kindSubstReduce (D,kmap,return)
		in
		  (Arrow_k (openness, kmap,return'))
		end
	      
	    in
	      fold_kinds (D,formals,base)
	    end  
	   | Code_k (formals,return) => 
	    let
	      fun base (D,kmap) = 
		let
		  val return' = kindSubstReduce (D,kmap,return)
		in
		  (Code_k (kmap,return'))
		end
	      
	    in
	      fold_kinds (D,formals,base)
	    end)

     (*Test Constructor validity 
      * con_valid con C => (con',k)
      * PRE: All kinds in D are canonical 
      * POST: con' is in beta-eta-normal form, k is most precise kind
      *)
     and con_valid (D : context, constructor : con) = 
       (case constructor 
	  of (Prim_c (pcon,args)) =>
	    let
	      val kind = primKind pcon
	      val (args',kinds) = 
		ListPair.unzip (List.map (fn x => (con_valid (D,x))) args)
	      val pcon' = 
		(case pcon
		   of (Record_c labels) => 
		     Record_c (ListMergeSort.sort gt_label labels)
		    | _ => pcon)
	      val con = (Prim_c (pcon',args'))
	    in
	      (*ASSERT*)
	      if List.all (fn T => alpha_sub_kind (T,Type_k)) kinds then
		(con,Singleton_k (kind,con))
	      else
		error "Invalid argument to primcon"
	    end
	   | (Mu_c (defs,var)) =>
	    let
	      (* Assumes that set implementation guarantees entries are 
	       * all distinct - i.e., no duplicates
	       *)
	      val def_list = Util.set2list defs
	      val var_kinds = List.map (fn (var,con) => (var,Word_k)) def_list
		
	      fun check_one D ((var,con),(cons,kinds)) =
		let
		  val (con,kind) = (con_valid (D,con))
		in
		  ((var,con)::cons,kind::kinds)
		end

	      fun cont D = (List.foldr (check_one D) ([],[]) def_list)
	      val (cons,kinds) = c_insert_list (D,var_kinds,cont)
	    in
	      (*ASSERT*)
	      if List.all (fn T => alpha_equiv_kind (T,Word_k)) kinds then
		(Mu_c (Util.list2set cons,var),Word_k)
	      else
		error "Invalid kind for recursive constructor"
	    end
	   | (Arrow_c (openness,confun)) => 
	    let
	      val confun = (case openness
			      of Open => arrow_valid (Type_k) confun D
			       | Closed => arrow_valid (Word_k) confun D)
	      val con = (Arrow_c (openness,confun))
	    in
	      (con,Singleton_k(Word_k,con))
	    end
	   | (Code_c confun) => 
	    let
	      val confun = arrow_valid (Word_k) confun D
	      val con = (Code_c confun)
	    in
	      (con,Singleton_k(Word_k,con))
	    end
	   | (v as (Var_c var)) => 
	    let
	      val kind = 
		(lookup D var 
		 handle undef_var => 
		   error "Encountered undefined variable in con_valid")
	    (*     val kind' = kind_valid kind *)
	    in
	      (v,kind)
	    end
	   | (Let_c (sort, binds,con)) => 
	    let 
	      fun base (D,binds) = 
		let
		  val (con',kind) = con_valid (D,con)
		  val cmap = list2cmap binds
		in
		  (NilUtil.substConInCon cmap con,
		   NilUtil.substConInKind cmap kind)
		end
	      fun step ((var,con),(D,binds),cont) = 
		let 
		  val (con',kind) = con_valid (D,con)
		  val con'' = NilUtil.substConInCon (list2cmap binds) con'
		  val binds' = (var,con'')::binds
		  val continue = fn D => cont (D,binds')
		in
		  c_insert (D,(var,kind),continue)
		end
	    in
	      c_foldl base step (D,[]) binds
	    end  
	   | (Fun_c (openness,formals,body)) => 
	    let
	      fun base (D,formals) = 
		let
		  fun substSingleton ((var,Singleton_k(_,scon)),con) =
		    varConConSubst var scon con
		    | substSingleton (_,con) = con
		    
		  val body = List.foldr substSingleton body formals
		  val (body,body_kind) = con_valid (D,body)
		  val kind = 
		    case openness
		      of Open => Arrow_k(Open,formals,body_kind)
		       | Close => Code_k(formals,body_kind)
		  val con = 
		    (*Assumes that kind is invariant under eta *)
		    (* reduction. True? Remember, open vs closed, etc*)
		    case (do_eta_fun (openness,formals,body))
		      of SOME con => con
		       | NONE => (Fun_c (openness,formals,body))
		in
		  (con,kind)
		end
	      
	      fun step ((var,kind),(D,kmap),k) = 
		let
		  val kind = kindSubstReduce (D,kmap,kind)
		in
		  c_insert (D,(var,kind),fn D => k (D,(var,kind)::kmap))
		end
	    in
	      c_foldl base step (D,[]) formals
	    end  
	   | (Closure_c (code,env)) => 
	    let
	      val (env,env_kind) = con_valid (D,env)
	    in
	      case (con_valid (D,code))
		of (code,Code_k((v,k1)::rest,body_kind)) => 
		  if alpha_equiv_kind (k1,env_kind) then
		    (Closure_c (code,env), Arrow_k(Closed,rest,body_kind))
		  else
		    error "Invalid kind for closure environment"
		 | _ => error "Invalid kind for closure constructor"
	    end

	   (* Sort records.  Useful later?*)
	   | (Crecord_c entries) => 
	    let
	      val dummy_var = Name.fresh_var ();
		
	      fun base (D,entry_info) =
		let
		  val (entries,entry_kinds) = ListPair.unzip entry_info
		  val kind = Record_k (Util.list2sequence entry_kinds)
		in
		  case (do_eta_record entries)
		    of SOME c => (c,kind)
		     | NONE => (Crecord_c entries,kind)
		end
	      
	      fun split (D,(label,con)) = 
		let
		  val (con,kind) = con_valid (D,con)
		in
		  (((label,con),((label,dummy_var),kind)),(dummy_var,kind))
		end
	      fun entry_sort ((l1,_),(l2,_)) = gt_label(l1,l2)
	      val entries' = ListMergeSort.sort entry_sort entries
	      fun entry_eq ((l1,_),(l2,_)) = eq_label(l1,l2)
	      val distinct = sorted_unique entry_eq entries'
	    in
	      if distinct then
		c_fold_acc base split D entries'
	      else
		error "Labels in record of constructors are not distinct"
	    end
	   | (Proj_c (rvals,label)) => 
	    let
	      val (rvals,record_kind) = con_valid (D,rvals)

	      val entry_kinds = 
		(case record_kind
		   of Record_k kinds => Util.sequence2list kinds
		    | _ => error "Unexpected kind returned from con_valid")
		   
	      fun propogate [] = error "Label not found in record kind"
		| propogate (((label2,var),kind)::rest) = 
		if eq_label (label,label2) then
		  kind
		else
		  (varConKindSubst var (Proj_c (rvals,label2)) (propogate rest))
	      val con = 
		case (do_beta_record (rvals,label))
		  of SOME c => c
		   | NONE => (Proj_c (rvals,label))
	      val kind = propogate entry_kinds
	    in
	      (con,kind)
	    end
	   | (App_c (cfun,actuals)) => 
	    let
	      val (cfun,cfun_kind) = con_valid (D,cfun)
	      val (formals,body_kind) = 
		case cfun_kind 
		  of (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		   | (Code_k (formals,body_kind)) => (formals,body_kind)
		   | _ => error "Invalid kind for constructor application"

	      val (actuals) = List.map (fn c => #1(con_valid (D,c))) actuals

	      val lookup = 
		let
		  fun find ([],[]) var = NONE
		    | find ((formal,_)::frest,actual::arest) var =
		    if eq_var (var,formal) then 
		      SOME actual
		    else
		      find (frest,arest) var
		    | find _ _ = 
		      error "Mismatch between formal and actual params"
		in
		  find (formals,actuals)
		end
	    in
	      case (do_beta_fun (cfun,actuals))
		of SOME c => con_valid (D,c)
		 | NONE => (App_c (cfun,actuals),
			    NilUtil.substConInKind lookup body_kind)
	    end
	   | (Annotate_c (annot,con)) => 
	    let
	      val (con,kind) = con_valid (D,con)
	    in
	      (Annotate_c (annot,con),kind)
	    end)

     (*Assuming tformals are not dependent*)
     and	arrow_valid comp_kind (effect,tformals,formals,return) D = 
       let 
	 fun cont D = 
	   let
	     val (return,kind) = con_valid (D,return)
	     val (formals,kinds) = 
	       ListPair.unzip (List.map (fn c => con_valid (D,c)) formals)
	   in
	     (*ASSERT*)
	     if (List.all (fn T => alpha_equiv_kind (T,comp_kind)) kinds)
	       andalso (alpha_equiv_kind (kind,comp_kind)) then
	       (effect,tformals,formals,return)
	     else
	       error "Invalid arrow constructor"
	   end
       in
	 c_insert_list (D,tformals,cont)
       end

     and kindSubstReduce (D,kmap,k) = 
       let
	 fun pull (c,kind) = 
	   (case kind
	      of Type_k => c
	       | Word_k => c
	       | Singleton_k (k,c2) => c2
	       | Record_k elts => 
		let
		  val entries = 
		    Util.mapsequence 
		    (fn ((label,var),kind) => 
		     (label,pull (Proj_c (c,label),kind))) elts
		in
		  (Crecord_c entries)
		end
	       | Arrow_k (openness, formals, return) => 
		let
		  val vars = List.map (fn (v,_) => (Var_c v)) formals
		  val c = pull (App_c (c,vars),return)
		in
		  (*Closures?  *)
		  case openness
		    of Open => Fun_c (Open,formals,c)
		     | Closed => 
		      error "Got closure in pullout - I thought that couldn't happen"
		end
	       | Code_k (formals, return) => 
		let
		  val vars = List.map (fn (v,_) => (Var_c v)) formals
		  val c = pull (App_c (c,vars),return)
		in
		  Fun_c (Closed,formals,c)
		end)
	      
	 fun cmap var = 
	   (case List.find (eq_var_pair var) kmap
	      of SOME (v,k) => SOME (pull (Var_c var,k))
	       | NONE => NONE)

	 val k = NilUtil.substConInKind cmap k
       in
	 kind_valid (D,k)
       end
     
     fun equiv_kind (D,k1,k2) = 
       let
	 val k1 = kind_valid (D,k1)
	 val k2 = kind_valid (D,k2)
       in
	 alpha_equiv_kind (k1,k2)
       end

     fun equiv_con (D,c1,c2) = 
       let
	 val (c1,_) = con_valid (D,c1)
	 val (c2,_) = con_valid (D,c2)
       in
	 alpha_equiv_con (c1,c2)
       end

     val reduce_kind = kind_valid

     fun reduce_con (D,con) = 
       let
	 val (con,kind) = con_valid (D,con)
       in
	 con
       end

     (* sub_kind (k1,k2) C
      * PRE:  k1,k2 are well-formed and canonical 
      *)
     fun sub_kind (D,k1,k2) = 
       let
	 val k1' = reduce_kind (D,k1)
	 val k2' = reduce_kind (D,k2)
       in
	 alpha_sub_kind (k1,k2)
       end

   end
