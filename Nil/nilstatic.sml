functor NilStaticFn(structure Annotation : ANNOTATION
		    structure Prim : PRIM
		    structure ArgNil : NIL
		    structure PpNil : PPNIL
		    structure Alpha : ALPHA
		    structure NilUtil : NILUTIL 
		    structure NilContext : NILCONTEXT
		    sharing NilUtil.Nil = NilContext.Nil = Alpha.Nil = PpNil.Nil = ArgNil
		    and Annotation = ArgNil.Annotation
		    and Prim = ArgNil.Prim
		    and type NilUtil.alpha_context = Alpha.alpha_context) :> 
  sig include NILSTATIC sharing Nil = ArgNil and type context = NilContext.context end = 
struct	
  
  structure Annotation = Annotation
  structure Prim = Prim
  structure Nil = ArgNil
  open Nil Prim

  val debug = ref false
    
  (* Local rebindings from imported structures *)

  local
    structure C :>
      sig 
	type context = NilContext.context
	val empty : unit -> context
	val insert_con : context*var*con -> context
	val find_con : context*var -> con option
	val remove_con : context*var -> context
	val insert_kind : context*var*kind -> context
	val find_kind : context*var -> kind option
	val remove_kind : context*var -> context
	val c_insert_con : context*var*con*(context->'a) -> 'a
	val c_remove_con : context*var*(context -> 'a) -> 'a
	val c_insert_kind : context*var*kind*(context->'a) -> 'a
	val c_remove_kind : context*var*(context -> 'a) -> 'a
	val c_insert_con_list : context * (var*con) list * (context -> 'a) -> 'a
	val c_insert_kind_list : context * (var*kind) list * (context -> 'a) -> 'a
	val c_foldl : ('state -> 'a) -> ('elt * 'state * ('state -> 'a) -> 'a) 
	  -> 'state -> 'elt list -> 'a
	val c_fold_con_acc : 
	  ((context * 'acc_elt list) -> 'a)
	  -> ((context * 'elt) -> ('acc_elt * (var * con)))
	  -> context -> 'elt list -> 'a
	val c_fold_kind_acc : 
	  ((context * 'acc_elt list) -> 'a)
	  -> ((context * 'elt) -> ('acc_elt * (var * kind)))
	  -> context -> 'elt list -> 'a
      end = NilContext

    structure NU :> 
      sig 
	val substConInExp : (Nil.var -> Nil.con option) -> Nil.exp -> Nil.exp
	val substConInCon : (Nil.var -> Nil.con option) -> Nil.con -> Nil.con
	val substConInKind : (Nil.var -> Nil.con option) -> Nil.kind -> Nil.kind
	val substExpInExp : (Nil.var -> Nil.exp option) -> Nil.exp -> Nil.exp
	val convar_occurs_free : Name.var * Nil.con -> bool
	val same_openness : Nil.openness * Nil.openness -> bool
	val same_effect : Nil.effect * Nil.effect -> bool
	val primequiv : primcon * primcon -> bool
	val sub_phase : phase*phase -> bool
	val get_phase : kind -> phase

	type alpha_context = Alpha.alpha_context
	val alpha_equiv_con : Nil.con * Nil.con -> bool
	val alpha_equiv_kind : Nil.kind * Nil.kind -> bool
	val alpha_sub_kind : Nil.kind * Nil.kind -> bool
	val alpha_normalize_con : Nil.con -> Nil.con
	val alpha_normalize_kind : Nil.kind -> Nil.kind
      end = NilUtil
  in
    open C NU
    val eq_var = Name.eq_var
    val eq_label = Name.eq_label
    val fresh_var = Name.fresh_var
    val assoc_eq = Listops.assoc_eq
    val eq_len = Listops.eq_len
    val map_second = Listops.map_second
  end


  (* Local helpers *)
  fun error s = Util.error "nilstatic.sml" s
  fun split ls = 
      let fun split' _ [] = error "split given empty list"
	    | split' acc [x] = (rev acc,x)
	    | split' acc (a::rest) = split' (a::acc) rest
      in split' [] ls
      end
  fun strip_singleton (Singleton_k(_,k,_)) = strip_singleton k
    | strip_singleton k = k

  fun eq_var2 var1 var2 = eq_var (var1,var2)
    
  fun first (fst,_) = fst
    
  fun eq_var_pair var = (eq_var2 var) o first
    
  fun type_or_word T = alpha_sub_kind (T,Type_k Runtime)
  fun is_word T = alpha_sub_kind (T,Word_k Runtime)
  fun list2cmap list = 
    (fn var => 
     case List.find (eq_var_pair var) list
       of SOME (v,c) => SOME c
	| NONE => NONE)
    
  fun sorted_unique p ([] | [_]) = true
    | sorted_unique p (fst::snd::rest) = 
    (not (p (fst,snd))) andalso sorted_unique p (snd::rest)
    
  fun gt_label (l1,l2) = 
    (case Name.compare_label (l1,l2)
       of GREATER => true
	| _ => false)

  fun subst_fn var1 con = 
    (fn var2 => 
     (if eq_var (var1,var2) then 
	SOME con
      else
	NONE))
    
  fun varConKindSubst var con = substConInKind (subst_fn var con)
    
  fun varConConSubst var con = substConInCon (subst_fn var con)
    
    
  fun primKind ((Int_c W64) | 
		(Float_c F32) |
		(Float_c F64)) = Type_k Runtime
    
    | primKind ((Int_c W32) | (Int_c W16) | (Int_c W8) | 
		(BoxFloat_c F64) | (BoxFloat_c F32) |
		(Exn_c) | (Array_c) | (Vector_c) | (Ref_c) | (Exntag_c) | (Sum_c _) |
		(Record_c _) | (Vararg_c _)) = Word_k Runtime
    

  fun do_beta_fun (Let_c (sort,cbnds,Annotate_c (_,con)),actuals) = 
    do_beta_fun (Let_c (sort,cbnds,con),actuals) 
    | do_beta_fun (Let_c (sort,((Open_cb (var,formals,body,body_kind))::rest | 
				(Code_cb (var,formals,body,body_kind))::rest),Var_c var2),actuals) =
    if eq_var (var,var2) then
      let
	fun subst_one ((var,_),actual,body) = 
	  varConConSubst var actual body
      in
	SOME (ListPair.foldr subst_one body (formals,actuals))
      end
    else
      NONE
    | do_beta_fun (Closure_c (code,env), actuals) = 
      do_beta_fun(code,actuals @ [env])
    | do_beta_fun (Annotate_c (an,con),actuals) = 
    (case do_beta_fun (con,actuals)
       of SOME con => SOME (Annotate_c (an,con))
	| NONE => NONE)
    | do_beta_fun cfun = NONE
       
       
  fun do_eta_fun (formals,body as App_c(con,actuals)) = 
    let
      val (formal_vars,_) = ListPair.unzip formals
      fun eq (var1,(Var_c var2)) = eq_var(var1,var2)
	| eq (formal,Annotate_c (an,con)) = eq (formal,con)
	| eq _ = false
      val rule_applies = 
	(ListPair.all eq (formal_vars,actuals)) andalso 
	not (List.exists (fn v => convar_occurs_free (v,con)) 
	     formal_vars)
    in
      if rule_applies then
	SOME con
      else
	NONE
    end
    | do_eta_fun (formals,Annotate_c (an,con)) = 
    (case do_eta_fun(formals,con)
       of SOME con => SOME (Annotate_c (an,con))
	| NONE => NONE)
    | do_eta_fun (formals,body) = NONE
       
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
	  c_insert_kind (D,var,kind,fn C => k (D,(var,kind)::kmap))
	end
    in
      c_foldl cont step (D,[]) kinds
    end  

  and kind_valid (D,kind) = 
    (case kind 
       of Type_k p => (Type_k p)
	| Word_k p => (Word_k p)
	| Singleton_k (p,kind,con) => 
	 let
	   val (con,kind) = (con_valid (D,con))
	   val phase = get_phase kind
	 in
	   if type_or_word kind andalso sub_phase (p,phase) then
	     (Singleton_k (phase,kind,con))
	   else
	     error "Invalid singleton kind"
	 end
	| Record_k elts => 
	 let
	   val elt_list = Util.sequence2list elts
	   val vars_and_kinds = List.map (fn ((l,v),k) => (v,k)) elt_list

	   fun base (D,kmap) = 
	     let
	       val entries = 
		 ListPair.map (fn (((l,_),_),(v,k)) => ((l,v),k)) 
		 (elt_list,kmap)
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
	 end)


  and con_valid (D : context, constructor : con) : con * kind = 
      if (!debug)
	  then con_valid''(D,constructor)
      else con_valid'(D,constructor)

  and con_valid'' (D : context, constructor : con) : con * kind = 
      let val _ = (print "con_valid called with constructor =\n";
		   PpNil.pp_con constructor; 
		   print "\nand context"; NilContext.print_context D;
		   print "\n\n")
	  val res as (c,k) = con_valid'(D,constructor)
	  val _ = (print "con_valid called with constructor =\n";
		   PpNil.pp_con constructor; print "\n";
		   print "returning k = \n";
		   PpNil.pp_kind k; print "\n";
		   print "returning c = \n";
		   PpNil.pp_con c; print "\n")
      in  res
      end

  and con_valid' (D : context, constructor : con) : con * kind = 
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
		 | (Sum_c {known = SOME i,tagcount}) => 
		  if (Word32.<=(Word32.fromInt 1,i) andalso Word32.<=(i,tagcount)) then
		    pcon
		  else
		    error "Illegal index to sum constructor"
		 | _ => pcon)
	   val con = (Prim_c (pcon',args'))

	 in
	   (*ASSERT*)
	   if List.all type_or_word kinds then
	     (con,Singleton_k (get_phase kind,kind,con))
	   else
	     error "Invalid argument to primcon"
	 end
	| (Mu_c (defs,var)) =>
	 let
	   (* Assumes that set implementation guarantees entries are 
	    * all distinct - i.e., no duplicates
	    *)
	   val def_list = Util.sequence2list defs
	   val var_kinds = List.map (fn (var,con) => (var,Word_k Runtime)) def_list
	     
	   fun check_one D ((var,con),(cons,kinds)) =
	     let
	       val (con,kind) = (con_valid (D,con))
	     in
	       ((var,con)::cons,kind::kinds)
	     end

	   fun cont D = (List.foldr (check_one D) ([],[]) def_list)
	   val (cons,kinds) = c_insert_kind_list (D,var_kinds,cont)
	 in
	   (*ASSERT*)
	   if List.all is_word (map strip_singleton kinds) then
	     (Mu_c (Util.list2sequence cons,var),Word_k Runtime)
	   else
	       (app (fn k => (print "kind = "; PpNil.pp_kind k; print "\n\n")) (map strip_singleton kinds);
		error "Invalid kind for recursive constructor")
	 end
	| (AllArrow_c (openness,effect,tformals,formals,numfloats,body)) =>
	 let
	   fun base (D,tformals) = 
	     let
	       (*replace v::S(c) with c in formals and body*)
	       fun substSingleton ((var,Singleton_k(p,kind,scon)),(rev_formals,conmap)) =
		 let 
		   val scon' = substConInCon (list2cmap conmap) scon
		   val kind' = substConInKind (list2cmap conmap) kind
		 in
		   ((var,Singleton_k(p,kind',scon'))::rev_formals,(var,scon')::conmap)
		 end
		 | substSingleton ((var,kind),(rev_formals,conmap)) = 
		 let 
		   val kind' = substConInKind (list2cmap conmap) kind
		 in
		   ((var,kind')::rev_formals,conmap)
		 end
	       val (rev_tformals,conmap) = List.foldl substSingleton ([],[]) tformals
	       val body' = substConInCon (list2cmap conmap) body
	       val tformals' = List.rev rev_tformals
	       val (body'',body_kind) = con_valid (D,body')
	       val (formals',formal_kinds) = 
		 ListPair.unzip (List.map (fn c => con_valid (D,c)) formals)
	       val con = AllArrow_c (openness,effect,tformals',formals',numfloats,body'')
	     in
	       (*ASSERT*)
	       if (List.all type_or_word formal_kinds) andalso 
		 (type_or_word body_kind) then
		 (con,Singleton_k(Runtime,Word_k Runtime,con))
	       else
		 error "Invalid arrow constructor"
	     end
	   
	   fun step ((var,kind),(D,kmap),k) = 
	     let
	       val kind = kindSubstReduce (D,kmap,kind)
	     in
	       c_insert_kind (D,var,kind,fn D => k (D,(var,kind)::kmap))
	     end
	 in
	   c_foldl base step (D,[]) tformals
	 end  
       
	| (v as (Var_c var)) => 
	     (case find_kind (D,var) of
		  SOME (Singleton_k (_,k,c as Var_c v')) => if (eq_var(var,v')) 
							   then (v,k) else con_valid(D,c)
	       | SOME (Singleton_k (_,k,c)) => con_valid(D,c)
	       | SOME k => (v,k)
	       | NONE => 
		     error ("Encountered undefined variable " ^ (Name.var2string var) 
			    ^ "in con_valid"))

        (* This is a lambda.  No, really. *)
	| (Let_c (sort,(((cbnd as Open_cb (var,formals,body,body_kind))::rest) | 
				((cbnd as Code_cb (var,formals,body,body_kind))::rest)),con)) => 
	 let
	   val origD = D
	   fun base (D,rev_formals) = 
	     let
	       val _ = if (!debug)
			   then (print "formals1 are ";
				 app (fn (v,k) => (PpNil.pp_var v; print " :: "; 
						   PpNil.pp_kind k; print "\n")) rev_formals;
				 print "\n")
		       else ()
	       (*replace v::S(c) with c in formals and body*)
	       fun substSingleton ((var,Singleton_k(p,kind,scon)),(formals,conmap)) =
		 let 
		   val scon' = substConInCon (list2cmap conmap) scon
		   val kind' = substConInKind (list2cmap conmap) kind
		 in
		   ((var,Singleton_k(p,kind',scon'))::formals,(var,scon')::conmap)
		 end
		 | substSingleton ((var,kind),(formals,conmap)) = 
		 let 
		   val kind' = substConInKind (list2cmap conmap) kind
		 in
		   ((var,kind')::formals,conmap)
		 end
	       val (formals',conmap) = List.foldl substSingleton ([],[]) rev_formals
	       val body' = substConInCon (list2cmap conmap) body
	       val _ = if !debug
			   then (print "formals' are ";
				 app (fn (v,k) => (PpNil.pp_var v; print " :: "; 
						   PpNil.pp_kind k; print "\n")) formals';
				 print "\n\n")
		       else ()
	       val (body'',body_kind') = con_valid (D,body')
	       val _ = (alpha_sub_kind (body_kind',body_kind)) orelse 
		 (error "invalid return kind for constructor function")
	       val (Con,openness) = 
		 case cbnd 
		   of Open_cb _ => (Open_cb,Open)
		    | _ => (Code_cb,Code)
	       val bndkind = Arrow_k(openness,formals',body_kind')
	       val replace = 
		 case (do_eta_fun (rev rev_formals,body))
		   of SOME con => con
		    | NONE => (Let_c (sort,[Con (var,formals',body'',body_kind')],Var_c var))
	       fun reduce (Annotate_c (annote,c)) =
		 let
		   val (c',kind') = reduce c
		 in
		   (Annotate_c (annote,c'),kind')
		 end
		 | reduce (Var_c var2) = 
		 if eq_var (var,var2) then
		   (replace,bndkind)
		 else
		   con_valid(origD,con)
		 | reduce _ = 
		   con_valid(origD,varConConSubst var replace con)
	     in
	       case rest 
		 of nil => reduce con
		  | _ => 
		   con_valid (origD,varConConSubst var replace (Let_c (sort,rest,con)))
	     end
	   fun step ((var,kind),(D,kmap),k) = 
	     let
	       val kind = kindSubstReduce (D,kmap,kind)
	     in
	       c_insert_kind (D,var,kind,fn D => k (D,(var,kind)::kmap))
	     end
	 in
	   c_foldl base step (D,[]) formals
	 end  
       
        | (Let_c (sort,cbnd as (Con_cb(var,kind,con)::rest),body)) =>
	   let
	     val (con',kind') = con_valid (D,con)
	     val recur_on = 
	       case rest 
		 of nil => body
		  | _ => (Let_c (sort,rest,body))
	   in
	     if alpha_sub_kind (kind',kind) then
	       con_valid (D,varConConSubst var con' recur_on)
	     else
	       error "Kind error in constructor declaration"
	   end
	| (Let_c (sort,[],body)) => con_valid (D,body)
	| (Closure_c (code,env)) => 
	   let
	     val (env',env_kind) = con_valid (D,env)
	   in
	     case (con_valid (D,code)) of
		 (code',code_kind as Arrow_k(Code,vklist,body_kind)) => 
		     let val (first,(v,klast)) = split vklist
		     in
			 if alpha_sub_kind (env_kind,klast) then
			     (Closure_c (code',env'), Arrow_k(Closure,first,body_kind))
			 else
			     (print "Invalid kind for closure environment:";
			      print " env_kind < klast failed\n";
			      print "env_kind is "; PpNil.pp_kind env_kind; print "\n";
			      print "klast is "; PpNil.pp_kind klast; print "\n";
			      print "code_kind is "; PpNil.pp_kind code_kind; print "\n";
			      error "Invalid kind for closure environment")
		     end
		| _ => error "Invalid closure: code component does not have code kind"
	   end
	 
	(* Sort records.  Useful later?*)
	| (Crecord_c entries) => 
	   let
	     
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
	       val var = fresh_var()
	     in
	       (((label,con),((label,var),kind)),(var,kind))
	     end
	   fun entry_sort ((l1,_),(l2,_)) = gt_label(l1,l2)
	   val entries' = ListMergeSort.sort entry_sort entries
	   fun entry_eq ((l1,_),(l2,_)) = eq_label(l1,l2)
	   val distinct = sorted_unique entry_eq entries'
	 in
	   if distinct then
	     c_fold_kind_acc base split D entries'
	   else
	     error "Labels in record of constructors are not distinct"
	 end
	| (Proj_c (rvals,label)) => 
	 let
	   val (rvals',record_kind) = con_valid (D,rvals)

	   val entry_kinds = 
	     (case (strip_singleton record_kind) of
		 Record_k kinds => Util.sequence2list kinds
	       | _ => (print"While con_valid-ing\n";
		       PpNil.pp_con constructor;
		       print"\nUnexpected kind returned from con_valid\n";
		       PpNil.pp_kind (strip_singleton record_kind);
		       print "\nand context is\n";
		       NilContext.print_context D;
		       error "Unexpected kind returned from con_valid"))
		
	   fun propogate [] = error "Label not found in record kind"
	     | propogate (((label2,var),kind)::rest) = 
	     if eq_label (label,label2) then
	       kind
	     else
	       (varConKindSubst var (Proj_c (rvals,label2)) (propogate rest))
	   val con = 
	     case (do_beta_record (rvals',label))
	       of SOME c => c
		| NONE => (Proj_c (rvals',label))
	   val kind = propogate entry_kinds
	 in
	   (con,kind)
	 end
	| (App_c (cfun,actuals)) => 
	 let
	   val (cfun',cfun_kind) = con_valid (D,cfun)
	   val (formals,body_kind) = 
	     case (strip_singleton cfun_kind) of
	         (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		| _ => (print "Invalid kind for constructor application\n";
			PpNil.pp_kind cfun_kind; print "\n";
			error "Invalid kind for constructor application")

	   val (actuals',actual_kinds) = 
	     ListPair.unzip (List.map (fn c => (con_valid (D,c))) actuals)

	   val (formal_vars,formal_kinds) = ListPair.unzip formals

	   fun match_params ((formal,fkind),actual_kind) = 
	     alpha_sub_kind (actual_kind,fkind)

	   val _ = if eq_len (actual_kinds,formal_kinds) then ()
		   else error "Constructor function applied to wrong number of arguments"
	   val apps = 
	     if ListPair.all alpha_sub_kind (actual_kinds,formal_kinds) 
		 then ListPair.zip (formal_vars,actuals')
	     else
	       (print "actual_kinds are:\n";
		app (fn k => (PpNil.pp_kind k; print "\n")) actual_kinds;
		print "\n\n";
		print "formal_kinds are:\n";
		app (fn k => (PpNil.pp_kind k; print "\n")) formal_kinds;
		print "\n";
		error "Constructor function failed: argument not subkind of expected kind")
	       
	   fun lookup v = assoc_eq (eq_var,v,apps)
	 in
	   case (do_beta_fun (cfun',actuals))
	     of SOME c => (if !debug then (print "beta-reduced to\n";
					  PpNil.pp_con c; print "\n")
			   else ();
			   con_valid (D,c))
	      | NONE => (print "failed to beta-reduce!\n";
			 (App_c (cfun,actuals),
			 substConInKind lookup body_kind))
	 end
	| (Typecase_c {arg,arms,default,kind}) => 
	 let
	   val kind' = kind_valid (D,kind)
	   fun doarm (pcon,args,body) = 
	     let
	       val pkind = primKind pcon
	       val args' = map_second (fn k => kind_valid (D,k)) args
	       val (body',body_kind) = c_insert_kind_list (D,args',fn D => con_valid (D,body))
	     in
	       if alpha_sub_kind (body_kind,kind') then
		 (pcon,args',body')
	       else
		 error "Illegal kind in typecase"
	     end
	   val (arg',arg_kind) = con_valid (D,arg)
	   val (default',def_kind) = con_valid (D,default)
	   val arms' = List.map doarm arms
	   fun do_beta_typecase (arg as (Prim_c (pcon,args))) = 
	     (case List.find (fn (pcon',formals,body) => primequiv (pcon,pcon')) arms
		of SOME (pcon',formals,body) => 
		  let
		    val (formal_vars,_) = ListPair.unzip formals
		    val apps = ListPair.zip (formal_vars,args)
		  in
		    if eq_len (formal_vars,args) then
		      substConInCon (list2cmap apps) body
		    else
		      error "Mismatch between formal and actual params in Typecase"
		  end
		 | NONE => default)
	     | do_beta_typecase (Annotate_c (annote,con)) = 
		(Annotate_c (annote,do_beta_typecase con))
	     | do_beta_typecase _ = (Typecase_c {arg=arg',arms=arms',
						 default=default',kind=kind'})
	   val con' = do_beta_typecase arg'
	 in
	   if alpha_sub_kind (def_kind,kind') andalso
	     type_or_word arg_kind then
	     (con',kind')
	   else
	     error "Error in type case"
	 end

	| (Annotate_c (annot,con)) => 
	 let
	   val (con',kind) = con_valid (D,con)
	 in
	   (Annotate_c (annot,con'),kind)
	 end)

  and kindSubstReduce (D,kmap,k) = 
    let
      fun pull (c,kind) = 
	(case kind
	   of Type_k p => c
	    | Word_k p => c
	    | Singleton_k (p,k,c2) => c2
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
	       val var = fresh_var()
	     in
	       (*Closures?  *)
	       case openness
		 of Open => Let_c (Parallel,[Open_cb (var,formals,c,return)],Var_c var)
		  | Code => Let_c (Parallel,[Code_cb (var,formals,c,return)],Var_c var)
		  | Closure => 
		   error "Got closure in pullout - expected open or code"
	     end)
	   
      fun cmap var = 
	(case List.find (eq_var_pair var) kmap
	   of SOME (v,k) => SOME (pull (Var_c var,k))
	    | NONE => NONE)

      val k = substConInKind cmap k
    in
      kind_valid (D,k)
    end
  
  fun kind_equiv (D,k1,k2) = 
    let
      val k1 = kind_valid (D,k1)
      val k2 = kind_valid (D,k2)
    in
      alpha_equiv_kind (k1,k2)
    end

  fun con_equiv (D,c1,c2) = 
    let
      val (c1,_) = con_valid (D,c1)
      val (c2,_) = con_valid (D,c2)
    in
      alpha_equiv_con (c1,c2)
    end

  val kind_reduce = kind_valid

  fun con_reduce (D,con) = 
    let
      val (con,kind) = con_valid (D,con)
    in
      con
    end

  fun sub_kind (D,k1,k2) = 
    let
      val k1' = kind_reduce (D,k1)
      val k2' = kind_reduce (D,k2)
    in
      alpha_sub_kind (k1,k2)
    end


end
