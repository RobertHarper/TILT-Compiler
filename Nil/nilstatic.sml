functor NilStaticFn(structure Annotation : ANNOTATION
		    structure Prim : PRIM
		    structure PrimUtil : PRIMUTIL
		    structure ArgNil : NIL
		    structure PpNil : PPNIL
		    structure Alpha : ALPHA
		    structure NilUtil : NILUTIL 
		    structure NilContext : NILCONTEXT
		    sharing NilUtil.Nil = NilContext.Nil = Alpha.Nil = PpNil.Nil = ArgNil
		    and Annotation = ArgNil.Annotation
		    and Prim = ArgNil.Prim = PrimUtil.Prim
		    and type NilUtil.alpha_context = Alpha.alpha_context
		    and type PrimUtil.con = ArgNil.con
		    and type PrimUtil.exp = ArgNil.exp) :(*>*)
  NILSTATIC where structure Nil = ArgNil and type context = NilContext.context = 
struct	
  
  structure Annotation = Annotation
  structure Prim = Prim
  structure Nil = ArgNil
  open Nil Prim

  val debug = ref false
    
  (* Local rebindings from imported structures *)

  local
    structure C :(*>*)
      sig 
	type context = NilContext.context
	val empty : unit -> context
	val insert_con : context*var*con -> context
	val insert_con_list : context * (var * con) list -> context
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

    structure NU :(*>*)
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
    val var2string = Name.var2string
    val label2string = Name.label2string
    val fresh_var = Name.fresh_var
    val assoc_eq = Listops.assoc_eq
    val eq_len = Listops.eq_len
    val eq_len3 = Listops.eq_len3
    val map_second = Listops.map_second
    val foldl_acc = Listops.foldl_acc
    val map3 = Listops.map3
    val app2 = Listops.app2
    val app3 = Listops.app3
    val zip = ListPair.zip
    val zip3 = Listops.zip3
    val unzip = ListPair.unzip
    val unzip3 = Listops.unzip3
    val unzip4 = Listops.unzip4
    val all3 = Listops.all3
    val map = List.map
    val same_intsize = PrimUtil.same_intsize
    val same_floatsize = PrimUtil.same_floatsize
    val set2list = Util.set2list
    val list2set = Util.list2set
    val mapsequence = Util.mapsequence
    val sequence2list = Util.sequence2list
    val list2sequence = Util.list2sequence
  end


  (* Local helpers *)


  fun error s = Util.error "nilstatic.sml" s

  fun c_all pred fc = 
    let
      fun all [] = true
	| all (fst::rest) = 
	if pred fst then
	  all rest
	else
	  fc fst
    in
      all
    end

  fun c_all1 pred fc = 
    let
      fun all [] = true
	| all (fst::rest) = 
	if pred fst then
	  all rest
	else
	  fc (SOME fst)
    in
      all
    end

  fun c_all2 pred fc = 
    let
      fun all ([],[]) = true
	| all (a::arest,b::brest) = 
	if pred (a,b) then
	  all (arest,brest)
	else
	  fc (SOME (a,b))
	| all _ = fc NONE
    in
      all
    end

  fun c_all3 pred fc = 
    let
      fun all ([],[],[]) = true
	| all (a::arest,b::brest,c::crest) = 
	if pred (a,b,c) then
	  all (arest,brest,crest)
	else
	  fc (SOME (a,b,c))
	| all _ = fc NONE
    in
      all
    end

  fun printl s = print (s^"\n")
  fun lprintl s = print ("\n"^s^"\n")


  fun perr_e exp = 
    (printl "Expression is";
     PpNil.pp_exp exp)

  fun perr_c con =
    (printl "Constructor is";
     PpNil.pp_con con)

  fun perr_k kind = 
    (printl "Kind is";
     PpNil.pp_kind kind)

  fun b_perr_k kind = (perr_k kind;false)

  fun perr_e_c (exp,con) = 
    (printl "Expression is";
     PpNil.pp_exp exp;
     lprintl "of type";
     PpNil.pp_con con)

  fun perr_c_c (con1,con2) = 
    (printl "Expected constructor";
     PpNil.pp_con con1;
     printl "Found constructor";
     PpNil.pp_con con2)

  fun perr_k_k (kind1,kind2) = 
    (printl "Expected kind";
     PpNil.pp_kind kind1;
     printl "Found kind";
     PpNil.pp_kind kind2)

  fun perr_c_k_k (con,kind1,kind2) = 
    (printl "Constructor is";
     PpNil.pp_con con;
     lprintl "Expected kind";
     PpNil.pp_kind kind1;
     lprintl "Found kind";
     PpNil.pp_kind kind2)

  fun perr_e_c_c (exp,con1,con2) = 
    (printl "Expression is";
     PpNil.pp_exp exp;
     lprintl "Expected type";
     PpNil.pp_con con1;
     lprintl "Found type";
     PpNil.pp_con con2)

  fun o_perr pr s opt =  
    let
      val _ = case opt 
		of (SOME arg) => pr arg
		 | NONE => printl s
    in
      false
    end

  val o_perr_e = o_perr perr_e
  val o_perr_c = o_perr perr_c
  val o_perr_k = o_perr perr_k
  val o_perr_e_c = o_perr perr_e_c
  val o_perr_c_c = o_perr perr_c_c
  val o_perr_k_k = o_perr perr_k_k
  val o_perr_c_k_k = o_perr perr_c_k_k
  val o_perr_e_c_c = o_perr perr_e_c_c

  fun split ls = 
      let fun split' _ [] = error "split given empty list"
	    | split' acc [x] = (rev acc,x)
	    | split' acc (a::rest) = split' (a::acc) rest
      in split' [] ls
      end

  fun strip_singleton (Singleton_k(_,k,_)) = strip_singleton k
    | strip_singleton k = k

  fun type_or_word T = alpha_sub_kind (T,Type_k Runtime)
  fun is_word T = alpha_sub_kind (T,Word_k Runtime)

  fun singletonize (phase,kind as Singleton_k _,con) = kind
    | singletonize (phase,kind,con) = 
    if type_or_word kind then
      case phase
	of SOME p => 
	  Singleton_k (p,kind,con)
	 | NONE => Singleton_k (get_phase kind,kind,con)
    else
      kind

  fun is_var_e (Var_e v) = true
    | is_var_e _ = false

  fun is_exn_con (Prim_c (Exn_c,_)) = true
    | is_exn_con (Annotate_c (_,con)) = is_exn_con con
    | is_exn_con _ = false

  fun strip_exntag (Prim_c (Exntag_c,[con])) = SOME con
    | strip_exntag (Annotate_c (_,con)) = strip_exntag con
    | strip_exntag _ = NONE

  fun strip_recursive (Mu_c (set,var)) = SOME (set,var)
    | strip_recursive (Annotate_c (_,con)) = strip_recursive con
    | strip_recursive _ = NONE

  fun strip_boxfloat (Prim_c (BoxFloat_c floatsize,[])) = SOME floatsize
    | strip_boxfloat (Annotate_c (_,con)) = strip_boxfloat con
    | strip_boxfloat _ = NONE

  fun strip_float (Prim_c (Float_c floatsize,[])) = SOME floatsize
    | strip_float (Annotate_c (_,con)) = strip_float con
    | strip_float _ = NONE

  fun strip_int (Prim_c (Int_c intsize,[])) = SOME intsize
    | strip_int (Annotate_c (_,con)) = strip_int con
    | strip_int _ = NONE

  fun strip_sum (Prim_c (Sum_c {tagcount,known},cons)) = SOME (tagcount,known,cons)
    | strip_sum (Annotate_c (_,con)) = strip_sum con
    | strip_sum _ = NONE

  fun strip_arrow (AllArrow_c body) = SOME body
    | strip_arrow (Annotate_c (_,con)) = strip_arrow con
    | strip_arrow _ = NONE

  fun get_arrow_return con = 
    case strip_arrow con
      of SOME (_,_,_,_,_,body_c) => SOME body_c
       | NONE => NONE

  fun strip_record (Prim_c (Record_c labels,cons)) = SOME (labels,cons)
    | strip_record (Annotate_c (_,con)) = strip_record con
    | strip_record _ = NONE

  fun curry2 f = fn a => fn b => f (a,b)
  fun curry3 f = fn a => fn b => fn c => f (a,b,c)

  val eq_var2 = curry2 eq_var
  val eq_label2 = curry2 eq_label

  fun find2 p listpair = 
    let
      fun find_one (a,b,NONE) = if p(a,b) then SOME (a,b) else NONE
	| find_one (_,_,state) = state
    in
      ListPair.foldl find_one NONE listpair
    end

  fun first (fst,_) = fst

  fun eq_var_pair var = (eq_var2 var) o first
    
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
      val (formal_vars,_) = unzip formals
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
	   val elt_list = sequence2list elts
	   val vars_and_kinds = map (fn ((l,v),k) => (v,k)) elt_list

	   fun base (D,kmap) = 
	     let
	       val entries = 
		 ListPair.map (fn (((l,_),_),(v,k)) => ((l,v),k)) 
		 (elt_list,kmap)
	     in
	       (Record_k (list2sequence entries))
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

  and pcon_valid (D : context, pcon : primcon, args : con list ) 
    : primcon * kind * (con list) * kind list = 
    let
      val (args',kinds) = 
	unzip (map (curry2 con_valid D) args)
    in
      (case pcon
	 of ((Int_c W64) | 
	     (Float_c F32) |
	     (Float_c F64)) => (pcon,Type_k Runtime,args',kinds)
	 | ((Int_c W32) | (Int_c W16) | (Int_c W8) | 
	    (BoxFloat_c F64) | (BoxFloat_c F32) |
	    (Exn_c) | (Array_c) | (Vector_c) | (Ref_c) | (Exntag_c)) 
	   => (pcon,Word_k Runtime,args',kinds)
	 | (Record_c labels) => 
	   (if c_all is_word b_perr_k kinds then
	      (Record_c (ListMergeSort.sort gt_label labels),Word_k Runtime,args',kinds)
	    else
	      error "Record contains field of non-word kind")
	 | (Sum_c {known,tagcount}) => 
	      (if c_all is_word b_perr_k kinds then
		 let
		   val valid =  
		     (case known 
			of SOME i => 
			  (Word32.<=(Word32.fromInt 0,i) andalso 
			   Word32.<(i,tagcount))
			 | NONE => true) 
		 in
		   if valid then
		     (pcon,Word_k Runtime,args',kinds)
		   else
		     error "Illegal index to sum constructor" 
		 end
	       else
		 error "Sum contains non-word component")
	 | (Vararg_c _) => 
		 (if c_all is_word b_perr_k kinds then
		    (pcon,Word_k Runtime,args',kinds)
		  else 
		    error "Vararg has non-word component"))
    end
  and con_valid' (D : context, constructor : con) : con * kind = 
     (case constructor 
       of (Prim_c (pcon,args)) =>
	 let

	   val (pcon',kind,args',kinds) = pcon_valid (D,pcon,args)
	   val con = (Prim_c (pcon',args'))

	 in
	     (con,singletonize (SOME Runtime,kind,con))
	 end
	| (Mu_c (defs,var)) =>
	 let
	   (* Assumes that set implementation guarantees entries are 
	    * all distinct - i.e., no duplicates
	    *)

val _ = (print "nilstatic.sml: MU_C case 0 with constructor =\n";
	 PpNil.pp_con constructor; print "\n")
	   val def_list = sequence2list defs

	   val var_kinds = map (fn (var,con) => (var,Word_k Runtime)) def_list
val _ = print "nilstatic.sml: MU_C case 1\n"	     
	   fun check_one D ((var,con),(cons,kinds)) =
	     let
		 val _ = (print "check_ont with con = ";
			  PpNil.pp_con con; print "\n")
	       val (con,kind) = (con_valid (D,con))
	     in
	       ((var,con)::cons,kind::kinds)
	     end

	   fun cont D = (List.foldr (check_one D) ([],[]) def_list)
val _ = print "nilstatic.sml: MU_C case 2\n"
	   val (cons,kinds) = c_insert_kind_list (D,var_kinds,cont)

val _ = print "nilstatic.sml: MU_C case 3\n"
	   val con' = Mu_c (list2sequence cons,var)

	   val kind = singletonize (SOME Runtime,Word_k Runtime,con')
val _ = print "nilstatic.sml: MU_C case 4\n"
	 in
	   if c_all is_word b_perr_k kinds then
	     (con',kind)
	   else
	     error "Invalid kind for recursive constructor"
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
		 unzip (map (curry2 con_valid D) formals)
	       val con = AllArrow_c (openness,effect,tformals',formals',numfloats,body'')
	     in
	       (*ASSERT*)
	       if (c_all type_or_word b_perr_k formal_kinds) andalso 
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
		  SOME (Singleton_k (_,k,c as Var_c v')) => 
		    if (eq_var(var,v')) 
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
	       val (entries,entry_kinds) = unzip entry_info
	       val kind = Record_k (list2sequence entry_kinds)
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
		 Record_k kinds => sequence2list kinds
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
	     unzip (map (curry2 con_valid D) actuals)

	   val (formal_vars,formal_kinds) = unzip formals

	   fun match_params ((formal,fkind),actual_kind) = 
	     alpha_sub_kind (actual_kind,fkind)

	   val apps = 
	     if c_all2 alpha_sub_kind 
	       (o_perr_k_k "Constructor function applied to wrong number of arguments") 
	       (actual_kinds,formal_kinds) 
	       then zip (formal_vars,actuals')
	     else
		error "Constructor function failed: argument not subkind of expected kind"
	   fun lookup v = assoc_eq (eq_var,v,apps)
	 in
	   case (do_beta_fun (cfun',actuals))
	     of SOME c => (if !debug then (print "beta-reduced to\n";
					  PpNil.pp_con c; print "\n")
			   else ();
			   con_valid (D,c))
	      | NONE => (if !debug then print "failed to beta-reduce!\n" else ();
			 (App_c (cfun,actuals),
			 substConInKind lookup body_kind))
	 end
	| (Typecase_c {arg,arms,default,kind}) => 
	 let
	   val kind' = kind_valid (D,kind)
	   fun doarm (pcon,args,body) = 
	     let
	       val (vars,kinds) = unzip args
	       val argcons = map Var_c vars
	       val (pcon',pkind,argcons',kinds') = 
		 c_insert_kind_list (D,args,fn D => pcon_valid (D,pcon,argcons))

	       val args' = zip (vars,kinds')
	       val (body',body_kind) = 
		 c_insert_kind_list (D,args',fn D => con_valid (D,body))

	     in
	       if alpha_sub_kind (body_kind,kind') then
		 (pcon',args,body')
	       else
		 error "Illegal kind in typecase"
	     end
	   val (arg',arg_kind) = con_valid (D,arg)
	   val (default',def_kind) = con_valid (D,default)
	   val arms' = map doarm arms
	   fun do_beta_typecase (arg as (Prim_c (pcon,args))) = 
	     (case List.find (fn (pcon',formals,body) => primequiv (pcon,pcon')) arms
		of SOME (pcon',formals,body) => 
		  let
		    val (formal_vars,_) = unzip formals
		    val apps = zip (formal_vars,args)
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
		 mapsequence 
		 (fn ((label,var),kind) => 
		  (label,pull (Proj_c (c,label),kind))) elts
	     in
	       (Crecord_c entries)
	     end
	    | Arrow_k (openness, formals, return) => 
	     let
	       val vars = map (fn (v,_) => (Var_c v)) formals
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


(* Term level type checking.  *)

  fun get_function_type (D,openness,Function (effect,recursive,tformals,
					      formals,fformals,body,return)) = 
    let
      val num_floats = Word32.fromInt (List.length fformals)
      val con = AllArrow_c (openness,effect,tformals,#2 (unzip formals),num_floats,return)
      val (con',kind) = con_valid (D,con)
    in
      (con',kind)
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
	   (value,Prim_c (Int_c intsize,[]),kind)
	 end
	| float (floatsize,string) => 
	 (value,Prim_c (Float_c floatsize,[]),Type_k Runtime)
	| array (con,arr) => 
	 let
	   val (con',kind) = con_valid (D,con)
	   fun check (i,exp) = 
	     let
	       val (exp',con',kind) = exp_valid (D,exp)
	     in
	       if alpha_equiv_con (con,con') then
		 Array.update (arr,i,exp')
	       else
		 error "Array contains expression of incorrect type"
	     end
	 in
	   Array.appi check (arr,0,NONE);
	   (array (con',arr),Prim_c (Array_c,[con']),Word_k Runtime)
	 end
	| vector (con,vec) => 
	 let
	   val (con',kind) = con_valid (D,con)
	   fun check i = 
	     let
	       val (exp',con',kind) = exp_valid (D,Array.sub (vec,i))
	     in
	       if alpha_equiv_con (con,con') then
		 exp'
	       else
		 error "Vector contains expression of incorrect type"
	     end
	   val vec' = Array.tabulate (Array.length vec,check) 
	 in
	   (vector (con',vec'),Prim_c (Vector_c,[con']),Word_k Runtime)
	 end
	| refcell expref =>
	 let
	   val (exp',con,kind) = exp_valid (D,!expref)
	 in
	   expref := exp';
	   (refcell expref,Prim_c (Ref_c,[con]),Word_k Runtime)
	 end
	| tag (atag,con) => 
	 let
	   val (con',kind) = con_valid (D,con)
	 in
	   (tag (atag,con'),
	    Prim_c (Exntag_c,[con']),Word_k Runtime)
	 end)
  and prim_valid (D,prim,cons,exps) = 
    (case (prim,cons,exps)
       of (record labels,cons,exps) =>
	 let
	   val fields = zip3 labels exps cons
	   fun field_gt ((l1,_,_),(l2,_,_)) = gt_label(l1,l2)
	   val fields' = ListMergeSort.sort field_gt fields
	   fun field_eq ((l1,_,_),(l2,_,_)) = eq_label(l1,l2)
	   val distinct = sorted_unique field_eq fields'
	   fun check_one (label,exp,con) = 
	     let
	       val (con',kind) = con_valid (D,con)
	       val (exp',con'',kind') = exp_valid (D,exp)
	     in
	       if alpha_equiv_con (con',con'') then
		 (label,exp',con')
	       else
		 (perr_c_c (con',con'');
		  error ("Type mismatch in record at field "^(label2string label)))
	     end
	   val fields'' = map check_one fields'
	   val (labels',exps',cons') = unzip3 fields'
	   val exp' = (record labels',cons',exps')
	   val con = Prim_c (Record_c labels',cons')
	   val (con',kind) = con_valid (D,con)
	 in
	   (exp',con',kind)
	 end
	| (select label,[],[exp]) =>
	 let
	   val (exp',con,kind) = exp_valid (D,exp)
	   val (labels,cons) = 
	     (case strip_record con 
		of SOME x => x
		 | NONE => 
		  (perr_e_c (exp,con);
		   error "Projection from value of non record type"))
	 in
	   case find2 (fn (l,c) => eq_label (l,label)) (labels,cons)
	     of SOME (_,con) => 
	       ((select label,[],[exp']),
		con,singletonize (SOME Runtime,Word_k Runtime,con))
	      | NONE => 
	       (perr_e_c (exp,con);
		printl ("Label "^(label2string label)^" projected from expression");
		error "No such label")
	 end
	| (inject {tagcount,field},cons,exps as ([] | [_])) =>  
	 let
	   val (cons',kinds) = 
	     unzip (map (curry2 con_valid D) cons)
	   val con = Prim_c (Sum_c {tagcount=tagcount,known=SOME field},cons')
	   val kind = singletonize (SOME Runtime,Word_k Runtime,con)
	 in
	   case exps 
	     of [] => 
	       if (field < tagcount) then
		 ((inject {tagcount=tagcount,field=field},cons',[]),con,kind)
	       else
		 (perr_e (Prim_e (NilPrimOp prim,cons',[]));
		  error "Illegal injection - field out of range")
	      | argexp::_ =>    
		 if (tagcount <= field) andalso 
		   ((Word32.toInt field) < ((Word32.toInt tagcount) + (List.length cons'))) then
		   let
		     val (argexp',argcon,argkind) = exp_valid (D,argexp)
		     val con_k = List.nth (cons',Word32.toInt (field-tagcount))
		   in
		     if alpha_equiv_con (argcon,con_k) then 
		       ((inject {tagcount=tagcount,field=field},cons',[argexp']),con,kind)
		     else
		       (perr_e_c_c (argexp',con_k,argcon);
			error "Illegal injection - type mismatch in args")
		   end
		 else
		   (perr_e (Prim_e (NilPrimOp prim,cons',[argexp]));
		    error "Illegal injection - field out of range")
	 end
	| (inject_record {tagcount,field},argcons,argexps) => 
	 let
	   val (argcons',argkinds) = 
	     unzip (map (curry2 con_valid D) argcons)
	   val (argexps',expcons,expkinds) = 
	     unzip3 (map (curry2 exp_valid D) exps)
	   val con = Prim_c (Sum_c {tagcount=tagcount,known=SOME field},argcons')
	   val kind = singletonize (SOME Runtime,Word_k Runtime,con)
	 in
	   if (tagcount <= field) andalso 
	     ((Word32.toInt field) < ((Word32.toInt tagcount) + (List.length argcons'))) then
	     let
	       val con_k = List.nth (argcons',Word32.toInt (field-tagcount))
	       val (labels,cons) = 
		 case strip_record con_k
		   of SOME (ls,cs) => (ls,cs)
		    | NONE => (printl "Field is not a record ";
			       PpNil.pp_con con_k;
			       error "Record injection on illegal field")
	     in
	       if c_all2 alpha_equiv_con 
		 (o_perr_c_c "Length mismatch") (expcons,cons) then 
		 ((inject_record {tagcount=tagcount,field=field},argcons',argexps'),con,kind)
	       else
		  error "Illegal record injection - type mismatch in args"
	     end
	   else
	     (printl "Expression ";
	      PpNil.pp_exp (Prim_e (NilPrimOp prim,cons,exps));
	      error "Illegal injection - field out of range")
	 end
	| (project_sum {tagcount,sumtype},argcons,[argexp]) => 
	 let
	   val (argexp',argcon,argkind) = exp_valid (D,argexp)
	   val (argcons',argkinds) = unzip (map (curry2 con_valid D) argcons)
	 in
	   case strip_sum argcon
	     of SOME (tagcount',SOME field,cons) =>
	       if tagcount' = tagcount andalso
		 field = sumtype andalso
		 field < tagcount then
		 let
		   val con_i = List.nth (argcons',Word32.toInt sumtype)
		   val kind = singletonize (SOME Runtime,Word_k Runtime,con_i)
		 in
		   if c_all2 alpha_equiv_con (o_perr_c_c "Length mismatch") (cons,argcons') then
		     ((prim,argcons',[argexp']),con_i,kind)
		   else
		      error "Arguments to project_sum don't match"
		 end
	       else 
		 (perr_e_c ((Prim_e (NilPrimOp prim,cons,exps)),
			    argcon);
		  error "Illegal projection - numbers don't match")
	      | _ => 
		 (perr_e_c ((Prim_e (NilPrimOp prim,cons,exps)),
			    argcon);
		  error "Illegal projection - expression not of sum type")
	 end
	| (project_sum_record {tagcount,sumtype,field},argcons,[argexp]) => 
	 let
	   val (argexp',argcon,argkind) = exp_valid (D,argexp)
	   val (argcons',argkinds) = unzip (map (curry2 con_valid D) argcons)
	 in
	   case strip_sum argcon
	     of SOME (tagcount',SOME sumtype,cons) =>
	       (if tagcount' = tagcount andalso
		  sumtype = sumtype andalso
		  sumtype < tagcount then
		  if c_all2 alpha_equiv_con (o_perr_c_c "Length mismatch") (cons,argcons') then
		    let
		      val con_i = List.nth (argcons',Word32.toInt sumtype)
		    in
		      case strip_record con_i
			of SOME (labels,cons) =>
			  if (Word32.toInt field) < List.length labels then
			    let 
			      val con_j = List.nth (cons,Word32.toInt field) 
			      val kind = singletonize (SOME Runtime,Word_k Runtime,con_j)
			    in
			      ((prim,argcons',[argexp']),con_j,kind)
			    end
			  else error "Project_sum_record field out of range"
			 | NONE => 
			    (perr_c con_i;
			     error "Non recrod type in sum record projection")
		    end
		  else
		    error "Arguments to project_sum don't match"
		else 
		  (perr_e (Prim_e (NilPrimOp prim,cons,exps));
		   error "Illegal projection - numbers don't match"))
	      | _ => 
		  (perr_e (Prim_e (NilPrimOp prim,cons,exps));
		   error "Illegal projection - expression not of sum type")
	 end
	| (box_float floatsize,[],[exp]) => 
	 let
	   val (exp',con,kind) = exp_valid (D,exp)
	   val box_con = Prim_c (BoxFloat_c floatsize,[])
	 in
	   case strip_float con
	     of SOME floatsize' => 
	       if same_floatsize (floatsize,floatsize') then
		 ((box_float floatsize,[],[exp']),
		  box_con,
		  singletonize (SOME Runtime,Word_k Runtime,box_con))
	       else
		 error "Mismatched float size in box float"
	      | NONE => error "Box float called on non-float"
	 end
	| (unbox_float floatsize,[],[exp]) => 
	 let
	   val (exp',con,kind) = exp_valid (D,exp)
	   val unbox_con = Prim_c (Float_c floatsize,[])
	 in
	   case strip_boxfloat con
	     of SOME floatsize' => 
	       if same_floatsize (floatsize,floatsize') then
		 ((unbox_float floatsize,[],[exp']),
		  unbox_con,
		  singletonize (SOME Runtime,Type_k Runtime,unbox_con))
	       else
		 error "Mismatched float size in box float"
	      | NONE => error "Unbox float called on non-boxfloat"
	 end
	| (roll,[argcon],[exp]) => 
	 let
	   val (argcon',argkind) = con_valid (D,argcon)
	   val (exp',con,kind) = exp_valid (D,exp)
	 in
	   case strip_recursive argcon' 
	     of SOME (set,var) =>
	       let
		 val def_list = set2list set
		 val (_,con') = valOf (List.find (fn (v,c) => eq_var (v,var)) def_list)
		 val cmap = list2cmap (map (fn (v,c) => (v,Mu_c (set,v))) def_list)
		 val con'' = substConInCon cmap con'
	       in
		 if con_equiv (D,con,con'') then
		   ((roll,[argcon'],[exp']),argcon',argkind)
		 else
		   (perr_e_c_c (exp',con'',con);
		    error "Error in roll")
	       end
	      | NONE => 
	       (printl "Roll primitive given argument of type";
		PpNil.pp_con argcon';
		lprintl " not a recursive type";
		error "Illegal constructor argument in roll")
	 end
	| (unroll,[con],[exp]) =>
	 let
	   val (argcon',argkind) = con_valid (D,con)
	   val (exp',con,kind) = exp_valid (D,exp)
	 in
	   (case strip_recursive argcon' 
	      of SOME (set,var) =>
		(if alpha_equiv_con (argcon',con) then
		   let
		     val def_list = set2list set
		     val (_,con') = valOf (List.find (fn (v,c) => eq_var (v,var)) def_list)
		     val cmap = list2cmap (map (fn (v,c) => (v,Mu_c (set,v))) def_list)
		     val con'' = substConInCon cmap con'
		   in
		     ((roll,[argcon'],[exp']),con'',
		      singletonize(SOME Runtime,Word_k Runtime,con''))
		   end
		 else
		   (perr_e_c_c (exp',argcon',con);
		    error "Error in unroll"))
	       | NONE => 
		   (printl "Urnoll primitive given argument of type";
		    PpNil.pp_con argcon';
		    lprintl " not a recursive type";
		    error "Illegal constructor argument in unroll"))
	 end
	| (make_exntag,[argcon],[]) => 
	 let
	   val (argcon',argkind) = con_valid (D,argcon)
	   val exp' = (make_exntag,[argcon'],[])
	   val con' = Prim_c (Exntag_c,[argcon'])
	   val kind' = singletonize (SOME Runtime,Word_k Runtime,con')
	 in
	   (exp',con',kind')
	 end
	| (inj_exn,[],[exp1,exp2]) => 
	 let
	   val (exp1',con1,kind1) = exp_valid (D,exp1)
	   val (exp2',con2,kind2) = exp_valid (D,exp2)
	 in
	   case strip_exntag con2
	     of SOME con => 
	       if alpha_equiv_con (con1,con) then
		 let
		   val exp' = (inj_exn,[],[exp1',exp2'])
		   val con' = Prim_c (Exn_c,[])
		   val kind'= singletonize (SOME Runtime,Word_k Runtime,con')
		 in
		   (exp',con',kind')
		 end
	       else
		 error "Type mismatch in exception injection"
	      | NONE =>  
		 (perr_e_c (exp2,con2);
		  error "Illegal argument to exception injection - not a tag")
	 end
	| (make_vararg (openness,effect),cons,exps) =>
	 error "make_vararg unimplemented....punting"
	| (make_onearg (openness,effect),cons,exps) =>  
	 error "make_onearg unimplemented....punting"
	| (peq,cons,exps) => error "Polymorphic equality should not appear at this level"
	| (prim,cons,exps) => 
	 (perr_e (Prim_e (NilPrimOp prim,cons,exps));
	  lprintl "No matching case in exp_valid";
	  error "Illegal primitive application"))

  and switch_valid (D,switch)  = 
    (case switch
       of Intsw_e {info=intsize,arg,arms,default} =>
	 let
	   val (arg',argcon,argkind) = exp_valid (D,arg)
	   val (ns,arm_exps) = unzip arms
	   val (arm_exps',arm_cons,arm_kinds) =
	     unzip3 (map (curry2 function_valid D) arm_exps)
	   val arms' = zip (ns,arm_exps')
	   val _ = 
	     case strip_int argcon
	       of SOME intsize' => 
		 if same_intsize (intsize,intsize') then ()
		 else error "Integer size mismatch in int switch"
		| NONE => 
		 (perr_e_c (arg',argcon);
		  error "Branch argument not an int")

	   fun check_one con arm_con = 
	     case strip_arrow arm_con
	       of SOME (_,_,[],[],_,body_c) => 
		 if alpha_equiv_con (con,body_c) then ()
		 else
		   (perr_c_c (con,body_c);
		    error "Branch arm types don't match")
		| SOME _ => error "IntSwitch has illegal function type - arguments present"
		| NONE => error "IntSwitch has non-function type"

	   val (default',rep_opt) = opt_arrow_con (D,default,arm_cons)
	   val (rep_con,rep_kind) = 
	     case rep_opt
	       of SOME v => v
		| NONE => error "No valid arms in int branch"
	   val _ = List.app (check_one rep_con) arm_cons
	 in
	   (Intsw_e {info=intsize,arg=arg',
		     arms=arms',default=default'},
	    rep_con,rep_kind)
	 end
	| Sumsw_e {info=(tagcount,decl_cons),arg,arms,default} => 
	 let
	   val (arg',argcon,argkind) = exp_valid (D,arg)
	   val (ns,arm_exps) = unzip arms
	   val (arm_exps',arm_cons,arm_kinds) =
	     unzip3 (map (curry2 function_valid D) arm_exps)
	   val arms' = zip (ns,arm_exps')
	   val (decl_cons',decl_kinds) = unzip (map (curry2 con_valid D) decl_cons)
	   val sum_cons = 
	     case strip_sum argcon
	       of SOME (tagcount,NONE,cons) => cons

		| SOME (tagcount,SOME _,cons) =>
		   error "Sum case called on known sum type constructor"
		| NONE => 
		 (perr_e_c (arg',argcon);
		  error "Branch argument not of sum type")
	   fun check_one con (arm_con,sum_con,decl_con) = 
	     case strip_arrow arm_con
	       of SOME (_,_,[],[arg_con],_,body_c) => 
		 if (alpha_equiv_con (con,body_c) andalso
		     alpha_equiv_con (arg_con,sum_con) andalso
		     alpha_equiv_con (decl_con,sum_con)) then ()
		 else
		   (printl "Argmument type :";
		    perr_c_c (sum_con,argcon);
		    printl "Return type";
		    perr_c_c (con,body_c);
		    error "Sum Branch arm types don't match")
		| SOME _ => error "SumSwitch has illegal function type - wrong arguments"
		| NONE => error "SumSwitch has non-function type"
	   val (default',rep_opt) = opt_arrow_con (D,default,arm_cons)
	   val (rep_con,rep_kind) = 
	     case rep_opt
	       of SOME v => v
		| NONE => error "No valid arms in sum branch"
	   val _ = 
	     if eq_len3 (arm_cons,sum_cons,decl_cons')  then
	       app3 (check_one rep_con) (arm_cons,sum_cons,decl_cons')
	     else
	       error "Argument/sum/branch lists of different lengths in sum switch"
	 in
	   (Sumsw_e {info=(tagcount,decl_cons'),arg=arg',
		     arms=arms',default=default'},
	    rep_con,rep_kind)
	 end
     | Exncase_e {info=_,arg,arms,default} =>
	 let
	   val (arg',argcon,argkind) = exp_valid (D,arg)
	   val (vars,arm_exps) = unzip arms
	   val (arm_exps',arm_cons,arm_kinds) =
	     unzip3 (map (curry2 function_valid D) arm_exps)
	   val (vars',var_cons,var_kinds) = 
	     unzip3 (map (curry2 exp_valid D) vars)

	   val _ = if c_all is_var_e (fn e => (perr_e e;false)) vars' then ()
		   else error "Non-variable index in exncase"

	   val arms' = zip (vars',arm_exps')

	   fun check_one con (var_con,arm_con) = 
	     case strip_arrow arm_con
	       of SOME (_,_,[],[arg_con],_,body_c) => 
		 (case strip_exntag var_con
		    of SOME exn_con =>
		      if (alpha_equiv_con (con,body_c) andalso
			  alpha_equiv_con (arg_con,var_con)) then 
			()
		      else
			(printl "Argument type :";
			 perr_c_c (var_con,arg_con);
			 printl "Return type";
			 perr_c_c (con,body_c);
			 error "Exn Branch arm types don't match")
		     | NONE => error ("Variable has wrong type"))
		| SOME _ => error "Exncase has illegal function type - wrong arguments"
		| NONE => error "SumSwitch has non-function type"

	   val (default',rep_opt) = opt_arrow_con (D,default,arm_cons)
	   val (rep_con,rep_kind) = 
	     case rep_opt
	       of SOME v => v
		| NONE => error "No valid arms in exn case"
	   val _ = 
	     if eq_len (var_cons,arm_cons)  then
	       app2 (check_one rep_con) (var_cons,arm_cons)
	     else
	       error "Argument/branch lists of different lengths in exncase"
	 in
	   (Exncase_e {info=(),arg=arg',
		       arms=arms',default=default'},
	    rep_con,rep_kind)
	 end
     | Typecase_e {info,arg=argcon,arms,default} =>
	 let
	   val (argcon',argkind) = con_valid (D,argcon)
	   val (pcons,arm_fns) = unzip arms
	   val (arm_fns',arm_cons,arm_kinds) = 
	     unzip3 (map (curry2 function_valid D) arm_fns)
	   val args = [] (*HACK!!! WRONG!!!*)
	   val (pcons',pkinds,_,_) = 
	     unzip4 (map (fn p => pcon_valid(D,p,args)) pcons)

	   val arms' = zip (pcons',arm_fns')

	   fun check_one con arm_con = error "Unimplemented"
(*	     case strip_arrow arm_con
	       of SOME (_,_,[],[pcon],_,body_c) => 
		 (case strip_exntag arg_con
		    of SOME exn_con =>
		      if alpha_equiv_con (con,body_c) then 
			()
		      else
			(printl "Argument type :";
			 perr_c_c (var_con,arg_con);
			 printl "Return type";
			 perr_c_c (con,body_c);
			 error "Exn Branch arm types don't match")
		     | NONE => error ("Variable "^(var2string v)^"has wrong type"))
		| SOME _ => error "Exncase has illegal function type - wrong arguments"
		| NONE => error "SumSwitch has non-function type"
*)
	   val (default',rep_opt) = opt_arrow_con (D,default,arm_cons)
	   val (rep_con,rep_kind) = 
	     case rep_opt
	       of SOME v => v
		| NONE => error "No valid arms in type case"
	   val _ = 
	       List.app (check_one rep_con) arm_cons
	 in
	   (Typecase_e {info=(),arg=argcon',
			arms=arms',default=default'},
	    rep_con,rep_kind)
	 end)
       
  and opt_arrow_con (D,SOME def,_) =
    let
      val (def',defcon,defkind) = exp_valid (D,def)
    in
      (SOME def',SOME (defcon,defkind))
    end
    | opt_arrow_con (D,NONE,fst::_) =
    (case get_arrow_return fst
       of SOME con => (NONE,SOME (con_valid (D,con)))
	| NONE => (NONE,NONE))
    | opt_arrow_con _ = (NONE,NONE)
  and function_valid (D,Function (effect,recursive,tformals,
				  formals,fformals,body,return)) = 
    let
      fun check_k ((var,kind),D) =
	let
	  val kind' = kind_valid (D,kind)
	in
	  ((var,kind'),insert_kind (D,var,kind'))
	end
      val (tformals',D') = foldl_acc check_k D tformals
      fun check_c ((var,con),D) = 
	let
	  val (con',kind) = con_valid (D,con)
	in
	  ((var,con'),insert_con (D,var,con'))
	end
      val (formals',D'') = foldl_acc check_c D' formals
      val D''' = 
	foldl (fn (v,D) => 
	       insert_con (D,v,Prim_c (Float_c F64,[]))) D'' fformals
      val (body',body_c,body_kind) = exp_valid (D''',body)
      val (return',return_kind) = con_valid (D''',return)
      val function' = Function (effect,recursive,tformals',formals',fformals,body',return')
      val (con',kind) = get_function_type (D,Open,function')
    in
      if alpha_equiv_con (body_c,return') then
	(function',con',kind)
      else
	(perr_e_c_c (body',return',body_c);
	 error "Return expression has wrong type")
    end
  and bnds_valid (D,bnds) = bnds_valid' (D,bnds,[])
  and bnds_valid' (D,[],rev_bnds) = (D,rev rev_bnds)
    | bnds_valid' (D,bnd::rest,rev_bnds) = 
    let
      val (D',bnd') = 
	(case bnd 
	   of Con_b (var, kind, con) =>
	     let
	       val kind' = kind_valid (D,kind)
	       val (con',kind'') = con_valid (D,con)
	     in
	       if alpha_equiv_kind (kind',kind'') then
		 (insert_kind (D,var,kind''),Con_b (var,kind'',con'))
	       else
		 (perr_c_k_k (con',kind',kind'');
		  error ("kind mismatch in constructor binding of "^(var2string var)))
	     end
	    | Exp_b (var, con, exp) =>
	     let
	       val (con',kind) = con_valid (D,con)
	       val (exp',con'',kind'') = exp_valid (D,exp)
	     in
	       if alpha_equiv_con (con',con'') then
		 (insert_con (D,var,con''),Exp_b (var,con'',exp'))
	       else
		 (perr_e_c_c (exp',con',con'');
		  error ("type mismatch in expression binding of "^(var2string var)))
	     end
	    | ((Fixopen_b defs) | (Fixcode_b defs)) =>
	     let
	       val def_list = set2list defs
	       val (vars,functions) = unzip def_list
	       val openness = 
		 (case bnd 
		    of Fixopen_b _ => Open
		     | _ => Code)
	       val (declared_c,declared_k) = 
		 (unzip (map (curry3 get_function_type D openness) functions))
	       val D' = insert_con_list (D,zip (vars,declared_c))
	       val (functions',found_c,found_k) = 
		 unzip3 (map (curry2 function_valid D') functions)
	       val defs' = list2set (zip (vars,functions'))
	     in
	       if c_all2 alpha_equiv_con (o_perr_c_c "Length mismatch") (declared_c,found_c) then
		 (D',Fixopen_b defs')
	       else
		 error "Declared type for function binding doesn't match found type"
	     end
	 | Fixclosure_b defs => error "Unimplemented")
    in
      bnds_valid' (D,rest,bnd'::rev_bnds)
    end

  and exp_valid (D : context,exp : exp) : (exp * con * kind) = 
    (case exp 
       of Var_e var => 
	 (case find_con (D,var)
	    of SOME con => 
	      let
		val (con',kind) = con_valid (D,con)
	      in
		(exp,con,kind)
	      end
	     | NONE => 
	      error ("Encountered undefined variable " ^ (Name.var2string var) 
		     ^ "in exp_valid"))
	| Const_e value => 
	    let
	      val (value',con,kind) = value_valid (D,value)
	    in
	      (Const_e value',con,singletonize (NONE,kind,con))
	    end
	| Let_e (letsort,bnds,exp) => 
	    let
	      val (D',bnds') = bnds_valid (D,bnds)
	      val (exp',con,kind) = exp_valid (D',exp)
	    in
	      (Let_e (letsort,bnds',exp'),con,singletonize (NONE,kind,con))
	    end
	| Prim_e (NilPrimOp prim,cons,exps) =>   
	    let
	      val ((prim',cons',exps'),con,kind) = prim_valid (D,prim,cons,exps)
	    in
	      (Prim_e (NilPrimOp prim',cons',exps'),con,kind)
	    end
	| Prim_e (PrimOp prim,cons,exps) =>   
	    let 
	      val (cons',kinds) = unzip (map (curry2 con_valid D) cons)
	      val con = PrimUtil.get_type prim cons'
	      val (con',kind) = con_valid (D,con)
	      val (exps',expcons,expkinds) = 
		unzip3 (map (curry2 exp_valid D) exps)
	      val exp' = Prim_e (PrimOp prim,cons',exps')
	    in
	      if c_all2 alpha_equiv_con (o_perr_c_c "Length mismatch") (cons',expcons) then
		(exp',con',kind)
	      else
		(map3 perr_e_c_c (exps',cons',expcons);
		 error "Argument type mismatch in primop")
	    end
	| Switch_e switch =>
	    let
	      val (switch',con,kind) = switch_valid (D,switch)
	    in
	      (Switch_e switch,con,kind)
	    end
	| ((App_e (openness as Code,exp as (Var_e _),cons,texps,fexps)) |  
	   (App_e (openness as (Closure | Open),exp,cons,texps,fexps))) =>
	let
	  val (cons',kinds) = unzip (map (curry2 con_valid D) cons)
	  val actuals_t = map (curry2 exp_valid D) texps
	  val actuals_f = map (curry2 exp_valid D) fexps
	  val (exp',con,kind) = exp_valid (D,exp)
	  val (openness',_,tformals,formals,numfloats,body) = 
	    (case strip_arrow con
	       of SOME c => c
		| NONE => (perr_e_c (exp',con);
			   error "Application of non-arrow expression"))

	      fun check_cons ([],[],[],D) = true
		| check_cons ([],(exp,con,kind)::actuals_f,con'::formals,D) = 
		if alpha_equiv_con (con,con') then
		  check_cons ([],actuals_f,formals,D)
		else
		  (perr_e_c_c (exp,con',con);
		   error "Formal/actual parameter type mismatch")
		| check_cons ((exp,con,kind)::actuals_t,actuals_f,con'::formals,D) = 
		  if alpha_equiv_con (con,con') then
		    check_cons (actuals_t,actuals_f,formals,D)
		  else
		  (perr_e_c_c (exp,con',con);
		   error "Formal/actual parameter type mismatch")
		| check_cons _ = error "exp_valid: Too few formals in parameter list"

	      fun check_kinds ([],[],D) = SOME D
		| check_kinds ((var,kind)::tformals,kind'::actuals,D) = 
		if alpha_sub_kind (kind',kind) then
		  check_kinds (tformals,actuals,insert_kind (D,var,kind))
		else
		  (perr_k_k (kind,kind');
		   error "Constructor parameter kind mismatch")
		| check_kinds (_,_,D) = NONE

	      val params_match = 
		case check_kinds (tformals,kinds,D)
		  of SOME D' =>
		    check_cons (actuals_t,actuals_f,formals,D')
		   | NONE => 
		    error "Mismatch between formal and actual constructor parameter lists"
	      val texps' = #1 (unzip3 actuals_t)
	      val fexps' = #1 (unzip3 actuals_f)
	      val body_kind = 
		case openness 
		  of Open => singletonize (SOME Runtime,Type_k Runtime,body)
		   | _ => singletonize (SOME Runtime,Word_k Runtime,body)
	    in
	      if same_openness (openness,openness') andalso
		((Word32.toInt numfloats) = (List.length fexps)) andalso
		eq_len (tformals,cons') andalso params_match 
		then
		  (App_e (openness,exp',cons',texps',fexps'),
		   body,body_kind)
	      else
		error "Error in application - different openness"
	    end
	| App_e _ => 
	    (printl "Application expression ";
	     PpNil.pp_exp exp;
	     error "Illegal application.  Closure with non-var?")
	| Raise_e (exp,con) => 
	    let
	      val (con',kind) = con_valid (D,con)
	      val (exp',con'',kind') = exp_valid (D,exp)
	    in
	      if is_exn_con (con'') then
		(exp',con',singletonize (NONE,kind',con'))
	      else
		(perr_e_c (exp',con');
		 error "Non exception raised - Ill formed expression")
	    end
	| Handle_e (exp,function) =>
	    (case function 
	       of Function (effect,recursive,[],[(var,c as Prim_c (Exn_c,[]))],
			    [],body,con) =>
		 let
		   val (con',kind) = con_valid (D,con)
		   val (exp',con'',kind') = exp_valid (D,exp)
		   val (body',con''',kind'') = exp_valid (insert_con (D,var,c),body)
		   val function' = Function (effect,recursive,[],
					     [(var,c)],[],body',con')
		 in
		   if alpha_equiv_con (con',con'') andalso
		     alpha_equiv_con (con'',con''') then
		     (Handle_e (exp',function'),con'',singletonize (NONE,kind',con''))
		   else
		     (print "Declared as : \n";
		      PpNil.pp_con con';
		      print "\nExpected : \n";
		      PpNil.pp_con con'';
		      print "\nFound : \n";
		      PpNil.pp_con con''';
		      error "Handler body has incorrect type")
		 end
	       | _ => 
		 (print "Body is :\n";
		  PpNil.pp_exp (Handle_e (exp,function));
		  error "Illegal body for handler"))
	       )  (*esac*)
end
