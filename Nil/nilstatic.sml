functor NilStaticFn(structure Annotation : ANNOTATION
		    structure PrimUtil : PRIMUTIL
		    structure ArgNil : NIL
		    structure PpNil : PPNIL
		    structure Alpha : ALPHA
		    structure NilUtil : NILUTIL 
		    structure NilContext : NILCONTEXT
		    structure NilError : NILERROR 
		         sharing NilUtil.Nil = NilContext.Nil = Alpha.Nil 
			       = PpNil.Nil = NilError.Nil = ArgNil
			 and Annotation = ArgNil.Annotation
			 and ArgNil.Prim = PrimUtil.Prim
			 and type NilUtil.alpha_context = Alpha.alpha_context
			 and type PrimUtil.con = ArgNil.con
		         and type PrimUtil.exp = ArgNil.exp) :(*>*) NILSTATIC 
  where structure Nil = ArgNil 
	and type context = NilContext.context = 
struct	
  
  structure Annotation = Annotation
  structure Nil = ArgNil
  open Nil 
  open Prim

  val debug = ref false
  

  local
      datatype entry = CON of con * NilContext.context | KIND of kind * NilContext.context
      val stack = ref ([] : entry list)
      fun push e = stack := (e :: (!stack))
  in
      fun push_con(c,context) = push(CON(c,context))
      fun push_kind(k,context) = push(KIND(k,context))
      fun pop() = stack := (tl (!stack))
      fun show_stack() = let val st = !stack
			     val _ = stack := []
			     fun show (CON(c,context)) =
				 (print "con_valid called with constructor =\n";
				  PpNil.pp_con c;
				  print "\nand context"; NilContext.print_context context;
				  print "\n\n")
			       | show (KIND(k,context)) =
				 (print "kind_valid called with constructor =\n";
				  PpNil.pp_kind k;
				  print "\nand context"; NilContext.print_context context;
				  print "\n\n")
			 in  app show (rev st)
			 end
  end

  (* Local rebindings from imported structures *)

  (*From NilContext*)
  type context = NilContext.context
  val empty = NilContext.empty
  val insert_con = NilContext.insert_con
  val insert_con_list = NilContext.insert_con_list
  val find_con = NilContext.find_con
  val remove_con = NilContext.remove_con
  val insert_kind = NilContext.insert_kind
  val insert_kind_list = NilContext.insert_kind_list
  val find_kind = NilContext.find_kind
  val remove_kind = NilContext.remove_kind

  (*From Alpha*)
  type alpha_context = Alpha.alpha_context

  (*From NilUtil*)
  val substConInExp = NilUtil.substConInExp
  val substConInCon = NilUtil.substConInCon
  val substConInKind = NilUtil.substConInKind
  val substExpInExp = NilUtil.substExpInExp
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
  val strip_singleton = NilUtil.strip_singleton

  (*From Name*)
  val eq_var = Name.eq_var
  val eq_label = Name.eq_label
  val var2string = Name.var2string
  val label2string = Name.label2string
  val fresh_var = Name.fresh_var

  (*From Listops*)
  val assoc_eq = Listops.assoc_eq
  val eq_len = Listops.eq_len
  val eq_len3 = Listops.eq_len3
  val map_second = Listops.map_second
  val foldl_acc = Listops.foldl_acc
  val map = Listops.map
  val map2 = Listops.map2
  val map3 = Listops.map3
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
  val printl = Util.printl
  val lprintl = Util.lprintl

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


  fun error s = Util.error "nilstatic.sml" s

  fun split ls = 
      let fun split' _ [] = error "split given empty list"
	    | split' acc [x] = (rev acc,x)
	    | split' acc (a::rest) = split' (a::acc) rest
      in split' [] ls
      end

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
	(case Name.compare_label (l1,l2) of
	     GREATER => true
	   | _ => false)

  fun subst_fn var1 con = 
    (fn var2 => 
     (if eq_var (var1,var2) then 
	SOME con
      else
	NONE))
    
  fun varConKindSubst var con = substConInKind (subst_fn var con)
    
  fun varConConSubst var con = substConInCon (subst_fn var con)

  fun type_or_word T = alpha_sub_kind (T,Type_k Runtime)
  fun is_word T = alpha_sub_kind (T,Word_k Runtime)

  fun singletonize (phase,kind as Singleton_k _,con) = kind
    | singletonize (phase,kind,con) = 
(*    if type_or_word kind then*)
      case phase
	of SOME p => 
	  Singleton_k (p,kind,con)
	 | NONE => Singleton_k (get_phase kind,kind,con)
(*    else*)
(*      kind*)

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

  fun foldSubstSingleton var_kind_list = 
    let
      val (rev_list,conmap) = List.foldl substSingleton ([],[]) var_kind_list
    in
      (rev rev_list,list2cmap conmap)
    end

  fun eta_reduce_fun lambda = 
    let
      fun eta_reduce_fun' 
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
	| eta_reduce_fun' _ = lambda
    in
      map_annotate eta_reduce_fun' lambda
    end

(* XXX must sort here? *)
     (*PRE: elements are in head normal form*)
  fun eta_reduce_record record_c = 
    let
      fun eta_reduce_record' (Crecord_c []) = record_c
	| eta_reduce_record' (Crecord_c ((label,con)::rest)) = 
	let
	  fun etable repcon (label,con) = 
	    (case strip_proj con
	       of SOME (con2,label2) => 
		 (eq_label (label,label2)) andalso 
		 (alpha_equiv_con (repcon,con2))
		| NONE => false)
	in
	  case strip_proj con
	    of SOME (c,l) => 
	      if ((eq_label (l,label)) andalso (all (etable c) rest)) then
		c
	      else 
		record_c
	     | NONE => record_c
	end
	| eta_reduce_record' _ = 
	(perr_c record_c;
	 (error "eta_reduce_record passed non record" handle e => raise e))
    in
      map_annotate eta_reduce_record' record_c
    end
  
  fun beta_reduce_typecase typecase = 
    let 
      fun beta_reduce_typecase' 
	(Typecase_c {arg,arms,default,kind}) = 
	(case strip_prim arg
	   of SOME (pcon,args) =>
	     (case List.find (fn (pcon',formals,body) => primequiv (pcon,pcon')) arms
		of SOME (pcon',formals,body) => 
		  let
		    val (vars,_) = unzip formals
		    val conmap = list2cmap (zip vars args)
		  in
		    if eq_len (vars,args) then
		      substConInCon conmap body
		    else
		      (error "Mismatch between formal and actual params in Typecase" handle e => raise e)
		  end
		 | NONE => default)
	    | _ => typecase)
	| beta_reduce_typecase' _ = 
	   (perr_c typecase;
	    (error "beta_reduce_typecase called on non type case" handle e => raise e))
    in
      map_annotate beta_reduce_typecase' typecase
    end

  fun beta_reduce_record proj = 
    let
      fun beta_reduce_record' (Proj_c (con,label)) = 
	(case strip_crecord con
	   of SOME entries =>
	     (case (List.find (fn ((l,_)) => eq_label (l,label))
		    entries )
		of SOME (l,c) => c
		 | NONE => (error "Field not in record" handle e => raise e))
	    | NONE => proj)
	| beta_reduce_record' _ = 
	   (perr_c proj;
	    (error "beta_reduce_record called on non-projection" handle e => raise e))
    in
      map_annotate beta_reduce_record' proj
    end

  fun foldl_acc2 ffun init list = 
      let 
	fun loop (state,[]) = ([],state)
	  | loop (state,fst::rest) =
	  let
	    val (fst',state') = ffun (fst,state)
	    val (rest',state'') = loop (state',rest)
	  in
	    (fst'::rest',state'')
	  end
      in
	loop (init,list)
      end
    
  fun foldKSR (D,kinds) = 
    let
	fun loop (D,[],kmap) = (D,rev kmap)
	  | loop (D,(var,kind)::rest,kmap) =
	  let
	    val kind' = kindSubstReduce (D,kmap,kind)
	  in
	    loop (insert_kind (D,var,kind'),rest,(var,kind')::kmap)
	  end
    in
      loop (D,kinds,[])
    end
 
  and beta_reduce_fun (D,app) = 
    let
      fun beta_reduce_fun' (app as (App_c (con,actuals))) = 
	let
	  fun beta_reduce_fun'' actuals
	    (Let_c (sort,(([Open_cb (var,formals,body,body_kind)]) |
			  ([Code_cb (var,formals,body,body_kind)])),con)) = 
		let val fooey = 1
		in    (case strip_var con of
			 SOME var2 =>
				 if eq_var (var,var2) then
				   let
				     val (vars,_) = unzip formals
				     val conmap = list2cmap (zip vars actuals)
				     val body' = substConInCon conmap body
				     val (body'',_) = con_valid(D,body')
				   in
				     body''
				   end
				 else app
			| NONE => app)
		end
	    | beta_reduce_fun'' actuals (Closure_c (code,env)) = 
	       beta_reduce_fun'' (actuals @ [env]) code
	    | beta_reduce_fun'' _ _ = app
	in    
	  map_annotate (beta_reduce_fun'' actuals) con
	end
	| beta_reduce_fun' con = 
	(perr_c con;
	 (error "beta_reduce_fun called on non-application" handle e => raise e))
    in
      map_annotate beta_reduce_fun' app
    end

  and kind_valid (D,kind) = 
      let val _ = push_kind(kind,D)
	  val _ = if (!debug)
		      then (print "kind_valid called with constructor =\n";
			    PpNil.pp_kind kind;
			    print "\nand context"; NilContext.print_context D;
			    print "\n\n")
		  else ()
        val res = kind_valid'(D,kind)
	  val _ = pop()
      in  res
      end

  and kind_valid' (D : context, kind : kind) : kind = 
    (case kind 
       of Type_k p => (Type_k p)
	| Word_k p => (Word_k p)
	| Singleton_k (p,kind,con) => 
	 let
	   val (con,kind) = (con_valid (D,con))
	   val kind = strip_singleton kind
	   val phase = get_phase kind
	 in
	   if sub_phase (p,phase) then
	     (Singleton_k (phase,kind,con))
	   else
	     (error "Invalid singleton kind" handle e => raise e)
	 end
	| Record_k elts => 
	 let
	   fun entry_sort (((l1,_),_),((l2,_),_)) = gt_label(l1,l2)
	   val elt_list = ListMergeSort.sort entry_sort (sequence2list elts)
	   val vars_and_kinds = map (fn ((l,v),k) => (v,k)) elt_list

	   val (D',vars_and_kinds') = foldKSR (D,vars_and_kinds)
	   val entries = 
	     map2 (fn (((l,_),_),(v,k)) => ((l,v),k)) (elt_list,vars_and_kinds')
	 in  (Record_k (list2sequence entries))
	 end
	| Arrow_k (openness, formals, return) => 
	 let
	   val (D,formals) = foldKSR (D,formals)
	   val return = kindSubstReduce (D,formals,return)
	 in
	   (Arrow_k (openness, formals,return))
	 end)

  and con_valid (D : context, constructor : con) : con * kind = 
      let val _ = push_con(constructor,D)
	  val _ = if (!debug)
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
	   (if c_all is_word b_perr_k kinds then
	      (Record_c (ListMergeSort.sort gt_label labels),Word_k Runtime,args,kinds)
	    else
	      (error "Record contains field of non-word kind" handle e => raise e))
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
		     (pcon,Word_k Runtime,args,kinds)
		   else
		     (error "Illegal index to sum constructor" handle e => raise e) 
		 end
	       else
		 (error "Sum contains non-word component" handle e => raise e))
	 | (Vararg_c _) => 
		 (if c_all is_word b_perr_k kinds then
		    (pcon,Word_k Runtime,args,kinds)
		  else 
		    (error "Vararg has non-word component" handle e => raise e)))
    end
  and con_valid' (D : context, constructor : con) : con * kind = 
    (case constructor 
       of (Prim_c (pcon,args)) =>
	 let
	   val (pcon,kind,args,kinds) = pcon_valid (D,pcon,args)
	   val con = (Prim_c (pcon,args))
	 in
	     (con,singletonize (SOME Runtime,kind,con))
	 end
	| (Mu_c (defs,var)) =>
	 let
	   (* Assumes that set implementation guarantees entries are 
	    * all distinct - i.e., no duplicates
	    *)

	   fun is_bound var = (case find_kind(D,var) of
				     SOME _ => true
				   | NONE => false)
	   val Mu_c(def_list,var) = NilUtil.rename_mu(is_bound,defs,var)

	   val var_kinds = map (fn (var,con) => (var,Word_k Runtime)) def_list

	   fun check_one D ((var,con),(cons,kinds)) =
	     let
	       val (con,kind) = (con_valid (D,con))
	     in
	       ((var,con)::cons,kind::kinds)
	     end

	   val (vars,cons) = unzip def_list

	   val D = insert_kind_list (D,var_kinds)
	     
	   val (cons,kinds) = unzip (map (curry2 con_valid D) cons)
	   val defs = list2sequence (zip vars cons)
	   val con = Mu_c (defs,var)
	   val kind = singletonize (SOME Runtime,Word_k Runtime,con)
	 in
	   if c_all is_word b_perr_k kinds then
	     (con,kind)
	   else
	     (error "Invalid kind for recursive constructor" handle e => raise e)
	 end
	| (AllArrow_c (openness,effect,tformals,formals,numfloats,body)) =>
	 let
	   val (D,tformals) = foldKSR (D,tformals)
	   val (tformals,conmap) = foldSubstSingleton tformals
	   val body = substConInCon conmap body
	   val (body,body_kind) = con_valid (D,body)
	   val (formals,formal_kinds) = 
	     unzip (map (curry2 con_valid D) formals)
	   val con = AllArrow_c (openness,effect,tformals,formals,numfloats,body)
	   val kind = Word_k Runtime
	   val kind = singletonize (SOME Runtime,kind,con)
	 in
	   (*ASSERT*)
	   if (c_all type_or_word b_perr_k formal_kinds) andalso 
	     (type_or_word body_kind) then
	     (con,kind)
	   else
	     (error "Invalid arrow constructor" handle e => raise e)
	 end
	| (v as (Var_c var)) => 
	     (case find_kind (D,var) 
		of SOME (k as (Singleton_k (_,k',c as Var_c v'))) =>  
		  if (eq_var(var,v'))  
		    then (v,k)  
		  else con_valid(D,c) 
		 | SOME (Singleton_k (_,k,c)) => con_valid(D,c)  
		 | SOME k => (v,Singleton_k(get_phase k,k,v))
		 | NONE => 
		    (error ("Encountered undefined variable " ^ (Name.var2string var) 
			   ^" in con_valid") handle e => raise e))

	| (Let_c (sort,(((cbnd as Open_cb (var,formals,body,body_kind))::rest) | 
			((cbnd as Code_cb (var,formals,body,body_kind))::rest)),con)) => 
	 let
	   val (formals,body,body_kind) = 
				let fun mapper (v,_) = (case NilContext.find_kind(D,v) of
								NONE => NONE
							      | SOME _ => SOME(v,Var_c (Name.derived_var v)))
				    val table = List.mapPartial mapper formals
				    fun subst v  = Listops.assoc_eq(eq_var,v,table)
				in  case table of
					[] => (formals,body,body_kind)
				      | _ => (map (fn (v,k) => ((case subst v of
								        SOME (Var_c v') => v'
								      | _ => v),
								NilUtil.substConInKind subst k)) formals,
						substConInCon subst body,
						substConInKind subst body_kind)
				end
	   val origD = D
	   val (D,formals) = foldKSR (D,formals)
	   val _ = if (!debug)
		     then (print "formals1 are ";
			   app (fn (v,k) => (PpNil.pp_var v; print " :: "; 
					     PpNil.pp_kind k; print "\n")) formals;
			   print "\n")
		   else ()
	   val (formals,conmap) = foldSubstSingleton formals
	   val body = substConInCon conmap body
	   val _ = if !debug
		     then (print "formals' are ";
			   app (fn (v,k) => (PpNil.pp_var v; print " :: "; 
					     PpNil.pp_kind k; print "\n")) formals;
			   print "\n\n")
		   else ()
	   val (body,body_kind') = con_valid (D,body)
	   val _ = if (alpha_sub_kind (body_kind',body_kind)) then ()
		   else (perr_k_k (body_kind,body_kind');
			 (error "invalid return kind for constructor function" handle e => raise e))
	   val (Con,openness) = 
	     case cbnd 
	       of Open_cb _ => (Open_cb,Open)
		| _ => (Code_cb,Code)
	   val lambda = (Let_c (sort,[Con (var,formals,body,body_kind')],Var_c var))
	   val lambda = eta_reduce_fun lambda
	   val bndkind = Arrow_k(openness,formals,body_kind)
	   val bndkind = singletonize(NONE,bndkind,lambda)
	 in
	   if (null rest) andalso (is_var_c con) andalso 
	     eq_opt (eq_var,SOME var,strip_var con) then
	     (lambda,bndkind)
	   else
	     con_valid (origD,varConConSubst var lambda (Let_c (sort,rest,con)))
	 end
        | (Let_c (sort,cbnd as (Con_cb(var,kind,con)::rest),body)) =>
	   let
	     val kind = kind_valid(D,kind) (* must normalize the constructors inside the kind *)
	     val (con,kind') = con_valid (D,con)
	     val con = varConConSubst var con (Let_c (sort,rest,body))
	   in
	     if alpha_sub_kind (kind',kind) then
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
	     val (env,env_kind) = con_valid (D,env)
	     val (code,code_kind) =  con_valid (D,code)
	   in
	     case (strip_singleton code_kind)
	       of Arrow_k (Code,vklist,body_kind) => 
		 let 
		   val (first,(v,klast)) = split vklist
		   val con = Closure_c (code,env)
		   val kind = Arrow_k(Closure,first,body_kind)
		   val kind = singletonize (NONE,kind,con)
		 in
		   if alpha_sub_kind (env_kind,klast) then
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
	     val distinct = sorted_unique eq_label labels
	     val (cons,kinds) = unzip (map (curry2 con_valid D) cons)
	     val k_entries = map2 (fn (l,k) => ((l,fresh_var()),k)) (labels,kinds)
	     val entries = zip labels cons
	     val con = Crecord_c entries
	     val con = eta_reduce_record con
	     val kind = singletonize (NONE,Record_k (list2sequence k_entries),con)
	   in 
	     if distinct then
	       (con,kind)
	     else
	       (PpNil.pp_list PpNil.pp_label' labels 
		("labels are: ",",",";",true);
	       (error "Labels in record of constructors not sorted or not distinct" handle e => raise e))
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
		
	   fun propogate [] = (error "Label not found in record kind" handle e => raise e)
	     | propogate (((label2,var),kind)::rest) = 
	     if eq_label (label,label2) then
	       kind
	     else
	       (varConKindSubst var (Proj_c (rvals,label2)) (propogate rest))

	   val con = Proj_c (rvals,label)
	   val con = beta_reduce_record con
	   val kind = singletonize (NONE,propogate entry_kinds,con)
	 in
	   (con,kind)
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

	   val (actuals,actual_kinds) = 
	     unzip (map (curry2 con_valid D) actuals)
	   val (formal_vars,formal_kinds) = unzip formals
	   val apps = 
	     if c_all2 alpha_sub_kind 
	       (o_perr_k_k "Constructor function applied to wrong number of arguments") 
	       (actual_kinds,formal_kinds) 
	       then zip formal_vars actuals
	     else
		(error "Constructor function failed: argument not subkind of expected kind" handle e => raise e)

	   val conmap = list2cmap apps
	   val con = App_c (cfun,actuals)
	   val con = beta_reduce_fun (D,con)
	   val kind = singletonize (NONE,substConInKind conmap body_kind,con)
	 in
	   (con,kind)
	 end
	| (Typecase_c {arg,arms,default,kind}) => 
	 let
	   val kind = kind_valid (D,kind)
	   fun doarm (pcon,args,body) = 
	     let
	       val (vars,kinds) = unzip args
	       val kinds = map (curry2 kind_valid D) kinds
	       val argcons = map Var_c vars
	       val args = zip vars kinds
	       val D = insert_kind_list (D,args)
	       val (pcon,pkind,argcons,kinds) = pcon_valid (D,pcon,argcons)
	       val (body,body_kind) = con_valid(D,body)
	     in
	       if alpha_sub_kind (body_kind,kind) then
		 (pcon,args,body)
	       else
		 (perr_k_k (kind,body_kind);
		  (error "Illegal kind in typecase" handle e => raise e))
	     end
	   val (arg,arg_kind) = con_valid (D,arg)
	   val (default,def_kind) = con_valid (D,default)
	   val arms = map doarm arms
	   val con = Typecase_c {arg=arg,arms=arms,
				 default=default,kind=kind}
	   val con = beta_reduce_typecase con
	   val kind = singletonize (NONE,kind,con)
	 in
	   if alpha_sub_kind (def_kind,kind) andalso
	     type_or_word arg_kind then
	     (con,kind)
	   else
	     (error "Error in type case" handle e => raise e)
	 end

	| (Annotate_c (annot,con)) => 
	 let
	   val (con,kind) = con_valid (D,con)
	 in
	   (Annotate_c (annot,con),kind)
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
		   (error "Got closure in pullout - expected open or code" handle e => raise e)
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
	   val (con,kind) = con_valid (D,con)
	   fun check exp = 
	     let
	       val (exp',con',kind) = exp_valid (D,exp)
	     in
	       if alpha_equiv_con (con,con') then
		 exp'
	       else
		 (error "Array contains expression of incorrect type" handle e => raise e)
	     end
	 in
	   Array.modify check arr;
	   (array (con,arr),Prim_c (Array_c,[con]),Word_k Runtime)
	 end
	| vector (con,vec) => 
	 let
	   val (con,kind) = con_valid (D,con)
	   fun check exp = 
	     let
	       val (exp',con',kind) = exp_valid (D,exp)
	     in
	       if alpha_equiv_con (con,con') then
		 exp'
	       else
		 (error "Vector contains expression of incorrect type" handle e => raise e)
	     end
	 in
	   Array.modify check vec;
	   (vector (con,vec),Prim_c (Vector_c,[con]),Word_k Runtime)
	 end
	| refcell expref =>
	 let
	   val (exp,con,kind) = exp_valid (D,!expref)
	 in
	   expref := exp;
	   (refcell expref,Prim_c (Ref_c,[con]),Word_k Runtime)
	 end
	| tag (atag,con) => 
	 let
	   val (con,kind) = con_valid (D,con)
	 in
	   (tag (atag,con),
	    Prim_c (Exntag_c,[con]),Word_k Runtime)
	 end)
  and prim_valid (D,prim,cons,exps) = 
    (case (prim,cons,exps)
       of (record labels,cons,exps) =>
	 let
	   val fields = zip3 labels exps cons
	   fun field_eq ((l1,_,_),(l2,_,_)) = eq_label(l1,l2)
	   val distinct = sorted_unique field_eq fields
	   fun check_one (label,exp,con) = 
	     let
	       val (con,kind) = con_valid (D,con)
	       val (exp,con',kind') = exp_valid (D,exp)
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
	   if distinct then
	     (exp,con,kind)
	   else
	     (error "Fields not distinct or not sorted" handle e => raise e)
	 end
	| (select label,[],[exp]) =>
	 let
	   val (exp,con,kind) = exp_valid (D,exp)
	   val (labels,cons) = 
	     (case strip_record con 
		of SOME x => x
		 | NONE => 
		  (perr_e_c (exp,con);
		   (error "Projection from value of non record type" handle e => raise e)))
	 in
	   case find2 (fn (l,c) => eq_label (l,label)) (labels,cons)
	     of SOME (_,con) => 
	       ((select label,[],[exp]),
		con,singletonize (SOME Runtime,Word_k Runtime,con))
	      | NONE => 
	       (perr_e_c (exp,con);
		printl ("Label "^(label2string label)^" projected from expression");
		(error "No such label" handle e => raise e))
	 end
	| (inject {tagcount,field},cons,exps as ([] | [_])) =>  
	 let
	   val (cons,kinds) = 
	     unzip (map (curry2 con_valid D) cons)
	   val con = Prim_c (Sum_c {tagcount=tagcount,known=SOME field},cons)
	   val kind = singletonize (SOME Runtime,Word_k Runtime,con)
	 in
	   case exps 
	     of [] => 
	       if (field < tagcount) then
		 ((inject {tagcount=tagcount,field=field},cons,[]),con,kind)
	       else
		 (perr_e (Prim_e (NilPrimOp prim,cons,[]));
		  (error "Illegal injection - field out of range" handle e => raise e))
	      | argexp::_ =>    
		 if (tagcount <= field) andalso 
		   ((Word32.toInt field) < ((Word32.toInt tagcount) + (List.length cons))) then
		   let
		     val (argexp,argcon,argkind) = exp_valid (D,argexp)
		     val con_k = List.nth (cons,Word32.toInt (field-tagcount))
		   in
		     if alpha_equiv_con (argcon,con_k) then 
		       ((inject {tagcount=tagcount,field=field},cons,[argexp]),con,kind)
		     else
		       (perr_e_c_c (argexp,con_k,argcon);
			(error "Illegal injection - type mismatch in args" handle e => raise e))
		   end
		 else
		   (perr_e (Prim_e (NilPrimOp prim,cons,[argexp]));
		    (error "Illegal injection - field out of range" handle e => raise e))
	 end
	| (inject_record {tagcount,field},argcons,argexps) => 
	 let
	   val (argcons,argkinds) = 
	     unzip (map (curry2 con_valid D) argcons)
	   val (argexps,expcons,expkinds) = 
	     unzip3 (map (curry2 exp_valid D) exps)
	   val con = Prim_c (Sum_c {tagcount=tagcount,known=SOME field},argcons)
	   val kind = singletonize (SOME Runtime,Word_k Runtime,con)
	 in
	   if (tagcount <= field) andalso 
	     ((Word32.toInt field) < ((Word32.toInt tagcount) + (List.length argcons))) then
	     let
	       val con_k = List.nth (argcons,Word32.toInt (field-tagcount))
	       val (labels,cons) = 
		 case strip_record con_k
		   of SOME (ls,cs) => (ls,cs)
		    | NONE => (printl "Field is not a record ";
			       PpNil.pp_con con_k;
			       (error "Record injection on illegal field" handle e => raise e))
	     in
	       if c_all2 alpha_equiv_con 
		 (o_perr_c_c "Length mismatch") (expcons,cons) then 
		 ((inject_record {tagcount=tagcount,field=field},argcons,argexps),con,kind)
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
	   val (argexp,argcon,argkind) = exp_valid (D,argexp)
	   val (argcons,argkinds) = unzip (map (curry2 con_valid D) argcons)
	 in
	   case strip_sum argcon
	     of SOME (tagcount',SOME field,cons) =>
	       if tagcount' = tagcount andalso
		 field = sumtype andalso
		 field < tagcount then
		 let
		   val con_i = List.nth (argcons,Word32.toInt sumtype)
		   val kind = singletonize (SOME Runtime,Word_k Runtime,con_i)
		 in
		   if c_all2 alpha_equiv_con (o_perr_c_c "Length mismatch") (cons,argcons) then
		     ((prim,argcons,[argexp]),con_i,kind)
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
	   val (argexp,argcon,argkind) = exp_valid (D,argexp)
	   val (argcons,argkinds) = unzip (map (curry2 con_valid D) argcons)
	 in
	   case strip_sum argcon
	     of SOME (tagcount',SOME sumtype,cons) =>
	       (if tagcount' = tagcount andalso
		  sumtype = sumtype andalso
		  sumtype < tagcount then
		  if c_all2 alpha_equiv_con (o_perr_c_c "Length mismatch") (cons,argcons) then
		    let
		      val con_i = List.nth (argcons,Word32.toInt sumtype)
		    in
		      case strip_record con_i
			of SOME (labels,cons) =>
			  if (Word32.toInt field) < List.length labels then
			    let 
			      val con_j = List.nth (cons,Word32.toInt field) 
			      val kind = singletonize (SOME Runtime,Word_k Runtime,con_j)
			    in
			      ((prim,argcons,[argexp]),con_j,kind)
			    end
			  else (error "Project_sum_record field out of range" handle e => raise e)
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
	   val (exp,con,kind) = exp_valid (D,exp)
	   val box_con = Prim_c (BoxFloat_c floatsize,[])
	 in
	   case strip_float con
	     of SOME floatsize' => 
	       if same_floatsize (floatsize,floatsize') then
		 ((box_float floatsize,[],[exp]),
		  box_con,
		  singletonize (SOME Runtime,Word_k Runtime,box_con))
	       else
		 (error "Mismatched float size in box float" handle e => raise e)
	      | NONE => (error "Box float called on non-float" handle e => raise e)
	 end
	| (unbox_float floatsize,[],[exp]) => 
	 let
	   val (exp,con,kind) = exp_valid (D,exp)
	   val unbox_con = Prim_c (Float_c floatsize,[])
	 in
	   case strip_boxfloat con
	     of SOME floatsize' => 
	       if same_floatsize (floatsize,floatsize') then
		 ((unbox_float floatsize,[],[exp]),
		  unbox_con,
		  singletonize (SOME Runtime,Type_k Runtime,unbox_con))
	       else
		 (error "Mismatched float size in box float" handle e => raise e)
	      | NONE => (error "Unbox float called on non-boxfloat" handle e => raise e)
	 end
	| (roll,[argcon],[exp]) => 
	 let
	   val (argcon,argkind) = con_valid (D,argcon)
	   val (exp,con,kind) = exp_valid (D,exp)
	 in
	   case strip_recursive argcon 
	     of SOME (set,var) =>
	       let
		 val def_list = set2list set
		 val (_,con') = valOf (List.find (fn (v,c) => eq_var (v,var)) def_list)
		 val cmap = list2cmap (map (fn (v,c) => (v,Mu_c (set,v))) def_list)
		 val con' = substConInCon cmap con'
	       in
		 if alpha_equiv_con (con,con') then
		   ((roll,[argcon],[exp]),argcon,argkind)
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
	   val (exp,con,kind) = exp_valid (D,exp)
	 in
	   (case strip_recursive argcon 
	      of SOME (set,var) =>
		(if alpha_equiv_con (argcon,con) then
		   let
		     val def_list = set2list set
		     val (_,con') = valOf (List.find (fn (v,c) => eq_var (v,var)) def_list)
		     val cmap = list2cmap (map (fn (v,c) => (v,Mu_c (set,v))) def_list)
		     val con' = substConInCon cmap con'
		   in
		     ((roll,[argcon],[exp]),con',
		      singletonize(SOME Runtime,Word_k Runtime,con'))
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
	   val kind = singletonize (SOME Runtime,Word_k Runtime,con)
	 in
	   (exp,con,kind)
	 end
	| (inj_exn,[],[exp1,exp2]) => 
	 let
	   val (exp1,con1,kind1) = exp_valid (D,exp1)
	   val (exp2,con2,kind2) = exp_valid (D,exp2)
	 in
	   case strip_exntag con2
	     of SOME con => 
	       if alpha_equiv_con (con1,con) then
		 let
		   val exp = (inj_exn,[],[exp1,exp2])
		   val con = Prim_c (Exn_c,[])
		   val kind = singletonize (SOME Runtime,Word_k Runtime,con)
		 in
		   (exp,con,kind)
		 end
	       else
		 (error "Type mismatch in exception injection" handle e => raise e)
	      | NONE =>  
		 (perr_e_c (exp2,con2);
		  (error "Illegal argument to exception injection - not a tag" handle e => raise e))
	 end
	| (make_vararg (openness,effect),cons,exps) =>
	 (error "make_vararg unimplemented....punting" handle e => raise e)
	| (make_onearg (openness,effect),cons,exps) =>  
	 (error "make_onearg unimplemented....punting" handle e => raise e)
	| (peq,cons,exps) => (error "Polymorphic equality should not appear at this level" handle e => raise e)
	| (prim,cons,exps) => 
	 (perr_e (Prim_e (NilPrimOp prim,cons,exps));
	  lprintl "No matching case in exp_valid";
	  (error "Illegal primitive application" handle e => raise e)))

  and switch_valid (D,switch)  = 
    (case switch
       of Intsw_e {info=intsize,arg,arms,default} =>
	 let
	   val (arg,argcon,argkind) = exp_valid (D,arg)
	   val (ns,arm_exps) = unzip arms
	   val (arm_exps,arm_cons,arm_kinds) =
	     unzip3 (map (curry2 function_valid D) arm_exps)
	   val arms = zip ns arm_exps
	   val _ = 
	     case strip_int argcon
	       of SOME intsize' => 
		 if same_intsize (intsize,intsize') then ()
		 else (error "Integer size mismatch in int switch" handle e => raise e)
		| NONE => 
		 (perr_e_c (arg,argcon);
		  (error "Branch argument not an int" handle e => raise e))

	   fun check_one con arm_con = 
	     case strip_arrow arm_con
	       of SOME (_,_,[],[],_,body_c) => 
		 if alpha_equiv_con (con,body_c) then ()
		 else
		   (perr_c_c (con,body_c);
		    (error "Branch arm types don't match" handle e => raise e))
		| SOME _ => (error "IntSwitch has illegal function type - arguments present" handle e => raise e)
		| NONE => (error "IntSwitch has non-function type" handle e => raise e)

	   val (default,rep_opt) = opt_arrow_con (D,default,arm_cons)
	   val (rep_con,rep_kind) = 
	     case rep_opt
	       of SOME v => v
		| NONE => (error "No valid arms in int branch" handle e => raise e)
	   val _ = List.app (check_one rep_con) arm_cons
	 in
	   (Intsw_e {info=intsize,arg=arg,
		     arms=arms,default=default},
	    rep_con,rep_kind)
	 end
	| Sumsw_e {info=(tagcount,decl_cons),arg,arms,default} => 
	 let
	   val (arg,argcon,argkind) = exp_valid (D,arg)
	   val (ns,arm_exps) = unzip arms
	   val (arm_exps,arm_cons,arm_kinds) =
	     unzip3 (map (curry2 function_valid D) arm_exps)
	   val arms = zip ns arm_exps
	   val (decl_cons,decl_kinds) = unzip (map (curry2 con_valid D) decl_cons)
	   val sum_cons = 
	     case strip_sum argcon
	       of SOME (tagcount,NONE,cons) => cons

		| SOME (tagcount,SOME _,cons) =>
		   (error "Sum case called on known sum type constructor" handle e => raise e)
		| NONE => 
		 (perr_e_c (arg,argcon);
		  (error "Branch argument not of sum type" handle e => raise e))
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
		    (error "Sum Branch arm types don't match" handle e => raise e))
		| SOME _ => (error "SumSwitch has illegal function type - wrong arguments" handle e => raise e)
		| NONE => (error "SumSwitch has non-function type" handle e => raise e)
	   val (default,rep_opt) = opt_arrow_con (D,default,arm_cons)
	   val (rep_con,rep_kind) = 
	     case rep_opt
	       of SOME v => v
		| NONE => (error "No valid arms in sum branch" handle e => raise e)
	   val _ = 
	     if eq_len3 (arm_cons,sum_cons,decl_cons)  then
	       app3 (check_one rep_con) (arm_cons,sum_cons,decl_cons)
	     else
	       (error "Argument/sum/branch lists of different lengths in sum switch" handle e => raise e)
	 in
	   (Sumsw_e {info=(tagcount,decl_cons),arg=arg,
		     arms=arms,default=default},
	    rep_con,rep_kind)
	 end
     | Exncase_e {info=_,arg,arms,default} =>
	 let
	   val (arg,argcon,argkind) = exp_valid (D,arg)
	   val (vars,arm_exps) = unzip arms
	   val (arm_exps,arm_cons,arm_kinds) =
	     unzip3 (map (curry2 function_valid D) arm_exps)
	   val (vars,var_cons,var_kinds) = 
	     unzip3 (map (curry2 exp_valid D) vars)

	   val _ = if c_all is_var_e (fn e => (perr_e e;false)) vars then ()
		   else (error "Non-variable index in exncase" handle e => raise e)

	   val arms = zip vars arm_exps

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
			 (error "Exn Branch arm types don't match" handle e => raise e))
		     | NONE => (error ("Variable has wrong type" handle e => raise e)))
		| SOME _ => (error "Exncase has illegal function type - wrong arguments" handle e => raise e)
		| NONE => (error "SumSwitch has non-function type" handle e => raise e)

	   val (default,rep_opt) = opt_arrow_con (D,default,arm_cons)
	   val (rep_con,rep_kind) = 
	     case rep_opt
	       of SOME v => v
		| NONE => (error "No valid arms in exn case" handle e => raise e)
	   val _ = 
	     if eq_len (var_cons,arm_cons)  then
	       app2 (check_one rep_con) (var_cons,arm_cons)
	     else
	       (error "Argument/branch lists of different lengths in exncase" handle e => raise e)
	 in
	   (Exncase_e {info=(),arg=arg,
		       arms=arms,default=default},
	    rep_con,rep_kind)
	 end
     | Typecase_e {info,arg=argcon,arms,default} =>
	 let
	   val (argcon,argkind) = con_valid (D,argcon)
	   val (pcons,arm_fns) = unzip arms
	   val (arm_fns,arm_cons,arm_kinds) = 
	     unzip3 (map (curry2 function_valid D) arm_fns)

	   fun check_one con (pcon,arm_con) = 
	     case strip_arrow arm_con
	       of SOME (_,_,[],args,_,body_c) => 
		 let
		   val (pcon,pkind,args,argkinds) = pcon_valid(D,pcon,args)
		 in
		   if alpha_equiv_con (body_c,con) andalso 
		     alpha_sub_kind (argkind,pkind) then
		     pcon
		   else
		     (perr_c_c (con,body_c);
		      perr_k_k (pkind,argkind);
		      (error "Typecase expression arm has wrong type" handle e => raise e))
		 end
		| _ => (perr_c arm_con;
			   (error "Typecase expression has illegal arm type" handle e => raise e))
	   val (default,rep_opt) = opt_arrow_con (D,default,arm_cons)
	   val (rep_con,rep_kind) = 
	     case rep_opt
	       of SOME v => v
		| NONE => (error "No valid arms in type case" handle e => raise e)
	   val pcons = map2 (check_one rep_con) (pcons,arm_cons)
	   val arms = zip pcons arm_fns
	 in
	   (Typecase_e {info=(),arg=argcon,
			arms=arms,default=default},
	    rep_con,rep_kind)
	 end)
       
  and opt_arrow_con (D,SOME def,_) =
    let
      val (def,defcon,defkind) = exp_valid (D,def)
    in
      (SOME def,SOME (defcon,defkind))
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
	  val kind = kind_valid (D,kind)
	in
	  ((var,kind),insert_kind (D,var,kind))
	end
      val (tformals,D') = foldl_acc check_k D tformals
      fun check_c ((var,con),D) = 
	let
	  val (con,kind) = con_valid (D,con)
	in
	  ((var,con),insert_con (D,var,con))
	end
      val (formals,D') = foldl_acc check_c D' formals
      val D' = 
	foldl (fn (v,D) => 
	       insert_con (D,v,Prim_c (Float_c F64,[]))) D' fformals
      val (body,body_c,body_kind) = exp_valid (D',body)
      val (return,return_kind) = con_valid (D',return)
      val function = Function (effect,recursive,tformals,formals,fformals,body,return)
      val (con,kind) = get_function_type (D,Open,function)
    in
      if alpha_equiv_con (body_c,return) then
	(function,con,kind)
      else
	(perr_e_c_c (body,return,body_c);
	 (error "Return expression has wrong type" handle e => raise e))
    end
  and bnds_valid (D,bnds) = foldl_acc bnd_valid' D bnds
  and bnd_valid (D,bnd) = bnd_valid' (bnd,D)
  and bnd_valid' (bnd,D) = 
    (case bnd 
       of Con_b (var, kind, con) =>
	 let
	   val kind' = kind_valid (D,kind)
	   val (con',kind'') = con_valid (D,con)
	   val D' = insert_kind (D,var,kind'')
	   val bnd' = Con_b (var,kind'',con')
	 in
	   if alpha_sub_kind (kind'',kind') then
	     (bnd',D')
	   else
	     (perr_c_k_k (con',kind',kind'');
	      (error ("kind mismatch in constructor binding of "^(var2string var)) handle e => raise e))
	 end
	| Exp_b (var, con, exp) =>
	 let
	   val (con',kind) = con_valid (D,con)
	   val (exp',con'',kind'') = exp_valid (D,exp)
	   val D' = insert_con (D,var,con'')
	   val bnd' = Exp_b (var,con'',exp')
	 in
	   if alpha_equiv_con (con',con'') then
	     (bnd',D')
	   else
	     (perr_e_c_c (exp',con',con'');
	      (error ("type mismatch in expression binding of "^(var2string var)) handle e => raise e))
	 end
	| ((Fixopen_b defs) | (Fixcode_b defs)) =>
	 let
	   val def_list = set2list defs
	   val (vars,functions) = unzip def_list
	   val (openness,constructor) = 
	     (case bnd 
		of Fixopen_b _ => (Open,Fixopen_b)
		 | _ => (Code,Fixcode_b))
	   val (declared_c,declared_k) = 
	     (unzip (map (curry3 get_function_type D openness) functions))
	   val D' = insert_con_list (D,zip vars declared_c)
	   val (functions',found_c,found_k) = 
	     unzip3 (map (curry2 function_valid D') functions)
	   val defs' = list2set (zip vars functions')
	   val bnd' = constructor defs'
	 in
	   if c_all2 alpha_equiv_con (o_perr_c_c "Length mismatch") (declared_c,found_c) then
	     (bnd',D')
	   else
	     (error "Declared type for function binding doesn't match found type" handle e => raise e)
	 end
	| Fixclosure_b defs => 
	 let
	   val (vars,closures) = unzip (set2list defs)
	   val tipes = map (fn cl => #1 (con_valid (D,#tipe cl))) closures
	   val D' = insert_con_list (D,zip vars tipes)
	   fun do_closure ({code,cenv,venv,tipe},tipe') = 
	     let
	       val (cenv',ckind) = con_valid (D',cenv)
	       val (venv',vcon,vkind) = exp_valid (D',venv)
	       val (code_type) = 
		 (case find_con (D',code)
		    of SOME k => k
		     | NONE => (printl ("Code pointer "^(var2string code));
				print " not defined in context";
				(error "Invalid closure" handle e => raise e)))
	       val con = 
		 (case strip_arrow code_type
		    of SOME (Code,effect,tformals,formals,numfloats,body_c) => 
		      let
			val (tformals',(v,last_k)) = split tformals
			val (formals',last_c) = split formals
		      in
			if alpha_sub_kind (ckind,last_k) andalso
			  alpha_equiv_con (vcon,last_c) 
			  then
			    AllArrow_c (Closure,effect,tformals',formals',numfloats,body_c)
			else
			  (perr_k_k (last_k,ckind);
			   perr_c_c (last_c,vcon);
			   (error "Mismatch in closure" handle e => raise e))
		      end
		     | _ => (perr_e_c (Var_e code,code_type);
			     (error "Code pointer in closure of illegal type" handle e => raise e)))
	     in
	       if alpha_equiv_con (con,tipe') then
		 {code=code,cenv=cenv',venv=venv',tipe=tipe'}
	       else
		 (perr_c_c (tipe',con);
		  (error "Type error in closure" handle e => raise e))
	     end
	   
	   val closures' = map2 do_closure (closures,tipes)
	   val defs' = list2set (zip vars closures')
	   val bnd' = Fixclosure_b defs
	 in
	   (bnd',D')
	 end)

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
	      (error ("Encountered undefined variable " ^ (Name.var2string var) 
		     ^ "in exp_valid") handle e => raise e))
	| Const_e value => 
	    let
	      val (value',con,kind) = value_valid (D,value)
	    in
	      (Const_e value',con,singletonize (NONE,kind,con))
	    end
	| Let_e (letsort,bnds,exp) => 
	    let
	      val (bnds',D') = bnds_valid (D,bnds)
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
		 (error "Argument type mismatch in primop" handle e => raise e))
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
			   (error "Application of non-arrow expression" handle e => raise e)))

	      fun check_cons ([],[],[],D) = true
		| check_cons ([],(exp,con,kind)::actuals_f,con'::formals,D) = 
		if alpha_equiv_con (con,con') then
		  check_cons ([],actuals_f,formals,D)
		else
		  (perr_e_c_c (exp,con',con);
		   (error "Formal/actual parameter type mismatch" handle e => raise e))
		| check_cons ((exp,con,kind)::actuals_t,actuals_f,con'::formals,D) = 
		  if alpha_equiv_con (con,con') then
		    check_cons (actuals_t,actuals_f,formals,D)
		  else
		  (perr_e_c_c (exp,con',con);
		   (error "Formal/actual parameter type mismatch" handle e => raise e))
		| check_cons _ = (error "exp_valid: Too few formals in parameter list" handle e => raise e)

	      fun check_kinds ([],[],D) = SOME D
		| check_kinds ((var,kind)::tformals,kind'::actuals,D) = 
		if alpha_sub_kind (kind',kind) then
		  check_kinds (tformals,actuals,insert_kind (D,var,kind))
		else
		  (perr_k_k (kind,kind');
		   (error "Constructor parameter kind mismatch" handle e => raise e))
		| check_kinds (_,_,D) = NONE

	      val params_match = 
		case check_kinds (tformals,kinds,D)
		  of SOME D' =>
		    check_cons (actuals_t,actuals_f,formals,D')
		   | NONE => 
		    (error "Mismatch between formal and actual constructor parameter lists" handle e => raise e)
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
		(error "Error in application - different openness" handle e => raise e)
	    end
	| App_e _ => 
	    (printl "Application expression ";
	     PpNil.pp_exp exp;
	     (error "Illegal application.  Closure with non-var?" handle e => raise e))
	| Raise_e (exp,con) => 
	    let
	      val (con',kind) = con_valid (D,con)
	      val (exp',con'',kind') = exp_valid (D,exp)
	    in
	      if is_exn_con (con'') then
		(exp',con',singletonize (NONE,kind',con'))
	      else
		(perr_e_c (exp',con');
		 (error "Non exception raised - Ill formed expression" handle e => raise e))
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
		      (error "Handler body has incorrect type" handle e => raise e))
		 end
	       | _ => 
		 (print "Body is :\n";
		  PpNil.pp_exp (Handle_e (exp,function));
		  (error "Illegal body for handler" handle e => raise e)))
	       )  (*esac*)

      fun wrap f arg = (f arg) 
	  handle e => (show_stack(); raise e)
      val con_valid = wrap con_valid
      val kind_valid = wrap kind_valid
      val con_reduce = wrap con_reduce
      val kind_reduce = wrap kind_reduce


      fun import_valid' (ImportValue (label,var,con),D) =
	let
	  val (con',kind) = con_valid(D,con)
	  val D' = insert_con(D,var,con')
	in
	  (ImportValue (label,var,con'),D')
	end
	| import_valid' (ImportType (label,var,kind),D) = 
	let
	  val kind' = kind_valid(D,kind)
	  val D' = insert_kind(D,var,kind')
	in
	  (ImportType (label,var,kind'),D')
	end

      fun import_valid (D,import) = import_valid' (import,D)

      fun export_valid (D,ExportValue (label,exp,con)) = 
	let
	  val (exp',found_con,kind) = exp_valid(D,exp)
	  val (con',kind) = con_valid(D,con)
	in
	  if alpha_equiv_con (found_con,con') then
	    ExportValue (label,exp',found_con)
	  else
	    (perr_e_c_c (exp',con',found_con);
	     (error "Type error in value exports of module" handle e => raise e))
	end
	| export_valid (D,ExportType (label,con,kind)) = 
	let
	  val (con',found_kind) = con_valid(D,con)
	  val kind' = kind_valid(D,kind)
	in
	  if alpha_sub_kind (found_kind,kind') then
	    ExportType (label,con',found_kind)
	  else
	    (perr_c_k_k (con',kind',found_kind);
	     (error "Type error in type exports of module" handle e => raise e))
	end

      fun module_valid (D,MODULE {bnds,imports,exports}) = 
	let
	  val (imports',D') = foldl_acc import_valid' D imports
	  val (bnds',D'') = bnds_valid(D',bnds)
	  val exports' = map (curry2 export_valid D'') exports
	in
	  MODULE {bnds=bnds',imports=imports',exports=exports'}
	end

      val module_valid = wrap module_valid
end
