
(* -------------------------------------------------------- *)
(* ------------ Context manipulation functions ------------ *)

functor NilUtilFn(structure Nil : NIL) :> 
  sig include NILUTIL sharing Nil = Nil end =
struct
  structure Nil = Nil
    
  open Nil

  (* Local rebindings from imported structures *)

  val member_eq = Listops.member_eq
  val eq_var = Name.eq_var
  val eq_label = Name.eq_label
  val map_second = Listops.map_second
  val foldl_acc = Listops.foldl_acc
  val eq_len = Listops.eq_len
  (**)
    
  fun error s = Util.error "nilutil.sml" s

  type state = ({bound_convar : var list,
		 bound_expvar : var list} *
		{exp_handler : exp * var list -> exp option,
		 con_handler : con * var list -> con option,
		 kind_handler : kind * var list -> kind option})
    
  fun add_convars (({bound_convar,bound_expvar},handlers) : state,tv) =
    ({bound_convar = tv @ bound_convar,
      bound_expvar = bound_expvar},handlers)
    
  fun add_convar (h : state ,v) = add_convars(h,[v])
    
  fun add_var (({bound_convar,bound_expvar},handlers),v) =
    ({bound_convar = bound_convar,
      bound_expvar = v :: bound_expvar},handlers)
    
  fun f_con (state : state) (con : con) : con = 
    let 
      val self = f_con state
      val ({bound_convar,...},
	   {con_handler,...}) = state
    in
      case (con_handler (con,bound_convar)) of
	SOME c => c
      | NONE =>
	  (case con
	     of (Prim_c (pcon,args)) => (Prim_c (pcon,map self args))
	       
	      | (Mu_c (defs,var)) =>
	       let
		 val (con_vars,cons) = ListPair.unzip (Util.set2list defs)
		 val state' = add_convars (state,con_vars)
		 val cons' = List.map (f_con state') cons
		 val defs' = Util.list2set (ListPair.zip (con_vars,cons'))
	       in
		 (Mu_c (defs',var))
	       end
	     
	      | (Arrow_c (openness,confun)) =>
	       Arrow_c (openness,f_arrow state confun)

	      | (Code_c confun) =>
	       Code_c (f_arrow state confun)

	      | (v as (Var_c var)) => v

	      | (Let_c (Parallel, bnds,body)) =>
	       let
		 val cvars  = map (fn (cvar, _) => cvar) bnds
		 val bnds'  = map (fn (var, con) => (var, self con)) bnds
		 val state' = add_convars (state, cvars)
		 val body'  = f_con state' body
	       in
		 Let_c (Parallel, bnds', body')
	       end

	      | (Let_c (Sequential, bnds, body)) => 
	       let
		 val (bnds',state') =
		   let 
		     fun fold_one ((cvar, con),state) =
		       let
			 val con'   = f_con state con
			 val state' = add_convar (state,cvar)
		       in
			 ((cvar,con'),state')
		       end
		   in
		     foldl_acc fold_one state bnds
		   end
		 val body' = f_con state' body
	       in
		 Let_c (Sequential, bnds', body')
	       end

	      | (Fun_c (openness,formals,body)) =>
	       let
		 val (args',state') =
		   let 
		     fun fold_one ((var,knd),state) = 
		       let
			 val knd' = f_kind state knd
			 val state' = add_convar (state,var)
		       in
			 ((var,knd'),state')
		       end
		   in
		     foldl_acc fold_one state formals
		   end
		 val body' = f_con state' body
	       in
		 Fun_c(openness, args', body')
	       end

	      | (Closure_c (code,env)) =>
	       let
		 val code' = f_con state code
		 val env' = f_con state env
	       in
		 Closure_c(code', env')
	       end

	      | (Crecord_c entries) =>
	       let
		 val entries' = map_second self entries
	       in
		 Crecord_c entries'
	       end

	      | (Proj_c (con,lbl)) =>
	       let
		 val con' = self con
	       in
		 Proj_c (con', lbl)
	       end

	      | (App_c (cfun,actuals)) =>
	       let
		 val cfun' = self cfun
		 val actuals' = map self actuals
	       in
		 App_c (cfun', actuals')
	       end

	      | (Annotate_c (annot,con)) => 
	       let
		 val con' = self con
	       in
		 Annotate_c (annot, con')
	       end)
    end

  and f_arrow (state : state) ((effect, knds, cons, result) : confun) = 
    let
      val con_vars = map (fn (var,_) => var) knds
      val knds' = map (fn (var,kind) => 
		       (var, f_kind state kind)) knds
      val state' = add_convars (state,con_vars)
      val cons'  = map (f_con state') cons
      val result' = f_con state' result
    in
      (effect, knds', cons', result')
    end

  and f_kind (state : state) (kind : kind) = 
    let 
      val self = f_kind state
      val ({bound_convar,...},
	   {kind_handler,...}) = state
    in
      case (kind_handler (kind,bound_convar)) of
	SOME c => c
      | NONE =>
	  case kind
	    of Type_k => kind

	     | Word_k => kind

	     | (Singleton_k(kind, con)) =>
	      let
		val kind' = self kind
		val con' = f_con state con
	      in
		Singleton_k(kind', con')
	      end

	     | (Record_k fieldseq) =>
	      let
		fun fold_one (((lbl,var),knd),state) = 
		  let
		    val kind'  = self kind
		    val state' = add_convar (state,var)
		  in
		    (((lbl, var), kind'),state')
		  end
		val field_list = Util.sequence2list fieldseq
		val (field_list',state') = 
		  foldl_acc fold_one state field_list
	      in
		Record_k (Util.list2sequence field_list')
	      end

	     | (Arrow_k (openness, args, result)) =>
	      let
		val (args', result') = f_arrow_kind state (args, result)
	      in
		Arrow_k (openness, args, result)
	      end

	     | (Code_k kindfun) =>
	      Code_k (f_arrow_kind state kindfun)
    end
  and f_arrow_kind state (args, result) =
    let
      val (args',state') =
	let 
	  fun fold_one ((var,knd),state) = 
	    let
	      val knd' = f_kind state knd
	      val state' = add_convar (state, var)
	    in
	      ((var,knd'),state')
	    end
	in
	  foldl_acc fold_one state args
	end
      val result' = f_kind state' result
    in
      (args', result')
    end


  val default_bound_convar = []
  val default_bound_expvar = []
  val default_bound = {bound_convar = default_bound_convar,
		       bound_expvar = default_bound_expvar}
  fun default_exp_handler _ = NONE
  fun default_con_handler _ = NONE
  fun default_kind_handler _ = NONE

  (* Exported Functions *) 

  fun con_free_convar (argcon : con) : var list = 
    let 
      val free : var list ref = ref []
      fun con_handler (Var_c v,bound) = 
	let
	  val _ = if (member_eq(eq_var,v,bound) orelse
		      member_eq(eq_var,v,!free))
		    then ()
		  else free := (v::(!free))
	in 
	  NONE 
	end
      
	| con_handler _ = NONE

      val handlers = (default_bound,
		      {exp_handler = default_exp_handler,
		       con_handler = con_handler,
		       kind_handler = default_kind_handler})
      val _ = f_con handlers argcon
    in !free
    end

  fun convar_occurs_free (var : var, con : con) : bool = 
    let
      val free_vars = con_free_convar con
    in
      List.exists (fn v => eq_var (v,var)) free_vars
    end

  local 
    fun con_handler conmap (Var_c var,bound) = 
      if (member_eq(eq_var,var,bound)) 
	then NONE
      else conmap var
      | con_handler _ _ = NONE

    fun state conmap = 
      (default_bound,
       {exp_handler = default_exp_handler,
	con_handler = con_handler conmap,
	kind_handler = default_kind_handler})
  in	  
    fun substConInCon conmap = f_con (state conmap) 

    fun substConInKind conmap = f_kind (state conmap)
  end

  fun same_openness (Open,Open) = true
    | same_openness (Closed,Closed) = true
    | same_openness _ = false

  fun same_effect (Total,Total) = true
    | same_effect (Partial,Partial) = true
    | same_effect _ = false


  (*Pre: Records are sorted by label *)
  fun primequiv (pcon1,pcon2) = 
    let
      fun same_int_size (size1,size2) = 
	(case (size1,size2)
	   of (Prim.W8,Prim.W8) => true
	    | (Prim.W16,Prim.W16) => true
	    | (Prim.W32,Prim.W32) => true
	    | (Prim.W64,Prim.W64) => true
	    | _ => false)

      fun same_float_size (size1,size2) =
	(case (size1,size2)
	   of (Prim.F32,Prim.F32) => true
	    | (Prim.F64,Prim.F64) => true
	    | _ => false)
    in
      case (pcon1,pcon2)
	of (Int_c size1,Int_c size2) => same_int_size (size1,size2)
	| ((Float_c size1,Float_c size2) |
	   (BoxFloat_c size1,BoxFloat_c size2)) => 
	  same_float_size (size1,size2)
	| ((Exn_c,Exn_c) |
	   (Array_c,Array_c) |
	   (Vector_c,Vector_c) |
	   (Ref_c,Ref_c) |
	   (Exntag_c,Exntag_c)) => true
	 | (Sum_c i,Sum_c j) => Util.eq_opt (op =,i,j)
	 | (Record_c fields1,Record_c fields2) => 
	  (List.length fields1 = List.length fields2) andalso
	  ListPair.all eq_label (fields1,fields2)
	 | (Vararg_c (openness1,effect1),Vararg_c (openness2,effect2)) => 
	  (same_openness (openness1,openness2) andalso
	   same_effect (effect1,effect2))
	 | _  => false
    end
  structure VarMap = Name.VarMap

  type varmap = var VarMap.map

  type alpha_context = (var VarMap.map * var VarMap.map)

  val bound : varmap * var -> bool = isSome o VarMap.find

  fun subst (bindings,var) = (getOpt (VarMap.find (bindings,var),var))

  val bind : varmap * var * var -> varmap = VarMap.insert 

  val empty_context : alpha_context = (VarMap.empty,VarMap.empty)

  fun alpha_vary_var ((subst1,subst2) : alpha_context, var1, var2) = 
    let
      val equal = eq_var (var1,var2)
      val var = if equal then var1
		else Name.fresh_var()
      val context' = 
	if not equal then 
	  (bind(subst1,var1,var),bind(subst2,var2,var))
	else 
	  (if bound(subst1,var1) then bind(subst1,var1,var) else subst1,
	     if bound(subst2,var2) then bind(subst2,var2,var) else subst2)
    in
      (context')
    end

  fun alpha_vary_var_list (context : alpha_context, var_list1,var_list2) =
    let
      fun vary_one  (var1,var2,context) = 
	alpha_vary_var (context,var1,var2)
    in
      ListPair.foldl vary_one context (var_list1,var_list2) 
    end
  
  fun subst_eq ((subst1,subst2),var1,var2) = 
    eq_var (subst (subst1,var1),subst (subst2,var2))

  fun alpha_equiv_kind' context (kind1,kind2) = 
    let
      val recur = alpha_equiv_kind' context
    in
      (case (kind1,kind2)
	 of (Type_k,Type_k) => true
	  | (Word_k,Word_k) => true
	  | (Singleton_k (kind1,con1),Singleton_k (kind2,con2)) => 
	   recur (kind1,kind2) andalso
	   alpha_equiv_con' context (con1,con2)

	  (* Need to check all orderings? *)
	  | (Record_k elts1,Record_k elts2) => 
	   let
	     fun equiv_one (((lbl1,var1),kind1),((lbl2,var2),kind2),
			    (context,so_far)) = 
	       let
		 val context' = alpha_vary_var (context,var1,var2)
		 val still = 
		   so_far andalso 
		   eq_label (lbl1,lbl2) andalso
		   alpha_equiv_kind' context (kind1,kind2)
	       in
		 (context',still)
	       end
	   in
	     eq_len (elts1,elts2) andalso 
	     (#2(ListPair.foldl equiv_one (context,true) (elts1,elts2)))
	   end

       | (Arrow_k (openness1, formals1, return1),
	   Arrow_k (openness2, formals2, return2)) =>
	   same_openness (openness1,openness2) andalso 
	   alpha_equiv_arrow_kind context ((formals1,return1),
					   (formals1,return2))
	 | (Code_k (formals1, return1),
	    Code_k (formals2, return2)) => 
	   alpha_equiv_arrow_kind context ((formals1,return1),
					   (formals1,return2))
	  | _ => false)
    end
  and alpha_equiv_arrow_kind context ((formals1, return1),
				      (formals2, return2)) = 
    let
      fun equiv_one ((var1,kind1),(var2,kind2),
		     (context,so_far)) = 
	let
	  val context' = alpha_vary_var (context,var1,var2)
	  val still = 
	    so_far andalso 
	    alpha_equiv_kind' context (kind1,kind2)
	in
	  (context',still)
	end
      val (context',all_equiv) = 
	(ListPair.foldl equiv_one (context,true) (formals1,formals2))
    in
      all_equiv andalso 
      eq_len (formals1,formals2) andalso 
      alpha_equiv_kind' context' (return1,return2)
    end

  and alpha_equiv_con' context (con1,con2) = 
    let
      val recur = alpha_equiv_con' context
    in
      (case (con1,con2)
	 of (Prim_c (pcon1,args1),Prim_c (pcon2,args2)) => 
	   primequiv (pcon1,pcon2) andalso
	   alpha_equiv_con_list context (args1,args2)
	   
	  (*Assume - sets must maintain ordering!  We only judge*)
	  (* mus with the same ordering to be equiv *) 
	  | (Mu_c (defs1,var1),Mu_c (defs2,var2)) =>
	   let
	     val def_list1 = Util.set2list defs1
	     val def_list2 = Util.set2list defs2
	     val (var_list1,con_list1) = ListPair.unzip def_list1
	     val (var_list2,con_list2) = ListPair.unzip def_list2
	     val context' = 
	       (alpha_vary_var_list (context,var_list1,var_list2))
	   in
	     subst_eq (context',var1,var2) andalso
	     alpha_equiv_con_list context' (con_list1,con_list2)
	   end

	  | (Arrow_c (openness1,confun1),Arrow_c (openness2,confun2)) => 
	   same_openness(openness1,openness2) andalso
	   alpha_equiv_confun context (confun1,confun2)

	  | (Code_c confun1,Code_c confun2) => 
	   alpha_equiv_confun context (confun1,confun2)

	  | (Var_c var1,Var_c var2) => 
	   subst_eq(context,var1,var2)

	  (* Ignore sort or not??? *)
	  | (Let_c (sort1, binds1,con1),Let_c (sort2, binds2,con2)) => 
	   let 
	     fun equiv_one ((var1,con1),(var2,con2),(context,so_far)) = 
	       let
		 val context' =  alpha_vary_var(context,var1,var2)
	       in
		 (context', 
		  so_far andalso alpha_equiv_con' context' (con1,con2))
	       end
	     val (context',defs_equiv) = 
	       (ListPair.foldl equiv_one (context,true) (binds1,binds2))
	   in
	     (defs_equiv andalso  alpha_equiv_con' context' (con1,con2))
	   end
	 
       | (Fun_c (openness1,formals1,body1),
	   Fun_c (openness2,formals2,body2)) => 
	   let 
	     fun equiv_one ((var1,kind1),(var2,kind2),(context,so_far)) = 
	       let
		 val context' = alpha_vary_var(context,var1,var2)
		 val still =  
		   so_far andalso 
		   alpha_equiv_kind' context' (kind1,kind2)
	       in
		 (context',still)
	       end
	     val (context',formals_equiv) = 
	       (ListPair.foldl equiv_one (context,true) (formals1,formals2))
	   in
	     same_openness (openness1,openness2) andalso
	     formals_equiv andalso 
	     (alpha_equiv_con' context' (body1,body2))
	   end

	 | (Closure_c (code1,env1),Closure_c (code2,env2)) => 
	   recur (code1,code2) andalso recur (env1,env2)

	 (* Cannot be dependent.  Note Precondition is sorted labels*)
	 | (Crecord_c entries1,Crecord_c entries2) => 
	   let
	     fun equiv_each ((lbl1,con1),(lbl2,con2)) = 
	       recur (con1,con2)
	   in
	     eq_len (entries1,entries2) andalso 
	     (ListPair.all equiv_each (entries1,entries2))
	   end

	 | (Proj_c (crec1,label1),Proj_c (crec2,label2)) => 
	   eq_label (label1,label2) andalso
	   recur (crec1,crec2)

	 | (App_c (cfun1,actuals1),App_c (cfun2,actuals2)) => 
	   recur (cfun1,cfun2) andalso
	   (ListPair.all recur (actuals1,actuals2))

	 | (Annotate_c (annot1,con1),Annotate_c (annot2,con2)) => 
	   recur (con1,con2)
	 | _ => false)
    end
  
  and alpha_equiv_confun context ((effect1,tformals1,formals1,return1),
				  (effect2,tformals2,formals2,return2)) =
    let
      fun tformal_equiv ((var1,kind1),(var2,kind2),(context,so_far)) = 
	let
	  val context' = alpha_vary_var (context,var1,var2)
	in
	  (context',(so_far andalso 
		     alpha_equiv_kind' context (kind1,kind2)))
	end
      val same_len = eq_len (tformals1,tformals2)
      val (context',t_equiv) = 
	ListPair.foldl tformal_equiv (context,same_len) (tformals1,tformals2)
    in
      t_equiv andalso 
      alpha_equiv_con_list context' (formals1,formals2) andalso
      alpha_equiv_con' context' (return1,return2)
    end

  and alpha_equiv_con_list context list_pair = 
    eq_len list_pair andalso
    ListPair.all (alpha_equiv_con' context) list_pair

  fun is_word (Word_k) = true
    | is_word (Type_k) = false
    | is_word _ = error "Invalid kind for constructor in Singleton kind"
    
  fun alpha_sub_kind' context (k1,k2) = 
    (case (k1,k2)
       of (Word_k,Word_k) => true
	| (Type_k,Type_k) => true
	| (Word_k,Type_k) => true
	| (Singleton_k _ ,Type_k) => true
	| (Singleton_k (k,c),Word_k) => is_word k
	| (Singleton_k (k1,c1),Singleton_k (k2,c2)) => 
	 alpha_equiv_con' context (c1,c2)

       | (Arrow_k (openness1, formals1, return1),   
	  Arrow_k (openness2, formals2, return2)) => 
	 (same_openness (openness1,openness2) andalso 
	  alpha_sub_arrow_kind context ((formals1,return1),
					(formals1,return2)))

	| (Code_k (formals1, return1), Code_k (formals2, return2)) => 
	 alpha_sub_arrow_kind context ((formals1,return1),
				       (formals1,return2))

	| (Record_k elts1,Record_k elts2) => 
	 let
	   fun sub_one (((lbl1,var1),kind1),((lbl2,var2),kind2),
			(context,so_far)) = 
	     let
	       val context' = alpha_vary_var (context,var1,var2)
	       val still = 
		 so_far andalso 
		 eq_label (lbl1,lbl2) andalso
		 alpha_sub_kind' context (kind1,kind2)
	     in
	       (context',still)
	     end
	 in
	   eq_len (elts1,elts2) andalso 
	   (#2(ListPair.foldl sub_one (context,true) (elts1,elts2)))
	 end
	| (_,_) => false)

  and alpha_sub_arrow_kind context ((formals1, return1),
				    (formals2, return2)) = 
    let
      fun sub_one ((var1,kind1),(var2,kind2),
		   (context,so_far)) = 
	let
	  val context' = alpha_vary_var (context,var1,var2)
	  val still = 
	    so_far andalso alpha_sub_kind' context (kind1,kind2)
	in
	  (context',still)
	end
      val (context',all_equiv) = 
	(ListPair.foldl sub_one (context,true) (formals1,formals2))
    in
      all_equiv andalso 
      eq_len (formals1,formals2) andalso 
      alpha_sub_kind' context' (return1,return2)
    end


  val alpha_equiv_con = alpha_equiv_con' empty_context

  val alpha_equiv_kind = alpha_equiv_kind' empty_context

  val alpha_sub_kind = alpha_sub_kind' empty_context
(* End exported functions *)
end;
