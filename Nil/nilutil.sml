
(* -------------------------------------------------------- *)
(* ------------ Context manipulation functions ------------ *)

functor NilUtilFn(structure Nil : NIL
		  structure IlUtil : ILUTIL) :> 
  sig include NILUTIL sharing Nil = Nil end =
struct
  structure Nil = Nil
    
  open Nil

  fun muExpand _ = raise Util.UNIMP


  fun generate_tuple_symbol (i : int) = Symbol.labSymbol(Int.toString i)
  fun generate_tuple_label (i : int) = Name.symbol_label(generate_tuple_symbol i)
  fun exp_tuple (eclist : (exp * con) list) = 
      let val clist = map #2 eclist
	  val elist = map #1 eclist
	  val labels = Listops.mapcount (fn (i,_) => generate_tuple_label(i+1)) elist
      in Prim_e(NilPrimOp(record labels),clist,elist)
      end
  fun con_tuple clist = 
      let val labels = Listops.mapcount (fn (i,_) => generate_tuple_label(i+1)) clist
      in Prim_c(Record_c labels,clist)
      end
  fun con_tuple_inject clist = 
      let val lc_list = Listops.mapcount (fn (i,c) => (generate_tuple_label(i+1),c)) clist
      in  Crecord_c lc_list
      end
  fun kind_tuple klist = let fun doer(i,k) = ((generate_tuple_label(i+1),Name.fresh_var()),k)
			     val lvk_list = Listops.mapcount doer klist
			 in  Record_k(Util.list2sequence lvk_list)
			 end
  val unitcon = con_tuple []
  val unitexp = exp_tuple []
  val boolcon = Prim_c(Sum_c{tagcount=0w2,known=NONE},[])
  val match_tag = Const_e(Prim.tag(IlUtil.match_tag,unitcon))
  val match_exn = Prim_e(NilPrimOp inj_exn,[unitcon],[match_tag,unitexp])


  (* Local rebindings from imported structures *)

  val member_eq = Listops.member_eq
  val eq_var = Name.eq_var
  val eq_label = Name.eq_label
  val map_second = Listops.map_second
  val foldl_acc = Listops.foldl_acc
  val eq_len = Listops.eq_len
  (**)
    
  fun error s = Util.error "nilutil.sml" s

    (* collections *)
 
    type 'a collection = 'a list

    val emptyCollection = nil
    val collMember = Listops.member_eq
    fun collInsert (lst, x) = x :: lst

    datatype 'a changeopt = NOCHANGE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a
    datatype state =
	STATE of {boundcvars : var collection,
		  boundevars : var collection,
		  conhandler : var collection * con -> con changeopt,
                  exphandler : var collection * exp -> exp changeopt,
		  kindhandler : var collection * kind -> kind changeopt}

    fun stateBindcvar (STATE{boundcvars,boundevars,conhandler,exphandler,kindhandler}) v =
	(STATE{boundcvars = collInsert (boundcvars, v),
	       boundevars = boundevars,
	       conhandler = conhandler,
	       exphandler = exphandler,
	       kindhandler = kindhandler})

    fun stateBindcvars state nil = state
      | stateBindcvars state (v::vs)= stateBindcvars (stateBindcvar state v) vs

    fun stateBindevar (STATE{boundcvars,boundevars,conhandler,exphandler,kindhandler}) v =
	(STATE{boundcvars = boundcvars,
	       boundevars = collInsert (boundevars, v),
	       conhandler = conhandler,
	       exphandler = exphandler,
	       kindhandler = kindhandler})

    fun stateBindevars state nil = state
      | stateBindevars state (v::vs)= stateBindevars (stateBindcvar state v) vs

(*
  type state = ({bound_convar : var list,
		 bound_expvar : var list} *
		{exp_handler : exp * var list -> exp option,
		 con_handler : con * var list -> con option,
		 kind_handler : kind * var list -> kind option})
*)    
  fun add_convars ((STATE{boundcvars,boundevars,conhandler,exphandler,kindhandler}) : state,tv) =
    STATE{boundcvars = tv @ boundcvars,
	  boundevars = boundevars,
	  conhandler = conhandler,
	  exphandler = exphandler,
	  kindhandler = kindhandler}

  fun add_convar (h : state ,v) = add_convars(h,[v])
    
  fun add_var ((STATE{boundcvars,boundevars,conhandler,exphandler,kindhandler}),v) =
      STATE{boundcvars = boundcvars,
	    boundevars = v :: boundevars,
	    conhandler = conhandler,
	    exphandler = exphandler,
	    kindhandler = kindhandler}
    

  fun f_con (state : state) (con : con) : con = 
    let 
      val self = f_con state
      val (STATE{boundcvars,conhandler,...}) = state
      fun do_cbnd (Con_cb(var, kind, con),state) = 
	  let val kind' = f_kind state kind
	      val con' = f_con state con
	  in (Con_cb(var, kind', con'), add_convar(state,var))
	  end
	| do_cbnd (Fun_cb(var, openness, vklist, c, k),state) = 
	  let fun folder((v,k),(vklist,s)) = 
	      let val k' = f_kind state k
		  val s' = add_convar (s,v)
	      in  ((v,k')::vklist,s')
	      end
	      val (rev_vklist',state') = foldl folder ([],state) vklist
	      val c' = f_con state' c
	      val k' = f_kind state' k
	  in (Fun_cb(var, openness, rev rev_vklist', c', k'), add_convar(state,var))
	  end
      fun docon con = 
	  (case con of
	       (Prim_c (pcon,args)) => (Prim_c (pcon,map self args))
	       
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
		   
	     | (Var_c var) => con

	     | (Let_c (letsort, cbnds, body)) => 
		   let
		       fun folder(cbnd,(rev_cbnds,accstate)) = 
			   let val s = (case letsort of
					    Parallel => state
					  | Sequential => accstate)
			       val (cbnd',state') = do_cbnd (cbnd,s)
			   in  (cbnd'::rev_cbnds, state')
			   end
		       val (rev_cbnds',state') = foldl folder ([],state) cbnds
		       val cbnds' = rev rev_cbnds'
		       val body' = f_con state' body
		   in
		       Let_c (letsort, cbnds', body')
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
	       
	     | Typecase_c {arg, arms, default} => 
		   let fun doarm(pc,vklist,c,k) =   
		       let fun folder((v,k),(vklist,s)) = 
			   let val k' = f_kind state k
			       val s' = add_convar (s,v)
			   in  ((v,k')::vklist,s')
			   end
			   val (rev_vklist',state') = foldl folder ([],state) vklist
		       in  (pc, rev rev_vklist', f_con state' c, f_kind state k)
		       end
		       val arms' = map doarm arms
		   in  Typecase_c{arg = self arg,
				  arms = arms',
				  default = Util.mapopt self default}
		   end

	     | (Annotate_c (annot,con)) => 
		   let
		       val con' = self con
		   in
		       Annotate_c (annot, con')
		   end)
    in
	case (conhandler (boundcvars,con)) of
	    CHANGE_NORECURSE c => c
	  | CHANGE_RECURSE c => docon c
	  | NOCHANGE => docon con
    end

  and f_arrow (state : state) ((effect, knds, cons, numfloat, result) : confun) = 
    let
      val con_vars = map (fn (var,_) => var) knds
      val knds' = map (fn (var,kind) => 
		       (var, f_kind state kind)) knds
      val state' = add_convars (state,con_vars)
      val cons'  = map (f_con state') cons
      val result' = f_con state' result
    in
      (effect, knds', cons', numfloat, result')
    end

  and f_kind (state : state) (kind : kind) = 
    let 
      val self = f_kind state
      val (STATE{boundcvars,kindhandler,...}) = state
      fun dokind kind = 
	  (case kind of
	       Type_k _ => kind

	     | Word_k _ => kind

	     | (Singleton_k(p,kind, con)) =>
	      let
		val kind' = self kind
		val con' = f_con state con
	      in
		Singleton_k(p,kind', con')
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
	      Code_k (f_arrow_kind state kindfun))
    in
      case (kindhandler (boundcvars,kind)) of
	CHANGE_NORECURSE k => k
      | CHANGE_RECURSE k => dokind k
      | NOCHANGE => dokind kind
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


  fun f_exp state (exp : exp) : exp = 
      let val self = f_exp state
	  val (STATE{boundevars,exphandler,...}) = state
	  fun dofun(Function(openness,effect,recur,
			     vklist,vclist,vflist,body,con)) = 
	      Function(openness,effect,recur,
		       map (fn (v,k) => (v,f_kind state k)) vklist,
		       map (fn (v,c) => (v,f_con state c)) vclist,
		       vflist,
		       self body,
		       f_con state con)
	  fun dosw {info,arg,arms,default} do_info do_arg do_prearm = 
	      {info = do_info info,
	       arg = do_arg arg,
	       arms = map (fn (t,f) => (do_prearm t, dofun f)) arms,
	       default = Util.mapopt self default}
	  fun identity x = x
	  fun doexp e = 
	      case e of
		  (Var_e _) => e
		| (Const_e _) => e
		| (Let_e (sort,bnds,body)) => 
		      let fun f_bnd bnd =
			  case bnd of
			      Con_b(v,k,c) => Con_b(v,f_kind state k,f_con state c)
			    | Exp_b(v,c,e) => Exp_b(v,f_con state c, self e)
			    | Fixfun_b vfset => 
				  let fun doer(v,f) = (v,dofun f)
				  in  Fixfun_b(Util.mapset doer vfset)
				  end
			    | Fixclosure_b vcset => 
				  let fun doer(v,{code,cenv,venv}) = 
				      (v,{code = (case (exphandler (boundevars,Var_e code)) of
						      NOCHANGE => code
						    | (CHANGE_RECURSE (Var_e v')) => v'
						    | (CHANGE_NORECURSE (Var_e v')) => v'
						    | _ => error "can't have non-var in cllosure code comp"),
				      cenv = f_con state cenv,
				      venv = self venv})
				  in  Fixclosure_b(Util.mapset doer vcset)
				  end
			  val bnds' = map f_bnd bnds
			  val body' = self body
		      in Let_e(sort,bnds',body')
		      end
		| (Prim_e (ap,clist,elist)) => Prim_e(ap,map (f_con state) clist, map self elist)
		| (Switch_e switch) => 
		      Switch_e(case switch of
				   Intsw_e sw => Intsw_e(dosw sw identity self identity)
				 | Sumsw_e sw => Sumsw_e(dosw sw (fn (w,clist) => (w,map (f_con state) clist)) 
							 self identity)
				 | Exncase_e sw => Exncase_e(dosw sw identity self identity)
				 | Typecase_e sw => Typecase_e(dosw sw identity (f_con state) identity))
		| (App_e (func,clist,elist,eflist)) => 
		      App_e(self func,
			    map (f_con state) clist,
			    map self elist, 
			    map self eflist)
		| (Call_e (codevar,clist,elist,eflist)) => 
		      Call_e((case (exphandler (boundevars,Var_e codevar)) of
				  NOCHANGE => codevar
				| (CHANGE_RECURSE (Var_e v')) => v'
				| (CHANGE_NORECURSE (Var_e v')) => v'
				| _ => error "can't have non-var in cllosure code comp"),
		      map (f_con state) clist,
		      map self elist, 
		      map self eflist)
		| Raise_e (e,c) => Raise_e(self e, f_con state c)
		| Handle_e (e1,f) => Handle_e(self e1, dofun f)
      in case (exphandler (boundevars,exp)) of
	  CHANGE_NORECURSE e => e
	| CHANGE_RECURSE e => doexp e
	| NOCHANGE => doexp exp
      end

  val default_bound_convar = []
  val default_bound_expvar = []
  fun default_exp_handler _ = NOCHANGE
  fun default_con_handler _ = NOCHANGE
  fun default_kind_handler _ = NOCHANGE

  (* --------------- Exported Functions -------------------------- *) 

  fun con_free_convar (argcon : con) : var list = 
    let 
      val free : var list ref = ref []
      fun con_handler (bound,Var_c v) = 
	let
	  val _ = if (member_eq(eq_var,v,bound) orelse
		      member_eq(eq_var,v,!free))
		    then ()
		  else free := (v::(!free))
	in 
	  NOCHANGE
	end
      
	| con_handler _ = NOCHANGE

      val handlers = (STATE{boundcvars = default_bound_convar,
			    boundevars = default_bound_expvar,
			    exphandler = default_exp_handler,
			    conhandler = con_handler,
			    kindhandler = default_kind_handler})
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
    fun con_handler conmap (bound,Var_c var) = 
      if (member_eq(eq_var,var,bound)) 
	then NOCHANGE
      else (case (conmap var) of
		NONE => NOCHANGE
	      | SOME c => CHANGE_NORECURSE c)
      | con_handler _ _ = NOCHANGE

    fun exp_handler expmap (bound,Var_e var) = 
      if (member_eq(eq_var,var,bound)) 
	then NOCHANGE
      else (case (expmap var) of
		NONE => NOCHANGE
	      | SOME e => CHANGE_NORECURSE e)
      | exp_handler _ _ = NOCHANGE

    fun cstate conmap = 
      (STATE{boundcvars = default_bound_convar,
	     boundevars = default_bound_expvar,
	     exphandler = default_exp_handler,
	     conhandler = con_handler conmap,
	     kindhandler = default_kind_handler})

    fun estate expmap = 
      (STATE{boundcvars = default_bound_convar,
	     boundevars = default_bound_expvar,
	     exphandler = exp_handler expmap,
	     conhandler = default_con_handler,
	     kindhandler = default_kind_handler})

    fun free_handler() =
	let val free_evars = ref []
	    val free_cvars = ref []
	    fun exp_handler (bound,Var_e v) = 
		(if (not (member_eq(eq_var,v,bound)) andalso not (member_eq(eq_var,v,!free_evars)))
		     then free_evars := v :: (!free_evars)
		 else ();
		     NOCHANGE)
	      | exp_handler _ = NOCHANGE
	    fun con_handler (bound,Var_c v) = 
		(if (not (member_eq(eq_var,v,bound)) andalso not (member_eq(eq_var,v,!free_cvars)))
		     then free_cvars := v :: (!free_cvars)
		 else ();
		     NOCHANGE)
	      | con_handler _ = NOCHANGE
	in
	    (free_evars,
	     free_cvars,
	     STATE{boundcvars = default_bound_convar,
		   boundevars = default_bound_expvar,
		   exphandler = exp_handler,
		   conhandler = con_handler,
		   kindhandler = default_kind_handler})
	end
  in	  
    fun substConInCon conmap = f_con (cstate conmap) 
    fun substConInKind conmap = f_kind (cstate conmap)
    fun substConInExp conmap = f_exp (cstate conmap)
    fun substExpInExp expmap = f_exp (estate expmap)
    fun freeExpVarInExp e = 
	let val (evars_ref,cvars_ref,handler) = free_handler()
	in  (f_exp handler e;
	     !evars_ref)
	end
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
	 | (Sum_c {known=k1,tagcount=t1},Sum_c {known=k2,tagcount=t2}) => (Util.eq_opt (op =,k1,k2)
									   andalso (t1 = t2))
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

  fun same_phase (Compiletime, Compiletime) = true
    | same_phase (Runtime, Runtime) = true
    | same_phase _ = false

  fun alpha_equiv_kind' context (kind1,kind2) = 
    let
      val recur = alpha_equiv_kind' context
    in
      (case (kind1,kind2)
	 of (Type_k p1, Type_k p2) => same_phase(p1,p2)
	  | (Word_k p1, Word_k p2) => same_phase(p1,p2)
	  | (Singleton_k (p1,kind1,con1),Singleton_k (p2,kind2,con2)) => 
		same_phase(p1,p2) andalso
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
	       fun equiv_one (Con_cb(var1,k1,con1),Con_cb(var2,k2,con2),(context,so_far)) = 
		   let
		       val context' =  alpha_vary_var(context,var1,var2)
		   in
		       (context', 
			so_far andalso alpha_equiv_con' context' (con1,con2))
		   end
		 | equiv_one (Fun_cb(var1,openness1,formals1,con1,k1),
			      Fun_cb(var2,openness2,formals2,con2,k2),
			      (context,so_far)) = 
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
		       val context'' =  alpha_vary_var(context',var1,var2)
		   in
		       (context', 
			so_far andalso same_openness (openness1,openness2) andalso
			alpha_equiv_con' context' (con1,con2))
		   end
		 | equiv_one(_,_,(context,_)) = (context,false)
	       val (context',defs_equiv) = 
		   (ListPair.foldl equiv_one (context,true) (binds1,binds2))
	   in
	       (defs_equiv andalso  alpha_equiv_con' context' (con1,con2))
	   end
(*	 
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
*)
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
  
  and alpha_equiv_confun context ((effect1,tformals1,formals1,flength1,return1),
				  (effect2,tformals2,formals2,flength2,return2)) =
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
      (flength1 = flength2) andalso
      alpha_equiv_con' context' (return1,return2)
    end

  and alpha_equiv_con_list context list_pair = 
    eq_len list_pair andalso
    ListPair.all (alpha_equiv_con' context) list_pair

  fun is_word (Word_k _) = true
    | is_word (Type_k _) = false
    | is_word _ = error "Invalid kind for constructor in Singleton kind"

  fun sub_phase(Runtime, Compiletime) = true
    | sub_phase(Runtime, Runtime) = true
    | sub_phase(Compiletime, Compiletime) = true
    | sub_phase(Compiletime, Runtime) = false

  fun alpha_sub_kind' context (k1,k2) = 
    (case (k1,k2)
       of (Word_k p1, Word_k p2) => sub_phase(p1,p2)
	| (Type_k p1, Type_k p2) => sub_phase(p1,p2)
	| (Word_k p1, Type_k p2) => sub_phase(p1,p2)
	| (Singleton_k (p1,_,_) ,Type_k p2) => sub_phase(p1,p2)
	| (Singleton_k (p1,k,c),Word_k p2) => sub_phase(p1,p2) andalso is_word k
	| (Singleton_k (p1,k1,c1),Singleton_k (p2,k2,c2)) => 
	      sub_phase(p1,p2) andalso alpha_equiv_con' context (c1,c2)

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
