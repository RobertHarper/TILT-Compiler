functor NilSubstFn(structure Nil : NIL
		   structure PpNil : PPNIL
		   sharing Nil = PpNil.Nil) :> 
  NILSUBST where type exp = Nil.exp and type con = Nil.con and type kind = Nil.kind = 
  struct

    type exp = Nil.exp
    type con = Nil.con
    type kind = Nil.kind
    type var = Name.var
    open Nil

    val debug = ref false
    val substituted = ref 0
    val short_circuited = ref 0

    fun inc int_ref = int_ref := !int_ref + 1

    fun get_stats () = {substituted=(!substituted),short_circuited=(!short_circuited)}
    fun reset_stats () = (substituted := 0;short_circuited := 0)

    val foldl_acc = Listops.foldl_acc
    val map_second = Listops.map_second
    val unzip = Listops.unzip
    val zip = Listops.zip
    val mapopt = Util.mapopt
    val set2list = Util.set2list
    val list2set = Util.list2set
    fun error s = Util.error "subst.sml" s

    local
      open LargeWord
    in
      val var2word = fromInt o Name.var2int

      val zero = fromInt 0
      structure VarMap = Name.VarMap

      type 'a subst = {test:word,
		       subst : 'a VarMap.map}
      (* test -> the bitwise or of the integer version of the elements
       *   invariant -> test = VarMap.foldli (fn (v,_,a) => orb (var2int v,a)) zero subst
       *      that is, test is the union of the set bits in the domain of the map
       *    Note that if is non zero, then
       *      there is a bit set in test that is not set in any element of the domain of
       *      the map, and hence v is not in the domain of the map.
       *    This is a simple optimization for the common case when the map is small.
       * subst -> the mapping from variables to values
       *)

      fun empty () : 'a subst = 
	{test = notb zero,
	 subst = VarMap.empty}
	
      fun fromList list : 'a subst = 
	let
	  fun fold ((var,value),(test,subst)) = 
	    (orb (test,(var2word var)),VarMap.insert(subst,var,value))
	  val (test',subst) = List.foldl fold (zero,VarMap.empty) list
	  val test = notb test'
	in
	  {test = test,
	   subst = subst}
	end

      fun add {test,subst} (var,value) : 'a subst = 
	let
	  val test = (andb (test,notb (var2word var)))
	  val subst = VarMap.insert (subst,var,value)
	in
	  {test = test,
	   subst = subst}
	end

      fun substitute {test,subst} var = 
	if (andb (test,var2word var)) = zero then
	  (inc substituted;VarMap.find (subst,var))
	else 
	  (inc substituted;inc short_circuited;NONE)

      fun is_empty {test,subst} = (VarMap.numItems subst) = 0

      (* val compose : ('a subst -> 'a -> 'a) -> ('a subst * 'a subst) -> 'a subst *
       *  subst_fn (compose subst_fn (subst2,subst1))
       *  is equivalent to (subst_fn subst2) o (subst_fn subst1)
       *)
      fun compose subst_fn (map1 as {test = test1,subst = subst1},
			    map2 as {test = test2,subst = subst2}) = 
	let
	  val subst2 = VarMap.map (subst_fn map1) subst2
	  fun combine (value1,value2) = value2
	  val subst = VarMap.unionWith combine (subst1,subst2)
	  val test = andb (test1,test2)
	in
	  {test = test, subst = subst}
	end
    end


    fun rebind Con (var,subst) = 
      let
	val var' = Name.derived_var var
      in
	(*It is sufficient here to add instead of compose
	 * because we are guaranteed that nothing in the domain of
	 * subst appears in the range of the things added
	 *)
	(var', add subst (var,Con var'))
      end

    fun rebind_list rebind (vars,subst) = 
      Listops.foldl_acc rebind subst vars

    val con_rebind = rebind Var_c
    val con_rebind_list = rebind_list con_rebind
    val exp_rebind = rebind Var_e
    val exp_rebind_list = rebind_list exp_rebind

    fun con_var_replace (subst,var) = 
      (case substitute subst var
	 of SOME (Nil.Var_c var) => var
	  | _ => var)

    fun substConInTFormals (conmap : con subst) (formals : (var * kind) list) = 
      let
	fun fold_one ((var,kind),conmap) =
	  let
	    val kind = substConInKind' conmap kind
	    val (var,conmap) = con_rebind (var,conmap)
	  in
	    ((var,kind),conmap)
	  end
      in
	foldl_acc fold_one conmap formals
      end
      
    and substConInKind' (conmap : con subst) (kind : kind) = 
      (case kind of
	 Type_k _ => kind
       | Word_k _ => kind
       | (Singleton_k(p,kind, con)) =>
	   let
	     val kind = substConInKind' conmap kind
	     val con = substConInCon' conmap con
	   in
	     Singleton_k(p,kind, con)
	   end
	 
       | (Record_k fieldseq) =>
	   let
	     fun fold_one (((lbl,var),kind),conmap) = 
	       let
		 val kind  = substConInKind' conmap kind
		 val (var,conmap) = con_rebind (var,conmap)
	       in
		 (((lbl, var), kind),conmap)
	       end
	     val field_list = Util.sequence2list fieldseq
	     val (field_list,conmap) = 
	       foldl_acc fold_one conmap field_list
	   in
	     Record_k (Util.list2sequence field_list)
	   end

       | (Arrow_k (openness, args, result)) =>
	   let
	     val (args,conmap) = substConInTFormals conmap args
	     val result = substConInKind' conmap result
	   in
	     Arrow_k (openness,args, result)
	   end)

    and substConInCon' (conmap : con subst) (con : con) = 
      let
	fun print_one (v,c) =
	  (print ((Name.var2string v)^"->");
	   PpNil.pp_con c)
	val _ = 
	  if !debug then
	    (Util.lprintl "Substituting map :";
	     VarMap.appi print_one (#subst conmap);
	     Util.lprintl "Into constructor :";
	     PpNil.pp_con con;
	     Util.printl "")
	  else ()
      in
      (case con 
	 of (Prim_c (pcon,args)) => 
	   (Prim_c (pcon,map (substConInCon' conmap) args))
	  | (Mu_c (defs,var)) =>
	   let
	     val (vars,cons) = unzip (set2list defs)
	     val (vars,conmap) = con_rebind_list (vars,conmap)
	     val cons = List.map (substConInCon' conmap) cons
	     val var = con_var_replace (conmap,var)
	     val defs = Util.list2set (zip vars cons)
	   in
	     (Mu_c (defs,var))
	   end
	  | (AllArrow_c (openness,effect,tformals,formals,flength,return)) =>
	   let
	     val (tformals,conmap) = substConInTFormals conmap tformals
	     val formals = map (substConInCon' conmap) formals
	     val return = substConInCon' conmap return
	   in
	     AllArrow_c (openness,effect,tformals,formals,flength,return)
	   end

	  | (Var_c var) => 
	   (case substitute conmap var
	      of SOME con => con
	       | _ => con)

	  | (Let_c (letsort, cbnds, body)) => 
	   let

	     fun do_confun Con conmap1 (var,formals,body,kind) = 
	       let
		 val (formals,conmap) = substConInTFormals conmap1 formals
		 val body = substConInCon' conmap body
		 val kind = substConInKind' conmap kind
		 val (var,conmap) = con_rebind (var,conmap1) (*Not conmap!!*)
	       in
		 (Con (var,formals,body,kind),conmap)
	       end
	     fun folder (cbnd,conmap) = 
	       case cbnd 
		 of Con_cb (var,kind,con) =>
		   let 
		     val kind = substConInKind' conmap kind
		     val con = substConInCon' conmap con
		     val (var,conmap) = con_rebind (var,conmap)
		     val cbnd = Con_cb (var,kind,con)
		   in  
		     (cbnd,conmap)
		   end
		  | Open_cb body => do_confun Open_cb conmap body
		  | Code_cb body => do_confun Code_cb conmap body

	     val (cbnds,conmap) = foldl_acc folder conmap cbnds
	     val body = substConInCon' conmap body
	   in
	     Let_c (letsort, cbnds, body)
	   end
	 
	  | (Closure_c (code,env)) =>
	   let
	     val code = substConInCon' conmap code
	     val env = substConInCon' conmap env
	   in
	     Closure_c(code, env)
	   end
	 
	  | (Crecord_c entries) =>
	   Crecord_c (map (fn (l,c) => (l,substConInCon' conmap c)) entries)
	   
	  | (Proj_c (con,lbl)) =>
	   let
	     val con = substConInCon' conmap con
	   in
	     Proj_c (con, lbl)
	   end
	 
	  | (App_c (cfun,actuals)) =>
	   let
	     val cfun = substConInCon' conmap cfun
	     val actuals = map (substConInCon' conmap) actuals
	   in
	     App_c (cfun, actuals)
	   end
	 
	  | Typecase_c {arg, arms, default, kind} => 
	   let 
	     val arg = substConInCon' conmap arg
	     fun doarm (pcon,args,body) = 
	       let
		 val (args,conmap) = substConInTFormals conmap args
		 val body = substConInCon' conmap body
	       in
		 (pcon,args,body)
	       end
	     val arms = map doarm arms
	     val default = substConInCon' conmap default
	     val kind = substConInKind' conmap kind
	   in  Typecase_c{arg = arg,
			  arms = arms,
			  default = default,
			  kind = kind}
	   end

	  | (Annotate_c (annot,con)) => 
	   let
	     val con = substConInCon' conmap con
	   in
	     Annotate_c (annot, con)
	   end)
      end
    fun id x = x
      
    fun substConInCon conmap = 
      if is_empty conmap then 
	id 
      else
	substConInCon' conmap

    fun substConInKind conmap = 
      if is_empty conmap then 
	id 
      else
	substConInKind' conmap

    fun substExpConInExp' (maps as (expmap : exp subst,conmap : con subst))
      (exp : exp) : exp = 
      (case exp
	 of Var_e var => 
	   (case substitute expmap var
	      of SOME exp => exp
	       | _ => exp)
	  | Const_e value => 
	      Const_e (substExpConInValue' maps value)
	  | Let_e (letsort,bnds,exp) => 
	      let
		val (bnds,maps) = 
		  substExpConInBnds' maps bnds
		val exp = substExpConInExp' maps exp
	      in
		(Let_e (letsort,bnds,exp))
	      end
	  | Prim_e (allprim,cons,exps) => 
	      let
		val cons = map (substConInCon' conmap) cons
		val exps = map (substExpConInExp' maps) exps
	      in
		(Prim_e (allprim,cons,exps))
	   end
	  | Switch_e switch => Switch_e (substExpConInSwitch' maps switch)
	  | App_e (openness,exp,cons,exps,floats) =>
	   let
	     val exp = substExpConInExp' maps exp
	     val cons = map (substConInCon' conmap) cons
	     val exps = map (substExpConInExp' maps) exps
	     val floats = map (substExpConInExp' maps) floats
	   in
	     App_e (openness,exp,cons,exps,floats)
	   end
	  | Raise_e (exp,con) =>
	   let 
	     val con = substConInCon' conmap con
	     val exp = substExpConInExp' maps exp
	   in
	     Raise_e (exp,con)
	   end
	  | Handle_e (exp,function) =>
	   let
	     val exp = substExpConInExp' maps exp
	     val function = substExpConInFunction' maps function
	   in
	     Handle_e (exp,function)
	   end)
    and substExpConInValue' 
      (maps as (expmap : exp subst,conmap : con subst)) value = 
      (case value
	 of ((Prim.int _) | (Prim.uint _) |(Prim.float _)) => value
	  | Prim.array (con,arr) => 
	   let
	     val con = substConInCon' conmap con
	   in
	     Array.modify (substExpConInExp' maps) arr;
	     Prim.array (con,arr)
	   end
	| Prim.vector (con,vec) => 
	   let
	     val con = substConInCon' conmap con
	   in
	     Array.modify (substExpConInExp' maps) vec;
	     Prim.vector (con,vec)
	   end
	| Prim.refcell expref =>
	 let
	   val exp = substExpConInExp' maps (!expref)
	 in
	   expref := exp;
	   Prim.refcell expref
	 end
	| Prim.tag (atag,con) => 
	 let
	   val con = substConInCon' conmap con
	 in
	   Prim.tag (atag,con)
	 end)
    and substExpConInBnds' maps bnds =
      foldl_acc substExpConInBnd' maps bnds
    and substExpConInBnd' (bnd,maps as (expmap,conmap)) = 
      (case bnd 
	 of Con_b (var, kind, con) =>
	   let
	     val kind = substConInKind conmap kind
	     val con = substConInCon' conmap con
	     val (var,conmap) = con_rebind (var,conmap)
	     val bnd = (Con_b (var,kind,con))
	   in
	     (bnd,(expmap,conmap))
	   end
	  | Exp_b (var, con, exp) =>
	   let
	     val con = substConInCon' conmap con
	     val exp = substExpConInExp' maps exp
	     val (var,expmap) = exp_rebind (var,expmap)
	     val bnd = (Exp_b (var,con,exp))
	   in
	     (bnd,(expmap,conmap))
	   end
	  | ((Fixopen_b defs) | (Fixcode_b defs)) =>
	   let
	     val (vars,functions) = unzip (set2list defs)
	     val (vars,expmap) = exp_rebind_list (vars,expmap)
	     val functions = 
	       map (substExpConInFunction' (expmap,conmap)) functions
	     val defs = list2set (zip vars functions)
	     val bnd = 
	       (case bnd 
		  of Fixopen_b _ => (Fixopen_b defs)
		   | _ => (Fixcode_b defs))
	   in
	     (bnd,(expmap,conmap))
	   end
	  | Fixclosure_b defs => 
	   let
	     val (vars,closures) = unzip (set2list defs)
	     val (vars,expmap) = exp_rebind_list (vars,expmap)
	     val closures = map (substExpConInClosure' (expmap,conmap)) closures
	     val defs = list2set (zip vars closures)
	     val bnd = Fixclosure_b defs
	   in
	     (bnd,(expmap,conmap))
	   end)
    and substExpConInSwitch'
      (maps as (expmap : exp subst, conmap : con subst)) switch = 
      (case switch
	 of Intsw_e {info=intsize,arg,arms,default} =>
	   let
	     val arg = substExpConInExp' maps arg
	     val arms = map_second (substExpConInFunction' maps) arms
	     val default = mapopt (substExpConInExp' maps) default
	   in
	     Intsw_e {info=intsize,arg=arg,
		      arms=arms,default=default}
	   end
	  | Sumsw_e {info=(tagcount,decl_cons),arg,arms,default} => 
	   let
	     val arg = substExpConInExp' maps arg
	     val arms = map_second (substExpConInFunction' maps) arms
	     val decl_cons = map (substConInCon' conmap) decl_cons
	     val default = mapopt (substExpConInExp' maps) default
	   in
	     Sumsw_e {info=(tagcount,decl_cons),arg=arg,
		      arms=arms,default=default}
	   end
	  | Exncase_e {info,arg,arms,default} =>
	   let
	     val arg = substExpConInExp' maps arg
	     fun mapper (exp,function) = 
	       let
		 val exp = substExpConInExp' maps exp
		 val function = substExpConInFunction' maps function
	       in
		 (exp,function)
	       end
	     val arms = map mapper arms
	     val default = mapopt (substExpConInExp' maps) default
	   in
	     Exncase_e {info=(),arg=arg,
			arms=arms,default=default}
	   end
	  | Typecase_e {info,arg,arms,default} =>
	   let
	     val arg = substConInCon' conmap arg
	     val arms = map_second (substExpConInFunction' maps) arms
	     val default = mapopt (substExpConInExp' maps) default
	   in
	     Typecase_e {info=(),arg=arg,
			 arms=arms,default=default}
	   end)
    and substExpConInFunction' (maps as (expmap : exp subst,conmap : con subst))
      (Function (effect,recursive,tformals,formals,fformals,body,return)) = 
      let
	fun con_folder ((var,kind),conmap) = 
	  let
	    val kind = substConInKind' conmap kind
	    val (var,conmap) = con_rebind (var,conmap)
	  in
	    ((var,kind),conmap)
	  end
	val (tformals,conmap) = 
	  foldl_acc con_folder conmap tformals

	fun exp_folder ((var,con),expmap) = 
	  let
	    val con = substConInCon' conmap con
	    val (var,expmap) = exp_rebind (var,expmap)
	  in
	    ((var,con),expmap)
	  end
	val (formals,expmap) = 
	  foldl_acc exp_folder expmap formals
	val (fformals,expmap) = exp_rebind_list (fformals,expmap)
	val body = substExpConInExp' (expmap,conmap) body
	val return = substConInCon' conmap return
      in
	Function (effect,recursive,tformals,formals,fformals,body,return)
      end
    and substExpConInClosure' (maps as (expmap : exp subst,conmap : con subst))
      {code:var, cenv:con, venv:exp, tipe:con} = 
      let
	val code = 
	  (case substitute expmap code 
	     of SOME (Var_e var) => var
	      | _ => code)
	val cenv = substConInCon' conmap cenv
	val venv = substExpConInExp' maps venv
	val tipe = substConInCon' conmap tipe
      in
	{code=code,cenv=cenv,venv=venv,tipe=tipe}
      end

    fun substConInExp conmap = 
      if is_empty conmap then 
	id 
      else
	substExpConInExp' (empty(),conmap)
     
    fun substExpInExp expmap = 
      if is_empty expmap then 
	id 
      else
	substExpConInExp' (expmap,empty())

    fun substExpConInExp (expmap,conmap) = 
      if (is_empty expmap) andalso (is_empty conmap) then 
	id 
      else
	substExpConInExp' (expmap,conmap)

    val con_subst_compose = compose substConInCon

    fun varConConSubst var con = 
      substConInCon' (add (empty()) (var,con))

    fun varConKindSubst var con = 
      substConInKind' (add (empty()) (var,con))

    fun wrap2 f name arg1 arg2 = 
      (Util.lprintl ("Entering"^name);
       (f arg1 arg2) before (Util.lprintl ("Leaving"^name)))

    val substConInCon = 
      if !debug then 
	wrap2 substConInCon "substConInCon"
      else substConInCon

    val substConInExp = 
      if !debug then 
	wrap2 substConInExp "substConInExp"
      else 
	substConInExp

    val substConInKind = 
      if !debug then 
	wrap2 substConInKind "substConInKind"
      else substConInKind

    val substExpInExp = 
      if !debug then 
	wrap2 substExpInExp "substExpInExp"
      else 
	substExpInExp

    val varConConSubst = 
      if !debug then 
	wrap2 varConConSubst "varConConSubst"
      else varConConSubst

    val varConKindSubst = 
      if !debug then 
	wrap2 varConKindSubst "varConSubst"
      else 
	varConKindSubst
  end
