(*$import Ppnil NILSUBST Stats *)

structure NilSubst :> NILSUBST 
  = 
  struct


    type exp = Nil.exp
    type con = Nil.con
    type kind = Nil.kind
    type bnd = Nil.bnd
    type var = Name.var
    open Nil

    val debug = ref false
    val profile = Stats.ff "nil_profile"
    val short_circuit = Stats.ff "subst_short_circuit"

    (*Preliminary results suggest that this is a bad idea.*)
    val subst_use_hash = Stats.ff "subst_use_hash"

    val local_subst_count = ref 0
    fun get_subst_count() = !local_subst_count
    fun reset_subst_count() = local_subst_count := 0

    val (subst_counter,
	 subst_total_size) = 
      (Stats.counter "subst_counter",
       Stats.int "subst_total_size")
    val (subst_short_circuited_counter,
	 subst_short_circuited_size) = 
      (Stats.counter "subst_short_circuited_counter",
       Stats.int "subst_short_circuited_size")

    val foldl_acc = Listops.foldl_acc
    val map_second = Listops.map_second
    val unzip = Listops.unzip
    val zip = Listops.zip
    val mapopt = Util.mapopt
    fun error s = Util.error "subst.sml" s

    local
      structure W = LargeWord
      type word = W.word
      val fromInt = W.fromInt
      val orb = W.orb
      val andb = W.andb
      val notb = W.notb
      val xorb = W.xorb
    in
      val b : word = 0wx3141592

      fun hash i = W.>>(W.*(fromInt i,b),0wx12)

      val var2word = fromInt o Name.var2int
	  
      fun var2word v = 
	if !subst_use_hash then 
	  (hash o Name.var2int) v
	else 
	  (fromInt o Name.var2int) v

      val zero = fromInt 0

      structure VarMap = Name.VarMap

      type 'a subst = {test:word,
		       subst : 'a VarMap.map}
      (* test -> the negation of the bitwise or of the integer version of the elements
       *   invariant -> test = notb (VarMap.foldli (fn (v,_,a) => orb (var2int v,a)) zero subst)
       *      that is, test is the negation of the union of the set bits in the domain of the map
       *    Note that if (andb (test,var2word var)) is not zero, then
       *      there is a bit set in var that is not set in any element of the domain of
       *      the map, and hence v is not in the domain of the map.
       *    This is a simple optimization for the common case when the map is small.
       * subst -> the mapping from variables to values
       *)

      fun empty () : 'a subst = 
	{test = notb zero,
	 subst = VarMap.empty}

      fun toList ({test,subst} : 'a subst) = VarMap.listItemsi subst

      fun fromList (list : (var * 'a) list) : 'a subst = 
	let
	  fun fold1 ((var,value),(test,subst)) = 
	    (orb (test,(var2word var)),VarMap.insert(subst,var,value))
	  fun fold2  ((var,value),subst) = 
	    VarMap.insert(subst,var,value)

	  val (test,subst) = 
	    if !short_circuit then 
	      List.foldl fold1 (zero,VarMap.empty) list
	    else 
	      (zero,List.foldl fold2 VarMap.empty list)
	  val test = notb test
	in
	  {test = test,
	   subst = subst}
	end

      fun add {test,subst} (var,value) : 'a subst =
	let
	  val test = 
	    if !short_circuit then 
	      (andb (test,notb (var2word var)))
	    else 
	      test
	  val subst = VarMap.insert (subst,var,value)
	in
	  {test = test,
	   subst = subst}
	end

      fun substitute {test,subst} var =
	if !profile then 
	  (subst_counter();
	   subst_total_size := !subst_total_size + (VarMap.numItems subst);
	   if !short_circuit then
	     if (andb (test,var2word var)) = zero then
	       VarMap.find (subst,var)
	     else 
	       (subst_short_circuited_counter();
		subst_short_circuited_size := 
		!subst_short_circuited_size + (VarMap.numItems subst);
		NONE)
	   else 
	     VarMap.find (subst,var))
	else
	  if !short_circuit then
	    if (andb (test,var2word var)) = zero then
	      VarMap.find (subst,var)
	    else 
	      NONE
	  else 
	    VarMap.find (subst,var)

      fun is_empty {test,subst} = (VarMap.numItems subst) = 0

      (* val compose : ('a subst -> 'a -> 'a) -> ('a subst * 'a subst) -> 'a subst
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

      fun merge (map1 as {test = test1,subst = subst1},
		 map2 as {test = test2,subst = subst2}) =
	let
	  fun combine (value1,value2) = value2
	  val subst = VarMap.unionWith combine (subst1,subst2)
	  val test = andb (test1,test2)
	in
	  {test = test, subst = subst}
	end

      fun print (printer : 'a -> unit) ({test,subst}: 'a subst) = 
	let
	  fun print1 (v,a) = 
	    (TextIO.print (Name.var2string v);
	     TextIO.print "->";
	     printer a;
	     TextIO.print "\n")
	in
	  (Util.lprintl "Substitution is";
	   VarMap.appi print1 subst;
	   Util.printl "")
	end

    end (* local *)

    fun rebind Con (var,subst) = 
      if is_empty subst then
	(var,subst)
      else
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

    val con_rebind' = rebind Var_c
    val con_rebind_list' = rebind_list con_rebind'
    val exp_rebind' = rebind Var_e
    val exp_rebind_list' = rebind_list exp_rebind'

    type mapping = exp subst * con subst
    val empty_mapping : mapping = (empty(),empty())
    fun is_empty_mapping (em,cm) = is_empty em andalso is_empty cm
    fun con_rebind(v,(em,cm)) = let val (v,cm) = con_rebind'(v,cm)
				in  (v,(em,cm))
				end
    fun con_rebind_list(vars,(em,cm)) = let val (vars,cm) = con_rebind_list'(vars,cm)
					in  (vars,(em,cm))
					end
    fun exp_rebind(v,(em,cm)) = let val (v,em) = exp_rebind'(v,em)
				in  (v,(em,cm))
				end
    fun exp_rebind_list(vars,(em,cm)) = let val (vars,em) = exp_rebind_list'(vars,em)
					in  (vars,(em,cm))
					end


    fun con_var_replace (subst,var) = 
      (case substitute subst var
	 of SOME (Nil.Var_c var) => (local_subst_count := !local_subst_count + 1; var)
	  | _ => var)

    fun id x = x
      
    fun substConInTFormals (mapping : mapping) (formals : (var * kind) list) = 
      let
	fun fold_one ((var,kind),mapping : mapping) =
	  let
	    val kind = substConInKind' mapping kind
	    val (var,mapping) = con_rebind (var,mapping)
	  in
	    ((var,kind),mapping)
	  end
      in
	foldl_acc fold_one mapping formals
      end
      
    and substConInFormals (mapping : mapping) (formals : (var * con) list) = 
      let
	fun fold_one ((var,con),mapping : mapping) =
	  let
	    val con = substConInCon' mapping con
	    val (var,mapping) = exp_rebind (var,mapping)
	  in
	    ((var,con),mapping)
	  end
      in
	foldl_acc fold_one mapping formals
      end

    and substConInKind' (mapping : mapping) (kind : kind) = 
      (case kind of
	 Type_k => kind
       | (Singleton_k(con)) => Singleton_k(substConInCon' mapping con)
       | (Record_k fieldseq) =>
	   let
	     fun fold_one (((lbl,var),kind),mapping) = 
	       let
		 val kind  = substConInKind' mapping kind
		 val (var,mapping) = con_rebind (var,mapping)
	       in
		 (((lbl, var), kind),mapping)
	       end
	     val (fieldseq,mapping) = Sequence.foldl_acc fold_one mapping fieldseq
	   in
	     Record_k fieldseq
	   end

       | (Arrow_k (openness, args, result)) =>
	   let
	     val (args,mapping) = substConInTFormals mapping args
	     val result = substConInKind' mapping result
	   in
	     Arrow_k (openness,args, result)
	   end)

    and substConInConBnd' (mapping : mapping) cbnd =
	let fun do_confun Con mapping1 (var,formals,body,kind) = 
	       let
		 val (formals,mapping) = substConInTFormals mapping1 formals
		 val body = substConInCon' mapping body
		 val kind = substConInKind' mapping kind
		 val (var,mapping) = con_rebind (var,mapping1) (*Not mapping!!*)
	       in
		 (Con (var,formals,body,kind),mapping)
	       end
	in
	       case cbnd of
		     Con_cb (var,con) =>
		   let 
		     val con = substConInCon' mapping con
		     val (var,mapping) = con_rebind (var,mapping)
		     val cbnd = Con_cb (var,con)
		   in  
		     (cbnd,mapping)
		   end
		  | Open_cb body => do_confun Open_cb mapping body
		  | Code_cb body => do_confun Code_cb mapping body

	end

    and substConInCon' (mapping : mapping) (con : con) = 
      let
	fun print_one (v,c) =
	  (TextIO.print ((Name.var2string v)^"->");
	   Ppnil.pp_con c)
	val _ = 
	  if !debug then
	    (Util.lprintl "Substituting map :";
	     VarMap.appi print_one (#subst (#2 mapping));
	     Util.lprintl "Into constructor :";
	     Ppnil.pp_con con;
	     Util.printl "")
	  else ()
      in
      (case con 
	 of (Prim_c (pcon,args)) => 
	   (Prim_c (pcon,map (substConInCon' mapping) args))
	  | (Mu_c (flag,defs)) =>
	   let
	     val (vars,cons) = unzip (Sequence.toList defs)
	     val (vars,mapping) = con_rebind_list (vars,mapping)
	     val cons = List.map (substConInCon' mapping) cons
	     val defs = Sequence.fromList (zip vars cons)
	   in
	     (Mu_c (flag,defs))
	   end
	  | ExternArrow_c(formals,return) =>
	   let
	     val formals = map (substConInCon' mapping) formals
	     val return = substConInCon' mapping return
	   in
	     ExternArrow_c (formals,return)
	   end

	  | (AllArrow_c (openness,effect,tformals,vlistopt,clist,flength,return)) =>
	   let
	     val (tformals,mapping) = substConInTFormals mapping tformals
	     val (vlistopt,clist,mapping) = 
		 case vlistopt of
		     SOME vars => let val (vclist,state) = substConInFormals mapping (Listops.zip vars clist)
				  in  (SOME(map #1 vclist), map #2 vclist, state)
				  end
		   | _ => (NONE, map (substConInCon' mapping) clist, mapping)
	     val return = substConInCon' mapping return
	   in
	     AllArrow_c (openness,effect,tformals,vlistopt,clist,flength,return)
	   end

	   | Typeof_c e => Typeof_c(substExpConInExp' mapping e)

	  | (Var_c var) => 
	   (case substitute (#2 mapping) var
	      of SOME con => (local_subst_count := !local_subst_count + 1; con)
	       | _ => con)

	  | (Let_c (letsort, cbnds, body)) => 
	   let
	     fun folder (cbnd,mapping) = substConInConBnd' mapping cbnd
	     val (cbnds,mapping) = foldl_acc folder mapping cbnds
	     val body = substConInCon' mapping body
	   in
	     Let_c (letsort, cbnds, body)
	   end
	 
	  | (Closure_c (code,env)) =>
	   let
	     val code = substConInCon' mapping code
	     val env = substConInCon' mapping env
	   in
	     Closure_c(code, env)
	   end
	 
	  | (Crecord_c entries) =>
	   Crecord_c (map (fn (l,c) => (l,substConInCon' mapping c)) entries)
	   
	  | (Proj_c (con,lbl)) =>
	   let
	     val con = substConInCon' mapping con
	   in
	     Proj_c (con, lbl)
	   end
	 
	  | (App_c (cfun,actuals)) =>
	   let
	     val cfun = substConInCon' mapping cfun
	     val actuals = map (substConInCon' mapping) actuals
	   in
	     App_c (cfun, actuals)
	   end
	 
	  | Typecase_c {arg, arms, default, kind} => 
	   let 
	     val arg = substConInCon' mapping arg
	     fun doarm (pcon,args,body) = 
	       let
		 val (args,mapping) = substConInTFormals mapping args
		 val body = substConInCon' mapping body
	       in
		 (pcon,args,body)
	       end
	     val arms = map doarm arms
	     val default = substConInCon' mapping default
	     val kind = substConInKind' mapping kind
	   in  Typecase_c{arg = arg,
			  arms = arms,
			  default = default,
			  kind = kind}
	   end
	  | (Annotate_c (TYPECHECKED kind,con)) => substConInCon' mapping con
	  | (Annotate_c (annot as FREE_VARS {con_vars,exp_vars},con)) => 
	   let
	     val (em,cm) = mapping
	     val {test,subst} = cm
	     val subst = VarMap.filteri (fn (v,c) => Name.VarSet.member (con_vars,v)) subst
	     val con = 
	       if (VarMap.numItems subst) = 0 then 
		 con else
		 substConInCon' (em,{test=test,subst=subst}) con
	   in
	     Annotate_c (annot, con)
	   end)
      end

    and substConInCon mapping = 
      if is_empty_mapping mapping then 
	id 
      else
	substConInCon' mapping

    and substConInKind mapping = 
      if is_empty_mapping mapping then 
	id 
      else
	substConInKind' mapping

    and substExpConInExp' (mapping : mapping) (exp : exp) : exp = 
      (case exp
	 of Var_e var => 
	   (case substitute (#1 mapping) var
	      of SOME exp => (local_subst_count := !local_subst_count + 1; exp)
	       | _ => exp)
	  | Const_e value => 
	      Const_e (substExpConInValue' mapping value)
	  | Let_e (letsort,bnds,exp) => 
	      let
		val (bnds,mapping) = 
		  substExpConInBnds' mapping bnds
		val exp = substExpConInExp' mapping exp
	      in
		(Let_e (letsort,bnds,exp))
	      end
	  | Prim_e (allprim,cons,exps) => 
	      let
		val cons = map (substConInCon mapping) cons
		val exps = map (substExpConInExp' mapping) exps
	      in
		(Prim_e (allprim,cons,exps))
	   end
	  | Switch_e switch => Switch_e (substExpConInSwitch' mapping switch)
	  | App_e (openness,exp,cons,exps,floats) =>
	   let
	     val exp = substExpConInExp' mapping exp
	     val cons = map (substConInCon mapping) cons
	     val exps = map (substExpConInExp' mapping) exps
	     val floats = map (substExpConInExp' mapping) floats
	   in
	     App_e (openness,exp,cons,exps,floats)
	   end
	  | Raise_e (exp,con) =>
	   let 
	     val con = substConInCon mapping con
	     val exp = substExpConInExp' mapping exp
	   in
	     Raise_e (exp,con)
	   end
	  | Handle_e (exp,bound,handler) =>
	   let
	     val exp = substExpConInExp' mapping exp
	     val (bound,mapping) = exp_rebind(bound,mapping)
	     val handler = substExpConInExp' mapping handler
	   in
	     Handle_e (exp,bound,handler)
	   end)
    and substExpConInValue' (mapping : mapping) value = 
      (case value of
	    Prim.int _ => value
	  | (Prim.uint _)  => value
	  | (Prim.float _) => value
	  | Prim.array (con,arr) => 
	   let
	     val con = substConInCon mapping con
	   in
	     Array.modify (substExpConInExp' mapping) arr;
	     Prim.array (con,arr)
	   end
	| Prim.vector (con,vec) => 
	   let
	     val con = substConInCon mapping con
	   in
	     Array.modify (substExpConInExp' mapping) vec;
	     Prim.vector (con,vec)
	   end
	| Prim.refcell expref =>
	 let
	   val exp = substExpConInExp' mapping (!expref)
	 in
	   expref := exp;
	   Prim.refcell expref
	 end
	| Prim.tag (atag,con) => 
	 let
	   val con = substConInCon mapping con
	 in
	   Prim.tag (atag,con)
	 end)

    and substExpConInBnds' mapping bnds =
      foldl_acc substExpConInBnd' mapping bnds


    and substExpConInBnd' (bnd,mapping : mapping) =
	let fun do_funs defs =  
	    let
		val (vars,functions) = unzip (Sequence.toList defs)
		val (vars,mapping) = exp_rebind_list (vars,mapping)
		val functions = 
		    map (substExpConInFunction' mapping) functions
		val defs = Sequence.fromList (zip vars functions)
	    in	(defs,mapping)
	    end
        in  (case bnd of
		 Con_b (phase,cb) =>
		     let
			 val (cb,mapping) = substConInConBnd' mapping cb
			 val bnd = (Con_b (phase,cb))
		     in
			 (bnd,mapping)
		     end
	       | Exp_b (var, _, exp) =>
		     let
			 val exp = substExpConInExp' mapping exp
			 val (var,mapping) = exp_rebind (var,mapping)
                         (*XXX overly conservative!
                            substitution in traceinfo might be malformed *)
			 val bnd = (Exp_b (var,TraceUnknown,exp))
		     in
			 (bnd,mapping)
		     end
	       | (Fixopen_b defs) => let val (defs,ecmap) = do_funs defs
				     in  (Fixopen_b defs, ecmap)
				     end
	       | (Fixcode_b defs) => let val (defs,ecmap) = do_funs defs
				     in  (Fixcode_b defs, ecmap)
				     end
	       | Fixclosure_b (flag,defs) => 
		      let
			  val (vars,closures) = unzip (Sequence.toList defs)
			  val (vars,mapping) = exp_rebind_list (vars,mapping)
			  val closures = map (substExpConInClosure' mapping) closures
			  val defs = Sequence.fromList (zip vars closures)
			  val bnd = Fixclosure_b (flag,defs)
		      in  (bnd,mapping)
		      end)
	end

    and substExpConInSwitch' (mapping : mapping) switch =
      (case switch
	 of Intsw_e {size,arg,arms,default} =>
	   let
	     val arg = substExpConInExp' mapping arg
	     val arms = map_second (substExpConInExp' mapping) arms
	     val default = mapopt (substExpConInExp' mapping) default
	   in
	     Intsw_e {size=size,arg=arg,
		      arms=arms,default=default}
	   end
	  | Sumsw_e {sumtype,arg,bound,arms,default} => 
	   let
	     val sumtype = substConInCon mapping sumtype
	     val arg = substExpConInExp' mapping arg
	     val default = mapopt (substExpConInExp' mapping) default
	     val (bound,mapping) = exp_rebind(bound,mapping)
	     val arms = map_second (substExpConInExp' mapping) arms

	   in  Sumsw_e {sumtype=sumtype,arg=arg,bound=bound,
			arms=arms,default=default}
	   end
	  | Exncase_e {arg,bound,arms,default} =>
	   let
	     val arg = substExpConInExp' mapping arg
	     val default = mapopt (substExpConInExp' mapping) default
	     val (bound,mapping) = exp_rebind(bound,mapping)
	     val arms = map (fn (e1,e2) => (substExpConInExp' mapping e1,
					    substExpConInExp' mapping e2)) arms
	   in Exncase_e {arg=arg,bound=bound,
			arms=arms,default=default}
	   end
	  | Typecase_e _ => error "typecase not handled")

    and substExpConInFunction' (mapping : mapping)
      (Function (effect,recursive,tformals,dep,formals,fformals,body,return)) = 
      let
	val (tformals,mapping) = substConInTFormals mapping tformals
	val (formals,mapping) = substConInFormals mapping formals
	val (fformals,mapping) = exp_rebind_list (fformals,mapping)
	val body = substExpConInExp' mapping body
	val return = substConInCon mapping return
      in
	Function (effect,recursive,tformals,dep,formals,fformals,body,return)
      end
    and substExpConInClosure' (mapping : mapping)
      {code:var, cenv:con, venv:exp, tipe:con} = 
      let
	val code = 
	  (case substitute (#1 mapping) code 
	     of SOME (Var_e var) => (local_subst_count := !local_subst_count + 1; var)
	      | _ => code)
	val cenv = substConInCon mapping cenv
	val venv = substExpConInExp' mapping venv
	val tipe = substConInCon mapping tipe
      in
	{code=code,cenv=cenv,venv=venv,tipe=tipe}
      end

    fun substConInExp mapping = 
      if is_empty_mapping mapping then 
	id 
      else
	substExpConInExp' empty_mapping
     
    fun substExpInExp mapping = 
      if is_empty_mapping mapping then 
	id 
      else
	substExpConInExp' mapping

    fun substExpConInExp mapping = 
      if (is_empty_mapping mapping) 
	then id 
      else
	substExpConInExp' mapping

    fun substConInBnd cm bnd = 
      if is_empty cm then 
	(bnd,cm)
      else
	let
	  (*substExpConInBnd' cannot make an empty expmap non-empty*)
	  val (bnd,mapping) = substExpConInBnd' (bnd,(empty(),cm))
	in
	  (bnd,#2 mapping)
	end


    fun varConConSubst var con = 
      substConInCon' (empty(),add (empty()) (var,con))

    fun varConKindSubst var con = 
      substConInKind' (empty(),add (empty()) (var,con))

    fun wrap2 f name arg1 arg2 = 
      (Util.lprintl ("Entering"^name);
       (f arg1 arg2) before (Util.lprintl ("Leaving"^name)))

    fun wrap1 f name arg1 = 
      (Util.lprintl ("Entering"^name);
       (f arg1) before (Util.lprintl ("Leaving"^name)))

    val substExpConInCon = 
      fn em_cm => if !debug then 
	        wrap1 substConInCon "substConInCon" em_cm
               else substConInCon em_cm

    val substConInCon = 
      fn cm => if !debug then 
	        wrap1 substConInCon "substConInCon" (empty(),cm)
               else substConInCon (empty(),cm)

    val substConInExp = fn cm =>
      if !debug then 
	wrap1 substConInExp "substConInExp" (empty(),cm)
      else 
	substConInExp (empty(),cm)

    val substConInKind = fn cm =>
      if !debug then 
	wrap1 substConInKind "substConInKind" (empty(),cm)
      else substConInKind (empty(),cm)

    val substExpInExp = fn em =>
      if !debug then 
	wrap1 substExpInExp "substExpInExp" (em,empty())
      else 
	substExpInExp (em,empty())

    val varConConSubst = 
      if !debug then 
	wrap2 varConConSubst "varConConSubst"
      else varConConSubst

    val varConKindSubst =
      if !debug then 
	wrap2 varConKindSubst "varConSubst"
      else 
	varConKindSubst

    val printConSubst = print Ppnil.pp_con



  end
