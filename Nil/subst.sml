(*$import Ppnil NILSUBST Stats NilError NilRewrite NilRename Option *)

structure NilSubst :> NILSUBST = 
  struct
    open Nil
      
    val isSome = Option.isSome
    val debug = Stats.ff "nil_debug"

    val profile = Stats.ff "nil_profile"
    val subst_profile = Stats.ff "subst_profile"
    val subtimer = fn args => fn args2 => if !profile orelse !subst_profile then Stats.subtimer args args2 else #2 args args2
     
    val (subst_counter,
	 subst_total_size) = 
      (Stats.counter "subst_counter",
       Stats.int "subst_total_size")

    val ignored = Stats.counter "subst_is_empty"
    val non_empty = Stats.counter "subst_non_empty"

    val repeated_subst = Stats.counter "repeated_subst"

    val foldl_acc = Listops.foldl_acc
    val map_second = Listops.map_second
    val unzip = Listops.unzip
    val zip = Listops.zip

    val mapopt = Util.mapopt
    val lprintl = Util.lprintl
    val printl = Util.printl

    val locate = NilError.locate "Subst"
    val assert = NilError.assert

    fun error s s' = Util.error (locate s) s'

    fun error' s = error "" s

    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet

    type 'a map = 'a VarMap.map

    local
      (*Possibly uncomputed data.*)
      datatype 'a thunk = FROZEN of (unit -> 'a) | THAWED of 'a
      type 'a delay = 'a thunk ref
      val eager = Stats.ff "subst_eager"
    in
      type 'a delay = 'a delay
      fun delay thunk = ref(if (!eager)
			      then THAWED (thunk())
			    else FROZEN thunk)
      fun immediate value = ref (THAWED value)
      fun thaw (ref (THAWED v)) = v
	| thaw (r as ref(FROZEN t)) = let val v = t()
					  val _ = r := (THAWED v)
				      in  v
				      end
      fun delayed (ref (FROZEN _)) = true
	| delayed (ref (THAWED _)) = false
    end

    (* Substitutions *)
    local
      open NilRewrite

      exception Rebound of var

      type varset = VarSet.set

      type state = {esubst : exp delay map,
		    csubst : con delay map}

      val substitute = VarMap.find

      val add = VarMap.insert

      fun annotate c = Annotate_c (SUBST_RESULT,c)

      fun is_empty s = (VarMap.numItems s) = 0
      fun empty () = VarMap.empty

      fun exp_var_xxx (state : state as {esubst,csubst},var,any) = 
(*	if !rename  then
	  let val var' = Name.derived_var var
	  in ({csubst = csubst,
	       esubst = add (esubst,var, immediate (Var_e var'))},
	      SOME var')
	  end
	else*) (state,NONE)


      fun con_var_xxx (state : state as {esubst,csubst},var,any) = 
(*	if !rename  then
	  let val var' = Name.derived_var var
	  in ({esubst = esubst,
	       csubst = add (csubst,var, immediate (Var_c var'))},
	      SOME var')
	  end
	else*) (state,NONE)

      fun conhandler (state : state as {csubst,...},con : con) =
	(case con
	   of Var_c var => 
	     (case substitute (csubst,var)
		of SOME con_delay => 
		  (subst_counter(); 
		   CHANGE_NORECURSE (state,thaw con_delay))
		 | _ => NORECURSE)
	    | (Proj_c (Var_c var,label)) => 
	     (case substitute (csubst,var)
		of SOME con_delay => 
		  let val con2 = thaw con_delay
		      val res = (case con2 of
				   (Crecord_c entries) => 
				     #2(valOf(List.find (fn ((l,_)) => Name.eq_label (l,label)) entries ))
				 | _ => Proj_c(con2,label))
		  in
		    (subst_counter(); 
		     CHANGE_NORECURSE (state,res))
		  end
		 | _ => NORECURSE)
	    | (Annotate_c (SUBST_RESULT,_)) => (repeated_subst();NOCHANGE)
	    | _ => NOCHANGE)
	   
      fun exphandler (state : state as {esubst,...},exp : exp) =
	(case exp
	   of Var_e var => 
	     (case substitute (esubst,var)
		of SOME exp_delay => 
		  (subst_counter(); 
		   CHANGE_NORECURSE (state,thaw exp_delay))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      val exp_con_handler = 
	let
	  val h = set_conhandler default_handler conhandler
	  val h = set_exphandler h  exphandler
	  val h = set_con_binder h con_var_xxx
	  val h = set_con_definer h con_var_xxx
	  val h = set_exp_binder h exp_var_xxx
	  val h = set_exp_binder h exp_var_xxx
	in h 
	end

      val {rewrite_con = substExpConInCon',
	   rewrite_exp = substExpConInExp',
	   rewrite_kind = substExpConInKind',
           rewrite_cbnd = substExpConInCBnd',
           rewrite_bnd = substExpConInBnd',
	   rewrite_trace = substExpConInTrace',...} = rewriters exp_con_handler
 
      fun empty_state (esubst : exp delay map,csubst : con delay map) : state = 
	{esubst = esubst, csubst = csubst}

      fun substExpConInXXX substituter (esubst,csubst) item = 
	let
	  val item =  	
	    if (is_empty esubst) andalso (is_empty csubst) then 
	      item 
	    else
	      substituter (empty_state (esubst,csubst)) item
	in
	  item
	end

      fun substExpInXXX substituter esubst item =
	if (is_empty esubst) then 
	  (ignored();item)
	else
	  (non_empty();substituter (empty_state (esubst, empty())) item)

      fun substConInXXX substituter csubst item =
	if (is_empty csubst) then 
	  (ignored();item)
	else
	  (non_empty();substituter (empty_state (empty(), csubst)) item)

      fun wrap substXXXInCon = fn s => fn c => substXXXInCon s c
    in
      val substConInCon   = fn s => subtimer("Subst:substConInCon",wrap (substConInXXX substExpConInCon') s)
      val substExpInExp   = fn s => subtimer("Subst:substExpInExp",substExpInXXX substExpConInExp' s)
      val substExpInCon   = fn s => subtimer("Subst:substExpInCon",wrap (substExpInXXX substExpConInCon') s)
      val substConInExp   = fn s => subtimer("Subst:substConInExp",substConInXXX substExpConInExp' s)
      val substConInKind  = fn s => subtimer("Subst:substConInKind",substConInXXX substExpConInKind' s)
      val substExpInKind  = fn s => subtimer("Subst:substExpInKind",substExpInXXX substExpConInKind' s)
      val substConInTrace = fn s => subtimer("Subst:substConInTrace",substConInXXX substExpConInTrace' s) 

      fun substConInCBnd csubst bnd = 
	let
	  val bnd = 
	    if is_empty csubst then
	      (ignored();bnd)
	    else
	      (non_empty();
	       case substExpConInCBnd' (empty_state (empty(), csubst)) bnd
		 of ([bnd],state) => bnd
		  | _ => error "substConInCBnd" "Substitution should not change number of bnds")
	in
	  bnd
	end

      fun substConInBnd csubst bnd = 
	let
	  val bnd = 
	    if is_empty csubst then
	      (ignored();bnd)
	    else
	      (non_empty();
	       case substExpConInBnd' (empty_state (empty(), csubst)) bnd
		 of ([bnd],state) => bnd
		  | _ => error "substConInBnd" "Substitution should not change number of bnds")
	in
	  bnd
	end
      val substConInBnd = fn s => subtimer("Subst:substExpConInBnd",substConInBnd s)
      val substConInCBnd = fn s => subtimer("Subst:substExpConInCbnd",substConInCBnd s)

      val substExpConInExp  = fn s => subtimer("Subst:substExpConInExp",substExpConInXXX(substExpConInExp') s)
      val substExpConInCon  = fn s => subtimer("Subst:substExpConInCon",wrap (substExpConInXXX(substExpConInCon')) s) 
      val substExpConInKind = fn s => subtimer("Subst:substExpConInKind",substExpConInXXX(substExpConInKind') s) 

    end  

    local 
      fun renameCon (con :con) : con delay = delay (fn () => NilRename.renameCon con)
      fun renameExp (exp :exp) : exp delay = delay (fn () => NilRename.renameExp exp)
    in
      fun varConExpSubst var con exp = substConInExp (VarMap.insert (VarMap.empty,var,renameCon con)) exp
      fun varConConSubst var con con2 = substConInCon (VarMap.insert (VarMap.empty,var,renameCon con)) con2
      fun varConKindSubst var con kind = substConInKind (VarMap.insert (VarMap.empty,var,renameCon con)) kind

      fun varExpExpSubst var exp1 exp2 = substExpInExp (VarMap.insert (VarMap.empty,var,renameExp exp1)) exp2
      fun varExpConSubst var exp con = substExpInCon (VarMap.insert (VarMap.empty,var,renameExp exp)) con
      fun varExpKindSubst var exp kind = substExpInKind (VarMap.insert (VarMap.empty,var,renameExp exp)) kind
    end

    type con_subst = con delay VarMap.map
    type exp_subst = exp delay VarMap.map


    functor SubstFn(type item
		    type item_subst = item delay VarMap.map
		    val substItemInItem : item_subst -> item -> item 
		    val renameItem : item -> item
		    val printer : item -> unit) 
      :> SUBST where type item = item
		 and type item_subst = item_subst =
    struct
      
      type var = Name.var
      type item = item
      type item_subst = item_subst


      fun rename (item :item) : item delay = delay (fn () => renameItem item)

      fun empty () : item_subst = VarMap.empty
	
      fun substitute subst var = mapopt thaw (VarMap.find (subst,var))
	
      fun toList (subst : item_subst) = map_second thaw (VarMap.listItemsi subst)
	
      fun sim_add subst (var,value) : item_subst = VarMap.insert (subst,var,rename value) 
      
      fun addl (var,item,subst) = 
	let
	  val item_delay = rename item
	  val map_subst = VarMap.insert (empty(),var,item_delay)
	  fun domap i = delay (fn () => substItemInItem map_subst (thaw i))
	in
	  VarMap.insert (VarMap.map domap subst,var,item_delay)
	end
      
      fun addr  (subst,var,item) = 
	VarMap.insert (subst,var,delay (fn () => substItemInItem subst (renameItem item)))

      fun is_empty subst = (VarMap.numItems subst) = 0
	
      fun compose (subst1,subst2) = 
	let
	  fun domap item_delay = delay (fn () => substItemInItem subst1 (thaw item_delay))
	  val subst2 = VarMap.map domap subst2
	  val subst = VarMap.unionWith #2 (subst1,subst2)
	in
	  subst
	end
      
      fun merge (subst1,subst2) = VarMap.unionWith #2 (subst1,subst2)

      fun simFromList (list : (var * item) list) : item_subst = 
	let
	  fun fold ((var,value),subst) = 
	    VarMap.insert(subst,var,rename value)
	    
	  val subst =  List.foldl fold VarMap.empty list
	in
	  subst
	end

      fun seqFromList (list : (var * item) list) : item_subst = 
	let
	  fun fold ((var,value),subst) = 
	    VarMap.insert (subst,var,delay (fn () => substItemInItem subst (renameItem value)))
	  val subst =  List.foldl fold VarMap.empty list
	in
	  subst
	end

      fun printf (printer : item -> unit) (subst: item_subst) = 
	let
	  fun print1 (v,a) = 
	    (TextIO.print (Name.var2string v);
	     TextIO.print "->";
	     printer (thaw a);
	     TextIO.print "\n")
	in
	  (Util.lprintl "Substitution is";
	   VarMap.appi print1 subst;
	   Util.printl "")
	end

      val print = printf printer
    end

    structure C = SubstFn(type item = con
			  type item_subst = con_subst
			  val substItemInItem = substConInCon
			  val renameItem = NilRename.renameCon
			  val printer = Ppnil.pp_con)
      
      
    structure E = SubstFn(type item = exp
			  type item_subst = exp_subst
			  val substItemInItem = substExpInExp
			  val renameItem = NilRename.renameExp
			  val printer = Ppnil.pp_exp)

(*    val substConInKind = 
      fn s => fn k => let val _ = if (Trace.enter trace) = 17831 
				    then ()(*(print "Calling substConInKind with subst = \n";
					  C.print s;
					  print "\nand kind = \n";
					  Ppnil.pp_kind k)*)
				  else ()
      
			  val res = substConInKind s k
			  val _ = Trace.exit trace
		      in res end      *)
  end
