(*$import Ppnil NILSUBST Stats NilError NilRewrite NilRename Option *)

structure NilSubst :> NILSUBST = 
  struct
    open Nil

    val isSome = Option.isSome
    val debug = Stats.ff "nil_debug"
    val profile = Stats.ff "nil_profile"
    val rename = Stats.ff "subst_rename"
      
    val (subst_counter,
	 subst_total_size) = 
      (Stats.counter "subst_counter",
       Stats.int "subst_total_size")

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

    val timer = Stats.subtimer
    val subtimer = Stats.subtimer

    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet

    type 'a map = 'a VarMap.map

    local
      (*Possibly uncomputed data.*)
      datatype 'a thunk = FROZEN of (unit -> 'a) | THAWED of 'a
      type 'a delay = 'a thunk ref
    in
      type 'a delay = 'a delay
      fun delay thunk = ref(FROZEN thunk)
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

      val substitute = fn args => subtimer ("Subst:substitute",VarMap.find) args

      val add = fn args => subtimer ("Subst:add",VarMap.insert) args

      fun is_empty s = (VarMap.numItems s) = 0
      fun empty () = VarMap.empty

      fun exp_var_xxx (state : state as {esubst,csubst},var,any) = 
	if !rename  then
	  let val var' = Name.derived_var var
	  in ({csubst = csubst,
	       esubst = add (esubst,var, immediate (Var_e var'))},
	      SOME var')
	  end
	else (state,NONE)


      fun con_var_xxx (state : state as {esubst,csubst},var,any) = 
	if !rename  then
	  let val var' = Name.derived_var var
	  in ({esubst = esubst,
	       csubst = add (csubst,var, immediate (Var_c var'))},
	      SOME var')
	  end
	else (state,NONE)

      fun conhandler (state : state as {csubst,...},con : con) =
	(case con
	   of Var_c var => 
	     (case substitute (csubst,var)
		of SOME con_delay => 
		  (subst_counter(); 
		   CHANGE_NORECURSE (state,thaw con_delay))
		 | _ => NORECURSE)
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
           rewrite_bnd = substExpConInBnd',...} = rewriters exp_con_handler
 
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
	  item 
	else
	  substituter (empty_state (esubst, empty())) item

      fun substConInXXX substituter csubst item =
	if (is_empty csubst) then 
	  item 
	else
	  substituter (empty_state (empty(), csubst)) item

    in

      val substConInCon = substConInXXX substExpConInCon'
      val substExpInExp = substExpInXXX substExpConInExp'
      val substExpInCon = substExpInXXX substExpConInCon'
      val substConInExp = substConInXXX substExpConInExp'
      val substConInKind = substConInXXX substExpConInKind'
      val substExpInKind = substExpInXXX substExpConInKind'

      fun substConInCBnd csubst bnd = 
	let
	  val bnd = 
	    if is_empty csubst then
	      bnd
	    else
	      (case substExpConInCBnd' (empty_state (empty(), csubst)) bnd
		 of ([bnd],state) => bnd
		  | _ => error "substConInCBnd" "Substitution should not change number of bnds")
	in
	  bnd
	end

      fun substConInBnd csubst bnd = 
	let
	  val bnd = 
	    if is_empty csubst then
	      bnd
	    else
	      (case substExpConInBnd' (empty_state (empty(), csubst)) bnd
		 of ([bnd],state) => bnd
		  | _ => error "substConInBnd" "Substitution should not change number of bnds")
	in
	  bnd
	end

      val substExpConInExp  = substExpConInXXX(substExpConInExp')
      val substExpConInCon  = substExpConInXXX(substExpConInCon')
      val substExpConInKind = substExpConInXXX(substExpConInKind')

    end  
(*
    (*Substitutions for single variables*)
    local
      open NilRewrite

      val renameConWRT = fn preds => subtimer("Subst:renameConWRT",NilRename.renameConWRT preds)
      val renameExpWRT = fn preds => subtimer("Subst:renameExpWRT",NilRename.renameExpWRT preds)

      type 'item state = {var : var,
			  item : 'item,
			  cbound : VarSet.set,
			  ebound : VarSet.set}

      val member = VarSet.member
      val add = VarSet.add
      exception Rebound of var

      fun exp_var_xxx (state : 'item state as {ebound,cbound,var,item},target,any) = 
	(if member (ebound,target) 
	   then raise Rebound target
	 else ({ebound = add (ebound,target),cbound = cbound,var = var,item = item},NONE))


      fun con_var_xxx (state : 'item state as {ebound,cbound,var,item},target,any) = 
	(if member (cbound,var) 
	   then raise Rebound var
	 else ({cbound = add (cbound,target),ebound = ebound,var = var,item = item},NONE))

      val member = fn set => fn item => member (set,item)

      fun varconhandler (state as {var,item,cbound,ebound},con : con) =
	(case con
	   of Var_c target => 
	     if Name.eq_var (var,target) 
	       then CHANGE_NORECURSE (state,renameConWRT (member ebound,member cbound) item) 
	     else NORECURSE
	    | _ => NOCHANGE)
	   
      fun varexphandler (state as {var,item,cbound,ebound},exp : exp) =
	(case exp
	   of Var_e target => 
	     if Name.eq_var (var,target) 
	       then CHANGE_NORECURSE (state,renameExpWRT (member ebound,member cbound) item) 
	     else NORECURSE
	    | _ => NOCHANGE)
	   
      val var_con_handler = 
	let 
	  val h = set_conhandler default_handler varconhandler
	  val h = set_con_binder h con_var_xxx
	  val h = set_con_definer h con_var_xxx
	  val h = set_exp_binder h exp_var_xxx
	  val h = set_exp_definer h exp_var_xxx
	in h
	end

      val var_exp_handler = 
	let 
	  val h = set_exphandler default_handler varexphandler
	  val h = set_con_binder h con_var_xxx
	  val h = set_con_definer h con_var_xxx
	  val h = set_exp_binder h exp_var_xxx
	  val h = set_exp_definer h exp_var_xxx
	in h
	end

      val {rewrite_exp = varConExpSubst',
	   rewrite_con = varConConSubst',
	   rewrite_kind = varConKindSubst',...} = rewriters var_con_handler
	
      val {rewrite_exp = varExpExpSubst',
	   rewrite_con = varExpConSubst',
	   rewrite_kind = varExpKindSubst',...} = rewriters var_exp_handler
	
      val pp_kind = Ppnil.pp_kind
      val pp_con = Ppnil.pp_con
      val pp_exp = Ppnil.pp_exp

      fun empty_state (var,item) = {var = var,item = item, cbound = VarSet.empty, ebound = VarSet.empty}

      fun varXXXYYYSubst (name,XXXprint,YYYprint,substitute) var Xitem Yitem =
	let
	  val item = substitute (empty_state (var,Xitem)) Yitem
	in
	  item
	end

    in
      val varConExpSubst  = varXXXYYYSubst ("varConExpSubst",pp_con,pp_exp,varConExpSubst')
      val varConConSubst  = varXXXYYYSubst ("varConConSubst",pp_con,pp_con,varConConSubst')
      val varConKindSubst = varXXXYYYSubst ("varConKindSubst",pp_con,pp_kind,varConKindSubst')

      val varExpExpSubst  = varXXXYYYSubst ("varExpExpSubst",pp_exp,pp_exp,varExpExpSubst')
      val varExpConSubst  = varXXXYYYSubst ("varExpConSubst",pp_exp,pp_con,varExpConSubst')
      val varExpKindSubst = varXXXYYYSubst ("varExpKindSubst",pp_exp,pp_kind,varExpKindSubst')
    end
*)

    local 
      fun renameCon (con :con) : con delay = delay (fn () => if !rename then NilRename.renameCon con else con)
      fun renameExp (exp :exp) : exp delay = delay (fn () => if !rename then NilRename.renameExp exp else exp)
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

      val renameItem = fn item => if !rename then renameItem item else item
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
      
  end
