(*$import Ppnil NILSUBST Stats NilError NilRewrite Option *)

structure NilSubst :> NILSUBST 
  = 
  struct
    open Nil

    val isSome = Option.isSome
    val debug = Stats.ff "nil_debug"
    val profile = Stats.ff "nil_profile"
      
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


    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet

    fun contains map var = isSome (VarMap.find (map,var))

    type 'a subst = 'a VarMap.map

    fun unique [] = true
      | unique ((v,_)::rest) = 
      (not (List.exists (fn (v',_) => Name.eq_var (v,v')) rest))
      andalso unique rest

    fun empty () : 'a subst = VarMap.empty

    fun toList (subst : 'a subst) = VarMap.listItemsi subst

    fun fromList (list : (var * 'a) list) : 'a subst = 
      let
	val _ = 
	  if !debug then
	    assert (locate "fromList PRE") 
	    [ 
	     (unique list, fn () => print "variable duplicated in list")
	     ]
	  else
	    ()

	fun fold ((var,value),subst) = 
	  VarMap.insert(subst,var,value)
	  
	val subst =  List.foldl fold VarMap.empty list
      in
	subst
      end

    fun add subst (var,value) : 'a subst =
      let
	val _ = 
	  if !debug then
	    assert (locate "add PRE") 
	    [ 
	     (not (contains subst var), 
	      fn () => print ("variable occurs in substitution"^(Name.var2string var)))
	     ]
	  else
	    ()
	    
	val subst = VarMap.insert (subst,var,value)
      in
	subst
      end

      fun substitute subst var =
	if !profile then 
	  (subst_total_size := !subst_total_size + (VarMap.numItems subst);
	   VarMap.find (subst,var))
	else
	    VarMap.find (subst,var)

      fun is_empty subst = (VarMap.numItems subst) = 0

      (* val compose : ('a subst -> 'a -> 'a) -> ('a subst * 'a subst) -> 'a subst
       *  subst_fn (compose subst_fn (subst2,subst1))
       *  is equivalent to (subst_fn subst2) o (subst_fn subst1)
       *)
      fun compose subst_fn (subst1,subst2) = 
	let
	  val subst2 = VarMap.map (subst_fn subst1) subst2
	  fun combine (value1,value2) = value2
	  val subst = VarMap.unionWith combine (subst1,subst2)
	in
	  subst
	end

      fun merge (subst1,subst2) =
	let
	  fun combine (value1,value2) = value2
	  val subst = VarMap.unionWith combine (subst1,subst2)
	in
	  subst
	end

      fun print (printer : 'a -> unit) (subst: 'a subst) = 
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

      
    local
      open NilRewrite

      fun exp_var_xxx ((exp_subst,con_subst),var,any) = 
	let
	  val var' = Name.derived_var var
	  val exp_subst = VarMap.insert (exp_subst,var,var')
	in
	  ((exp_subst,con_subst),SOME var')
	end

      fun con_var_xxx ((exp_subst,con_subst),var,any) = 
	let
	  val var' = Name.derived_var var
	  val con_subst = VarMap.insert (con_subst,var,var')
	in
	  ((exp_subst,con_subst),SOME var')
	end

      fun conhandler (subst as (exp_subst,con_subst),con : con) =
	(case con
	   of Var_c var => 
	     (case VarMap.find (con_subst,var)
		of SOME var => (CHANGE_NORECURSE (subst,Var_c var))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      fun exphandler (subst as (exp_subst,con_subst),exp : exp) =
	(case exp
	   of Var_e var => 
	     (case VarMap.find (exp_subst,var)
		of SOME var => (CHANGE_NORECURSE (subst,Var_e var))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      val exp_handlers = 
	let
	  val h = set_exphandler default_handler exphandler
	  val h = set_exp_binder h exp_var_xxx
	  val h = set_exp_definer h exp_var_xxx
	in
	  h
	end

      val {rewrite_exp = renameEVarsExp',
	   rewrite_con = renameEVarsCon',
	   rewrite_kind = renameEVarsKind',...} = rewriters exp_handlers

      val con_handlers = 
	let
	  val h = set_conhandler default_handler conhandler
	  val h = set_con_binder h con_var_xxx
	  val h = set_con_definer h con_var_xxx
	in
	  h
	end

      val {rewrite_exp = renameCVarsExp',
	   rewrite_con = renameCVarsCon',
	   rewrite_kind = renameCVarsKind',...} = rewriters con_handlers

      val all_handlers =  
	let
	  val h = set_conhandler exp_handlers conhandler
	  val h = set_con_binder h con_var_xxx
	  val h = set_con_definer h con_var_xxx
	in
	  h
	end

      val {rewrite_exp = renameExp',
	   rewrite_con = renameCon',
	   rewrite_kind = renameKind',
	   rewrite_bnd = renameBnd',
	   rewrite_cbnd = renameCBnd',
	   rewrite_mod = renameMod'} = rewriters all_handlers

      val empty = (VarMap.empty,VarMap.empty)

      fun renameBnd bnd = 
	let
	  val (bnds,substs) = renameBnd' empty bnd
	in
	  (hd bnds,substs)
	end

      fun renameCBnd bnd = 
	let
	  val (bnds,(esubst,subst)) = renameCBnd' empty bnd
	  val _ = 
	    if !debug then
	      assert (locate "POST:renameCBnd")
	      [
	       (is_empty esubst,fn () => TextIO.print "Renaming cbnd should not export evar changes")
	       ]
	    else ()
	in
	  (hd bnds,subst)
	end

    in
      val renameEVarsExp = renameEVarsExp' empty
      val renameEVarsCon = renameEVarsCon' empty
      val renameEVarsKind = renameEVarsKind' empty
	
      val renameCVarsExp = renameCVarsExp' empty
      val renameCVarsCon = renameCVarsCon' empty
      val renameCVarsKind = renameCVarsKind' empty

      val renameExp = renameExp' empty
      val renameCon = renameCon' empty
      val renameKind = renameKind' empty
      val renameMod = renameMod' empty
      val renameBnd = renameBnd
      val renameCBnd = renameCBnd
    end
  

    local
      open NilRewrite

      type state = {esubst : var VarMap.map,
		    csubst : var VarMap.map,
		    epred : var -> bool,
		    cpred : var -> bool}

      fun exp_var_xxx (state :state as {esubst,csubst,epred,cpred},var,any) = 
	if epred var then
	  let
	    val var' = Name.derived_var var
	    val esubst = VarMap.insert (esubst,var,var')
	  in
	    ({esubst=esubst,csubst=csubst,epred=epred,cpred=cpred},SOME var')
	  end
	else (state,NONE)

      fun con_var_xxx (state :state as {esubst,csubst,epred,cpred},var,any) = 
	if cpred var then
	  let
	    val var' = Name.derived_var var
	    val csubst = VarMap.insert (csubst,var,var')
	  in
	    ({esubst = esubst,csubst = csubst, epred = epred, cpred = cpred},SOME var')
	  end
	else (state,NONE)
	  
      fun conhandler (state : state as {csubst,...},con : con) =
	(case con
	   of Var_c var => 
	     (case VarMap.find (csubst,var)
		of SOME var => (CHANGE_NORECURSE (state,Var_c var))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      fun exphandler (state : state as {esubst,...},exp : exp) =
	(case exp
	   of Var_e var => 
	     (case VarMap.find (esubst,var)
		of SOME var => (CHANGE_NORECURSE (state,Var_e var))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      val all_handlers = 
	let
	  val h = set_exphandler default_handler exphandler
	  val h = set_exp_binder h exp_var_xxx
	  val h = set_exp_definer h exp_var_xxx
	  val h = set_conhandler h conhandler
	  val h = set_con_binder h con_var_xxx
	  val h = set_con_definer h con_var_xxx
	in
	  h
	end

      val {rewrite_exp = renameExpWRT',
	   rewrite_con = renameConWRT',
	   rewrite_kind = renameKindWRT',
	   rewrite_bnd = renameBndWRT',
	   rewrite_cbnd = renameCBndWRT',
	   rewrite_mod = renameModWRT'} = rewriters all_handlers

      fun empty (epred,cpred) = {esubst = VarMap.empty,csubst = VarMap.empty,epred = epred,cpred = cpred}

      fun renameBndWRT preds bnd = 
	let
	  val (bnds,substs) = renameBndWRT' (empty preds) bnd
	in
	  (hd bnds,substs)
	end

(*      fun renameCBndWRT bnd = 
	let
	  val (bnds,(esubst,subst)) = renameCBnd' empty bnd

	    if !debug then
	      assert (locate "POST:renameCBnd")
	      [
	       (is_empty esubst,fn () => TextIO.print "Renaming cbnd should not export evar changes")
	       ]
	    else ()
	in
	  (hd bnds,subst)
	end
*)
    in
      fun renameExpWRT preds = renameExpWRT' (empty preds)
      fun renameConWRT preds = renameConWRT' (empty preds)
      fun renameKindWRT preds = renameKindWRT' (empty preds)
    end

    (*Is renamed predicate *)
    local 
      open NilRewrite

      val find = HashTable.find
      val insert = HashTable.insert

      exception Rebound of var
      exception Unbound 

      type varset = (var,unit) HashTable.hash_table
      type state = {cpred : var -> bool,
		    epred : var -> bool,
		    cbound : varset,
		    ebound : varset}

      fun exp_var_xxx (state : state as {epred,ebound,...},var,any) = 
	(if isSome (find ebound var) orelse (epred var)
	   then raise Rebound var
	 else (insert ebound (var,());(state,NONE)))

      fun con_var_xxx (state :state as {cpred,cbound,...},var,any) = 
	(if isSome (find cbound var) orelse (cpred var)
	   then raise Rebound var
	 else (insert cbound (var,());(state,NONE)))

      val all_handlers =  
	let
	  val h = set_con_binder default_handler con_var_xxx
	  val h = set_con_definer h con_var_xxx
	  val h = set_exp_binder h exp_var_xxx
	  val h = set_exp_definer h exp_var_xxx
	in
	  h
	end

      val {rewrite_exp = checkExp,
	   rewrite_con = checkCon,
	   rewrite_kind = checkKind,
	   rewrite_mod = checkMod,...} = rewriters all_handlers

      fun isRenamedXXX checker (epred,cpred) item = 
	let
	  val cbound = Name.mk_var_hash_table(20,Unbound)
	  val ebound = Name.mk_var_hash_table(20,Unbound)
	in 
	  ((checker {cpred = cpred,epred = epred,cbound = cbound,ebound = ebound} item; 
	    true)
	   handle Rebound var => 
	     (lprintl ("Variable "^(Name.var2string var)^" rebound");
	      false))
	end

      fun ff _ = false
    in
      val isRenamedExp = isRenamedXXX checkExp (ff,ff)
      val isRenamedCon = isRenamedXXX checkCon (ff,ff)
      val isRenamedKind = isRenamedXXX checkKind (ff,ff)
      val isRenamedMod = isRenamedXXX checkMod (ff,ff)
      val isRenamedExpWRT = isRenamedXXX checkExp
      val isRenamedConWRT = isRenamedXXX checkCon
      val isRenamedKindWRT = isRenamedXXX checkKind
    end



    local
      open NilRewrite

      val efree = ref VarSet.empty
      val cfree = ref VarSet.empty

      type state = {bound : VarSet.set}

      fun exp_var_xxx (state : state as {bound},var,any) = 
        ({bound = VarSet.add (bound, var)}, NONE)

      fun con_var_xxx (state : state as {bound},var,any) = 
        ({bound = VarSet.add (bound, var)}, NONE)

      fun conhandler (state as {bound},con : con) =
	(case con
	   of Var_c var => 
	      	if (VarSet.member (bound, var)) then
                   (cfree := VarSet.add(!cfree, var); NOCHANGE)
                else
                   NOCHANGE
	    | _ => NOCHANGE)

      fun exphandler (state as {bound}, exp : exp) =
	(case exp
	   of Var_e var => 
	      	if (VarSet.member (bound, var)) then
                   (efree := VarSet.add(!efree, var); NOCHANGE)
                else
                   NOCHANGE
	    | _ => NOCHANGE)

      val exp_con_handler = 
	let
	  val h = set_conhandler default_handler conhandler
	  val h = set_exphandler h  exphandler
	  val h = set_con_binder h con_var_xxx
	  val h = set_con_definer h con_var_xxx
	  val h = set_exp_binder h exp_var_xxx
	  val h = set_con_binder h exp_var_xxx
	in h 
	end

      val {rewrite_con = freeInCon',
	   rewrite_exp = freeInExp',
	   rewrite_kind = freeInKind',...} = rewriters exp_con_handler
 
      val empty_state  : state = {bound = VarSet.empty}

      fun freeInXXX freeer item = 
	let
          val _ = (efree := VarSet.empty;
	           cfree := VarSet.empty)
	  val _  = freeer empty_state item
          val answer = (!efree, !cfree)
          val _ = (efree := VarSet.empty;
	           cfree := VarSet.empty)
	in
	  answer
	end

    in
      val freeInCon = freeInXXX freeInCon'
      val freeInExp = freeInXXX freeInExp'
      val freeInKind = freeInXXX freeInKind'
    end


    (* Substitutions *)
    local
      open NilRewrite

      exception Rebound of var

      type varset = VarSet.set

      type state = {cfree : varset,
		    efree : varset,
		    esubst : exp subst,
		    csubst : con subst}

      fun exp_var_xxx (state : state as {efree,cfree,esubst,csubst},var,any) = 
	(if VarSet.member (efree,var) 
	   then let val var' = Name.derived_var var
                in ({efree = efree, cfree = cfree,
                     csubst = csubst,
                     esubst = add esubst (var, Var_e var')},
                    SOME var')
                end
  	   else (state, NONE))


      fun con_var_xxx (state : state as {efree,cfree,esubst,csubst},var,any) = 
	(if VarSet.member (cfree,var) 
	   then let val var' = Name.derived_var var
                in ({efree = efree, cfree = cfree,
                     esubst = esubst,
                     csubst = add csubst (var, Var_c var')},
                    SOME var')
                end
  	   else (state, NONE))

      fun conhandler (state : state as {csubst,...},con : con) =
	(case con
	   of Var_c var => 
	     (case substitute csubst var
		of SOME con => 
		  (subst_counter(); 
		   CHANGE_NORECURSE (state,renameCon con))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)
	   
      fun exphandler (state : state as {esubst,...},exp : exp) =
	(case exp
	   of Var_e var => 
	     (case substitute esubst var
		of SOME exp => 
		  (subst_counter(); 
		   CHANGE_NORECURSE (state,renameExp exp))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      val exp_con_handler = 
	let
	  val h = set_conhandler default_handler conhandler
	  val h = set_exphandler h  exphandler
	  val h = set_con_binder h con_var_xxx
	  val h = set_con_definer h con_var_xxx
	  val h = set_exp_binder h exp_var_xxx
	  val h = set_con_binder h exp_var_xxx
	in h 
	end

      val {rewrite_con = substExpConInCon',
	   rewrite_exp = substExpConInExp',
	   rewrite_kind = substExpConInKind',
           rewrite_cbnd = substExpConInCBnd',
           rewrite_bnd = substExpConInBnd',...} = rewriters exp_con_handler
 
      fun empty_state (esubst : exp subst,csubst : con subst) : state = 
	let
           fun folder freeer (substituted_val, (efree,cfree)) =
             let val (efree', cfree') = freeer substituted_val
             in
                 (VarSet.union (efree, efree'),
                  VarSet.union (cfree, cfree'))
             end
           val empty_frees = (VarSet.empty, VarSet.empty)
           val frees = 
                VarMap.foldl (folder freeInExp) empty_frees esubst
           val (efree, cfree) = 
                VarMap.foldl (folder freeInCon) frees csubst
	in	
 	   {esubst = esubst, csubst = csubst,
	    efree = efree, cfree = cfree}
        end

      fun substExpConInXXX substituter (esubst,csubst) item = 
	let
	  val item =  substituter (empty_state (esubst,csubst)) item
	in
	  item
	end

      fun substExpInXXX substituter esubst item =
            substituter (empty_state (esubst, empty())) item
      fun substConInXXX substituter csubst item =
            substituter (empty_state (empty(), csubst)) item

    in
      type con_subst = con subst
      type exp_subst = exp subst

      val substConInCon = substConInXXX substExpConInCon'
      val substExpInExp = substExpInXXX substExpConInExp'
      val substExpInCon = substExpInXXX substExpConInCon'
      val substConInExp = substConInXXX substExpConInExp'
      val substConInKind = substConInXXX substExpConInKind'
      val substExpInKind = substExpInXXX substExpConInKind'
      fun substConInCBnd csubst bnd = 
	let
	  val bnd = 
	    (case substExpConInCBnd' (empty_state (empty(), csubst)) bnd
	       of ([bnd],state) => bnd
		| _ => error "substConInCBnd" "Substitution should not change number of bnds")
	in
	  bnd
	end

      fun substConInBnd csubst bnd = 
	let
	  val bnd = 
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

    (*Substitutions for single variables*)
    local
      open NilRewrite

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

      fun varXXXYYYSubst (name,XXXrenamed,YYYrenamed,XXXprint,YYYprint,substitute) var Xitem Yitem =
	let
	  val _ = 
	    if !debug then 
	      assert (locate ("PRE:"^name))
	      [
	       (XXXrenamed Xitem,
		fn () => (lprintl ("Unrenamed item passed to "^name^" for substitution");
			  XXXprint Xitem)),
	       (YYYrenamed Yitem,
		fn () => (lprintl ("Item not renamed passed to "^name);
			  YYYprint Yitem))
	       ]
	    else ()

	  val item = substitute (empty_state (var,Xitem)) Yitem

	  val _ = 
	    if !debug then 
	      assert (locate ("POST:"^name))
	      [
(*	       (YYYrenamed item,
		fn () => (lprintl ("Failure to rename properly in "^name);
			  YYYprint item)) *)
	       ]
	    else ()
	in
	  item
	end

    in
      val varConExpSubst  = varXXXYYYSubst ("varConExpSubst",isRenamedCon,isRenamedExp,pp_con,pp_exp,varConExpSubst')
      val varConConSubst  = varXXXYYYSubst ("varConConSubst",isRenamedCon,isRenamedCon,pp_con,pp_con,varConConSubst')
      val varConKindSubst = varXXXYYYSubst ("varConKindSubst",isRenamedCon,isRenamedKind,pp_con,pp_kind,varConKindSubst')

      val varExpExpSubst  = varXXXYYYSubst ("varExpExpSubst",isRenamedExp,isRenamedExp,pp_exp,pp_exp,varExpExpSubst')
      val varExpConSubst  = varXXXYYYSubst ("varExpConSubst",isRenamedExp,isRenamedCon,pp_exp,pp_con,varExpConSubst')
      val varExpKindSubst = varXXXYYYSubst ("varExpKindSubst",isRenamedExp,isRenamedKind,pp_exp,pp_kind,varExpKindSubst')
    end



    val con_subst_compose = compose substConInCon
      
    val printConSubst = print Ppnil.pp_con

  end
