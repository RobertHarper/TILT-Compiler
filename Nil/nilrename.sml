(*$import Ppnil NILRENAME Stats NilError NilRewrite Option *)

structure NilRename :> NILRENAME = 
  struct
    open Nil
      
    val lprintl = Util.lprintl
    val printl = Util.printl

    val locate = NilError.locate "Rename"
    val assert = NilError.assert

    fun error s s' = Util.error (locate s) s'

    fun error' s = error "" s

    val isSome = Option.isSome

    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet

    type 'a map = 'a Name.VarMap.map

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
      (*Default trace handler should suffice
       *)
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
	   rewrite_mod = renameMod',...} = rewriters all_handlers

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
	   rewrite_mod = renameModWRT',...} = rewriters all_handlers

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


  end
