(*$import Ppnil NILSUBST Stats *)

structure NilSubst :> NILSUBST 
  = 
  struct
    open Nil

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

      val derived_var = Name.derived_var

(*      fun derived_var v = 
	if !debug then
	  let
	    val var = Name.derived_var v
	  in
	    (Util.lprintl ("Renaming "^(Name.var2string v)^" to "^(Name.var2string var));
	     var)
	  end
	else Name.derived_var v
	*)
      fun exp_var_xxx ((exp_subst,con_subst),var,any) = 
	let
	  val var' = derived_var var
	  val exp_subst = VarMap.insert (exp_subst,var,var')
	in
	  ((exp_subst,con_subst),var')
	end

      fun con_var_xxx ((exp_subst,con_subst),var,any) = 
	let
	  val var' = derived_var var
	  val con_subst = VarMap.insert (con_subst,var,var')
	in
	  ((exp_subst,con_subst),var')
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
	 else (insert ebound (var,());(state,var)))

      fun con_var_xxx (state :state as {cpred,cbound,...},var,any) = 
	(if isSome (find cbound var) orelse (cpred var)
	   then raise Rebound var
	 else (insert cbound (var,());(state,var)))

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

    (*Substitutions*)
    local 
      open NilRewrite

      fun conhandler (subst,con : con) =
	(case con
	   of Var_c var => 
	     (case substitute subst var
		of SOME con => (subst_counter(); CHANGE_NORECURSE (subst,renameCon con))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      fun exphandler (subst,exp : exp) =
	(case exp
	   of Var_e var => 
	     (case substitute subst var
		of SOME exp => (subst_counter(); CHANGE_NORECURSE (subst,renameExp exp))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      val con_handler = set_conhandler default_handler conhandler

      val exp_handler = set_exphandler default_handler exphandler

      val {rewrite_con = substConInCon',
	   rewrite_exp = substConInExp',
	   rewrite_kind = substConInKind',
	   rewrite_cbnd = substConInCBnd',
	   rewrite_bnd = substConInBnd',...} = rewriters con_handler

      val {rewrite_con = substExpInCon',
	   rewrite_exp = substExpInExp',
	   rewrite_kind = substExpInKind',...} = rewriters exp_handler


      fun substXXXInYYY (name,renamedWRT,XXXprint,YYYprint,peek,substituter,renamed) subst item = 
	let
	  val _ = 
	    if !debug then 
	      assert (locate ("PRE:"^name))
	      [
	       (renamedWRT (peek subst) item,
		fn () => (lprintl ("Item not renamed passed to "^name);
			  printl "Subst is:";
			  print XXXprint subst;
			  lprintl "Substituted into:";
			  YYYprint item))
	       ]
	    else ()

	  val item = 
	    if is_empty subst 
	      then item
	    else substituter subst item

	  val _ = 
	    if !debug then 
	      assert (locate ("POST:"^name))
	      [
	       (renamed item,
		fn () => (lprintl ("Failure to rename properly in "^name);
			  YYYprint item))
	       ]
	    else ()
	in
	  item
	end
			
      fun peek subst var = isSome (substitute subst var)
      fun ff _ = false

      fun peek_c subst = (peek subst,ff)
      fun peek_e subst = (ff,peek subst)
      val pp_kind = Ppnil.pp_kind
      val pp_con = Ppnil.pp_con
      val pp_exp = Ppnil.pp_exp
    in
      type con_subst = con subst
      type exp_subst = exp subst

      val substConInCon  = substXXXInYYY ("substConInCon",isRenamedConWRT,pp_con,pp_con,peek_c,
					  substConInCon',isRenamedCon)
      val substExpInExp  = substXXXInYYY ("substExpInExp",isRenamedExpWRT,pp_exp,pp_exp,peek_e,
					  substExpInExp',isRenamedExp)
      val substExpInCon  = substXXXInYYY ("substExpInCon",isRenamedConWRT,pp_exp,pp_con,peek_e,
					  substExpInCon',isRenamedCon)
      val substConInExp  = substXXXInYYY ("substConInExp",isRenamedExpWRT,pp_con,pp_exp,peek_c,
					  substConInExp',isRenamedExp)
      val substConInKind = substXXXInYYY ("substConInKind",isRenamedKindWRT,pp_con,pp_kind,peek_c,
					  substConInKind',isRenamedKind)
      val substExpInKind = substXXXInYYY ("substExpInKind",isRenamedKindWRT,pp_exp,pp_kind,peek_e,
					  substExpInKind',isRenamedKind)
      fun substConInCBnd subst bnd = 
	let
	  val bnd = 
	    (case substConInCBnd' subst bnd
	       of ([bnd],subst) => bnd
		| _ => error "substConInCBnd" "Substitution should not checnge number of bnds")
	in
	  bnd
	end

      fun substConInBnd subst bnd = 
	let
	  val bnd = 
	    (case substConInBnd' subst bnd
	       of ([bnd],subst) => bnd
		| _ => error "substConInBnd" "Substitution should not checnge number of bnds")
	in
	  bnd
	end

    end

    (*Combined exp and con substs*)
    local
      open NilRewrite

      fun conhandler ((esubst,csubst),con : con) =
	(case con
	   of Var_c var => 
	     (case substitute csubst var
		of SOME con => (subst_counter(); CHANGE_NORECURSE ((esubst,csubst),renameCon con))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)
	   
      fun exphandler ((esubst,csubst),exp : exp) =
	(case exp
	   of Var_e var => 
	     (case substitute esubst var
		of SOME exp => (subst_counter(); CHANGE_NORECURSE ((esubst,csubst),renameExp exp))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      val exp_con_handler = set_exphandler (set_conhandler default_handler conhandler) exphandler

      val {rewrite_con = substExpConInCon',
	   rewrite_exp = substExpConInExp',
	   rewrite_kind = substExpConInKind',...} = rewriters exp_con_handler
	


      val pp_kind = Ppnil.pp_kind
      val pp_con = Ppnil.pp_con
      val pp_exp = Ppnil.pp_exp
	
      fun substExpConInXXX (name,renamedWRT,XXXprint,substituter,esubstituter,csubstituter,renamed) (esubst,csubst) item = 
	let

	  fun peek subst var = isSome (substitute subst var)

	  val _ = 
	    if !debug then 
	      assert (locate ("PRE:substExpConIn"^name))
	      [
	       (renamedWRT (peek esubst,peek csubst) item,
		(fn () => (lprintl ("Item not renamed passed to substExpConIn"^name);
			   printl "ESubst is:";
			   print pp_exp esubst;
			   printl "CSubst is:";
			   print pp_con csubst;
			   lprintl "Substituted into:";
			   XXXprint item)))
	       ]
	    else ()

	  val item = 
	    if is_empty esubst then
	      csubstituter csubst item
	    else if is_empty csubst then
	      esubstituter esubst item
		 else substituter (esubst,csubst) item

	  val _ = 
	    if !debug then 
	      assert (locate ("POST:substExpConIn"^name))
	      [
	       (renamed item,
		(fn () => (lprintl ("Failure to rename properly in substExpConIn"^name);
			   XXXprint item)))
	       ]
	    else ()
	in
	  item
	end
    in
      val substExpConInExp  = substExpConInXXX("Exp",isRenamedExpWRT,pp_exp,
					       substExpConInExp',substExpInExp,substConInExp,
					       isRenamedExp)
      val substExpConInCon  = substExpConInXXX("Con",isRenamedConWRT,pp_con,
					       substExpConInCon',substExpInCon,substConInCon,
					       isRenamedCon)

      val substExpConInKind = substExpConInXXX("Kind",isRenamedKindWRT,pp_kind,
					       substExpConInKind',substExpInKind,substConInKind,
					       isRenamedKind)

    end  

    (*Substitutions for single variables*)
    local
      open NilRewrite

      fun varconhandler (state as (subst_var,subst_con ),con : con) =
	(case con
	   of Var_c var => if Name.eq_var (subst_var,var) then CHANGE_NORECURSE (state,subst_con) else NORECURSE
	    | _ => NOCHANGE)
	   
      fun varexphandler (state as (subst_var,subst_exp ),exp : exp) =
	(case exp
	   of Var_e var => if Name.eq_var (subst_var,var) then CHANGE_NORECURSE (state,subst_exp) else NORECURSE
	    | _ => NOCHANGE)
	   
      val var_con_handler = set_conhandler default_handler varconhandler

      val var_exp_handler = set_exphandler default_handler varexphandler

      val {rewrite_exp = varConExpSubst',
	   rewrite_con = varConConSubst',
	   rewrite_kind = varConKindSubst',...} = rewriters var_con_handler
	
      val {rewrite_exp = varExpExpSubst',
	   rewrite_con = varExpConSubst',
	   rewrite_kind = varExpKindSubst',...} = rewriters var_exp_handler
	
      val pp_kind = Ppnil.pp_kind
      val pp_con = Ppnil.pp_con
      val pp_exp = Ppnil.pp_exp
	
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

	  val item = substitute (var,Xitem) Yitem

	  val _ = 
	    if !debug then 
	      assert (locate ("POST:"^name))
	      [
	       (YYYrenamed item,
		fn () => (lprintl ("Failure to rename properly in "^name);
			  YYYprint item))
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
