(*$import Ppnil NILSUBST Stats NilError NilRewrite NilRename Option *)

structure NilSubst :> NILSUBST = 
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

    val timer = Stats.timer
    val subtimer = Stats.subtimer

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

      
    (* Substitutions *)
    local
      open NilRewrite

      exception Rebound of var

      type varset = VarSet.set

      type state = {esubst : exp subst,
		    csubst : con subst}

      val contains = fn subst => subtimer ("Subst:contains",contains subst)
      val substitute = fn subst => subtimer ("Subst:substitute",substitute subst)
	
      fun exp_var_xxx (state : state as {esubst,csubst},var,any) = 
	let val var' = Name.derived_var var
	in ({csubst = csubst,
	     esubst = 
	     if (contains esubst var) then
	       error (locate "exp_var_xxx") "already bound"
	     else
	       add esubst (var, Var_e var')},
	    SOME var')
	end


      fun con_var_xxx (state : state as {esubst,csubst},var,any) = 
	let val var' = Name.derived_var var
	in ({esubst = esubst,
	     csubst = 
	     if (contains csubst var) then
	       error (locate "con_var_xxx") "already bound"
	     else add csubst (var, Var_c var')},
	    SOME var')
	end

      fun conhandler (state : state as {csubst,...},con : con) =
	(case con
	   of Var_c var => 
	     (case substitute csubst var
		of SOME con => 
		  (subst_counter(); 
		   CHANGE_NORECURSE (state,con))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)
	   
      fun exphandler (state : state as {esubst,...},exp : exp) =
	(case exp
	   of Var_e var => 
	     (case substitute esubst var
		of SOME exp => 
		  (subst_counter(); 
		   CHANGE_NORECURSE (state,exp))
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
 
      fun empty_state (esubst : exp subst,csubst : con subst) : state = 
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



    val con_subst_compose = compose substConInCon
      
    val printConSubst = print Ppnil.pp_con

  end
