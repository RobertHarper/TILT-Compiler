(* This stucture implements and abstract type of substitutions, supporting
 * both simultaneuous (parallel) and sequential substitutions.  Included
 * are notions of composition of substitutions
 *)

structure NilSubst :> NILSUBST = 
  struct

    (* IMPORTS *)
    open Curtain
    open Nil

    (* Stats *)
    val debug         = Stats.ff "nil_debug"
    val profile       = Stats.ff "nil_profile"
    val subst_profile = Stats.ff "subst_profile"

    val subtimer = fn args => fn args2 => if !profile orelse !subst_profile then Stats.subtimer args args2 else #2 args args2


    (* Option *)
    val isSome = Option.isSome

    (*Listops *)
    val foldl_acc  = Listops.foldl_acc
    val map_second = Listops.map_second
    val unzip      = Listops.unzip
    val zip        = Listops.zip

    val locate = NilError.locate "Subst"
    val assert = NilError.assert

    (* Util *)
    val mapopt   = Util.mapopt
    val lprintl  = Util.lprintl
    val printl   = Util.printl

    fun error s s' = Util.error (locate s) s'

    fun error' s = error "" s

    (* Name *)
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet


    (* Thunks.  This is useful to avoid renaming things that
     * won't actually be substituted in.
     *)
    local
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

    type 'a map = 'a VarMap.map

    structure CSubst = CurtainSubst.CSubst

    type con_subst = CSubst.item_subst
    (* The definition of the types.  These are exported*)
    type exp_subst = exp delay map

    (* How to carry out substitutions on the various levels.  Uses the rewriter.
     *)
    local
      open NilRewrite

      type state = {esubst : exp_subst,csubst : con_subst}

      val substitute = VarMap.find

      val add = VarMap.insert

      fun is_empty s = (VarMap.numItems s) = 0

      fun empty ()   = VarMap.empty

      fun kindhandler(state : state as {esubst,csubst},kind : kind) = 
	let


	  fun makeLet c = 
	    let
	      val items    = CSubst.toList csubst
	      val cbnds    = map Con_cb items
	    in C.Let_c(Sequential,cbnds,c)
	    end

	in
(*	  if is_empty esubst andalso is_empty csubst then NORECURSE
	  else*)
	    if is_empty esubst then
	      (case K.expose kind of
		 SingleType_k c => CHANGE_NORECURSE(state,K.SingleType_k (makeLet c))
	       | Single_k c     => CHANGE_NORECURSE(state,K.Single_k (makeLet c))
	       | _ => NOCHANGE)
	    else NOCHANGE
	end


      (* What to do with constructors.  If the constructor is a variable,
       * see if it is in the subst.  If it is a projection, you might as
       * well beta-reduce it.  (This turns out to be a big win) 
       *)
      fun conhandler (state : state as {csubst,esubst},con : con) =
	let
	  fun do_trim() = NOCHANGE
(*	    let val csubst = trim(csubst,freeConVarInCon con)
	    in if is_empty csubst andalso is_empty esubst then NORECURSE
	       else NOCHANGE
	    end*)
	in
	  (case C.expose con
	     of Var_c var => 
	       (case CSubst.substitute csubst var
		  of SOME con => 
		    CHANGE_NORECURSE (state,con)
		   | _ => NORECURSE)
	      | (Proj_c (c,label)) => 
		  (case C.expose c 
		     of (Var_c var) =>
		       (case CSubst.substitute csubst var
			  of SOME con2 => 
			    let 
			      val res = (case C.expose con2 of
					   (Crecord_c entries) => 
					     #2(valOf(List.find (fn ((l,_)) => Name.eq_label (l,label)) entries ))
					 | _ => C.hide(Proj_c(con2,label)))
			    in CHANGE_NORECURSE (state,res)
			    end
			   | NONE => NORECURSE)
		      | _ => do_trim())
	      | _ => do_trim())
	end
	
      (* What to do with an exp.
       *)
      fun exphandler (state : state as {esubst,csubst},exp : exp) =
	(case E.expose exp
	   of Var_e var => 
	     (case substitute (esubst,var)
		of SOME exp_delay => CHANGE_NORECURSE (state,thaw exp_delay)
		 | _ => NORECURSE)
	    | _ => NOCHANGE
(*		let val csubst = trim(csubst,freeConVarInExp exp)
		in if is_empty csubst andalso is_empty esubst then NORECURSE
		   else NOCHANGE
		end*))

      (* Set the handlers, starting with a default handler
       *)
      val exp_con_handler = 
	let
	  val h = set_conhandler default_handler conhandler
	  val h = set_exphandler h  exphandler
	  val h = set_kindhandler h kindhandler
	in h 
	end

      (* Generate the rewriters.
       *)
      val {rewrite_con = substExpConInCon',
	   rewrite_exp = substExpConInExp',
	   rewrite_kind = substExpConInKind',
           rewrite_cbnd = substExpConInCBnd',
           rewrite_bnd = substExpConInBnd',
	   rewrite_trace = substExpConInTrace',...} = rewriters exp_con_handler
 
      fun empty_state (esubst : exp delay map,csubst : con_subst) : state = 
	{esubst = esubst, csubst = csubst}

      (* Given a rewriter, carry out an expression and constructor
       * substitution in an item
       *)
      fun substExpConInXXX substituter (esubst,csubst) item = 
	let
	  val item =  	
	    if (is_empty esubst) andalso (CSubst.is_empty csubst) then item 
	    else substituter (empty_state (esubst,csubst)) item
	in item
	end

      (* Given a rewriter, carry out an expression substitutions
       *)
      fun substExpInXXX substituter esubst item =
	if (is_empty esubst) then item
	else substituter (empty_state (esubst, CSubst.empty())) item

    in
      val substExpInExp   = fn s => subtimer("Subst:substExpInExp",  substExpInXXX substExpConInExp' s)
      val substExpInCon   = fn s => subtimer("Subst:substExpInCon",  substExpInXXX substExpConInCon' s)
      val substExpInKind  = fn s => subtimer("Subst:substExpInKind", substExpInXXX substExpConInKind' s)
      val substExpConInExp  = fn s => subtimer("Subst:substExpConInExp", substExpConInXXX(substExpConInExp')  s)
      val substExpConInCon  = fn s => subtimer("Subst:substExpConInCon", substExpConInXXX(substExpConInCon')  s) 
      val substExpConInKind = fn s => subtimer("Subst:substExpConInKind",substExpConInXXX(substExpConInKind') s) 

    end  



    (* Here we define the abstract interface for substitutions.  This section 
     * defines the bits that are generic to all levels in a functor parameterized 
     * by the actual substitution functions, which are level specific. 
     *)

    functor SubstFn(type item                                         (*What is it: e.g. con, exp *)
		    type item_subst = item delay VarMap.map           (*The substitution type  *)
		    val substItemInItem : item_subst -> item -> item  (*For composition *)
		    val renameItem : item -> item)                       (*To print them out *)
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
	in VarMap.insert (VarMap.map domap subst,var,item_delay)
	end
      
      fun addr  (subst,var,item) = 
	VarMap.insert (subst,var,delay (fn () => substItemInItem subst (renameItem item)))

      fun is_empty subst = (VarMap.numItems subst) = 0
	
      fun compose (subst1,subst2) = 
	let
	  fun domap item_delay = delay (fn () => substItemInItem subst1 (thaw item_delay))
	  val subst2 = VarMap.map domap subst2
	  val subst = VarMap.unionWith #2 (subst1,subst2)
	in subst
	end
      
      fun merge (subst1,subst2) = VarMap.unionWith #2 (subst1,subst2)

      fun simFromList (list : (var * item) list) : item_subst = 
	let
	  fun fold ((var,value),subst) = 
	    VarMap.insert(subst,var,rename value)
	    
	  val subst =  List.foldl fold VarMap.empty list
	in subst
	end

      fun seqFromList (list : (var * item) list) : item_subst = 
	let
	  fun fold ((var,value),subst) = 
	    VarMap.insert (subst,var,delay (fn () => substItemInItem subst (renameItem value)))
	  val subst =  List.foldl fold VarMap.empty list
	in subst
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

    end


    structure C = CSubst
    val substConInCon  = fn s => subtimer("Subst:substConInCon", CurtainSubst.substConInCon  s)
    val substConInExp  = fn s => subtimer("Subst:substConInExp", CurtainSubst.substConInExp  s)
    val substConInKind = fn s => subtimer("Subst:substConInKind",CurtainSubst.substConInKind s)

    val substConInTrace = fn s => subtimer("Subst:substConInTrace",CurtainSubst.substConInTrace s)
    val substConInBnd   = fn s => subtimer("Subst:substConInBnd",CurtainSubst.substConInBnd s)
    val substConInCBnd  = fn s => subtimer("Subst:substConInCBnd",CurtainSubst.substConInCBnd s)

    (* Instantiate the functor for expressions*)
    structure E = SubstFn(type item = exp
			  type item_subst = exp_subst
			  val substItemInItem = substExpInExp
			  val renameItem = NilRename.renameExp)

    (*Substitutions for one variable
     *)
    local 
      fun renameExp (exp :exp) : exp delay = delay (fn () => NilRename.renameExp exp)
    in

      fun varConExpSubst var con exp   = substConInExp  (C.sim_add (C.empty()) (var,con)) exp
      fun varConConSubst var con con2  = substConInCon  (C.sim_add (C.empty()) (var,con)) con2
      fun varConKindSubst var con kind = substConInKind (C.sim_add (C.empty()) (var,con)) kind

      fun varExpExpSubst var exp1 exp2 = substExpInExp (VarMap.insert (VarMap.empty,var,renameExp exp1)) exp2
      fun varExpConSubst var exp con = substExpInCon (VarMap.insert (VarMap.empty,var,renameExp exp)) con
      fun varExpKindSubst var exp kind = substExpInKind (VarMap.insert (VarMap.empty,var,renameExp exp)) kind
    end

  end
