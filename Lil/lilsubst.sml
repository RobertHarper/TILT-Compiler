(* This stucture implements and abstract type of substitutions, supporting
 * both simultaneuous (parallel) and sequential substitutions.  Included
 * are notions of composition of substitutions
 *)

structure LilSubst :> LILSUBST = 
  struct

    (* IMPORTS *)
    open Lil
    structure LO = Listops

    (* Stats *)
    val debug         = Stats.ff "lil_debug"
    val simplify      = Stats.tt "LilSubstSimplify"
    (* Option *)
    val isSome = Option.isSome

    (*Listops *)
    val foldl_acc  = Listops.foldl_acc
    val map_second = Listops.map_second
    val unzip      = Listops.unzip
    val zip        = Listops.zip


    (* Util *)
    val mapopt   = Util.mapopt
    val lprintl  = Util.lprintl
    val printl   = Util.printl
      
    fun error s = Util.error "lilsubst.sml" s
      
    local
      val pp_conr : (con -> unit) ref = ref (fn c => error "pp_con not defined")
    in
      fun install {pp_con : con -> unit} = pp_conr := pp_con
      fun pp_con c = (!pp_conr) c
    end

    (* Name *)
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    (* Thunks.  This is useful to avoid renaming things that
     * won't actually be substituted in.
     *)
    structure Delay :> SUBST_DELAY =
    struct
      datatype 'a thunk = FROZEN of (unit -> 'a) | THAWED of 'a
      type 'a delay = 'a thunk ref
      val eager = Stats.ff "lilsubst_eager"
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
    end (* Delay *)
    open Delay

 
    type 'a map = 'a VarMap.map

    (* The definition of the types.  These are exported*)
    type sv32_subst  = sv32 delay map 
    type sv64_subst  = sv64 delay map 
    type con_subst  = con delay map 
    type kind_subst = kind delay map 

    fun restrict (subst : 'a delay map,frees : VarSet.set) = 
      let
(*	val _ = print "\nFiltering subst\n";
	val _ = VarMap.appi (fn (a,c) => (print ((Name.var2string a)^"=");pp_con (thaw c);print "\t")) subst
	val _ = print "\n with frees \n";
	val _ = VarSet.app (fn a => print ((Name.var2string a)^"\t")) frees*)
	val subst = VarMap.filteri (fn (v,a) => VarSet.member(frees,v)) subst
(*	val _ = print "\nNew subst:\n";
	val _ = VarMap.appi (fn (a,c) => (print ((Name.var2string a)^"=");pp_con (thaw c);print "\t")) subst
	val _ = print "\n"*)
      in subst
      end
    (* How to carry out substitutions on the various levels.  Uses the rewriter.
     *)
    local
      open LilRewrite


      type state = {ksubst : kind_subst option,
		    csubst : con_subst option,
		    sv64subst : sv64_subst option,
		    sv32subst : sv32_subst option}

      fun substitute (s,var) = VarMap.find (s,var)

      fun is_empty s = (VarMap.numItems s) = 0

      fun empty ()   = VarMap.empty

      fun kindhandler(state : state as {ksubst,...},kind : kind) = 
	(case ksubst
	   of NONE => NORECURSE
	    | SOME ksubst => 
	     (case #k kind 
		of Var_k j => 
		  (case substitute (ksubst,j)
		     of SOME kind_delay => 
		       CHANGE_NORECURSE (state,thaw kind_delay)
		      | _ => NORECURSE)
		 | _ => 
		     let
		       val ksubst = restrict (ksubst,free_kvars_kind kind)
		     in 
		       if is_empty ksubst then 
			 NORECURSE
		       else
			 CHANGE_RECURSE({csubst = NONE,ksubst = SOME ksubst,sv32subst = NONE,sv64subst = NONE},kind)
		     end))


      fun conhandler (state : state as {csubst,ksubst,sv32subst,sv64subst},con : con) =
	(case csubst 
	   of NONE => 
	     (case ksubst 
		of NONE => NORECURSE 
		 | SOME ksubst => 
		  let
		    val ksubst = restrict (ksubst,free_kvars_con con)
		  in 
		    if is_empty ksubst then 
		      NORECURSE
		    else
		      CHANGE_RECURSE({csubst = NONE,ksubst = SOME ksubst,sv32subst = sv32subst,sv64subst = sv64subst},con)
		  end)
	    | SOME csubst => 
	     let
	       fun default csubst = 
		 let
(*		   val _ = print ("Restrict case: con is:\n")
		   val _ = pp_con con
		   val _ = print "\n"*)
		   val csubst = restrict(csubst,free_cvars_con con)

		 in 
		   case (is_empty csubst,ksubst)
		     of (true,NONE) => NORECURSE
		      | (true,_ ) => CHANGE_RECURSE({csubst = NONE,ksubst = ksubst,sv32subst = sv32subst,sv64subst = sv64subst},con)
		      | (false,_) => CHANGE_RECURSE({csubst = SOME csubst,ksubst = ksubst,sv32subst = sv32subst,sv64subst = sv64subst},con)
		 end
	     in
	       case cout con
		 of Var_c var => 
		   (case substitute (csubst,var)
		      of SOME con_delay => 
			let 
(*			  val _ = print ("Var subst case:"^(Name.var2string var)^"==")
			  val _ = pp_con (thaw con_delay)
			  val _ = print "\n"*)
			in
			  CHANGE_NORECURSE (state,thaw con_delay)
			end
		       | _ => NORECURSE)
		  | Case_c (arg,arms,def) =>
		      let
			fun reduce arg = 
			  (case cout arg
			     of Inj_c (i,k,argc) => 
			       (case (LO.assoc_eq (TilWord32.equal,i,arms),def) 
				  of (SOME (a,c),_) => 
				    let
(*				      val _ = print "Reducing case: arg is\n"
				      val _ = pp_con argc
				      val _ = print ("\nArm: "^(Name.var2string a)^" == ")
				      val _ = pp_con c
				      val _ = print "\n"*)
				      val state = {csubst = SOME(VarMap.insert(csubst,a,immediate argc)),
						   ksubst = ksubst,sv32subst = sv32subst,sv64subst = sv64subst}
				    (* CHANGE_RECURSE is wrong here! Note that c has not been
				     * passed to the conhandler yet.  So it is wrong to say that
				     * we are done with it: for example, if it is a variable...
				     *)
				    in case conhandler(state,c)
					 of NOCHANGE => CHANGE_RECURSE(state,c)
					  | NORECURSE => CHANGE_NORECURSE(state,c)
					  | other => other
				    end
				   | (_,SOME c) => 
				    (case conhandler(state,c)
				       of NOCHANGE => CHANGE_RECURSE(state,c)
					| NORECURSE => CHANGE_NORECURSE(state,c)
					| other => other)
				   | _ => error "sumcase has no appropriate arm and no default")
			      | _ => default csubst)
		      in
			case (!simplify,cout arg)
			  of (true,Var_c a) => 
			    (case substitute (csubst,a)
			       of SOME con_delay => reduce (thaw con_delay)
				| NONE => default csubst)
			   | _ => default csubst  (* reduce arg is wrong here, bc inj arg has not been rewritten *)
		      end
		  | Pi1_c c => 
		      (case (!simplify,cout c)
			 of (true,Var_c a) => 
			   (case substitute (csubst,a)
			      of SOME con_delay => 
				let
				  val con = thaw con_delay 
				in
				  (case cout con
				     of Pair_c (c1,c2) => CHANGE_NORECURSE(state,c1)
				      | _ => CHANGE_NORECURSE(state,mk_con (Pi1_c con)))
				end
			       | NONE => default csubst)
			  | _ =>  default csubst)

		  | Pi2_c c => 
		      (case (!simplify,cout c)
			 of (true,Var_c a) => 
			   (case substitute (csubst,a)
			      of SOME con_delay => 
				let
				  val con = thaw con_delay 
				in
				  (case cout con
				     of Pair_c (c1,c2) => CHANGE_NORECURSE(state,c2)
				      | _ => CHANGE_NORECURSE(state,mk_con (Pi2_c con)))
				end
			       | NONE => default csubst)
			  | _ =>  default csubst)

		  | _ => default csubst
	     end)


      fun exphandler (state : state as {sv32subst,sv64subst,csubst,...},_ : exp) =
	(case (sv32subst,sv64subst)
	   of (NONE,NONE) => (case csubst of NONE => NORECURSE | _ => NOCHANGE)
	    | _ => NOCHANGE)

      fun sv32handler (state : state as {sv32subst,csubst,...},sv : sv32) =
	(case sv32subst 
	   of NONE => NOCHANGE  (* exphandler already does the short-circuiting *)
	    | SOME sv32subst => 
	     (case sv 
		of Var_32 var => 
		  (case substitute (sv32subst,var)
		     of SOME sv32_delay => 
		       CHANGE_NORECURSE (state,thaw sv32_delay)
		      | _ => NORECURSE)
		 | _ => NOCHANGE))

      fun sv64handler (state : state as {sv64subst,csubst,...},sv : sv64) =
	(case sv64subst 
	   of NONE => NOCHANGE  (* exphandler already does the short-circuiting *)
	    | SOME sv64subst => 
	     (case sv 
		of Var_64 var => 
		  (case substitute (sv64subst,var)
		     of SOME sv64_delay => 
		       CHANGE_NORECURSE (state,thaw sv64_delay)
		      | _ => NORECURSE)
		 | _ => NOCHANGE))


      (* Set the handlers, starting with a default handler
       *)
      val exp_con_kind_handler = 
	let
	  val HANDLER 
	    {
	     bndhandler,
	     conhandler = _,
	     exphandler,
	     sv32handler = _,
	     sv64handler = _,
	     kindhandler = _,
	     kind_var_bind,
	     con_var_bind,
	     exp_var32_bind,
	     exp_var64_bind
	     } : state handler = default_handler
	in
	  HANDLER 
	  {
	   bndhandler = bndhandler,
	   conhandler = conhandler,
	   exphandler = exphandler,
	   sv32handler = sv32handler,
	   sv64handler = sv64handler,
	   kindhandler = kindhandler,
	   kind_var_bind = kind_var_bind,
	   con_var_bind = con_var_bind,
	   exp_var32_bind = exp_var32_bind,
	   exp_var64_bind = exp_var64_bind
	   }
	end

      (* Generate the rewriters.
       *)
      val {rewrite_con  = substSv32Sv64ConKindInCon',
	   rewrite_exp  = substSv32Sv64ConKindInExp',
	   rewrite_sv32 = substSv32Sv64ConKindInSv32',
	   rewrite_sv64 = substSv32Sv64ConKindInSv64',
	   rewrite_op32 = substSv32Sv64ConKindInOp32',
	   rewrite_op64 = substSv32Sv64ConKindInOp64',
	   rewrite_kind = substSv32Sv64ConKindInKind',
           rewrite_bnd  = substSv32Sv64ConKindInBnd',...} = rewriters exp_con_kind_handler
 
      fun empty_state (sv32subst : sv32_subst, sv64subst : sv64_subst, csubst : con_subst,ksubst : kind_subst) : state = 
	let
	  fun chk s = if is_empty s then NONE else SOME s
	in
	  {ksubst = chk ksubst, csubst = chk csubst, sv32subst = chk sv32subst, sv64subst = chk sv64subst}
	end
      (* Given a rewriter, carry out a kind and constructor
       * substitution in an item
       *)
      fun substConKindInXXX substituter (csubst,ksubst) item = 
	let
	  val item =  	
	    if (is_empty ksubst) andalso (is_empty csubst) then item 
	    else substituter (empty_state (empty(),empty(),csubst,ksubst)) item
	in item
	end

      (* Given a rewriter, carry out a kind substitutions
       *)
      fun substKindInXXX substituter ksubst item =
	if (is_empty ksubst) then item
	else substituter (empty_state (empty(),empty(), empty(),ksubst)) item

      (* Given a rewriter, carry out a constructor substitutions
       *)
      fun substConInXXX substituter csubst item =
	if (is_empty csubst) then item
	else substituter (empty_state (empty(), empty(),csubst,empty())) item


    in
      (* Given a rewriter, carry out a term and constructor 
       * substitution in an item
       *)
      fun substSv32Sv64ConInItem substSv32Sv64ConKindInItem (sv32subst,sv64subst,csubst) item = 
	let

	  val item =  	
	    if (is_empty sv32subst) andalso  
	       (is_empty sv64subst) andalso 
	       (is_empty csubst) 
	      then item
	    else substSv32Sv64ConKindInItem (empty_state (sv32subst,sv64subst,csubst,empty())) item
	in item
	end

      val substSv32Sv64ConInExp  = substSv32Sv64ConInItem substSv32Sv64ConKindInExp'
      val substSv32Sv64ConInSv32 = substSv32Sv64ConInItem substSv32Sv64ConKindInSv32'
      val substSv32Sv64ConInSv64 = substSv32Sv64ConInItem substSv32Sv64ConKindInSv64'


      (* Given a rewriter, carry out a term substitution
       *)
      fun substSv32Sv64InExp (sv32subst,sv64subst) exp =
	if (is_empty sv32subst) andalso (is_empty sv64subst) then exp
	else substSv32Sv64ConKindInExp' (empty_state (sv32subst, sv64subst,empty(), empty())) exp

      (* Given a rewriter, carry out a sv32 substitution
       *)
      fun substSv32InSv32 sv32subst sv32 =
	if (is_empty sv32subst) then sv32
	else substSv32Sv64ConKindInSv32' (empty_state (sv32subst, empty(),empty(), empty())) sv32

      (* Given a rewriter, carry out a sv64 substitution
       *)
      fun substSv64InSv64 sv64subst sv64 =
	if (is_empty sv64subst) then sv64
	else substSv32Sv64ConKindInSv64' (empty_state (empty(),sv64subst,empty(), empty())) sv64


      (* Given a rewriter, carry out a sv32 substitution
       *)
      fun substSv32InExp sv32subst exp =
	if (is_empty sv32subst) then exp
	else substSv32Sv64ConKindInExp' (empty_state (sv32subst, empty(),empty(), empty())) exp

      (* Given a rewriter, carry out a sv64 substitution
       *)
      fun substSv64InExp sv64subst exp =
	if (is_empty sv64subst) then exp
	else substSv32Sv64ConKindInExp' (empty_state (empty(),sv64subst,empty(), empty())) exp


      val substConInCon   = fn subst => fn con => 
	let
(*	  val _ = print "Substituting in con: con is\n"
	  val _ = pp_con con
	  val _ = print "\nSubst is:\n"
	  val _ = VarMap.appi (fn (a,c) => (print ((Name.var2string a)^"=");pp_con (thaw c);print "\t")) subst
	  val _ = print "\n"*)
	  val con = substConInXXX substSv32Sv64ConKindInCon' subst con
(*	  val _ = print "New con is\n"
	  val _ = pp_con con
	  val _ = print "\n"*)
	in con
	end
      val substKindInKind = substKindInXXX substSv32Sv64ConKindInKind' 

      val substConInSv32 = substConInXXX substSv32Sv64ConKindInSv32'
      val substConInOp32 = substConInXXX substSv32Sv64ConKindInOp32'
      val substConInOp64 = substConInXXX substSv32Sv64ConKindInOp64'

      val substConInExp   = substConInXXX substSv32Sv64ConKindInExp'
      val substKindInCon  = substKindInXXX substSv32Sv64ConKindInCon'

      val substConKindInCon  = substConKindInXXX(substSv32Sv64ConKindInCon') 

      fun substConInBnd csubst bnd = 
	let
	  val bnd = 
	    if is_empty csubst then bnd
	    else
	      case substSv32Sv64ConKindInBnd' (empty_state (empty(), empty(), csubst, empty())) bnd
		of (state,[bnd]) => bnd
		 | _ => error "substConInBnd" "Substitution should not change number of bnds"
	in
	  bnd
	end

      fun substSv32Sv64InBnd (sv32subst,sv64subst) bnd = 
	let
	  val bnd = 
	    if (is_empty sv32subst) andalso (is_empty sv64subst) then bnd
	    else
	      case substSv32Sv64ConKindInBnd' (empty_state (sv32subst, sv64subst,empty(), empty())) bnd
		of (state,[bnd]) => bnd
		 | _ => error "substExpInBnd" "Substitution should not change number of bnds"
	in
	  bnd
	end

    end  


    (* Here we define the abstract interface for substitutions.  This section 
     * defines the bits that are generic to all levels in a functor parameterized 
     * by the actual substitution functions, which are level specific. 
     *)
    structure Help =
    struct
	structure Delay = Delay
	val error = error
    end

    (* Instantiate the functor for small values*)
    structure SV32 = SubstFn(structure Help = Help
			     type item = sv32
			     type item_subst = sv32_subst
			     val substItemInItem = substSv32InSv32
			     val renameItem = fn sv => sv (*LilRename.renameSv32*)
			     val printer = fn c => error "Can't print substs")

    (* Instantiate the functor for small values*)
    structure SV64 = SubstFn(structure Help = Help
			     type item = sv64
			     type item_subst = sv64_subst
			     val substItemInItem = substSv64InSv64
			     val renameItem = fn sv => sv (*LilRename.renameSv64*)
			     val printer = fn c => error "Can't print substs")
      
    (* Instantiate the functor for constructors*)
    structure C = SubstFn(structure Help = Help
			  type item = con
			  type item_subst = con_subst
			  val substItemInItem = substConInCon
			  val renameItem = fn c => c 
			  val printer = fn c => error "Can't print substs")
      
    (* Instantiate the functor for kinds *)
    structure K = SubstFn(structure Help = Help
			  type item = kind
			  type item_subst = kind_subst
			  val substItemInItem = substKindInKind
			  val renameItem = fn c => c(*LilRename.renameKind*)
			  val printer = fn k => error "Can't print substs")

    (*Substitutions for one variable
     *)
    local 
      fun renameCon (con :con) : con delay = delay (fn () => con (*LilRename.renameCon con*))
      fun renameSv32 (sv :sv32) : sv32 delay = delay (fn () => sv (*LilRename.renameSv32 sv*))
      fun renameSv64 (sv :sv64) : sv64 delay = delay (fn () => sv (*LilRename.renameSv64 sv*))
      fun renameKind (kind :kind) : kind delay = delay (fn () => kind (*LilRename.renameKind kind*))

      fun item_insert (s,var,delay) = VarMap.insert(s,var,delay)
    in


      fun varSv32ExpSubst var sv32 exp   = substSv32InExp  (item_insert(SV32.empty(),var,renameSv32 sv32)) exp
      fun varSv64ExpSubst var sv64 exp   = substSv64InExp  (item_insert(SV64.empty(),var,renameSv64 sv64)) exp

      fun varConSv32Subst var con sv32 = substConInSv32  (item_insert(C.empty(),var,renameCon con)) sv32
      fun varConExpSubst var con exp   = substConInExp  (item_insert(C.empty(),var,renameCon con)) exp
      fun varConConSubst var con con2  = substConInCon  (item_insert(C.empty(),var,renameCon con)) con2

      fun varKindConSubst var kind con = substKindInCon (item_insert(K.empty(),var,renameKind kind))  con
      fun varKindKindSubst var kind1 kind2 = substKindInKind (item_insert(K.empty(),var,renameKind kind1))  kind2
    end

  end
