structure LilTypecheck :> LILTYPECHECK = 
  struct
    open Lil 
    open Prim

    structure LO = Listops

    structure LC = LilContext
    structure LS = LilSubst
    structure LD = LilDefs
    structure LU = LilUtil 
    structure Dec = Deconstruct.Dec
    structure Elim = Deconstruct.Elim
    structure Eq = LilTypeEquiv
    structure Synth = Synthesis
    structure R = Reduce
    structure PU = LilPrimUtil
    structure LR = LilRename

    val paranoid = Stats.ff "LilTypecheckParanoid"
    val diag = Stats.ff "LilTypecheckDiag"
    val printargs = Stats.tt "LilTypecheckShowStack"
    val limitargs = Stats.tt "LilTypecheckLimitStack"
    val checkISsynth = Stats.tt "LilTypecheckCheckIsSynth"
    val limit = ref 6

    val chatlev = Stats.int("LilTypecheckChatlev",0)

    fun chatp i = !chatlev >= i

    fun chat i s = if chatp i then print s else ()

    fun msg s = if !diag then print s else ()

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    type env = LilContext.context

    (* Used as an escape continuation to avoid type checking dead vcase 
     * branches.
     * *)
    exception Dead

    local
      datatype arg = E of exp | SV32 of sv32 | OP32 of op32 | C of con | K of kind | PAIR of arg * arg | NO 

      fun pp_arg a = 
	let
	  fun print1 s pp item = 
	    (print ("\t"^s^" is:\n");
	     pp item;
	     print "\n\n")
	in
	  case a 
	    of E e => print1 "Exp" PpLil.pp_exp e
	     | SV32 sv => print1 "Sv32" PpLil.pp_sv32 sv
	     | OP32 eop => print1 "Op32" PpLil.pp_op32 eop
	     | C c => print1 "Con" PpLil.pp_con c
	     | K k => print1 "Kind" PpLil.pp_kind k
	     | PAIR (a1,a2) => (pp_arg a1;pp_arg a2)
	     | NO => ()
	end
      val stack : (string * arg) list ref = ref []
      fun push s = stack := s :: (!stack)
      fun error s = (push s;NONE)

    in
      exception IllTyped

      fun FAIL' s = (push s;raise IllTyped) 
      fun FAIL s = FAIL' (s,NO)
      fun FAILc s c = FAIL' (s,C c)
      fun FAILk s k = FAIL' (s,K k)
      fun FAILck s c k = FAIL' (s,PAIR (C c,K k))
      fun FAILv32 s sv = FAIL' (s,SV32 sv)
      fun FAILv32t s sv t = FAIL' (s,PAIR (SV32 sv,C t))
      fun FAILe s e = FAIL'(s,E e)
      fun FAILet s e t = FAIL'(s,PAIR(E e,C t))
      fun FAILcc s c1 c2 = FAIL'(s,PAIR(C c1,C c2))
      fun FAILkk s k1 k2 = FAIL'(s,PAIR(K k1,K k2))
      fun FAILo32 s eop = FAIL' (s,OP32 eop)

      fun reset () = stack := []
      fun errors () = 
	let
	  fun printstring (s,arg) = print ("\tERROR: "^s^"\n")
	  fun printall (s,arg) = 
	    let
	      val () = printstring (s,arg)
	      val () = pp_arg arg
	    in () 
	    end
	  val () = 
	    if !printargs then 
	      if !limitargs andalso (!limit < List.length (!stack)) then
		let
		  val extra = List.length (!stack) - !limit
		  val base = List.take (!stack,extra)
		  val top  = List.drop (!stack,extra)
		  val () = printall (hd base)
		  val () = app printstring (tl base)
		  val () = app printall top
		in ()
		end
	      else app printall (!stack)
	    else app printstring (!stack)
	in ()
	end
    end

    fun ANDALSO (b1,b2) = fn s => (b1 s;b2 s;())
    fun ASSERT b s = if b then () else (FAIL s)
    fun ASSERTc b s c = if b then () else (FAILc s c)
    infix 0 ANDALSO
      
    fun DECT f t s = 
      (case f t
	 of SOME a => a
	  | NONE => FAILc s t)

    fun VALOF opt s = 
      (case opt
	 of SOME a => a
	  | NONE => FAIL s)
	 
    fun ISSOME opt s = 
      (case opt
	 of SOME a => ()
	  | NONE => FAIL s)

    fun APP f l s = List.app (fn i => f i s) l

    fun ALLEQ eq l s = 
      let
	fun loop l = 
	  (case l
	     of [] => ()
	      | [a] => ()
	      | a::b::rest => if eq a b then loop (b::rest) else FAIL s)
      in loop l
      end

    fun EQUALTYPES c1 c2 s = 
      if Eq.C.equal c1 c2 then () else FAILcc s c1 c2
    fun EQUALKINDS k1 k2 s = 
      if Eq.K.equal k1 k2 then () else FAILkk s k1 k2

    fun bind_kvar args   = ((LC.bind_kvar args)   handle LC.Rebound s => FAIL ("Kind variable rebound "^s))
    fun bind_cvar args   = ((LC.bind_cvar args)   handle LC.Rebound s => FAIL ("Con variable rebound "^s))
    fun bind_cvars args  = ((LC.bind_cvars args)  handle LC.Rebound s => FAIL ("Con variables rebound "^s))
    fun bind_var32 args  = ((LC.bind_var32 args)  handle LC.Rebound s => FAIL ("Sv32 variable rebound "^s))
    fun bind_var32s args = ((LC.bind_var32s args) handle LC.Rebound s => FAIL ("Sv32 variables rebound "^s))
    fun bind_var64 args  = ((LC.bind_var64 args)  handle LC.Rebound s => FAIL ("Sv64 variable rebound "^s))
    fun bind_var64s args = ((LC.bind_var64s args) handle LC.Rebound s => FAIL ("Sv64 variables rebound "^s))
    fun bind_label args  = ((LC.bind_label args)  handle LC.Rebound s => FAIL ("Label rebound "^s))
    fun bind_labels args = ((LC.bind_labels args) handle LC.Rebound s => FAIL ("Label rebound "^s))

    fun find_kvar args  = ((LC.find_kvar args)  handle LC.Unbound s => FAIL ("Kind variable unbound "^s))
    fun find_cvar args  = ((LC.find_cvar args)  handle LC.Unbound s => FAIL ("Con variable unbound "^s))
    fun find_var32 args = ((LC.find_var32 args) handle LC.Unbound s => FAIL ("Sv32 variable unbound "^s))
    fun find_var64 args = ((LC.find_var64 args) handle LC.Unbound s => FAIL ("Sv64 variable unbound "^s))
    fun find_label args = ((LC.find_label args) handle LC.Unbound s => FAIL ("Label unbound "^s))

    fun split_cvar args = ((LC.split_cvar args) 
			   handle LC.Unbound s => FAIL ("Split of unbound variable "^s)
				| LC.Rebound s => FAIL ("Split rebinds variable "^s))
    fun unfold_cvar args = ((LC.unfold_cvar args) 
			    handle LC.Unbound s => FAIL ("Unfold of unbound variable "^s)
				 | LC.Rebound s => FAIL ("Unfold rebinds variable "^s))

    fun vcase_cvar args = ((LC.vcase_cvar args) 
			   handle LC.Unbound s => FAIL ("Vcase of unbound variable "^s)
				| LC.Rebound s => FAIL ("Vcase rebinds variable "^s))
    val clear_vars = LC.clear_vars 


    structure K = 
      struct
	type env = {env : env, p : bool}
	fun flip ({env,p} : env) : env = {env = env, p = not p}
	fun parity p = if p then LC.Pos else LC.Neg
	fun neg p = (case p
		       of LC.Pos => LC.Neg
			| LC.Neg => LC.Pos
			| _ => p)
	local
	  val checked : env KindMap.map ref = ref KindMap.empty
	    
	  fun context_eq_vars({env = env1,p = pos1},{env = env2,p = pos2},vars) = 
	    let
	      val samepos = pos1 = pos2
	      val vars = Name.VarSet.listItems vars
	      fun agree a = 
		let val p1 = find_kvar (env1,a) and p2 = find_kvar (env2,a)
		in
		  (p2 = LC.Any) orelse
		  if samepos then p1 = p2
		  else (neg p1) = p2
		end
	    in List.all agree vars
	    end
	in
	  
	  fun is_ok env k = 
	    (case KindMap.find(!checked,k)
	       of SOME oldenv => context_eq_vars (env,oldenv,free_kvars_kind k) 
		| NONE => false)
	  fun reset_checked () = checked := KindMap.empty
	  fun set_checked (c,env) = checked := (KindMap.insert(!checked,c,env))
	end

	fun mu_var_bind ({env,p},j) = {env = bind_kvar (env,j,parity p),p=p}
	fun all_var_bind ({env,p},j) = {env = bind_kvar (env,j,LC.Any),p=p}

	fun check env k s = 
	  if is_ok env k then ()
	  else
	    let
	      val () = ((check' env k) handle IllTyped => FAILk s k)
	      val () = set_checked (k,env)
	    in ()
	    end

	and check' env (k : Lil.kind) = 
	  (case kout k
	     of T s => ()
	      | Tmem => ()
	      | Unit_k => ()
	      | Nat_k => ()
	      | Arrow_k (k1,k2) => (check (flip env) k1 ANDALSO check env k2) "Ill formed arrow kind"
	      | Prod_k (k1,k2)  => (check env k1 ANDALSO check env k2) "Ill formed product kind"
	      | Sum_k ks => APP (check env) ks "Ill formed sum kind"
	      | Var_k j => 
	       (case (find_kvar (#env env,j),#p env)
		  of (LC.Any,_) => ()
		   | (LC.Pos,true) => ()
		   | (LC.Neg,false) => ()
		   | (LC.Pos,_) => FAIL "Variable has positive parity in a negative position"
		   | (LC.Neg,_) => FAIL "Variable has negative parity in a positive position")
	      | Mu_k (j,k) => 
		 let
		   val env = mu_var_bind (env,j)
		 in check env k "Ill formed Mu kind"
		 end
	      | All_k (j,k) =>
		 let
		   val env = all_var_bind (env,j)
		 in check env k "Ill formed All kind"
		 end)

	val check = fn env => fn k => fn s => check {env=env,p=true} k s
      end


    structure C = 
      struct


	local
	  val checked : (env * kind) ConMap.map ref = ref ConMap.empty
	    
	  fun context_eq_vars(c1,c2,vars) = 
	    let
	      val vars = Name.VarSet.listItems vars
	      fun agree a = Eq.K.equal (find_cvar (c1,a)) (find_cvar (c2,a))
	    in List.all agree vars
	    end
	  
	  fun context_covers_vars(env,vars) = 
	    let
	      val () = Name.VarSet.app (fn j => ignore(find_kvar(env,j))) vars
	    in true
	    end
	in
	  
	  fun is_ok env c = 
	    (case ConMap.find(!checked,c)
	       of SOME (oldenv,k) => 
		 if context_eq_vars (env,oldenv,free_cvars_con c) andalso context_covers_vars (env,free_kvars_con c)
		   then SOME k else NONE
		| NONE => NONE)
	  fun reset_checked () = checked := ConMap.empty
	  fun set_checked (c,envk) = checked := (ConMap.insert(!checked,c,envk))
	end
      
	type env = env

	fun psynth p = Synth.Kindof.primcon p	  

	and synth env c s   = ((synth' env c) handle IllTyped => FAILc s c)
	and check env c k s = ((check' env c k) handle IllTyped => FAILck s c k)

	and check' env c k = 
	  let
	    val () = 
	      if !paranoid then 
		let 
		  val () = K.check env (LR.renameKind k)  "C.check' got bad kind" 
		in ()
		end
	      else ()
	  in
	    case is_ok env c
	      of SOME k' => EQUALKINDS k' k "Synthesized kind and assigned kind differ"
	       | NONE => 
		if !checkISsynth then
		  let
		    val k' = synth env c "check': bad con"
		    val () = EQUALKINDS k' k "Synthesized kind and assigned kind differ"
		    val () = set_checked (c,(env,k))
		  in ()
		  end
		else
		  let
		    val () = check'' env c k
		    val () = set_checked (c,(env,k))
		  in ()
		  end
	  end
	and check'' env (c : Lil.con) (k : Lil.kind) = 
	  let
	    val () = 
	      (case cout c of
		 Var_c a => EQUALKINDS (find_cvar (env,a)) k "Variable has wrong kind"
	       | Nat_c w => EQUALKINDS (LD.K.nat()) k "Nat_c: wrong kind"
	       | App_c (c1,c2) => 
		  let
		    val argk = synth env c2 "App_c: bad arg"
		    val arrk = LD.K.arrow argk k
		  in check env c1 arrk "App_c: bad arg"
		  end
	       | APP_c (c1,argk) =>
		  let
		    val k1 = synth env c1 "APP_c: bad function"
		    val () = K.check env argk "APP_c: bad arg" 
		    val k' = Elim.K.APP k1 argk
		  in EQUALKINDS k k' "APP_c: synthesized kind differs"
		  end
	       | Pi1_c c1 => 
		  let
		    val pairk = synth env c1 "Pi1 from bad tuple"
		    val k1 = VALOF (Elim.K.pi1' pairk) "Pi1 from non-tuple"
		  in EQUALKINDS k1 k "Pi1_c: kinds differ"
		  end
	       | Pi2_c c2 =>
		  let
		    val pairk = synth env c2 "Pi2 from bad tuple"
		    val k2 = VALOF (Elim.K.pi2' pairk) "Pi2 from non-tuple"
		  in EQUALKINDS k2 k "Pi2_c: kinds differ"
		  end
	       | Prim_c p => EQUALKINDS (psynth p) k "Prim_c: kinds differ"
	       | Pr_c (j,(a,uk),k',r,body)  => 
		   let
		     val mu = LD.K.mu (j,uk)
		     val () = K.check env mu "Pr_c has bad from kind"
		     val env = bind_kvar(env,j,LC.Pos)
		     val () = K.check env k' "Pr_c has bad to kind"
		     val env = bind_cvar(env,(a,uk))
		     val env = bind_cvar(env,(r,LD.K.arrow (LD.K.var j) k'))
		     val () = check env body k' "Pr_c body is bad or can't be given kind"
		     val resk = LD.K.arrow mu (LS.varKindKindSubst j mu k')
		   in EQUALKINDS resk k "Pr_c: kinds differ"
		   end
	       | Case_c (argc,arms,def) => 
		   let
		     val sumk = synth env argc "Case_c arg is bad"
		     val ks = VALOF (Dec.K.sum' sumk) "Case_c: arg kind not a sum"
		     val arms = LO.insertion_sort (fn (a,b) => LU.cmpw32(#1 a,#1 b)) arms
		     (* remaining arms, remaining kinds, current index, complete coverage *)
		     fun checkarms (arms,ks,iw,complete) = 
		       (case (arms,ks)
			  of ([],_) => 
			    (case def
			       of SOME c => check env c k "Case_c: Bad default"
				| NONE => ASSERT ((null ks) andalso complete) "Case_c: Coverage incomplete")
			   | ((w,(a,c))::restarms,ki::ks) => 
			       if w > iw then checkarms (arms,ks,iw + 0w1,false)
			       else
				 let
				   val () = ASSERT (w = iw) "Case_c: duplicate arm"
				   val () = check (bind_cvar(env,(a,ki))) c k "Case_c has bad arm"
				 in checkarms(restarms,ks,iw+0w1,complete)
				 end
			   | _ => FAIL "too many arms!")
		     val () = checkarms (arms,ks,0w0,true)
		   in ()
		   end
	       | LAM_c (j,c) => 
		   let
		     val (j',k') = VALOF (Dec.K.forall' k) "LAM_c: not a forall kind"
		     val k' = LS.varKindKindSubst j' (mk_kind (Var_k j)) k'
		     val env = bind_kvar (env,j,LC.Any)
		   in check env c k' "LAM_c has bad body"
		   end
	       | Lam_c ((a,deck),c) =>
		   let
		     val (argk,resk) = VALOF (Dec.K.arrow' k) "Lam_c: not an arrow kind"
		     val () = EQUALKINDS argk deck "Lam_c: arg kinds disagree"
		     val env = bind_cvar (env,(a,deck))
		   in check env c resk "Lam_c has bad body"
		   end
	       | Pair_c (c1,c2) => 
		   let
		     val (k1,k2) = VALOF (Dec.K.pair' k) "Pair_c: not a pair kind"
		     val () = check env c1 k1 "Pair_c has bad left component"
		     val () = check env c2 k2 "Pair_c has bad right component"
		   in ()
		   end
	       | Star_c => EQUALKINDS (LD.K.unit()) k "Star_c: kinds differ"
	       | Inj_c (w,k',c)  => 
		   let
		     val () = K.check env k' "Inj_c has bad kind"
		     val () = EQUALKINDS k k' "Inj_c: kinds differ"
		     val kw = VALOF (LD.KOps.sumw w k) "Inj_c has non/bad-sum kind decoration"
		   in check env c kw "Inj_c has bad con"
		   end
	       | Fold_c (k',c) => 
		   let
		     val () = K.check env k' "Fold has bad kind"
		     val () = EQUALKINDS k k' "Fold_c: kinds differ"
		     val ku = VALOF (Elim.K.unfold' k) "Fold has non-mu decoration"
		   in check env c ku "Fold has bad con"
		   end
	       | Ptr_c c => 
		   let
		     val () = check env c (LD.K.TM()) "Ptr_c: bad arg"
		   in EQUALKINDS k (LD.K.T32()) "Ptr_c: kinds differ"
		   end)
	  in ()
	  end

	and synth' env (c : Lil.con) = 
	  case is_ok env c
	    of SOME k' => k'
	     | NONE => 
	  let
	    val k = 
	      (case cout c of
		 Var_c a => find_cvar (env,a)
	       | Nat_c w => LD.K.nat()
	       | App_c (c1,c2) => 
		   let
		     val k = synth env c1 "Constructor application of bad function"
		       
		     val (k1,k2) = VALOF (Dec.K.arrow' k) "Application of constructor of non-arrow kind"
		       
		     val () = check env c2 k1 "Constructor application got bad arg"
		       
		   in k2
		   end
	       | APP_c (c1,k) =>
		   let
		     val k1 = synth env c1 "Constructor Kapp of bad function"
		       
		     val () = ISSOME (Dec.K.forall' k1) "Application of constructor of non-forall kind"
		       
		     val () = K.check env k "Constructor application got bad arg"
		   in Elim.K.APP k1 k
		   end
	       | Pi1_c c1 => VALOF (Elim.K.pi1' (synth env c1 "Pi1 from bad tuple")) "Pi1 from non-tuple"
	       | Pi2_c c2 => VALOF (Elim.K.pi2' (synth env c2 "Pi2 from bad tuple")) "Pi2 from non-tuple"
	       | Prim_c p => psynth p
	       | Pr_c (j,(a,k),k',r,body)  => 
		   let
		     val mu = LD.K.mu (j,k)
		     val () = K.check env mu "Pr_c has bad from kind"
		     val env = bind_kvar(env,j,LC.Pos)
		     val () = K.check env k' "Pr_c has bad to kind"
		     val env = bind_cvar(env,(a,k))
		     val env = bind_cvar(env,(r,LD.K.arrow (LD.K.var j) k'))
		     val () = check env body k' "Pr_c body is bad or can't be given kind"
		     val resk = LD.K.arrow mu (LS.varKindKindSubst j mu k')
		   in resk
		   end
	       | Case_c (argc,arms,def) => 
		   let
		     val sumk = synth env argc "Case_c arg is bad"
		     val ks = VALOF (Dec.K.sum' sumk) "Case_c: arg kind not a sum"
		     val arms = LO.insertion_sort (fn (a,b) => LU.cmpw32(#1 a,#1 b)) arms

		     fun optcheck env c kopt s =
		       (case kopt
			  of SOME k => (check env c k s;k)
			   | NONE => synth env c s)

		     fun doarms (arms,ks,kopt,iw,complete) = 
		       (case (arms,ks)
			  of ([],_) => 
			    (case def
			       of SOME c => optcheck env c kopt "Case_c: Bad default"
				| NONE => 
				 let val () = ASSERT (null ks andalso complete) "Case_c: Coverage incomplete"
				 in VALOF kopt "Case_c: No arms!!"
				 end)
			   | ((w,(a,c))::restarms,ki::ks) => 
			       if iw < w then doarms (arms,ks,kopt,iw + 0w1,false)
			       else
				 let
				   val k = optcheck (bind_cvar(env,(a,ki))) c kopt "Case_c has bad arm"
				 in doarms(restarms,ks,SOME k,iw + 0w1,complete)
				 end
			   | _ => FAIL "too many arms!")
		     val k = doarms (arms,ks,NONE,0w0,true)
		   in k
		   end
	       | LAM_c (j,c) => 
		   let
		     val env = bind_kvar (env,j,LC.Any)
		     val k = synth env c "LAM_c has bad body"
		     val retk = LD.K.forall (j,k)
		   in retk
		   end
	       | Lam_c ((a,k),c) =>
		   let
		     val () = K.check env k "Lam_c has bad formal kind"
		     val env = bind_cvar (env,(a,k))
		     val retk = synth env c "Lam_c has bad body"
		   in LD.K.arrow k retk
		   end
	       | Pair_c (c1,c2) => 
		   let
		     val k1 = synth env c1 "Pair_c has bad left component"
		     val k2 = synth env c2 "Pair_c has bad right component"
		   in LD.K.pair (k1,k2)
		   end
	       | Star_c => LD.K.unit()
	       | Inj_c (w,k,c)  => 
		   let
		     val () = K.check env k "Inj_c has bad kind"
		     val kw = VALOF (LD.KOps.sumw w k) "Inj_c has non/bad-sum kind decoration"
		     val () = check env c kw "Inj_c has bad con"
		   in k
		   end
	       | Fold_c (k,c) => 
		   let
		     val () = K.check env k "Fold has bad kind"
		     val k' = VALOF (Elim.K.unfold' k) "Fold has non-mu decoration"
		     val () = check env c k' "Fold has bad con"
		   in k
		   end
	       | Ptr_c c => 
		   let
		     val () = check env c (LD.K.TM()) "Ptr_c: bad arg"
		   in LD.K.T32()
		   end)

	    val () = set_checked (c,(env,k))

	    val () = if !paranoid then K.check env (LR.renameKind k) "C.synth' returning bad kind" else ()
	  in k
	  end
      end

    structure T = 
      struct


	fun discriminable (tagcount : Lil.w32) (sum_args : Lil.con list) : bool = 
	  (case (tagcount,sum_args)
	     of (_,[]) => true
	      | (0w0,[_]) => true
	      | (_,[c]) => isSome (Dec.C.ptr' c)
	      | (_, args) => 
	       let
		 fun tagged c = 
		   (case Dec.C.tuple_ptr_ml' c
		      of SOME(t::_) => isSome (Dec.C.tag' t)
		       | _ => false)
	       in
		 List.all tagged args
	       end)

	fun check32 (env : LC.context) (t : Lil.con) (s : string) = ((C.check' env t (LD.K.T32())) handle IllTyped => FAILc s t)
	fun check64 (env : LC.context) (t : Lil.con) (s : string) = ((C.check' env t (LD.K.T64())) handle IllTyped => FAILc s t)
	fun checklist (env : LC.context) (ts : Lil.con list) (s : string) = app (fn t => check32 env t s) ts
      end

    structure E = 
      struct
	type env = LC.context

	fun check synth s env args t s' = 
	  let
	    val t' = synth env args s'
	  in EQUALTYPES t' t (s^": synthesized type and assigned type not equal")
	  end 

	fun checklist checker env args types s = 
	  ((LO.app2 (fn (a,t) => checker env a t s) (args,types)) 
	   handle (e as UtilError.BUG _) => FAIL (UtilError.errormsg e))

	fun checkopt checker env arg t s =
	  (case arg
	     of SOME a => ignore (checker env a t s)
	      | NONE => ())

	fun synthlist synther env args s = 
	  LO.map (fn a => synther env a s) args
	  
	fun coercion env q s = 
	  let 
	    val t = ((coercion' env q) handle IllTyped => FAIL s)
	    val () = if !paranoid then T.check32 env (LR.renameCon t) "coercion' returned bad type" else ()
	  in t 
	  end
	and primarg env arg s = 
	  let 
	    val t = ((primarg' env arg) handle IllTyped => FAIL s)
	    val () = if !paranoid then 
	      (case arg 
		 of slice (sz,_) => T.check32 env (LR.renameCon (LD.T.embed sz t)) "primarg' returned bad type"
		  | arg32 _ => T.check32 env (LR.renameCon t) "primarg' returned bad type" 
		  | arg64 _ => T.check64 env (LR.renameCon t) "primarg' returned bad type")
		     else ()
	  in t 
	  end
	and primargcheck env arg t s = check primarg "primarg" env arg t s
	and primarg_union env arg s = 
	  let 
	    val t = ((primarg_union' env arg) handle IllTyped => FAIL s)
	  in t 
	  end
	and primarg_unioncheck env arg t s = check primarg_union "primarg_union" env arg t s
	and value size env v s = ((value' size env v) handle IllTyped => FAIL s)
	and valuecheck size env v t s = check (value size) "value" env v t s
	and sv64 env sv s = 
	  let 
	    val t = ((sv64' env sv) handle IllTyped => FAIL s)
	    val () = if !paranoid then T.check64 env (LR.renameCon t) "sv64' returned bad type" else ()
	  in t 
	  end
	and sv64check env arg t s = check sv64 "sv64" env arg t s
	and sv32 env sv s = 
	  let 
	    val t = ((sv32' env sv) handle IllTyped => FAILv32 s sv)
	    val () = if !paranoid then T.check32 env (LR.renameCon t) "sv32' returned bad type" else ()
	  in t 
	  end
	and sv32check env arg t s = (check sv32 "sv32" env arg t s) handle IllTyped => FAILv32 "sv32check error" arg
	and op64 env fop s = 
	  let 
	    val t = ((op64' env fop) handle IllTyped => FAIL s)
	    val () = if !paranoid then T.check64 env (LR.renameCon t) "op64' returned bad type" else ()
	  in t 
	  end
	and op64check env arg t s = check op64 "op64" env arg t s
	and lilprimop32 env arg s = 
	  let 
	    val t = ((lilprimop32' env arg) handle IllTyped => FAIL s)
	    val () = if !paranoid then T.check32 env (LR.renameCon t) "lilprimop32' returned bad type" else ()
	  in t 
	  end
	and op32 env eop s = 
	  let 
	    val t = ((op32' env eop) handle IllTyped => FAILo32 s eop)
	    val () = if !paranoid then T.check32 env (LR.renameCon t) "op32' returned bad type" else ()
	  in t 
	  end
	and op32check env arg t s = check op32 "op32" env arg t s

	and cccheck env arg s = ((cccheck' env arg) handle IllTyped => FAIL s)

	and exp env e s = 
	  let 
	    val () = ((exp' env e) handle IllTyped => FAILe s e)
	  in ()
	  end
	and expcheck env arg t s = ((expcheck' env arg t) handle IllTyped => FAILet s arg t)

	and coercion' env ((q,args) : coercion) = 
	  let
	    val () = app (fn t => T.check32 env t "Coercion arg is ill-formed") args
	  in
	    case (q,args)
	      of (Roll, [to])   => 
		let
		  val from = VALOF (Elim.C.unroll' to) "Roll: Can't unroll recursive type."
		in LD.T.coercion from to
		end
	       | (Unroll, [from]) => 
		let
		  val to = VALOF (Elim.C.unroll' from) "Unroll: Can't unroll recursive type"
		in LD.T.coercion from to
		end
	       | (Pack, [to,hiding])  =>
		let
		  val (_,lam) = VALOF (Dec.C.exists' to) "Pack: not an existential type"
		  val from = LD.C.app lam hiding
		in  LD.T.coercion from to
		end
	       | (ForgetKnown,[from]) => 
		let
		  val (which,tagcount,carriers) = VALOF (Dec.C.ksum' from) "Forgetknown: type is not a ksum"
		  val to = LD.T.sum tagcount carriers
		in  LD.T.coercion from to
		end
	       | (ProjKnown,[from]) => 
		let
		  val (which,tagcount,carriers) = VALOF (Dec.C.ksum_ml' from) "Forgetknown: type is not a ksum"
		  val n = TilWord32.uminus(which,tagcount)
		  val to = VALOF(LU.wnth' n carriers) "Proj: not enough carriers"
		in LD.T.coercion from to
		end
	       | (InjUnion, [to])   => 
		let
		  val (which,tagcount,carriers) = VALOF (Dec.C.ksum_ml' to) "InjUnion: to is not a sum type"
		  val from = if which < tagcount then LD.T.tag which
			     else (List.nth (carriers,TilWord32.toInt (which - tagcount)) 
				   handle _ => FAIL "InjUnion: sum arm out of range")
		in LD.T.coercion from to
		end
	       | (InjForget, [ksum]) =>
		let
		  val (which,tagcount,carriers) = VALOF (Dec.C.ksum_ml' ksum) "InjForget: type is not a ksum"
		  val from = if which < tagcount then LD.T.tag which
			     else (List.nth (carriers,TilWord32.toInt (which - tagcount)) 
				   handle _ => FAIL "InjForget: sum arm out of range")
		  val to = LD.T.sum' tagcount carriers
		in LD.T.coercion from to
		end
	    | _ => FAIL "Wrong args"
	  end
	and primarg' env arg = 
	  (case arg
	     of slice (sz,sv) => 
	       let
		 val t = sv32 env sv "slice: bad sv"
		 val (sz',c) = VALOF (Dec.C.embed' t) "slice: not an embedded type"
		 val _ = ASSERT (sz = sz') "slice: bad sizes"
	       in c
	       end
	      | arg32 sv => sv32 env sv "primarg32: bad sv"
	      | arg64 sv => sv64 env sv "primarg64: bad sv")
	and primarg_union' env arg = 
	  (case arg
	     of slice (sz,sv) => 
               let 
                 val t = sv32 env sv "slice: bad sv" 
                 val (sz',c) = VALOF (Dec.C.embed' t) "slice: not an embedded type" 
                 val _ = ASSERT (sz = sz') "slice: bad sizes" 
               in LD.C.inl (LD.K.T32or64()) t
	       end
	      | arg32 sv => LD.C.inl (LD.K.T32or64()) (sv32 env sv "primarg32: bad sv")
	      | arg64 sv => LD.C.inr (LD.K.T32or64()) (sv64 env sv "primarg64: bad sv"))
	and value' size env arg  =
	  (case size
	     of B1 => 
	       (case arg
		  of (Prim.int (Prim.W8, w))  => LD.T.intt B1
		   | (Prim.uint (Prim.W8, w)) => LD.T.intt B1
		   | _ => FAIL "value got bad prim value")
	      | B2 => FAIL "no supported 2 byte values"
	      | B4 => 
		  (case arg
		     of (Prim.int (Prim.W8, w))  => LD.T.embed B1 (LD.T.intt B1)
		      | (Prim.int (Prim.W32, w)) => LD.T.intt B4
		      | (Prim.uint (Prim.W8, w)) => LD.T.embed B1 (LD.T.intt B1)
		      | (Prim.uint (Prim.W32, w)) => LD.T.intt B4
		      | (Prim.intvector (Prim.W8,arr))    => 
		       let
			 val _ = Array.app (fn arg => primargcheck (LC.empty()) arg (LD.T.intt B1) "vector has bad elt") arr
		       in LD.T.ptr (LD.T.array B4 (LD.T.intt B1))
		       end
		      | _ => FAIL "bad 4 byte prim value")
	      | B8 => 
		   (case arg
		      of Prim.float (floatsize, f) => LD.T.float ()
		       | _ => FAIL "bad 8 byte prim value"))

	and sv64' env sv = 
	  (case sv
	     of Var_64 xf => find_var64 (env,xf)
	      | Const_64 v => value B8 env v "Const_64: bad constant")
	and sv32' env sv =
	  (case sv
	     of Var_32 x => find_var32(env,x)
	      | Label l => find_label(env,l)
	      | Coercion q => coercion env q "Coercion: badness"
	      | Coerce (q,v) => 
	       let
		 val qtype = sv32 env q "Coerce: Coercion has bad type"
		 val vtype = sv32 env v "Coerce: arg has bad type"
		 val (from,to) = VALOF (Dec.C.coercion' qtype) "Coerce: Coercion not of coercion type"
		 val () = EQUALTYPES from vtype "Coerce: Coercion from type and arg type differ"
	       in to
	       end
	      | Tabs ((a,k),sv) => 
	       let
		 val () = K.check env k "Tabs: kind is bad"
		 val env = bind_cvar (env,(a,k))
		 val t = sv32 env sv "Tabs: body has bad type"
	       in LD.T.forall (a,k) t
	       end
	      | TApp _ => 
	       let
		 val (sv,cons) = Dec.E.nary_tapp sv
		 val ftype = sv32 env sv "Tapp: bad applicand"
		 fun loop (svtype,cons,subst) = 
		   (case cons
		      of [] => LS.substConInCon subst svtype
		       | c::cons => 
			let
			  val ((a,k),svtype) = VALOF (Dec.C.forall_ml' svtype) "TApp: not a forall"
			  val () = C.check env c k "TApp: bad con"
			  val subst = LS.C.sim_add subst (a,c)
			in loop (svtype,cons,subst)
			end)
	       in loop (ftype,cons,LS.C.empty())
	       end
	       
(*	      | TApp (sv, c) => 
	       let
		 val () = chat 4 "\tworking on TApp\n"
		 val ftype = sv32 env sv "TApp: bad app"
		 val () = chat 4 "\t deconstructing forall\n"
		 val (k,lam) = VALOF (Dec.C.forall' ftype) "TApp: app not a forall"
		 val () = chat 4 "\t checking arg\n"
		 val () = C.check env c k "TApp: bad con"
		 val () = chat 4 "\t done with TApp\n"
	       in LD.C.app lam c
	       end*)
	      | Tag w32    => 
	       let
		 val limit = TilWord32.fromInt 256
		 val () = ASSERT (TilWord32.ulte(w32,limit)) "Tags must be small (<= 256)"
	       in LD.T.tag w32 
	       end
	      | Unit => LD.T.unit()
	      | Const_32 v => value B4 env v "Const_32: bad constant")
	and op64' env fop = 
	  (case fop
	     of Val_64 sv => sv64 env sv "Val_64: bad sv"
	      | Unbox sv => 
	       let
		 val t32   = sv32 env sv "Unbox: bad arg"
		 val bxdt  = VALOF (Dec.C.ptr' t32) "Unbox: val has non-ptr type"
		 val (s,t) = VALOF (Dec.C.boxed' bxdt) "Unbox: val has non-boxed type"
		 val () = case s of B8 => () | _ => FAIL "Unbox: bad size"
	       in t
	       end
	      | Prim64 (p,pargs) => 
	       let 
		 val (total,arg_types,rtype) = PU.get_type () p []
		 val () = checklist primargcheck env pargs arg_types "Prim64 got bad argument"
		 val () = T.check64 env rtype "Prim64 has bad return type"
	       in rtype
	       end	      
	      | ExternAppf (f,args) => 
	       let
		 val ft = sv32 env f "ExternAppf: bad fun"
		 val (size,ts,rtype) = VALOF (Dec.C.externarrow' ft) "ExternAppf: type not Extern_c"
		 val () = ASSERT (size = B8) "ExternAppf: not a 64 bit return type"
		 val (_,ts) = VALOF (Dec.C.list' ts) "ExternAppf: Extern arg not a list"
		 val () = checklist primarg_unioncheck env args ts "ExternAppf got bad 32bit argument"
	       in rtype
	       end
)

	and lilprimop32' env (pop32,cons,sv32s,sv64s) =
	  (case (pop32,cons,sv32s,sv64s)
	     of (Box, [],[],[sv])   => 
	       let
		 val t = sv64 env sv "Box: bad arg"
	       in LD.T.ptr (LD.T.boxed B8 t)
	       end
	      | (Tuple, [], svs,[]) => 
	       let
		 val ts = synthlist sv32 env svs "Tuple: bad field"
	       in LD.T.tupleptr' ts
	       end
	      | (Select w32,[],[sv],[]) => 
	       let
		 val t  = sv32 env sv "Select: bad arg"
		 val tp = DECT Dec.C.ptr' t "Select: arg not of ptr type"
		 val ts = VALOF (Dec.C.tuple' tp) "Select: arg not of tuple type"
		 val t = VALOF (Dec.C.nth' (LU.w2i w32) ts) "Select: bad tuple type arg (too short?)"
	       in t
	       end
	      | (Dyntag,[c],[],[])      => 
	       let
		 val () = T.check32 env c "Dyntag: bad type"
	       in LD.T.dyntag c
	       end
	      | (Ptreq,[],[sv1,sv2],[])      => 
	       let
		 val t1 = sv32 env sv1 "Ptreq: bad arg 1"
		 val t2 = sv32 env sv2 "Ptreq: bad arg 2"
		 val t1 = DECT Dec.C.ptr' t1 "Ptreq: arg 1 not of ptr type"
		 val t2 = DECT Dec.C.ptr' t2 "Ptreq: arg 2 not of ptr type"
		 val () = EQUALTYPES t1 t2 "Ptreq: args have different type"
	       in LD.T.bool()
	       end

	      | _ => FAIL "Wrong number of args to lilprim")
	and op32' env eop = 
	  (case eop
	     of Val sv => sv32 env sv "Val: bad arg"
	      | Prim32 (p,cs,primargs) => 
	       let 
		 val () = T.checklist env cs "Prim32: bad type args"
		 val (total,arg_types,rtype) = PU.get_type () p cs
		 val () = checklist primargcheck env primargs arg_types "Prim32 got bad argument"
		 val () = T.check32 env rtype "Prim32 has bad return type"
	       in rtype
	       end
	      | PrimEmbed (sz,p,primargs) => 
	       let 
		 val (total,arg_types,rtype) = PU.get_type () p []
		 val () = checklist primargcheck env primargs arg_types "PrimEmbed got bad argument"
		 val rtype = LD.T.embed sz rtype
		 val () = T.check32 env rtype "PrimEmbed has bad return type"
	       in rtype
	       end
	      | LilPrimOp32 args => lilprimop32 env args "LilPrimOp32: bad prim"
	      | ExternApp (f,args) => 
	       let
		 val ft = sv32 env f "ExternApp: bad fun"
		 val (size,ts,rtype) = VALOF (Dec.C.externarrow' ft) "ExternApp: type not Extern_c"
		 val () = ASSERT (size = B4) "ExternApp: bad function return type"
		 val (_,ts) = VALOF (Dec.C.list' ts) "ExternApp: Extern arg not a list"
		 val () = checklist primarg_unioncheck env args ts "ExternApp got bad argument"
	       in rtype
	       end
	      | App (f,vs,fvs) =>
	       let
		 val () = chat 6 "Working on App\n"
		 val ft = sv32 env f "App: bad fun"
		 val () = chat 6 "Deconstructing arrow type\n"
		 val (t32s,t64s,rtype) = VALOF (Dec.C.arrow_ml' ft) "App: fun of non-arrow type"
		 val () = chat 6 "Checking T32s\n"
		 val () = checklist sv32check env vs t32s "App got bad 32bit arg"
		 val () = chat 6 "Checking T64s\n"
		 val () = checklist sv64check env fvs t64s "App got bad 64bit arg"
		 val () = chat 6 "Done with App\n"
	       in rtype
	       end
	      | Call (f,vs,fvs) =>
	       let
		 val ft = sv32 env f "Call: bad fun"
		 val (t32s,t64s,rtype) = VALOF (Dec.C.code_ml' ft) "Call: fun of non-code type"
		 val () = checklist sv32check env vs t32s "Call got bad 32bit arg"
		 val () = checklist sv64check env fvs t64s "Call got bad 64bit arg"
	       in rtype
	       end

	      | Switch sw => switch env sw 
	      | Raise (c,sv) => 
	       let
		 val () = sv32check env sv (LD.T.exn()) "Raise: bad exn"
		 val () = T.check32 env c "Raise: bad type"
	       in c
	       end
	      | Handle {t,e,h = {b,he}}=> 
	       let
		 val () = expcheck env e t "Handle: bad body"
		 val env = bind_var32 (env,(b,LD.T.exn()))
		 val () = expcheck env he t "Handle: bad handler"
	       in t
	       end)

        and expcheck' env e t = 
	  (case #e e
	     of Val32_e sv => sv32check env sv t "Val32: bad sv"
	      | Let_e p => ((pexpcheck env p t) 
			    handle IllTyped => FAILet "Let_e check: badness" e t
				 | Dead => ()))

	and exp' env e =  
	  (case #e e
	     of Val32_e sv => ignore(sv32 env sv "Val32: bad sv")
	      | Let_e p => ((pexp env p) 
			    handle IllTyped => FAILe "Let_e: badness" e
				 | Dead => ()))
	and pexpcheck env (bnds,e) t = 
	  let
	    fun loop ([],env,subst) = expcheck env (LS.substConInExp subst e) (LS.substConInCon subst t) "Letcheck: bad body"
	      | loop (b::bnds,env,subst) = 
	      let
(*		val _ = print "Bnd is:\n"
		val _ = PpLil.pp_bnd b*)
		val b = LS.substConInBnd subst b
(*		val _ = print "\nNew Bnd is:\n"
		val _ = PpLil.pp_bnd b*)
		val (env,subst') = bnd env b
(*		val _ = print "\nSubst add is:\n"
		val _ = LS.C.printf (PpLil.pp_con) subst'*)
		val subst = LS.C.compose (subst',subst)
	      in loop (bnds,env,subst)
	      end
	  in loop (bnds,env,LS.C.empty())
	  end
	and pexp env (bnds,e) = 
	  let
	    fun loop ([],env,subst) = exp env (LS.substConInExp subst e) "Let: bad body"
	      | loop (b::bnds,env,subst) = 
	      let
		val b = LS.substConInBnd subst b
		val (env,subst') = bnd env b
		val subst = LS.C.compose (subst',subst)
	      in loop (bnds,env,subst)
	      end
	  in loop (bnds,env,LS.C.empty())
	  end

	and bnd env b = 
	  (case b 
	     of Fixcode_b vfs    => (bind_functions (env,vfs),LS.C.empty())
	      | Exp32_b b  => (bind_op32 (env,b),LS.C.empty())
	      | Exp64_b b  => (bind_op64 (env,b),LS.C.empty())
	      | Unpack_b b => (bind_unpack (env,b),LS.C.empty())
	      | Split_b b  => bind_split (env,b)
	      | Inj_b b    => bind_inj (env,b)
	      | Unfold_b b => bind_unfold (env,b))
        and checkvks env vks s = 
	  let
	    val () = LO.app_second (fn k => K.check env k s) vks
	    val env = bind_cvars (env,vks)
	  in env
	  end
        and checklvks env lvks s = 
	  let
	    val vks = map (fn (l,v,k) => (v,k)) lvks
	    val () = LO.app_second (fn k => K.check env k s) vks
	    val env = bind_cvars (env,vks)
	  in env
	  end
        and checkvt32s env vts s = 
	  let
	    val () = LO.app_second (fn t => T.check32 env t s) vts
	    val env = bind_var32s (env,vts)
	  in env
	  end
        and checkvt64s env vts s = 
	  let
	    val () = LO.app_second (fn t => T.check64 env t s) vts
	    val env = bind_var64s (env,vts)
	  in env
	  end
        and checklts env lts s =
	  let
	    val () = LO.app_second (fn t => T.check32 env t s) lts
	    val env = bind_labels (env,lts)
	  in env
	  end

	and function env (f,Function {tFormals    : (var * kind) list,
				      eFormals    : (var * con) list,
				      fFormals    : (var * con) list,
				      rtype       : con,
				      body        : exp}) =
	  let
	    fun msg s = "Function "^(Name.var2string f)^": "^s

	    val () = chat 3 (msg "Working on tFormals\n")
	    val env = checkvks env tFormals (msg " bad tFormal")
	    val () = chat 3 (msg "Working on eFormals\n")
	    val env = checkvt32s env eFormals (msg " bad eFormal")
	    val () = chat 3 (msg "Working on fFormals\n")
	    val env = checkvt64s env fFormals (msg " bad fFormal")
	    val () = chat 3 (msg "Working on return type\n")
	    val () = T.check32 env rtype (msg " bad return type")
	    val () = chat 3 (msg "Working on body\n")
	    val () = expcheck env body rtype (msg " bad body")
	    val () = chat 3 (msg "Finished\n")
	  in ()
	  end

	and switch env sw = 
	  (case sw 
	     of Sumcase {arg : sv32,arms :(w32  * var * exp) list, default: exp option, rtype : con} => 
	       let
		 val () = T.check32 env rtype "Sumcase: bad return type"
		 val t = sv32 env arg "Sumcase: bad arg"
		 val (tagcount,sum_args) = VALOF (Dec.C.sum_ml' t) "sum_totalcount: not a sum"
		 val () = ASSERTc (T.discriminable tagcount sum_args) "Sumcase: type is not discriminable" t

		 val len = LU.i2w(List.length sum_args)
		 val n = TilWord32.uplus (tagcount,len)
		 fun armcheck (w,v,exp) = 
		   let
		     val () = ASSERT (TilWord32.ult(w,n)) "Sumcase: tag out of range"
		     val env = bind_var32 (env,(v,LD.COps.sum2ksum' w t))
		     val () = expcheck env exp rtype "Sumcase: bad arm body"
		   in ()
		   end
		 val () = app armcheck arms
		 val () = checkopt expcheck env default rtype "Sumcase: bad default"
	       in rtype
	       end
	      | Dyncase {arg : sv32,arms :(sv32 * (var * con) * exp) list, default: exp,        rtype : con} => 
	       let
		 val () = T.check32 env rtype "Dyncase: bad return type"
		 val t = sv32 env arg  "Dyncase: bad arg"
		 val (dyntag,c,s) = DECT Dec.C.exn_packet' t "Dyncase: not an exn packet"
		 val () = EQUALTYPES (LD.T.dyntag c) dyntag "Dyncase: exn packet fields are wrong"
		 val () = EQUALTYPES (LD.T.stringt()) s "Dyncase: name field not a string"
		 fun armcheck (sv,(v,c),exp) = 
		   let
		     val () = T.check32 env c "Dyncase: bad arm formal type"
		     val (d,elt,_) = DECT Dec.C.exn_packet' c "Dyncase: arm arg not of exn packet type"
		     val () = sv32check env sv d "Dyncase: bad arm exntag"
		     val () = EQUALTYPES (LD.T.dyntag elt) d "Dyncase: exn stamp packet fields are wrong"
		     val env = bind_var32 (env,(v,c)) 
		     val () = expcheck env exp rtype "Dyncase: bad arm body"
		   in ()
		   end
		 val () = app armcheck arms
		 val () = expcheck env default rtype "Dyncase: bad default"
	       in rtype
	       end
	      | Intcase {arg : sv32,arms :(w32 * exp) list,size : size,   default: exp,        rtype : con} => 
	       let
		 val () = T.check32 env rtype "Intcase: bad return type"
		 val () = sv32check env arg (LD.COps.mkT32 size (LD.T.intt size)) "Intcase: bad arg"
		 fun armcheck (w,exp) = 
		   let
		     val () = expcheck env exp rtype "Intcase: bad arm body"
		   in ()
		   end
		 val () = app armcheck arms
		 val () = expcheck env default rtype "Intcase: bad default"
	       in rtype
	       end
	   | Ifthenelse {arg : conditionCode, thenArm : exp,elseArm : exp,rtype : con} =>
	       let
		 val () = T.check32 env rtype "Ifthenelse: bad rtype"
		 val () = cccheck env arg "Ifthenelse: bad conditionCode"
		 val () = expcheck env thenArm rtype "Ifthenelse: bad thenArm"
		 val () = expcheck env elseArm rtype "Ifthenelse: bad elseArm"
	       in rtype
	       end)
        and cccheck' env cc = 
	  (case cc
	     of Exp_cc e => expcheck env e (LD.T.bool()) "Exp_cc: bad exp"
	      | And_cc (cc1,cc2) => (cccheck env cc1 "And_cc: bad left cc";
				     cccheck env cc2 "And_cc: bad right cc")
	      | Or_cc (cc1,cc2) => (cccheck env cc1 "And_cc: bad left cc";
				    cccheck env cc2 "And_cc: bad right cc")
	      | Not_cc cc => cccheck env cc "NOT_cc: bad cc")
	and bind_functions (env,vfs) = 
	  let
	    val () = case LU.break_fix vfs
		      of [_] => ()
		       | _ => FAIL "FIX is not strongly connected"

	    val vts = LO.map_second Synth.Typeof.function vfs
	    (* Technically, this context may now be ill-formed,
	     * since we have not checked the types of the functions.
	     * however, all of these types will now be checked,
	     * so we should eventually catch any problems.
	     *)
	    val env = bind_var32s (env,vts)
	    val () = app (function env) vfs
	  in env
	  end
	and bind_op32 (env,(x,eop))  = bind_var32 (env,(x,op32 env eop "Exp32_b: bad op"))
	and bind_op64 (env,(xf,fop)) = bind_var64 (env,(xf,op64 env fop "Exp64_b: bad op"))
	and bind_unpack (env,(a,x,sv)) = 
	  let
	    val t = sv32 env sv "Unpack_b: bad sv"
	    val (k,lam) = VALOF (Dec.C.exists' t) "Unpack_b: not an existential"
	    val env = bind_cvar(env,(a,k))
	    val t = LD.C.app lam (LD.C.var a)
	    val env = bind_var32(env,(x,t))
	  in env
	  end
	and bind_split (env,(b,g,c)) =
	  let
	    val k = C.synth env c "Split_b: bad arg"
	    val () = ISSOME (Dec.K.pair' k) "Split_b: Not of pair kind"
	  in
	    (case cout (Reduce.whnf c)
	       of Var_c a => split_cvar (env,(a,(b,g)))
		| Pair_c (c1,c2) => 
		 let
		   val subst = LS.C.empty()
		   val subst = LS.C.sim_add subst (b,c1)
		   val subst = LS.C.sim_add subst (g,c2)
		 in (env,subst)
		 end
		(* Either an error, or else need a non-refining case *)
		| _ => FAILc "Split on path!" c)
	  end
	and bind_inj (env,(w,b,c,sv)) = 
	  let
	    val sumk = C.synth env c "Let inj: bad argument"
	    val ks = VALOF (Dec.K.sum' sumk) "Let inj: arg not a sum"
	    val c = R.whnf c   (*XXX eta?*)
	    val es = 
	      (case cout c
		 of Var_c a => 
		   let
		     val () = chat 4 "\tchecking letinj: calling vcase_var\n"
		     val envs = vcase_cvar (env,(a,b))
		     fun doarm (i,(env,subst)) = 
		       let
			 val iw = LU.i2w i
		       in
			 if iw = w then ()
			 else
			   let
			     val sv = LS.substConInSv32 subst sv
			   in sv32check env sv (LD.T.void()) "Let inj: bad dead arm"
			   end
		       end
		     val () = chat 4 "\tchecking arms\n"
		     val _ = LO.mapcount doarm envs
		     val () = chat 4 "\tdone checking arms\n"
		     val es = VALOF (LU.wnth' w envs) "Let inj: index too large"
		   in es
		   end
		  | Inj_c (w',_,c1) => 
		   let
		     val subst = LS.C.empty()
		     val subst = LS.C.sim_add subst (b,c1)
		   in
		     if w = w' then (env,subst)
		     else
		       let
			 val sv = LS.substConInSv32 subst sv
			 val () = sv32check env sv (LD.T.void()) "Let inj: bad dead arm in triv"
		       in raise Dead
		       end
		   end
		  | _ => FAILc "Let inj: got path?" c)
	  in es
	  end		  
	and bind_unfold (env,(b,c)) =
	  let
	    val k = C.synth env c "Unfold_b: bad arg"
	    val () = ISSOME (Dec.K.mu' k) "Unfold_b: Not of mu kind"
	  in
	    case cout (Reduce.whnf c)
	      of Var_c a => unfold_cvar (env,(a,b))
	       | Fold_c (k,c) => 
		let
		  val subst = LS.C.empty()
		  val subst = LS.C.sim_add subst (b,c)
		in (env,subst)
		end
	       (* Either an error, or else need a non-refining case *)
	       | _ => FAILc "Unfold on path not handled" c
	  end


       fun coercions env qlist tend = 
	 (case qlist
	    of [] => tend
	     | q::qlist => 
	      let
		val qtype = coercion env q "coercions: Coercion has bad type"
		val (from,to) = VALOF (Dec.C.coercion' qtype) "coercions: Coercion not of coercion type"
		val () = EQUALTYPES to tend "coercions: Coercion from type and arg type differ"
	      in coercions env qlist from
	      end)
      end
    structure D = 
      struct
	fun check env d = 
	  (case d
	     of Dboxed (l,sv64) => E.sv64check env sv64 (LD.T.float()) "Dboxed: bad box"
	      | Dtuple (l,t,qs,svs) => 
	       let
		 val () = T.check32 env t "Dtuple: bad type"
		 val tupt = E.coercions env qs t 
		 val t = VALOF (Dec.C.ptr' tupt) "Dtuple: type not a ptr"
		 val ts = VALOF (Dec.C.tuple_ml' t) "Dtuple: type not a tuple"
		 val () = LO.app2 (fn (sv,t) => E.sv32check env sv t "Dtuple: bad field") (svs,ts)
	       in ()
	       end
	      | Darray (l,sz,t,svs) => 
	       let
		 val () = C.check env t (LD.K.Type sz) "Darray: bad type"
		 val () = app (fn v => E.valuecheck sz env v t "Darray: bad elt") svs
	       in ()
	       end
	
	      | Dcode (l,f) => 
	       let
		 val fvar = Name.fresh_named_var (Name.label2name l)
	       in E.function env (fvar,f)
	       end)

	fun add_dtype (d,env) = 
	  (case d
	     of Dboxed (l,sv64) => bind_label(env,(l,LD.T.ptr (LD.T.boxed_float())))
	      | Darray (l,sz,t,svs) => bind_label (env,(l,LD.T.ptr (LD.T.array sz t)))
	      | Dtuple (l,t,qs,svs) => bind_label(env,(l,t))
	      | Dcode (l,f) => bind_label(env,(l,Synth.Typeof.code f)))

	fun datalist dlist env s = 
	  (let
	     val env = foldl add_dtype env dlist
	     val () = app (check env) dlist
	   in env
	   end handle IllTyped => FAIL s)
      end

    fun wrap f = 
      let
	val () = reset()
	val res = (f ()) handle any => (errors();reset();raise any)
	val () = reset()
      in res
      end

    fun wrap1 f a = wrap (fn () => f a)
    fun wrap2 f a b = wrap (fn () => f a b)
    fun wrap3 f a b c = wrap (fn () => f a b c)

    structure K = 
      struct
	val reset = K.reset_checked
	val check = fn env => fn k => K.check env k "KIND CHECK FAILED"
	val check = wrap2 check
      end

    structure C = 
      struct
	val reset = C.reset_checked
	val check = fn env => fn c => fn k => C.check env c k "CON CHECK FAILED"
	val synth = fn env => fn c => C.synth env c "CON SYNTH FAILED"
	val check = wrap3 check
	val synth = wrap2 synth
      end
    structure T = 
      struct
	val check32 = fn env => fn t => T.check32 env t "TYPE CHECK FAILED"
	val check64 = fn env => fn t => T.check64 env t "TYPE64 CHECK FAILED"
	val check32 = wrap2 check32
	val check64 = wrap2 check64
      end

    structure E = 
      struct
	val check = fn env => fn e => fn t => E.expcheck env e t "EXP CHECK FAILED"
	val checkvks = fn env => fn vks => E.checkvks env vks "VKS LIST CHECK FAILED"
	val checklvks = fn env => fn lvks => E.checklvks env lvks "LVKS LIST CHECK FAILED"
	val checklts = fn env => fn lts => E.checklts env lts "LTS LIST CHECK FAILED"
	val synth = fn env => fn e => E.exp env e "EXP SYNTH FAILED"
	val check = wrap3 check
	val synth = wrap2 synth

      end

    structure D = 
      struct
	val check = fn env => fn d => D.datalist env d "BAD DATA SEGMENT"
	val check = wrap2 check
      end

    fun reset_checked () = (K.reset();C.reset())

    structure M = 
      struct


	fun check (MODULE {unitname : string,
			   parms : Name.LabelSet.set,
			   entry_c : label * var * kind,
			   entry_r : label * con,
			   timports : (label * var * kind) list,
			   vimports : (label * con) list,
			   data   : data list,
			   confun : con}) = 
	  let
	    val () = reset_checked()
	    val () = msg "\tValidating type imports\n"
	    val env = E.checklvks (LC.empty()) timports
	    val () = msg "\tValidating term imports\n"
	    val env = E.checklts env vimports
	    val () = msg "\tValidating data\n"
	    val env  = D.check data env
	    val () = msg "\tValidating confun\n"
	    val () = if !paranoid then ASSERT (LR.isRenamedCon confun) "CONFUN fails renaming check" else ()
	    val confun_k  = C.synth env confun

	    val () = msg "\tValidating entry_c\n"
	    val (entry_c_l,entry_c_a,entry_c_k) = entry_c
	    val ()  = K.check env entry_c_k

	    val () = msg "\tValidating entry_r\n"
	    val env = bind_cvar (env,(entry_c_a,entry_c_k))
	    val (entry_r_l,entry_r_t) = entry_r
	    val ()  = T.check32 env entry_r_t

	    val () = msg "\tChecking interface conformance \n"
	    val () = EQUALKINDS entry_c_k confun_k "entry_c kind does not match confun"
	    val entry_r_t = LS.varConConSubst entry_c_a confun entry_r_t
	    val expfun_t = 
	      (case LU.get_data_entry data entry_r_l
		 of Dcode (l,f) => Synth.Typeof.code f
		  | _ => FAIL "Bad entry_r_l")

	    val () = EQUALTYPES expfun_t entry_r_t "entry_r type doesn't match expfun"


	    val () = msg "\tDone validating module\n"
	    val () = reset_checked()
	  in ()
	  end handle any => (reset_checked();raise any)
	val check = wrap1 check
      end

    structure I = 
      struct
	fun check (INTERFACE {unitname : string,
			      timports : timport list,
			      entry_c = (entry_cl,entry_cv,entry_c_kind) : label * var * kind,
			      entry_r = (entry_rl,entry_r_con) : label * con}) = 
	  let
	    val () = reset_checked()
	    val () = msg "\tValidating type imports\n"
	    val env = E.checklvks (LC.empty()) timports
	    val () = msg "\tValidating confun kind\n"
	    val _  = K.check env entry_c_kind
	    val env = bind_cvar (env,(entry_cv,entry_c_kind))
	    val () = msg "\tValidating term fun type\n"
	    val _  = C.check env entry_r_con (LD.K.T32())
	    val () = msg "\tDone validating interface\n"
	    val () = reset_checked()
	  in ()
	  end handle any => (reset_checked();raise any)
	val check = wrap1 check
      end

  end