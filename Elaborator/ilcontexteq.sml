(* Equality of contexts *)

functor IlContextEq (structure IlContext : ILCONTEXT
		     structure IlUtil : ILUTIL
		     structure Ppil : PPIL
		     sharing Ppil.Il = IlUtil.Il = IlContext.Il) : ILCONTEXTEQ =
    struct

	val debug = ref false
	nonfix mod

	open IlContext
	open Il

	fun error s = Util.error "IlContextEq" s

	fun foldand f [] = true
	  | foldand f (x::xs) = f x andalso foldand f xs

 

	(* alpha-conversion is necessary when checking contexts for
	 * equality.  This is done by explicitly maintaining a `var
	 * map' mapping variables to variables.  *)

	exception NOT_EQUAL    (* raised when contexts are not equal *)

	fun wrap str f arg = let fun msg() = (print str; print " returning false\n")
				 val res = (f arg handle NOT_EQUAL => (msg(); raise NOT_EQUAL))
				 val _ = if res then () 
					 else msg()
			     in  res
			     end
	fun wrap' (str,thunk) f arg =
	    let fun msg() = (print str; print " returning false\n"; thunk arg)
				 val res = (f arg handle NOT_EQUAL => (msg(); raise NOT_EQUAL))
				 val _ = if res then () 
					 else msg()
			     in  res
			     end

	type vm = var Name.VarMap.map
	structure VM =
	    struct val empty : vm = Name.VarMap.empty
		   val add : var * var * vm -> vm = fn (v,v',vm) => Name.VarMap.insert(vm,v,v')
		   val lookup : vm -> var -> var = 
		       fn vm => fn v => case Name.VarMap.find(vm,v) of
		                             SOME v => v
					   | NONE => error ("VM.lookup failed on " ^ (Name.var2string v))
		   fun eq_var(vm,v,v') = 
		       case Name.VarMap.find(vm,v) of
			   SOME v => Name.eq_var(v,v')
			 | NONE => Name.eq_var(v,v')
		   val eq_var = wrap' ("eq_var",fn(_,v,v') =>
				       (Ppil.pp_var v; print ", "; Ppil.pp_var v'; print "\n"))
				       eq_var
	    end


	fun extend_vm_context (c : context, c' : context, vm) : vm * var list =
	    let val vlist = Context_Varlist c
		val vlist' = Context_Varlist c'
		fun mapper ctxt v = (case Context_Lookup'(ctxt,v) of
				    SOME (l,_) => (* is this too conservative? *)
					if (IlUtil.is_exportable_lab l)
					    then SOME(l, v)
					else NONE
				  | NONE => (print "extend_vm_context: could not find var = ";
					     Ppil.pp_var v; print "\n";
					     error "extend_vm_context"))
		val lvlist = List.mapPartial (mapper c) vlist
		val lvlist' = List.mapPartial (mapper c') vlist'

		val _ = if length lvlist <> length lvlist' 
			    then (print "extend_vm_contxt: lvlist length not equal\n";
				  raise NOT_EQUAL)
			else ()
		fun folder ((l,v), vm) =
		      case Context_Lookup(c',[l]) of
			   SOME(SIMPLE_PATH v',_) => VM.add(v,v',vm)
		         | SOME(COMPOUND_PATH (v',_),_) => VM.add(v,v',vm)
			 | NONE => (print "label not found in c'\n"; 
				    raise NOT_EQUAL)
	    in (foldr folder vm lvlist, map #2 lvlist)
	    end

	fun sdecs_lookup([],l) = NONE
	  | sdecs_lookup(SDEC(l',dec)::sdecs,l) =
	    if Name.eq_label(l,l') then SOME dec else sdecs_lookup(sdecs,l)

	fun sbnds_lookup([],l) = NONE
	  | sbnds_lookup(SBND(l',bnd)::sbnds,l) =
	    if Name.eq_label(l,l') then SOME bnd else sbnds_lookup(sbnds,l)

	fun add_dec(DEC_EXP(v,_), DEC_EXP(v',_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_MOD(v,_), DEC_MOD(v',_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_CON(v,_,_), DEC_CON(v',_,_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_EXCEPTION(t,_), DEC_EXCEPTION(t',_), vm) = vm
	  | add_dec _ = raise NOT_EQUAL

	fun add_bnd(BND_EXP(v,_), BND_EXP(v',_), vm) = VM.add(v,v',vm)
	  | add_bnd(BND_MOD(v,_), BND_MOD(v',_), vm) = VM.add(v,v',vm)
	  | add_bnd(BND_CON(v,_), BND_CON(v',_), vm) = VM.add(v,v',vm)
	  | add_bnd _ = raise NOT_EQUAL

	fun extend_vm_sdecs(sdecs,sdecs',vm) : vm =
	    let val _ = if length sdecs <> length sdecs' then raise NOT_EQUAL else ()
	    in foldr (fn (SDEC(l,dec), vm) => case sdecs_lookup(sdecs',l)
		                                of SOME dec' => add_dec(dec,dec',vm)
					         | NONE => raise NOT_EQUAL) vm sdecs
	    end

	fun extend_vm_sbnds(sbnds,sbnds',vm) : vm =
	    let val _ = if length sbnds <> length sbnds' then raise NOT_EQUAL else ()
	    in foldr (fn (SBND(l,bnd), vm) => case sbnds_lookup(sbnds',l)
		                                of SOME bnd' => add_bnd(bnd,bnd',vm)
					         | NONE => raise NOT_EQUAL) vm sbnds
	    end


	fun eq_labels([],[]) = true
	  | eq_labels(lbl::lbls,lbl'::lbls') = 
	    Name.eq_label(lbl,lbl') andalso eq_labels(lbls,lbls')
	  | eq_labels _ = false

	val eq_labels = wrap "eq_labels" eq_labels

	fun eq_vars(vm,v1,v2) = 
	    let fun loop([],[],vm) = SOME vm
		  | loop(v::vs,v'::vs',vm) = loop(vs,vs',VM.add(v,v',vm))
		  | loop _ = NONE
	    in  loop(v1,v2,vm)
	    end

	fun eq_opt equal (opt1,opt2) = 
	    case (opt1,opt2) of
		(NONE,NONE) => true
	      | (SOME c1, SOME c2) => equal(c1,c2)
	      | _ => false

	fun eq_path(vm,path,path') =
	    case (path, path')
	      of (SIMPLE_PATH v, SIMPLE_PATH v') => VM.eq_var(vm,v,v')
               | (COMPOUND_PATH(v,lbls), COMPOUND_PATH(v',lbls')) =>
		     VM.eq_var(vm,v,v') andalso eq_labels(lbls,lbls')
	       | _ => false

	fun eq_pathopt (vm,pathopt,pathopt') = 
	    eq_opt (fn (p1,p2) => eq_path(vm,p1,p2)) (pathopt,pathopt')

	val eq_path = wrap "eq_path" eq_path
	val eq_pathopt = wrap "eq_pathopt" eq_pathopt

	fun eq_mod' (vm,MOD_VAR v,MOD_VAR v') = VM.eq_var(vm,v,v')
	  | eq_mod' (vm,MOD_PROJECT(m1,l1),MOD_PROJECT(m2,l2)) = 
	    eq_mod(vm,m1,m2) andalso Name.eq_label(l1,l2)
	  | eq_mod' (vm,MOD_FUNCTOR(v1,s1,m1),MOD_FUNCTOR(v2,s2,m2)) = 
	    eq_signat (vm,s1,s2) andalso eq_mod(VM.add(v1,v2,vm),m1,m2)
	  | eq_mod' (vm,MOD_STRUCTURE sbnds1,MOD_STRUCTURE sbnds2) = eq_sbnds(vm,sbnds1,sbnds2)
	  | eq_mod' _ = false

	and eq_mod arg = wrap "eq_mod" eq_mod' arg

	and eq_con(vm,con,con') =
	    case (con,con')
	      of (CON_VAR v, CON_VAR v') => VM.eq_var(vm,v,v')           
	       | (CON_TYVAR tv, CON_TYVAR tv') => eq_tyvar(vm,tv,tv')
	       | (CON_OVAR ov, CON_OVAR ov') => eq_ovar(vm,ov,ov')
	       | (CON_FLEXRECORD fr, CON_FLEXRECORD fr') => eq_flexinforef(vm,fr,fr')
	       | (CON_INT intsize, CON_INT intsize') => intsize=intsize'
	       | (CON_UINT intsize, CON_UINT intsize') => intsize=intsize'
	       | (CON_FLOAT floatsize, CON_FLOAT floatsize') => floatsize=floatsize'
	       | (CON_ARRAY con, CON_ARRAY con') => eq_con(vm,con,con')   
	       | (CON_VECTOR con, CON_VECTOR con') => eq_con(vm,con,con')
	       | (CON_ANY, CON_ANY) => true           
	       | (CON_REF con, CON_REF con') => eq_con(vm,con,con')           
	       | (CON_TAG con, CON_TAG con') => eq_con(vm,con,con')           
	       | (CON_ARROW(cons,con,b,arrow_oneshot), CON_ARROW(cons',con',b',arrow_oneshot')) =>
		     eq_cons(vm,cons,cons') andalso eq_con(vm,con,con') andalso b=b'
		     andalso eq_arrow_oneshot(vm,arrow_oneshot,arrow_oneshot')
	       | (CON_APP(con1,con2), CON_APP(con1',con2')) =>
		     eq_con(vm,con1,con1') andalso eq_con(vm,con2,con2')
	       | (CON_MU(con), CON_MU(con')) => eq_con(vm,con,con')    
	       | (CON_RECORD labcons, CON_RECORD labcons') => eq_labcons(vm,labcons,labcons')        
	       | (CON_FUN(vars,con), CON_FUN(vars',con')) => 
		     (case eq_vars(vm,vars,vars') of
			  NONE => false
			| SOME vm => eq_con(vm,con,con'))
	       | (CON_SUM{noncarriers,carriers,special}, 
		  CON_SUM{noncarriers=noncarriers',carriers=carriers',special=special'}) =>
		     noncarriers=noncarriers' andalso special=special' 
		     andalso eq_cons(vm,carriers,carriers')
	       | (CON_TUPLE_INJECT cons, CON_TUPLE_INJECT cons') => 
		     eq_cons(vm,cons,cons')
	       | (CON_TUPLE_PROJECT(i,con), CON_TUPLE_PROJECT(i',con')) => 
		     i=i' andalso eq_con(vm,con,con')
	       | (CON_MODULE_PROJECT(mod,lab), CON_MODULE_PROJECT(mod',lab')) =>
		     eq_mod(vm,mod,mod') andalso Name.eq_label(lab,lab')
	       | _ => false
 

	and eq_cons(vm,[],[]) = true
	  | eq_cons(vm,con::cons,con'::cons') = eq_con(vm,con,con') andalso eq_cons(vm,cons,cons')
	  | eq_cons _ = false

	and eq_conopt(vm,SOME con,SOME con') = eq_con(vm,con,con')
	  | eq_conopt(vm,NONE,NONE) = true
	  | eq_conopt _ = false

	and eq_kind(vm,kind,kind') = 
	    case (kind,kind')
	      of (KIND_TUPLE i, KIND_TUPLE i') => i=i'
	       | (KIND_ARROW p, KIND_ARROW p') => p=p'
	       | (KIND_INLINE(kind,con), KIND_INLINE(kind',con')) => 
		     eq_kind(vm,kind,kind') andalso eq_con(vm,con,con')
	       | _ => false
 
	and eq_tyvar(vm,tv,tv') =
	    case (Tyvar.tyvar_deref tv, Tyvar.tyvar_deref tv')
	      of (SOME con, SOME con') => eq_con(vm,con,con')
	       | _ => false

	and eq_ovar(vm,ov,ov') = eq_tyvar(vm, Tyvar.ocon_deref ov, Tyvar.ocon_deref ov')

	and eq_labcons(vm,[],[]) = true
	  | eq_labcons(vm,(l,con)::labcons,(l',con')::labcons') =
	    Name.eq_label(l,l') andalso eq_con(vm,con,con') 
	    andalso eq_labcons(vm,labcons,labcons')
	  | eq_labcons _ = false

	and eq_flexinforef(vm,fr,fr') =
	    let fun find (ref(FLEXINFO(_,_,labcons))) = labcons
		  | find (ref(INDIRECT_FLEXINFO fr)) = find fr
	    in eq_labcons(vm,find fr, find fr')
	    end

	and eq_arrow_oneshot(vm,aos,aos') =
	    case (Util.oneshot_deref aos, Util.oneshot_deref aos')
	      of (SOME a, SOME a') => a=a'
               | _ => false


	and eq_signat'(vm,signat,signat') =
	    case (signat,signat')
	      of (SIGNAT_STRUCTURE(pathopt, sdecs), SIGNAT_STRUCTURE(pathopt', sdecs')) =>
		  eq_pathopt(vm,pathopt,pathopt') andalso eq_sdecs(vm,sdecs,sdecs')
	       | (SIGNAT_FUNCTOR(v,signat1,signat2,a), SIGNAT_FUNCTOR(v',signat1',signat2',a')) =>
		  eq_signat(vm,signat1,signat1') andalso a=a' andalso
		  eq_signat(VM.add(v,v',vm),signat2,signat2')
	       | (SIGNAT_INLINE_STRUCTURE{self=s1,code=c1,imp_sig=i1,abs_sig=a1},
		  SIGNAT_INLINE_STRUCTURE{self=s2,code=c2,imp_sig=i2,abs_sig=a2}) =>
		  eq_pathopt(vm,s1,s2) andalso
		  eq_sbnds(vm,c1,c2) andalso
		  eq_sdecs(vm,i1,i2) andalso
		  eq_sdecs(vm,a1,a2)
               | _ => false

	and eq_signat arg = wrap "eq_signat" eq_signat' arg

	and eq_sdecs'(vm,sdecs,sdecs') =
	    let	val vm = extend_vm_sdecs(sdecs,sdecs',vm)
	    in foldand (fn (SDEC(l,dec)) =>
			case sdecs_lookup(sdecs',l)
			  of SOME dec' => eq_dec(vm,dec,dec')
			   | NONE => false) sdecs
	    end	    

	and eq_sdecs arg = wrap "eq_sdecs" eq_sdecs' arg

	and eq_sbnds'(vm,sbnds,sbnds') =
	    let	val vm = extend_vm_sbnds(sbnds,sbnds',vm)
	    in foldand (fn (SBND(l,bnd)) =>
			case sbnds_lookup(sbnds',l)
			  of SOME bnd' => eq_bnd(vm,bnd,bnd')
			   | NONE => false) sbnds
	    end	    

	and eq_sbnds arg = wrap "eq_sbnds" eq_sbnds' arg

	and eq_dec(vm,dec,dec') =
	    case (dec, dec')
	      of (DEC_EXP(v,con),DEC_EXP(v',con')) => 
		  VM.eq_var(vm,v,v') andalso eq_con(vm,con,con') 
	       | (DEC_MOD(v,signat), DEC_MOD(v',signat')) => 
	          VM.eq_var(vm,v,v') andalso eq_signat(vm,signat,signat')
               | (DEC_CON(v,kind,conopt), DEC_CON(v',kind',conopt')) =>
		  VM.eq_var(vm,v,v') andalso eq_kind(vm,kind,kind') andalso
		  eq_conopt(vm,conopt,conopt')
	       | (DEC_EXCEPTION(t,con), DEC_EXCEPTION(t',con')) => eq_con(vm,con,con') 
	       | _ => false                        (* MEMO: is this right?? *)
 
	and eq_bnd(vm,bnd,bnd') =
	    case (bnd, bnd')
	      of (BND_EXP(v,exp),BND_EXP(v',exp')) => 
		  VM.eq_var(vm,v,v') andalso eq_exp(vm,exp,exp')
	       | (BND_MOD(v,m), BND_MOD(v',m')) => 
	          VM.eq_var(vm,v,v') andalso eq_mod(vm,m,m')
               | (BND_CON(v,c), BND_CON(v',c')) =>
		  VM.eq_var(vm,v,v') andalso eq_con(vm,c,c')
	       | _ => false                        (* MEMO: is this right?? *)

	and eq_exp (vm,VAR v,VAR v') = VM.eq_var(vm,v,v')
	  | eq_exp (vm,MODULE_PROJECT(m1,l1),MODULE_PROJECT(m2,l2)) = 
	    eq_mod(vm,m1,m2) andalso Name.eq_label(l1,l2)
	  | eq_exp(vm,INJ{noncarriers=nc1,carriers=c1,special=s1,inject=eopt1},
		   INJ{noncarriers=nc2,carriers=c2,special=s2,inject=eopt2}) = 
	       nc1=nc2 andalso s1=s2 andalso 
	       Listops.eq_list(fn (c1,c2) => eq_con(vm,c1,c2), c1,c2) andalso
	       eq_opt (fn (e1,e2) => eq_exp(vm,e1,e2)) (eopt1,eopt2)
	  | eq_exp(vm,ROLL(c1,e1),ROLL(c2,e2)) = eq_con(vm,c1,c2) andalso eq_exp(vm,e1,e2)
	  | eq_exp(vm,UNROLL(c11,c12,e1),UNROLL(c21,c22,e2)) = 
	       eq_con(vm,c11,c21) 
	       andalso eq_con (vm,c12,c22)
	       andalso eq_exp(vm,e1,e2)
	  | eq_exp(vm,FIX(b1,a1,fbnds1),FIX(b2,a2,fbnds2)) = 
	       b1=b2 andalso a1=a2 andalso eq_fbnds(vm,fbnds1,fbnds2)
	  | eq_exp _ = false

	and eq_fbnds(vm,fbnds1,fbnds2) = 
	    let val fbnds1 = map (fn (FBND x) => x) fbnds1
		val fbnds2 = map (fn (FBND x) => x) fbnds2
		fun extend (((v1,_,_,_,_),(v2,_,_,_,_)),vm) = VM.add(v1,v2,vm)
		fun equaler vm ((_,arg1,c1,rc1,e1),(_,arg2,c2,rc2,e2)) = 
		    let val vm = VM.add(arg1,arg2,vm)
		    in  eq_con(vm,c1,c2) andalso eq_con(vm,rc1,rc2) andalso eq_exp(vm,e1,e2)
		    end
	    in  (length fbnds1 = length fbnds2) andalso
		Listops.eq_list (equaler (foldl extend vm (Listops.zip fbnds1 fbnds2)), fbnds1, fbnds2)
	    end

	fun eq_pc(vm, pc, pc') =
	    case (pc, pc')
	      of (PHRASE_CLASS_EXP(exp,con), PHRASE_CLASS_EXP(exp',con')) =>
		  eq_exp(vm,exp,exp') andalso eq_con(vm,con,con')
	       | (PHRASE_CLASS_CON(con,kind), PHRASE_CLASS_CON(con',kind')) =>
		  eq_con(vm,con,con') andalso eq_kind(vm,kind,kind')		  
	       | (PHRASE_CLASS_MOD(mod,signat), PHRASE_CLASS_MOD(mod',signat')) =>
		  eq_mod(vm,mod,mod') andalso eq_signat(vm,signat,signat')
	       | (PHRASE_CLASS_SIG signat, PHRASE_CLASS_SIG signat') =>
		  eq_signat(vm,signat,signat')
	       | _ => false 

	fun eq_cntxt(vm,c,c',vars) =
	    let fun folder v =
		    let fun diag s = 
			if (!debug)
			    then (print s;
				  print "eq_cntxt was processing v = ";
				  Ppil.pp_var v; print "\n";
				  print "context 1 = \n";
				  Ppil.pp_context c;
				  print "\n\ncontext 2 = \n";
				  Ppil.pp_context c';
				  print "\n\n")
			else ()
			val res = 
			    (case (Context_Lookup'(c,v), Context_Lookup'(c',VM.lookup vm v)) of
				 (SOME (_, pc), SOME (_, pc')) => eq_pc(vm,pc,pc')
			       | _ => false)
				handle e => (diag "eq_cntxt caught exception\n";
					     raise e)
		    in  if res then res else (diag "eq_cntxt got false\n"; false)
		    end
		val res = foldand folder vars
		val _ = (print "eq_cntxt returning "; 
			 print (Bool.toString res);
			 print "\n")
	    in  res
	    end	    

	fun eq_context (c: context, c': context) : bool =
	    let 
(*
		val _ = (print "context c = \n";
			 Ppil.pp_context c; 
			 print "\n";
			 print "context c' = \n";
			 Ppil.pp_context c'; 
			 print "\n")
*)
		val (vm,vlist) = extend_vm_context(c,c',VM.empty)
(*
		val _ = print "done extend_vm_context\n"
*)
	    in eq_cntxt(vm,c,c',vlist)
	    end handle NOT_EQUAL => false

    end


