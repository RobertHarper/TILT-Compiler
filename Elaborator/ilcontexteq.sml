(* Equality of contexts *)

functor IlContextEq (structure IlContext : ILCONTEXT) : ILCONTEXTEQ =
    struct

	nonfix mod

	open IlContext
	open Il

	fun error s = Util.error "IlContextEq" s

	fun foldand f [] = true
	  | foldand f (x::xs) = f x andalso foldand f xs

	(* alpha-conversion is necessary when checking contexts for
	 * equality.  This is done by explicitly maintaining a `var
	 * map' mapping variables to variables.  *)

	type vm = var Name.VarMap.map
	structure VM =
	    struct val empty : vm = Name.VarMap.empty
		   val add : var * var * vm -> vm = fn (v,v',vm) => Name.VarMap.insert(vm,v,v')
		   val lookup : vm -> var -> var = 
		       fn vm => fn v => case Name.VarMap.find(vm,v)
		                          of SOME v => v
					   | NONE => error "VM.lookup"
		   fun eq_var(vm,v,v') = Name.eq_var(lookup vm v, v')
	    end

	exception NOT_EQUAL    (* raised when contexts are not equal *)

	fun extend_vm_context (c : context, c' : context, vm) : vm =
	    let val label_var_pairs = 
		map (fn v => case Context_Lookup'(c,v)
		               of SOME (l,_) => (l, v)
				| NONE => error "extend_vm_context") (Context_Varlist c)
		val _ = if length label_var_pairs <> length (Context_Varlist c') then
		          raise NOT_EQUAL
			else ()
	    in foldr (fn ((l,v), vm) => 
		      case Context_Lookup(c',[l])
			of SOME(SIMPLE_PATH v',_) => VM.add(v,v',vm)
		         | SOME(COMPOUND_PATH (v',_),_) => VM.add(v,v',vm)
			 | NONE => raise NOT_EQUAL) vm label_var_pairs
	    end

	fun sdecs_lookup([],l) = NONE
	  | sdecs_lookup(SDEC(l',dec)::sdecs,l) =
	    if Name.eq_label(l,l') then SOME dec else sdecs_lookup(sdecs,l)

	fun add_dec(DEC_EXP(v,_), DEC_EXP(v',_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_MOD(v,_), DEC_MOD(v',_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_CON(v,_,_), DEC_CON(v',_,_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_EXCEPTION(t,_), DEC_EXCEPTION(t',_), vm) = vm
	  | add_dec _ = raise NOT_EQUAL

	fun extend_vm_sdecs(sdecs,sdecs',vm) : vm =
	    let val _ = if length sdecs <> length sdecs' then raise NOT_EQUAL else ()
	    in foldr (fn (SDEC(l,dec), vm) => case sdecs_lookup(sdecs',l)
		                                of SOME dec' => add_dec(dec,dec',vm)
					         | NONE => raise NOT_EQUAL) vm sdecs
	    end

	fun eq_labels([],[]) = true
	  | eq_labels(lbl::lbls,lbl'::lbls') = 
	    Name.eq_label(lbl,lbl') andalso eq_labels(lbls,lbls')
	  | eq_labels _ = false

	fun eq_vars(vm,[],[]) = true
	  | eq_vars(vm,v::vs,v'::vs') = VM.eq_var(vm,v,v') andalso eq_vars(vm,vs,vs')
	  | eq_vars _ = false

	fun eq_mod (vm,MOD_VAR v,MOD_VAR v') = VM.eq_var(vm,v,v')
	  | eq_mod _ = false

	fun eq_con(vm,con,con') =
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
	       | (CON_MUPROJECT(i,con), CON_MUPROJECT(i',con')) =>
		     i=i' andalso eq_con(vm,con,con')    
	       | (CON_RECORD labcons, CON_RECORD labcons') => eq_labcons(vm,labcons,labcons')        
	       | (CON_FUN(vars,con), CON_FUN(vars',con')) => 
		     eq_vars(vm,vars,vars') andalso eq_con(vm,con,con')
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

	fun eq_path(vm,path,path') =
	    case (path, path')
	      of (SIMPLE_PATH v, SIMPLE_PATH v') => VM.eq_var(vm,v,v')
               | (COMPOUND_PATH(v,lbls), COMPOUND_PATH(v',lbls')) =>
		     VM.eq_var(vm,v,v') andalso eq_labels(lbls,lbls')
	       | _ => false

	fun eq_pathopt(vm,pathopt,pathopt') =
	    case (pathopt, pathopt')
	      of (SOME p, SOME p') => eq_path(vm,p,p')
	       | (NONE, NONE) => true
	       | _ => false   

	fun eq_signat(vm,signat,signat') =
	    case (signat,signat')
	      of (SIGNAT_STRUCTURE(pathopt, sdecs), SIGNAT_STRUCTURE(pathopt', sdecs')) =>
		  eq_pathopt(vm,pathopt,pathopt') andalso eq_sdecs(vm,sdecs,sdecs')
	       | (SIGNAT_FUNCTOR(v,signat1,signat2,a), SIGNAT_FUNCTOR(v',signat1',signat2',a')) =>
		  eq_signat(vm,signat1,signat1') andalso a=a' andalso
		  eq_signat(VM.add(v,v',vm),signat2,signat2')
               | _ => false

	and eq_sdecs(vm,sdecs,sdecs') =
	    let	val vm = extend_vm_sdecs(sdecs,sdecs',vm)
	    in foldand (fn (SDEC(l,dec)) =>
			case sdecs_lookup(sdecs',l)
			  of SOME dec' => eq_dec(vm,dec,dec')
			   | NONE => false) sdecs
	    end	    

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
 
	fun eq_exp (vm,VAR v,VAR v') = VM.eq_var(vm,v,v')
	  | eq_exp _ = false

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

	fun eq_cntxt(vm,c,c') =
	    let val vars = Context_Varlist c
	    in foldand (fn v =>
			case (Context_Lookup'(c,v), Context_Lookup'(c',VM.lookup vm v)) 
			  of (SOME (_, pc), SOME (_, pc')) => eq_pc(vm,pc,pc')
			   | _ => false) vars
	    end	    

	fun eq_context (c: context, c': context) : bool =
	    let val vm = extend_vm_context(c,c',VM.empty)
	    in eq_cntxt(vm,c,c')
	    end handle NOT_EQUAL => false

    end


