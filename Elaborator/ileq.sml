structure IlEq
    :> sig
	   val eq_con : Il.con * Il.con -> bool
       end =
struct

    open Il

    fun error s = Util.error "ileq.sml" s
    val debug = Stats.ff "IlEqDebug"

    local
	(* alpha-conversion is done by explicitly maintaining a `var
	 * map' mapping variables to variables.  *)

	exception NOT_EQUAL

	fun wrap str f arg =
	    if (!debug) then
		let fun msg() = (print str; print " returning false\n")
		    val res = (f arg handle NOT_EQUAL => (msg(); raise NOT_EQUAL))
		    val _ = if res then ()
			    else msg()
		in  res
		end
	    else f arg

	fun wrap' (str,thunk) f arg =
	    if (!debug) then
		let fun msg() = (print str; print " returning false\n"; thunk arg)
		    val res = (f arg handle NOT_EQUAL => (msg(); raise NOT_EQUAL))
		    val _ = if res then ()
			    else msg()
		in  res
		end
	    else f arg
	structure VM :> sig
			    type vm
			    val empty : vm
			    val add : var * var * vm -> vm
			    val lookup: vm -> var -> var
			    val eq_var : vm * var * var -> bool
			end =
	struct
	    type vm = var Name.VarMap.map
	    val empty = Name.VarMap.empty
	    fun add (v,v',vm) = Name.VarMap.insert(vm,v,v')
	    fun lookup vm v =
		(case Name.VarMap.find(vm,v)
		   of SOME v => v
		    | NONE => error ("VM.lookup failed on " ^ (Name.var2string v)))
	    fun eq_var (vm,v,v') =
		(case Name.VarMap.find(vm,v)
		   of SOME v => Name.eq_var(v,v')
		    | NONE => Name.eq_var(v,v'))
	end
	fun sdecs_lookup([],l) = NONE
	  | sdecs_lookup(SDEC(l',dec)::sdecs,l) =
	    if Name.eq_label(l,l') then SOME dec else sdecs_lookup(sdecs,l)

	fun sbnds_lookup([],l) = NONE
	  | sbnds_lookup(SBND(l',bnd)::sbnds,l) =
	    if Name.eq_label(l,l') then SOME bnd else sbnds_lookup(sbnds,l)

	fun add_dec(DEC_EXP(v,_,_,_), DEC_EXP(v',_,_,_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_CON(v,_,_,_), DEC_CON(v',_,_,_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_MOD(v,_,_), DEC_MOD(v',_,_), vm) = VM.add(v,v',vm)
	  | add_dec _ = raise NOT_EQUAL

	fun add_bnd(BND_EXP(v,_), BND_EXP(v',_), vm) = VM.add(v,v',vm)
	  | add_bnd(BND_MOD(v,_,_), BND_MOD(v',_,_), vm) = VM.add(v,v',vm)
	  | add_bnd(BND_CON(v,_), BND_CON(v',_), vm) = VM.add(v,v',vm)
	  | add_bnd _ = raise NOT_EQUAL

	fun extend_vm_sdecs(sdecs,sdecs',vm) : VM.vm =
	    let val _ = if length sdecs <> length sdecs' then raise NOT_EQUAL else ()
	    in foldr (fn (SDEC(l,dec), vm) => case sdecs_lookup(sdecs',l)
		                                of SOME dec' => add_dec(dec,dec',vm)
					         | NONE => let val _ = if (!debug)
									   then (print "XXX label mismatch ";
										 Ppil.pp_label l; print "\n")
								       else ()
							   in  raise NOT_EQUAL
							   end) vm sdecs
	    end

	fun extend_vm_sbnds(sbnds,sbnds',vm) : VM.vm =
	    let val _ = if length sbnds <> length sbnds' then raise NOT_EQUAL else ()
	    in foldr (fn (SBND(l,bnd), vm) => case sbnds_lookup(sbnds',l)
		                                of SOME bnd' => add_bnd(bnd,bnd',vm)
					         | NONE => let val _ = if (!debug)
									   then (print "XXX label mismatch ";
										 Ppil.pp_label l; print "\n")
								       else ()
							   in  raise NOT_EQUAL
							   end) vm sbnds
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

	fun eq_path(vm,PATH(v,lbls), PATH(v',lbls')) = VM.eq_var(vm,v,v') andalso eq_labels(lbls,lbls')

	fun eq_pathopt (vm,pathopt,pathopt') =
	    eq_opt (fn (p1,p2) => eq_path(vm,p1,p2)) (pathopt,pathopt')

	val eq_path = wrap "eq_path" eq_path
	val eq_pathopt = wrap "eq_pathopt" eq_pathopt

	fun eq_mod' (vm,MOD_VAR v,MOD_VAR v') = VM.eq_var(vm,v,v')
	  | eq_mod' (vm,MOD_PROJECT(m1,l1),MOD_PROJECT(m2,l2)) =
	    eq_mod(vm,m1,m2) andalso Name.eq_label(l1,l2)
	  | eq_mod' (vm,MOD_FUNCTOR(a1,v1,s1,m1,s1'),MOD_FUNCTOR(a2,v2,s2,m2,s2')) =
	    a1 = a2 andalso
	    eq_signat (vm,s1,s2) andalso eq_mod(VM.add(v1,v2,vm),m1,m2) andalso
	    eq_signat (vm,s1',s2')
	  | eq_mod' (vm,MOD_STRUCTURE sbnds1,MOD_STRUCTURE sbnds2) = eq_sbnds(vm,sbnds1,sbnds2)
	  | eq_mod' _ = false

	and eq_mod arg = wrap "eq_mod" eq_mod' arg

	and eq_con(vm,con,con') =
	    let val res = case (con,con') of
	         (CON_VAR v, CON_VAR v') => VM.eq_var(vm,v,v')
	       | (CON_TYVAR tv, _) => (case Tyvar.tyvar_deref tv of
					   NONE => error "eq_con on unresolved CON_TYVAR"
					 | SOME c => eq_con(vm, c, con'))
	       | (_, CON_TYVAR tv') => (case Tyvar.tyvar_deref tv' of
					   NONE => error "eq_con on unresolved CON_TYVAR"
					 | SOME c' => eq_con(vm, con, c'))
	       | (CON_OVAR ov, _) => eq_con(vm, CON_TYVAR (Tyvar.ocon_deref ov), con')
	       | (_, CON_OVAR ov') => eq_con(vm, con, CON_TYVAR (Tyvar.ocon_deref ov'))
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
		     eq_cons(vm,cons,cons') andalso eq_con(vm,con,con')
		     andalso b=b'
		     andalso eq_arrow_oneshot(vm,arrow_oneshot,arrow_oneshot')
	       | (CON_APP(con1,cons2), CON_APP(con1',cons2')) =>
		     eq_con(vm,con1,con1') andalso eq_cons(vm,cons2,cons2')
	       | (CON_MU(con), CON_MU(con')) => eq_con(vm,con,con')
	       | (CON_RECORD labcons, CON_RECORD labcons') => eq_labcons(vm,labcons,labcons')
	       | (CON_FUN(vars,con), CON_FUN(vars',con')) =>
		     (case eq_vars(vm,vars,vars') of
			  NONE => false
			| SOME vm => eq_con(vm,con,con'))
	       | (CON_COERCION(vars,c1,c2), CON_COERCION (vars',c1',c2')) =>
		     (case eq_vars(vm,vars,vars') of
			  NONE => false
			| SOME vm => eq_con(vm,c1,c1') andalso eq_con(vm,c2,c2'))
	       | (CON_SUM{names,noncarriers,carrier,special},
		  CON_SUM{names=names',noncarriers=noncarriers',carrier=carrier',special=special'}) =>
		     Listops.eq_list(Name.eq_label,names,names') andalso
		     noncarriers=noncarriers' andalso special=special'
		     andalso eq_con(vm,carrier,carrier')
	       | (CON_TUPLE_INJECT cons, CON_TUPLE_INJECT cons') =>
		     eq_cons(vm,cons,cons')
	       | (CON_TUPLE_PROJECT(i,con), CON_TUPLE_PROJECT(i',con')) =>
		     i=i' andalso eq_con(vm,con,con')
	       | (CON_MODULE_PROJECT(m,lab), CON_MODULE_PROJECT(m',lab')) =>
		     eq_mod(vm,m,m') andalso Name.eq_label(lab,lab')
	       | _ => false
		val _ = if res orelse not (!debug)
			    then ()
			else (print "XXX eq_con false - \ncon = ";
			      Ppil.pp_con con;
			      print "\ncon' = ";
			      Ppil.pp_con con'; print "\n")
	    in res
	    end


	and eq_cons(vm,[],[]) = true
	  | eq_cons(vm,con::cons,con'::cons') = eq_con(vm,con,con') andalso eq_cons(vm,cons,cons')
	  | eq_cons _ = false

	and eq_conopt(vm,SOME con,SOME con') = eq_con(vm,con,con')
	  | eq_conopt(vm,NONE,NONE) = true
	  | eq_conopt _ = false

	and eq_expopt(vm,SOME exp,SOME exp') = eq_exp(vm,exp,exp')
	  | eq_expopt(vm,NONE,NONE) = true
	  | eq_expopt _ = false

	and eq_kind(vm,kind,kind') =
	    case (kind,kind') of
		 (KIND_TUPLE i, KIND_TUPLE i') => i=i'
	       | (KIND_ARROW (i,k), KIND_ARROW (i',k')) => i=i' andalso eq_kind(vm,k, k')
	       | (KIND, KIND) => true
	       | _ => false

	and eq_tyvar(vm,tv,tv') =
	    case (Tyvar.tyvar_deref tv, Tyvar.tyvar_deref tv')
	      of (SOME con, SOME con') => eq_con(vm,con,con')
	       | _ => false

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


	and eq_signat(vm,signat,signat') =
	  let val res =
	    case (signat,signat') of
	         (SIGNAT_STRUCTURE sdecs, SIGNAT_STRUCTURE sdecs') =>
		     eq_sdecs(vm,sdecs,sdecs')
	       | (SIGNAT_FUNCTOR(v,signat1,signat2,a), SIGNAT_FUNCTOR(v',signat1',signat2',a')) =>
		  eq_signat(vm,signat1,signat1') andalso a=a' andalso
		  eq_signat(VM.add(v,v',vm),signat2,signat2')
               | (SIGNAT_VAR v1, SIGNAT_VAR v2) => VM.eq_var(vm,v1,v2)
               | (SIGNAT_OF p1, SIGNAT_OF p2) => eq_path(vm,p1,p2)
               | _ => false
		val _ = if res orelse not (!debug)
			    then ()
			else (print "XXX eq_signat false - \nsignat = ";
			      Ppil.pp_signat signat;
			      print "\nsignat' = ";
			      Ppil.pp_signat signat'; print "\n")
	  in  res
	  end

	and eq_sdecs'(vm,sdecs,sdecs') =
	    let	val vm = extend_vm_sdecs(sdecs,sdecs',vm)
		val res = Listops.andfold
		    (fn (SDEC(l,dec)) =>
		     case sdecs_lookup(sdecs',l) of
			 SOME dec' => eq_dec(vm,dec,dec')
		       | NONE => let val _ = if (!debug)
						 then (print "XXX eq_sdecs' returning false due to ";
						       Ppil.pp_label l; print "\n")
					     else ()
				 in  false
				 end) sdecs
	    in  res
	    end

	and eq_sdecs arg = wrap "eq_sdecs" eq_sdecs' arg

	and eq_sbnds'(vm,sbnds,sbnds') =
	    let	val vm = extend_vm_sbnds(sbnds,sbnds',vm)
	    in Listops.andfold
		(fn (SBND(l,bnd)) =>
			case sbnds_lookup(sbnds',l)
			  of SOME bnd' => eq_bnd(vm,bnd,bnd')
			   | NONE => false) sbnds
	    end

	and eq_sbnds arg = wrap "eq_sbnds" eq_sbnds' arg

	and eq_dec(vm,dec,dec') =
	    let val res = case (dec, dec')
	      of (DEC_EXP(v,con,exp,inline),DEC_EXP(v',con',exp',inline')) =>
		  VM.eq_var(vm,v,v') andalso eq_con(vm,con,con')  andalso
		  eq_expopt(vm,exp,exp') andalso inline=inline'
	       | (DEC_MOD(v,b,signat), DEC_MOD(v',b',signat')) =>
	          VM.eq_var(vm,v,v') andalso (b = b') andalso eq_signat(vm,signat,signat')
               | (DEC_CON(v,kind,conopt,inline), DEC_CON(v',kind',conopt',inline')) =>
		  VM.eq_var(vm,v,v') andalso eq_kind(vm,kind,kind') andalso
		  eq_conopt(vm,conopt,conopt') andalso inline=inline'
	       | _ => false
		val _ = if res orelse not (!debug)
			    then ()
			else (print "XXX eq_dec false - \ndec = ";
			      Ppil.pp_dec dec;
			      print "\ndec' = ";
			      Ppil.pp_dec dec'; print "\n")
	    in res
	    end
	and eq_bnd(vm,bnd,bnd') =
	    case (bnd, bnd')
	      of (BND_EXP(v,exp),BND_EXP(v',exp')) =>
		  VM.eq_var(vm,v,v') andalso eq_exp(vm,exp,exp')
	       | (BND_MOD(v,b,m), BND_MOD(v',b',m')) =>
	          VM.eq_var(vm,v,v') andalso (b = b') andalso eq_mod(vm,m,m')
               | (BND_CON(v,c), BND_CON(v',c')) =>
		  VM.eq_var(vm,v,v') andalso eq_con(vm,c,c')
	       | _ => false

	and eq_exp (vm,VAR v,VAR v') = VM.eq_var(vm,v,v')
	  | eq_exp (vm,MODULE_PROJECT(m1,l1),MODULE_PROJECT(m2,l2)) =
	    eq_mod(vm,m1,m2) andalso Name.eq_label(l1,l2)
	  | eq_exp(vm,INJ{sumtype=c1,field=f1,inject=eopt1},
		   INJ{sumtype=c2,field=f2, inject=eopt2}) =
	       eq_con(vm,c1,c2) andalso f1=f2 andalso
	       eq_opt (fn (e1,e2) => eq_exp(vm,e1,e2)) (eopt1,eopt2)
	  | eq_exp (vm,COERCE(coercion,cs,e),COERCE(coercion',cs',e')) =
	       eq_exp(vm,coercion,coercion') andalso
	       eq_cons(vm,cs,cs') andalso
	       eq_exp(vm,e,e')
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
    in
	val eq_con = fn (con,con') => eq_con(VM.empty,con,con')
    end

end
