(* Static semantics *)
functor IlStatic(structure Il : IL
		 structure IlLookup : ILLOOKUP 
		 structure PrimUtil : PRIMUTIL
		 structure Ppil : PPIL
		 structure IlUtil : ILUTIL
		 sharing Ppil.Il = IlUtil.Il = IlLookup.Il = Il
		 sharing PrimUtil.Prim = Il.Prim
		 sharing type PrimUtil.con = Il.con
		 sharing type PrimUtil.exp = Il.exp)
  : ILSTATIC = 
  struct

    open Util Listops
    structure Il = Il
    open Il Ppil IlUtil
    open IlLookup  
    open Prim Tyvar Name

    val error = fn s => error "ilstatic.sml" s
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()
    fun fail s = raise (FAILURE s)

   (* ------- decs extension ------------------------------------------- *)
   fun add_dec_mod (m,(v,s)) = (DEC_MOD(v,s)) :: m
   fun add_dec_exp (m,(v,c)) = (DEC_EXP(v,c)) :: m
   fun add_dec_con (m,(v,k,c)) = (DEC_CON(v,k,SOME c)) :: m
   fun add_dec_exps (decs,[]) = decs
     | add_dec_exps (decs,(a::rest)) = add_dec_exps(add_dec_exp(decs,a),rest)
   fun add_dec_cons (decs,[]) = decs
     | add_dec_cons (decs,(a::rest)) = add_dec_cons(add_dec_con(decs,a),rest)
   fun add_dec_dec (decs,dec) = dec::decs

   (* ------- decs and signat lookup ------------------------------------- *)
   local
     fun help s match fetch ([]) = raise (NOTFOUND s)
       | help s match fetch (a::rest) = if (match a) then fetch a 
					else help s match fetch rest
   in
     fun DecVarLookup (var, decs) = 
	 help ("Var " ^ var2string var)
	 (fn (DEC_EXP (v,_)) => eq_var(var,v) | _ => false)
	 (fn (DEC_EXP (_,con)) => con | _ => error "DecVarLookup") decs
     fun DecExnLookup (tag, decs) = 
	 help ("Exn " ^ tag2string tag)
	 (fn (DEC_EXCEPTION (n,_)) => eq_tag(tag,n) | _ => false)
	 (fn (DEC_EXCEPTION (_,con)) => con 
           | _ => error "DecExnLookup") decs
     fun DecConvarLookup (var, decs) = 
	 help ("Convar " ^ var2string var)
	 (fn (DEC_CON (v,_,SOME _)) => eq_var(var,v) | _ => false)
	 (fn (DEC_CON (_,_,SOME con)) => con | _ => error "DecConvarLookup") decs
     fun DecKindLookup (var, decs) = 
	 help ("Kind "^ var2string var)
	 (fn (DEC_CON (v,_,_)) => eq_var(var,v) | _ => false)
	 (fn (DEC_CON (_,k,_)) => k | _ => error "DecKindLookup") decs
     fun DecSignatLookup (var, decs) = 
	 help ("Signat " ^ var2string var)
	 (fn (DEC_MOD (v,_)) => eq_var(var,v) | _ => false)
	 (fn (DEC_MOD(_,s)) => s | _ => error "DecSignatLookup") decs
     fun RdecLookup (label,rdecs) = 
	 help ("rdec " ^ label2string label)
	 (fn (RDEC (l,_)) => eq_label(label,l))
	 (fn (RDEC (_,con)) => con) rdecs

     (* if the module argument is present, 
         then the component returned, if it is a DEC_CON, will have
	 all occurrences of variables that occur as part of that
	 signature replaced by a module projection *)
     fun SignatLookup (label,signat : Il.signat ,mopt : mod option) : sdec option = 
       (case signat of
	  (SIGNAT_FUNCTOR _) => error "SignatLookup on a functor signature"
	| (SIGNAT_DATATYPE (_,_,sdecs)) => SignatLookup(label,SIGNAT_STRUCTURE sdecs, mopt)
	| (SIGNAT_STRUCTURE sdecs) => 
	    let val _ = debugdo (fn () => (print "signat_lookup called with label = : "; 
					   pp_label label;
					   print "\nand signat:\n"; pp_signat signat; 
					   print "\n\n"))
	      fun help (acc,SDEC(l,DEC_CON(v,k,SOME c))) = 
		let val newc = (case mopt of
				  NONE => c
				| SOME m => con_subst_var_withproj(c,acc,m))
		in SOME(SDEC(l,DEC_CON(v,k,SOME newc)))
		end
		| help (acc,sdec) = SOME sdec
	      fun loop acc [] = NONE
		| loop acc ((cur as (SDEC (l,d))) :: rest) = if (eq_label(label,l)) 
							       then help(acc,cur)
							     else loop (cur::acc) rest
	    in  loop [] sdecs
	    end)
   end



   (* ------- checks for the syntactic valuability of ------- *)
   fun Exp_IsSyntacticValue exp = 
     (case exp of
	SCON _ => true
      | RECORD rbnds => foldr (fn (a,b) => a andalso b) true 
	                (map (fn RBND(l,e) => Exp_IsSyntacticValue e) rbnds)
      | FIX _ => true
      | INJ (_,_,e) => Exp_IsSyntacticValue e
      | TAG(_,c) => true
      | _ => false)
   and Module_IsSyntacticValue module = 
      (case module of
	 MOD_STRUCTURE sbnds => foldr (fn (a,b) => a andalso b) true 
	                        (map (fn SBND(l,b) => Bnd_IsSyntacticValue b) sbnds)
       | (MOD_FUNCTOR _) => true
       | _ => false)
   and Bnd_IsSyntacticValue bnd = 
     (case bnd of
       BND_EXP (v,e) => Exp_IsSyntacticValue e
     | BND_MOD (v,m) => Module_IsSyntacticValue m
     | BND_CON (v,c) => true
     | BND_FIXITY _ => true)


     (* ---------- Bound Rules from page 15 - 16 ----------------- *)
   datatype bound_name = BOUND_VAR of var | BOUND_NAME of tag
   fun Decs_Bound x = 
     let
       fun Dec_Bound (DEC_EXP (v,_)) = SOME(BOUND_VAR v)
	 | Dec_Bound (DEC_MOD (v,_)) = SOME(BOUND_VAR v)
	 | Dec_Bound (DEC_CON (v,_,_)) = SOME(BOUND_VAR v)
	 | Dec_Bound (DEC_EXCEPTION (n,_)) = SOME(BOUND_NAME n)
	 | Dec_Bound (DEC_FIXITY _) = NONE
     in List.mapPartial Dec_Bound x
     end
   fun var_in (var,[]) = false
     | var_in (var,(BOUND_VAR v)::rest) = eq_var(var,v) orelse var_in(var,rest)
     | var_in (var,_::rest) = var_in(var,rest)
   fun name_in (name,[]) = false
     | name_in (name,(BOUND_NAME v)::rest) = eq_tag(name,v) orelse name_in(name,rest)
     | name_in (name,_::rest) = name_in(name,rest)
   fun Sdecs_Domain sdecs = map (fn SDEC(l,_) => l) sdecs
 


   (* --------- structural equality on scons, kinds ------------ *)
   fun eq_scon (s1,s2) = s1 = s2
   fun eq_kind (KIND_TUPLE n1, KIND_TUPLE n2) = n1 = n2
     | eq_kind (KIND_ARROW (m1,n1), KIND_ARROW(m2,n2)) = (m1 = m2) andalso (n1 = n2)
     | eq_kind _ = false


   (* ---------------------------------------------------------------
      oneshot arrow unifier: note that unset does NOT unify with unset
     ---------------------------------------------------------------- *)
   fun eq_comp (comp1,comp2,is_sub) = (case (oneshot_deref comp1,oneshot_deref comp2) of
					   (SOME x, SOME y) => (x = y) orelse (is_sub andalso x = TOTAL)
					 | (SOME x, NONE) => (oneshot_set(comp2,x); true)
					 | (NONE, SOME x) => (oneshot_set(comp1,x); true)
					 | (NONE, NONE) => ((eq_oneshot(comp1,comp2)) orelse
							    (oneshot_set(comp1,PARTIAL); 
							     oneshot_set(comp2,PARTIAL); true)))
   fun comp_unify (a1,a2) = if (eq_comp(a1,a2,false)) then () else error "comp_unify failed"


   (* ------------------------------------------------------------ 
      type (sub)-equality:
	 First, normalize argument types:
           (1) a module projection whose type is known to the known type
           (2) beta-reducing constructor function application
	   (3) overloaded type expression to the carried type
         Then, performs structural equality except when a type variable.
	 When a type variable is encountered, the unifier routine
	     is called with the type variable and the given type.
	     The unifier may be side-effecting or not.
    -------------------------------------------------------------- *)
   fun unify_maker fetch set (constrained,tyvar,c,decs,is_sub) = 
     let 
       val self = unify_maker fetch set
(* XXX       val origv = tyvar_getvar tyvar *)
     in (case (fetch tyvar) of
	   NONE => (case c of
		      CON_TYVAR tv =>
(*			eq_var(tyvar_getvar tv,origv) orelse *)
			  eq_tyvar(tv,tyvar) orelse 
			(case (fetch tv) of
			   SOME c' => self(constrained,tyvar,c',decs,is_sub)
			 | NONE => (set(constrained,tyvar,c); true))
		    | _ => not (con_occurs(c,tyvar))
(*			  not (con_occurs(c,origv)))  *)
			  andalso (set(constrained,tyvar,c); true))
	 | (SOME c') => meta_eq_con self constrained (c',c,decs,is_sub))
     end

   and hard_unifier arg =
     let 
       fun hard_fetch tv = tyvar_deref tv
       fun hard_set (constr,tv,c) = (debugdo (fn () => (print "now hard-setting ";
							print (tyvar2string tv); print " to ";
							pp_con c; print "\n"));
				     if constr then con_constrain c else ();
				     if (tyvar_is_use_equal tv) then con_useeq c else ();
				     tyvar_set(tv,c))
     in  unify_maker hard_fetch hard_set arg
     end
   and soft_unifier () =
     let 
       val table = ref ([] : (con tyvar * con) list)
       val constr_con = ref ([] : con list)
       val useeq_con = ref ([] : con list)
       fun soft_fetch tyvar = let 
(*				  val v = tyvar_getvar tyvar *)
				  fun loop [] = NONE
				    | loop ((tv,c)::rest) = 
(*				      if (eq_var(v,tyvar_getvar tv)) *)
				      if (eq_tyvar(tyvar,tv))
					  then SOME c
				      else NONE
			      in (case (tyvar_deref tyvar) of
				    SOME c' => SOME c'
				  | NONE => loop (!table))
			      end
       fun soft_set (constr,tv,c) = let val _ = debugdo (fn () => 
							 (print "now soft-setting ";
							  print (tyvar2string tv); print " to ";
							  pp_con c; print "\n"))
					val _ = if constr then constr_con := c::(!constr_con) else ()
					val _ = if (tyvar_is_use_equal tv) 
						  then useeq_con := c::(!useeq_con) else ()
				    in table := (tv,c)::(!table)
				    end
       fun soft_unify arg = unify_maker soft_fetch soft_set arg
     in (soft_unify,table,constr_con,useeq_con)
     end
   and eq_con (con1,con2,decs) = meta_eq_con hard_unifier false (con1,con2,decs,false)
   and sub_con (con1,con2,decs) = meta_eq_con hard_unifier false (con1,con2,decs,true)
   and soft_eq_con (con1,con2,decs) = 
     let val (soft_unify,table,constr_con,useeq_con) = soft_unifier()
     in  if (meta_eq_con soft_unify false (con1,con2,decs,false))
	   then (app tyvar_set (!table); 
		 app (fn c => con_constrain c) (!constr_con);
		 app (fn c => con_useeq c) (!useeq_con);
		 true)
	 else false
     end
   and meta_eq_con unifier constrained (con1,con2,decs,is_sub) = 
     let 
       fun normalize c : (bool * con) = HeadNormalize(c,decs)
(*	 (case c of
	    CON_OVAR ocon => (true, ocon_deref ocon)
	  | CON_TUPLE_PROJECT (i,c) => let val (f,c) = normalize c
				       in case c of
					   CON_TUPLE_INJECT cons => 
					       let val len = length cons
					       in if (i >= 0 andalso i < len)
						      then 
							  let val (f',c') = normalize(List.nth(cons,i))
							  in (f orelse f', c')
							  end
						  else
						      error "normalize: con tuple projection - index wrong"
					       end
					 | _ => (f,CON_TUPLE_PROJECT(i,c))
				       end
	  | CON_APP(c1,c2) => let val (f1,c1') = normalize c1
				  val (f2,c2') = normalize c2
				  val (f,c) = (case (c1',c2') of
						   (CON_FUN(vars,cons), _) =>
						       normalize(ConApply(c1',c2'))
						  | _ => (false,CON_APP(c1',c2')))
			      in (f1 orelse f2 orelse f, c)
			      end
	  | (CON_MODULE_PROJECT (m,l)) =>
	       (let 
		   val s = GetModSig(m,decs)
		   fun loop [] = (false,c)
		     | loop ((SDEC(curl,DEC_CON(_,_,SOME curc)))::rest) = 
		       if eq_label(curl,l)
			   then normalize curc 
		       else loop rest
		     | loop (_::rest) = loop rest
	       in (case s of 
		       (SIGNAT_DATATYPE (_,_,sdecs)) => loop sdecs
		     | SIGNAT_STRUCTURE sdecs => loop sdecs
		     | _ => (false,c))
	       end
	   handle NOTFOUND _ => (false,c))
	  | _ => (false,c))
*)

       val _ = debugdo (fn () => (print "\nUnifying"; 
				  if constrained then
				      print " CONSTRAINED: "
				  else print " not constrained: ";
				  pp_con con1;
				  print "\nwith:     "; pp_con con2;
				  print "\nusing decs = \n"; pp_decs decs;
				  print "\n"))
       val (isocon1, con1) = normalize con1
       val (isocon2, con2) = normalize con2
       val _ = debugdo (fn () => (print "\nHeadNormalized to: "; pp_con con1;
				  print "\nand:     "; pp_con con2;
				  if isocon1 then
				      print " isocon1 is CONSTRAINED: "
				  else print " isocon1 is not constrained: ";
				  if isocon2 then
				      print " isocon2 is CONSTRAINED: "
				  else print " isocon2 is not constrained: ";
				  print "\n"))
       val constrained = constrained orelse isocon1 orelse isocon2
       val self = meta_eq_con unifier constrained

       val res =  
	 (case (con1,con2) of
	    (CON_TYVAR tv1, CON_TYVAR tv2) => 
(*		(eq_var(tyvar_getvar tv1,tyvar_getvar tv2) *)
		(eq_tyvar(tv1,tv2) 
		 orelse (unifier(constrained,tv1,con2,decs,is_sub)))
	  | (CON_TYVAR tv1, _) => unifier(constrained,tv1,con2,decs,is_sub)
	  | (_, CON_TYVAR tv2) => unifier(constrained,tv2,con1,decs,is_sub)
	  | (CON_VAR v1, CON_VAR v2) => eq_var(v1,v2)
	  | (CON_APP(c1_a,c1_r), CON_APP(c2_a,c2_r)) => self(c1_a,c2_a,decs,is_sub) 
	                                                andalso self(c1_r,c2_r,decs,is_sub)
	  | (CON_MODULE_PROJECT (m1,l1), CON_MODULE_PROJECT (m2,l2)) => 
		let fun eq_modval (MOD_VAR v1,MOD_VAR v2) = eq_var(v1,v2)
		      | eq_modval (MOD_PROJECT (m,l), 
				   MOD_PROJECT(m',l')) = eq_modval(m,m') andalso eq_label(l,l')
		      | eq_modval (MOD_SEAL(m,s),m') = eq_modval(m,m')
		      | eq_modval (m',MOD_SEAL(m,s)) = eq_modval(m,m')
		      | eq_modval((MOD_VAR _ | MOD_PROJECT _),
				  (MOD_VAR _ | MOD_PROJECT _)) = false
		      | eq_modval(m1,m2) = (print "eq_modval for a non value\nm1 = \n";
					    pp_mod m1; print "\nm2 = \n";
					    pp_mod m2; print "\n";
					    error "eq_modval for a non value")
		in eq_modval(m1,m2) andalso eq_label(l1,l2)
		end
	  | ((CON_INT is1, CON_INT is2) | (CON_UINT is1, CON_UINT is2)) => is1 = is2
	  | (CON_FLOAT fs1, CON_FLOAT fs2) => fs1 = fs2
	  | (CON_ANY, CON_ANY) => true
	  | (CON_ARRAY c1, CON_ARRAY c2) => self(c1,c2,decs,is_sub)
	  | (CON_VECTOR c1, CON_VECTOR c2) => self(c1,c2,decs,is_sub)
	  | (CON_REF c1, CON_REF c2) => self(c1,c2,decs,is_sub)
	  | (CON_TAG c1, CON_TAG c2) => self(c1,c2,decs,is_sub)
	  | (CON_ARROW (c1_a,c1_r,comp1), CON_ARROW(c2_a,c2_r,comp2)) => 
		(eq_comp(comp1,comp2,is_sub)
		 andalso self (c2_a,c1_a,decs,is_sub)
		 andalso self(c1_r,c2_r,decs,is_sub))
	  | (CON_MUPROJECT (i1,c1), CON_MUPROJECT(i2,c2)) => (i1=i2) andalso (self(c1,c2,decs,is_sub))
	  | (CON_RECORD rdecs1, CON_RECORD rdecs2) => 
		let val list1 = map (fn (RDEC x) => x) rdecs1
		    val list1_sorted = sort_labelpair list1
		    val list2 = map (fn (RDEC x) => x) rdecs2
		    val list2_sorted = sort_labelpair list2
		    fun help ((l1,c1),(l2,c2)) = eq_label(l1,l2) 
			andalso self(c1,c2,decs,is_sub)
		in
		    eq_list(help,list1,list2)
		end
	  | (CON_FUN (vs1,c1), CON_FUN(vs2,c2)) => 
		(length vs1 = length vs2) andalso
		let val decs' = foldl (fn (v,acc) => ((DEC_CON(v,KIND_TUPLE 1, NONE))::acc)) decs vs1
		    val table = map2 (fn (v1,v2) => (v2,CON_VAR v1)) (vs1,vs2)
		    val c2' = con_subst_convar(c2,table)
		in self(c1,c2',decs',is_sub)
		end
	  | (CON_SUM (i1,cs1), CON_SUM (i2,cs2)) => 
		(i1=i2) andalso
		eq_list (fn (a,b) => self(a,b,decs,is_sub), cs1, cs2)
	  | (CON_TUPLE_INJECT cs1, CON_TUPLE_INJECT cs2) => 
		eq_list (fn (a,b) => self(a,b,decs,is_sub), cs1, cs2)
	  | (CON_TUPLE_PROJECT (i1, c1), CON_TUPLE_PROJECT(i2,c2)) => 
		(i1 =i2) andalso (self(c1,c2,decs,is_sub))
	  | _ => false)
       val _ = debugdo (fn () => print (if res then "unified\n" else "NOT unified\n"))
     in res
     end


   and eq_dec (d1,d2,decs) = 
       (case (d1,d2) of
	    (DEC_EXP (v1,c1), DEC_EXP(v2,c2)) => (eq_var(v1,v2)) andalso eq_con (c1,c2,decs)
	  | (DEC_MOD(v1,s1), DEC_MOD(v2,s2)) => (eq_var(v1,v2)) andalso eq_sig (decs,s1,s2)
	  | (DEC_CON (v1,k1,NONE), DEC_CON(v2,k2,NONE)) => (eq_var(v1,v2)) 
	  | (DEC_CON (v1,k1,SOME c1), DEC_CON(v2,k2,SOME c2)) => ((eq_var(v1,v2)) andalso 
								  eq_con (c1,c2,decs))
	  | (DEC_EXCEPTION (n1,c1), DEC_EXCEPTION(n2,c2)) => ((eq_tag(n1,n2)) andalso 
							      eq_con (c1,c2,decs))
	  | (DEC_FIXITY ft1, DEC_FIXITY ft2) => eq_list(fn ((l1,f1),(l2,f2)) => 
							(eq_label(l1,l2) andalso f1=f2),ft1,ft2)
	  | _ => false)

   and eq_sdec decs (SDEC(l1,d1),SDEC(l2,d2)) = eq_label(l1,l2) andalso (eq_dec(d1,d2,decs))
   and eq_sdecs (decs,sdecs1,sdecs2) = eq_list(eq_sdec decs, sdecs1, sdecs2)


   and eq_sig (decs,SIGNAT_STRUCTURE sdecs1, 
	       SIGNAT_STRUCTURE sdecs2) = eq_sdecs(decs,sdecs1,sdecs2)
     | eq_sig (decs,SIGNAT_DATATYPE (_,_,sdecs1), 
	       SIGNAT_DATATYPE (_,_,sdecs2)) = eq_sdecs(decs,sdecs1,sdecs2)
     | eq_sig (decs,SIGNAT_FUNCTOR (v1,s1_arg,s1_res,comp1), 
	       SIGNAT_FUNCTOR (v2,s2_arg,s2_res,comp2)) = 
       (eq_comp(comp1,comp2,false) andalso (eq_var(v1,v2)) andalso (eq_sig(decs,s1_arg,s2_arg)))
       andalso let val decs' = add_dec_mod(decs,(v1,s1_arg))
	       in  eq_sig (decs',s1_res,s2_res)
	       end
     | eq_sig _ = false



   and Exp_IsValuable exp decs = 
     (Exp_IsSyntacticValue exp) orelse
     (case exp of
	MODULE_PROJECT (m,l) => Module_IsValuable m decs
      | APP(e1,e2) => let val e1_con = GetExpCon(e1,decs)
			  val e1_con_istotal = 
			      (case e1_con of
				   CON_ARROW(_,_,comp) => eq_comp(comp,oneshot_init TOTAL,false)
				 | _ => false)
		      in e1_con_istotal andalso (Exp_IsValuable e1 decs) 
			  andalso (Exp_IsValuable e2 decs)
		      end
     | RECORD rbnds => foldr (fn (a,b) => a andalso b) true 
                       (map (fn RBND(l,e) => Exp_IsValuable e decs) rbnds)
     | LET (bnds,e) => (case (Bnds_IsValuable' bnds decs) of 
			  NONE => false
			| SOME decs => Exp_IsValuable e decs)
     | ROLL (c,e) => Exp_IsValuable e decs
     | UNROLL (c,e) => Exp_IsValuable e decs
     | PROJ (c,i,e) =>  Exp_IsValuable e decs
     | OVEREXP (_,v,os) => v orelse (case (oneshot_deref os) of
				       NONE => false
				     | SOME e => Exp_IsValuable e decs)
     | _ => false)

   (* Rules 140 - 143 *)
   and Bnds_IsValuable' [] decs = SOME decs
     | Bnds_IsValuable' (bnd::rest) decs = 
       let val self = Bnds_IsValuable' rest
       in  (case bnd of
		BND_EXP (v,e) => if (Exp_IsValuable e decs) 
				     then self (add_dec_exp(decs,(v,GetExpCon(e,decs))))
				 else NONE
	      | BND_MOD (v,m) => if (Module_IsValuable m decs)
				     then self (add_dec_mod(decs,(v,GetModSig(m,decs)))) 
				 else NONE
	      | BND_CON (v,c) => self (add_dec_con(decs,(v,GetConKind(c,decs),c)))
	      | BND_FIXITY _ => self decs)
       end

   and Bnds_IsValuable bnds decs = (case (Bnds_IsValuable' bnds decs) of
				      NONE => false
				    | SOME decs => true)
   and Sbnds_IsValuable sbnds decs = Bnds_IsValuable (map (fn (SBND(_,b)) => b) sbnds) decs
     
   (* Rules 144 - 147 *)
   and Module_IsValuable m decs =
     (Module_IsSyntacticValue m) orelse
     (case m of
       MOD_VAR v => true
     | MOD_STRUCTURE sbnds => Sbnds_IsValuable sbnds decs
     | MOD_PROJECT (m,l) => true
     | MOD_APP (m1,m2) => (case (GetModSig(m1,decs)) of
			     SIGNAT_FUNCTOR(_,_,_,comp) =>
			       eq_comp(oneshot_init TOTAL,comp,false) andalso
			       (Module_IsValuable m1 decs) andalso (Module_IsValuable m2 decs)
			   | _ => false)
     | _ => false)
	
   and GetSconCon (scon) : con = PrimUtil.value_type scon


  (* Rules 35 - 48 *)
   and GetConKind (arg : con, decs) : kind = 
     let val con = arg (* Normalize(arg,decs) *)
     in case con of
       (CON_TYVAR tv) => KIND_TUPLE 1 
     (* (DecKindLookup(tyvar_getvar tv,decs) : kind) *)
     | (CON_VAR v) => (DecKindLookup(v,decs) : kind)
     | (CON_OVAR ocon) => raise UNIMP
     | (CON_INT _) => KIND_TUPLE 1
     | (CON_FLOAT _) => KIND_TUPLE 1
     | (CON_UINT _) => KIND_TUPLE 1
     | (CON_ANY) => KIND_TUPLE 1
     | (CON_REF _) => KIND_TUPLE 1
     | (CON_ARRAY _) => KIND_TUPLE 1
     | (CON_VECTOR _) => KIND_TUPLE 1
     | (CON_TAG _) => KIND_TUPLE 1
     | (CON_ARROW _) => KIND_TUPLE 1
     | (CON_APP (c1,c2)) => let val (k1,k2) = (GetConKind(c1,decs),GetConKind(c2,decs))
			    in (case (k1,k2) of
				  (KIND_ARROW(a,b),KIND_TUPLE c) => 
				    if (a=c) then KIND_TUPLE b
				    else error "GetConKind: kind mismatch in CON_APP"
				| _ => error "GetConKind: kind mismatch in CON_APP")
			    end
     | (CON_MUPROJECT _) => KIND_TUPLE 1
     | (CON_RECORD _) => KIND_TUPLE 1
     | (CON_FUN (vs,c)) => (case GetConKind(c,foldl (fn (v,acc) => 
						     ((DEC_CON(v,KIND_TUPLE 1, NONE))::acc)) decs vs) of
			      KIND_TUPLE n => KIND_ARROW(length vs,n)
			    | (KIND_ARROW _) => error "kind of constructor body not a KIND_TUPLE")
     | (CON_SUM _) => KIND_TUPLE 1
     | (CON_TUPLE_INJECT cs) => KIND_TUPLE (length cs)
     | (CON_TUPLE_PROJECT (i,c)) => (case (GetConKind(c,decs)) of
				       KIND_TUPLE n =>
					 (if (i > 0 andalso i <= n)
					    then KIND_TUPLE 1
					  else
					    (print "GetConKind got: ";
					     pp_con con;
					     print "\n";
					     error "got CON_TUPLE_PROJECT in GetConKind"))
				     | _ => error "got CON_TUPLE_PROJECT in GetConKind")
     | (CON_MODULE_PROJECT (m,l)) => let val signat = GetModSig(m,decs)
				     in (case (SignatLookup(l,signat,NONE)) of
					   NONE => error "no such label in sig"
					 | SOME(SDEC(_,DEC_CON(v,k,_))) => k
					 | _ => error "label in sig not a DEC_CON")
				     end
     end

   (* --------- Rules 69 to 96 ----------- *)
   and GetExpCon (earg,decs) = 
     (debugdo (fn () => (print "GetExpCon called with exp = \n";
			 pp_exp earg; print "\nand decs = \n";
			 pp_decs decs; print "\n"));
     case earg of
       SCON scon => GetSconCon scon
     | OVEREXP (con,_,_) => con
     | PRIM (p,cs) => PrimUtil.get_type p cs
     | ILPRIM (ip) => PrimUtil.get_iltype ip
     | (VAR v) => DecVarLookup(v,decs)
(*     | SEQ elist => GetExpCon(List.last elist, decs) *)
     | (APP (e1,e2)) => let val con1 = Normalize(GetExpCon(e1,decs),decs)
			    val con2 = GetExpCon(e2,decs)
			in  (case con1 of
			       CON_ARROW (arg_con,res_con,discard_complete) => 
				 if sub_con(arg_con,con2,decs) then res_con
				 else error "Type mismatch in expression application"
			     | _ => error "Type of first expression in application is not an arrow")
			end
     | (FIX (fbnds, var)) => 
	   let fun loop [] = error "FIX: var is not in any fbnd expression"
		 | loop ((FBND(v',v,c,c',e))::rest) = if (eq_var(var,v')) 
							  then (v,c,e)
						      else loop rest
	       val (v,c,e) = loop fbnds
	   in if (Exp_IsValuable e decs)
		  then let (* Rule 75 *)
			   val decs' = add_dec_exp(decs,(v,c))
			   val econ = GetExpCon(e,decs')
		       in CON_ARROW(c,econ,oneshot_init TOTAL)
		       end
	      else (* Rule 74 *)
		  let
		      fun extend [] ds = ds
			| extend ((FBND(v',v,c,c',e))::rest) ds = 
			  extend rest (add_dec_exp(ds,(v',CON_ARROW(c,c',
								    oneshot_init PARTIAL))))
		      val decs' = extend fbnds decs
		      fun test [] = ()
			| test ((FBND(v',v,c,c',e))::rest) = 
			  let val bodyc = GetExpCon(e,add_dec_exp(decs',(v,c)))
			      fun local_error s = (print ("GetExpCon " ^ s ^ " failed on: ");
						   pp_exp earg; print "\nc' = "; pp_con c';
						   print "\nbodyc = "; pp_con bodyc;
						   print "\nwith context";
						   pp_decs decs'; print "\n";
						   error "FIX(rule 74): premises fails: ill-typed")
			  in if (eq_con(c',bodyc,decs'))
				 then test rest 
			     else local_error ""
				 handle _ => local_error "raise error"
			  end
		      val _ = test fbnds
		      val decs'' = add_dec_exp(decs',(v,c))
		  in CON_ARROW(c,GetExpCon(e,decs''),oneshot_init PARTIAL)
		  end
	   end
     | (RECORD (rbnds)) => 
	   let val rdecs = (map (fn r => GetRbndRdec r decs) rbnds)
	       val temp1 = map (fn RDEC x => x) rdecs
	       val temp2 = sort_labelpair temp1
	       val rdecs' = map RDEC temp2
	   in CON_RECORD rdecs'
	   end
     | (RECORD_PROJECT (exp,l,c)) => 
	   let val con = GetExpCon(exp,decs)
	   in (case con of 
		   (CON_RECORD rdecs) => RdecLookup(l,rdecs)
		 | _ => error "Record Proj on exp not of type CON_RECORD")
	   end
     | (SUM_TAIL (c,e)) => 
	   let val con = GetExpCon(e,decs)
	   in if (eq_con(c,con,decs)) 
		  then (case con of
			    CON_SUM(SOME i,cons) => List.nth(cons,i)
			  | _ => error "adornment of SUM_TAIL not a CON_SUM_I")
	      else error "SUM_TAIL: adornment wrong"
	   end
     | (HANDLE (body,handler)) => 
	   let val bcon = GetExpCon(body,decs)
	       val hcon = GetExpCon(handler,decs)
	   in (case hcon of
		   CON_ARROW (CON_ANY,res_con,discard_complete) =>
		       if (eq_con(res_con,bcon,decs)) then bcon 
		       else error "Type mismatch between handler and body of HANDLE"
		 | _ => error "Handler not of type Any -> con")
	   end
     | (RAISE (c,e)) => let val econ = GetExpCon(e,decs)
			in (case econ of
				CON_ANY => c
			      | _ => error "type of expression raised is not ANY")
		    end
     | (LET (bnds,e)) => let 
			    val decs' = GetBndsDecs(decs,bnds) @ decs
			    val econ = GetExpCon(e,decs')
			    val econ_kind = GetConKind(econ,decs)
			in  (case econ_kind of
			      (KIND_TUPLE 1) => econ
			    | _ => error "body of let has a type not of kind KIND_TUPLE(1)")
		       end
     | (NEW_STAMP con) => CON_TAG con
     | (EXN_INJECT (e1,e2)) => let 
				   val c1 = GetExpCon(e1,decs)
				   val c2 = GetExpCon(e2,decs)
			       in if (eq_con (c1,CON_TAG c2,decs))
				   then CON_ANY
				  else error "EXN_INJECT tag type and value: type mismatch"
			       end
     | (MK_REF e) => CON_REF (GetExpCon(e,decs))
     | (GET e) => (case GetExpCon(e,decs) of
			CON_REF c => Normalize(c,decs)
		      | _ => error "GET given expression whose normalized type is not CON_REF(con)")
     | (SET (e1,e2)) => CON_RECORD []  (* unit *)
     | (ROLL (c,e) | UNROLL(c,e)) => 
       let val cNorm = Normalize' "ROLL/UNROLL" (c,decs)
	   val cNorm_kind = GetConKind(cNorm,decs)
	   val econ = GetExpCon(e,decs)
       in (case (cNorm,cNorm_kind) of
	     (CON_MUPROJECT(i,cInner),KIND_TUPLE 1) => 
	       let val cInner_norm = Normalize' "ROLL/UNROLL 2" (cInner,decs)
	       in  (case GetConKind(cInner_norm,decs) of
		    KIND_ARROW(n,n') =>
		      (if ((n = n') andalso (0 <= i) andalso (i < n))
			   then 
			       let 
				   val temp = (fn j => CON_MUPROJECT(j,cInner_norm))
				   val contemp = if (n=1) then temp 0
						 else CON_TUPLE_INJECT(map0count temp n)
				   val con2 = if (n = 1)
						  then CON_APP(cInner_norm,contemp)
					      else CON_TUPLE_PROJECT(i,CON_APP(cInner_norm,contemp))
			       in
				   case earg of
				       (ROLL _) => 
					   if (eq_con(econ,con2,decs))
					       then cNorm
					   else error "ROLL: expression type does not match decoration"
				     | (UNROLL _) => 
					   if (eq_con(econ,cNorm,decs))
					       then Normalize' "ROLL/UNROLL 3" (con2,decs)
					   else (print "UNROLL: expression type does not match decoration";
						 print "\necon = "; pp_con econ;
						 print "\ncNorm = "; pp_con cNorm;
						 print "\ndecs = "; pp_decs decs;
						 error "UNROLL: expression type does not match decoration")
				     | _ => error "IMPOSSIBLE: what happened to (UN)ROLL"
			       end
		       else error "projected decoration has the wrong KIND_ARROW")
		  | _ => error "projected decoration has the wrong kind")
	       end
	   | _ => (print "Decoration of ROLL has kind ";
		   pp_kind cNorm_kind;
		   print " and was:\n"; pp_con cNorm;
		   print "\n";
		   error "decoration of ROLL not a projection"))
       end
    | (INJ (cons,i,e)) => let val econ = GetExpCon(e,decs)
			       val clist = map (fn c => Normalize(c,decs)) cons
			       val n = length clist
			   in if (0 <= i andalso i < n andalso 
				  (eq_con(econ, List.nth(clist,i), decs)))
				then CON_SUM(NONE,clist)
			      else (error "INJ: injection field out of range")
			   end
     | (PROJ (cons,i,e)) => let val econ = GetExpCon(e,decs)
				val clist = map (fn c => Normalize(c,decs)) cons
			    in (case econ of
				    CON_SUM(_,clist2) =>
					let val n = length clist
					in if (0 <= i andalso i <n andalso 
					       eq_list (fn (x,y) => eq_con(x,y,decs), clist, clist2))
					       then List.nth(clist,i)
					   else error "PROJ: index out of range"
					end
				  | _ =>  (print "PROJ: expression type is not sum type.\n";
					   print "Expression is: \n"; pp_exp e; print "\n";
					   print "Reduce type is: \n"; pp_con econ; print "\n";
					   error "PROJ: expression not of con CON_SUM"))
			    end
     | (TAG (n,con)) => let val nCon = Normalize(DecExnLookup(n,decs),decs)
			in (case nCon of
				CON_TAG c => if (eq_con(c,con,decs)) 
						 then CON_ANY
					     else error "TAG: decorated name con and exp con mismatch"
			      | _ => error "TAG: decorated name does not have con CON_TAG")
		      end
     | (EXN_CASE (arg,arms,eopt)) =>
	   let 
	       val argcon = GetExpCon(arg,decs)
	       val rescon = fresh_con()
	       fun checkarm(e1,c,e2) = 
		   let val c1 = GetExpCon(e1,decs)
		       val c2 = GetExpCon(e2,decs)
		   in (eq_con(c1,CON_TAG c, decs))
		       andalso eq_con(c2,CON_ARROW(c,rescon,oneshot()),decs)
		   end
	       val _ = (case eopt of
			    NONE => ()
			  | SOME e => let val optcon = GetExpCon(e,decs)
				      in if (eq_con(optcon,CON_ARROW(CON_ANY,rescon,
								     oneshot()),decs)) then ()
					 else error "default case has bad type in EXN_CASE"
				      end)
	   in if (eq_con(argcon,CON_ANY,decs))
		  then if (andfold checkarm arms)
			   then rescon
		       else error "rescon does not match in EXN_CASE"
	      else error "arg not a CON_ANY in EXN_CASE"
	   end
     | (CASE (cons,earg,earms,edef)) => 
	   let 
	       val n = length earms
	       val consNorm = map (fn c => Normalize' "CASE" (c,decs)) cons
	       val eargCon = GetExpCon(earg,decs)
	       val sumcon = CON_SUM (NONE,consNorm)
	       val rescon = fresh_con()
	       fun loop [] [] = 
		   (case edef of 
			NONE => rescon
		      | SOME edef => 
			    let val defcon = GetExpCon(edef,decs)
			    in  if (eq_con(CON_ARROW(sumcon,rescon,
						     oneshot_init PARTIAL),
					   defcon,decs))
				    then con_deref rescon
				else error "default arm type mismatch"
			    end)
		 | loop (NONE::arms) (con::cons) = loop arms cons
		 | loop ((SOME exp)::arms) (con::cons) = 
		   let val c = GetExpCon(exp,decs)
		   in if (eq_con(CON_ARROW(con,rescon,
					   oneshot_init PARTIAL),
				 c,decs))
			  then loop arms cons
		      else error "case arm type mismatch"
		   end
		 | loop _ _ = error "CASE: number of con != number of arms"
	   in if (eq_con(eargCon,sumcon,decs))
		  then loop earms consNorm
	      else 
		  error "CASE: expression type and decoration con mismatch"
	   end
     | (MODULE_PROJECT(m,l)) => 
		  let val signat = GetModSig(m,decs)
		  in (case SignatLookup(l,signat,SOME m) of
			NONE => error "MODULE_PROJECT: label not in modsig"
		      | (SOME (SDEC(_,DEC_EXP(_,con)))) => con
		      | _ => fail "MODULE_PROJECT: label in modsig not DEC_EXP")
		  end
     | (SEAL (e,c)) => Normalize(c,decs))


   (* ----------- rules 22 - 25 ------------------------- *)
   and GetBndDec (decs,BND_EXP (v,e))  = DEC_EXP(v,GetExpCon (e,decs))
     | GetBndDec (decs,BND_MOD (v,m))  = DEC_MOD(v,GetModSig (m,decs))
     | GetBndDec (decs,BND_CON (v,c))  = DEC_CON(v,GetConKind(c,decs),SOME c)
     | GetBndDec (decs,BND_FIXITY arg) = DEC_FIXITY arg
   and GetBndsDecs (decs,bnds) = GetBndsDecs'(decs,bnds,[])
   and GetBndsDecs' (decs,[],acc) = rev acc
     | GetBndsDecs' (decs,bnd::rest,acc) = let val d = GetBndDec(decs,bnd)
					   in GetBndsDecs' (add_dec_dec(decs,d),rest, d::acc)
					   end
   and GetSbndSdec (decs,SBND (l, bnd)) = SDEC(l,GetBndDec(decs,bnd))
   and GetSbndsSdecs (decs, sbnds) = map (fn sbnd => GetSbndSdec(decs,sbnd)) sbnds

   (* ----------- rules 31 - 32 ------------------------- *)
   and GetRbndRdec (RBND (l,e)) decs : rdec = RDEC (l,GetExpCon(e,decs))


   (* ----------------------------------------------------------------
    Given a signature s of a module m, replace each use of an internal
      variable by a module projection(s). *)
   and NormalizeSig'(t : (var * con) list, mopt: mod option, signat : signat) = 
     let 
       fun loop table [] = []
	 | loop table ((SDEC(l,dec))::rest) = 
	 let 
	   fun self d vopt =
	     (case (vopt,mopt) of
		(SOME v, SOME m) => (SDEC(l,d) :: 
				     (loop ((v,CON_MODULE_PROJECT(m,l))::table) rest))
	      | _ => (SDEC(l,d)) :: (loop table rest))
	 in (case dec of
	       DEC_EXP(v,c) => self (DEC_EXP(v,con_subst_var(c,table))) NONE
	     | DEC_MOD(v,s) => let val mopt' = (case mopt of
						    SOME m => SOME (MOD_PROJECT(m,l))
						  | NONE => NONE)
			       in self (DEC_MOD(v,NormalizeSig'(table,mopt', s))) NONE
			       end
	     | DEC_CON(v,k,NONE) => self dec (SOME v)
	     | DEC_CON(v,k,SOME c) => self (DEC_CON(v,k,SOME (con_subst_var(c,table)))) (SOME v)
	     | DEC_EXCEPTION _ => self dec NONE
	     | DEC_FIXITY _ => self dec NONE)
	 end
     in (case signat of
	   SIGNAT_FUNCTOR (v,s1,s2,a) => SIGNAT_FUNCTOR(v,NormalizeSig'(t,NONE,s1),
							NormalizeSig'(t,NONE,s2),a)
	 | SIGNAT_DATATYPE (dt,tb, sdecs) => SIGNAT_DATATYPE(dt,tb,loop t sdecs)
	 | SIGNAT_STRUCTURE sdecs => SIGNAT_STRUCTURE(loop t sdecs))
     end
   and NormalizeSig(m: mod, signat : signat) = 
     let val res = NormalizeSig'([],SOME m,signat)
       val _ = debugdo (fn () => (print "NormalizeSig': signat is\n";
				  pp_signat signat; print "\n\nand returning res:\n";
				  pp_signat res; print "\n\n"))
     in res
     end

   (* ------------ Return a module's signature    -------------- *)
   and GetModSig (module, decs : decs) =
     (debugdo (fn () => (print "GetModSig called with module = \n";
			 pp_mod module; print "\nand decs = \n";
			 pp_decs decs; print "\n"));
	       
      case module of
       (MOD_VAR v) => NormalizeSig(MOD_VAR v, DecSignatLookup(v,decs))
     | MOD_STRUCTURE (sbnds) => 
	   let fun loop [] acc decs = rev acc
		 | loop (sb::sbs) acc decs = 
		   let 
		       val sdec = GetSbndSdec(decs,sb)
		       val SDEC(_,dec) = sdec
		   in loop sbs (sdec::acc) (decs@[dec])
		   end
	       val res = SIGNAT_STRUCTURE ((loop sbnds [] decs) : sdec list)
	   in res
	   end
(*     | MOD_DATATYPE  (dts,tbs,sbnds) => 
    SIGNAT_DATATYPE(dts,tbs,map (fn s => GetSbndSdec(decs,s)) sbnds) 
*)
     | MOD_FUNCTOR (v,s,m) => 
	   let val decs' = add_dec_mod(decs,(v,s))
	       val signat = GetModSig(m,decs')
	   in  SIGNAT_FUNCTOR(v,s,signat,oneshot_init 
			      (if (Module_IsValuable m decs') then TOTAL else PARTIAL))
	   end
     | MOD_APP (a,b) => 
	   let val _ = print "\n\nMOD_APP case in GetModSig\n"
	       val _ = (print "a is\n"; pp_mod a; print "\n")
	       val _ = (print "b is\n"; pp_mod b; print "\n")
	       val asignat = GetModSig(a,decs)
	       val bsignat = GetModSig(b,decs)
	       val _ = print "\n\nMOD_APP case in GetModSig got asignat and bsignat\n"
	       val _ = (print "asignat is\n"; pp_signat asignat; print "\n")
	       val _ = (print "bsignat is\n"; pp_signat bsignat; print "\n")
	   in case asignat of
	       (SIGNAT_STRUCTURE _) => error "Can't apply a structure signature"
	     | (SIGNAT_DATATYPE _) => error "Can't apply a datatype signature"
	     | SIGNAT_FUNCTOR (v,csignat,dsignat,_) =>
		   if (Sig_IsSub(decs, bsignat, csignat))
		       then dsignat
			    else error ("Signature Application where" ^ 
					" argument and parameter signature mismatch")
	   end
     | MOD_PROJECT (m,l) => 
	   let 
	       val _ = debugdo (fn () => (print "GetModSig called with: "; 
					  pp_mod module; print "\n"))
	       val signat = GetModSig(m,decs)
	       val _ = debugdo (fn () => (print "retrieved signat of \n"; 
					  pp_signat signat; print "\n"))
	       val res = case SignatLookup(l,signat,SOME m) of
				NONE => (print "GetModSig: SignatLookup MOD_PROJECT failed with label ";
					 pp_label l;
					 print "\nand with signat = \n";
					 pp_signat signat;
					 print "\n";
					 error "MOD_PROJECT failed to find label ")
			      | (SOME (SDEC(_,DEC_MOD(v,s)))) => s
			      | _ => error "MOD_PROJECT found label not of flavor DEC_MOD"
	       val _ = debugdo (fn () => (print "returning res = "; pp_signat res; print "\n"))
	   in res
	   end
     | MOD_SEAL (m,s) => s)

    and HeadNormalize (arg,decs) : (bool * con) = 
	 (case arg of
	      CON_OVAR ocon => (true, #2(HeadNormalize(CON_TYVAR (ocon_deref ocon),decs)))
	    | (CON_TYVAR tv) => (tyvar_isconstrained tv,
				 case tyvar_deref tv of
				     NONE => arg
				   | SOME c => #2(HeadNormalize(c,decs)))
	    | (CON_VAR v) => let fun loop [] = error "Normalize could not reduce CON_VAR"
				   | loop (DEC_CON(v',_,copt)::rest) = 
				     if (eq_var (v,v'))
					 then (case copt of
						   NONE => (false, CON_VAR v)
						 | SOME c => HeadNormalize(c,decs))
				     else loop rest
				   | loop (_::rest) = loop rest
			     in loop decs
			     end
	  | CON_TUPLE_PROJECT (i,c) => let val (f,c) = HeadNormalize(c,decs)
				       in case c of
					   CON_TUPLE_INJECT cons => 
					       let val len = length cons
					       in if (i >= 0 andalso i < len)
						      then 
							  let val (f',c') = HeadNormalize(List.nth(cons,i),
											  decs)
							  in (f orelse f', c')
							  end
						  else
						      error "HeadNormalize: con tuple projection - index wrong"
					       end
					 | _ => (f,CON_TUPLE_PROJECT(i,c))
				       end
	  | CON_APP(c1,c2) => let val (f1,c1') = HeadNormalize(c1,decs)
				  val (f2,c2') = HeadNormalize(c2,decs)
				  val (f,c) = (case (c1',c2') of
						   (CON_FUN(vars,cons), _) =>
						    HeadNormalize(ConApply(c1',c2'),decs)
						  | _ => (false,CON_APP(c1',c2')))
			      in (f1 orelse f2 orelse f, c)
			      end
	  | (CON_MODULE_PROJECT (m,l)) =>
	       (let 
		   val s = GetModSig(m,decs)
		   fun loop [] = (false,arg)
		     | loop ((SDEC(curl,DEC_CON(_,_,SOME curc)))::rest) = 
		       if eq_label(curl,l)
			   then HeadNormalize(curc,decs)
		       else loop rest
		     | loop (_::rest) = loop rest
	       in (case s of 
		       (SIGNAT_DATATYPE (_,_,sdecs)) => loop sdecs
		     | SIGNAT_STRUCTURE sdecs => 
			   (debugdo (fn () => (print "HeadNormalize: CON_MODULE_PROJECT case: l = "; pp_label l;
					       print "\n and sdecs = ";
					       pp_sdecs sdecs;
					       print "\n"));
			    loop sdecs)
		     | _ => (false,arg))
	       end
	   handle NOTFOUND _ => (false,arg))
	  | c => (false,c))

	
    and Normalize' str (arg,decs) = 
	(debugdo (fn () => (print "Normalize called with string = ";
			    print str; print "\n"));
	 Normalize (arg,decs))
	
 (* -------- performs CON application, tuple projection, and module projections ----- *)
   and Normalize (arg,decs) = 
       (debugdo (fn () => (print "Normalize called with con = ";
			   pp_con arg; print "\nand decs = \n";
			   pp_decs decs;	print "\n"));
      case arg of
	(CON_INT _ | CON_FLOAT _ | CON_UINT _ | CON_ANY) => arg
      | (CON_REF c)               => CON_REF (Normalize(c,decs))
      | (CON_ARRAY c)             => CON_ARRAY (Normalize(c,decs))
      | (CON_VECTOR c)            => CON_VECTOR (Normalize(c,decs))
      | (CON_TAG c)               => CON_TAG (Normalize(c,decs))
      | (CON_ARROW (c1,c2,comp))  => CON_ARROW (Normalize(c1,decs),Normalize(c2,decs),comp)
      | (CON_MUPROJECT (i,c))     => CON_MUPROJECT (i, Normalize(c,decs))
      | (CON_RECORD rdecs)        => let fun f (RDEC(l,c)) = RDEC(l,Normalize(c,decs))
	                             in CON_RECORD (map f rdecs)
                                     end
      | (CON_FUN (vs,c))          => CON_FUN (vs,c)
      | (CON_SUM (i,cs))          => CON_SUM (i,map (fn c => Normalize(c,decs)) cs)
      | (CON_TUPLE_INJECT cs)     => CON_TUPLE_INJECT (map (fn c => Normalize(c,decs)) cs)
      | (CON_TUPLE_PROJECT (i,c)) => Normalize(#2(HeadNormalize(arg,decs)),decs)
      | (CON_MODULE_PROJECT (m as MOD_STRUCTURE sbnds,l)) => (* no need to normalize m *)
	    (case Sbnds_Lookup(m,sbnds,[l]) of
		 (_, PHRASE_CON c) => Normalize(c,decs)
	       | _ => error "Module_Lookup found a non con while normalizing a con_mod_proj")
      | (CON_MODULE_PROJECT (m,l)) => 
	    (let val _ = debugdo (fn () =>
				  (print "normalize about to call getmodsig of m = \n";
				   pp_mod m;
				   print "\nand decs = \n";
				   pp_decs decs;
				   print "\n"))
		 val signat = GetModSig(m,decs)
		 val _ = debugdo (fn () => (print "normalize got back sig:\n";
					    pp_signat signat; print "\n"))
	     in  case SignatLookup(l,signat,SOME m) of
		 NONE => error "CON_MOD_PROJECT failed to find label"
	       | (SOME (SDEC(_,DEC_CON(v,k,SOME con)))) => con
	       | (SOME (SDEC(_,DEC_CON(v,k,NONE)))) => arg
	       | (SOME _) => error "CON_MOD_PROJECT found label not DEC_CON"
	     end)
      | (CON_VAR v) => Normalize(#2(HeadNormalize(arg,decs)),decs)
      | (CON_TYVAR co) => (case tyvar_deref co of
			      SOME c => Normalize(c,decs)
			    | NONE => arg)
      | (CON_APP(a,b)) => Normalize(#2(HeadNormalize(arg,decs)),decs))



     (* Rule 33 - 34 *)
     and Kind_Valid (KIND_TUPLE n)     = n >= 0
       | Kind_Valid (KIND_ARROW (m,n)) = (m >= 0) andalso (n >= 0)

     (* Rules 1 - 2 *)	
     and Decs_Valid [] = true
       | Decs_Valid (a::rest) = Dec_Valid(rest,a) andalso Decs_Valid rest
     (* Rule 7 - 12 *)
     and Dec_Valid (decs : decs,dec) = 
       let fun var_notin v  = (not (var_in(v,Decs_Bound decs)))
	   fun name_notin n = (not (name_in(n,Decs_Bound decs)))
       in  (case dec of
	      DEC_EXP(v,c) => (var_notin v) andalso (GetConKind(c,decs) = (KIND_TUPLE 1))
	    | DEC_MOD(v,s) => (var_notin v) andalso (Sig_Valid(decs,s))
	    | DEC_CON (v,k,NONE) => (var_notin v) andalso (Kind_Valid k)
	    | DEC_EXCEPTION(name,CON_TAG c) => ((name_notin name) andalso 
						(GetConKind(c,decs) = (KIND_TUPLE 1)))
	    | _ => false)
       end
					       
     (* Rules 97 - 98 *)	
     and Sdecs_Valid (decs, []) = Decs_Valid decs
       | Sdecs_Valid (decs, (SDEC(label,dec)::rest)) = 
	 (Dec_Valid(decs,dec) andalso 
	  Sdecs_Valid(add_dec_dec(decs,dec),rest) andalso 
	  (not (List.exists (fn l => eq_label(label,l)) 
		(Sdecs_Domain rest))))

     (* Rules 106 - 108 *)
     and Sig_Valid (decs, SIGNAT_STRUCTURE sdecs) = Sdecs_Valid(decs,sdecs)
       | Sig_Valid (decs, SIGNAT_DATATYPE (_,_,sdecs)) = Sdecs_Valid(decs,sdecs)
       | Sig_Valid (decs, SIGNAT_FUNCTOR(v,s_arg,s_res,comp)) = 
	 (Sig_Valid(decs,s_arg) andalso 
	  Sig_Valid(add_dec_mod(decs,(v,s_arg)),s_res))
     (* Rules 13 - 15 *)
     and Dec_IsSub (decs,d1,d2) = 
	 let val temp = 
	     (case (d1,d2) of
		  (DEC_MOD(v1,s1),DEC_MOD(v2,s2)) => 
		      eq_var(v1,v2) andalso Sig_IsSub(decs,s1,s2)
		| (DEC_CON(v1,k1,SOME c1),DEC_CON(v2,k2,NONE)) => 
		      eq_var(v1,v2) andalso eq_kind(k1,k2) 
		      andalso eq_kind(k1,GetConKind(c1,decs))
		| _ => false)
	 in temp orelse (eq_dec(d1,d2,decs))
	 end

     (* Rules 99 - 100 *)
     and Sdecs_IsSub (decs,sdecs1,sdecs2) =
	 let 
	     fun help subster (v1,v2,sdecs) =
		 (case (subster(SIGNAT_STRUCTURE sdecs,[(v1,v2)])) of
		      SIGNAT_STRUCTURE sdecs' => sdecs'
		    | _ => error "Sdecs_IsSub subst failed")
	     fun match_var [] [] = []
	       | match_var (SDEC(l1,dec1)::rest1) (SDEC(l2,dec2)::rest2) = 
		 (case (dec1,dec2) of
		      (DEC_MOD(v1,s1),DEC_MOD(v2,s2)) =>
			  SDEC(l2,(DEC_MOD(v1,s2)))::
			  (match_var rest1 (help sig_subst_modvar (v1,MOD_VAR v2,rest2)))
		    | (DEC_EXP(v1,c1),DEC_EXP(v2,c2)) =>
			  SDEC(l2,(DEC_EXP(v1,c2)))
			  ::(match_var rest1 (help sig_subst_expvar (v1,VAR v2,rest2)))
		    | (DEC_CON(v1,k1,c1),DEC_CON(v2,k2,c2)) =>
			  SDEC(l2,(DEC_CON(v1,k2,c2)))
			  ::(match_var rest1 (help sig_subst_convar (v1,CON_VAR v2,rest2)))
		    | _ => SDEC(l2,dec2)::(match_var rest1 rest2))
	     val sdecs2' = match_var sdecs1 sdecs2
	     fun loop decs [] [] = true
	       | loop decs (SDEC(l1,dec1)::rest1) (SDEC(l2,dec2)::rest2) = 
		 (eq_label(l1,l2) andalso Dec_IsSub(decs,dec1,dec2)
		  andalso  Sdecs_IsSub(add_dec_dec(decs,dec1),rest1, rest2))
	       | loop decs _ _ = false
	 in loop decs sdecs1 sdecs2'
	 end

     (* Rules 109 - 112 *)
     and Sig_IsSub' (decs, SIGNAT_STRUCTURE sdecs1, SIGNAT_STRUCTURE sdecs2) = 
	 Sdecs_IsSub(decs,sdecs1,sdecs2)
       | Sig_IsSub' (decs, SIGNAT_DATATYPE (dt1,tb1,sdecs1), 
		     SIGNAT_DATATYPE(dt2,tb2,sdecs2)) = Sdecs_IsSub(decs,sdecs1,sdecs2)
       | Sig_IsSub' (decs, SIGNAT_FUNCTOR(v1,s1_arg,s1_res,comp1), 
		     SIGNAT_FUNCTOR(v2,s2_arg,s2_res,comp2)) = 
	 ((eq_comp(comp1,comp2,true)) andalso 
	  Sig_IsSub(decs,s2_arg,s1_arg) andalso 
	  let val s1_res' = if (eq_var(v1,v2)) then s1_res
			    else sig_subst_modvar(s1_res,[(v1,MOD_VAR v2)])
	  in Sig_IsSub(add_dec_mod(decs,(v2,s2_arg)), s1_res', s2_res)
	  end)
       | Sig_IsSub' _ = false
     and Sig_IsSub (decs,s1,s2) = 
	 let val _ = debugdo (fn () => (print "Sig_issub with s1 = \n";
					pp_signat s1; print "\n\nand s2 = ";
					pp_signat s2; print "\n\n\n"))
	 in Sig_IsSub'(decs,s1,s2)
	 end

     fun context2decs (CONTEXT c) : decs = 
	let
	    fun loop [] = []
	      | loop ((CONTEXT_INLINE (_,v,inline))::rest) = 
		      (case inline of
			   INLINE_MODSIG(m,s) => (DEC_MOD(v,s))::loop rest
			 | INLINE_EXPCON(e,c) => (DEC_EXP(v,c))::loop rest
			 | INLINE_CONKIND(c,k) => (DEC_CON(v,k,SOME c))::loop rest
			 | _ => loop rest)
	      | loop (CONTEXT_SDEC(SDEC(_,dec))::rest) = dec :: (loop rest)
	      | loop ((CONTEXT_SIGNAT _ | CONTEXT_SCOPED_TYVAR _)::rest) = loop rest
	in loop c
	end

    (* --------- type unifiers with error messages ------------------------ *)
    type printer = Il.con -> unit
    fun con_unify(decs : decs, msg : string, 
		  (str1 : string, con1 : con),
		  (str2 : string, con2 : con), thunk) =
	if (eq_con(con1,con2,decs)) then ()
	else (print "\n\n>>>In "; print msg;
	      print ", could not unify "; print str1; print ":\n"; pp_con con1;
	      print "\n>>>with "; print str2; print ":\n"; pp_con con2; print "\n\n\n";
	      thunk();
	      error ("In " ^ msg ^ ", could not unify " ^  str1 ^ " with " ^ str2))
    fun con_unify'(context : context, msg, arg1, arg2, thunk) =
	con_unify(context2decs context, msg, arg1, arg2, thunk)
    val eq_con = fn (d,c1,c2) => eq_con(c1,c2,d)
    val eq_con' = fn (context,c1,c2) => eq_con(context2decs context,c1,c2)
    val sub_con = fn (d,c1,c2) => sub_con(c1,c2,d)
    val sub_con' = fn (context,c1,c2) => sub_con(context2decs context,c1,c2)
    val soft_eq_con = fn (d,c1,c2) => soft_eq_con(c1,c2,d)
    val soft_eq_con' = fn (context,c1,c2) => soft_eq_con(context2decs context,c1,c2)

    val GetExpCon = fn (d,e) => GetExpCon(e,d)
    val GetConKind = fn (d,c) => GetConKind(c,d)
    val GetModSig = fn (d,m) => GetModSig(m,d)
    val Module_IsValuable = fn (d,m) => Module_IsValuable m d
    val Exp_IsValuable = fn (d,e) => Exp_IsValuable e d
    val Bnds_IsValuable = fn (d,bs) => Bnds_IsValuable bs d
    val Sbnds_IsValuable = fn (d,ss) => Sbnds_IsValuable ss d

  end
