(* Static semantics *)
functor IlStatic(structure Il : IL
		 structure Ppil : PPIL
		 structure IlUtil : ILUTIL
		 sharing Ppil.Il = IlUtil.Il = Il)
  : ILSTATIC = 
  struct

    structure Il = Il
    open Il Ppil IlUtil
    open Util Name Prim Tyvar

    val error = error "ilstatic.sml"
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
   exception NOTFOUND of string
   local
     fun help s match fetch ([]) = raise (NOTFOUND s)
       | help s match fetch (a::rest) = if (match a) then fetch a else help s match fetch rest
   in
     fun DecVarLookup (var, decs) = help ("Var " ^ var2string var)
                                         (fn (DEC_EXP (v,_)) => eq_var(var,v) | _ => false)
                                         (fn (DEC_EXP (_,con)) => con | _ => error "DecVarLookup") decs
     fun DecExnLookup (tag, decs) = help ("Exn " ^ tag2string tag)
                                          (fn (DEC_EXCEPTION (n,_)) => eq_tag(tag,n) | _ => false)
                                          (fn (DEC_EXCEPTION (_,con)) => con | _ => error "DecExnLookup") decs
     fun DecConvarLookup (var, decs) = help ("Convar " ^ var2string var)
                                            (fn (DEC_CON (v,_,SOME _)) => eq_var(var,v) | _ => false)
                                            (fn (DEC_CON (_,_,SOME con)) => con | _ => 
					        error "DecConvarLookup") decs
     fun DecKindLookup (var, decs) = help ("Kind "^ var2string var)
                                          (fn (DEC_CON (v,_,_)) => eq_var(var,v) | _ => false)
                                          (fn (DEC_CON (_,k,_)) => k | _ => error "DecKindLookup") decs
     fun DecSignatLookup (var, decs) = help ("Signat " ^ var2string var)
                                            (fn (DEC_MOD (v,_)) => eq_var(var,v) | _ => false)
                                            (fn (DEC_MOD(_,s)) => s | _ => error "DecSignatLookup") decs
     fun RdecLookup (label,rdecs) = help ("rdec " ^ label2string label)
                                         (fn (RDEC (l,_)) => eq_label(label,l))
					 (fn (RDEC (_,con)) => con) rdecs
     (* if the module argument is present, 
       then the component returned, if it is a DEC_CON, will have
	 all occurrences of variables that occur as part of that
	 signature replaced by a module projection *)
     fun SignatLookup (label,signat,mopt : mod option) : sdec option = 
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
      | RECORD rbnds => foldr (fn (a,b) => a andalso b) true (map (fn RBND(l,e) => Exp_IsSyntacticValue e) rbnds)
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
   fun eq_comp (comp1,comp2) = (case (oneshot_deref comp1,oneshot_deref comp2) of
				  (SOME x, SOME y) => x = y
				| (SOME x, NONE) => (oneshot_set(comp2,x); true)
				| (NONE, SOME x) => (oneshot_set(comp1,x); true)
				| (NONE, NONE) => ((eq_oneshot(comp1,comp2)) orelse
						   (oneshot_set(comp1,PARTIAL); 
						    oneshot_set(comp2,PARTIAL); true)))
   fun comp_unify arg = if (eq_comp arg) then () else error "comp_unify failed"

   (* ------------------------------------------------------------ 
      type equality:
	  First, normalize argument types:
         (1) a module projection whose type is known to the known type
         (2) beta-reducing constructor function application
	  (3) overloaded type expression to the carried type
        Then, performs structural equality except when a type variable.
	  When a type variable is encountered, the unifier routine
	     is called with the type variable and the given type.
	     The unifier may be side-effecting or not.
    -------------------------------------------------------------- *)
   fun unify_maker fetch set (constrained,tyvar,c,decs) = 
     let 
       val self = unify_maker fetch set
       val origv = tyvar_getvar tyvar
     in (case (fetch tyvar) of
	   NONE => (case c of
		      CON_TYVAR tv =>
			eq_var(tyvar_getvar tv,origv) orelse
			(case (fetch tv) of
			   SOME c' => self(constrained,tyvar,c',decs)
			 | NONE => (set(constrained,tyvar,c); true))
		    | _ => (not (con_occurs(c,origv))) andalso (set(constrained,tyvar,c); true))
	 | (SOME c') => meta_eq_con self constrained (c',c,decs))
     end

   and hard_unifier arg =
     let 
       fun hard_fetch tv = tyvar_deref tv
       fun hard_set (constr,tv,c) = (debugdo (fn () => (print "now hard-setting ";
							pp_var (tyvar_getvar tv); print " to ";
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
       fun soft_fetch tyvar = let val v = tyvar_getvar tyvar
				  fun loop [] = NONE
				    | loop ((tv,c)::rest) = if (eq_var(v,tyvar_getvar tv))
							      then SOME c
							    else NONE
			      in (case (tyvar_deref tyvar) of
				    SOME c' => SOME c'
				  | NONE => loop (!table))
			      end
       fun soft_set (constr,tv,c) = let val _ = debugdo (fn () => (print "now soft-setting ";
								   pp_var (tyvar_getvar tv); print " to ";
								   pp_con c; print "\n"))
					val _ = if constr then constr_con := c::(!constr_con) else ()
					val _ = if (tyvar_is_use_equal tv) 
						  then useeq_con := c::(!useeq_con) else ()
				    in table := (tv,c)::(!table)
				    end
       fun soft_unify arg = unify_maker soft_fetch soft_set arg
     in (soft_unify,table,constr_con,useeq_con)
     end
   and eq_con (con1,con2,decs) = meta_eq_con hard_unifier false (con1,con2,decs)
   and soft_eq_con (con1,con2,decs) = 
     let val (soft_unify,table,constr_con,useeq_con) = soft_unifier()
     in  if (meta_eq_con soft_unify false (con1,con2,decs))
	   then (app tyvar_set (!table); 
		 app (fn c => con_constrain c) (!constr_con);
		 app (fn c => con_useeq c) (!useeq_con);
		 true)
	 else false
     end
   and meta_eq_con unifier constrained (con1,con2,decs) = 
     let 
       val _ = debugdo (fn () => (print "\nUnifying: "; pp_con con1;
				  print "\nwith:     "; pp_con con2;
				  print "\n") )
       fun normalize c = 
	 (case c of
	    CON_OVAR ocon => (true, ocon_deref ocon)
	  | CON_APP(c1,c2) => let val (f1,c1') = normalize c1
				  val (f2,c2') = normalize c2
			      in (f1 orelse f2,
				  case (c1,c2) of
				    (CON_FUN(vars,cons),CON_TUPLE_INJECT(arg_cons)) => ConApply(c1,c2)
				  | _ => CON_APP(c1',c2'))
			      end
	  | (CON_MODULE_PROJECT (m,l)) =>
	      (false, let 
			val s = GetModSig(m,decs)
			fun loop [] = c
			  | loop ((SDEC(curl,DEC_CON(_,_,SOME curc)))::rest) = 
			  if eq_label(curl,l)
			    then curc 
			  else loop rest
			  | loop (_::rest) = loop rest
		      in (case s of 
			    (SIGNAT_DATATYPE (_,_,sdecs)) => loop sdecs
			  | SIGNAT_STRUCTURE sdecs => loop sdecs
			  | _ => c)
		      end
		    handle NOTFOUND _ => c)
	  | _ => (false,c))


       val (isocon1, con1) = normalize con1
       val (isocon2, con2) = normalize con2
       val constrained = constrained orelse isocon1 orelse isocon2
       val self = meta_eq_con unifier constrained

       val res =  
	 (case (con1,con2) of
	    (CON_TYVAR tv1, CON_TYVAR tv2) => (eq_var(tyvar_getvar tv1, tyvar_getvar tv2) 
							orelse (unifier(constrained,tv1,con2,decs)))
	  | (CON_TYVAR tv1, _) => unifier(constrained,tv1,con2,decs)
	  | (_, CON_TYVAR tv2) => unifier(constrained,tv2,con1,decs)
	  | (CON_VAR v1, CON_VAR v2) => eq_var(v1,v2)
	  | (CON_APP(c1_a,c1_r), CON_APP(c2_a,c2_r)) => self(c1_a,c2_a,decs) 
	                                                andalso self(c1_r,c2_r,decs)
	  | (CON_MODULE_PROJECT (m1,l1), CON_MODULE_PROJECT (m2,l2)) => ((m1 = m2) andalso 
									 eq_label(l1,l2))
	  | (CON_INT, CON_INT) => true
	  | (CON_UINT, CON_UINT) => true
	  | (CON_FLOAT, CON_FLOAT) => true
	  | (CON_CHAR, CON_CHAR) => true
	  | (CON_ANY, CON_ANY) => true
	  | (CON_LIST c1, CON_LIST c2) => self(c1,c2,decs)
	  | (CON_ARRAY c1, CON_ARRAY c2) => self(c1,c2,decs)
	  | (CON_VECTOR c1, CON_VECTOR c2) => self(c1,c2,decs)
	  | (CON_REF c1, CON_REF c2) => self(c1,c2,decs)
	  | (CON_TAG c1, CON_TAG c2) => self(c1,c2,decs)
	  | (CON_ARROW (c1_a,c1_r,comp1), CON_ARROW(c2_a,c2_r,comp2)) => (self (c1_a,c2_a,decs)
									  andalso self(c1_r,c2_r,decs)
									  andalso eq_comp(comp1,comp2))
	  | (CON_MUPROJECT (i1,c1), CON_MUPROJECT(i2,c2)) => (i1=i2) andalso (self(c1,c2,decs))
	  | (CON_RECORD rdecs1, CON_RECORD rdecs2) => let val list1 = map (fn (RDEC x) => x) rdecs1
							  val list1_sorted = sort_labelpair list1
							  val list2 = map (fn (RDEC x) => x) rdecs2
							  val list2_sorted = sort_labelpair list2
							  fun help ((l1,c1),(l2,c2)) = eq_label(l1,l2) 
							    andalso self(c1,c2,decs)
						      in
							eq_list(help,list1,list2)
						      end
	  | (CON_FUN (vs1,c1), CON_FUN(vs2,c2)) => eq_list(eq_var,vs1,vs2) andalso self(c1,c2,decs)
	  | (CON_SUM (i1,cs1), CON_SUM (i2,cs2)) => (i1=i2) andalso
						      eq_list (fn (a,b) => self(a,b,decs), cs1, cs2)
	  | (CON_TUPLE_INJECT cs1, CON_TUPLE_INJECT cs2) => eq_list (fn (a,b) => self(a,b,decs), cs1, cs2)
	  | (CON_TUPLE_PROJECT (i1, c1), CON_TUPLE_PROJECT(i2,c2)) => (i1 =i2) andalso (self(c1,c2,decs))
	  | _ => false)
       val _ = debugdo (fn () => print (if res then "unified\n" else "NOT unified\n"))
     in res
     end


   and eq_dec (DEC_EXP (v1,c1), DEC_EXP(v2,c2), decs) = (eq_var(v1,v2)) andalso eq_con (c1,c2,decs)
     | eq_dec (DEC_MOD(v1,s1), DEC_MOD(v2,s2), decs) = (eq_var(v1,v2)) andalso eq_sig (decs,s1,s2)
     | eq_dec (DEC_CON (v1,k1,NONE), DEC_CON(v2,k2,NONE), decs) = (eq_var(v1,v2)) 
     | eq_dec (DEC_CON (v1,k1,SOME c1), DEC_CON(v2,k2,SOME c2), decs) = ((eq_var(v1,v2)) andalso 
									       eq_con (c1,c2,decs))
     | eq_dec (DEC_EXCEPTION (n1,c1), DEC_EXCEPTION(n2,c2), decs) = ((eq_tag(n1,n2)) andalso 
								     eq_con (c1,c2,decs))
     | eq_dec (DEC_FIXITY ft1, DEC_FIXITY ft2, _) = eq_list(fn ((l1,f1),(l2,f2)) => 
							     (eq_label(l1,l2) andalso f1=f2),ft1,ft2)
     | eq_dec _ = false
   and eq_sdec decs (SDEC(l1,d1),SDEC(l2,d2)) = eq_label(l1,l2) andalso (eq_dec(d1,d2,decs))
   and eq_sdecs (decs,sdecs1,sdecs2) = eq_list(eq_sdec decs, sdecs1, sdecs2)

   and eq_sig (decs,SIGNAT_STRUCTURE sdecs1, SIGNAT_STRUCTURE sdecs2) = eq_sdecs(decs,sdecs1,sdecs2)
     | eq_sig (decs,SIGNAT_DATATYPE (_,_,sdecs1), SIGNAT_DATATYPE (_,_,sdecs2)) = eq_sdecs(decs,sdecs1,sdecs2)
     | eq_sig (decs,SIGNAT_FUNCTOR (v1,s1_arg,s1_res,comp1), SIGNAT_FUNCTOR (v2,s2_arg,s2_res,comp2)) = 
         eq_comp(comp1,comp2) andalso (eq_var(v1,v2)) andalso (eq_sig(decs,s1_arg,s2_arg)) andalso
	 let val decs' = add_dec_mod(decs,(v1,s1_arg))
	 in  eq_sig (decs',s1_res,s2_res)
	 end
     | eq_sig _ = false



   and Exp_IsValuable exp decs = 
     (Exp_IsSyntacticValue exp) orelse
     (case exp of
	MODULE_PROJECT (m,l) => Module_IsValuable m decs
      | APP(e1,e2) => let val e1_con = GetExpCon(e1,decs)
			  val e1_con_istotal = (case e1_con of
						  CON_ARROW(_,_,comp) => eq_comp(comp,oneshot_init TOTAL)
						| _ => false)
		      in e1_con_istotal andalso (Exp_IsValuable e1 decs) andalso (Exp_IsValuable e2 decs)
		      end
     | RECORD rbnds => foldr (fn (a,b) => a andalso b) true (map (fn RBND(l,e) => Exp_IsValuable e decs) rbnds)
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
      (case bnd of
	 BND_EXP (v,e) => if (Exp_IsValuable e decs) 
			    then Bnds_IsValuable' rest (add_dec_exp(decs,(v,GetExpCon(e,decs))))
			  else NONE
       | BND_MOD (v,m) => if (Module_IsValuable m decs)
			    then Bnds_IsValuable' rest (add_dec_mod(decs,(v,GetModSig(m,decs)))) 
			  else NONE
       | BND_CON (v,c) => Bnds_IsValuable' rest (add_dec_con(decs,(v,GetConKind(c,decs),c)))
       | BND_FIXITY _ => Bnds_IsValuable' rest decs)

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
			       eq_comp(oneshot_init TOTAL,comp) andalso
			       (Module_IsValuable m1 decs) andalso (Module_IsValuable m2 decs)
			   | _ => false)
     | _ => false)
	
   and GetPrimCon (prim) : con = 
     let 
       fun help (arg,res) = CON_ARROW(arg,res,oneshot_init PARTIAL)
       fun help' (args,res) = help(con_tuple args,res)
     in
       (case prim of
	  PRIM0 p =>
	  (case p of
(*	     NILprim {instance} => CON_LIST instance *)
	    (SOFT_VTRAPprim _ | SOFT_ZTRAPprim _ | HARD_VTRAPprim _ | HARD_ZTRAPprim _) => con_unit)
	| PRIM1 p =>
	     (case p of
(*		NOTprim  => help(con_bool,con_bool) *)
	        MK_REFprim {instance} => help(instance,CON_REF instance)
	      | DEREFprim {instance} => help(CON_REF instance,instance)
(*	      | SIZEprim => help(con_string, CON_INT)
	      | CHRprim  => help(CON_INT, CON_CHAR)
	      | ORDprim  => help(CON_CHAR, CON_INT)
	      | EXPLODEprim => help(CON_VECTOR CON_CHAR, CON_LIST CON_CHAR)
	      | IMPLODEprim => help(CON_LIST CON_CHAR, CON_VECTOR CON_CHAR) *)
	      | (NEG_FLOATprim | ABS_FLOATprim 
		   (* | SQRTprim | SINprim | COSprim | ARCTANprim | EXPprim | LNprim *)
		    ) => help(CON_FLOAT,CON_FLOAT)
	      | (NOT_INTprim | NEG_INTprim | ABS_INTprim) => help(CON_INT, CON_INT)
	      | (NOT_UINTprim) => help(CON_UINT, CON_UINT)
	      | FLOAT2INTprim => help(CON_FLOAT, CON_INT)
	      | INT2FLOATprim => help(CON_INT, CON_FLOAT)
	      | INT2UINTprim => help(CON_INT, CON_UINT)
	      | UINT2INTprim => help(CON_UINT, CON_INT)
(*	      | ISNILprim {instance} => help(CON_LIST instance, con_bool)
	      | CARprim {instance} => help(CON_LIST instance, instance)
	      | CDRprim {instance} => help(CON_LIST instance, CON_LIST instance)
	      | OPEN_INprim => raise UNIMP
	      | OPEN_OUTprim => raise UNIMP
	      | INPUTprim => raise UNIMP
	      | LOOKAHEADprim => raise UNIMP
	      | CLOSE_INprim => raise UNIMP
	      | END_OF_STREAMprim => raise UNIMP
	      | CLOSE_OUTprim => raise UNIMP
	      | USEprim => raise UNIMP
	      | FLUSH_OUTprim => raise UNIMP *)
	      | LENGTH1prim {instance} => help(CON_ARRAY instance, CON_INT)
(*	      | LENGTH2prim {instance} => raise UNIMP *)
		    )

	 | PRIM2 p =>
	  (case p of
(*	     ANDprim  => help'([con_bool,con_bool],con_bool) *)
	     SETREFprim {instance} => help'([CON_REF instance,instance],con_unit)
(*	   | ORprim  => help'([con_bool,con_bool],con_bool) 
	   | EQ_BOOLprim  => help'([con_bool,con_bool],con_bool)
	   | XORprim  => help'([con_bool,con_bool],con_bool) *)
	   | EQ_REFprim {instance} => help'([CON_REF instance, CON_REF instance],con_bool)
(*	   | STRING_CONCATprim => help'([con_string,con_string],con_string) *)
	   | (EQ_CHARprim | NEQ_CHARprim) => help'([CON_CHAR,CON_CHAR],con_bool)
(*	   | (EQ_STRINGprim | NEQ_STRINGprim) => help'([con_string,con_string],con_bool) *)
	   | (PLUS_FLOATprim | MINUS_FLOATprim | MUL_FLOATprim | 
		DIV_FLOATprim) => help'([CON_FLOAT, CON_FLOAT], CON_FLOAT)
	   | (LESS_FLOATprim | GREATER_FLOATprim |
	       LESSEQ_FLOATprim | GREATEREQ_FLOATprim | 
	       EQ_FLOATprim | NEQ_FLOATprim)  => help'([CON_FLOAT, CON_FLOAT], con_bool)
	  | (PLUS_INTprim | MINUS_INTprim | MUL_INTprim | 
	       DIV_INTprim  | MOD_INTprim | 
	       QUOT_INTprim | REM_INTprim) => help'([CON_INT, CON_INT], CON_INT)
   	  | (LESS_INTprim | GREATER_INTprim |
	       LESSEQ_INTprim | GREATEREQ_INTprim | 
	       EQ_INTprim | NEQ_INTprim)  => help'([CON_INT, CON_INT], con_bool)
	  | (LSHIFT_INTprim | RSHIFT_INTprim | 
	       AND_INTprim | OR_INTprim) => help'([CON_INT, CON_INT], CON_INT)
	  | (PLUS_UINTprim | MINUS_UINTprim | MUL_UINTprim | 
	     DIV_UINTprim  | MOD_UINTprim) => help'([CON_UINT, CON_UINT], CON_UINT)
	  | (LESS_UINTprim | GREATER_UINTprim |
	       LESSEQ_UINTprim | GREATEREQ_UINTprim | 
	       EQ_UINTprim | NEQ_UINTprim)  => help'([CON_UINT, CON_UINT], con_bool)
	  | (LSHIFT_UINTprim | RSHIFT_UINTprim) => help'([CON_UINT, CON_INT], CON_UINT)
	  | (AND_UINTprim | OR_UINTprim) => help'([CON_UINT, CON_UINT], CON_UINT)
(*	  | CONSprim {instance} => help'([instance,CON_LIST instance],CON_LIST instance) *)

	  | ARRAY1prim  {instance} => help'([CON_INT, instance], CON_ARRAY instance)
	  | SUB1prim    {instance} => help'([CON_ARRAY instance, CON_INT], instance)
(*	  | OUTPUTprim => help'([CON_INT, con_string], con_unit) *)
		 )

	 | PRIM3 p =>
	  (case p of
	     UPDATE1prim {instance} => help'([CON_ARRAY instance, CON_INT, instance], con_unit)
(*	   | ARRAY2prim  {instance} => raise UNIMP
	   | SUB2prim    {instance} => raise UNIMP *) )

(*	 | PRIM4 p =>
	     (case p of
		UPDATE2prim {instance} => raise UNIMP) *)
	       )

     end

   and GetSconCon (scon) : con = 
     let fun binop c = CON_ARROW(con_tuple[c,c],con_bool,oneshot_init PARTIAL)
     in (case scon of
	   (INT _) => CON_INT
	 | (UINT _) => CON_UINT
	 | (STRING _) => CON_VECTOR CON_CHAR
	 | (BOOL _) => con_bool
	 | (FLOAT _) => CON_FLOAT
	 | (CHAR _) => CON_CHAR)
     end

  (* Rules 35 - 48 *)
   and GetConKind (arg : con, decs) : kind = 
     let val con = arg (* Normalize(arg,decs) *)
     in case con of
       (CON_TYVAR tv) => (DecKindLookup(tyvar_getvar tv,decs) : kind)
     | (CON_VAR v) => (DecKindLookup(v,decs) : kind)
     | (CON_OVAR ocon) => raise UNIMP
     | (CON_INT) => KIND_TUPLE 1
     | (CON_FLOAT) => KIND_TUPLE 1
     | (CON_UINT) => KIND_TUPLE 1
     | (CON_CHAR) => KIND_TUPLE 1
     | (CON_ANY) => KIND_TUPLE 1
     | (CON_REF _) => KIND_TUPLE 1
     | (CON_LIST _) => KIND_TUPLE 1
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
     | (CON_FUN (vs,c)) => (case GetConKind(c,decs) of
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
     case earg of
       SCON scon => GetSconCon scon
     | OVEREXP (con,_,_) => con
     | PRIM p => GetPrimCon p
     | (VAR v) => DecVarLookup(v,decs)
     | SEQ elist => GetExpCon(List.last elist, decs)
     | LOC (c,e) => let val con = GetExpCon(!e,decs)
		    in if eq_con(c,CON_REF con,decs)
			then c
		       else error "corrupt loc cell"
		    end
     | (APP (e1,e2)) => let val con1 = Normalize(GetExpCon(e1,decs),decs)
			    val con2 = GetExpCon(e2,decs)
			in  (case con1 of
			       CON_ARROW (arg_con,res_con,discard_complete) => 
				 if eq_con(arg_con,con2,decs) then res_con
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
				 extend rest (add_dec_exp(ds,(v',CON_ARROW(c,c',oneshot_init PARTIAL))))
			       val decs' = extend fbnds decs
			       fun test [] = ()
				 | test ((FBND(v',v,c,c',e))::rest) = 
				 let val bodyc = GetExpCon(e,add_dec_exp(decs',(v,c)))
				 in if (eq_con(c',bodyc,decs'))
				      then test rest 
				    else (print "GetExpCon failed on: ";
					  pp_exp earg; print "\nc' = "; pp_con c';
					  print "\nbodyc = "; pp_con bodyc;
					  print "\nwith context";
					  pp_decs decs'; print "\n";
					  error "FIX(rule 74): premises fails: ill-typed")
				 end
			       val _ = test fbnds
			       val decs'' = add_dec_exp(decs',(v,c))
			     in CON_ARROW(c,GetExpCon(e,decs''),oneshot_init PARTIAL)
			     end
			end
     | (RECORD (rbnds)) => let val rdecs = (map (fn r => GetRbndRdec r decs) rbnds)
			       val temp1 = map (fn RDEC x => x) rdecs
			       val temp2 = sort_labelpair temp1
			       val rdecs' = map RDEC temp2
			   in CON_RECORD rdecs'
			   end
     | (RECORD_PROJECT (exp,l,c)) => 
			   let val con = GetExpCon(exp,decs)
			   in (case con of 
				   (CON_RECORD rdecs) => RdecLookup(l,rdecs)
				 | _ => error "Record Projection on exp not of type CON_RECORD")
			   end
     | (SUM_TAIL (c,e)) => let val con = GetExpCon(e,decs)
			   in if (eq_con(c,con,decs)) 
				  then (case con of
					    CON_SUM(SOME i,cons) => List.nth(cons,i)
					  | _ => error "adornment of SUM_TAIL not a CON_SUM_I")
			      else error "SUM_TAIL: adornment wrong"
			   end
     | (HANDLE (body,handler)) => let val bcon = GetExpCon(body,decs)
				     val hcon = GetExpCon(handler,decs)
				 in (case hcon of
				       CON_ARROW (CON_ANY,res_con,discard_complete) =>
					 if (eq_con(res_con,bcon,decs)) then bcon 
					 else error "Type mismatch between handler and body of HANDLE"
				     | _ => error "Handler not of type Any -> con")
				 end
     | (RAISE e) => let val econ = GetExpCon(e,decs)
		    in (case econ of
			  CON_ANY => fresh_con()
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
     | (REF (c,e)) => CON_REF (Normalize(c,decs))
     | (GET (c,e)) => (case GetExpCon(e,decs) of
			CON_REF c => Normalize(c,decs)
		      | _ => error "GET given expression whose normalized type is not CON_REF(con)")
     | (SET (c,e1,e2)) => CON_RECORD []  (* unit *)
     | (ROLL (c,e) | UNROLL(c,e)) => 
       let val cNorm = Normalize(c,decs)
	   val cNorm_kind = GetConKind(cNorm,decs)
	   val econ = GetExpCon(e,decs)
       in (case (cNorm,cNorm_kind) of
	       (CON_MUPROJECT(i,cInner),KIND_TUPLE 1) => 
	         let val cInner_norm = Normalize(cInner,decs)
		 in  (case GetConKind(cInner_norm,decs) of
			  KIND_ARROW(n,n') =>
			      (if ((n = n') andalso (1 <= i) andalso (i <= n))
				   then 
				       let 
					   val temp = (fn j => CON_MUPROJECT(j,cInner_norm))
					   val contemp = CON_TUPLE_INJECT(map0count temp n)
					   val con2 = CON_TUPLE_PROJECT(i,CON_APP(cInner_norm,contemp))
				       in
					   case earg of
					       (ROLL _) => 
						   if (eq_con(econ,con2,decs))
						       then cNorm
						   else error "ROLL: expression type does not match decoration"
					     | (UNROLL _) => 
						       if (eq_con(econ,cNorm,decs))
							   then con2
						       else error "UNROLL: expression type does not match decoration"
					     | _ => error "IMPOSSIBLE: what happened to (UN)ROLL"
				       end
			       else error "projected decoration has the wrong KIND_ARROW")
			| _ => error "projected decoration has the wrong kind")
		 end
	     | _ => error "decoration of ROLL not a projection")
       end
    | (INJ (cons,i,e)) => let val econ = GetExpCon(e,decs)
			       val clist = map (fn c => Normalize(c,decs)) cons
			       val n = length clist
			   in if (1 <= i andalso i <= n andalso 
				  (eq_con(econ, List.nth(clist,i-1), decs)))
				then CON_SUM(NONE,clist)
			      else error "INJ: expression con and decoration con do not match"
			   end
     | (PROJ (cons,i,e)) => let val econ = GetExpCon(e,decs)
				val clist = map (fn c => Normalize(c,decs)) cons
			    in (case econ of
				    CON_SUM(_,clist2) =>
					let val n = length clist
					in if (1 <= i andalso i <=n andalso 
					       eq_list (fn (x,y) => eq_con(x,y,decs), clist, clist2))
					       then List.nth(clist,i-1)
					   else error "PROJ: index out of range"
					end
				  | _ =>  error "PROJ: expression not of con CON_SUM")
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
			  val consNorm = map (fn c => Normalize(c,decs)) cons
			  val eargCon = GetExpCon(earg,decs)
			  val sumcon = CON_SUM (NONE,consNorm)
			  val rescon = fresh_con()
			in if (eq_con(eargCon,sumcon,decs))
			     then let 
				    fun loop [] [] = (case edef of 
							NONE => rescon
						      | SOME edef => let val defcon = GetExpCon(edef,decs)
								     in if (eq_con(CON_ARROW(sumcon,rescon,
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
				  in loop earms consNorm
				  end
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
     | (SEAL (e,c)) => Normalize(c,decs)


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
		(SOME v, SOME m) => (SDEC(l,d)) :: (loop ((v,CON_MODULE_PROJECT(m,l))::table) rest)
	      | _ => (SDEC(l,d)) :: (loop table rest))
	 in (case dec of
	       DEC_EXP(v,c) => self (DEC_EXP(v,con_subst_var(c,table))) NONE
	     | DEC_MOD(v,s) => self (DEC_MOD(v,NormalizeSig'(table,
							     (case mopt of
								SOME m => SOME (MOD_PROJECT(m,l))
							      | NONE => NONE), s))) NONE
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
     case module of
       (MOD_VAR v) => NormalizeSig(MOD_VAR v, DecSignatLookup(v,decs))
     | MOD_STRUCTURE (sbnds) => let fun loop [] acc decs = rev acc
				      | loop (sb::sbs) acc decs = let val sdec = GetSbndSdec(decs,sb)
								      val SDEC(_,dec) = sdec
								  in loop sbs (sdec::acc) (decs@[dec])
								  end
				in SIGNAT_STRUCTURE ((loop sbnds [] decs) : sdec list)
				end
(*     | MOD_DATATYPE  (dts,tbs,sbnds) => SIGNAT_DATATYPE(dts,tbs,map (fn s => GetSbndSdec(decs,s)) sbnds) *)
     | MOD_FUNCTOR (v,s,m) => let val decs' = add_dec_mod(decs,(v,s))
			      val signat = GetModSig(m,decs')
			  in  SIGNAT_FUNCTOR(v,s,signat, 
					     oneshot_init 
					     (if (Module_IsValuable m decs) then TOTAL else PARTIAL))
			  end
     | MOD_APP (a,b) => let val _ = print "\n\nMOD_APP case in GetModSig\n"
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
     | MOD_PROJECT (m,l) => let 
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
     | MOD_SEAL (m,s) => s

 (* -------- performs CON application and normalizes module projections ----- *)
   and Normalize (arg,decs) = arg
(*     (case arg of
	(CON_INT | CON_FLOAT | con_bool | CON_UINT | CON_CHAR | CON_ANY) => arg
      | (CON_REF c)               => CON_REF (Normalize(c,decs))
      | (CON_LIST c)              => CON_LIST (Normalize(c,decs))
      | (CON_ARRAY c)             => CON_ARRAY (Normalize(c,decs))
      | (CON_VECTOR c)            => CON_VECTOR (Normalize(c,decs))
      | (CON_TAG c)               => CON_TAG (Normalize(c,decs))
      | (CON_ARROW (c1,c2,comp))  => CON_ARROW (Normalize(c1,decs),Normalize(c2,decs),comp)
      | (CON_MUPROJECT (i,c))       => CON_MUPROJECT (i, Normalize(c,decs))
      | (CON_UNROLL c)            => CON_UNROLL (Normalize(c,decs))
      | (CON_RECORD rdecs)        => CON_RECORD (map (fn RDEC(l,c) => RDEC(l,Normalize(c,decs))) rdecs)
      | (CON_FUN (vs,c))          => CON_FUN (vs,Normalize(c,decs))
      | (CON_SUM cs)              => CON_SUM (map (fn c => Normalize(c,decs)) cs)
      | (CON_TUPLE_INJECT cs)     => CON_TUPLE_INJECT (map (fn c => Normalize(c,decs)) cs)
      | (CON_TUPLE_PROJECT (i,c)) => CON_TUPLE_PROJECT (i, Normalize(c,decs))
      | (CON_MODULE_PROJECT (m,l))=> (let val _ = print "normalize about to call getmodsig\n"
					  val signat = GetModSig(m,decs)
					  val _ = (print "normalize got back sig:\n";
						   pp_signat signat; print "\n")
				     in  case SignatLookup(l,signat,m) of
				       NONE => error "CON_MOD_PROJECT failed to find label"
     | (SOME (SDEC(_,DEC_CON(v,k,SOME con)))) => con
				     | (SOME _) => error "CON_MOD_PROJECT found label not DEC_CON"
				     end)
     | (CON_VAR v) => CON_VAR v
      | (CON_TYVAR (v,co)) => (case oneshot_deref co of
			      SOME c => Normalize(c,decs)
			    | NONE => (Normalize(DecConvarLookup(v,decs),decs) handle NOTFOUND _ => arg))
      | (CON_APP(c1 as CON_FUN(vars,cons),c2 as CON_TUPLE_INJECT arg_cons)) => ConApply(c1,c2)
      | (CON_APP (a,b))           => arg)
*)

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
	    | DEC_EXCEPTION(name,CON_TAG c) => (name_notin name) andalso (GetConKind(c,decs) = (KIND_TUPLE 1))
	    | _ => false)
       end
					       
     (* Rules 97 - 98 *)	
     and Sdecs_Valid (decs, []) = Decs_Valid decs
       | Sdecs_Valid (decs, (SDEC(label,dec)::rest)) = (Dec_Valid(decs,dec) andalso 
							Sdecs_Valid(add_dec_dec(decs,dec),rest) andalso 
							(not (List.exists (fn l => eq_label(label,l)) 
							      (Sdecs_Domain rest))))

     (* Rules 106 - 108 *)
     and Sig_Valid (decs, SIGNAT_STRUCTURE sdecs) = Sdecs_Valid(decs,sdecs)
       | Sig_Valid (decs, SIGNAT_DATATYPE (_,_,sdecs)) = Sdecs_Valid(decs,sdecs)
       | Sig_Valid (decs, SIGNAT_FUNCTOR(v,s_arg,s_res,comp)) = (Sig_Valid(decs,s_arg) andalso 
							     Sig_Valid(add_dec_mod(decs,(v,s_arg)),s_res))
     (* Rules 13 - 15 *)
     and Dec_IsSub (decs,d1,d2) = 
       ((case (d1,d2) of
	   (DEC_MOD(v1,s1),DEC_MOD(v2,s2)) => eq_var(v1,v2) andalso Sig_IsSub(decs,s1,s2)
	 | (DEC_CON(v1,k1,SOME c1),DEC_CON(v2,k2,NONE)) => (eq_var(v1,v2) andalso eq_kind(k1,k2)
								  andalso eq_kind(k1,GetConKind(c1,decs)))
	 | _ => false) orelse
	   (eq_dec(d1,d2,decs)))

     (* Rules 99 - 100 *)
     and Sdecs_IsSub (decs,[],[]) = true
       | Sdecs_IsSub (decs,SDEC(l1,dec1)::rest1,SDEC(l2,dec2)::rest2) = (eq_label(l1,l2) andalso Dec_IsSub(decs,dec1,dec2)
									 andalso 
									 Sdecs_IsSub(add_dec_dec(decs,dec1), rest1, rest2))
       | Sdecs_IsSub _ = false
     (* Rules 109 - 112 *)
     and Sig_IsSub' (decs, SIGNAT_STRUCTURE sdecs1, SIGNAT_STRUCTURE sdecs2) = Sdecs_IsSub(decs,sdecs1,sdecs2)
       | Sig_IsSub' (decs, SIGNAT_DATATYPE (dt1,tb1,sdecs1), 
		     SIGNAT_DATATYPE(dt2,tb2,sdecs2)) = Sdecs_IsSub(decs,sdecs1,sdecs2)
       | Sig_IsSub' (decs, SIGNAT_FUNCTOR(v1,s1_arg,s1_res,comp1), SIGNAT_FUNCTOR(v2,s2_arg,s2_res,comp2)) = 
                   ((eq_comp(comp1,comp2) orelse (oneshot_deref comp1) = (SOME TOTAL)) andalso 
		    eq_var(v1,v2) andalso 
		    Sig_IsSub(decs,s2_arg,s1_arg) andalso 
		    Sig_IsSub(add_dec_mod(decs,(v1,s2_arg)), s1_res, s2_res))
       | Sig_IsSub' _ = false
     and Sig_IsSub (decs,s1,s2) = 
       let val _ = debugdo (fn () => (print "Sig_issub with s1 = \n";
				      pp_signat s1; print "\n\nand s2 = ";
				      pp_signat s2; print "\n\n\n"))
       in Sig_IsSub'(decs,s1,s2)
       end

     fun context2decs (CONTEXT c) : decs = 
       let fun loop [] = []
	     | loop ((CONTEXT_INLINE _)::rest) = loop rest
	     | loop ((CONTEXT_VAR (_,v,c))::rest) = DEC_EXP(v,c) :: (loop rest)
	     | loop ((CONTEXT_CONVAR (_,v,k,co))::rest) = DEC_CON(v,k,co) :: (loop rest)
	     | loop ((CONTEXT_MODULE (_,v,s))::rest) = DEC_MOD(v,s) :: (loop rest)
	     | loop ((CONTEXT_SIGNAT _ | CONTEXT_SCOPED_TYVAR _ | CONTEXT_FIXITY _)::rest) = loop rest
       in loop c
       end

    (* --------- type unifiers with error messages ------------------------ *)
    fun con_unify(context : context, msg : string, 
		  con1 : con, str1 : string, 
		  con2 : con, str2 : string) = 
      if (eq_con(con1,con2,context2decs context)) then ()
      else (print "\n\nIn "; print msg;
	    print ", could not unify "; print str1; print ":\n"; pp_con con1;
	    print "\nwith "; print str2; print ":\n"; pp_con con2; print "\n\n\n";
	    error ("In " ^ msg ^ ", could not unify " ^  str1 ^ "with " ^ str2))
    fun con_unify'(context : context, msg : string, 
		   con1 : con, str1 : string, p1 : unit -> 'a, 
		   con2 : con, str2 : string, p2 : unit -> 'a) = 
      if (eq_con(con1,con2,context2decs context)) then ()
      else (print "\n\nIn "; print msg;
	    print ", could not unify:\n"; p1(); print "\n-- of type "; print str1; 
	    print ":\n"; pp_con con1;
	    print "\n\n--with:\n"; p2(); print "\n-- of type "; 
	    print str2; print ":\n"; pp_con con2; print "\n\n\n";
	    error ("In " ^ msg ^ ", could not unify."))

    val GetExpCon = fn (d,e) => GetExpCon(e,d)
    val GetConKind = fn (d,c) => GetConKind(c,d)
    val GetModSig = fn (d,m) => GetModSig(m,d)
    val Module_IsValuable = fn (d,m) => Module_IsValuable m d
    val Exp_IsValuable = fn (d,e) => Exp_IsValuable e d
    val Bnds_IsValuable = fn (d,bs) => Bnds_IsValuable bs d
    val Sbnds_IsValuable = fn (d,ss) => Sbnds_IsValuable ss d

  end