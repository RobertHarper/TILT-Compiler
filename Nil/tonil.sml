functor Tonil(structure Ilstatic : ILSTATIC
              structure Ilutil : ILUTIL
              structure Nilutil : NILUTIL
              structure Ilcontext : ILCONTEXT
              structure Ppil : PPIL
                 sharing Ilutil.Il = Ilstatic.Il = Ppil.Il = Ilcontext.Il
		 sharing Nilutil.Nil.Prim = Ilstatic.Il.Prim
             ) =
struct
   structure Il = Ilstatic.Il
   structure Nil = Nilutil.Nil

   open Nil

   fun error msg = Util.error "tonil.sml" msg

   exception Tonil_HashExn
   val vmap = Name.mk_var_hash_table (100, Tonil_HashExn) : (var, var*var) HashTable.hash_table

   fun add_to_vmap (v, v_c, v_r) = HashTable.insert vmap (v, (v_c, v_r))

   val lookupVmap = HashTable.find vmap

   fun idmap _ = NONE
       
   fun extendmap (map, key, value) key' =
       if (Name.eq_var (key, key')) then SOME value else map key'
       

      fun newSplit var = 
	  let
	      val var_name = Name.var2string var
	      val var_c = Name.fresh_named_var (var_name ^ "_c")
	      val var_r = Name.fresh_named_var (var_name ^ "_r")
	  in
	      add_to_vmap(var, var_c, var_r);
	      (var_c, var_r)
	  end

       fun splitFreshVar () = 
	   let
	       val var = Name.fresh_var ()
	       val (var_c, var_r) = newSplit var
	   in
	       (var, var_c, var_r)
	   end

       fun splitVar var =
	   (case (lookupVmap var) of
		NONE => newSplit var
	      | SOME (var_c, var_r) => (var_c, var_r))

       fun makeLabels n =
	   let
	       fun loop i = 
		   if (i > n) then 
		       nil
		   else
		       (Ilutil.generate_tuple_label n) :: (loop (i + 1))
	   in
	       loop 1
	   end

       fun makeVars n =
	   if (n <= 0) then
	       nil
	   else
	       Name.fresh_var() :: (makeVars (n-1))

   fun makeKndTuple 1 = Type_k
     | makeKndTuple n =
       let
	   fun makeFields i =
	       if (i <= n) then
		   ((Ilutil.generate_tuple_label i, Name.fresh_var()), Type_k)
                   :: makeFields (i+1)
	       else
		   nil
       in
	   Record_k (Util.list2sequence (makeFields n))
       end

   fun makeLetC nil body = body
     | makeLetC bnds body = Let_c (Sequential, bnds, body)

   val count = ref 0
   fun gms (args as (_, module)) = 
       (count := !count + 1;
	Ilstatic.GetModSig args)

   fun myzip (nil, nil) = nil
     | myzip (x::xs, y::ys) = (x,y) :: myzip (xs, ys)
     | myzip _ = error "(myzip)"

   fun myunzip lst = 
       let
	   fun loop nil xaccum yaccum = (List.rev xaccum, List.rev yaccum)
	     | loop ((x,y)::rest) xaccum yaccum = loop rest (x::xaccum) (y::yaccum)
       in
	   loop lst nil nil
       end

   fun lookupList eq lst k' =
       let
	   fun loop nil = NONE
	     | loop ((k,v)::xs) = if (eq (k,k')) then SOME v else loop xs
       in
	   loop lst
       end

   fun xeffect (Il.TOTAL) = Total
     | xeffect (Il.PARTIAL) = Partial

   fun xilprim (Prim.eq_uint intsize) = Prim.eq_int intsize
     | xilprim (Prim.neq_uint intsize) = Prim.neq_int intsize
     | xilprim (Prim.not_uint intsize) = Prim.not_int intsize
     | xilprim (Prim.and_uint intsize) = Prim.and_int intsize
     | xilprim (Prim.or_uint intsize) = Prim.or_int intsize
     | xilprim (Prim.lshift_uint intsize) = Prim.lshift_int intsize

   fun xoneshot oneshot = 
       (case (Util.oneshot_deref oneshot) of
	     NONE   => error "(xoneshot)  oneshot unset"
	  |  SOME x => x)

   fun xtyvar tyvar =
       (case (Il.Tyvar.tyvar_deref tyvar) of
	    NONE => error "(xtyvar)  tyvar unset"
          | SOME x => x)

   fun xovar ovar = xtyvar (Il.Tyvar.ocon_deref ovar)

   fun xmod decs (il_mod as (Il.MOD_VAR var')) = 
       let
	   val (var'_c, var'_r) = splitVar var'
       in
	   {rev_cbnds = [],
	    rev_ebnds = [],
            name_c = var'_c,
            name_r = var'_r,
	    il_signat = gms (decs, il_mod)}
       end

     | xmod decs (Il.MOD_APP(mod1, mod2)) =
       let
	   val (var, var_c, var_r) = splitFreshVar ()

	   val {rev_cbnds = rev_cbnds', 
		rev_ebnds = rev_ebnds', 
		name_c = name'_c,
                name_r = name'_r,
		il_signat = Il.SIGNAT_FUNCTOR(var',il_sig1',il_sig2',_)} = 
	       xmod decs mod1

	   val {rev_cbnds = rev_cbnds'', 
		rev_ebnds = rev_ebnds'', 
                name_c = name''_c,
                name_r = name''_r,
		il_signat = Il.SIGNAT_STRUCTURE(_,il_sdecs'')} = xmod decs mod2
       in
	   {rev_cbnds = (var_c, App_c(Var_c name'_c, [Var_c name''_c]))
	                 :: rev_cbnds' @ rev_cbnds'',
	    rev_ebnds = Exp_b(var_r, App_e(Var_e name'_r, 
					   [Var_c name''_c],
					   [Var_e name''_r]))
                         :: rev_ebnds' @ rev_ebnds'',
            name_c = var_c,
	    name_r = var_r,
	    il_signat = Ilutil.remove_modvar_signat(il_sig2', var', il_sdecs'')}
       end
   
     | xmod decs (Il.MOD_SEAL(module,_)) = xmod decs module
     
     | xmod decs (il_mod as (Il.MOD_PROJECT (module, lbl))) =
       let
	   val (var, var_c, var_r) = splitFreshVar ()
	   val {rev_cbnds, rev_ebnds, name_c, name_r, ...} = 
	       xmod decs module
	   val il_signat = gms (decs, il_mod)
       in
	   {rev_cbnds = (var_c, Proj_c(Var_c name_c, lbl)) :: rev_cbnds,
	    rev_ebnds = (Exp_b (var_r, Prim_e(NilPrimOp(select lbl), 
					      [], SOME [Var_e name_r])))
                          :: rev_ebnds,
            name_c = var_c,
	    name_r = var_r,
	    il_signat = il_signat}
       end

     | xmod decs (Il.MOD_FUNCTOR(var', il_signat', body)) =
       let
	   val (var, var_c, var_r) = splitFreshVar ()

	   (* Split the argument parameter *)
	   val (var'_c, var'_r) = splitVar var'
	   val (knd, con) = xsig decs (Var_c var'_c, il_signat')
     
           (* Split the functor body *)
           val d = Il.DEC_MOD(var', il_signat')
	   val decs' = Ilcontext.add_context_dec (decs, Ilstatic.SelfifyDec d)
	   val {rev_cbnds, 
		rev_ebnds, 
		name_c = name''_c,
		name_r = name''_r,
		il_signat = il_signat''} = xmod decs' body

	   val var_not_in_defn = Name.fresh_var ()
	   val (arrow, effect) = 
	       if (Ilstatic.Module_IsValuable (decs', body)) then
		   (Il.TOTAL, Total)
	       else 
		   (Il.PARTIAL, Partial)

	   val (knd'', con'') = xsig decs' (Var_c name''_c, il_signat'') 

           val cbnds = rev rev_cbnds
           val ebnds = rev rev_ebnds
       in
	   {rev_cbnds = [(var_c, Fun_c(Open, [(var'_c, knd)], 
					     makeLetC cbnds (Var_c name''_c)))],
            rev_ebnds = [Fixfun_b [(var_not_in_defn,
				   Function(Open, effect, Leaf,
					    [(var'_c, knd)],
					    [(var'_r, con)],
					    Let_e(Sequential,
						  (map Con_b cbnds) @ ebnds,
						  Var_e name''_r),
					    con''))]],
	    name_c = var_c,
	    name_r = var_r,
	    il_signat = Il.SIGNAT_FUNCTOR(var', il_signat', il_signat'',
					  Util.oneshot_init arrow)}
       end
   
     | xmod decs (Il.MOD_STRUCTURE sbnds) =
       let
	   val (var, var_c, var_r) = splitFreshVar ()
	   val {cbnds, crbnds, ebnds, erlabels, erfields, ercons,
		il_sdecs} = xsbnds decs sbnds
       in
	   {rev_cbnds = [(var_c, Crecord_c crbnds)] @ (rev cbnds),
	    rev_ebnds = [Exp_b (var_r, Prim_e (NilPrimOp (record erlabels),
					       ercons, SOME erfields))] @ 
	                (rev ebnds),
            name_c = var_c,
	    name_r = var_r,
	    il_signat = Il.SIGNAT_STRUCTURE (NONE, il_sdecs)} 
       end

   and xsbnds decs [] =  
       {cbnds = nil, crbnds = nil, 
	ebnds = nil, erlabels = nil, 
	erfields = nil, ercons = nil,
	il_sdecs = nil}

     | xsbnds decs (Il.SBND(lab, Il.BND_EXP(var, il_exp)) :: rest) = 
       let
	   val il_con = Ilstatic.GetExpCon (decs, il_exp)
	   val il_dec = Il.DEC_EXP(var, il_con)
	   val decs' = Ilcontext.add_context_exp'(decs, var, il_con)
	   val {cbnds, crbnds, ebnds, erlabels, erfields, ercons, il_sdecs} =
	       xsbnds decs' rest
	   val (exp, tipe) = xexp decs il_exp
       in
	   {cbnds = cbnds,
	    crbnds = crbnds,
	    ebnds = (Exp_b (var, exp)) :: ebnds,
	    erlabels = lab :: erlabels,
	    erfields = (Var_e var) :: erfields,
	    ercons = tipe :: ercons,
	    il_sdecs = Il.SDEC(lab, il_dec) :: il_sdecs}
       end

     | xsbnds decs (Il.SBND(lab, Il.BND_CON(var, il_con)) :: rest) = 
       let
           val il_knd = Ilstatic.GetConKind (decs, il_con)
	   val il_dec = Il.DEC_CON(var, il_knd, SOME il_con)
	   val decs' = 
	       Ilcontext.add_context_con'(decs, var, il_knd, SOME il_con)
	   val {cbnds, crbnds, ebnds, erlabels, erfields, ercons, il_sdecs} =
	       xsbnds decs' rest
           val (con,_) = xcon decs il_con
       in
	   {cbnds = (var, con) :: cbnds,
	    crbnds = (lab, Var_c var) :: crbnds,
	    ebnds = ebnds,
	    erlabels = erlabels, 
	    erfields = erfields,
	    ercons = ercons,
	    il_sdecs = Il.SDEC(lab, il_dec) :: il_sdecs}
       end

    | xsbnds decs (Il.SBND(lab, Il.BND_MOD(var', il_mod)) :: rest) =
      let
	  val {rev_cbnds, rev_ebnds, name_c, name_r, il_signat} = xmod decs il_mod
	  val il_dec = Il.DEC_MOD(var', il_signat)
	  val decs' = Ilcontext.add_context_dec(decs, Ilstatic.SelfifyDec il_dec)
	  val {cbnds, crbnds, ebnds, erlabels, erfields, ercons, il_sdecs} =
	      xsbnds decs' rest
	  
	  val (var'_c, var'_r) = splitVar var'
	  val (knd, con) = xsig decs (Var_c var'_c, il_signat)
      in
	  {cbnds = (rev rev_cbnds) @ ((var'_c, Var_c name_c) :: cbnds),
	   crbnds = (lab, Var_c var'_c) :: crbnds,
	   ebnds = (rev rev_ebnds) @ ((Exp_b (var'_r, Var_e name_r)) :: ebnds),
	   erlabels = lab :: erlabels,
	   erfields = (Var_e var'_r) :: erfields,
	   ercons = con :: ercons,
	   il_sdecs = Il.SDEC(lab, il_dec) :: il_sdecs}
      end

   and xflexinfo decs (ref (Il.INDIRECT_FLEXINFO f)) = xflexinfo decs f
     | xflexinfo decs (ref (Il.FLEXINFO(_,true, recs))) = 
       let
	   val (lbls, cons) = xrdecs decs recs
	   val con = Prim_c(Record_c lbls, cons)
       in
	   (con, Singleton_k (Word_k, con))
       end

   and xrdecs decs [] = ([], [])
     | xrdecs decs ((lab, il_con) :: rest) = 
       let
	   val (labs, cons) = xrdecs decs rest
	   val (con, _) = xcon decs il_con
       in
	   (lab :: labs, con :: cons)
       end

   and xcon decs (il_con as (Il.CON_VAR var)) = 
       let
	   val con = Var_c var
	   val il_kind = Ilstatic.GetConKind(decs, il_con)
	   val kind = xkind il_kind
       in
	   (con, Singleton_k (kind, con))
       end

     | xcon decs (Il.CON_TYVAR tv) = xcon decs (xtyvar tv)

     | xcon decs (Il.CON_OVAR ov) = xcon decs (xovar ov)

     | xcon decs (Il.CON_FLEXRECORD fr) = xflexinfo decs fr

     | xcon decs ((Il.CON_INT intsize) | (Il.CON_UINT intsize)) =
       let
	   val con = Prim_c (Int_c intsize, [])
       in
           (* XXX *)
	   (* BUG---SHOULD CALL ISWORD SINCE INTSIZE >= 64!  *)
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (Il.CON_FLOAT floatsize) = 
       let
	   val con = Prim_c (BoxFloat_c floatsize, [])
       in
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (Il.CON_ARRAY il_con) = 
       let
	   val (con', knd') = xcon decs il_con 
	   val con = Prim_c (Array_c, [con'])
       in
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (Il.CON_VECTOR il_con) = 
       let
	   val (con', knd') = xcon decs il_con 
	   val con = Prim_c (Vector_c, [con'])
       in
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (Il.CON_ANY) = 
       let
	   val con = Prim_c(Exn_c, [])
       in
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (Il.CON_REF il_con) = 
       let
	   val (con', knd') = xcon decs il_con
	   val con = Prim_c (Ref_c, [con'])
       in
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (Il.CON_TAG il_con) = 
       let
	   val (con', knd') = xcon decs il_con
	   val con = Prim_c (Exntag_c, [con'])
       in
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (Il.CON_ARROW (il_con1, il_con2, arr)) =
       let
	   val (con1, _) = xcon decs il_con1
           val (con2, _) = xcon decs il_con2
	   val eff = (case (xoneshot arr) of
			  Il.TOTAL => Total
			| Il.PARTIAL => Partial)
	   val con = Arrow_c(Open, (eff, [], [con1], con2))
       in
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (il_con as Il.CON_APP (il_con1, il_con2)) = 
       let
	   val (con1, _) = xcon decs il_con1
           val (con2, _) = xcon decs il_con2
	   val con = App_c(con1, [con2])
	   val il_knd = Ilstatic.GetConKind (decs, il_con)
	   val knd = xkind il_knd
       in
	   (con, Singleton_k(knd, con))
       end

     | xcon decs (Il.CON_MUPROJECT(i, Il.CON_FUN(vars, 
						 Il.CON_TUPLE_INJECT cons))) =
       let
	   val decs' =
	       Ilcontext.add_context_decs 
	       (decs,(map (fn v => 
			   Il.DEC_CON(v, Il.KIND_TUPLE 1, NONE))
		      vars))
	   val (cons', _) = myunzip (map (xcon decs') cons)
	   val con = Mu_c (Util.list2set (Listops.zip vars cons'), 
			   List.nth (vars, i-1))
       in
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (Il.CON_MUPROJECT(i, Il.CON_FUN([var], con))) =
       let
	   val decs' = 
	       Ilcontext.add_context_con'(decs, var, Il.KIND_TUPLE 1, NONE)
	   val (con',_) = xcon decs' con
	   val con = Mu_c (Util.list2set [(var, con')], var)
       in
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (Il.CON_RECORD rdecs) = 
       let
	   val (lbls, cons) = xrdecs decs rdecs
	   val con = Prim_c (Record_c lbls, cons)
       in
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (Il.CON_FUN (vars, il_con1)) = 
       let
	   val decs' = 
	       Ilcontext.add_context_decs 
	       (decs, (map (fn v => Il.DEC_CON(v, Il.KIND_TUPLE 1, NONE))
		       vars))
	   val (con1, Singleton_k(knd1,_)) = xcon decs' il_con1
	   val args = map (fn v => (v, Type_k)) vars
	   val con = Fun_c(Open, args, con1)
       in
	   (con, Singleton_k(Arrow_k(Open, args, knd1), con))
       end

     | xcon decs (Il.CON_SUM (units, il_cons)) = 
       let
	   val (cons, _) = myunzip (map (xcon decs) il_cons)
	   val con = Prim_c (Sum_c units, cons)
       in
	   (con, Singleton_k(Word_k, con))
       end

     | xcon decs (il_con as (Il.CON_TUPLE_INJECT il_cons)) = 
       let
	   val (cons, knds) = myunzip (map (xcon decs) il_cons)
	   val tuple_length = List.length cons
	   val labels = makeLabels tuple_length
	   val vars = makeVars tuple_length
	   val con = Prim_c(Record_c labels, cons)
	   val knd = Record_k (Util.list2sequence 
			       (myzip (myzip (labels,vars), knds)))
       in
	   (con, Singleton_k(knd, con))
       end

     | xcon decs (il_con as (Il.CON_TUPLE_PROJECT (i, il_con1))) = 
       let
	   val (con1, Singleton_k(Record_k seq,_)) = xcon decs il_con1
	   val lbl = Ilutil.generate_tuple_label i
	   val con = Proj_c(con1, lbl)
	   fun equal((l,_),(l',_)) = Name.eq_label (l,l')
	   val knd = 
	       (case (Util.sequence_lookup equal seq (lbl,Name.fresh_var())) of
		    SOME knd => knd
		  | NONE => (print "(xcon) Error translating:\n";
			     Ppil.pp_con il_con;
			     print "Cannot find label ";
			     Ppil.pp_label lbl;
			     error "(xcon) CON_TUPLE_PROJECT"))
       in
	   (con, Singleton_k(knd, con))
       end

     | xcon decs (il_con as (Il.CON_MODULE_PROJECT (modv, lab))) = 
       let
	   val il_knd = Ilstatic.GetConKind (decs, il_con)
	   val {rev_cbnds,name_c,...} = xmod decs modv
	   val con = makeLetC (rev rev_cbnds) (Proj_c (Var_c name_c, lab))
	   val knd = xkind il_knd
       in
	   (con, Singleton_k(knd, con))
       end
    
     | xcon decs c = (print "Error:  Unrecognized constructor:\n";
		      Ppil.pp_con c;
		      error "(xcon):  Unrecognized constructor")
   
   and toFunction decs (exp as Il.FIX _) =
       let
	   val (Let_e (_, [Fixfun_b fns], Var_e var), _) = xexp decs exp
       in
	   case	(Util.sequence_lookup (Name.eq_var) fns var) of
	       SOME f => f
	     | NONE => error "(toFunction): impossible"
       end
     | toFunction _ _ = error "(toFunction): not a FIX expression"

   and xvalue decs (Prim.int (intsize, w)) = 
       (Const_e (Prim.int (intsize, w)), Prim_c(Int_c intsize, nil))

     | xvalue decs (Prim.uint (intsize, w)) = 
       (Const_e (Prim.uint (intsize, w)), Prim_c(Int_c intsize, nil))

     | xvalue decs (Prim.float (floatsize, f)) = 
       (Prim_e (NilPrimOp (box_float floatsize),
		[], SOME [Const_e (Prim.float (floatsize, f))]),
	Prim_c(BoxFloat_c floatsize,nil))

     | xvalue decs (Prim.array (il_con, a)) = 
       let
	   val il_exps = Array.foldr (op ::) nil a
           val (con,_) = xcon decs il_con 
	   val (exps, _) = myunzip (map (xexp decs) il_exps)
       in
	   (Const_e (Prim.array (con, Array.fromList exps)), 
	    Prim_c(Array_c, [con]))
       end

     | xvalue decs (Prim.vector (il_con, v)) = 
       let
	   val il_exps = Array.foldr (op ::) nil v
           val (con, _) = xcon decs il_con
	   val (exps, _) = myunzip (map (xexp decs) il_exps)
       in
	   (Const_e (Prim.vector (con, Array.fromList exps)), 
	    Prim_c(Array_c, [con]))
       end

     | xvalue decs (Prim.refcell (ref il_exp)) = 
       let
	   val (exp, con) = xexp decs il_exp
       in
	   (* BUG *)
	   (* SHOULD PRESERVE EQUIVALENCE OF REF VALUES BUT DOESN'T !!! *)
	   (Const_e (Prim.refcell (ref exp)), Prim_c(Ref_c, [con]))
       end

     | xvalue decs (Prim.tag(tag, il_con))  =
       let
	   val (con, _) = xcon decs il_con
       in
	   (Const_e (Prim.tag (tag, con)), Prim_c(Exntag_c, [con]))
       end

   and xexp decs (Il.OVEREXP(_, true, exp)) = xexp decs (xoneshot exp)

     | xexp decs (Il.SCON il_scon) = xvalue decs il_scon

     | xexp decs (il_exp as (Il.APP (Il.PRIM (prim, il_cons), il_arg))) = 
       let
	   val (cons, _) = myunzip (map (xcon decs) il_cons)
	   val (arg, _) = xexp decs il_arg
	   val il_con = Ilstatic.GetExpCon (decs, il_exp)
	   val (con, _) = xcon decs il_con
       in
	   (Prim_e (PrimOp prim, cons, SOME [arg]), con)
       end

     | xexp decs (il_exp as Il.PRIM (prim, il_cons)) = 
       let
	   val (cons, _) = myunzip (map (xcon decs) il_cons)
	   val il_con = Ilstatic.GetExpCon (decs, il_exp)
	   val (con, _) = xcon decs il_con
       in
	   (Prim_e (PrimOp prim, cons, NONE), con)
       end

     | xexp decs (il_exp as (Il.APP (Il.ILPRIM ilprim, il_arg))) = 
       let
	   val (arg, _) = xexp decs il_arg
	   val il_con = Ilstatic.GetExpCon (decs, il_exp)
	   val (con, _) = xcon decs il_con
       in
	   (Prim_e (PrimOp (xilprim ilprim), [], SOME [arg]), con)
       end

     | xexp decs (il_exp as (Il.ILPRIM ilprim)) = 
       let
	   val il_con = Ilstatic.GetExpCon (decs, il_exp)
	   val (con, _) = xcon decs il_con
       in
	   (Prim_e (PrimOp (xilprim ilprim), [], NONE), con)
       end

     | xexp decs (il_exp as (Il.VAR var)) = 
       let
	   val il_con = Ilstatic.GetExpCon (decs, il_exp)
	   val (con, _) = xcon decs il_con
       in
	   (Var_e var, con)
       end

     | xexp decs (Il.APP (il_exp1, il_exp2)) = 
       let
	   val (exp1, Arrow_c(_,(_,_,_,con))) = xexp decs il_exp1
	   val (exp2, _) = xexp decs il_exp2
       in
	   (App_e (exp1, [], [exp2]), con)
       end

     | xexp decs (Il.FIX (il_arrow, fbnds, var)) = 
       let
	   val set = xfbnds decs fbnds
	   val (SOME (Function (_,_,_,vklist,vclist,_,result_con))) = 
	       Util.set_lookup Name.eq_var set var
	   val effect = (case il_arrow of 
			     Il.PARTIAL => Partial 
			   | Il.TOTAL => Total)
       in
	   (Let_e (Sequential, [Fixfun_b set], Var_e var), 
	    Arrow_c (Open, (effect, vklist, #2 (myunzip vclist), result_con)))
       end

     | xexp decs (Il.RECORD rbnds) = 
       let
	   val (labels, exps, cons) = xrbnds decs rbnds
       in
	   (Prim_e (NilPrimOp (record labels), [], SOME exps),
	    Prim_c (Record_c labels, cons))
       end

     | xexp decs (il_exp0 as (Il.RECORD_PROJECT (il_exp, label, il_record_con))) =
       let
	   val (exp, _) = xexp decs il_exp
           val fields = (case (Ilstatic.con_head_normalize' (decs, il_record_con)) of
                           Il.CON_RECORD fields => fields
                         | hnf => (print "Oops\n";
				   Ppil.pp_exp il_exp0;
				   print "\n";
				   Ppil.pp_con il_record_con;
				   print "\n";
				   Ppil.pp_con hnf;
				   error "xexp: RECORD_PROJECT 1"))
           val il_con = (case (Listops.assoc_eq(Name.eq_label, label, fields)) of
			     SOME il_con => il_con
			   | NONE => (print "Oops\n";
				      Ppil.pp_exp il_exp0;
				      print "\n";
				      Ppil.pp_con il_record_con;
				      print "\n";
				      Ppil.pp_label label;
				      error "xexp: RECORD_PROJECT 2"))
	   val (con, _) = xcon decs il_con
       in
	   (Prim_e (NilPrimOp (select label), [], SOME [exp]), con)
       end

     | xexp decs (Il.SUM_TAIL (_, il_exp)) =
       let
	   val (exp, Prim_c(Sum_c (SOME i), cons)) = xexp decs il_exp
       in
	   (Prim_e (NilPrimOp (project_sum (Word32.fromInt i)), 
		    cons, SOME [exp]),
	    List.nth (cons, i))
       end

     | xexp decs (Il.HANDLE (il_exp1, il_exp2 as (Il.FIX (_,fbnds, var)))) = 
       let
	   val (exp1, con) = xexp decs il_exp1
	   val exp2 = toFunction decs il_exp2
       in
	   (Handle_e (exp1, exp2), con)
       end

     | xexp decs (Il.RAISE (il_con, il_exp)) = 
       let
	   val (exp, _) = xexp decs il_exp
	   val (con, _) = xcon decs il_con
       in
	   (Raise_e (exp, con), con)
       end

     | xexp decs (Il.LET (bnds, il_exp)) = 
       let
	   val (extended_decs, bnds') = xbnds decs bnds
	   val (exp, con) = xexp extended_decs il_exp
       in
	   (Let_e (Sequential, bnds', exp), con)
       end

     | xexp decs (Il.NEW_STAMP il_con) = 
       let
	   val (con, _) = xcon decs il_con
       in 
	   (Prim_e(NilPrimOp make_exntag, [con], SOME []),
	    Prim_c (Exntag_c, []))
       end

     | xexp decs (Il.EXN_INJECT (il_tag, il_exp)) =
       let
	   val (tag, _) = xexp decs il_tag
	   val (exp, _) = xexp decs il_exp
       in
           (Prim_e (NilPrimOp inj_exn, [], SOME [tag, exp]),
	    Prim_c (Exn_c, []))
       end

     | xexp decs (Il.ROLL (il_con, il_exp)) = 
       let
	   val (con, _) = xcon decs il_con
	   val (exp, _) = xexp decs il_exp
       in
	   (Prim_e(NilPrimOp roll, [con], SOME [exp]), con)
       end

     | xexp decs (il_exp as (Il.UNROLL (_, il_exp1))) = 
       let
	   val il_con = Ilstatic.GetExpCon (decs, il_exp)
	   val (con, _) = xcon decs il_con
	   val (exp1, con1) = xexp decs il_exp1
       in
	   (Prim_e(NilPrimOp unroll, [con1], SOME [exp1]), con)
       end

     | xexp decs (Il.INJ (il_cons, n, il_exp)) =
       let
	   val (exp, _) = xexp decs il_exp
	   val (cons, _) = myunzip (map (xcon decs) il_cons)
       in
	   (Prim_e(NilPrimOp (inject (Word32.fromInt n)), cons, SOME [exp]),
	    Prim_c(Sum_c (SOME n), cons))
       end

     | xexp decs (Il.CASE (il_cons, il_exp, il_arms, il_default)) =
       let
	   val (cons, _) = myunzip (map (xcon decs) il_cons)
	   val (exp, _) = xexp decs il_exp
	       
	   fun xarms (n, []) = []
             | xarms (n, NONE :: rest) = xarms (n+1, rest)
	     | xarms (n, SOME e :: rest) = 
	       (Word32.fromInt n, toFunction decs e) :: (xarms (n+1, rest))

	   val (arms as ((_,Function(_,_,_,_,_,_,con))::_)) = xarms (0, il_arms)

	   val default = (case il_default of 
			      NONE => NONE
			    | SOME e => SOME (#1 (xexp decs e)))
       in
	   (Switch_e(Sumsw_e {info = cons, arg  = exp, arms = arms, 
			      default = default}),
	    con)
       end

     | xexp decs (e as Il.EXN_CASE (il_exp, il_arms, il_default)) =
       let
           val _ = Ppil.pp_exp e

	   val (exp, _) = xexp decs il_exp

	   fun xarms [] = []
             | xarms ((il_tag_exp, _, exp) :: rest) = 
	       (#1 (xexp decs il_tag_exp), toFunction decs exp) :: (xarms rest)

	   val (arms as ((_,Function(_,_,_,_,_,_,con))::_)) = xarms il_arms

	   val default = (case il_default of 
			      NONE => NONE
			    | SOME e => SOME (#1 (xexp decs e)))
       in
	   (Switch_e(Exncase_e {info = (), arg = exp, arms = arms,
				default = default}),
	    con)
       end

     | xexp decs (il_exp as (Il.MODULE_PROJECT (module, label))) =
       let
	   val {rev_cbnds, rev_ebnds, 
		name_c, name_r, il_signat} = xmod decs module
	   val (_, Prim_c(Record_c lbls, cons)) = 
	       xsig decs (Var_c name_c, il_signat)
	   val cbnds = rev rev_cbnds
	   val (SOME con') = lookupList Name.eq_label (myzip (lbls,cons)) label
       in
	   (Let_e (Sequential, 
		   (map Con_b cbnds) @ (rev rev_ebnds), 
		   Prim_e (NilPrimOp (select label), cons, SOME [Var_e name_r])),
	    makeLetC cbnds con')
       end

     | xexp decs (Il.SEAL (exp,_)) = xexp decs exp

     | xexp decs _ = error "(xexp) unrecognized expression"

   and xfbnds decs fbnds = 
       let
	   val decs' = 
	       Ilcontext.add_context_decs
	       (decs, (map (fn (Il.FBND(var,_,con,con',_)) => 
			     Il.DEC_EXP(var, Il.CON_ARROW(con,con',
							  Util.oneshot_init 
							  Il.PARTIAL)))
		       fbnds))

	   fun loop [] = []
	     | loop (Il.FBND(var, var', il_con1, il_con2, body) :: rest) = 
	       let
		   val rest' = loop rest
		   val decs'' = 
		       Ilcontext.add_context_exp'(decs', var', il_con1)
		   val (body', _) = xexp decs'' body
                   (* overly conservative ! *)
		   val (effect, recursive) = 
		       if (Ilstatic.Exp_IsValuable(decs'', body)) then
			   (Total, Leaf)
		       else
			   (Partial, Nonleaf)
		   val (con1, _) = xcon decs il_con1
		   val (con2, _) = xcon decs il_con2
	       in
		   (var, Function(Open, effect, recursive, [], 
				  [(var', con1)], body', con2))
		   :: rest'
	       end
       in
	   Util.list2set (loop fbnds)
       end
   handle e => (print "uncaught exception in xfbnds\n";
		raise e)

   and xrbnds decs [] = ([], [], [])
     | xrbnds decs ((label, il_exp) :: rest) = 
       let
	   val (exp, con) = xexp decs il_exp
	   val (labels, exps, cons) = xrbnds decs rest
       in
	   (label :: labels, exp :: exps, con :: cons)
       end

   and xbnds decs [] = (decs, nil)
     | xbnds decs (il_bnd :: rest) = 
       let
	   val (bnds, il_dec) = 
	       (case il_bnd of
		    (Il.BND_EXP(var, il_exp)) => 
			let 
			    val il_con = Ilstatic.GetExpCon (decs, il_exp)
			    val (exp, con) = xexp decs il_exp
			in
			    ([Exp_b(var, exp)], 
			     Il.DEC_EXP(var, il_con))
			end

		  | (Il.BND_MOD(var, il_module)) => 
			let
			    val (var_c, var_r) = splitVar var
			    val {rev_ebnds, rev_cbnds,
				 name_c, name_r, il_signat} = 
				xmod decs il_module
 			in
			    ((Exp_b (var_r, Var_e name_r)) :: 
			     (rev_ebnds @ 
			      (map Con_b ((var_c, Var_c name_c) :: rev_cbnds))),
			     Ilstatic.SelfifyDec (Il.DEC_MOD(var, il_signat)))
			end

		  | (Il.BND_CON(var,il_con)) =>
			let
			    val il_knd = Ilstatic.GetConKind (decs, il_con)
			    val (con, _) = xcon decs il_con
			in
			    ([Con_b(var, con)],
			     Il.DEC_CON(var, il_knd, SOME il_con))
			end)

	   val (decs', bnds'') = 
	       xbnds (Ilcontext.add_context_dec (decs, il_dec)) rest
       in
	   (decs', bnds @ bnds'')
       end

   and xsig decs (con0, Il.SIGNAT_FUNCTOR (var, sig_dom, sig_rng, arrow_shot))=
       let
	   val (var_c, var_r) = splitVar var
	   val (knd, con) = xsig decs (Var_c var, sig_dom)
           val d = Il.DEC_MOD(var, sig_dom)
           val (knd', con') = 
	       xsig (Ilcontext.add_context_dec (decs, Ilstatic.SelfifyDec d))
	            (App_c(con0, [Var_c var]), sig_rng)
       in
	   (Arrow_k (Open, [(var_c, knd)], knd'),
	    Arrow_c (Open, (xeffect (xoneshot arrow_shot), 
			    [(var_c, knd)], [con], con')))
       end

     | xsig decs (con0, Il.SIGNAT_STRUCTURE (_,sdecs)) =
       let
	   val {crdecs, erlabs, ercons} = xsdecs decs (con0, idmap, sdecs)
       in
	   (Record_k (Util.list2sequence crdecs),
	    Prim_c(Record_c erlabs, ercons))
       end
       
   and xsdecs decs (con0, _, []) = {crdecs = nil, erlabs = nil, ercons = nil}
     | xsdecs decs (con0, subst, 
		    Il.SDEC(lbl, d as Il.DEC_MOD(var,signat)) :: rest) =
       let
	   val (var_c, var_r) = splitVar var
	   val (knd, con) = xsig decs (Proj_c(con0, lbl), signat)
           val decs' = Ilcontext.add_context_dec(decs, Ilstatic.SelfifyDec d)
	   val {crdecs, erlabs, ercons} =
	       xsdecs decs' (con0, extendmap (subst, var_c, Proj_c(con0, lbl)),
			    rest)
       in
	   {crdecs = ((lbl, var_c), knd) :: crdecs,
	    erlabs = lbl :: erlabs,
	    ercons = con :: ercons}
       end
     | xsdecs decs (con0, subst, Il.SDEC(lbl, d as Il.DEC_EXP(var,con)) :: rest) =
       let
	   val (con',_) = xcon decs con
           val decs' = Ilcontext.add_context_dec(decs, d)
	   val {crdecs, erlabs, ercons} = xsdecs decs' (con0, subst, rest)
       in
	   {crdecs = crdecs,
	    erlabs = lbl :: erlabs,
            ercons = con' :: ercons}
       end
     | xsdecs decs (con0, subst, Il.SDEC(lbl, d as Il.DEC_CON(var, knd, 
							 maybecon))::rest)=
       let
	   val knd' = xkind knd
	   val knd'' =(case maybecon of
			   NONE => knd'
			 | SOME il_con => #2 (xcon decs il_con))
           val decs' = Ilcontext.add_context_dec (decs, d)
	   val {crdecs, erlabs, ercons} = 
	       xsdecs decs' (con0, extendmap (subst, var, Proj_c(con0, lbl)),
			    rest)
       in
	   {crdecs = ((lbl, var), knd'') :: crdecs,
	    erlabs = erlabs,
	    ercons = ercons}
       end

   and xkind (Il.KIND_TUPLE n) = makeKndTuple n
     | xkind (Il.KIND_ARROW (1,m)) =
         Arrow_k (Open, [(Name.fresh_var(), Type_k)], makeKndTuple m)
     | xkind (Il.KIND_ARROW (n,m)) = 
         let
	     val (Record_k args) = makeKndTuple n
	 in
	     Arrow_k (Open,
		      map (fn ((_,v),k) => (v,k)) args,
		      makeKndTuple m)
	 end
end

