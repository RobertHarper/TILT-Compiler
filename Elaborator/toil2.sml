(* todo : LetExp and CaseExp: valuability computation too conservative
          optimize coercion functors to recognize when they are entirely unnecessary
          optimize coercion functions of polymorphic values to be identity by normalizing type argument positions

   See CVS revision 1.130 for PvalDec and PletExp.
*)

(* The translation from EL to IL *)
structure Toil :> TOIL =
  struct

    open AstHelp Il IlStatic IlUtil Ppil Pat
    open Util Listops Name IlContext Tyvar
    open Prim Error

    val parse_error = fn s => error "toil.sml: parse impossibility" s
    val pat_error = fn s => error "toil.sml: pattern impossibility" s
    val elab_error = fn s => error "toil.sml: elaborator impossibility" s
    val error = fn s => error "toil.sml" s

    val debug = Stats.ff("ToilDebug")

    fun debugdo t = if (!debug) then (t(); ()) else ()

    type tyvar = (context,con,exp) Tyvar.tyvar
    type ocon = (context,con,exp) Tyvar.ocon
    type filepos = Ast.srcpos -> string * int * int

    fun dummy_exp' arg = let val (e,c) = Error.dummy_exp arg
			in  (e,c,true)
			end
    val dummy_strexp_result = (MOD_STRUCTURE[],SIGNAT_STRUCTURE[],true)


    (*  --------- Elaboration state contains: -------------------------
        overload table, eq table, flex tables,
	source position stack, error state
        -------------------------------------------------------------- *)

    local
      val overload_table = ref ([] : (region * ocon) list list)
      val eq_table = ref ([] : (region * tyvar) list)
      val flex_table = ref ([] : (label * flexinfo ref * con * exp Util.oneshot) list)
    in
	fun reset_eq() = eq_table := []
	fun reset_elaboration fp =
	    (Error.reset fp;
	     reset_eq();
	     overload_table := [[]];
	     flex_table := [])
	fun overload_table_push () = overload_table := nil :: !overload_table
	fun overload_table_pop () = (case !overload_table
				       of nil => error "overload_table_pop: overload table is empty"
					| (a::b) => (overload_table := b; rev a))
	fun overload_table_empty () = rev (List.concat (!overload_table)) before overload_table := []
	fun add_overload_entry ocon = (case !overload_table
				       of nil => error "add_overload_entry: overload table is empty"
					| (a::b) => (overload_table := ((peek_region(),ocon)::a) :: b))
	fun get_flex_table () = !flex_table
	fun add_flex_entry (l,rc,fc,e) = flex_table := (l,rc,fc,e)::(!flex_table)
	fun add_eq_entry tyvar =
	    (debugdo (fn () =>
		      (print "marking tyvar for equality: ";
		       pp_con (CON_TYVAR tyvar); print "\n"));
	     eq_table := (peek_region(), tyvar) :: (!eq_table))
	fun get_eq_table() = !eq_table
    end

    fun make_overload (context, cons_exps : (con * exp) list, default : int) : exp * con * bool =
	let
	    val eshot = oneshot()
	    fun mk_constraint (con,exp) =
		let
		    fun check(tyvar,is_hard) =
			let val c' = CON_TYVAR tyvar
			    val match = (if is_hard then eq_con else soft_eq_con)
				(context,con,c')
			in  match
			end
		    fun thunk() =
			(case (oneshot_deref eshot) of
			     SOME _ => ()
			   | NONE => oneshot_set(eshot,exp))
		in  (thunk, check)
		end
	    val constraints = map mk_constraint cons_exps
	    val ocon = Tyvar.fresh_ocon(context,constraints,default)
	    val con = CON_OVAR ocon
	    val exp = OVEREXP(con,true,eshot)
	    val _ = add_overload_entry ocon
	in  (exp,con,Exp_IsValuable(context,exp))
	end

    (* -------------------- special constant overloading -------------------- *)
    (*
	Special constant overloading.  See also pat.sml:/conLiteral.
    *)
    local
	fun make_overload' (context, f : 'a -> con, g : 'a -> exp,
			    domain : 'a list, default : int) : exp * con * bool =
	    let val cons_exps = map (fn a => (f a, g a)) domain
	    in  make_overload (context, cons_exps, default)
	    end

	structure W = TilWord64

	fun in_range_int (low : W.word, high : W.word) (lit : W.word) : bool =
	    W.slte (low, lit) andalso W.sgte (high, lit)
	fun in_range_uint (high : W.word) (lit : W.word) : bool =
	    W.ulte (lit, high)

	val is_int32 : W.word -> bool =
	    in_range_int (W.fromDecimalString "~2147483648",
			  W.fromDecimalString "2147483647")

	val is_uint8 : W.word -> bool =
	    in_range_uint (W.fromHexString "FF")

	val is_uint32 : W.word -> bool =
	    in_range_uint (W.fromHexString "FFFFFFFF")

	val is_float64 : string -> bool =
	    fn _ => true	(* XXX *)

	fun check (p : 'a -> bool, what : string) (lit : 'a) : unit =
	    if p lit then ()
	    else (error_region(); print what; print " constant too large\n")

	val check_int : W.word -> unit = check (is_int32, "integer")
	val check_uint : W.word -> unit = check (is_uint32, "word")
	val check_float : string -> unit = check (is_float64, "floating point")
    in
	fun make_int_overload (context, lit : W.word) : exp * con * bool =
	    (check_int lit; (SCON(int(W32,lit)), CON_INT W32, true))

	fun make_float_overload (context, s : string) : exp * con * bool =
	    (check_float s; (SCON(float(F64,s)), CON_FLOAT F64, true))

	fun make_uint_overload (context, lit : W.word) : exp * con * bool =
	    let val _ = check_uint lit
	    in
		if is_uint8 lit then
		    make_overload' (context, CON_UINT,
				    fn size => SCON(uint(size,lit)),
				    [W32, W8], 0)
		else
		    (SCON(uint(W32,lit)), CON_UINT W32, true)
	    end

	fun make_string_overload (context, s : string) : exp * con * bool =
	    (SCON(intvector (Prim.W8,
			  Array.fromList
			  (map (fn c => SCON(uint(W8,W.fromInt (ord c))))
			   (explode s)))), con_string, true)

	fun make_char_overload (context, s : string) : exp * con * bool =
	    (SCON(uint(W8,
		       (case (explode s) of
			    [c] => W.fromInt (ord c)
			  | _ => parse_error "Ast.CharExp carries string with size different from one"))),
	     CON_UINT W8, true)
    end

    (* ----------------- overload_resolver ----------------------- *)
    fun overload_help (warn,force) (region,ocon) : unit =
	let val _ = push_region region
	    val f = if force then ocon_constrain_default else ocon_constrain
	    val _ =
		(case f ocon
		   of 0 =>
		       (error_region();
			print "overloaded type: none of the constraints are satisfied\n")
		    | 1 => ()
		    | n =>
		       if warn then
			   (error_region();
			    print "overloaded type: more than one constraint is satisfied\n")
		       else ())
	    val _ = pop_region()
	in  ()
	end
    fun resolve_overloads' (overloads : (region * ocon) list) =
	let val _ = debugdo(fn() =>
			    (print "\nresolving overloads\n";
			     app (fn (r,ocon) =>
				  (push_region r; print (peek_region_string()); pop_region();
				   print " "; pp_con (CON_OVAR ocon); print "\n")) overloads;
			     print "(done resolving overloads)\n"))
	in  app (overload_help (true,true)) overloads
	end
    fun resolve_overloads (f : unit -> 'a) : 'a =
	let val _ = overload_table_push()
	    val res = f()
	    val _ = resolve_overloads' (overload_table_pop())
	in  res
	end
    fun resolve_all_overloads () = resolve_overloads' (overload_table_empty())

     (* ----------------- Helper Functions ----------------------- *)
    fun is_non_const context (syms : Symbol.symbol list) =
	(length syms = 1 andalso Symbol.eq(hd syms,Symbol.varSymbol "ref")) orelse
	(Datatype.is_nonconst_constr context syms) orelse
	(case Datatype.exn_lookup context syms of
	     NONE => false
	   | SOME {stamp,carried_type=NONE} => false
	   | SOME {stamp,carried_type=SOME _} => true)

    fun parse_pats context pats =
	(case InfixParse.parse_pat(Context_Fixity context,is_non_const context, pats) of
	     SOME result => result
	   | NONE => (error_region();
		      print "cannot parse pattern\n";
		      [Ast.StringPat "dummypat"]))
    fun parse_pat context pat = (case parse_pats context [pat] of
				     [pat] => pat
				   | _ => error "parse_pat getting back more than 1 pat")



    fun reduce_to_remove (context, c, sbnds) =
	let
	    val boundModVars = (List.mapPartial
				(fn (SBND(_,BND_MOD(v,_,_))) => SOME v
			          | _ => NONE) sbnds)
	    val boundConVars = (List.mapPartial
				(fn (SBND(_,BND_CON(v,_))) => SOME v
			          | _ => NONE) sbnds)
	    fun loop con =
		let
		    val occurVars = Name.VarSet.listItems(con_free con)
		    val escVars = Listops.list_inter_eq(eq_var,boundModVars,occurVars)
		in
		    if (null escVars)
			then con
		    else (case (con_reduce_once(context,con)) of
			      NONE =>
				  (error_region();
				   print "abstract type escaping scope\n";
				   print "  Let body type: "; pp_con c; print "\n";
				   print "  Reduced type: "; pp_con con; print "\n";
				   print "  Escaping: ";
				   app (fn v => (pp_var v; print " ")) escVars;
				   print "\n";
				   Error.dummy_type (context, "escaped_type"))
			    | SOME con => loop con)
		end
	in if (null boundModVars andalso null boundConVars)
	       then c
	   else loop c
	end

     fun sbnd_ctxt_list2modsig (sbnd_ctxt_list : decresult)
	 : (mod * signat) =
	 let fun loop (acc1,acc2) arg =
	     (case arg of
		  [] => (acc1,acc2)
		| ((NONE,_)::rest) => loop (acc1,acc2) rest
		| ((SOME sbnd,CONTEXT_SDEC sdec)::rest) => loop (sbnd::acc1,sdec::acc2) rest
		| ((SOME sbnd,_)::_) => elab_error "sbnd_ctxt_list2modsig: sbnd without sdec")
	     val (sbnds,sdecs) = loop ([],[]) (rev sbnd_ctxt_list)
	 in (MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE sdecs)
	 end

     fun add_context_fixity_entries (context : context,
				     sbnd_ctxt_list : decresult) : context =
         let val entries = List.mapPartial (fn (_, x as CONTEXT_FIXITY _) => SOME x | _ => NONE)
	                     sbnd_ctxt_list
	 in  add_context_entries(context,entries)
         end

     fun add_context_sbnd_ctxts
	 (context : context,
	  sbnd_ctxt : decresult) : sbnd list * context =
	 let
	     val sbnds = List.mapPartial #1 sbnd_ctxt
	     val ces = map #2 sbnd_ctxt
	     val ctxt = add_context_entries(context,ces)
	 in  (sbnds,ctxt)
	 end

     (* -- translate and package a list of objects using an object translator
           inputs  : a translator that takes objects to a list of
	               (optional) bindings and context-entries
		     the bool in the binding pair indicates whether
		       it should be inlined
                     a context
                     a list of objects
           outputs : a list of (optional) bindings and context-entries
     *)
     fun packagedecs (xobj : context * 'a -> decresult * bool)
         (context, sequential) (objs : 'a list) : decresult * bool =
         let
             fun loop context [] = ([],true)
               | loop context [obj] = xobj(context,obj)
               | loop context (obj::rest) =
                 let
                     val (sbnd_ctxt_list,pure) = xobj(context,obj)
                     val context' = if sequential
					then #2(add_context_sbnd_ctxts(context,sbnd_ctxt_list))
				    else context
                     val (sbnd_ctxt_restlist,restpure) = loop context' rest
                 in (sbnd_ctxt_list @ sbnd_ctxt_restlist, pure andalso restpure)
                 end
             val (sbnd_ctxt_list,pure) = loop context objs
             fun maxmap_folder((NONE,_),map) = map
               | maxmap_folder((SOME(SBND(l,_)),_),map) =
                 (case Name.LabelMap.find(map,l) of
                     SOME r => (r := 1 + (!r);
                                map)
                   | NONE => Name.LabelMap.insert(map,l,ref 1))
             val maxmap = foldl maxmap_folder Name.LabelMap.empty sbnd_ctxt_list
             fun uniquify [] = []
               | uniquify (sbnd_ctxt::rest) =
                 (case sbnd_ctxt of
                      (NONE,ce) => (NONE,ce) :: (uniquify rest)
                    | (SOME(sbnd as (SBND(l,bnd))),
                       ce as (CONTEXT_SDEC(SDEC(_,dec)))) =>
                          (case (Name.LabelMap.find(maxmap,l)) of
                              NONE => elab_error "maxmap must have entry at this point"
                            | SOME r =>
                                  (r := (!r) - 1;
                                   if (!r = 0)
                                       then sbnd_ctxt :: (uniquify rest)
                                   else if (!r < 0)
                                            then elab_error "maxmap count inconsistency"
                                        else (* rename case *)
                                            let val s = Name.label2name l
						val l' = Name.fresh_internal_label s
                                            in (SOME (SBND(l',bnd)),
                                                CONTEXT_SDEC(SDEC(l',dec)))
                                                :: (uniquify rest)
                                            end))
                    | (SOME _, _) => elab_error "packagedec: got sbnd without CONTEXT_SDEC")
         in (uniquify sbnd_ctxt_list,pure)
         end

     fun contains_generative_signature (context, s : signat) : bool =
	 let 
	     exception GenerativeSignatureSeen
	     fun sig_handler (SIGNAT_FUNCTOR(_,_,_,GENERATIVE)) = raise GenerativeSignatureSeen
	       | sig_handler _ = NONE
	     val handler = (default_exp_handler, default_con_handler, default_mod_handler,
			    default_sdec_handler, sig_handler)
	 in
	     (sig_handle handler (deep_reduce_signat context s); false)
	     handle GenerativeSignatureSeen => true
	 end



    (* ---------------------------------------------------------
      ------------------ EXPRESSIONS --------------------------
      --------------------------------------------------------- *)
    (* ----- Polymorphic (sdec) Instantiation ------------------------
       given a list of sdecs, return a triple consisting of
      (1) sbnds with the fresh types or already available types
      (2) the same sdecs except replace each opaque type with a fresh type
      (3) a list of the the fresh or already present types
      ---------------------------------------------------------------- *)
     exception NoEqExp
     fun polyinst_opt (ctxt,sdecs) : (sbnds * sdecs * con list) option  =
       (let
	   val _ = debugdo (fn () =>
			    (print "polyinst called with sdecs = "; pp_sdecs sdecs; print "\n"))
	   fun help sdecs =
	       (case sdecs of
		    [] => ([],[],[])
		  | ((sdec1 as SDEC(l1,DEC_CON(v1,k1,copt,_))) :: (sdec2 as SDEC(l2,DEC_EXP(v2,c2,_,_))) :: rest) =>
			let
			    val (con,eq_con,e2) =
				(case copt of
				     NONE => let val tyvar = fresh_named_tyvar (ctxt,"eq_tyvar")
						 val _ = add_eq_entry tyvar
						 val _ = tyvar_use_equal tyvar
						 val con = CON_TYVAR tyvar
						 val exp_os = valOf (tyvar_eq_hole tyvar)
						 val eq_con = con_eqfun ctxt con
						 val e2 = OVEREXP(eq_con,true,exp_os)
					     in (con,eq_con,e2)
					     end
				   | SOME con => let val e2 = (case xeq(ctxt,con) of
								   SOME (e,c) => e
								 | NONE => raise NoEqExp)
						     val eq_con = con_eqfun ctxt con
						 in (con,eq_con,e2)
						 end)

			    val (sbnds,sdecs,cons) = help rest
			    val sbnds' = (SBND(l1,BND_CON(v1,con))) ::
					  (SBND(l2,BND_EXP(v2,e2))) :: sbnds
			    val sdecs' = (SDEC(l1,DEC_CON(v1,k1,SOME con,false)))::
					  (SDEC(l2,DEC_EXP(v2,c2,NONE,false))) :: sdecs
			    val cons' = con :: cons
			in (sbnds',sdecs',cons')
			end
		  | (SDEC(l,DEC_CON(v,k,NONE,_))::rest) =>
			let val c = fresh_con ctxt
			    val (sbnds,sdecs,cons) = help rest
			in ((SBND(l,BND_CON(v,c)))::sbnds,
			    (SDEC(l,DEC_CON(v,k,SOME c,false)))::sdecs,
			    c::cons)
			end
		  | (SDEC(l,DEC_CON(v,k,SOME c,i))::rest) =>
			let val (sbnds,sdecs,cons) = help rest
			in ((SBND(l,BND_CON(v,c)))::sbnds,
			    (SDEC(l,DEC_CON(v,k,SOME c,i)))::sdecs,
			    c::cons)
			end
		  | _ => (print "polyinst got strange sdecs:\n";
			  pp_sdecs sdecs;
			  elab_error "polyinst received strange sdecs"))
	   val (r as (sbnds,sdecs,cons)) = help sdecs
	   val _ = debugdo (fn () =>
			    (print "polyinst returning sbnds = "; pp_sbnds sbnds; print "\n";
			     print "polyinst returning sdecs = "; pp_sdecs sdecs; print "\n";
			     print "polyinst returning cons = "; pp_list pp_con' cons ("(",",",")",false); print "\n"))
       in SOME(r)
       end
   handle NoEqExp => NONE)

    and polyinst arg = valOf(polyinst_opt arg)

    (* ---------- Polymorphic function Instantiation ------------------ *)
    and polyfun_inst (context, module : mod, s : signat) : exp * con =
       let
	 val _ = debugdo (fn () => (print "polyfun_inst called with module:\n";
				     pp_mod module; print "\nand sig = \n";
				     pp_signat s; print "\n\n"))
	 val (e,c) = polyfun_inst' (context,module,s)
	 val _ = debugdo (fn () => (print "polyfun_inst returning e =\n";
				    pp_exp e; print "\nand c =\n";
				    pp_con c; print "\n"))
       in (e,c)
       end
    and polyfun_inst' (context, module : mod, s : signat) : exp * con =
       (case s of
	  (SIGNAT_FUNCTOR(v,SIGNAT_STRUCTURE arg_sdecs,
			  SIGNAT_STRUCTURE [SDEC(_,DEC_EXP(resv,resc,eopt,inline))],_)) =>
	  let
(*
	    val _ = (print "polyfun_inst' got module of:\n";
		     Ppil.pp_mod module;
		     print "\n\n  and signature of:\n";
		     Ppil.pp_signat s;
		     print "\n\n")
*)
	    local
		fun dotype l v = let val tv = fresh_tyvar context
				     val c = CON_TYVAR tv
				 in (tv,(SBND(l,BND_CON(v,c)),
					 SDEC(l,DEC_CON(v,KIND, SOME c,false))))
				 end
		fun help ([] : sdecs) = []
		  | help (SDEC(l1,DEC_CON(v1,_,_,_)) :: SDEC(l2,DEC_EXP(v2,c2,_,_)) :: rest) =
		    let
			val (tyvar,(sbnd1,sdec1)) = dotype l1 v1
			val _ = tyvar_use_equal tyvar
			val _ = add_eq_entry tyvar
			val con = CON_TYVAR tyvar
			val eq_con = con_eqfun context con
			val exp_os = valOf (tyvar_eq_hole tyvar)
			val eqexp = OVEREXP(eq_con,true,exp_os)
			val sbnd2 = SBND(l2,BND_EXP(v2,eqexp))
			val sdec2 = SDEC(l2,DEC_EXP(v2,c2,NONE,false))
		    in (sbnd1,sdec1) :: (sbnd2,sdec2) :: (help rest)
		    end
		  | help (SDEC(l,DEC_CON(v,_,_,_)) :: rest) = (#2(dotype l v))::(help rest)
		  | help sdec = (pp_sdecs sdec;
				 elab_error "unexpected sig to arg struct of polymorphic fun")
		val temp = help arg_sdecs
	    in
	      val (signat_poly_sbnds, signat_poly_sdecs) = Listops.unzip temp
	      val mod_poly = MOD_STRUCTURE signat_poly_sbnds
	      val signat_poly = SIGNAT_STRUCTURE signat_poly_sdecs
	    end
	    val new_rescon : con =
		remove_modvar_type(resc,v,signat_poly_sdecs)
		handle e => (debugdo (fn () =>
				      (print "remove_modvar failed: called from polyfun_inst";
				       print "target con: "; pp_con resc; print "\n";
				       print "variable to be removed  =  "; pp_var v; print "\n";
				       print "sdecs of var = \n"; pp_sdecs signat_poly_sdecs; print "\n"));
			     error "remove_modvar failed: called from polyfun_inst")
	    val exp = (case (eopt,inline) of
			   (SOME e,inline) =>
			       let fun folder (SBND(l,BND_CON(_,c)),s) = subst_add_conpath(s,PATH(v,[l]),c)
				     | folder _ = error "bad functor application"
				   val subst = foldl folder empty_subst signat_poly_sbnds
			       in  exp_subst(e,subst)
			       end
			 | _ => MODULE_PROJECT(MOD_APP(module,mod_poly), it_lab))
(*
	    val _ = (print "polyfun_inst returning exp :\n";
		     pp_exp exp; print "\n\n  and rescon:\n";
		     pp_con new_rescon; print "\n\n")
*)
	  in (exp,new_rescon)
	  end
       |  _ =>  (print "polyfun_inst received s of\n";
		 pp_signat s;
		 print "\n";
		 elab_error "rule 224 or 226 not applicable in polyfun_inst"))


     and xrecordexp (context : context, sorted, sym_expr_list) : (exp * con * bool) =
	 let fun doer((sym,expr),(acc,va_acc)) =
	     let val label = symbol_label sym
		 val (exp,con,va) = xexp(context,expr)
	     in ((label,(label,exp),(label,con))::acc,va andalso va_acc)
	     end
	     val (label_rbnd_rdec,va) = foldr doer ([],true) sym_expr_list
	     val sorted = sorted orelse (label_issorted (map #1 label_rbnd_rdec))
	 in
	     if sorted
		 then (RECORD(map #2 label_rbnd_rdec), CON_RECORD(map #3 label_rbnd_rdec),va)
	     else
		 let
		     fun make_var(l,rb,rd) = (l,(fresh_named_var (label2string l),rb,rd))
		     val label_var_rbnd_rdec = map make_var label_rbnd_rdec
		     val bnds = map (fn (_,(v,(_,e),_)) => BND_EXP(v,e)) label_var_rbnd_rdec
		     val label_var_rbnd_rdec = sort_labelpair label_var_rbnd_rdec
		     val con = CON_RECORD(map (fn (_,(_,_,rd)) => rd) label_var_rbnd_rdec)
		     val body = RECORD(map (fn (l,(v,_,_)) => (l,VAR v)) label_var_rbnd_rdec)
		 in (LET(bnds,body), con, va)
		 end
	 end

     and xpath (context : context, path : Ast.path, do_coerce) : (exp * con * bool) =
	 let
	     fun unbound() =
		 let val _ = (error_region();
			      print "unbound variable or constructor: ";
			      AstHelp.pp_path path;
			      print "\n")
		 in dummy_exp'(context,"unbound_var")
		 end
	     fun eqcase iseq =
		 let val tyvar = fresh_named_tyvar (context,"teq")
		     val _ = tyvar_use_equal tyvar
		     val _ = add_eq_entry tyvar
		     val con = CON_TYVAR tyvar
		     val arg_con = con_tuple[con,con]
		     val eq_con = con_eqfun context con
		     val exp_os = valOf (tyvar_eq_hole tyvar)
		     val eqexp = OVEREXP(eq_con,true,exp_os)
		     val res = if iseq
				   then eqexp
			       else let val v = fresh_named_var "neq_arg"
					val con_bool = con_bool context
					val bool_in = bool_in context
					val con_internal_bool = con_internal_bool context
					val true_exp = internal_bool_exp context true
					val false_exp = internal_bool_exp context false
					val switch = make_ifthenelse context (APP(eqexp,VAR v),
									      false_exp,true_exp,con_internal_bool)
				    in  #1(make_lambda(v,arg_con,con_bool,COERCE(bool_in,[],switch)))
				    end
		 in (res,eq_con,true)
		 end
	     val labs = map symbol_label path
	     val ovld_option = (case labs of
				    [lab] => Context_Lookup_Overload(context, lab)
				  | _ => NONE)
	     fun coerceArrow (c as (CON_ARROW (args, res, closed, comp))) =
		 (case oneshot_deref comp
		    of SOME TOTAL =>
			let val comp' = oneshot()
			    val _ = oneshot_set (comp', PARTIAL)
			in  SOME (CON_ARROW (args, res, closed, comp'))
			end
		     | SOME PARTIAL => NONE
		     | NONE => (error_region();
				print "path has unresolved arrow type\n";
				print "Path: "; AstHelp.pp_path path; print "\n";
				print "Type: "; pp_con c; print "\n";
				error "path has unresolved arrow type";
				NONE))
	       | coerceArrow _ = NONE
	     fun coerce con =
		 if do_coerce then
		     let val con' = (case con
				       of CON_ARROW _ => con
					| _ => con_normalize (context, con))
		     in  (case coerceArrow con'
			    of SOME c => (debugdo (fn () =>
						   (print "partializing "; AstHelp.pp_path path; print "\n";
						    print "Given type: "; pp_con con; print "\n";
						    print "Partial type: "; pp_con c; print "\n"));
					  c)
			     | NONE => con)
		     end
		 else con
	 in  (case ovld_option of
		  SOME (OVLD (_, NONE)) => (error_region();
					    print "no default type for overloaded identifier: ";
					    AstHelp.pp_path path; print "\n";
					    error "no default type for overloaded identifier")
		| SOME (OVLD (ce, SOME n)) => make_overload (context, ce, n)
		| NONE => (* identifier is long or is not overloaded *)
		      (case (Context_Lookup_Labels(context,labs)) of
			   SOME(_,PHRASE_CLASS_EXP (_,c,SOME e,true)) => (e,coerce c,Exp_IsValuable(context,e))
			 | SOME(_,PHRASE_CLASS_EXP (e,c,_,_)) => (e,coerce c,true)
			 | SOME(_,PHRASE_CLASS_EXT (v,_,c)) => (VAR v,coerce c,true)
			 | SOME(_,PHRASE_CLASS_MOD (m, _, s, _)) =>
			       (case s of
				    SIGNAT_FUNCTOR _ =>
					let val (e,c) = polyfun_inst (context,m,s)
					in  (e,coerce c,true)
					end
				  | SIGNAT_STRUCTURE sdecs =>
					let
					    fun dosdec (SDEC(l,DEC_EXP(_,c,_,_))) =
						if (eq_label (l,mk_lab))
						    then (MODULE_PROJECT(m,mk_lab),coerce c,true)
						else unbound()
					      | dosdec (SDEC(l,DEC_MOD(_,_,s))) =
						    if (eq_label(l,mk_lab))
							then
							    let val mk_mod = MOD_PROJECT(m,mk_lab)
								val (e,c) = polyfun_inst(context,mk_mod,s)
							    in  (e,coerce c,true)
							    end
						    else unbound()
					      | dosdec _ = unbound()
					in (case sdecs of
						[sdec] => dosdec sdec
					      | [_,sdec] => dosdec sdec
					      | _ => unbound())
					end)
			 | SOME (_, PHRASE_CLASS_CON _) => unbound()
			 | SOME (_, PHRASE_CLASS_SIG _) => unbound()
			 | NONE => if (length path = 1 andalso (Symbol.eq(hd path,Symbol.varSymbol "=")))
				       then eqcase true
				   else if (length path = 1 andalso
					    (Symbol.eq(hd path,Symbol.varSymbol "<>")))
					    then eqcase false
					else unbound()))
	 end

     and xexp (context : context, exp : Ast.exp) : (exp * con * bool) = (* returns valuablilty *)
      (
(*
       print "xexp called ";
       AstHelp.pp_exp exp;
       print "\n";
*)
       case exp of
	 Ast.IntExp lit => make_int_overload (context, lit)
       | Ast.WordExp lit => make_uint_overload (context, lit)
       | Ast.RealExp s => make_float_overload (context, s)
       | Ast.StringExp s => make_string_overload (context, s)
       | Ast.CharExp s => make_char_overload (context, s)
       | (Ast.TupleExp exps) => xrecordexp(context,length exps < 10,
					   mapcount (fn(n,a) => (generate_tuple_symbol (n+1),a)) exps)
       | (Ast.RecordExp sym_exps) => xrecordexp(context,false, sym_exps)
       | Ast.ListExp args =>
	     let fun loop [] = AstHelp.nil_exp
		   | loop (a::b) = Ast.AppExp{function=AstHelp.cons_exp,
					      argument=Ast.TupleExp[a,loop b]}
	     in xexp(context,loop args)
	     end
       | Ast.SelectorExp s =>
     (* for the record, i'd like to say that this construct is a pain in the butt - Perry *)
	     let
		 val label = symbol_label s
		 val stamp = new_stamp()
		 val fieldcon = fresh_con context
		 val the_ref = ref(FLEXINFO(stamp,false,[(label,fieldcon)]))
		 val rescon = CON_ARROW([CON_FLEXRECORD the_ref],
					fieldcon, false, oneshot_init PARTIAL)
		 val eshot : exp Util.oneshot = oneshot()
		 val exp = OVEREXP(rescon,true,eshot)
		 val _ = add_flex_entry(label,the_ref,fieldcon,eshot)
	     in (exp,rescon,true)
	     end
       | Ast.VarExp path => xpath (context, path, true)
       | Ast.LetExp {dec,expr} =>
             let val var1 = fresh_named_var("LetStr")
                 val lbl = to_open(internal_label "LetStr")
		 val sbnd_ctxt_list = xdec (context, dec)
                 val (mod1,sig1) = sbnd_ctxt_list2modsig sbnd_ctxt_list
		 val context = add_context_mod(context,lbl,var1,sig1)
		 val context = add_context_fixity_entries(context,sbnd_ctxt_list)
		 val (e,c,va) = xexp(context,expr)
		 val bnd = BND_MOD(var1,false,mod1)
		 val c = reduce_to_remove(context,c,[SBND(lbl,bnd)])
	     in  (LET([bnd],e),c,false)
	     end


       | Ast.FlatAppExp _ => (case InfixParse.parse_exp(Context_Fixity context, exp) of
				  SOME exp' => xexp(context,exp')
				| NONE => (error_region();
					   print "cannot parse FlatAppExp\n";
					   dummy_exp'(context,"bad_application")))

       | Ast.CcallExp (function,arguments) =>
	     let val (e,con,va) = xexp(context,function)
		 val arrow =
		     (case con of
			  CON_ARROW(_,_,true,arrow) => arrow
			| _ => (case (con_normalize(context,con)) of
				    CON_ARROW(_,_,true,arrow) => arrow
				  | _ => oneshot()))
		 val e_con_va_args = map (fn e => xexp(context,e)) arguments
		 val va_args = Listops.orfold #3 e_con_va_args
		 val exp_args = map #1 e_con_va_args
		 val con_args = map #2 e_con_va_args
		 val spec_rescon = fresh_con context
		 val spec_funcon = CON_ARROW(con_args,spec_rescon,
					     true,arrow)
	     in
		 if (semi_sub_con(context,spec_funcon,con))
		     then (let val va3 =
			       (case oneshot_deref arrow of
				      NONE => (oneshot_set(arrow,PARTIAL); false)
				    | SOME PARTIAL => false
				    | SOME TOTAL => true)
			   in  (EXTERN_APP(con,e,exp_args),
				con_deref spec_rescon,
				va andalso va_args andalso va3)
			   end)
		 else
		     (case (con_normalize(context,con)) of
			 CON_ARROW(param_cons,_,_,_) =>
			     (error_region(); print " external application is ill-typed.\n";
			      print "  Function domain: ";
			      app (fn c => (pp_con c; print "; ")) param_cons;
			      print "\n  Argument type: ";
			      app (fn c => (pp_con c; print "; ")) con_args;
			      print "\n";
			      dummy_exp'(context,"bad_application"))
		       | nonarrow => (error_region();
				      print " operator is not a function.  Has type:\n";
				      pp_con nonarrow;
				      print "\n";
				      dummy_exp'(context,"bad_application")))
	     end

       | Ast.AppExp {argument,function} =>
	     let val (e1,con1,va1) = (case AstHelp.exp_strip function
					of Ast.VarExp path => xpath(context,path,false)
					 | _ => xexp(context,function))
		 val (e2,con2,va2) = xexp(context,argument)
		 val arrow = oneshot()
		 val rescon = fresh_con context
		 val funcon = CON_ARROW([con2],rescon,false,arrow)
		 fun constrain (OVEREXP (CON_OVAR ocon,_,_)) =
			   (overload_help (false,false) (peek_region(),ocon); ())
		   | constrain _ = ()
	     in  if (semi_sub_con(context,funcon,con1))
		     then (let val va3 = (case oneshot_deref arrow of
					      NONE => (oneshot_set(arrow,PARTIAL); false)
					    | SOME PARTIAL => false
					    | SOME TOTAL => true)
			       val _ = constrain e1
			       val _ = constrain e2
			       val exp = APP(e1,e2)
			   in  (exp_try_reduce (context,exp),
				con_deref rescon,
				va1 andalso va2 andalso va3)
			   end)
		 else
		     (case (con_normalize(context,con1)) of
			 CON_ARROW([param_con],rescon,_,_) =>
			     (error_region(); print " application is ill-typed.\n";
			      print "  Function domain: "; pp_con param_con;
			      print "\n  Argument type: "; pp_con con2;
			      print "\n  Expanded function domain: "; pp_con (con_normalize(context,param_con));
			      print "\n  Expanded argument type: "; pp_con (con_normalize(context,con2));
			      print "\n";
			      dummy_exp'(context,"bad_application"))
		       | nonarrow => (error_region();
				      print " operator is not a function. Has type:\n";
				      pp_con nonarrow;
				      print "\n";
				      dummy_exp'(context,"bad_application")))
	     end
       | Ast.AndalsoExp (e1,e2) =>
	     xexp(context,Ast.IfExp{test=e1,thenCase=e2,elseCase=AstHelp.false_exp})
       | Ast.OrelseExp (e1,e2) =>
	     xexp(context,Ast.IfExp{test=e1,thenCase=AstHelp.true_exp,elseCase=e2})
       | Ast.IfExp {test,thenCase,elseCase} =>
	     let val pr1 = {pat=true_pat,exp=thenCase}
		 val pr2 = {pat=false_pat,exp=elseCase}
	     in xexp(context,Ast.CaseExp {expr=test,
					  rules=[Ast.Rule pr1, Ast.Rule pr2]})
	     end
       | Ast.ConstraintExp {expr, constraint} =>
	     let val (exp,con,va) = xexp(context,expr)
		 val con' = xty(context,constraint)
	     in if sub_con(context,con,con')
		    then (SEAL(exp,con'),con',va)
		else let val (e,c) = dummy_exp(context,"badseal")
		     in  error_region();
			 print "constraint does not match expression type\n";
			 print "  Expression type: "; pp_con con;
			 print "\n  Constraint type: "; pp_con con';
			 print "\n";
			 (SEAL(e,con'),con',va)
		     end
	     end
       | Ast.VectorExp expr_list =>
	     let val c = fresh_con context
		 val ecv_list = map (fn e => xexp(context,e)) expr_list
		 val elist = map #1 ecv_list
		 fun folder (_,c',_) = (eq_con(context,c,c') orelse
					(error_region();
					 print "ill-typed vector expression\n";
					 false))
	     in if (andfold folder ecv_list)
		    then (SCON(vector(c,Array.fromList elist)), CON_VECTOR c,
			  andfold (fn (_,_,va) => va) ecv_list)
		else dummy_exp'(context, "bad_vector")
	     end
       | Ast.WhileExp {test,expr} =>
	     let
		 val (teste,testc,_) = xexp(context,test)
		 val (be,bc,_) = xexp(context,expr)
		 val body_ec = (be,bc)
	     in  if (eq_con(context,testc,con_bool context))
		     then
			 let val loop_var = fresh_named_var "loop"
			     val arg_var = fresh_named_var "loop_arg"
			     val (then_exp,_) = make_seq[body_ec,
							 (APP(VAR loop_var, unit_exp),
							  con_unit)]
			     val loop_body = make_ifthenelse context (teste,then_exp,unit_exp,con_unit)
			     val loop_fun = FIX(true,PARTIAL,[FBND(loop_var,arg_var,con_unit,con_unit,loop_body)])
			 in (LET([BND_EXP(loop_var,loop_fun)],APP(VAR loop_var, unit_exp)),
			     con_unit, false)
			 end
		 else (error_region();
		       print "while construct given a test clause not of boolean type\n";
		       dummy_exp'(context,"badwhile"))
	     end
       | Ast.HandleExp {expr,rules} => (* almost same as CaseExp except need to wrap with HANDLE *)
	     let
		 val (exp',rescon,va) = xexp(context,expr)
		 val v = fresh_named_var "handle_exn"
		 val arms = map (fn (Ast.Rule{pat,exp})=>(parse_pat context pat,exp)) rules
		 val (hbe,hbc) = caseCompile{context = context,
					     arms = arms,
					     arg = (v,CON_ANY),
					     reraise = true}
		 val (he,hc) = make_lambda(v,CON_ANY,hbc,hbe)
	     in if (eq_con(context,rescon,hbc))
		    then (HANDLE(rescon,exp',he),rescon,va)
		else let val rescon = rescon
		     in  if sub_con(context,hbc,rescon)
			     then (HANDLE(rescon,exp',he),rescon,va)
			 else (error_region();
			       print "mismatch between handle body and handler\n";
			       dummy_exp'(context,"bad_handle"))
		     end
	     end
       | Ast.RaiseExp e =>
	     let val (exp,con,_) = xexp(context,e)
		 val c = fresh_con context
	     in if (eq_con(context,con,CON_ANY))
		    then (RAISE (c,exp),c,false)
		else (error_region(); print "raise not given an expression of exn type\n";
		      dummy_exp'(context,"badraise"))
	     end
       | Ast.SeqExp elist =>
	     let val ecvlist = map (fn e => xexp(context,e)) elist
		 val eclist = map (fn (e,c,v) => (e,c)) ecvlist
		 val (e,c) = make_seq eclist
	     in  (e,c,andfold (fn (e,c,v) => v) ecvlist)
	     end
       | Ast.FnExp [] => parse_error "Ast.FnExp with empty list"
       | Ast.FnExp rules =>
	     let val arms = map (fn (Ast.Rule{pat,exp}) => (parse_pats context [pat],exp)) rules
		 val {arglist,body} = funCompile{context = context,
						 rules = arms}
		 fun help ((v,c),(e,resc)) = make_lambda(v,c,resc,e)
		 val (e,c) = foldr help body arglist
	     in  (e,c,true)
	     end
       | Ast.CaseExp {expr,rules} =>
	     let fun getarm (Ast.Rule{pat,exp}) = (parse_pat context pat,exp)
		 val arms = map getarm rules
		 val (arge,argc,_) = xexp(context,expr)
		 val v = fresh_named_var "casearg"
		 val (bodye,c) = caseCompile{context = context,
					     arms = arms,
					     arg = (v,argc),
					     reraise = false}
		 val e = make_let([BND_EXP(v,arge)], bodye)
	     in (e,c,false)
	     end
       | Ast.MarkExp(exp,region) =>
	     let val _ = push_region region
		 val res = xexp(context,exp)
		 val _ = pop_region()
	     in res
	     end)


    (* ---------------------------------------------------------
      ------------------ DECLARATIONS --------------------------
      --------------------------------------------------------- *)
   and xdatatype (context,datatycs) : (sbnd * sdec) list =
        Datatype.compile{context=context,
			 typecompile=xty,
			 datatycs=datatycs,
			 eq_compile=xeq',
			 is_transparent=false}

     and xfundec (context : context, dec_list : ((label * var) list * con * con *
	               (Ast.pat list * Ast.exp) list) list, tyvar_stamp, sdecs1, var_poly, open_lbl) =
	 let
	     (* We must discover all the variables to generalize over.
	      For the user-level variables, we must also compile in a context
              with these user-level variables bound.  As a first
	      step, we find all the user-level variables in the program.
	      At some future point when the syntax includes explicit scoping,
	      we must include those too *)

	     val fun_ids : (label * var) list list = map #1 dec_list

	     (* --- context with type variables --- *)
	     val context' = add_context_mod(context,open_lbl,var_poly,
					    SIGNAT_STRUCTURE sdecs1)

	     (* --- create the context with all the fun_ids typed --- *)
	     val context_fun_ids =
		 let fun help ((ids,c,_,_),ctxt) =
			let fun add ((l,v), ctxt) = add_context_exp(ctxt,l,v,c)
			in  foldl add ctxt ids
			end
		 in  foldl help context' dec_list
		 end

	     (* substitution to eliminate all but a representative variable for each function *)
	     fun rep (ids:(label * var) list) : label * var * (label * var) list =
		(case ids of
		    (l,v)::ids => (l,v,ids)
		|   nil => error "xfundec called with an empty id list")
	     val subst_fun_vars =
		let fun help ((ids,_,_,_),subst) =
			let val (_,v,ids) = rep ids
			    val exp = VAR v
			    fun add ((l,v),subst) = subst_add_expvar(subst,v,exp)
			in  foldl add subst ids
			end
		in  foldl help empty_subst dec_list
		end

	     val fbnd_con_list =
		 (map (fn (ids,fun_con,body_con,matches) =>
			let val (_,var',_) = rep ids
			    val {body = (bodye,bodyc), arglist} =
			         funCompile {context = context_fun_ids,
					     rules = matches}
			    val bodye = exp_subst(bodye,subst_fun_vars)

			    fun con_folder ((_,c),acc) = CON_ARROW([c],acc,false,oneshot_init PARTIAL)
			    val func = foldr con_folder bodyc arglist
			    val _ = if eq_con(context',body_con,bodyc)
					then ()
				    else (error_region();
					  print "function constraint does not match body type\n";
					  print "Actual type: "; pp_con bodyc; print "\n";
					  print "Constraint type: "; pp_con body_con; print "\n";
					  print "Actual reduced type: ";
					  pp_con (con_normalize(context',bodyc)); print "\n";
					  print "Constraint reduced type: ";
					  pp_con (con_normalize(context',body_con)); print "\n")
			    val _ = if eq_con(context',fun_con,func)
					then ()
				    else (error_region();
					  print "function constraint does not match function type\n";
					  print "Actual type: "; pp_con func; print "\n";
					  print "Constraint type: "; pp_con fun_con; print "\n")
			    local
				fun help ((v,c),(e,resc)) = make_lambda(v,c,resc,e)
			    in
			      val (var1,con1)::tlArglist = arglist
			      val (bigbodye,bigbodyc) = foldr help (bodye,bodyc) tlArglist
			    end
			in (FBND(var',var1,con1,bigbodyc,bigbodye),func)
			end)
		 dec_list)

	     val fbnds = map #1 fbnd_con_list
	     val fbnd_cons : con list = map #2 fbnd_con_list

	     val top_label = to_cluster (internal_label "cluster")
	     val top_var = fresh_named_var "cluster"

	     (* Note: the phase-splitter will emit much better code if functions
	      * are elaborated in the way that it expects (at least while do_polyrec
	      * and elaborator_specific_optmizations are true).  Therefore, please
	      * don't change this without changing the corresponding cases in
	      * tonil.  -leaf
	      *)
	     (*
		We arrange the sbnds and sdecs from

			val rec f as g = e1
			and h = e2

		to look like the user had typed

			val rec f = [f/g]e1
			and h = [f/g]e2
			val g = f

		So that the phase splitter sees what it expects wrt f and h.
	     *)
	     val top_exp = FIX(true,PARTIAL,fbnds)
	     val (top_con,trivial_bundle) =
		(case fbnd_cons of
		    [c] => (c,true)
		|   _ => (con_tuple fbnd_cons, false))
	     val top_exp_con = (top_exp,top_con)

	     local
		 val tyvar_lbls'_useeq =
		     rebind_free_type_var(length sdecs1,
					  tyvar_stamp,top_con,
					  context_fun_ids,var_poly)
		 fun help(_,tlab,iseq) = (tlab,iseq)
		 val temp = map help tyvar_lbls'_useeq
		 val sdecs2 = make_typearg_sdecs context temp
	     in
		 val sdecs = sdecs1 @ sdecs2
		 val sdecs = reduce_typearg_sdecs (top_exp, (var_poly,[]), sdecs)
	     end
	     val monomorphic = null sdecs

	     val temp_exp =
		if monomorphic then VAR top_var
		else
		    let val s = MOD_APP(MOD_VAR top_var,MOD_VAR var_poly)
		    in  MODULE_PROJECT(s,them_lab)
		    end

	     val exp_con_list =
		if trivial_bundle then [(temp_exp,top_con)]
		else
		    let fun mapper (i,c) =
			    let val lab = generate_tuple_label (i+1)
				val e = RECORD_PROJECT(temp_exp,lab,top_con)
			    in  (e,c)
			    end
		    in  mapcount mapper fbnd_cons
		    end

	     val modsig_helper :
			label * (label * var) list * exp * con ->
			(sbnd * sdec) * (sbnd * sdec) list =
		if monomorphic then
		    fn (_,ids,exp,con) =>
			let val (l,v,ids) = rep ids
			    val first =
				(SBND(l,BND_EXP(v,exp)),
				 SDEC(l,DEC_EXP(v,con,NONE,false)))
			    fun other (l',v') =
				(SBND(l',BND_EXP(v',VAR v)),
				 SDEC(l',DEC_EXP(v',con,NONE,false)))
			in  (first,map other ids)
			end
		else
		    fn (inner_lab,ids,exp,con) =>
			let val (l,v,ids) = rep ids
			    val inner = Name.fresh_named_var ((Name.var2name v) ^ "_inner")
			    val sig_poly = SIGNAT_STRUCTURE sdecs
			    val sbnd = SBND(inner_lab, BND_EXP(inner,exp))
			    val sdec = SDEC(inner_lab, DEC_EXP(inner,con,NONE,false))
			    val inner_sig = SIGNAT_STRUCTURE [sdec]
			    val functor_mod = MOD_FUNCTOR(TOTAL,var_poly,sig_poly,
							  MOD_STRUCTURE[sbnd],inner_sig)
			    val functor_sig =
				SIGNAT_FUNCTOR(var_poly,sig_poly,inner_sig,TOTAL)
			    val first =
				(SBND(l,BND_MOD(v,true,functor_mod)),
				 SDEC(l,DEC_MOD(v,true,functor_sig)))
			    fun other (l',v') =
				(SBND(l',BND_MOD(v',true,MOD_VAR v)),
				 SDEC(l',DEC_MOD(v',true,functor_sig)))
			in  (first,map other ids)
			end

	     val (top_sbnd_sdec,_) = modsig_helper (them_lab,[(top_label,top_var)],top_exp,top_con)
	     fun folder (ids,(exp,con),(acc_rep,acc_other)) =
		let val (rep,other) = modsig_helper (it_lab,ids,exp,con)
		in  (rep::acc_rep, other::acc_other)
		end
	     val (rep,other) = foldl2 folder (nil,nil) (fun_ids,exp_con_list)
	     val rep_sbnds_sdecs = rev rep
	     val other_sbnds_sdecs = List.concat(rev other)
	     val sbnds_sdecs = top_sbnd_sdec :: (rep_sbnds_sdecs @ other_sbnds_sdecs)
	     val sbnds_entries = (map (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec))
				  sbnds_sdecs)
	 in  sbnds_entries
	 end

     and xdec (args : context * Ast.dec) : decresult =
         #1(xdec' args)

     (* xdec' returns a bool as well, which is true iff the declaration is "pure" in the sense of "transparent" *)
     and xdec' (context : context, d : Ast.dec) : decresult * bool =
       (case d of
          (* --- The tricky thing about this value declarations is figuring
	     --- out what type variables need to be generalized.  There are two
             --- sources: those arising explicitly from the program text and those
             --- arising from the type inference process.
	     --- (1) Source-level Type Variables:
             ---     In the former case, these explicit type variables must be put into
             ---     the translation context before encountering.  This can be done by
             ---     a prepass over the program to explicitly scope these user-level type varaibles.
             ---     In ML96, these there is a syntax for the programmer to give these type variables
             ---     explicit scope.  Nonetheless, since this is optional, we must create the
             ---     (narrowest) scope for those variables that the user did not explicitly scope.
             ---     As a matter of efficiency, it would be better to compute the scope of user-level
             ---     type variables in a prepass rather than repeatedly scan the expression for them.
             --- (2) Unresolved meta-variables(unless constrained) are also eligible for type generalization.
             ---     If a meta-variable is to be generalized, it is resolved to a type variables which is
             ---     then generalized.  Unresolved meta-variables can appear in the type of the
             ---     expression being bound.  These must be generalized.  Meta-variables that are generated
             ---     and appear in the translated expression but not the translated type may be generalized
             ---     or not.  If they are not generalized however, they MUST be resolved at some point to some
             ---     well-formed type.
             --- (3) Note that until pattern matching is done, metavariables that seem to be generalizable
             ---     may not be. *)
	  Ast.ValDec ([],[],_) => ([],true)
	| Ast.ValDec (vblist,[],ref tyvars) =>
	    let
		local
		  val pe_list = map vb_strip vblist
		in
		  val (pat,expr) = (case pe_list of
				      [] => pat_error "let with nothing should not get here"
				    | [(p,e)] => (p,e)
				    | _ => (Ast.TuplePat(map #1 pe_list),
					    Ast.TupleExp(map #2 pe_list)))
		end

		val tyvar_stamp = new_stamp()
		val tyvars = map tyvar_strip tyvars
		local
		     fun help tyvar =
			  let val type_str = Symbol.name tyvar
			      val type_lab = symbol_label tyvar
			      val is_eq =  ((size type_str >= 2) andalso
					    (String.substring(type_str,0,2) = "''"))
			  in  (type_lab,is_eq)
			  end
		     val temp = map help tyvars
		in  val temp_sdecs = make_typearg_sdecs context temp
		end
		val lbl_poly = to_open(internal_label ("!varpoly"))
		val var_poly = fresh_named_var "varpoly"
		val context' = add_context_mod(context,lbl_poly,var_poly,SIGNAT_STRUCTURE temp_sdecs)
		val lbl = internal_label "!bindarg"
		val v = fresh_named_var "bindarg"
		val (e,con,va) = xexp(context',expr)
		val sbnd_sdec = (SBND(lbl,BND_EXP(v,e)),SDEC(lbl,DEC_EXP(v,con,NONE,false)))
                val parsed_pat = parse_pat context pat
		val bind_sbnd_sdec = (bindCompile{context = context',
						  bindpat = parsed_pat,
						  arg = (v,con)})
		val varsBound = String.concat(map (fn (SBND(l,_),_) => label2name' l) bind_sbnd_sdec)
		val valbind_string = "valbind" ^ varsBound
		val lbl_valbind = internal_label ("!" ^ valbind_string)
		val var_valbind = fresh_named_var valbind_string
		val sbnd_sdec_list =
		    (case bind_sbnd_sdec of
			 [(SBND(lbl',BND_EXP(v'',VAR v')),_)] =>
			     if (eq_var(v,v'))
				 then [(SBND(lbl',BND_EXP(v'',e)), SDEC(lbl',DEC_EXP(v'',con,NONE,false)))]
			     else sbnd_sdec::bind_sbnd_sdec
		       | _ => sbnd_sdec::bind_sbnd_sdec)
		local
		    val context' = add_context_exp'(context',v,con)
		in
		    val is_irrefutable = va andalso Sbnds_IsValuable(context', map #1 bind_sbnd_sdec)
		end
		fun refutable_case () =
		    map (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec)) sbnd_sdec_list
		and irrefutable_case () =
		    let
			val tyvar_lbls_useeq = rebind_free_type_var(length tyvars,
								    tyvar_stamp,con,
								    context,var_poly)
			val lbls_useeq = (map (fn (_,l,f) => (l,f)) tyvar_lbls_useeq)
			val poly_sdecs = temp_sdecs @ (make_typearg_sdecs context lbls_useeq)
			val (sbnds,sdecs) = (map #1 sbnd_sdec_list, map #2 sbnd_sdec_list)
			val poly_sdecs = reduce_typearg_sdecs (e, (var_poly,[]), poly_sdecs)
		        val a = if is_irrefutable then TOTAL else PARTIAL
		    in
			(case poly_sdecs of
			     [] => map2 (fn (sbnd,sdec) => (SOME sbnd, CONTEXT_SDEC sdec)) (sbnds,sdecs)
			   | _ =>
				 let
				     val sig_poly = SIGNAT_STRUCTURE poly_sdecs
				     val labs = map (fn SDEC (l,_) => l) sdecs
				     val cons = map (fn SDEC(l,DEC_EXP(_,c,_,_)) => c | _ => elab_error "Rule 237") sdecs
				     fun mod_sig_help (l,c) =
					 let val modapp = MOD_APP(MOD_VAR var_valbind,MOD_VAR var_poly)
					     val inner_var = fresh_named_var "inner_valbind"
					     val outer_var = fresh_named_var "outer_valbind"

					     val temp_mod = MOD_STRUCTURE[SBND(it_lab,
									       BND_EXP(inner_var,
										       MODULE_PROJECT(modapp,l)))]
					     val temp_sig = SIGNAT_STRUCTURE [SDEC(it_lab,
										   DEC_EXP(inner_var,c,NONE,false))]
					     val bnd = BND_MOD(outer_var,true,
								MOD_FUNCTOR(a,var_poly,sig_poly,
									    temp_mod,temp_sig))
					     val dec = DEC_MOD(outer_var,true,
							       SIGNAT_FUNCTOR(var_poly,sig_poly,
									      temp_sig, a))
					 in (SBND(l,bnd),SDEC(l,dec))
					 end
				     val temp_mod = MOD_FUNCTOR(a,var_poly,sig_poly,
								MOD_STRUCTURE sbnds,
								SIGNAT_STRUCTURE sdecs)
				     val temp_sig = SIGNAT_FUNCTOR(var_poly,sig_poly,
								   SIGNAT_STRUCTURE sdecs, a)
				     val rest_sbnds_sdecs = map2 mod_sig_help (labs,cons)
				     val final_sbnds = ((SBND(lbl_valbind,BND_MOD(var_valbind,true,temp_mod)))::
							(map #1 rest_sbnds_sdecs))
				     val final_sdecs = ((SDEC(lbl_valbind,DEC_MOD(var_valbind,true,temp_sig))) ::
							(map #2 rest_sbnds_sdecs))
				 in map2 (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec)) (final_sbnds, final_sdecs)
				 end)
		    end
	    in if (is_irrefutable)
		then (irrefutable_case(),true)
	       else (refutable_case(),true)
	    end
        (* recursive value dec: i.e. functions *)
	| Ast.ValDec ([],rvblist,ref tyvars) =>
	    let val tyvar_stamp = new_stamp()
		val tyvars = map tyvar_strip tyvars
		local fun help tyvar =
		    let val type_str = Symbol.name tyvar
			val type_lab = symbol_label tyvar
			val is_eq =  ((size type_str >= 2) andalso
				      (String.substring(type_str,0,2) = "''"))
		    in  (type_lab,is_eq)
		    end
		    val temp = map help tyvars
		in  val sdecs1 = make_typearg_sdecs context temp
		end

		val var_poly = fresh_named_var "var_poly"
		val open_lbl = to_open(internal_label "!opened")
		val context' = add_context_mod(context,open_lbl,var_poly,SIGNAT_STRUCTURE sdecs1)

		fun rvb_help (pat:Ast.pat, exp:Ast.exp) =
		let
		    fun check_constraint (what:string, con:con, constraint:Ast.ty) : unit =
			let val con' = xty(context',constraint)
			in  if eq_con(context',con,con') then ()
			    else (error_region();
				  print ("val rec " ^ what ^ " has conflicting constraints\n");
				  print "Constraint type: "; pp_con con; print "\n";
				  print "Conflicting type: "; pp_con con'; print "\n";
				  print "Reduced constraint type: ";
				  pp_con (con_normalize(context',con)); print "\n";
				  print "Reduced conflicting type: ";
				  pp_con (con_normalize(context',con')); print "\n")
			end
		    val fun_con = fresh_named_con(context',"fun_con")
		    fun getvars (pat:Ast.pat) : Ast.symbol list =
			(case pat of
			    Ast.VarPat[s] => [s]
			|   Ast.ConstraintPat {pattern,constraint} =>
				(check_constraint("rhs", fun_con, constraint);
				 getvars pattern)
			|   Ast.LayeredPat {varPat,expPat} =>
				getvars varPat @ getvars expPat
			|   _ =>
				(error_region();
				 print "illegal pattern in val rec\n";
				 nil))
		    val vars = getvars (parse_pat context' pat)
		    val body_con = fresh_named_con (context',"body_con")
		    fun help (Ast.Rule{pat,exp}) : Ast.pat list * Ast.exp =
			let val parsed_pat =
				(case (parse_pat context' pat) of
				    Ast.ConstraintPat{pattern,constraint} =>
					(check_constraint("match", body_con, constraint);
					 pattern)
				|   pat => pat)
			in  ([parsed_pat],exp)
			end
		    val matches =
			(case exp of
			     Ast.FnExp rules => map help rules
			   | _ => parse_error "val rec requires a fn expression")
		    val ids = map (fn s =>
				   let val l = symbol_label s
				       val v = fresh_named_var(label2string l)
				   in  (l,v)
				   end) vars
		in  (ids, fun_con, body_con, matches)
		end
		val dec_list : ((label * var) list * con * con *
				(Ast.pat list * Ast.exp) list) list =
		    map (rvb_help o vb_strip) rvblist
	    in  (xfundec (context,dec_list,tyvar_stamp,sdecs1,var_poly,open_lbl),true)
	    end

	| Ast.ValDec (vblist,rvblist,tyvars) =>
	    (xdec (context, Ast.ValDec(vblist,[],tyvars)) @
	     xdec (context, Ast.ValDec([],rvblist,tyvars)),
	     true)

	| Ast.FunDec (fblist,ref tyvars) =>
	    let val tyvar_stamp = new_stamp()
		val tyvars = map tyvar_strip tyvars
		local fun help tyvar =
		    let val type_str = Symbol.name tyvar
			val type_lab = symbol_label tyvar
			val is_eq =  ((size type_str >= 2) andalso
				      (String.substring(type_str,0,2) = "''"))
		    in  (type_lab,is_eq)
		    end
		    val temp = map help tyvars
		in  val sdecs1 = make_typearg_sdecs context temp
		end

		val var_poly = fresh_named_var "var_poly"
		val open_lbl = to_open(internal_label "!opened")
		val context' = add_context_mod(context,open_lbl,var_poly,SIGNAT_STRUCTURE sdecs1)

		fun fb_help clause_list =
		    let
			val fun_con = fresh_named_con (context',"fun_con")
			val body_con = fresh_named_con (context',"body_con")
			fun help (Ast.Clause{pats, resultty, exp}, nameopt) =
			    let val _ =
				(case resultty of
				     SOME ty => if (eq_con(context',xty(context',ty),body_con))
						    then ()
						else (error_region();
						      print "conflicting result type constraints\n")
				   | NONE => ())
				val (name, (p, e)) =
				    (case pats of
					 {item=Ast.VarPat[s],fixity=NONE,...}::rest =>
					     (symbol_label s,
					      (parse_pats context' (map #item rest),exp))
				       | _ =>
					 (case (parse_pats context' (map #item pats)) of
					      (Ast.VarPat[s])::rest =>
						  (symbol_label s, (rest, exp))
					    | (Ast.AppPat{constr = Ast.VarPat[s], argument}::rest) =>
						  (symbol_label s, (argument::rest,exp))
					    | _ =>
						(error_region();
						 print "illegal pattern for function declaration\n";
						 reject "illegal pattern")))
				val _ =
				    (case nameopt of
					 NONE => ()
				       | SOME curName =>
					     if eq_label(name,curName)
						 then ()
					     else (error_region();
						   print "clauses don't all have same name\n"))
			    in  ((p,e), SOME name)
			    end
			val (matches, SOME id) = foldl_acc help NONE clause_list
			val fun_var = fresh_named_var (label2string id)
		    in ([(id,fun_var)], fun_con, body_con, matches)
		    end
		val dec_list : ((label * var) list * con * con *
				(Ast.pat list * Ast.exp) list) list =
		    map (fb_help o fb_strip) fblist
	    in  (xfundec (context,dec_list,tyvar_stamp,sdecs1,var_poly,open_lbl),true)
	    end


	| Ast.ExternDec (sym,ty) =>
	    let val var = gen_var_from_symbol sym
		val lab = symbol_label sym
		val con = xty(context,ty)
		val con = con_normalize(context,con)
	    in  ([(NONE, CONTEXT_EXTERN(lab,var,lab,con))],true)
	    end
	| Ast.SeqDec decs => packagedecs xdec' (context, true) decs
	| Ast.OpenDec pathlist =>
	      let fun help (i,path) =
		  (case (Context_Lookup_Labels(context,map symbol_label path)) of
		       SOME(_,PHRASE_CLASS_MOD(m,b,s,_)) =>
			   let
			       val str = foldl (fn (s,acc) => acc ^ (Symbol.name s))
				            "openlbl" path
                               val l = to_open(internal_label str)
			       val v = fresh_named_var "openvar"

			   in  SOME(SOME (SBND(l,BND_MOD(v,false,m))),
				    CONTEXT_SDEC(SDEC(l,DEC_MOD(v,false,s))))
			   end
		     | _ => (error_region(); print "unbound structure: ";
			     AstHelp.pp_path path; print "\n";
			     NONE))
	      in (List.mapPartial (fn x => x) (mapcount help pathlist), true)
	      end
	| Ast.TypeDec tblist => (xtybind(context,tblist),true)
	| Ast.DatatypeDec {datatycs,withtycs=[]} =>
	      let val sbnd_sdecs = xdatatype(context,datatycs)
	      in  (map (fn (sb,sd) => (SOME sb, CONTEXT_SDEC sd)) sbnd_sdecs, true)
	      end
	| Ast.DatatypeDec {datatycs,withtycs} =>
	      let val (dt,wt) = (case InfixParse.parse_datbind(datatycs,withtycs) of
				     SOME result => result
				   | NONE => (error_region();
					      print "cannot parse datbind\n";
					      error "cannot parse datbind"))
		  val dec = Ast.SeqDec[Ast.DatatypeDec{datatycs=dt,withtycs=[]},
				       Ast.TypeDec wt]
	      in  (xdec (context,dec), true)
	      end
	| Ast.StrDec strblist => xstrbinds(context,strblist)

	| Ast.ExceptionDec [] => parse_error "ExceptionDec []"
	| Ast.ExceptionDec [Ast.MarkEb (eb,r)] =>
	      let val _ = push_region r
		  val res = xdec (context, Ast.ExceptionDec [eb])
		  val _ = pop_region()
	      in (res, true)
	      end
	| Ast.ExceptionDec [Ast.EbGen {exn,etype}] =>
	      if ok_to_bind(context,exn) then
		let
		  (* N.B. Runtime/exn.c cares how exceptions are compiled. *)
		  val exn_str = Symbol.name exn
		  val id_bar = symbol_label exn
		  val var = fresh_named_var "exn_stamp"
		  val mkvar = fresh_named_var "mk"
		  val exnmodvar = fresh_named_var "exnmod"
		  val v = fresh_named_var "injectee"
		  val con = (case etype of
			       NONE => con_unit
			     | SOME ty => xty(context,ty))
		  val (mk_exp,mk_con) =
		      (case etype of
			   NONE => (EXN_INJECT(exn_str,VAR var,unit_exp), CON_ANY)
			 | SOME ty => (#1 (make_total_lambda(v,con,CON_ANY,
							     EXN_INJECT(exn_str, VAR var,VAR v))),
				       CON_ARROW([con], CON_ANY, false, oneshot_init TOTAL)))
		  val inner_mod = MOD_STRUCTURE[SBND(stamp_lab, BND_EXP(var,NEW_STAMP con)),
						SBND(mk_lab, BND_EXP(mkvar,mk_exp))]
		  val inner_sig = SIGNAT_STRUCTURE
		                       [SDEC(stamp_lab,DEC_EXP(var,CON_TAG con,NONE,false)),
					SDEC(mk_lab,DEC_EXP(mkvar,mk_con,NONE,false))]
		in ([(SOME(SBND(id_bar,BND_MOD(exnmodvar,false,inner_mod))),
		      CONTEXT_SDEC(SDEC(id_bar,DEC_MOD(exnmodvar,false,inner_sig))))],
		    true)
		end
	    else 
		(error_region(); 
		 print ("Rebinding of "^(Symbol.name exn)^" not permitted\n");
		 ([],true))
	| Ast.ExceptionDec [Ast.EbDef {exn: Symbol.symbol, edef: Ast.path}] =>
            if ok_to_bind(context,exn) then
	      (case (Context_Lookup_Labels(context,map symbol_label edef)) of
		   SOME(_,PHRASE_CLASS_MOD(m,_,s,_)) =>
		       let val id_bar = symbol_label exn
			   val path_mk_exp = MODULE_PROJECT(m,mk_lab)
			   val path_stamp_exp = MODULE_PROJECT(m,stamp_lab)
			   val path_mk_con = GetExpCon(context,path_mk_exp)
			   val path_stamp_con = GetExpCon(context,path_stamp_exp)
			   val itvar = fresh_named_var "exn_tag"
			   val mkvar = fresh_named_var "exn_injector"
			   val modvar = fresh_named_var "exn_structure"
			   val inner_mod = MOD_STRUCTURE[SBND(stamp_lab, BND_EXP(itvar,path_stamp_exp)),
							 SBND(mk_lab, BND_EXP(mkvar,path_mk_exp))]

			   val inner_sig = SIGNAT_STRUCTURE [SDEC(stamp_lab, DEC_EXP(itvar,path_stamp_con,NONE,false)),
							     SDEC(mk_lab, DEC_EXP(mkvar,path_mk_con,NONE,false))]
		       in ([(SOME(SBND(id_bar,BND_MOD(modvar,false,inner_mod))),
			     CONTEXT_SDEC(SDEC(id_bar,DEC_MOD(modvar,false,inner_sig))))],
			   true)
		       end
		 | _ => (error_region(); print "unbound exception: ???\n";
			 ([],true)))
	    else 
		(error_region(); 
		 print ("Rebinding of "^(Symbol.name exn)^" not permitted\n");
		 ([],true))
	| Ast.ExceptionDec eblist => (xdec (context,Ast.SeqDec(map (fn eb => Ast.ExceptionDec [eb]) eblist)), true)

        (* Rule 244 *)
	| Ast.LocalDec (dec1,dec2) =>
	      let
		  val (sbnd_ctxt_list1,pure1) = xdec' (context,dec1)
		  val (_,context') = add_context_sbnd_ctxts(context,sbnd_ctxt_list1)
		  val (sbnd_ctxt_list2,pure2) = xdec' (context',dec2)
		  val (_,context'') = add_context_sbnd_ctxts(context',sbnd_ctxt_list2)
		  fun genSubstFromSbnd (subst, self, SBND(l,BND_MOD(v,_,m))) =
		      let val self2 = join_path_labels(self, [l])
			  val subst = (case (mod2path m) of
					   SOME _ =>subst_add_modpath(subst, self2, m)
					 | _ => subst)
		      in  genSubstFromMod (subst, self2, m)
		      end
		    | genSubstFromSbnd (subst, _, _) = subst
		  and genSubstFromMod (subst, self, MOD_STRUCTURE sbnds) =
		      let fun folder (sbnd, subst) = genSubstFromSbnd(subst, self, sbnd)
		      in  foldl folder subst sbnds
		      end
		    | genSubstFromMod (subst, _, _) = subst
		  fun genSubstFromSbndTop (SBND(l,BND_MOD(v,_,m)),subst) = genSubstFromMod(subst, PATH(v, []), m)
		    | genSubstFromSbndTop (_, subst) = subst
		  val subst = foldl genSubstFromSbndTop empty_subst (List.mapPartial #1 sbnd_ctxt_list1)
		  fun getVarFromDec (DEC_EXP(v,_,_,_)) = v
		    | getVarFromDec (DEC_CON(v,_,_,_)) = v
		    | getVarFromDec (DEC_MOD(v,_,_)) = v
		  fun getDecFromSbndCtxt (_, CONTEXT_SDEC(SDEC(_,dec))) = SOME dec
		    | getDecFromSbndCtxt _ = NONE
		  val localVars = map getVarFromDec (List.mapPartial getDecFromSbndCtxt sbnd_ctxt_list1)
		  val localVars = Name.VarSet.addList(Name.VarSet.empty,localVars)
		  fun reduceSbndCtxt (sbndOpt, CONTEXT_SDEC(SDEC(l,dec))) =
		      (sbndOpt,CONTEXT_SDEC(SDEC(l,
		        (case dec of
			     DEC_EXP(v,c,eOpt,b) => DEC_EXP(v, con_subst(c,subst), eOpt, b)
			   | DEC_CON(_,_,NONE,_) => dec
			   | DEC_CON(v,k,SOME c,b) => DEC_CON(v,k,SOME(con_subst(c,subst)), b)
			   | DEC_MOD(v,b,s) => DEC_MOD(v,b,sig_subst(s,subst))))))
		    | reduceSbndCtxt sbndCtxtEntry = sbndCtxtEntry

		  val sbnd_ctxt_list1 = map reduceSbndCtxt sbnd_ctxt_list1
		  val sbnd_ctxt_list2 = map reduceSbndCtxt sbnd_ctxt_list2

		  fun renameSbndCtxt (SOME (SBND(l,bnd)),CONTEXT_SDEC(SDEC(_,dec))) =
		      let val v = getVarFromDec dec
			  val lbl = internal_label ("local_" ^ (Name.label2name' l))
		      in  (SOME (SBND(lbl,bnd)), CONTEXT_SDEC(SDEC(lbl,dec)))
		      end
		    | renameSbndCtxt arg = arg

		  val sbnd_ctxt_list1 = map renameSbndCtxt sbnd_ctxt_list1
		  val res = sbnd_ctxt_list1 @ sbnd_ctxt_list2
		  val pure = pure1 andalso pure2
	      in  (res,pure)
	      end


        (* Must augment translation context with fixity information *)
	| Ast.FixDec {fixity,ops} => let (* given symbol is in FixSymbol space *)
				       fun helper sym =
					   let val sym' = Symbol.varSymbol(Symbol.name sym)
					       val lab = symbol_label sym'
					   in (NONE, CONTEXT_FIXITY (lab, fixity))
					   end
				     in (map helper ops, true)
				     end

	| Ast.SigDec sigbs =>
              (map (fn sigb => (NONE,xsigb(context,sigb))) sigbs, true)
	| Ast.AbstypeDec {abstycs,withtycs,body} =>
	      let val ldec = Ast.DatatypeDec{datatycs = abstycs, withtycs = withtycs}
		  fun get_dec(Ast.MarkDb(db,_)) = get_dec db
		    | get_dec(Ast.Db{tyc,tyvars,...}) =
		      let val ty = Ast.ConTy(Ast.TypathHead(tyc),map Ast.VarTy tyvars)
		      in Ast.TypeDec[Ast.Tb{tyc=tyc,tyvars=tyvars,def=ty}]
		      end
		  val bdec = Ast.SeqDec((map get_dec abstycs) @ [body])
		  val desugared_dec = Ast.LocalDec(ldec,bdec)
	      in (xdec (context, desugared_dec),true)
	      end
	| Ast.OvldDec (sym,ignored_type,exp_list) =>
	      let val con_exp_list = map (fn e => let val (e,c,_) = xexp(context,e)
						  in (c,e)
						  end) exp_list
		  val l = symbol_label sym
		  fun fail () =
		      (error_region();
		       print "overloaded identifier with ambiguous default: "; AstHelp.pp_sym sym; print "\n";
		       debugdo (fn () =>
				(print "con_exp_list = "; pp_ovld (OVLD (con_exp_list, NONE)); print "\n"));
		       error "overloaded identifier with ambiguous default")
		  val default = ovld_default (l, con_exp_list, fail)
		  val v = gen_var_from_symbol sym
		  val ce = CONTEXT_OVEREXP(l,OVLD (con_exp_list, default))
	      in  ([(NONE,ce)],true)
	      end
	| Ast.ImportDec strlist => parse_error "import declaration not handled"

        (* translate declaration by dropping region information *)
	| Ast.MarkDec (dec,region) => let val _ = push_region region
					  val res = xdec' (context,dec)
					  val _ = pop_region()
				      in res
				      end)


    (* ---------------------------------------------------------
      ------------------ TYPE EXPRESSIONS----------------------
      --------------------------------------------------------- *)
    and xty (context, ty) : con =
      (case ty of
	 Ast.VarTy tyvar =>
	     let val sym = AstHelp.tyvar_strip tyvar
	     in (case (IlStatic.Context_Lookup_Labels(context,[symbol_label sym])) of
		     SOME(p,PHRASE_CLASS_CON (_,_,SOME inline_con,true)) => inline_con
		   | SOME(p,PHRASE_CLASS_CON (_,_,_,_)) => path2con p
		   | _ => (error_region(); print "unbound type constructor: ";
			   AstHelp.pp_sym sym; print "\n";
			   Error.dummy_type(context,"unbound_varty")))
	     end
       | Ast.MarkTy (ty,r) =>
	     let val _ = push_region r
		 val res = xty(context,ty)
		 val _ = pop_region()
	     in res
	     end
       | Ast.TupleTy(tys) =>
	     let fun loop _ [] = []
		   | loop n (a::rest) = (generate_tuple_symbol n,a)::(loop (n+1) rest)
	     in xty(context, Ast.RecordTy(loop 1 tys))
	     end
       | Ast.RecordTy(sym_ty_list) => let val lab_ty_list = map (fn (s,t) => (symbol_label s,t)) sym_ty_list
					  val sorted_lab_ty_list = sort_labelpair lab_ty_list
					  fun doer (lab,ty) = (lab,xty(context,ty))
				      in CON_RECORD(map doer sorted_lab_ty_list)
				      end

       | Ast.ConTy (typath,ty_list) =>
	     let 
		 val con_list = map (fn t => xty(context,t)) ty_list

		 val weird_case = ref false

                 exception Unbound

		 fun xmodpath (Ast.TypathHead sym) : mod * signat =
                     (case Context_Lookup_Labels(context,[symbol_label sym]) of
                         SOME (_,PHRASE_CLASS_MOD(m,_,s,_)) => (m,s)
		       | _ => raise Unbound)
		   | xmodpath (Ast.TypathProj (mp,sym)) =
                     let val (m,s) = xmodpath mp
		     in
                         (case Sig_Lookup(m,s,[symbol_label sym]) of
			      SOME (_,PHRASE_CLASS_MOD(m',_,s',_)) => (m',s')
			    | _ => raise Unbound)
		     end
		   | xmodpath (Ast.TypathApp (mp1,mp2)) =
	             (case (xmodpath mp1) of (f,SIGNAT_FUNCTOR(v,s1,s2,APPLICATIVE)) =>
			  let val (argmod,argsig) = xmodpath mp2

                              (* These two checks ensure that Fst(argsig) is really a subkind of
			         Fst(s1) after phase-splitting.  The first check is necessary because
				 the Fst_Sig operation throws away generative functor components.
				 By an invariant of the elaborator, it's already known that argsig is 
				 free of generative signatures, so there is no need to check that. *)

			      val _ = not(contains_generative_signature(context,argsig))
				      orelse raise Unbound
			      val _ = Sig_IsSub(context,Fst_Sig(context,argsig),
						Fst_Sig(context,s1)) orelse raise Unbound

			      val s = sig_subst(s2,subst_modvar(v,argmod))
			      val m = MOD_APP(f,argmod)
			  in (case PeelModSig(m,s) of
				  (_,PHRASE_CLASS_MOD(m,_,s,_)) => (m,s)
				| _ => raise Unbound)
			  end
		        | _ => raise Unbound)

                 fun xtypath (Ast.TypathHead sym) : con * kind =
		     (case Context_Lookup_Labels(context,[symbol_label sym]) of
			 SOME (_,PHRASE_CLASS_CON(c,k,copt,inline)) =>
                           ((case (copt,inline) of (SOME c',true) => c' | _ => c), k)
		       | _ =>
		          if Symbol.eq(sym, Symbol.tycSymbol "-->")
			      then (weird_case := true;
				    let fun split _ [] = error "need at least result type"
					  | split cons [c] = (rev cons,c)
					  | split cons (c::d) = split (c::cons) d
					val (arg_cons,res_con) = split [] con_list
				    in (CON_ARROW(arg_cons,res_con,true,oneshot_init PARTIAL),KIND)
				    end)
			  else raise Unbound)
		   | xtypath (Ast.TypathProj (mp,sym)) =
		     let val (m,s) = xmodpath mp
		     in
                         (case Sig_Lookup(m,s,[symbol_label sym]) of
			      SOME (_,PHRASE_CLASS_CON(c,k,copt,inline)) =>
                                ((case (copt,inline) of (SOME c',true) => c' | _ => c), k)
			    | _ => raise Unbound)
		     end
		   | xtypath _ = parse_error "Type path is neither TypathHead nor TypathProj"

		 val (con,k) = xtypath typath 
		     handle Unbound =>
			       (weird_case := true;
				error_region();
				print "Ill-formed type constructor: ";
				pp_typath typath;
				print "\n";
(*				pp_pathlist AstHelp.pp_sym' syms; print "\n"; *)
				(Error.dummy_type(context,"unbound_conty"),KIND))

	     in  if !weird_case then con else
		 (case (con_list,k) of
		      ([],KIND) => con
		    | (_,KIND_ARROW(n, KIND)) =>
				  if (n = length con_list)
				      then ConApply(true,con,con_list)
				  else (error_region();
					tab_region();
					print "type constructor ";
					pp_con con;
					print " wants ";
					print (Int.toString n);
					print " arguments, given ";
					print (Int.toString (length con_list));
					print "\n";
					Error.dummy_type(context,"badarity_type"))
	            | _ => (pp_kind k; print "\nand c = ";
		            pp_con con;
			    elab_error "external_label mapped to type with unexpected kind"))

	     end

     )
    (* ---------------------------------------------------------
      ------------------ TYPE DEFINITIONS ---------------------
      --------------------------------------------------------- *)
    and xtybind (context : context, tblist : Ast.tb list) : decresult =
       let
	 fun doer tb =
	   let
	     val (tyc,tyvars,def) = tb_strip tb
	     val tyvars = map tyvar_strip tyvars
	     val vars = map (fn s => gen_var_from_symbol s) tyvars
	     val tyvars_bar = map (fn s => symbol_label s) tyvars
	     val context' = (foldl (fn ((v,tv),c) =>
				    add_context_con(c,tv,v,KIND,NONE))
			     context (zip vars tyvars_bar))
	     val con' = xty(context',def)
	     val n = length tyvars
	     val (con,kind) = (case tyvars of
				 [] => (con',KIND)
			       | _ => (CON_FUN(vars,con'),KIND_ARROW(n,KIND)))
	     val var = gen_var_from_symbol tyc
	     val tyc_bar = symbol_label tyc
	   in (SOME(SBND(tyc_bar,BND_CON(var,con))),
	       CONTEXT_SDEC(SDEC(tyc_bar,DEC_CON(var,kind,SOME con,false))))
	   end
       in map doer tblist
       end

    (* ---------------------------------------------------------
      ------------------ TYPE DESCRIPTIONS ---------------------
      --------------------------------------------------------- *)
   and xtypedesc (context : context, sym_tyvar_tyopt_list, is_eq) : sdecs =
       let
	   fun loop [] = []
	     | loop ((sym,tyvars : Ast.tyvar list, tyopt)::rest) =
	       let
		   val type_label = symbol_label sym
		   val type_var = gen_var_from_symbol sym
		   val kind =
		       (case tyvars of
			    [] => KIND
			  | _ => KIND_ARROW(length tyvars,KIND))
		   fun doty ty =
		       if is_eq then error "eqtypedesc with definition" else
		       let
			   val vars = map (fn _ => fresh_var()) tyvars
			   val tyvars_bar = map (fn s => symbol_label (tyvar_strip s)) tyvars
			   val context' = (foldl (fn ((v,tv),c) =>
						  add_context_con(c,tv,v,KIND,NONE))
					   context (zip vars tyvars_bar))
			   val con' = xty(context',ty)
		       in  (case tyvars of
				[] => con'
			      | _ => (CON_FUN(vars,con')))
		       end
                   val conopt = mapopt doty tyopt
		   val type_sdec = SDEC(type_label, DEC_CON(type_var,kind,conopt,false))
	       in  
		   if not(is_eq) then type_sdec :: (loop rest) else
		   let
		       val eq_label = to_eq type_label
		       val eq_var = fresh_named_var (label2string eq_label)
		       val eq_dec =
			   (case tyvars of
				[] => DEC_EXP(eq_var,con_eqfun context (CON_VAR type_var),NONE,false)
			      | _ =>
				let val vpoly = fresh_named_var "vpoly"
				    val lbls = Listops.map0count (canonical_tyvar_label true) (length tyvars)
				    fun mapper l =
					let val eql = to_eq l
					    val v = fresh_var()
					    val sdec1 = SDEC(l,DEC_CON(v,KIND, NONE,false))
					    val sdec2 = SDEC(eql,DEC_EXP(fresh_var(),
									 con_eqfun context(CON_VAR v),
									 NONE, false))
					in  [sdec1,sdec2]
					end
				    val sdecs = List.concat (map mapper lbls)
				    val sigpoly = SIGNAT_STRUCTURE sdecs
				    val args = map (fn l => CON_MODULE_PROJECT(MOD_VAR vpoly,l)) lbls
				    val eq_con = con_eqfun context(CON_APP(CON_VAR type_var,
									   args))
				    val innersig = SIGNAT_STRUCTURE [SDEC(it_lab,
									  DEC_EXP(fresh_var(),
										  eq_con,NONE,false))]
				in  DEC_MOD(eq_var,true,SIGNAT_FUNCTOR(vpoly,sigpoly,innersig,TOTAL))
				end)

		       val eq_sdec = SDEC(eq_label, eq_dec)
		   in 
		       type_sdec :: eq_sdec:: (loop rest)
		   end
	       end
       in loop sym_tyvar_tyopt_list
       end

    (* ---------------------------------------------------------
      ------------ Signature bindings and expressions ----------
      --------------------------------------------------------- *)

     and xsigexp (context,sigexp) : signat = xsigexp' false (context,sigexp)

     and xsigexp' (just_static : bool) (context,sigexp) : signat =
       let val xsigexp = xsigexp' just_static
	   val xspec = xspec' just_static
       in
       (case sigexp of
	  Ast.VarSig s => (case (IlStatic.Context_Lookup_Labels(context,[symbol_label s])) of
			       SOME(_,PHRASE_CLASS_SIG (v,_)) => 
				   if just_static then Fst_Sig(context,SIGNAT_VAR v) else SIGNAT_VAR v
			     | _ => (error_region();
				     print "unbound signature: ";
				     AstHelp.pp_sym s;
				     print "\n";
				     SIGNAT_STRUCTURE []))
	| Ast.BaseSig speclist => SIGNAT_STRUCTURE (xspec(context,speclist) @ [ident_sdec])
	| Ast.MarkSig (s,r) => let val _ = push_region r
				   val res = xsigexp(context,s)
				   val _ = pop_region()
			       in res
			       end
	| Ast.AugSig (s, []) => xsigexp(context,s)
	| Ast.AugSig (s, ((Ast.WhStruct (syms1,syms2))::rest)) =>
	      let val (m2,s2,_) = xstrexp(context,Ast.VarStr syms2, Ast.NoSig)
		  val s = xsigexp(context,Ast.AugSig(s,rest))
		  val s2_is_struct = (case reduce_signat context s2 of
					  SIGNAT_STRUCTURE _ => true
					| _ => false)
		  val (context,makesig,sdecs_opt) =
		      (case reduce_signat context s of
			   SIGNAT_STRUCTURE sdecs => (context,SIGNAT_STRUCTURE, SOME sdecs)
			 | s' as SIGNAT_RDS (v,sdecs) => (add_context_mod'(context,v,Fst_Sig(context,s')),
							  fn sdecs => SIGNAT_RDS(v,sdecs),
							  SOME sdecs)
			 | _ => (context,SIGNAT_STRUCTURE, NONE))
	      in  case (s2_is_struct,sdecs_opt) of
		  (false, _) => (error_region();
				 print "rhs of where-structure is a non-structure\n";
				 s)
		| (_,NONE) => (error_region();
			       print "can't where a non-structure signature\n";
			       s)
		| (_, SOME sdecs) =>
			   makesig(Signature.xsig_where_structure
				   (context,sdecs,map symbol_label syms1,m2,s2))
	      end
	| Ast.AugSig (s, ((Ast.WhType(syms, tyvars, ty))::rest)) =>
	      let val s = xsigexp(context,Ast.AugSig(s,rest))
		  val mjunk = fresh_named_var "mjunk"
		  val (context,makesig,sdecs_opt) =
		      (case reduce_signat context s of
			   SIGNAT_STRUCTURE sdecs => (context,SIGNAT_STRUCTURE, SOME sdecs)
			 | s' as SIGNAT_RDS (v,sdecs) => (add_context_mod'(context,v,Fst_Sig(context,s')),
							  fn sdecs => SIGNAT_RDS(v,sdecs),
							  SOME sdecs)
			 | _ => (context,SIGNAT_STRUCTURE, NONE))
		  val ctxt' = add_context_mod'(context,mjunk,s)
	      in  (case sdecs_opt of 
		       SOME sdecs =>
			   (case (Sig_Lookup(MOD_VAR mjunk,
					     GetModSig(ctxt',MOD_VAR mjunk),
					     map symbol_label syms)) of
				SOME(labels,PHRASE_CLASS_CON(_,k,_,_)) =>
				    let val sym_vars = map (fn tv =>
							    let val sym = AstHelp.tyvar_strip tv
							    in  (sym, gen_var_from_symbol sym)
							    end) tyvars
					fun folder ((sym,var),context) =
					    add_context_sdec(context,SDEC(symbol_label sym,
									  DEC_CON(var,
									      KIND, NONE,false)))
					val context = foldl folder context sym_vars
					val c = xty(context,ty)
					val c = (case sym_vars of
						     [] => c
						   | _ => CON_FUN(map #2 sym_vars, c))
				    in makesig(Signature.xsig_where_type(context,sdecs,labels,c,k))
				   end
			      | _ => (error_region();
				      print "Can't where type a non-type component\n";
				      s))
		     | NONE => (error_region();
				print "Can't where type a non-structure signature\n";
				s))
	      end
	| Ast.FunSig((symOpt,sigarg),sigres,astarrow) => 
          let val (strid,v) = (case symOpt of SOME sym => (symbol_label sym, gen_var_from_symbol sym)
	                                | NONE => (functor_arg_lab, fresh_named_var "functor_arg"))
	      val sig1 = xsigexp(context,sigarg)
              val arrow = (case astarrow of Ast.Generative => GENERATIVE
	                     | Ast.Applicative =>
			       if contains_generative_signature(context,sig1)
				   then (error_region();
					 print "Argument of applicative functor signature cannot contain generative functor signatures\n";
					 GENERATIVE)
			       else APPLICATIVE)
	      val context' = add_context_mod(context,strid,v,sig1)
	      val sig2 = xsigexp(context',sigres)
	  in 
	      SIGNAT_FUNCTOR(v,sig1,sig2,arrow)
	  end
        | Ast.RdsSig speclist =>
	  if just_static then xsigexp(context, Ast.BaseSig speclist) else
	  let 
	      val recvar = fresh_named_var "var_rds"
	      val reclab = to_open(var2label recvar)
	      val fstsdecs = xspec' true (context,speclist)
	      val fstsig = SIGNAT_STRUCTURE fstsdecs
	      val context' = add_context_mod(context,reclab,recvar,fstsig)
	      val sdecs = xspec' false (context',speclist)

	      val fstsdecs' = Fst_Sdecs(context',sdecs)

              (* The bad case here arises only if you have an rds like

                 sig rec ... type t = u ... type u end,

                 where u is already defined in the context
                 (and so the first pass of elaborating the rds goes through fine),
		 but on the second pass u is taken mean the one specified
		 later on inside the rds. *)

	      val _ = if Name.VarSet.member(sdecs_free(fstsdecs'),recvar)
			  then reject "Ill-formed rds: contains static-on-static dependency"
		      else ()

              (* sanity check, could be removed eventually, note that it
	         is done in the original context since the static part of
                 sdecs should not refer to recvar *)

	      val _ = if Sdecs_IsEqual(context,fstsdecs,fstsdecs') 
		         orelse get_error() = Error
	              then ()
		      else elab_error "RdsSig case of xsigexp producing ill-formed rds"

	  in
	      SIGNAT_RDS(recvar, sdecs @ [ident_sdec])
	  end
     )
     end

     and xsigb(context,Ast.MarkSigb(sigb,r)) : context_entry =
	 let val _ = push_region r
	     val res = xsigb(context,sigb)
	     val _ = pop_region()
	 in res
	 end
       | xsigb(context,Ast.Sigb{name,def}) =
	 let val v = gen_var_from_symbol name
	 in CONTEXT_SIGNAT(symbol_label name,v,
			   xsigexp(context,def))

	 end

    (* ---------------------------------------------------------
      ------------------ SIGNATURE SPECIFICATIONS --------------
      --------------------------------------------------------- *)

     and xspec (orig_ctxt, specs : Ast.spec list) : sdecs = xspec' false (orig_ctxt,specs)

     and xspec' (just_static : bool) (orig_ctxt, specs : Ast.spec list) : sdecs =
       let
	 val xsigexp = xsigexp' just_static
	 datatype sdecs_tag = ADDITIONAL of sdecs | ALL_NEW of sdecs
	 fun xspec1 context prev_sdecs spec : sdecs_tag =
	     case spec of
		 (Ast.ValSpec vtlist) => (* Rules 257 - 258 *)
	       if just_static then ADDITIONAL [] else
	       let
		 fun doer (sym,ty) = 
          	  if ok_to_bind(context,sym) then
	           (case (free_tyvar_ty(ty,fn _ => false)) of
			[] => SOME(SDEC(symbol_label sym,
				   DEC_EXP(gen_var_from_symbol sym,xty(context,ty),NONE,false)))
		    | ftv_sym =>
			let
			    val varpoly = fresh_named_var "var_poly"
			    fun help (n,tv_sym) =
				let val type_lab = symbol_label tv_sym
				    val eq_lab = to_eq type_lab
				    val type_var = fresh_var()
				    val eq_var = fresh_var()
				    val type_str = Symbol.name tv_sym
				    val is_eq =  ((size type_str >= 2) andalso
						  (String.substring(type_str,0,2) = "''"))
				    val eq_con = con_eqfun context (CON_VAR type_var)
				    val type_sdec = SDEC(type_lab,DEC_CON(type_var,KIND,
									  NONE,false))
				    val eq_sdec = SDEC(eq_lab, DEC_EXP(eq_var,eq_con,NONE,false))
				in if is_eq
				    then [type_sdec,eq_sdec]
				   else [type_sdec]
				end
			    val sigpoly = SIGNAT_STRUCTURE(flatten(mapcount help ftv_sym))
			    val context' = add_context_mod(context, to_open(internal_label "opened"),
							   varpoly, sigpoly)
			    val con = xty(context',ty)
			    val fsig = SIGNAT_FUNCTOR(varpoly,sigpoly,
						      SIGNAT_STRUCTURE([SDEC(it_lab,
									     DEC_EXP(fresh_var(),
										     con,NONE,false))]),
						      TOTAL)
			in SOME(SDEC(symbol_label sym, DEC_MOD(fresh_named_var "unused",true,fsig)))
			end)
		   else (error_region(); 
			 print ("Rebinding of "^(Symbol.name sym)^" not permitted\n");
			 NONE)
	       in ADDITIONAL(List.mapPartial doer vtlist)
	       end
	   | (Ast.StrSpec (sym_sigexp_path_list)) =>
	       let
		   fun doer (sym,sigexp, NONE) =
		       let val s = xsigexp(context,sigexp)
			   val produce_nothing = just_static andalso
			         (case s of SIGNAT_FUNCTOR (_,_,_,GENERATIVE) => true | _ => false)
		       in  if produce_nothing then NONE
			   else SOME(SDEC(symbol_label sym,DEC_MOD(fresh_var(),false,s)))
		       end
		     | doer (sym,sigexp, SOME path) =
                       (* 
			  This case is just treating the spec
                            structure A : SIG = Path
                          as syntactic sugar for
                            include (sig structure A : SIG end where A = Path)
		       *)
		       let val s = xsigexp(context,Ast.AugSig(Ast.BaseSig[Ast.StrSpec[(sym,sigexp,NONE)]],
							      [Ast.WhStruct([sym],path)]))
		       in (case s of SIGNAT_STRUCTURE [sdec,_] => SOME(sdec)
		           | _ => elab_error "Error in Ast.StrSpec case of xspec")
		       end
	       in ADDITIONAL(List.mapPartial doer sym_sigexp_path_list)
	       end
	   | (Ast.IncludeSpec sigexp) =>
	       (case (reduce_signat context (xsigexp(context,sigexp))) of
		  SIGNAT_STRUCTURE sdecs => ADDITIONAL (butlast sdecs)
		| _ => (error_region();
			print "May not 'include' non-structure signature\n";
			ADDITIONAL []))
	   | (Ast.FctSpec sym_fsigexp_list) =>
		 xspec1 context prev_sdecs 
	           (Ast.StrSpec(map (fn (sym,sigexp) => (sym,sigexp,NONE)) sym_fsigexp_list))
	   | (Ast.TycSpec (typdesc_list,is_eq)) => 
		 ADDITIONAL(xtypedesc(context,typdesc_list, is_eq andalso not just_static))
	   | (Ast.ExceSpec exlist) => (* Rules 260 - 261 *)
	         if just_static then ADDITIONAL [] else
		 let fun doer (sym,tyopt) =
                   if ok_to_bind(context,sym) then
			let 
			    val (mk_con,stamp_con) =
			  (case tyopt of
			     NONE => (CON_ANY, CON_TAG con_unit)
			   | (SOME ty) => let val con = xty(context,ty)
					  in (CON_ARROW([con],CON_ANY,false,oneshot_init TOTAL),
					      CON_TAG con)
					  end)
			    val inner_sig =
			      SIGNAT_STRUCTURE([SDEC(stamp_lab,
						    DEC_EXP(fresh_var(),stamp_con,NONE,false)),
					       SDEC(mk_lab,
						    DEC_EXP(fresh_var(),mk_con,NONE,false))])
			in SOME(SDEC(symbol_label sym, DEC_MOD(fresh_var(),false,inner_sig)))
			end
		   else (error_region(); 
			 print ("Rebinding of "^(Symbol.name sym)^" not permitted\n"); 
			 NONE)
		 in ADDITIONAL(List.mapPartial doer exlist)
		 end
          
           (* For datatype specs: What we would like to do in principle is to take the sdecs
	      out of the open-labeled module sdecs in which they come packaged from xdatatype,
	      so that we may enable the invariant that signatures never have any open-labeled
	      sdecs in them (aside from inner datatype modules).  In order to do this, we would
	      need to do some substitutions, since the sdecs that come out of xdatatype have
	      (hierarchical, of course) dependencies on one another.  Instead, we just leave them
              in their open-labeled shells, and we make signature matching aware of how to match
              against datatype sdecs. -Derek *)

	   | (Ast.DataSpec {datatycs, withtycs = []}) =>
		if just_static
		    then ADDITIONAL(Datatype.compile_static(context,datatycs))
		else
	            let val sbnd_sdecs = xdatatype (context,datatycs)
			val sdecs = map #2 sbnd_sdecs
		    in  ADDITIONAL sdecs
		    end
		
(*
   Some broken code for the DataSpec case:

                 let fun strip (sdec as SDEC(_,DEC_MOD(_,_,SIGNAT_STRUCTURE sdecs'))) = sdecs'
		      | strip _ = elab_error "DataSpec case"
		 in  ADDITIONAL(map_concat strip sdecs)
                 end
*)
(*
   
		    (case sdecs of
			[SDEC(lab,DEC_MOD(_,_,SIGNAT_STRUCTURE sdecs'))] =>
			    if is_open lab then ADDITIONAL sdecs' else ADDITIONAL sdecs
		      | _ => ADDITIONAL sdecs)
*)

           (* Actually, withtype specs are supported, but InfixParse.parse_datbind macro-expands 
	      them out prior to this point. *)
	   | (Ast.DataSpec {datatycs, withtycs}) => parse_error "withtype specs not supported"
	   | (Ast.ShareTycSpec paths) => ALL_NEW(Signature.xsig_sharing_types
						 (orig_ctxt,prev_sdecs,mapmap symbol_label paths))
	   | (Ast.ShareStrSpec paths) => (ALL_NEW(Signature.xsig_sharing_structures
						 (orig_ctxt,prev_sdecs,mapmap symbol_label paths)))
	   | (Ast.MarkSpec (s,r)) => let val _ = push_region r
					 val res = xspec1 context prev_sdecs s
					 val _ = pop_region ()
				     in res
				     end

         fun loop ctxt prev_sdecs [] = prev_sdecs
           | loop ctxt prev_sdecs ((Ast.DataSpec{datatycs,withtycs=wt as (_::_)})::specrest) =
		let val (dt,wt) = (case InfixParse.parse_datbind(datatycs,wt) of
				       SOME result => result
				     | NONE => (error_region();
						print "cannot parse datspec\n";
						error "cannot parse datspec"))
		    val dspec = Ast.DataSpec{datatycs=dt,withtycs=[]}
		    fun strip tb = let val (s,tvs,ty) = AstHelp.tb_strip tb
				   in  (s,tvs,SOME ty)
				   end
		    val wspec = Ast.TycSpec (map strip wt,false)
		in  loop ctxt prev_sdecs (dspec::wspec::specrest)
		end
           | loop ctxt prev_sdecs (spec::specrest) =
	     (case (xspec1 ctxt prev_sdecs spec) of
		  ADDITIONAL sdecs' =>
		      let val ctxt' = add_context_sdecs(ctxt,sdecs')
		      in loop ctxt' (prev_sdecs @ sdecs') specrest
		      end
		| ALL_NEW sdecs' =>
		      let val ctxt' = add_context_sdecs(orig_ctxt,sdecs')
		      in loop ctxt' sdecs' specrest
		      end)

           val sdecs = loop orig_ctxt [] specs

       in
	   if no_dups compare_label (map (fn (SDEC(lab,_)) => lab) sdecs)
	   then sdecs
	   else (error_region();
		 print "Cannot specify two components with the same name in a signature\n";
		 [])
       end


(*
    (* ---------------------------------------------------------
      ------------------ FUNCTOR BINDINDS ---------------------
      --------------------------------------------------------- *)
     and xfctbind (context : context, fctbs : Ast.strb list) : decresult =
	 let
	     fun help (context,(name,def)) : decresult =
		 (case def of
		      (Ast.VarFct (path,Ast.NoSig)) =>
			  (case (Context_Lookup_Labels(context,map symbol_label path)) of
			       SOME(path,PHRASE_CLASS_MOD(m,_,s as (SIGNAT_FUNCTOR _),_)) =>
				   let val l = symbol_label name
				       val v = fresh_named_var "functor_var"
				   in [(SOME(SBND(l,BND_MOD(v,false,m))),
					CONTEXT_SDEC(SDEC(l,DEC_MOD(v,false,s))))]
				   end
			     | _ => (error_region();
				    print "unbound functor: ";
				    AstHelp.pp_path path;
				    print "\n"; []))
		    | (Ast.VarFct (path,_)) => parse_error "functor signatures not handled"
		    | (Ast.BaseFct {params=[(argnameopt,sigexp)],body,constraint}) =>
			  let
			      val arglabel = (case argnameopt of
						  NONE => to_open(internal_label "FunctorArg")
						| SOME s => symbol_label s)
			      val funid = symbol_label name
			      val argvar = fresh_named_var "funct_arg"
			      val signat = xsigexp(context,sigexp)
			      val context' = add_context_mod(context,arglabel,argvar, signat)
			      val (m',s',pure) = xstrexp(context',body,constraint)
			      val v = fresh_named_var "functor_var"
			      val arrow = if pure then APPLICATIVE else GENERATIVE
			      val sbnd = SBND(funid,BND_MOD(v,false,MOD_FUNCTOR(arrow, argvar,signat,m',s')))
			      val sdec = SDEC(funid,DEC_MOD(v,false,SIGNAT_FUNCTOR(argvar,signat,s',arrow)))
			  in [(SOME sbnd, CONTEXT_SDEC sdec)]
			  end
		    | (Ast.BaseFct {params=[],body,constraint}) => parse_error "Functor of order 0"
		    | (Ast.BaseFct _) => parse_error "No higher order functors"
		    | (Ast.LetFct _) => parse_error "No lets in functor bindings"
		    | (Ast.AppFct _) => parse_error "No higher order functors"
		    | (Ast.MarkFct (f,r)) => let val _ = push_region r
						 val res = help (context,(name,f))
						 val _ = pop_region()
					     in res
					     end)
	     val help = fn x => (resolve_overloads (fn () => help x), true)
	 in  #1(packagedecs help (context,true) (map fctb_strip fctbs))
	 end
*)

    (* ---------------------------------------------------------
      ------------------ STRUCTURE EXPRESSION -----------------
      --------------------------------------------------------- *)

     and xstrexp_seal (context : context, strb : Ast.strexp, sigexp : Ast.sigexp) : mod * signat * bool =
	 let val sig_target = xsigexp(context,sigexp)
	     val (module,sig_actual,pure) = xstrexp(context,strb,Ast.NoSig)
	 in  if (Sig_IsSub(context, sig_actual, sig_target))
		 then (MOD_SEAL(module,sig_target), sig_target, pure)
	     else let val mod_result = Signature.xcoerce_seal(context,module,sig_actual,sig_target)
		  in  (mod_result, sig_target, pure)
		  end
	 end

     and xstrexp (context : context, strb : Ast.strexp, Ast.StrongOpaque sigexp) : (mod * signat * bool) =
	 let val (m,s,pure) = xstrexp_seal(context,strb,sigexp)
	 in  (m,s,false)
	 end

       | xstrexp (context, strb, Ast.WeakOpaque sigexp) =
	 xstrexp_seal(context,strb,sigexp)

       | xstrexp (context, strb, Ast.Transparent sigexp) =
	 let val (module,signat,pure) = xstrexp(context,strb,Ast.NoSig)
	     val sig' = xsigexp(context,sigexp)
	     val (mod_result,sig_result) =
		 Signature.xcoerce_transparent (context,module,signat,sig')
	 in  (mod_result, sig_result, pure)
	 end

       | xstrexp (context, strb, Ast.NoSig) =
	(case strb of
	     Ast.VarStr path =>
		 (case Context_Lookup_Labels(context,map symbol_label path) of
		      SOME (_,PHRASE_CLASS_MOD(m,_,s,_)) => (m,s,true)
		    | _ => (error_region();
			    print "unbound module: ";
			    AstHelp.pp_path path;
			    print "\n";
			    dummy_strexp_result))
	   | Ast.AppStr (_,[]) => parse_error "AppStr with no arguments"
	   | Ast.AppStr (funpath,strexpbools) =>
             (case (Context_Lookup_Labels(context,map symbol_label funpath)) of
		  SOME(_,PHRASE_CLASS_MOD(f,_,SIGNAT_FUNCTOR(var1,sig1,sig2,arrow),_)) =>
		   let
                     exception EscapeFromAppStr

                     (* Elaborates a single functor application. *)
		     fun dofunctorapp (context,f,var1,sig1,sig2,arrow,strexp) : mod * signat * bool =
		       let
			  val (argmod,argsig,argpure) = xstrexp(context,strexp,Ast.NoSig)
			  val argpathOpt = mod2path argmod
			  val pure = (arrow = APPLICATIVE) andalso argpure
		       in
			  if isSome(argpathOpt) andalso Sig_IsSub(context,argsig,sig1)
			      then (MOD_APP(f,argmod),
				    sig_subst(sig2,subst_modvar(var1,argmod)),
				    pure)
			  else
			      let val var_coerced = fresh_named_var "functorArgCoerced"
				  val (mod_coerced,sig_coerced) =
				      (case argpathOpt of
					   SOME argpath => 
					       Signature.xcoerce_functor(context,argpath,argsig,sig1)
					 | NONE => 
					       Signature.xcoerce_transparent(context,argmod,argsig,sig1))
				  val path_coerced = 
				      case Context_Lookup_Path(add_context_mod'(context,var_coerced,sig_coerced),
							       PATH(var_coerced,[])) of
					  SOME(_,PHRASE_CLASS_MOD(m,_,_,_)) => m
					| _ => elab_error "AppStr case"
				  val mod_result = MOD_APP(f,path_coerced)
				  val sig_result = sig_subst(sig2,subst_modvar(var1,path_coerced))
			      in
				  (make_existential_mod(var_coerced,mod_coerced,mod_result),
				   make_existential_sig(var_coerced,sig_coerced,sig_result),
				   pure)
			      end
		       end

                     (* Elaborates curried functor application F(arg1)...(argn) by first elaborating
                        F(arg1)...(argn-1) to a module M, then returning
                        [hidden = M, visible* = hidden(argn)], except that hidden must be peeled before applying to argn.
                        The argument strexps is assumed to be in reverse order (i.e. [argn,...,arg1]).
                      *)
		     fun docurriedapps (strexps) : mod * signat * bool = (
		       case strexps of
                           [strexp] => dofunctorapp(context,f,var1,sig1,sig2,arrow,strexp)
			 | (strexp::strexps) => 
                           let val (mod_curried,sig_curried,pure) = docurriedapps strexps
			       val var_curried = fresh_named_var("curriedFunctorApplication")
			       val context' = add_context_mod'(context,var_curried,sig_curried)
			       val (f,(var1,sig1,sig2,arrow)) = 
				     (case Context_Lookup_Path(context',PATH(var_curried,[])) of
					  SOME(_,PHRASE_CLASS_MOD(f,_,SIGNAT_FUNCTOR fsig,_)) => (f,fsig)
					| _ => (error_region();
						print "cannot apply a non-functor\n";
						raise EscapeFromAppStr))
			       val (mod_result,sig_result,pure') = dofunctorapp(context',f,var1,sig1,sig2,arrow,strexp)
			   in (make_existential_mod(var_curried,mod_curried,mod_result),
			       make_existential_sig(var_curried,sig_curried,sig_result),
			       pure andalso pure')
			   end
                     )

		   in
		       docurriedapps(rev(map #1 strexpbools))
		       handle EscapeFromAppStr => dummy_strexp_result
		   end

		| SOME _ => (error_region();
			     print "cannot apply a non-functor\n";
			     dummy_strexp_result)
		| NONE => (error_region();
			   print "functor identifier not bound: ";
			   AstHelp.pp_path funpath;
			   print "\n";
			   dummy_strexp_result))
	   | Ast.LetStr (dec,strexp) => 
		 let val var1 = fresh_named_var "letbound"
		     val (sbnd_ctxt_list,pure1) = xdec' (context,dec)
		     val (mod1,sig1) = sbnd_ctxt_list2modsig sbnd_ctxt_list
		     val context = add_context_mod(context,open_unseen_lab,var1,sig1)
		     val context = add_context_fixity_entries(context,sbnd_ctxt_list)
		     val (mod2,sig2,pure2) = xstrexp(context,strexp,Ast.NoSig)
		     val final_mod = make_existential_mod(var1,mod1,mod2)
		     val final_sig = make_existential_sig(var1,sig1,sig2)
		     val pure = pure1 andalso pure2
		 in (final_mod, final_sig, pure)
		 end
	   | Ast.BaseStr dec =>
		 let
		     val (sbnd_ctxt_list,pure) = xdec' (context,dec)
		     val sbnds = (List.mapPartial #1 sbnd_ctxt_list) @ [ident_sbnd]
		     val sdecs = (List.mapPartial (fn (_,CONTEXT_SDEC sdec) => SOME sdec
		                                    | _ => NONE) sbnd_ctxt_list) @ [ident_sdec]
		 in (MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE sdecs, pure)
		 end
	   | Ast.ConstrainedStr (strexp,constraint) => xstrexp(context,strexp,constraint)
	   | Ast.MarkStr (strexp,r) => let val _ = push_region r
					   val res = xstrexp(context,strexp,Ast.NoSig)
					   val _ = pop_region()
				       in res
				       end
	   | (Ast.BaseFct {params=[],body,constraint}) => parse_error "Functor has no parameters"
           | (Ast.BaseFct {params,body,constraint}) =>
		let
                    fun doit ((argnameopt,sigexp),context) =
		        let val arglabel = (case argnameopt of
						NONE => functor_arg_lab
					      | SOME s => symbol_label s)
			    val argvar = fresh_named_var "functor_arg"
			    val signat = xsigexp(context,sigexp)
			    val context' = add_context_mod(context,arglabel,argvar,signat)
			in  ((argvar,signat),context')
			end
		    val (args,context') = foldl_acc doit context params
		    val (m',s',pure) = xstrexp(context',body,constraint)
		    fun tomodsig [(v,s)] = 
			let val arrow = if not pure orelse contains_generative_signature(context',s)
					    then GENERATIVE
					else APPLICATIVE
			in (MOD_FUNCTOR(arrow,v,s,m',s'),SIGNAT_FUNCTOR(v,s,s',arrow))
			end
		      | tomodsig ((v,s)::args) =
			let val (fmod,fsig) = tomodsig args
			    val arrow = if contains_generative_signature(context',s)
					    then GENERATIVE
					else APPLICATIVE
			in  (MOD_FUNCTOR(arrow,v,s,fmod,fsig),
			     SIGNAT_FUNCTOR(v,s,fsig,arrow))
			end
		    val (fmod,fsig) = tomodsig args
		in
                    (fmod,fsig,pure)
		end
      )



    (* ---------------------------------------------------------
      ------------------ STRUCTURE BINDINGS --------------------
      --------------------------------------------------------- *)
     and xstrbinds (context : context, strbs : Ast.strb list)
	 : decresult * bool =
       let val strbs = map strb_strip strbs
	   val allpure = ref true
	   fun help (n,(strexp,constraint)) =
	       let val v = fresh_named_var "strbindvar"
		   val l = symbol_label n
		   val (m,s,pure) = resolve_overloads (fn () => xstrexp(context,strexp,constraint))
		   val _ = allpure := (pure andalso !allpure)
	       in (SOME(SBND(l,BND_MOD(v,false,m))),
		   CONTEXT_SDEC(SDEC(l,DEC_MOD(v,false,s))))
	       end
       in (map help strbs, !allpure)
       end


    and xeq' (ctxt : context, bool : (con * exp * exp * (con -> con)) option, argcon : con)
	: (exp * con) option =
	let
	    fun vector_eq ctxt =
		let val (m,s) = IlUtil.vector_eq ctxt
		in  polyfun_inst(ctxt,m,s)
		end
	in  Equal.compile{polyinst_opt = polyinst_opt,
			  vector_eq = vector_eq,
			  word8vector_eq = IlUtil.word8vector_eq,
			  bool = bool,
			  context = ctxt,
			  con = argcon}
	end

    and xeq (ctxt : context, argcon : con) : (exp * con) option = xeq' (ctxt, NONE, argcon)

    fun xdec_as_topspec (context : context, dec : Ast.dec) : entries =
	let val decresult = xdec (context, dec)
	    val (sbndopts, entries) = Listops.unzip decresult
	    val _ = if List.exists isSome sbndopts then
			error "interface contains an sbnd"
		     else ()
	in  entries
	end

    fun xtopspec (ctxt : context, topspec : Ast.topspec) : entries =
	(case topspec
	   of Ast.TopSpec specs =>
		let val sdecs = xspec (ctxt, specs)
		    val entries = map CONTEXT_SDEC sdecs
		in  entries
		end
	    | Ast.SigSpec x => xdec_as_topspec (ctxt, Ast.SigDec x)
	    | Ast.OvldSpec x => xdec_as_topspec (ctxt, Ast.OvldDec x)
	    | Ast.FixSpec x => xdec_as_topspec (ctxt, Ast.FixDec x)
	    | Ast.ExternSpec x => xdec_as_topspec (ctxt, Ast.ExternDec x)
	    | Ast.SeqSpec topspecs =>
		let fun folder (topspec : Ast.topspec,
				ctxt : context) : entries * context =
			let val entries = xtopspec (ctxt,topspec)
			    val ctxt = add_context_entries (ctxt,entries)
			in  (entries,ctxt)
			end
		    val (entries_list,_) = foldl_acc folder ctxt topspecs
		    val entries = List.concat entries_list
		in  entries
		end)

    (*
	Exported functions set up Error state, resolve overloading and
	flexible records, and instantiate unset tyvars.
    *)
    fun overload_wrap (ctxt:context) (fp:filepos) (xobj:'a -> 'b)
		      (rewriter:IlUtil.handler -> 'b -> 'b) (arg:'a) : 'b option =
      let
	fun flex_help (l,ref(FLEXINFO(stamp,false,rdecs)),fieldc,eshot) =
	    (error_region();
	     print "Unresolved flex record with label: ";
	     pp_label l; print "\nrdecs are:";
	     pp_con (CON_RECORD rdecs); print "\nfieldc is:";
	     pp_con fieldc; print "\n")
	  | flex_help (l,ref(FLEXINFO(_,true,rdecs)),fieldc,eshot) =
	    let val v = fresh_named_var "flex_eta_var"
		val recc = CON_RECORD rdecs
		val body = RECORD_PROJECT(VAR v,l,recc)
		val (e,c) = make_lambda(v,recc,fieldc,body)
	    in oneshot_set(eshot,e)
	    end
	  | flex_help (l,ref(INDIRECT_FLEXINFO fr),fieldc,eshot) = flex_help (l,fr,fieldc,eshot)
	fun eq_help (reg,tyvar) =
	    (case tyvar_eq_hole tyvar
	       of NONE => ()
	        | SOME os =>
		  (case oneshot_deref os
		     of SOME _ => ()
		      | NONE =>
			 let
			     val _ = (push_region reg; error_region(); pop_region();
				      print "unresolved equality"; print "\n")
			     val _ = debugdo (fn () =>
					      let
						  fun printOpt f NONE = print "NONE"
						    | printOpt f (SOME x) = (print "SOME "; f x)
					      in
						  print " tyvar = "; print (tyvar2string tyvar); print "\n";
						  print " con = "; printOpt pp_con (tyvar_deref tyvar); print "\n"
					      end)
			 in ()
			 end))
	fun tyvar_help tv =
	    (case (Tyvar.tyvar_deref tv) of
		 SOME _ => ()
	       | NONE => (Error.warn_region_with "top-level unresolved tyvar -- setting to unit: ";
			  pp_con (CON_TYVAR tv); print "\n";
			  Stats.counter_inc(Stats.counter"toil.unresolved_tyvar");
			  ignore(IlStatic.eq_con (ctxt, CON_TYVAR tv, con_unit))))
	fun tyvar_con_handler (c:con) : con option =
	    (case c
	       of CON_TYVAR tyvar => (tyvar_help tyvar; NONE)
		| _ => NONE)
	val tyvar_handler : IlUtil.handler =
	    (default_exp_handler, tyvar_con_handler, default_mod_handler,
	     default_sdec_handler, default_sig_handler)
	val _ = reset_elaboration fp
	val res = xobj arg
	val flex_table = get_flex_table()
	val eq_table = get_eq_table()
	val _ = resolve_all_overloads()
	val _ = app flex_help flex_table
        val _ = ignore (rewriter tyvar_handler res)
	val _ = app eq_help eq_table
	val result = (case (get_error()) of
			  NoError => SOME res
			| Warn => SOME res
			| Error => NONE)
	val _ = reset_elaboration Error.nofilepos
      in result
      end

    val expcompile = xexp
    val typecompile = xty

    val xdec : Il.context * filepos * Ast.dec -> Il.decresult option =
	fn (ctxt,fp,dec) =>
	(TVClose.closeDec dec;
	 overload_wrap ctxt fp xdec IlUtil.decresult_handle (ctxt,dec))

    val xtopspec : Il.context * filepos * Ast.topspec -> Il.entries option =
	fn (ctxt,fp,topspec) => overload_wrap ctxt fp xtopspec IlUtil.entries_handle (ctxt,topspec)

    val seal : Il.context * filepos * Il.mod * Il.signat * Il.signat -> Il.mod option =
	fn (ctxt,fp,ma,sa,st) =>
	overload_wrap ctxt fp Signature.xcoerce_seal IlUtil.mod_handle (ctxt,ma,sa,st)

  end
