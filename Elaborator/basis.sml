(* Forms the initial basis for elaboration *)
functor Basis(structure Il : IL
	      structure IlContext : ILCONTEXT
	      structure IlStatic : ILSTATIC
	      structure Ppil : PPIL
	      structure IlUtil : ILUTIL
	      structure Datatype : DATATYPE
	      structure Toil : TOIL
	      sharing Datatype.IlContext = IlContext
	      sharing IlContext.Il = Ppil.Il = IlUtil.Il = IlStatic.Il 
		                   = Toil.Il = Il) : BASIS =
  struct

    structure Il = Il
    structure Datatype = Datatype
    open Il IlUtil Datatype Ppil
    open Util Name Prim Tyvar
    open IlContext

    val empty_context : context = empty_context
    val error = fn s => error "basis.sml" s

    local
      open Ast
      open Symbol
      open Fixity
    in 
      val table = [(":=", infixleft 3), 
		   ("o",  infixleft 3),
		   ("=",  infixleft 4),
		   ("<>", infixleft 4),
		   ("<",  infixleft 4),
		   (">",  infixleft 4),
		   ("<=", infixleft 4),
		   (">=", infixleft 4),
		   ("@",  infixleft 5),
		   ("::", infixright 5),
		   ("+",  infixleft 6),
		   ("-",  infixleft 6),
		   ("^",  infixleft 6),
		   ("div",infixleft 7),
		   ("mod",infixleft 7),
		   ("/",  infixleft 7),
		   ("*",  infixleft 7)]
      val default_fixity_table = map (fn (str,f) => (symbol_label(varSymbol(str)), f)) table
    end

    fun initial_context () : context * sbnd list =
      let
	  val result = ref (add_context_fixity(empty_context,default_fixity_table))
	  val sbnds_result = ref ([] : sbnd list)

	  fun mk_var_lab str = symbol_label(Symbol.varSymbol str)
	  fun mk_tyc_lab str = symbol_label(Symbol.tycSymbol str)
	  fun mk_var str = fresh_named_var str
	  fun binop_con(conopt) = let val con = (case conopt of 
						     NONE => fresh_con empty_context
						   | SOME c => c)
				  in CON_ARROW(con_tuple[con,con],con,oneshot_init PARTIAL)
				  end
	  fun var_entry (s,c) = result := add_context_sdec(!result,SDEC(mk_var_lab s, DEC_EXP(mk_var s, c)))
	  fun type_entry ctxt s k c = add_context_sdec(ctxt,SDEC(mk_tyc_lab s, DEC_CON(mk_var s, k, SOME c)))
	  fun exp_entry (str,e) = 
	      let val inline = INLINE_EXPCON(e,IlStatic.GetExpCon(!result,e))
	      in  result := add_context_inline(!result, mk_var_lab str, fresh_named_var str, inline)
	      end
	  fun mono_entry (str,prim) = exp_entry(str, PRIM (prim,[]))
	  fun scon_entry (str,scon) = exp_entry(str, SCON scon)
	  fun poly_entry (str,c2exp) = 
	      let val argvar = fresh_var()
		  val l = internal_label str
		  val argsig = SIGNAT_STRUCTURE(NONE,
						[SDEC(l,DEC_CON(fresh_var(),
								KIND_TUPLE 1, 
								NONE))])
		  val instcon = CON_MODULE_PROJECT(MOD_VAR argvar,l)
		  val resmod = MOD_STRUCTURE[SBND(it_lab,
						  BND_EXP(fresh_var(),
							  (c2exp instcon)))]
		  val m = MOD_FUNCTOR(argvar,argsig,resmod)
		  val s = IlStatic.GetModSig(empty_context,m)
	      in result := add_context_inline(!result,mk_var_lab str, fresh_named_var str, INLINE_MODSIG(m,s))
	      end
	  fun over_entry str con_thunker constraints =
	      result := add_context_inline(!result,
					   mk_var_lab str, 
					   fresh_named_var str,
					   INLINE_OVER(fn _ => 
						       let val eshot = oneshot()
							   val ocon = uocon_inst (empty_context,
										  fresh_uocon constraints, 
										  con_thunker eshot)
					       val con = CON_OVAR ocon
						       in (OVEREXP(con,true,eshot),ocon)
						       end))
	    
	    

	 (* -------------- add the base tags ------------------------- *)
	  local 
	      fun tag_help (str,t) = result := add_context_sdec(!result,
							      SDEC(fresh_internal_label str,
								   DEC_EXCEPTION(t,con_unit)))
		  
	      val basetag_list = [("fail", fail_tag),
				  ("bind", bind_tag),
				  ("match", match_tag)]
	  in  val _ = app tag_help basetag_list
	  end


	 (* -------------- add the base types ------------------------- *)
	  local
	      val basetype_list = [("float", CON_FLOAT F64),
				   ("real", CON_FLOAT F64),
				   ("int", CON_INT W32), 
				   ("uint", CON_UINT W32),
				   ("char", CON_UINT W8), 
				   ("string", con_string),
				   ("exn",CON_ANY),
				   ("unit", con_unit),
				   ("ref",let val v = fresh_var()
					  in CON_FUN([v],CON_REF (CON_VAR v))
					  end),
				   ("->",let val v1 = fresh_var()
					     val v2 = fresh_var()
					 in CON_FUN([v1,v2],CON_ARROW (CON_VAR v1, CON_VAR v2, 
								       oneshot_init PARTIAL))
					 end)]
	      fun add_basetype (s,c) =
		  result := add_context_inline(!result,
					       symbol_label (Symbol.tycSymbol s),
					       fresh_named_var s,
					       INLINE_CONKIND(c,IlStatic.GetConKind(empty_context,c)))
	  in
	      val _ = app add_basetype basetype_list
	  end
      
(* ---------------------- begin of example test case for overloading ---------------- *)
	val context = 
	   let
	       datatype X = A | B
	       fun con_thunk exp_oneshot x = 
		   (case (oneshot_deref exp_oneshot,x) of
			(SOME _,_) => ()
		      | (NONE,A) => oneshot_set(exp_oneshot,PRIM (plus_int W32,[]))
		      | (NONE,B) => oneshot_set(exp_oneshot,PRIM (plus_float F64,[])))
	       fun constraints (c,res) (tyvar, 
					helpers as  {hard : con * con -> bool,
						     soft : con * con -> bool},
					is_hard) = 
		   if ((if is_hard then hard else soft)(c,CON_TYVAR tyvar))
		       then MATCH res
		   else FAIL
	       val cstr1 = constraints (CON_ARROW(con_tuple[CON_INT W32, CON_INT W32], 
						  CON_INT W32, oneshot_init PARTIAL), A)
	       val cstr2 = constraints (CON_ARROW(con_tuple[CON_FLOAT F64, CON_FLOAT F64], 
						  CON_FLOAT F64, oneshot_init PARTIAL), B)
	   in over_entry "over" con_thunk [cstr1,cstr2]
	   end
(* ---------------------- end of example test case for overloading ---------------- *)


	  (* ----------------- add base monomorphic values -------------- *)
	  local
	      val basevalue_list = 
		  [("true", true_exp),
		   ("false", false_exp),
		   ("not", let
			       val arg_var = fresh_named_var "not_arg"
			       val not_body = make_ifthenelse(VAR arg_var,false_exp,true_exp,con_bool)
			   in  #1(make_lambda(arg_var, con_bool, con_bool, not_body))
			   end),
		   ("size", PRIM(length1 false,[CON_UINT W8]))]

	      val baseprimvalue_list = 
		  [("+", (plus_int W32)),
		   ("-", (minus_int W32)),
		   ("*", (mul_int W32)),
		   ("div", (div_int W32)),
		   ("mod", (mod_int W32)),
		   ("quot", (quot_int W32)),
		   ("rem", (rem_int W32)),
		   ("<", (less_int W32)),
		   (">", (greater_int W32)),
		   ("<=", (lesseq_int W32)),
		   (">=", (greatereq_int W32)),
		   ("<>", (neq_int W32)),
		   ("notb", (not_int W32)),
		   ("<<", (lshift_int W32)),
		   (">>", (rshift_uint W32)),
		   ("~>>", (rshift_int W32)),
		   ("&&", (and_int W32)),
		   ("||", (or_int W32)),
		   ("~", (neg_int W32)),
		   ("abs", (abs_int W32)),

		   (* XXX need to do unsigned and real stuff *)
		   (* 	   mono_entry "explode" (EXPLODE), *)
		   (*	   mono_entry "implode" (IMPLODE), *)

		   ("floor", (float2int)),
		   ("real", (int2float)),
	   
		   ("output", output),
		   ("input", input)]


	  in  val _ = app exp_entry basevalue_list
	      val _ = app mono_entry baseprimvalue_list
	  end

	  (* ----------------- add base monomorphic variables -------------- *)
	  local
	      val basevar_list = 
		  [("chr", CON_ARROW(CON_INT W32, CON_UINT W8, oneshot_init PARTIAL)),
		   ("ord", CON_ARROW(CON_UINT W8, CON_INT W32, oneshot_init TOTAL)),
		   ("^", CON_ARROW(con_tuple[con_string,con_string],
				   con_string,oneshot_init TOTAL))
		   ]
	  in val _ = app var_entry basevar_list
	  end


	  (* ----------------- add base polymorphic variables -------------- *)
	  local 
	      val basepolyvalue_list = 
		  [("ref", (fn c => let val v = fresh_var()
				    in #1(make_total_lambda(v,c,CON_REF c,
							    APP(PRIM(mk_ref,[c]),VAR v)))
				    end)),
		   ("!", (fn c => let val v = fresh_var()
				  in #1(make_total_lambda(v,CON_REF c,c,
							  APP(PRIM(deref,[c]),VAR v)))
				  end)),
		   (":=", (fn c => let val v = fresh_var()
				       val pc = con_tuple[CON_REF c, c]
				       fun proj n = RECORD_PROJECT(VAR v,generate_tuple_label n,pc)
				   in #1(make_total_lambda(v,pc,
							   con_unit,APP(PRIM(setref,[c]),
									exp_tuple[proj 1, proj 2])))
				   end))]
	  in val _ = app poly_entry basepolyvalue_list
	  end

	   
       (* ----------------- add base datatypes ----------------- *)
	  local
	      open Ast 
	      open Symbol
	      val booldb = [Db{def=[(varSymbol "true",NONE),
				    (varSymbol "false",NONE)],
			       tyc=tycSymbol "bool",tyvars=[]}]
	      val listdb = [Db{def=[(varSymbol "::",
				     SOME (TupleTy
					   [VarTy (Tyv (tyvSymbol "'a")),
					    ConTy ([tycSymbol "list"],
						   [VarTy (Tyv (tyvSymbol "'a"))])])),
				    (varSymbol "nil",NONE)],
		  tyc=tycSymbol "list",
		  tyvars=[Tyv (tyvSymbol "'a")]}]

	      val suspdb = 
		  [Db{def=[(varSymbol "#Susp",
                            SOME
                              (ConTy
                                 ([tycSymbol "->"],
                                  [ConTy ([tycSymbol "unit"],[]),
				   VarTy (Tyv (tyvSymbol "'a"))])))],
		  tyc=tycSymbol "#susp",
		  tyvars=[Tyv (tyvSymbol "'a")]}]

		    
		fun typecompile(ctxt,ty) = 
		    let fun zfp(_:Ast.srcpos) = ("zfp",0,0)
		    in (case Toil.xty(ctxt,zfp,ty) of
			    SOME c => c
			  | NONE => (print "context in typecompile was:";
				     pp_context ctxt;
				     error "Compilation error in initial basis"))
		    end

		val list_sbnd_sdecs = Datatype.compile {context = !result,
							typecompile = typecompile,
							datatycs = listdb : Ast.db list,
							withtycs = [] : Ast.tb list,
							eq_compile = Toil.xeq}
		    
		    
		val (mlist,slist) = (MOD_STRUCTURE(map #1 list_sbnd_sdecs),
				     SIGNAT_STRUCTURE(NONE, map #2 list_sbnd_sdecs))
		    
		val bool_sbnd_sdecs = Datatype.compile {context = !result,
							typecompile = typecompile,
							datatycs = booldb : Ast.db list,
							withtycs = [] : Ast.tb list,
							eq_compile = Toil.xeq}
		val (mbool,sbool) = (MOD_STRUCTURE(map #1 bool_sbnd_sdecs),
				     SIGNAT_STRUCTURE(NONE, map #2 bool_sbnd_sdecs))

		(* we compute a precise signature for bool type so that the elaborator can use
		   the fact that a bool is a CON_MUPROJECT(unit + unit) *)
		val sbool = IlStatic.GetModSig(!result,mbool)

		val susp_sbnd_sdecs = Datatype.compile {context = !result,
							typecompile = typecompile,
							datatycs = suspdb : Ast.db list,
							withtycs = [] : Ast.tb list,
							eq_compile = Toil.xeq}
		    
		    
		val (msusp,ssusp) = (MOD_STRUCTURE(map #1 susp_sbnd_sdecs),
				     SIGNAT_STRUCTURE(NONE, map #2 susp_sbnd_sdecs))

		val bool_label = open_internal_label "bools"
		val bool_var = fresh_named_var "bools"
		val list_label = open_internal_label "lists"
		val list_var = fresh_named_var "lists"
		val susp_label = open_internal_label "susps"
		val susp_var = fresh_named_var "susps"
		val sbool = IlStatic.SelfifySig(SIMPLE_PATH bool_var, sbool)
		val slist = IlStatic.SelfifySig(SIMPLE_PATH list_var, slist)
		val ssusp = IlStatic.SelfifySig(SIMPLE_PATH susp_var, ssusp)

	      val (datatype_sdecs, datatype_sbnds) =
		  ([SDEC(bool_label, DEC_MOD(bool_var, sbool)),
		    SDEC(susp_label, DEC_MOD(susp_var, ssusp)),
		    SDEC(list_label, DEC_MOD(list_var, slist))],
		   [SBND(bool_label, BND_MOD(bool_var,mbool)),
		    SBND(susp_label, BND_MOD(susp_var,msusp)),
		    SBND(list_label, BND_MOD(list_var,mlist))])
	  in
	      val _ = result := add_context_sdecs(!result,datatype_sdecs)
	      val _ = sbnds_result := datatype_sbnds
	  end
      in  (!result, !sbnds_result)
      end
  
  end
