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

    val int32 = CON_INT W32
    val uint32 = CON_UINT W32
    val float64 = CON_FLOAT F64

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

    fun initial_context () : context * sbnd list * sdec list =
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
	  fun mono_entry (str,prim) = exp_entry(str, ETAPRIM (prim,[]))
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
	      val basetype_list = [("float", float64),
				   ("real", float64),
				   ("int", int32), 
				   ("uint", uint32),
				   ("char", CON_UINT W8), 
				   ("string", con_string),
				   ("exn",CON_ANY),
				   ("unit", con_unit),
				   ("ref",let val v = fresh_var()
					  in CON_FUN([v],CON_REF (CON_VAR v))
					  end),
				   ("array",let val v = fresh_var()
					    in CON_FUN([v],CON_ARRAY (CON_VAR v))
					    end),
				   ("vector",let val v = fresh_var()
					     in CON_FUN([v],CON_VECTOR (CON_VAR v))
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
      

	val context = 
	   let
	       fun constraints (c,res) (tyvar, 
					helpers as  {hard : con * con -> bool,
						     soft : con * con -> bool},
					is_hard) = 
		   if ((if is_hard then hard else soft)(c,CON_TYVAR tyvar))
		       then MATCH res
		   else FAIL
	       datatype X = INT_CASE | FLOAT_CASE
	       fun con_thunk (intres,floatres) exp_oneshot x = 
		   (case (oneshot_deref exp_oneshot,x) of
			(SOME _,_) => ()
		      | (NONE,INT_CASE) => oneshot_set(exp_oneshot,intres)
		      | (NONE,FLOAT_CASE) => oneshot_set(exp_oneshot,floatres))
	       val intbin = (CON_ARROW(con_tuple[int32, int32], 
				       int32, oneshot_init PARTIAL), INT_CASE)
	       val intuni = (CON_ARROW(int32,int32, oneshot_init PARTIAL), INT_CASE)
	       val intpred = (CON_ARROW(con_tuple[int32, int32], 
					con_bool, oneshot_init PARTIAL), INT_CASE)
	       val floatuni = (CON_ARROW(float64,float64, oneshot_init PARTIAL), FLOAT_CASE)
	       val floatbin = (CON_ARROW(con_tuple[float64, float64], 
					 float64, oneshot_init PARTIAL), FLOAT_CASE)
	       val floatpred = (CON_ARROW(con_tuple[float64, float64], 
					  con_bool, oneshot_init PARTIAL), FLOAT_CASE)
	       fun add_uni_entry (str,thunk) = over_entry str thunk (map constraints [intuni,floatuni])
	       fun add_bin_entry (str,thunk) = over_entry str thunk (map constraints [intbin,floatbin])
	       fun add_pred_entry (str,thunk) = over_entry str thunk (map constraints [intpred,floatpred])
	       val uni_table = [("~", con_thunk(ETAPRIM (neg_int W32,[]),
						ETAPRIM (neg_float F64,[])))]
	       val bin_table = [("+", con_thunk(ETAPRIM (plus_int W32,[]),
						ETAPRIM (plus_float F64,[]))),
				("-", con_thunk(ETAPRIM (minus_int W32,[]),
						ETAPRIM (minus_float F64,[]))),
				("*", con_thunk(ETAPRIM (mul_int W32,[]),
						ETAPRIM (mul_float F64,[])))]
	       val pred_table = [("<", con_thunk(ETAPRIM (less_int W32,[]),
						 ETAPRIM (less_float F64,[]))),
				 (">", con_thunk(ETAPRIM (greater_int W32,[]),
						 ETAPRIM (greater_float F64,[]))),
				 ("<=", con_thunk(ETAPRIM (lesseq_int W32,[]),
						  ETAPRIM (lesseq_float F64,[]))),
				 (">=", con_thunk(ETAPRIM (greatereq_int W32,[]),
						  ETAPRIM (greatereq_float F64,[])))]
	   in  app add_uni_entry uni_table; 
	       app add_bin_entry bin_table; 
	       app add_pred_entry pred_table
	   end



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
		   ("size", ETAPRIM(length1 false,[CON_UINT W8]))]

	      val baseprimvalue_list = 
		  [("/", (mul_float F64)),
		   ("div", (div_int W32)),
		   ("mod", (mod_int W32)),
		   ("quot", (quot_int W32)),
		   ("rem", (rem_int W32)),
		   ("ult", (less_uint W32)),
		   ("ugt", (greater_uint W32)),
		   ("ulte", (lesseq_uint W32)),
		   ("ugte", (greatereq_uint W32)),

		   ("<>", (neq_int W32)),
		   ("notb", (not_int W32)),
		   ("<<", (lshift_int W32)),
		   (">>", (rshift_uint W32)),
		   ("~>>", (rshift_int W32)),
		   ("&&", (and_int W32)),
		   ("||", (or_int W32)),
		   ("abs", (abs_int W32)),

		   ("uinta8touinta32", (uinta2uinta (W8,W32))),
		   ("uintv8touintv32", (uintv2uintv (W8,W32))),
		   ("uint8toint32", (uint2int (W8,W32))),
		   ("int32touint8", (int2uint (W32,W8))),
		   ("int32touint32", (int2uint (W32,W32))),
		   ("uint32toint32", (uint2int (W32,W32))),
		   ("uplus", (plus_uint W32)),
		   ("umult", (mul_uint W32)),
		   ("uminus", (minus_uint W32)),
		   ("udiv", (div_uint W32)),

		   (* XXX need to do unsigned and real stuff *)
		   (* 	   mono_entry "explode" (EXPLODE), *)
		   (*	   mono_entry "implode" (IMPLODE), *)

		   ("floor", (float2int)),
		   ("real", (int2float)),
	   
		   ("open_in", open_in),
		   ("lookahead", lookahead),
		   ("input", input),
		   ("input1", input1),
		   ("end_of_stream", end_of_stream),
		   ("close_in", close_in),

		   ("open_out", open_out),
		   ("output", output),
		   ("flush_out", flush_out),
		   ("close_out", close_out)]


	  in  val _ = app exp_entry basevalue_list
	      val _ = app mono_entry baseprimvalue_list
	  end



	  (* ----------------- add base polymorphic variables -------------- *)
	  local 
	      val basepolyvalue_list = 
		  [("unsafe_array",(fn c => let val v = fresh_var()
						 val argc = con_tuple[uint32, c]
						 val x = RECORD_PROJECT(VAR v,generate_tuple_label 1,argc)
						 val y = RECORD_PROJECT(VAR v,generate_tuple_label 2,argc)
					     in #1(make_lambda(v,argc,CON_ARRAY c,
							       PRIM(array1 true,[c],[x,y])))
					    end)),
		  ("unsafe_array2vector",(fn c => let val v = fresh_var()
						  in #1(make_lambda(v,CON_ARRAY c, CON_VECTOR c,
								    PRIM(array2vector,[c],[VAR v])))
						  end)),
		  ("unsafe_sub",(fn c => let val v = fresh_var()
					     val argc = con_tuple[CON_ARRAY c, uint32]
					     val x = RECORD_PROJECT(VAR v,generate_tuple_label 1,argc)
					     val y = RECORD_PROJECT(VAR v,generate_tuple_label 2,argc)
					 in #1(make_total_lambda(v,argc,c,
								 PRIM(sub1 true,[c],[x,y])))
					 end)),
		  ("unsafe_vector",(fn c => let val v = fresh_var()
						 val argc = con_tuple[uint32, c]
						 val x = RECORD_PROJECT(VAR v,generate_tuple_label 1,argc)
						 val y = RECORD_PROJECT(VAR v,generate_tuple_label 2,argc)
					     in #1(make_lambda(v,argc,CON_VECTOR c,
							       PRIM(array1 false, [c],[x,y])))
					    end)),
		  ("unsafe_vsub",(fn c => let val v = fresh_var()
					     val argc = con_tuple[CON_VECTOR c, uint32]
					     val x = RECORD_PROJECT(VAR v,generate_tuple_label 1,argc)
					     val y = RECORD_PROJECT(VAR v,generate_tuple_label 2,argc)
					 in #1(make_total_lambda(v,argc,c,
								 PRIM(sub1 false,[c],[x,y])))
					 end)),
		  ("unsafe_update",(fn c => let val v = fresh_var()
						val argc = con_tuple[CON_ARRAY c, uint32, c]
						val x = RECORD_PROJECT(VAR v,generate_tuple_label 1,argc)
						val y = RECORD_PROJECT(VAR v,generate_tuple_label 2,argc)
						val z = RECORD_PROJECT(VAR v,generate_tuple_label 3,argc)
					    in #1(make_total_lambda(v,argc,con_unit,
								    PRIM(update1,[c],[x,y,z])))
					    end)),
		  ("array_length",(fn c => let val v = fresh_var()
					   in #1(make_total_lambda(v,CON_ARRAY c,uint32,
								   PRIM(length1 true,[c],[VAR v])))
					   end)),
		  ("vector_length",(fn c => let val v = fresh_var()
					   in #1(make_total_lambda(v,CON_VECTOR c,uint32,
								   PRIM(length1 false,[c],[VAR v])))
					   end)),
		  ("ref", (fn c => let val v = fresh_var()
				    in #1(make_total_lambda(v,c,CON_REF c,
							    PRIM(mk_ref,[c],[VAR v])))
				    end)),
		   ("!", (fn c => let val v = fresh_var()
				  in #1(make_total_lambda(v,CON_REF c,c,
							  PRIM(deref,[c],[VAR v])))
				  end)),
		   (":=", (fn c => let val v = fresh_var()
				       val pc = con_tuple[CON_REF c, c]
				       fun proj n = RECORD_PROJECT(VAR v,generate_tuple_label n,pc)
				   in #1(make_total_lambda(v,pc,
							   con_unit,PRIM(setref,[c],
									 [proj 1, proj 2])))
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
	      val listdb = [Db{def=[(varSymbol "nil",NONE),
				    (varSymbol "::",
				     SOME (TupleTy
					   [VarTy (Tyv (tyvSymbol "'a")),
					    ConTy ([tycSymbol "list"],
						   [VarTy (Tyv (tyvSymbol "'a"))])]))],
		  
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
		    
		    
		val (list_sbnds,list_sdecs) = (map #1 list_sbnd_sdecs,
					       map #2 list_sbnd_sdecs)

(*
		val _ = (print "=========================================\n";
			 print "list_sbnds are: ";
			 Ppil.pp_sbnds list_sbnds;
			 print "\n\n";
			 print "list_sdecss are: ";
			 Ppil.pp_sdecs list_sdecs;
			 print "\n\n\n")
*)

		val bool_sbnd_sdecs = Datatype.compile {context = !result,
							typecompile = typecompile,
							datatycs = booldb : Ast.db list,
							withtycs = [] : Ast.tb list,
							eq_compile = Toil.xeq}
		val (bool_sbnds,_) = (map #1 bool_sbnd_sdecs,
				      map #2 bool_sbnd_sdecs)

		(* we compute a precise signature for bool type so that the elaborator can use
		   the fact that a bool is a CON_MUPROJECT(unit + unit) *)
		val bool_sdecs = IlStatic.GetSbndsSdecs(!result,bool_sbnds)

		val susp_sbnd_sdecs = Datatype.compile {context = !result,
							typecompile = typecompile,
							datatycs = suspdb : Ast.db list,
							withtycs = [] : Ast.tb list,
							eq_compile = Toil.xeq}
		    
		    
		val (susp_sbnds,susp_sdecs) = (map #1 susp_sbnd_sdecs,
					       map #2 susp_sbnd_sdecs)

	      val datatype_sbnds = bool_sbnds @ susp_sbnds @ list_sbnds 
	      val datatype_sdecs = bool_sdecs @ susp_sdecs @ list_sdecs 
	      val datatype_self_sdecs = map (fn (SDEC(l,dec)) => SDEC(l,IlStatic.SelfifyDec dec)) datatype_sdecs
	  in
	      val _ = result := add_context_sdecs(!result,datatype_self_sdecs)
	      val _ = sbnds_result := datatype_sbnds
	      val datatype_sdecs = datatype_sdecs
	  end
      in  (!result, !sbnds_result, datatype_sdecs)
      end
  
  end
