(*$import Ast Il IlContext IlStatic Ppil IlUtil Datatype Toil BASIS Stats *)
(* Forms the initial basis for elaboration *)
structure Basis
   :> BASIS =
  struct

    structure Datatype = Datatype
    open Il IlUtil Datatype Ppil
    open Util Name Prim Tyvar
    open IlContext

    val empty_context : context = empty_context
    val error = fn s => error "basis.sml" s

    val uint8 = CON_UINT W8
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

    fun initial_context () : context * sbnd list * sdec list * context =
      let
	  val result = ref (add_context_fixity(empty_context,default_fixity_table))
	  val noninline_result = ref empty_context
	  val sbnds_result = ref ([] : sbnd list)

	  fun mk_var_lab str = symbol_label(Symbol.varSymbol str)
	  fun mk_tyc_lab str = symbol_label(Symbol.tycSymbol str)
	  fun mk_var str = fresh_named_var str
	  fun var_entry (s,c) = 
	      let val v = mk_var s
	      in  (result := add_context_sdec(!result,SDEC(mk_var_lab s, DEC_EXP(v,c)));
		   noninline_result := add_context_sdec(!noninline_result,SDEC(mk_var_lab s, DEC_EXP(v, c))))
	      end
	  fun type_entry ctxt s k c = add_context_sdec(ctxt,SDEC(mk_tyc_lab s, DEC_CON(mk_var s, k, SOME c)))
	  fun exp_entry (str,e) = 
	      let val inline = INLINE_EXPCON(e,IlStatic.GetExpCon(!result,e))
	      in  result := add_context_inline(!result, mk_var_lab str, fresh_named_var str, inline)
	      end
	  fun mono_entry (str,prim) = exp_entry(str, ETAPRIM (prim,[]))
	  fun ilmono_entry (str,prim) = exp_entry(str, ETAILPRIM (prim,[]))
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
		  val ctxt = add_context_dec(empty_context,
					     IlStatic.SelfifyDec empty_context
					       (DEC_MOD(argvar,argsig)))
		  val inner_sig = IlStatic.GetModSig(ctxt,resmod)
		  val m = MOD_FUNCTOR(argvar,argsig,resmod,inner_sig)
		  val s = SIGNAT_FUNCTOR(argvar,argsig,inner_sig,TOTAL)
	      in result := add_context_inline(!result,mk_var_lab str, 
					      fresh_named_var str, INLINE_MODSIG(m,s))
	      end
	  fun over_entry str con_exp =
	      result := add_context_inline(!result,
					   mk_var_lab str, 
					   fresh_named_var str,
					   INLINE_OVER con_exp)


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
				   ("word32", uint32),
				   ("word8", uint8),
				   ("char", CON_UINT W8), 
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
					 in CON_FUN([v1,v2],CON_ARROW ([CON_VAR v1], CON_VAR v2, 
								       false, oneshot_init PARTIAL))
					 end)]
	      fun add_basetype (s,c) =
		  result := add_context_inline(!result,
					       symbol_label (Symbol.tycSymbol s),
					       fresh_named_var ("inline_" ^ s),
					       INLINE_CONKIND(c,IlStatic.GetConKind(empty_context,c)))
	  in
	      val _ = app add_basetype basetype_list
	  end
      
        (* -------------- add the overloaded base values ---------------- *)	    
	val context = 
	   let
	       fun constraints (c,res) (tyvar, 
					helpers as  {hard : con * con -> bool,
						     soft : con * con -> bool},
					is_hard) = 
		   let val c' = CON_TYVAR tyvar
(*
		       val _ = (print "basis: constraints before...\n";
				print "   c is "; Ppil.pp_con c; print "\n";
				print "   c' is "; Ppil.pp_con c'; print "\n")
*)
		       val res = 
			   if ((if is_hard then hard else soft)(c,CON_TYVAR tyvar))
			       then MATCH res
			   else FAIL
(*
		       val _ = (print "basis: constraints after...\n";
				print "   c is "; Ppil.pp_con c; print "\n";
				print "   c' is "; Ppil.pp_con c'; print "\n\n\n")
*)
		   in res
		   end
	       val intbin = CON_ARROW([con_tuple[int32, int32]], 
				      int32, false, oneshot_init PARTIAL)
	       val wordbin = CON_ARROW([con_tuple[uint32, uint32]], 
				      uint32, false, oneshot_init PARTIAL)
	       val intuni = CON_ARROW([int32], int32, false, oneshot_init PARTIAL)
	       val intpred = con_eqfun int32
	       val floatuni = CON_ARROW([float64], float64, false, oneshot_init PARTIAL)
	       val floatbin = CON_ARROW([con_tuple[float64, float64]], 
					 float64, false, oneshot_init PARTIAL)
	       val floatpred = con_eqfun float64
	       val charpred = con_eqfun uint8
	       val wordpred = con_eqfun uint32
	       fun add_uni_entry (str,ei,ef) = over_entry str [(intuni, ei), (floatuni, ef)]
	       fun add_bin_entry (str,ei,ew,ef) = over_entry str [(intbin, ei), (wordbin, ew), (floatbin, ef)]
	       fun add_pred_entry (str,ei,ew,ec,ef) = over_entry str [(charpred, ei), 
								   (wordpred, ew), 
								   (intpred, ec), 
								   (floatpred, ef)]
	       val uni_table = [("~", ETAPRIM (neg_int W32,[]),
						ETAPRIM (neg_float F64,[]))]
	       val bin_table = [("+", ETAPRIM (plus_int W32,[]),
				      ETAPRIM (plus_uint W32,[]),
				      ETAPRIM (plus_float F64,[])),
				("-", ETAPRIM (minus_int W32,[]),
                                      ETAPRIM (minus_uint W32,[]),
				      ETAPRIM (minus_float F64,[])),
				("*", ETAPRIM (mul_int W32,[]),
                                      ETAPRIM (mul_uint W32,[]),
				      ETAPRIM (mul_float F64,[]))]
	       val pred_table = [("<", ETAPRIM (less_uint W8,[]),
				       ETAPRIM (less_uint W32,[]),
				       ETAPRIM (less_int W32,[]),
				       ETAPRIM (less_float F64,[])),
				 (">", ETAPRIM (greater_uint W8,[]),
				       ETAPRIM (greater_uint W32,[]),
				       ETAPRIM (greater_int W32,[]),
				       ETAPRIM (greater_float F64,[])),
				 ("<=", ETAPRIM (lesseq_uint W8,[]),
				       ETAPRIM (lesseq_uint W32,[]),
				        ETAPRIM (lesseq_int W32,[]),
					ETAPRIM (lesseq_float F64,[])),
				 (">=", ETAPRIM (greatereq_uint W8,[]),
				        ETAPRIM (greatereq_uint W32,[]),
				        ETAPRIM (greatereq_int W32,[]),
					ETAPRIM (greatereq_float F64,[]))]
	   in  app add_uni_entry uni_table; 
	       app add_bin_entry bin_table; 
	       app add_pred_entry pred_table 
	   end



	  (* ----------------- add base monomorphic values -------------- *)
	  local
	      val basevalue_list = 
		  [("littleEndian", if (!(Stats.bool "littleEndian"))
					then true_exp else false_exp),
	           ("true", true_exp),
		   ("false", false_exp),
		   ("not", let
			       val arg_var = fresh_named_var "not_arg"
			       val not_body = make_ifthenelse(VAR arg_var,false_exp,true_exp,con_bool)
			   in  #1(make_lambda(arg_var, con_bool, con_bool, not_body))
			   end)]
	      val baseilprimvalue_list = 
		  [("<<", (lshift_uint W32)),
		   ("&&", (and_uint W32)),
		   ("^^", (xor_uint W32)),
		   ("||", (or_uint W32)),
		   ("!!", (not_uint W32)),
		   ("andbyte", (and_uint W8)),
		   ("orbyte", (or_uint W8))]


	      val baseprimvalue_list = 
		  [("/", (div_float F64)),
		   ("float_eq", (eq_float F64)),
		   ("float_neq", (neq_float F64)),
(*		   ("div", (div_int W32)),
		   ("mod", (mod_int W32)),
*)
		   ("quot", (quot_int W32)),
		   ("rem", (rem_int W32)),
		   ("ilt", (less_int W32)),
		   ("igt", (greater_int W32)),
		   ("ilte", (lesseq_int W32)),
		   ("igte", (greatereq_int W32)),
		   ("blt", (less_uint W8)),
		   ("bgt", (greater_uint W8)),
		   ("blte", (lesseq_uint W8)),
		   ("bgte", (greatereq_uint W8)),
		   ("flt", (less_float F64)),
		   ("fgt", (greater_float F64)),
		   ("flte", (lesseq_float F64)),
		   ("fgte", (greatereq_float F64)),
		   ("ult", (less_uint W32)),
		   ("ugt", (greater_uint W32)),
		   ("ulte", (lesseq_uint W32)),
		   ("ugte", (greatereq_uint W32)),

		   ("notb", (not_int W32)),
		   (">>", (rshift_uint W32)),
		   ("~>>", (rshift_int W32)),
		   ("abs", (abs_int W32)),

		   ("andb", (and_int W32)),
		   ("xorb", (xor_int W32)),
		   ("orb", (or_int W32)),

		   ("uinta8touinta32", (uinta2uinta (W8,W32))),
		   ("uintv8touintv32", (uintv2uintv (W8,W32))),
		   ("uint8toint32", (uint2int (W8,W32))),
		   ("uint32toint8", (uint2int (W32,W8))),
		   ("uint8touint32", (uint2uint (W8,W32))),
		   ("uint32touint8", (uint2uint (W32,W8))),
		   ("int32touint8", (int2uint (W32,W8))),
		   ("int32touint32", (int2uint (W32,W32))),
		   ("uint32toint32", (uint2int (W32,W32))),
		   ("iplus", (plus_int W32)),
		   ("imult", (mul_int W32)),
		   ("iminus", (minus_int W32)),
		   ("iquot", (div_int W32)),
		   ("irem", (mod_int W32)),
		   ("bplus", (plus_uint W8)),
		   ("bmult", (mul_uint W8)),
		   ("bminus", (minus_uint W8)),
		   ("bdiv", (div_uint W8)),
		   ("bmod", (mod_uint W8)),
		   ("uplus", (plus_uint W32)),
		   ("umult", (mul_uint W32)),
		   ("uminus", (minus_uint W32)),
		   ("udiv", (div_uint W32)),
		   ("umod", (mod_uint W32)),
		   ("fplus", (plus_float F64)),
		   ("fmult", (mul_float F64)),
		   ("fminus", (minus_float F64)),
		   ("fdiv", (div_float F64)),

		   ("abs_float", (abs_float F64)),

		   (* XXX need to do unsigned and real stuff *)


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
(* real_getexp should take a 64-bit IEEE float:
    (1) it the loads the 64-bit pattern as a long
    (2) shift right logical 52 bits
    (3) and to retain only low 11 bits
    (4) subtract 1023 from this quantity and return as an int *)

	      val basevar_list = [
(*				  ("real_logb", CON_ARROW([CON_FLOAT F64], CON_INT W32, true, oneshot_init TOTAL)),
  ("real_scalb", CON_ARROW([CON_FLOAT F64, CON_INT W32], 
 CON_FLOAT F64, true, oneshot_init TOTAL)), *)
(*
				  ("sqrt", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("sin", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("cos", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("tan", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("atan", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("asin", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("acos", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("sinh", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("cosh", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("tanh", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("exp", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("ln", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("log10", CON_ARROW([CON_FLOAT F64], CON_FLOAT F64, true, oneshot_init TOTAL)),
				  ("getRoundingMode", CON_ARROW([CON_INT W32], CON_INT W32, true, oneshot_init TOTAL)),
				  ("setRoundingMode", CON_ARROW([CON_INT W32], CON_INT W32, true, oneshot_init TOTAL)),

				  ("ml_timeofday", CON_ARROW([con_unit], con_tuple[CON_INT W32, CON_INT W32], 
							     true, oneshot_init PARTIAL))
*)]

	  in  val _ = app exp_entry basevalue_list
	      val _ = app mono_entry baseprimvalue_list
	      val _ = app ilmono_entry baseilprimvalue_list
	      val _ = app var_entry basevar_list
	  end

(*
	  (* ----------------- add functions for standard basis ------------ *)
	  local
	      fun arrow arg result = CON_ARROW([arg], result, true, oneshot_init PARTIAL)
	      val posix = [("exnName", arrow CON_ANY con_string),
			   ("exnMessage", arrow CON_ANY con_string),
			   ("posix_error_msg", arrow (CON_INT W32) con_string),
			   ("posix_error_name", arrow (CON_INT W32) con_string),
			   ("posix_error_num", arrow con_string (CON_INT W32)),
			   ("posix_filesys_num", arrow con_string (CON_UINT W32)),
			   ("posix_filesys_opendir", arrow con_string (CON_INT W32)),
			   ("posix_filesys_readdir", arrow (CON_INT W32) con_string),
			   ("posix_filesys_rewinddir", arrow (CON_INT W32) con_unit),
			   ("posix_filesys_closedir", arrow (CON_INT W32) con_unit),
			   ("posix_filesys_chdir", arrow con_string con_unit),
			   ("posix_filesys_getcwd", arrow con_unit con_string),
			   ("posix_filesys_openf", arrow (con_tuple[con_string,CON_UINT W32, CON_UINT W32]) (CON_INT W32)),
			   ("posix_filesys_umask", arrow (CON_UINT W32) (CON_INT W32)),
			   ("posix_filesys_link", arrow (con_tuple[con_string, con_string]) con_unit),
			   ("posix_filesys_rename", arrow (con_tuple[con_string, con_string]) con_unit),
			   ("posix_filesys_symlink", arrow (con_tuple[con_string, con_string]) con_unit),
			   ("posix_filesys_mkdir", arrow (con_tuple[con_string, CON_UINT W32]) con_unit),
			   ("posix_filesys_mkfifo", arrow (con_tuple[con_string, CON_UINT W32]) con_unit),
			   ("posix_filesys_unlink", arrow con_string con_unit),
			   ("posix_filesys_rmdir", arrow con_string con_unit),
			   ("posix_filesys_readlink", arrow con_string con_string),
			   ("posix_filesys_ftruncate", arrow (con_tuple[CON_INT W32, CON_INT W32]) con_unit)
(* xxxxxxx
			   ("posix_filesys_stat", arrow con_string con_stat),
			   ("posix_filesys_lstat", arrow con_string con_stat),
			   ("posix_filesys_fstat", arrow (CON_INT W32) con_stat),
			   ("posix_filesys_access", arrow (con_tuple[con_string, CON_UINT W32]) con_bool),
			   ("posix_filesys_chmod", arrow (con_tuple[con_string, CON_UINT W32]) con_unit),
			   ("posix_filesys_fchmod", arrow (con_tuple[CON_INT W32, CON_UINT W32]) con_unit),
			   ("posix_filesys_chown", arrow (con_tuple[con_string, CON_UINT W32, CON_UINT W32]) con_unit),
			   ("posix_filesys_fchown", arrow (con_tuple[CON_INT W32, CON_UINT W32, CON_UINT W32]) con_unit),
			   ("posix_filesys_utime", arrow (con_tuple[con_string, CON_INT W32, CON_INT W32]) con_unit),
			   ("posix_filesys_pathconf", arrow (con_tuple[con_string, con_string])
			    (con_tuple[CON_UINT W32, CON_INT W32]) con_unit),
			   ("posix_filesys_fpathconf", arrow (con_tuple[CON_INT W32, con_string]),
			    (con_tuple[CON_UINT W32, CON_INT W32]) con_unit)
xxxxx *)
			   ]
	  in    val _ = app var_entry posix
	  end
*)

	  (* ----------------- add base polymorphic variables -------------- *)
	  local 
	      val basepolyvalue_list = 
		  [("unsafe_array",(fn c => let val v = fresh_var()
						 val argc = con_tuple[uint32, c]
						 val x = RECORD_PROJECT(VAR v,generate_tuple_label 1,argc)
						 val y = RECORD_PROJECT(VAR v,generate_tuple_label 2,argc)
					     in #1(make_lambda(v,argc,CON_ARRAY c,
							       PRIM(create_table (OtherArray false),[c],[x,y])))
					    end)),
		   ("empty_array",(fn c => PRIM(create_empty_table (OtherArray false),[c],[]))),
		   ("empty_vector",(fn c => PRIM(create_empty_table (OtherVector false),[c],[]))),
		  ("unsafe_array2vector",(fn c => let val v = fresh_var()
						  in #1(make_lambda(v,CON_ARRAY c, CON_VECTOR c,
								    PRIM(array2vector (OtherArray false),[c],[VAR v])))
						  end)),
		  ("unsafe_vector2array",(fn c => let val v = fresh_var()
						  in #1(make_lambda(v,CON_VECTOR c, CON_ARRAY c,
								    PRIM(vector2array (OtherVector false),[c],[VAR v])))
						  end)),
		  ("unsafe_sub",(fn c => let val v = fresh_var()
					     val argc = con_tuple[CON_ARRAY c, uint32]
					     val x = RECORD_PROJECT(VAR v,generate_tuple_label 1,argc)
					     val y = RECORD_PROJECT(VAR v,generate_tuple_label 2,argc)
					 in #1(make_total_lambda(v,argc,c,
								 PRIM(sub (OtherArray false),[c],[x,y])))
					 end)),
		  ("unsafe_vector",(fn c => let val v = fresh_var()
						 val argc = con_tuple[uint32, c]
						 val x = RECORD_PROJECT(VAR v,generate_tuple_label 1,argc)
						 val y = RECORD_PROJECT(VAR v,generate_tuple_label 2,argc)
					     in #1(make_lambda(v,argc,CON_VECTOR c,
							       PRIM(create_table (OtherVector false), [c],[x,y])))
					    end)),
		  ("unsafe_vsub",(fn c => let val v = fresh_var()
					     val argc = con_tuple[CON_VECTOR c, uint32]
					     val x = RECORD_PROJECT(VAR v,generate_tuple_label 1,argc)
					     val y = RECORD_PROJECT(VAR v,generate_tuple_label 2,argc)
					 in #1(make_total_lambda(v,argc,c,
								 PRIM(sub (OtherVector false),[c],[x,y])))
					 end)),
		  ("unsafe_update",(fn c => let val v = fresh_var()
						val argc = con_tuple[CON_ARRAY c, uint32, c]
						val x = RECORD_PROJECT(VAR v,generate_tuple_label 1,argc)
						val y = RECORD_PROJECT(VAR v,generate_tuple_label 2,argc)
						val z = RECORD_PROJECT(VAR v,generate_tuple_label 3,argc)
					    in #1(make_total_lambda(v,argc,con_unit,
								    PRIM(update (OtherArray false),[c],[x,y,z])))
					    end)),
		  ("array_length",(fn c => let val v = fresh_var()
					   in #1(make_total_lambda(v,CON_ARRAY c,uint32,
								   PRIM(length_table (OtherArray false),[c],[VAR v])))
					   end)),
		  ("vector_length",(fn c => let val v = fresh_var()
					   in #1(make_total_lambda(v,CON_VECTOR c,uint32,
								   PRIM(length_table (OtherVector false),[c],[VAR v])))
					   end)),
		  (* NOT TOTAL! has a store effect - otherwise we would generalize *)
		  ("ref", (fn c => let val v = fresh_var()
				    in #1(make_lambda(v,c,CON_REF c,
							    ILPRIM(mk_ref,[c],[VAR v])))
				    end)),
		   ("!", (fn c => let val v = fresh_var()
				  in #1(make_total_lambda(v,CON_REF c,c,
							  ILPRIM(deref,[c],[VAR v])))
				  end)),
		   (":=", (fn c => let val v = fresh_var()
				       val pc = con_tuple[CON_REF c, c]
				       fun proj n = RECORD_PROJECT(VAR v,generate_tuple_label n,pc)
				   in #1(make_total_lambda(v,pc,
							   con_unit,ILPRIM(setref,[c],
									 [proj 1, proj 2])))
				   end))]
	  in val _ = app poly_entry basepolyvalue_list
	  end


	   
       (* ----------------- add base datatypes ----------------- *)
	  local
	      open Ast 
	      open Symbol
	      (* we want false to be 0 and true to be 1 *)
	      val booldb = [Db{rhs=Constrs[(varSymbol "false",NONE),
					   (varSymbol "true",NONE)],
			       tyc=tycSymbol "bool",tyvars=[]}]
	      val listdb = [Db{rhs=Constrs[(varSymbol "nil",NONE),
					    (varSymbol "::",
					     SOME (TupleTy
						   [VarTy (Tyv (tyvSymbol "'a")),
						    ConTy ([tycSymbol "list"],
							   [VarTy (Tyv (tyvSymbol "'a"))])]))],
		  
		  tyc=tycSymbol "list",
		  tyvars=[Tyv (tyvSymbol "'a")]}]

	      val suspdb = 
		  [Db{rhs=Constrs[(varSymbol "#Susp",
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


(*
		val list_sbnd_sdecs = Datatype.compile {context = !result,
							typecompile = typecompile,
							datatycs = listdb : Ast.db list,
							eq_compile = Toil.xeq,
							eq_compile_mu = Toil.xeq_mu}
		    
		val (list_sbnds,list_sdecs) = (map #1 list_sbnd_sdecs,
					       map #2 list_sbnd_sdecs)

*)
(*
		val _ = (print "=========================================\n";
			 print "list_sbnds are: ";
			 Ppil.pp_sbnds list_sbnds;
			 print "\n\n";
			 print "list_sdecss are: ";
			 Ppil.pp_sdecs list_sdecs;
			 print "\n\n\n")
*)

		val bool_sbnd_sdecs = Datatype.compile {transparent = true,
							context = !result,
							typecompile = typecompile,
							datatycs = booldb : Ast.db list,
							eq_compile = Toil.xeq,
							eq_compile_mu = Toil.xeq_mu}
		val (bool_sbnds,bool_sdecs) = (map #1 bool_sbnd_sdecs,
					       map #2 bool_sbnd_sdecs)


		val bool_self_sdecs = map (fn (SDEC(l,dec)) => 
					   SDEC(l,IlStatic.SelfifyDec (!result) dec)) bool_sdecs
(*
	      fun self_sdec (SDEC(l,dec)) = SDEC(l,IlStatic.SelfifyDec dec)

	      val lbl = fresh_open_internal_label "basis_dt_inline"
	      val v = fresh_named_var "basis_dt_inline"
	      val bool_sbnds_inline = 
		  let val temp = MOD_STRUCTURE(bool_sbnds)
		  in  (case IlUtil.make_inline_module(!result,temp,SOME(SIMPLE_PATH v), true) of
			   SOME (MOD_STRUCTURE sbnds) => sbnds
			 | _ => (print "cannot inline datatype module!";
				 Ppil.pp_mod temp; print "\n";
				 error "cannot inline datatype module!"))
		  end
	      val final_sdec = self_sdec final_sdec
*)

	      val _ = result := add_context_sdecs(!result, bool_self_sdecs)

(*
	      val _ = (print "!result is: "; Ppil.pp_context (!result); print "\n";
		       print "final_sdec is: "; Ppil.pp_sdec final_sdec;
		       print "\nnew_context is: "; Ppil.pp_context new_context; print "\n\n")
*)
		      
	  in
(*	      val datatype_self_sdecs = datatype_self_sdecs *)
(*	      val _ = sbnds_result := datatype_sbnds *)
(*	      val datatype_sdecs = datatype_sdecs *)
	  end
      in  (!result, [], [], !noninline_result)
        (* (!result, !sbnds_result, datatype_sdecs, 
	    add_context_sdecs(!result,datatype_self_sdecs)) *)
      end

(*    val initial_context = Util.memoize initial_context *)
  
  end



