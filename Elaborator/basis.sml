(*$import Prelude TopLevel Util Name Prim Tyvar Symbol Fixity Ast Il IlContext IlStatic Ppil IlUtil Datatype Toil BASIS Stats *)
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

    val small_context = Stats.ff("SmallContext")

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

    fun initial_context () : Il.context =
      let
	  val context = ref empty_context
	  val entries = ref []
	  fun add_ce ce = (entries := ((NONE,ce)::(!entries)); 
			   context := (add_context_entries(!context,[ce])))
	  fun add_sbnd(sbnd,sdec) = 
	      (entries := ((SOME sbnd, CONTEXT_SDEC sdec) :: (!entries));
	       context := add_context_sdec(!context, IlContext.SelfifySdec (!context) sdec))

	  fun mk_var_lab str = symbol_label(Symbol.varSymbol str)
	  fun mk_tyc_lab str = symbol_label(Symbol.tycSymbol str)
	  fun mk_var str = fresh_named_var str

	  (* ------------ Do the fixity ---------------- *)
	  val _ = map (fn lf => add_ce(CONTEXT_FIXITY lf)) default_fixity_table

	  fun exp_entry (str,e) = 
	      let val var = fresh_named_var str
		  val lab = mk_var_lab str
		  val c = IlStatic.GetExpCon(!context,e)
		  val bnd = BND_EXP(var, e)
		  val dec = DEC_EXP(var, c, SOME e, true)
		  val sbnd = SBND(lab, bnd)
		  val sdec = SDEC(lab, dec)
	      in  add_sbnd(sbnd,sdec)
	      end

	  fun mono_entry (str,prim) = exp_entry(str, ETAPRIM (prim,[]))
	  fun ilmono_entry (str,prim) = exp_entry(str, ETAILPRIM (prim,[]))
	  fun scon_entry (str,scon) = exp_entry(str, SCON scon)

	  fun poly_entry (str,c2exp) = 
	      let val argvar = fresh_var()
		  val l = internal_label str
		  val argsig = SIGNAT_STRUCTURE([SDEC(l,DEC_CON(fresh_var(),
								KIND,
								NONE, false))])
		  val inner_ctxt = add_context_dec(empty_context,
						   IlContext.SelfifyDec empty_context
						   (DEC_MOD(argvar,false,argsig)))
		  val instcon = CON_MODULE_PROJECT(MOD_VAR argvar,l)
		  val exp = c2exp instcon
		  val con = IlStatic.GetExpCon(inner_ctxt,exp)
		  val inner_var = fresh_named_var str
		  val body = MOD_STRUCTURE([SBND(it_lab,BND_EXP(inner_var, exp))])
		  val ressig = SIGNAT_STRUCTURE [SDEC(it_lab,DEC_EXP(inner_var,con, SOME exp, true))]
		  val module = MOD_FUNCTOR(TOTAL,argvar,argsig,body,ressig)
		  val signat = SIGNAT_FUNCTOR(argvar,argsig,ressig,TOTAL)
		  val lab = mk_var_lab str
		  val var = fresh_named_var (str ^ "_mod")
		  val sbnd = SBND(lab, BND_MOD(var, true, module))
		  val sdec = SDEC(lab, DEC_MOD(var, true, signat))
	      in  add_sbnd(sbnd,sdec)
	      end

	  fun over_entry str con_exp =
	      let val entry = CONTEXT_OVEREXP(mk_var_lab str, con_exp)
	      in   add_ce entry
	      end
	      


	  fun type_entry (s,c) =
	      let val lab = symbol_label (Symbol.tycSymbol s)
		  val var = fresh_named_var s
		  val k = IlStatic.GetConKind(!context,c)
		  val sdec = SDEC(lab, DEC_CON(var,k, SOME c, true))
		  val sbnd = SBND(lab, BND_CON(var, c))
	      in  add_sbnd(sbnd,sdec)
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
	  in
	      val _ = app type_entry basetype_list
	  end
      
        (* -------------- add the overloaded base values ---------------- *)	    
	val _ = 
	   let
	       val intbin = CON_ARROW([con_tuple[int32, int32]], 
				      int32, false, oneshot_init PARTIAL)
	       val wordbin = CON_ARROW([con_tuple[uint32, uint32]], 
				      uint32, false, oneshot_init PARTIAL)
	       val bytebin = CON_ARROW([con_tuple[uint8, uint8]], 
				       uint8, false, oneshot_init PARTIAL)
	       val intuni = CON_ARROW([int32], int32, false, oneshot_init PARTIAL)
	       val intpred = con_eqfun int32
	       val floatuni = CON_ARROW([float64], float64, false, oneshot_init PARTIAL)
	       val floatbin = CON_ARROW([con_tuple[float64, float64]], 
					 float64, false, oneshot_init PARTIAL)
	       val floatpred = con_eqfun float64
	       val charpred = con_eqfun uint8
	       val wordpred = con_eqfun uint32
	       fun add_uni_entry (str,ei,ef) = over_entry str [(intuni, ei), (floatuni, ef)]
	       fun add_bin_entry (str,ei,eb,ew,ef) = over_entry str [(intbin, ei), (bytebin, eb), (wordbin, ew), (floatbin, ef)]
	       fun add_pred_entry (str,ei,ew,ec,ef) = over_entry str [(charpred, ei), 
								   (wordpred, ew), 
								   (intpred, ec), 
								   (floatpred, ef)]
	       val uni_table = [("~", ETAPRIM (neg_int W32,[]),
						ETAPRIM (neg_float F64,[]))]
	       val bin_table = [("+", ETAPRIM (plus_int W32,[]),
				      ETAPRIM (plus_uint W8,[]),
				      ETAPRIM (plus_uint W32,[]),
				      ETAPRIM (plus_float F64,[])),
				("-", ETAPRIM (minus_int W32,[]),
                                      ETAPRIM (minus_uint W8,[]),
                                      ETAPRIM (minus_uint W32,[]),
				      ETAPRIM (minus_float F64,[])),
				("*", ETAPRIM (mul_int W32,[]),
                                      ETAPRIM (mul_uint W8,[]),
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
(*
	           ("true", true_exp),
		   ("false", false_exp),
*)
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
(* these are defined in Basis/toplevel.sml	   
                   ("div", (div_int W32)),
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
		   ("real", (int2float)) ]

(* real_getexp should take a 64-bit IEEE float:
    (1) it the loads the 64-bit pattern as a long
    (2) shift right logical 52 bits
    (3) and to retain only low 11 bits
    (4) subtract 1023 from this quantity and return as an int *)



	  in  val _ = if (not (!small_context))
			  then (app exp_entry basevalue_list;
				app mono_entry baseprimvalue_list;
				app ilmono_entry baseilprimvalue_list)
		      else ()
	  end


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
					 in #1(make_lambda(v,argc,c,
								 PRIM(sub (OtherArray false),[c],[x,y])))
					 end)),
		  ("unsafe_vector",(fn c => let val v = fresh_var()
						 val argc = con_tuple[uint32, c]
						 val x = RECORD_PROJECT(VAR v,generate_tuple_label 1,argc)
						 val y = RECORD_PROJECT(VAR v,generate_tuple_label 2,argc)
					     in #1(make_total_lambda(v,argc,CON_VECTOR c,
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
					    in #1(make_lambda(v,argc,con_unit,
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
				  in #1(make_lambda(v,CON_REF c,c,
							  ILPRIM(deref,[c],[VAR v])))
				  end)),
		   (":=", (fn c => let val v = fresh_var()
				       val pc = con_tuple[CON_REF c, c]
				       fun proj n = RECORD_PROJECT(VAR v,generate_tuple_label n,pc)
				   in #1(make_lambda(v,pc,
						     con_unit,ILPRIM(setref,[c],
								     [proj 1, proj 2])))
				   end))]
	  in val _ = if (not (!small_context))
			 then app poly_entry basepolyvalue_list
		     else ()
	  end

(*
	  val _ = if (null (!entries))
		      then ()
		  else error "initial context has entries"
*)
      in  !context
      end


  end
