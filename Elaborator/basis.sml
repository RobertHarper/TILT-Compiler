(*$import Prelude TopLevel Util Name Prim Tyvar Symbol Fixity Ast Il IlContext IlStatic Ppil IlUtil Datatype Toil BASIS Stats *)

(* Everything provided here is inlined.  There is no corresponding
   object code.  Most primitives are in structure TiltPrim.  We
   provide some top-level inlined values and types which either can't
   be expressed in Basis/Firstlude.sml or are included here for
   efficiency.
*)
   
signature BIND =
sig
    type t
    val start : Il.context -> t

    val add_sbnd_top : (Il.sbnd * Il.sdec) * t -> t
    val add_sbnd : (Il.sbnd * Il.sdec) * t -> t
	
    val finish : t -> Il.context
end

(*
(* bindings go to top-level *)
structure BindTop :> BIND =
struct
    type t = Il.context
    fun start context = context 
    fun add_sbnd_top ((_, sdec), context) = IlContext.add_context_sdec (context, IlContext.SelfifySdec context sdec)
    val add_sbnd = add_sbnd_top
    fun finish context = context
end
*)

(* bindings go to a top-level structure *)
structure Bind :> BIND =
struct
    datatype t = B of {sbnds : Il.sbnds,
		       sdecs : Il.sdecs,
		       context : Il.context}

    fun start context = B {sbnds=nil, sdecs=nil, context=context}

    fun add_sdec_ctxt (sdec, context) =
	IlContext.add_context_sdec (context, IlContext.SelfifySdec context sdec)
	
    fun add_sbnd_top ((_, sdec), B {sbnds, sdecs, context}) =
	B {sbnds=sbnds,
	   sdecs=sdecs,
	   context=add_sdec_ctxt (sdec, context)}

    fun add_sbnd ((sbnd,sdec), B {sbnds, sdecs, context}) =
	B {sbnds=sbnd :: sbnds,
	   sdecs=sdec :: sdecs,
	   context=context}
		
    fun finish (B {sbnds, sdecs, context}) =
	let val str = "TiltPrim"
	    val lab = Name.symbol_label (Symbol.strSymbol str)
	    val var = Name.fresh_named_var str
	    open Il
	    val body = MOD_STRUCTURE sbnds
	    val signat = SIGNAT_STRUCTURE sdecs
	    val sbnd = SBND(lab, BND_MOD(var, true, body))
	    val sdec = SDEC(lab, DEC_MOD(var, true, signat))
	in  add_sdec_ctxt (sdec,context)
	end
end

(* Forms the initial basis for elaboration *)
structure Basis :> BASIS =
  struct

    structure Bind = Bind
    open Il IlUtil Datatype Ppil
    open Util Name Prim Tyvar
    open IlContext

    val empty_context : context = empty_context
    val error = fn s => error "basis.sml" s

    val uint8 = CON_UINT W8
    val int32 = CON_INT W32
    val uint32 = CON_UINT W32
    val float64 = CON_FLOAT F64

    fun mk_var_lab str = symbol_label(Symbol.varSymbol str)
(*  fun mk_tyc_lab str = symbol_label(Symbol.tycSymbol str)
    fun mk_var str = fresh_named_var str
*)
	
    fun exp_entry (str,e) = 
	let val var = fresh_named_var str
	    val lab = mk_var_lab str
	    val c = IlStatic.GetExpCon(empty_context,e)
	    val bnd = BND_EXP(var, e)
	    val dec = DEC_EXP(var, c, SOME e, true)
	    val sbnd = SBND(lab, bnd)
	    val sdec = SDEC(lab, dec)
	in  (sbnd,sdec)
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
	in  (sbnd,sdec)
	end

    fun type_entry (s,c) =
	let val lab = symbol_label (Symbol.tycSymbol s)
	    val var = fresh_named_var s
	    val k = IlStatic.GetConKind(empty_context,c)
	    val sdec = SDEC(lab, DEC_CON(var,k, SOME c, true))
	    val sbnd = SBND(lab, BND_CON(var, c))
	in  (sbnd,sdec)
	end

    fun initial_context () : Il.context =
      let
	  val bindings = ref (Bind.start empty_context)
	  fun wrap binder x = bindings := binder (x, !bindings)
	  val add_top = wrap Bind.add_sbnd_top
	  val add     = wrap Bind.add_sbnd

	 (* -------------- add the base types ------------------------- *)
	  local
	      val toptype_list = [("->",let val v1 = fresh_var()
					    val v2 = fresh_var()
					in CON_FUN([v1,v2],CON_ARROW ([CON_VAR v1], CON_VAR v2, 
								      false, oneshot_init PARTIAL))
					end),
				  ("real", float64),
				  ("int", int32),
				  ("word", uint32),
				  ("char", uint8),
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
					    end)
				  ]
	      val basetype_list = [("float64", float64),
				   ("int32", int32),
				   ("uint32", uint32),
				   ("uint8", uint8),
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
					     end)
				   ]
	  in
	      val _ = (app (add_top o type_entry) toptype_list;
		       app (add     o type_entry) basetype_list)
	  end
      
	  (* ----------------- add base monomorphic values -------------- *)
	  local
	      val topvalue_list =
		  [("not", let
			       val arg_var = fresh_named_var "not_arg"
			       val not_body = make_ifthenelse(VAR arg_var,false_exp,true_exp,con_bool)
			   in  #1(make_lambda(arg_var, con_bool, con_bool, not_body))
			   end)]
	      val basevalue_list = 
		  [("littleEndian", if (!(Stats.bool "littleEndian"))
					then true_exp else false_exp)]
		  
	      val baseilprimvalue_list = 
		  [("<<", (lshift_uint W32)),
		   ("&&", (and_uint W32)),
		   ("^^", (xor_uint W32)),
		   ("||", (or_uint W32)),
		   ("!!", (not_uint W32)),
		   
		   ("andbyte", (and_uint W8)),
		   ("orbyte", (or_uint W8))]

	      val topprimvalue_list =
		  [("ord", (uint2int (W8,W32))),
(* Primitive doesn't raise exn.
		   ("chr", (int2uint (W32,W8))),
*)
		   ("real", (int2float))]

	      val baseprimvalue_list =
		  [("float_eq", (eq_float F64)),
		   ("float_neq", (neq_float F64)),
		   
(* these are non-trivial, see Basis/Numeric/pre-int.sml
                   ("div", (div_int W32)),
		   ("mod", (mod_int W32)),
*)
		   ("bdiv", (div_uint W8)),
		   ("bmod", (mod_uint W8)),
		   ("udiv", (div_uint W32)),
		   ("umod", (mod_uint W32)),
		   ("iquot", (quot_int W32)),
		   ("irem", (rem_int W32)),
		   ("fdiv", (div_float F64)),
		   
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

		   ("ineg", (neg_int W32)),
		   ("fneg", (neg_float F64)),

(* This does not pass RTL
		   ("iabs", (abs_int W32)),
*)
		   ("fabs", (abs_float F64)),
		   
		   ("iplus", (plus_int W32)),
		   ("imult", (mul_int W32)),
		   ("iminus", (minus_int W32)),
		   
		   ("uplus", (plus_uint W32)),
		   ("umult", (mul_uint W32)),
		   ("uminus", (minus_uint W32)),
		   
		   ("bplus", (plus_uint W8)),
		   ("bmult", (mul_uint W8)),
		   ("bminus", (minus_uint W8)),
		   
		   ("fplus", (plus_float F64)),
		   ("fmult", (mul_float F64)),
		   ("fminus", (minus_float F64)),

		   ("notb", (not_int W32)),
		   (">>", (rshift_uint W32)),
		   ("~>>", (rshift_int W32)),

		   ("andb", (and_int W32)),
		   ("xorb", (xor_int W32)),
		   ("orb", (or_int W32)),

		   ("uinta8touinta32", (uinta2uinta (W8,W32))),
		   ("uintv8touintv32", (uintv2uintv (W8,W32))),
		   
		   ("uint8toint32", (uint2int (W8,W32))),
		   ("uint8touint32", (uint2uint (W8,W32))),
		   
		   ("uint32toint32", (uint2int (W32,W32))),
		   ("uint32touint8", (uint2uint (W32,W8))),
		   
		   ("int32touint32", (int2uint (W32,W32))),
		   ("int32touint8", (int2uint (W32,W8))),
		   
		   ("int2float", (int2float)),
		   ("float2int", (float2int))
		   
		   (* XXX need to do unsigned and real stuff *)]

(* real_getexp should take a 64-bit IEEE float:
    (1) it the loads the 64-bit pattern as a long
    (2) shift right logical 52 bits
    (3) and to retain only low 11 bits
    (4) subtract 1023 from this quantity and return as an int *)

	  in  val _ = (app (add_top o exp_entry) topvalue_list;
		       app (add     o exp_entry) basevalue_list;
		       app (add_top o mono_entry) topprimvalue_list;
		       app (add     o mono_entry) baseprimvalue_list;
		       app (add     o ilmono_entry) baseilprimvalue_list)
	  end


	  (* ----------------- add base polymorphic variables -------------- *)
	  local
	      val toppolyvalue_list =
		  [(* NOT TOTAL! has a store effect - otherwise we would generalize *)
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
					     end))]
		   
	  in  val _ = (app (add_top o poly_entry) toppolyvalue_list;
		       app (add     o poly_entry) basepolyvalue_list)
	  end

	  local
	      val datatycs = [Ast.Db {tyc = Symbol.tycSymbol "bool",
				      tyvars = [],
				      rhs = Ast.Constrs [(Symbol.varSymbol "false", NONE),
							 (Symbol.varSymbol "true", NONE)]}]
	      val bool_sbnds_sdecs = 
		  Datatype.compile {context = empty_context,
				    typecompile = Toil.typecompile,
				    datatycs = datatycs,
				    eq_compile = Toil.xeq,
				    is_transparent = true}
	  in
	      val _ = app add_top bool_sbnds_sdecs
	  end     
      
      in  Bind.finish (!bindings)
      end

  end
