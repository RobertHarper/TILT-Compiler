(* Forms the initial basis for elaboration *)
functor Basis(structure Il : IL
	      structure Ppil : PPIL
	      structure IlUtil : ILUTIL
	      structure Datatype : DATATYPE
	      sharing Ppil.Il = IlUtil.Il = Datatype.Il = Il) : BASIS =
  struct

    structure Il = Il
    structure Datatype = Datatype
    open Il IlUtil Datatype Ppil
    open Util Name Prim Tyvar

    val empty_context : context = CONTEXT []
    val error = fn s => error "basis.sml" s

    local
      open Ast
      open Symbol
      open Fixity
    in 
      val table = [(":=", infixleft 3), (* XXX is this right? *)
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
      val default_fixity_table = map (fn (str,f) => (symbol2label(varSymbol(str)), f)) table
    end

    fun initial_context (exp2con, mod2sig, xty) : context * sbnd list =
      let
	fun mk_var_lab str = symbol2label(Symbol.varSymbol str)
	fun mk_tyc_lab str = symbol2label(Symbol.tycSymbol str)
	fun mk_var str = fresh_named_var str
	fun binop_con(conopt) = let val con = (case conopt of NONE => fresh_con() | SOME c => c)
				in CON_ARROW(con_tuple[con,con],con,oneshot_init PARTIAL)
				end
	fun var_entry s c = CONTEXT_SDEC(SDEC(mk_var_lab s, DEC_EXP(mk_var s, c)))
	fun type_entry s k c = CONTEXT_SDEC(SDEC(mk_tyc_lab s, DEC_CON(mk_var s, k, SOME c)))
	fun exp_entry str e = CONTEXT_INLINE(mk_var_lab str, fresh_named_var str, INLINE_EXPCON(e,exp2con e))
	fun mono_entry str prim = exp_entry str (PRIM (prim,[]))
	fun scon_entry str scon = exp_entry str (SCON scon)
	fun poly_entry str c2exp = let val argvar = fresh_var()
				       val l = fresh_int_label()
				       val argsig = SIGNAT_STRUCTURE[SDEC(l,DEC_CON(fresh_var(),
										    KIND_TUPLE 1, 
										    NONE))]
				       val instcon = CON_MODULE_PROJECT(MOD_VAR argvar,l)
				       val resmod = MOD_STRUCTURE[SBND(it_lab,
								       BND_EXP(fresh_var(),
									       (c2exp instcon)))]
				       val m = MOD_FUNCTOR(argvar,argsig,resmod)
				       val s = mod2sig m
				   in CONTEXT_INLINE(mk_var_lab str, fresh_named_var str, INLINE_MODSIG(m,s))
				   end
	fun over_entry str exp_thunk constraints =
	  CONTEXT_INLINE(mk_var_lab str, 
			 fresh_named_var str,
			 INLINE_OVER(fn _ => 
				     let val ocon = uocon_inst (fresh_uocon constraints)
				     in (exp_thunk,ocon)
				     end))

	val fixity_entries = 
	    [CONTEXT_SDEC(SDEC(fresh_int_label(),DEC_FIXITY default_fixity_table))]

	val (datatype_entries, datatype_sbnds) =
	    let
		open Ast 
		open Symbol
		val booldb = [Db
                             {def=[(varSymbol "true",NONE),
                                   (varSymbol "false",NONE)],
                              tyc=tycSymbol "bool",tyvars=[]}]
		val listdb = [Db
			      {def=[(varSymbol "::",
				     SOME
				     (TupleTy
				      [VarTy (Tyv (tyvSymbol "'a")),
				       ConTy
					([tycSymbol "list"],
					 [VarTy
					  (Tyv (tyvSymbol "'a"))])])),
				    (varSymbol "nil",NONE)],
			       tyc=tycSymbol "list",
			       tyvars=[Tyv (tyvSymbol "'a")]}]
	      val list_sbnd_sdecs = Datatype.compile {context = CONTEXT [],
						      typecompile = xty,
						      datatycs = listdb : Ast.db list,
						      withtycs = [] : Ast.tb list}
	      val (mlist,slist) = (MOD_STRUCTURE(map #1 list_sbnd_sdecs),
				   SIGNAT_STRUCTURE(map #2 list_sbnd_sdecs))
	      val (mlist,slist) = (case (make_inline_module(CONTEXT[],mlist)) of
				       SOME m => (
(*
						  print "original mlist is\n";
						  pp_mod mlist;
						  print "inlinable mlist is\n";
						  pp_mod m;
						  print "\n";
*)
						  (m,mod2sig m)) (* slist *)
				     | NONE => (print "cannot inline module mlist = \n";
						pp_mod mlist;
						print "\n";
						error "cannot inline list module"))
	      val bool_sbnd_sdecs = Datatype.compile {context = CONTEXT [],
						      typecompile = xty,
						      datatycs = booldb : Ast.db list,
						      withtycs = [] : Ast.tb list}
	      val (mbool,snool) = (MOD_STRUCTURE(map #1 bool_sbnd_sdecs),
				   SIGNAT_STRUCTURE(map #2 bool_sbnd_sdecs))
	      val (mbool,sbool) = (case (make_inline_module(CONTEXT[],mbool)) of
				       SOME m => (m,mod2sig m)
				     | NONE => (print "cannot inline bool mbool = \n";
						pp_mod mbool;
						print "\n";
						error "cannot inline bool module"))
(*
	      val _ = (print "DONE BOOLS with mbool = \n";
		       pp_mod mbool; print "\n")
*)
	      val bool_label = fresh_named_open_label "bools"
	      val bool_var = fresh_named_var "bools"
	      val list_label = fresh_named_open_label "lists"
	      val list_var = fresh_named_var "lists"
	    in
		([CONTEXT_INLINE(bool_label, bool_var, INLINE_MODSIG(mbool,sbool)),
		  CONTEXT_INLINE(list_label, list_var, INLINE_MODSIG(mlist,slist))],
		 [SBND(bool_label, BND_MOD(bool_var,mbool)),
		  SBND(list_label, BND_MOD(list_var,mlist))])
	    end

	local
	    val basetype_list = [("float", CON_FLOAT F32),
				 ("int", CON_INT W32), 
				 ("uint", CON_UINT W32),
				 ("real", CON_FLOAT F64), 
				 ("char", CON_UINT W8), 
				 ("string", con_string),
				 ("exn",CON_ANY),
				 ("unit", con_unit)]
	    fun entry_maker (s,c) = CONTEXT_INLINE(symbol2label (Symbol.tycSymbol s),
						   fresh_named_var s,
						   INLINE_CONKIND(c,KIND_TUPLE 1))
	in
	    val type_entries = map entry_maker basetype_list
	end

	val val_entries = 
	  [
	   over_entry "over" 
	   (fn i => if (i=0) then PRIM (plus_int W32,[])
		    else if (i=1) then PRIM (plus_float F64,[])
			 else error "over receieved int not 0 or 1")
	   [CON_ARROW(con_tuple[CON_INT W32, CON_INT W32], CON_INT W32, oneshot_init PARTIAL),
	    CON_ARROW(con_tuple[CON_FLOAT F64, CON_FLOAT F64], CON_FLOAT F64, oneshot_init PARTIAL)],
	   
	   exp_entry "true" true_exp,
	   exp_entry "false" false_exp,
	   
(*
	   mono_entry "not" (NOT),
	   
	   mono_entry "size" (SIZE),
	   mono_entry "chr" (CHR),
	   mono_entry "ord" (ORD),
	   mono_entry "explode" (EXPLODE),
	   mono_entry "implode" (IMPLODE), 
*)
	   var_entry "^" (CON_ARROW(con_tuple[con_string,con_string],con_string,oneshot_init TOTAL)),

	   poly_entry "ref" (fn c => let val v = fresh_var()
				     in #1(make_lambda(v,c,CON_REF c,MK_REF(VAR v)))
				     end),
	   poly_entry "!" (fn c => let val v = fresh_var()
				   in #1(make_lambda(v,CON_REF c,c,GET(VAR v)))
				   end),
	   poly_entry ":=" (fn c => let val v = fresh_var()
					val pc = con_tuple[CON_REF c, c]
					fun proj n = RECORD_PROJECT(VAR v,generate_tuple_label n,pc)
				    in #1(make_lambda(v,pc,
						      con_unit,SET(proj 1, proj 2)))
				    end),
	   
	   mono_entry "+" (plus_int W32),
	   mono_entry "-" (minus_int W32),
	   mono_entry "*" (mul_int W32),
	   mono_entry "div" (div_int W32),
	   mono_entry "mod" (mod_int W32),
	   mono_entry "quot" (quot_int W32),
	   mono_entry "rem" (rem_int W32),
	   mono_entry "<" (less_int W32),
	   mono_entry ">" (greater_int W32),
	   mono_entry "<=" (lesseq_int W32),
	   mono_entry ">=" (greatereq_int W32),
	   mono_entry "<>" (neq_int W32),
	   mono_entry "notb" (not_int W32),
	   mono_entry "<<" (lshift_int W32),
	   mono_entry ">>" (rshift_uint W32),
	   mono_entry "~>>" (rshift_int W32),
	   mono_entry "&&" (and_int W32),
	   mono_entry "||" (or_int W32),
	   mono_entry "~" (neg_int W32),
	   mono_entry "abs" (abs_int W32),
	   (* XXX need to do unsigned and real stuff *)
	   
	   mono_entry "floor" (float2int),
	   mono_entry "real" (int2float),
	   
	   mono_entry "output" output,
	   mono_entry "input" input
	   ]

      in
	(CONTEXT (fixity_entries @ type_entries @ datatype_entries @ val_entries),
	 datatype_sbnds)
      end
    
  end
