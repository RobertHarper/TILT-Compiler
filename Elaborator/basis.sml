(* Forms the initial basis for elaboration *)
functor Basis(structure Il : IL
	      structure IlUtil : ILUTIL
	      structure Datatype : DATATYPE
	      sharing IlUtil.Il = Datatype.Il = Il) : BASIS =
  struct

    structure Il = Il
    structure Datatype = Datatype
    open Il IlUtil Datatype
    open Util Name Prim Tyvar

    val empty_context : context = CONTEXT []
    val error = error "basis.sml"

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

    fun initial_context (exp2con, mod2sig, xty) : context =
      let
	fun mk_var_lab str = symbol2label(Symbol.varSymbol str)
	fun mk_tyc_lab str = symbol2label(Symbol.tycSymbol str)
	fun mk_var str = fresh_named_var str
	fun binop_con(conopt) = let val con = (case conopt of NONE => fresh_con() | SOME c => c)
				in CON_ARROW(con_tuple[con,con],con,oneshot_init PARTIAL)
				end
	fun var_entry s c = CONTEXT_VAR(mk_var_lab s, mk_var s, c)
	fun type_entry s k c = CONTEXT_CONVAR(mk_tyc_lab s, mk_var s, k, SOME c)
	fun exp_entry str e = CONTEXT_INLINE(mk_var_lab str, INLINE_EXPCON(e,exp2con e))
	fun mono_entry str prim = exp_entry str (PRIM prim)
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
				   in CONTEXT_INLINE(mk_var_lab str, INLINE_MODSIG(m,s))
				   end
	fun over_entry str exp bvc = 
	  CONTEXT_INLINE(mk_var_lab str, 
			 INLINE_OVER(fn _ => 
				       let 
					 fun subst(c,v_tv) = let fun help (v,tv) = (v,CON_TYVAR tv)
							     in con_subst_var(c,map help v_tv)
							     end
					 val oc = ocon_inst (fresh_ocon bvc) subst
				       in (exp,oc)
				       end))

	val fixity_entries = 
	  [CONTEXT_MODULE(fresh_open_label(),fresh_var(),
			  SIGNAT_STRUCTURE[SDEC(fresh_int_label(),DEC_FIXITY default_fixity_table)])]

	val datatype_entries =
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
	      val (mlist,slist) = Datatype.compile {context = CONTEXT [],
						    typecompile = xty,
						    datatycs = listdb : Ast.db list,
						    withtycs = [] : Ast.tb list}
	      val (mbool,sbool) = Datatype.compile {context = CONTEXT [],
						    typecompile = xty,
						    datatycs = booldb : Ast.db list,
						    withtycs = [] : Ast.tb list}
	    in
		[CONTEXT_MODULE(fresh_open_label(),fresh_var(),sbool),
		 CONTEXT_MODULE(fresh_open_label(),fresh_var(),slist)]
	    end

	val val_entries = 
	  [
	   over_entry "over" 
	   (fn [i] => if (i=0) then PRIM (PRIM2 PLUS_INTprim)
		     else if (i=1) then PRIM (PRIM2 PLUS_FLOATprim)
			  else error "over receieved int not 0 or 1"
	     | _ => error "over did not receiev 1 int")

	    let val tv = fresh_tyvar "ir"
	      val c = CON_TYVAR tv
	      val v = tyvar_getvar tv
	      val body = CON_ARROW(con_tuple[c,c],c,oneshot_init TOTAL)
	      val var_constraint = [(v,[CON_INT, CON_FLOAT])]
	    in  (body,var_constraint)
	    end,
	   
	   scon_entry "true" (BOOL true),
	   scon_entry "false" (BOOL false),
	   
(*
	   mono_entry "not" (PRIM1 NOTprim),
	   
	   mono_entry "size" (PRIM1 SIZEprim),
	   mono_entry "chr" (PRIM1 CHRprim),
	   mono_entry "ord" (PRIM1 ORDprim),
	   mono_entry "explode" (PRIM1 EXPLODEprim),
	   mono_entry "implode" (PRIM1 IMPLODEprim),
	   mono_entry "^" (PRIM2 STRING_CONCATprim), *)

	   poly_entry "ref" (fn c => let val v = fresh_var()
				     in #1(make_lambda(v,c,CON_REF c,REF(c,VAR v)))
				     end),
	   poly_entry "!" (fn c => let val v = fresh_var()
				   in #1(make_lambda(v,CON_REF c,c,GET(c,VAR v)))
				   end),
	   poly_entry ":=" (fn c => let val v = fresh_var()
					val pc = con_tuple[CON_REF c, c]
					fun proj n = RECORD_PROJECT(VAR v,generate_tuple_label n,pc)
				    in #1(make_lambda(v,pc,
						      con_unit,SET(c,proj 1, proj 2)))
				    end),
	   
	   mono_entry "+" (PRIM2 PLUS_INTprim),
	   mono_entry "-" (PRIM2 MINUS_INTprim),
	   mono_entry "*" (PRIM2 MUL_INTprim),
	   mono_entry "div" (PRIM2 DIV_INTprim),
	   mono_entry "mod" (PRIM2 MOD_INTprim),
	   mono_entry "quot" (PRIM2 QUOT_INTprim),
	   mono_entry "rem" (PRIM2 REM_INTprim),
	   mono_entry "<" (PRIM2 LESS_INTprim),
	   mono_entry ">" (PRIM2 GREATER_INTprim),
	   mono_entry "<=" (PRIM2 LESSEQ_INTprim),
	   mono_entry ">=" (PRIM2 GREATEREQ_INTprim),
	   mono_entry "<>" (PRIM2 NEQ_INTprim),
	   mono_entry "notb" (PRIM1 NOT_INTprim),
	   mono_entry "<<" (PRIM2 LSHIFT_INTprim),
	   mono_entry ">>" (PRIM2 RSHIFT_INTprim),
	   mono_entry "&&" (PRIM2 AND_INTprim),
	   mono_entry "||" (PRIM2 OR_INTprim),
	   mono_entry "~" (PRIM1 NEG_INTprim),
	   mono_entry "abs" (PRIM1 ABS_INTprim),
	   (* XXX need to do unsigned and real stuff *)
	   
	   mono_entry "floor" (PRIM1 FLOAT2INTprim),
	   mono_entry "real" (PRIM1 INT2FLOATprim)
	   
(*	   mono_entry "output" (PRIM2 OUTPUTprim) *)
	   
	   ]

      in
	CONTEXT (fixity_entries (* @ datatype_entries *) @ val_entries)
      end
    
  end