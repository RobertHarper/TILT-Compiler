(* Forms the initial basis for elaboration *)
functor Basis(structure Il : IL
	      structure IlContext : ILCONTEXT
	      structure IlStatic : ILSTATIC
	      structure Ppil : PPIL
	      structure IlUtil : ILUTIL
	      structure Datatype : DATATYPE
	      sharing IlContext.Il = Ppil.Il = IlUtil.Il = IlStatic.Il = Datatype.Il = Il) : BASIS =
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
      val default_fixity_table = map (fn (str,f) => (symbol_label(varSymbol(str)), f)) table
    end

    fun initial_context (xty, xeq) : context * sbnd list =
      let
	fun mk_var_lab str = symbol_label(Symbol.varSymbol str)
	fun mk_tyc_lab str = symbol_label(Symbol.tycSymbol str)
	fun mk_var str = fresh_named_var str
	fun binop_con(conopt) = let val con = (case conopt of 
						   NONE => fresh_con empty_context
						 | SOME c => c)
				in CON_ARROW(con_tuple[con,con],con,oneshot_init PARTIAL)
				end
	fun var_entry ctxt s c = add_context_sdec(ctxt,SDEC(mk_var_lab s, DEC_EXP(mk_var s, c)))
	fun type_entry ctxt s k c = add_context_sdec(ctxt,SDEC(mk_tyc_lab s, DEC_CON(mk_var s, k, SOME c)))
	fun exp_entry ctxt str e = 
	    let val inline = INLINE_EXPCON(e,IlStatic.GetExpCon(empty_context,e))
	    in  add_context_inline(ctxt, mk_var_lab str, fresh_named_var str, inline)
	    end
	fun mono_entry ctxt str prim = exp_entry ctxt str (PRIM (prim,[]))
	fun scon_entry ctxt str scon = exp_entry ctxt str (SCON scon)
	fun poly_entry ctxt str c2exp = 
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
	    in add_context_inline(ctxt,mk_var_lab str, fresh_named_var str, INLINE_MODSIG(m,s))
	    end
	fun over_entry ctxt str con_thunker constraints =
	    add_context_inline(ctxt,
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


	val context = empty_context
	val context = add_context_fixity(context,default_fixity_table)

	local
	    val basetype_list = [("float", CON_FLOAT F64),
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
	    fun folder ((s,c),ctxt) = 
		add_context_inline(ctxt,
				   symbol_label (Symbol.tycSymbol s),
				   fresh_named_var s,
				   INLINE_CONKIND(c,IlStatic.GetConKind(empty_context,c)))
	in
	    val context = foldl folder context basetype_list
	end


	val context = 
	   let
	       datatype X = A | B
	       fun con_thunk exp_oneshot x = (case (oneshot_deref exp_oneshot,x) of
						  (NONE,_) => ()
						| (_,A) => oneshot_set(exp_oneshot,PRIM (plus_int W32,[]))
						| (_,B) => oneshot_set(exp_oneshot,PRIM (plus_float F64,[])))
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
	   in over_entry context "over" con_thunk [cstr1,cstr2]
	   end

       val context = exp_entry context "true" true_exp
       val context = exp_entry context "false" false_exp
	   
(*
	   mono_entry "not" (NOT),
	   
	   mono_entry "size" (SIZE),
	   mono_entry "chr" (CHR),
	   mono_entry "ord" (ORD),
	   mono_entry "explode" (EXPLODE),
	   mono_entry "implode" (IMPLODE), 
*)
       val context = var_entry context "^" (CON_ARROW(con_tuple[con_string,con_string],
						      con_string,oneshot_init TOTAL))

       val context = poly_entry context "ref" (fn c => let val v = fresh_var()
						       in #1(make_total_lambda(v,c,CON_REF c,
									       APP(PRIM(mk_ref,[c]),VAR v)))
						       end)
       val context = poly_entry context "!" (fn c => let val v = fresh_var()
					     in #1(make_total_lambda(v,CON_REF c,c,
								     APP(PRIM(deref,[c]),VAR v)))
					     end)
       val context = poly_entry context ":=" 
	   (fn c => let val v = fresh_var()
			val pc = con_tuple[CON_REF c, c]
			fun proj n = RECORD_PROJECT(VAR v,generate_tuple_label n,pc)
		    in #1(make_total_lambda(v,pc,
					    con_unit,APP(PRIM(setref,[c]),
							 exp_tuple[proj 1, proj 2])))
		    end)
	   
       val context = mono_entry context "+" (plus_int W32)
       val context = mono_entry context "-" (minus_int W32)
       val context = mono_entry context "*" (mul_int W32)
       val context = mono_entry context "div" (div_int W32)
       val context = mono_entry context "mod" (mod_int W32)
       val context = mono_entry context "quot" (quot_int W32)
       val context = mono_entry context "rem" (rem_int W32)
       val context = mono_entry context "<" (less_int W32)
       val context = mono_entry context ">" (greater_int W32)
       val context = mono_entry context "<=" (lesseq_int W32)
       val context = mono_entry context ">=" (greatereq_int W32)
       val context = mono_entry context "<>" (neq_int W32)
       val context = mono_entry context "notb" (not_int W32)
       val context = mono_entry context "<<" (lshift_int W32)
       val context = mono_entry context ">>" (rshift_uint W32)
       val context = mono_entry context "~>>" (rshift_int W32)
       val context = mono_entry context "&&" (and_int W32)
       val context = mono_entry context "||" (or_int W32)
       val context = mono_entry context "~" (neg_int W32)
       val context = mono_entry context "abs" (abs_int W32)
	   (* XXX need to do unsigned and real stuff *)
	   
       val context = mono_entry context "floor" (float2int)
       val context = mono_entry context "real" (int2float)
	   
       val context = mono_entry context "output" output
       val context = mono_entry context "input" input


       fun tag_help ctxt str t con = add_context_sdec(ctxt,SDEC(fresh_internal_label str,
								DEC_EXCEPTION(t,con)))
	   
       val context = tag_help context "fail" fail_tag con_unit
       val context = tag_help context "bind" bind_tag con_unit
       val context = tag_help context "match" match_tag con_unit
	   
	   
       val (datatype_sdecs, datatype_sbnds) =
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
		    

		val list_sbnd_sdecs = Datatype.compile {context = context,
							typecompile = xty,
							datatycs = listdb : Ast.db list,
							withtycs = [] : Ast.tb list,
							eq_compile = xeq}
		    
		    
		val (mlist,slist) = (MOD_STRUCTURE(map #1 list_sbnd_sdecs),
				     SIGNAT_STRUCTURE(NONE, map #2 list_sbnd_sdecs))
		(*
                 val (mlist,slist) = (case (make_inline_module(context,mlist)) of
		                             SOME m => (
						  print "original mlist is\n";
						  pp_mod mlist;
						  print "inlinable mlist is\n";
						  pp_mod m;
						  print "\n";

						  (m,IlStatic.GetModSig(context,m)))
				     | NONE => (print "cannot inline module mlist = \n";
						pp_mod mlist;
						print "\n";
						error "cannot inline list module"))
		  handle e as (IlUtil.NOTFOUND s) => (print "\n***"; print s; print "\n"; raise e)
*)						 


	      val bool_sbnd_sdecs = Datatype.compile {context = context,
						      typecompile = xty,
						      datatycs = booldb : Ast.db list,
						      withtycs = [] : Ast.tb list,
						      eq_compile = xeq}
	      val (mbool,sbool) = (MOD_STRUCTURE(map #1 bool_sbnd_sdecs),
				   SIGNAT_STRUCTURE(NONE, map #2 bool_sbnd_sdecs))
	      val (mbool,sbool) = (case (make_inline_module(context,mbool)) of
				       SOME m => (m,IlStatic.GetModSig(context,m))
				     | NONE => (print "cannot inline bool mbool = \n";
						pp_mod mbool;
						print "\n";
						error "cannot inline bool module"))


	      val bool_label = open_internal_label "bools"
	      val bool_var = fresh_named_var "bools"
	      val list_label = open_internal_label "lists"
	      val list_var = fresh_named_var "lists"
	      val sbool = IlStatic.SelfifySig(SIMPLE_PATH bool_var, sbool)
	      val slist = IlStatic.SelfifySig(SIMPLE_PATH list_var, slist)
	    in
		(
(*		 [CONTEXT_INLINE(bool_label, bool_var, INLINE_MODSIG(mbool,sbool)),
		  CONTEXT_INLINE(list_label, list_var, INLINE_MODSIG(mlist,slist))], *)
		 [SDEC(bool_label, DEC_MOD(bool_var, sbool)),
		  SDEC(list_label, DEC_MOD(list_var, slist))],
		 [SBND(bool_label, BND_MOD(bool_var,mbool)),
		  SBND(list_label, BND_MOD(list_var,mlist))])
	    end

	val context = add_context_sdecs(context,datatype_sdecs)
      in
	  (context, datatype_sbnds)      
      end
    
  end
