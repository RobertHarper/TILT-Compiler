functor IlEval(structure Il : IL
	       structure IlStatic : ILSTATIC
	       structure IlUtil : ILUTIL
	       structure Ppil : PPIL
	       structure IlLookup : ILLOOKUP
	     sharing IlLookup.Il = Ppil.Il = IlUtil.Il = IlStatic.Il = Il)
   : ILEVAL =  
  struct

    structure Il = Il
    open Il IlStatic IlUtil Ppil 
    open Util Name IlLookup Tyvar
    open Prim

    val error = error "ileval.sml"
    val error_exp = error_exp "ileval.sml"
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()

    (*  ------------------  Functions to check for valuability ------------------------- *)

    fun exp_isval (exp : exp) = 
	(case exp of
	     (OVEREXP _ | VAR _ | APP _ | RECORD_PROJECT _ | SUM_TAIL _ 
	     | HANDLE _ | RAISE _ | LET _ | NEW_STAMP _ | REF _ | GET _ 
	     | SET _ | PROJ _ | CASE _ | EXN_CASE _ | MODULE_PROJECT _ | SEAL _ | SEQ _) => false
	   | (SCON _ | PRIM _ | FIX _ | TAG _) => true
	   | (RECORD rbnds) => andfold (fn (RBND(l,bnd)) => exp_isval bnd) rbnds
	   | EXN_INJECT (tag,e) => exp_isval e
	   | ROLL (c,e) => (con_isval c) andalso (exp_isval e)
	   | UNROLL (c,e) => (con_isval c) andalso (exp_isval e)
	   | INJ (cons,i,e) => (andfold con_isval cons) andalso (exp_isval e)
	   | LOC(c,er) => (con_isval c) andalso (exp_isval (!er)))

    and bnd_isval (bnd : bnd) = 
	(case bnd of
	     BND_EXP (v,e) => exp_isval e
	   | BND_MOD (v,m) => mod_isval m
	   | BND_CON (v,c) => con_isval c
	   | BND_FIXITY _ => true)

    and dec_isval (dec : dec) = 
	(case dec of
	     DEC_EXP (v,c) => con_isval c
	   | DEC_MOD (v,s) => true
	   | DEC_CON (v,k,NONE) => true
	   | DEC_CON (v,k,SOME c) => con_isval c
	   | DEC_EXCEPTION (_,c) => con_isval c
	   | DEC_FIXITY _ => true)

    and con_isval (con : con) =
	(case con of
	     (CON_OVAR _ | CON_APP _ 
	     | CON_TUPLE_PROJECT _ | CON_MODULE_PROJECT _) => false
	   | (CON_MUPROJECT(i,c)) => con_isval c
	   | (CON_VAR _ | CON_FUN _ | CON_INT | CON_UINT | CON_FLOAT | CON_CHAR | CON_ANY ) => true
	   | (CON_LIST c | CON_ARRAY c | CON_VECTOR c | CON_REF c | CON_TAG c) => con_isval c
	   | (CON_ARROW (c1,c2,_)) => (con_isval c1) andalso (con_isval c2)
	   | (CON_RECORD rdecs) => andfold (fn (RDEC(l,bnd)) => con_isval bnd) rdecs
	   | (CON_SUM (iopt,cons)) => andfold con_isval cons
	   | (CON_TUPLE_INJECT(cons)) => andfold con_isval cons)

    and mod_isval (module : mod) = 
	(case module of 
	     (MOD_VAR _ | MOD_APP _ | MOD_SEAL _ | MOD_PROJECT _) => false
	   | (MOD_FUNCTOR _) => true
	   | (MOD_STRUCTURE sbnds) => andfold (fn (SBND(l,bnd)) => bnd_isval bnd) sbnds)

    (* ------------------------------ Evaluations -------------------------- *)
    val indent = ref 0
    exception exn_packet of exp  (* we use ML's exceptions to model exceptions in the evaluator *)
    exception NOTFOUND of string
    type env = bnd list
    val init_env = []
    fun env_bndextend (env,bnd) = bnd::env
    fun env_modextend (env,v,m) = (BND_MOD(v,m))::env
    fun env_expextend (env,v,e) = (BND_EXP(v,e))::env
    fun env_conextend (env,v,c) = (BND_CON(v,c))::env

    fun lookup bnds tv = 
	let fun loop [] = (print "could not find var = ";
			   Ppil.pp_var tv; print " in binding list:\n";
			   Ppil.pp_bnds bnds;
			   print "\n\n";
			   raise NOTFOUND "lookup")
	      | loop ((cur as (BND_EXP(v,_) | BND_CON(v,_) | BND_MOD(v,_)))::rest) = 
	             if (eq_var(v,tv)) then cur else loop rest 
	      | loop (_::rest) = loop rest
	in loop bnds
	end

    fun structure_lookup sbnds l =
	let fun loop [] = (print "could not find label "; 
			   Ppil.pp_label l; print " in sbnds:\n";
			   Ppil.pp_sbnds sbnds;
			   print "\n\n";
			   error "no matching label to mod_project")
	      | loop (SBND(l',bnd)::rest) = if (eq_label(l,l'))
						then bnd
					    else loop rest
	in loop sbnds
	end
    fun con_metalookup lup pp k = (case lup k of
				      BND_CON (v,c) => c
				    | b => (print "con_metalookup of "; pp k;
					    print " found "; Ppil.pp_bnd b;
					    raise (NOTFOUND "con_metalookup")))
    fun exp_metalookup lup pp k = (case lup k of
				      BND_EXP (v,e) => e
				    | b => (print "exp_metalookup of "; pp k;
					    print " found "; Ppil.pp_bnd b;
					    raise (NOTFOUND "exp_metalookup")))
    fun mod_metalookup lup pp k = (case lup k of
				      BND_MOD (v,m) => m
				    | b => (print "mod_metalookup of "; pp k;
					    print " found "; Ppil.pp_bnd b;
					    raise (NOTFOUND "mod_metalookup")))
    fun con_lookup env = con_metalookup (lookup env) Ppil.pp_var
    fun exp_lookup env = exp_metalookup (lookup env) Ppil.pp_var
    fun mod_lookup env = mod_metalookup (lookup env) Ppil.pp_var
    fun con_str_lookup sbnds = con_metalookup (structure_lookup sbnds) Ppil.pp_label
    fun exp_str_lookup sbnds = exp_metalookup (structure_lookup sbnds) Ppil.pp_label
    fun mod_str_lookup sbnds = mod_metalookup (structure_lookup sbnds) Ppil.pp_label

    fun str2float str = (case (Float.fromString str) of 
			     NONE => error "bad string to convert to float"
			   | SOME f => f)
    val float2str = Float.toString



    fun eval_con env (con : con) : con = 
	(case con of
	    (CON_INT | CON_UINT | CON_FLOAT | CON_CHAR | CON_ANY) => con
	  | (CON_TYVAR tv) => (case (tyvar_deref tv) of
				  SOME c => eval_con env c
				| NONE => (print "encountered unresolved "; Ppil.pp_con con;
				      print " during eval_con";
				      error "encountered unresolved CON_TYVAR during eval_con"))
	  | (CON_VAR v) => con_lookup env v
	  | (CON_OVAR oc) => if (ocon_is_inst oc) then (ocon_deref oc) else error "uninst overloaded ovar"
	  | CON_LIST c => CON_LIST (eval_con env c)
	  | CON_ARRAY c => CON_ARRAY (eval_con env c)
	  | CON_VECTOR c => CON_VECTOR (eval_con env c)
	  | CON_REF c => CON_REF (eval_con env c)
	  | CON_TAG c => CON_TAG (eval_con env c)
	  | CON_ARROW (c1,c2,a) => CON_ARROW(eval_con env c1, eval_con env c2, a)
	  | CON_APP (c1,c2) => 
		let val c1' = eval_con env c1
		    val c2' = eval_con env c1
		in (case (c1',c2') of
			(CON_FUN ([],_),_) => error "CON_APP applied to CON_FUN taking no argument"
		      | (CON_FUN([v],body),_) => eval_con env (con_subst_var(body,[(v,c2')]))
		      | (CON_FUN(vars,body),CON_TUPLE_INJECT (args)) => 
							       eval_con env (con_subst_var(body,
											   zip vars args))
		      | _ => error "CON_APP applied to a first arg which is not CON_FUN")
		end
	  | CON_RECORD rdecs => CON_RECORD (map (fn (RDEC(l,c)) => RDEC(l,eval_con env c)) rdecs)
	  | CON_TUPLE_INJECT cs => CON_TUPLE_INJECT(map (eval_con env) cs)
	  | CON_TUPLE_PROJECT(i,c) => (case (eval_con env c) of
					   CON_TUPLE_INJECT cs => List.nth(cs,i-1)
					 | _ => error "Cannot project out from non CON_TUPLE_INJ")
	  | CON_MODULE_PROJECT (m,l) => (case (eval_mod env m) of
					     MOD_STRUCTURE sbnds => eval_con env (con_str_lookup sbnds l)
					   | _ => error "cannot mod_project from non MOD_STRUCT")
	  | CON_FUN _ => con
	  | CON_SUM (iopt,cs) => CON_SUM(iopt,map (eval_con env) cs)
	  | CON_MUPROJECT(i,c) => con)

    and reduce_con env (con : con) : con = 
	(case con of
	    (CON_INT | CON_UINT | CON_FLOAT | CON_CHAR | CON_ANY) => con
	  | (CON_VAR v) => (con_lookup env v
			    handle (NOTFOUND _) => con)
	  | (CON_TYVAR tv) => (case (tyvar_deref tv) of
				  SOME c => reduce_con env c
				| NONE => (print "encountered unresolved "; Ppil.pp_con con;
					   print " during reduce_con";
					   error "encountered unresolved CON_TYVAR during reduce_con"))
	  | (CON_OVAR oc) => if (ocon_is_inst oc) then (ocon_deref oc) else error "uninst overloaded ovar"
	  | CON_LIST c => CON_LIST (reduce_con env c)
	  | CON_ARRAY c => CON_ARRAY (reduce_con env c)
	  | CON_VECTOR c => CON_VECTOR (reduce_con env c)
	  | CON_REF c => CON_REF (reduce_con env c)
	  | CON_TAG c => CON_TAG (reduce_con env c)
	  | CON_ARROW (c1,c2,a) => CON_ARROW(reduce_con env c1, reduce_con env c2, a)
	  | CON_APP (c1,c2) => 
		let val c1' = reduce_con env c1
		    val c2' = reduce_con env c1
		in (case (c1',c2') of
			(CON_FUN ([],_),_) => error "CON_APP applied to CON_FUN taking no argument"
		      | (CON_FUN([v],body),_) => reduce_con env (con_subst_var(body,[(v,c2')]))
		      | (CON_FUN(vars,body),CON_TUPLE_INJECT (args)) => 
							       reduce_con env (con_subst_var(body,
											   zip vars args))
		      | _ => CON_APP(c1',c2'))
		end
	  | CON_RECORD rdecs => CON_RECORD (map (fn (RDEC(l,c)) => RDEC(l,reduce_con env c)) rdecs)
	  | CON_TUPLE_INJECT cs => CON_TUPLE_INJECT(map (reduce_con env) cs)
	  | CON_TUPLE_PROJECT(i,c) => (case (reduce_con env c) of
					   CON_TUPLE_INJECT cs => List.nth(cs,i-1)
					 | c' => CON_TUPLE_PROJECT(i,c'))
	  | CON_MODULE_PROJECT (m,l) => (case (reduce_mod env m) of
					     MOD_STRUCTURE sbnds => reduce_con env (con_str_lookup sbnds l)
					   | m' => CON_MODULE_PROJECT(m',l))
	  | CON_FUN _ => con
	  | CON_SUM (iopt,cs) => CON_SUM(iopt,map (reduce_con env) cs)
	  | CON_MUPROJECT(i,c) => con)

    and eval_prim (prim,arg) = 
	(case (prim,arg) of
	   (PRIM0 p0,_) =>
	     (case p0 of
		  (SOFT_VTRAPprim _ | SOFT_ZTRAPprim _ | HARD_VTRAPprim _ | HARD_ZTRAPprim _) => unit_exp)
	 | (PRIM1 p1, a) =>
	      (case (p1,a) of
		   (MK_REFprim {instance},_) => LOC(instance,ref a)
		 | (DEREFprim {instance}, LOC(_,er)) => (er := a; unit_exp)
		 | (DEREFprim _,_) => error "DEREF did not get loc"
        	 | (NEG_FLOATprim, SCON(FLOAT str)) => SCON(FLOAT(float2str(~(str2float str))))
		 | (NOT_INTprim, SCON(INT w)) => SCON(INT(Word32.notb w))
		 | (NEG_INTprim, SCON(INT w)) => SCON(INT(Word32.fromLargeInt(~(Word32.toLargeInt w))))
		 | (ABS_INTprim, SCON(INT w)) => SCON(INT(Word32.fromLargeInt(abs(Word32.toLargeInt w))))
		 | ((NOT_INTprim | NEG_INTprim | ABS_INTprim), _) => error "not/neg/abs_int got a non-int"
		 | (FLOAT2INTprim, SCON(FLOAT str)) => SCON(INT(Word32.fromInt(floor(str2float str))))
        	 | ((NEG_FLOATprim | ABS_FLOATprim | FLOAT2INTprim), _) => 
		       error "neg/abs/floor_float got a non-float"
		 | (INT2FLOATprim, SCON(INT w)) => SCON(FLOAT(float2str(real(Word32.toInt w))))
		 | (INT2UINTprim, SCON(INT w)) => SCON(UINT w)
		 | (UINT2INTprim, SCON(UINT w)) => SCON(INT w)
		 | ((INT2FLOATprim | INT2UINTprim), _) => error "INT2FLOAT or INT2UINTprim got non-int"
		 | (UINT2INTprim, _) => error "UINT2INT got non-uint"
		 | (LENGTH1prim {instance}, _) => raise UNIMP)
	 | (PRIM2 p2, RECORD[RBND(l1,a),RBND(l2,b)]) =>
	      (case (p2,a,b) of
		   (EQ_REFprim{instance}, LOC(_,er1), LOC(_,er2)) => if (er1 = er2) then true_exp else false_exp
		 | (SETREFprim{instance}, LOC(_,er1), _) => (er1 := b; unit_exp)
		 | (EQ_CHARprim, SCON(CHAR c1), SCON(CHAR c2)) => if (c1 = c2) then true_exp else false_exp
		 | (NEQ_CHARprim, SCON(CHAR c1), SCON(CHAR c2)) => if (not(c1 = c2)) then true_exp else false_exp
		 | ((PLUS_FLOATprim | MINUS_FLOATprim | MUL_FLOATprim | DIV_FLOATprim                
		    | LESS_FLOATprim | GREATER_FLOATprim | LESSEQ_FLOATprim | GREATEREQ_FLOATprim 
		    | EQ_FLOATprim | NEQ_FLOATprim), SCON(FLOAT str1), SCON(FLOAT str2)) =>
		       let val f1 = str2float str1
			   val f2 = str2float str2
			   fun bwrapper booler = if booler(f1,f2) then true_exp else false_exp
			   fun fwrapper fooler = SCON(FLOAT(float2str(fooler(f1,f2))))
		       in (case p2 of
			       PLUS_FLOATprim => fwrapper (op +)
			     | MINUS_FLOATprim => fwrapper (op -)
			     | MUL_FLOATprim => fwrapper (op * )
			     | DIV_FLOATprim => fwrapper (op /)
			     | LESS_FLOATprim => bwrapper (op <)
			     | GREATER_FLOATprim  => bwrapper (op >)
			     | LESSEQ_FLOATprim  => bwrapper (op <=)
			     | GREATEREQ_FLOATprim  => bwrapper (op >=)
			     | EQ_FLOATprim  => bwrapper (op =)
			     | NEQ_FLOATprim => bwrapper (not o (op =))
			     | _ => error "what happened to float op")
		       end
		 | ((PLUS_INTprim | MINUS_INTprim | MUL_INTprim | DIV_INTprim | MOD_INTprim
                    | QUOT_INTprim | REM_INTprim 
		    | LESS_INTprim | GREATER_INTprim | LESSEQ_INTprim | GREATEREQ_INTprim 
		    | EQ_INTprim | NEQ_INTprim
		    | LSHIFT_INTprim | RSHIFT_INTprim | AND_INTprim | OR_INTprim), 
		       SCON(INT w1), SCON(INT w2)) =>
		       let val i1 = Word32.toInt w1
			   val i2 = Word32.toInt w2
			   val ws2 = Word31.fromInt i2
			   fun wwrapper wooler = SCON(INT(wooler(w1,w2)))
			   fun wiwrapper wiooler = SCON(INT(wiooler(w1,ws2)))
			   fun bwrapper booler = if booler(i1,i2) then true_exp else false_exp
			   fun iwrapper iooler = SCON(INT(Word32.fromInt(iooler(i1,i2))))
		       in (case p2 of
			       PLUS_INTprim => iwrapper (op +)
			     | MINUS_INTprim => iwrapper (op -)
			     | MUL_INTprim => iwrapper (op * )
			     | DIV_INTprim => iwrapper (op div)
			     | MOD_INTprim => iwrapper (op mod)
			     | QUOT_INTprim => iwrapper (op quot)
			     | REM_INTprim => iwrapper (op rem)
			     | LESS_INTprim => bwrapper (op <)
			     | GREATER_INTprim  => bwrapper (op >)
			     | LESSEQ_INTprim  => bwrapper (op <=)
			     | GREATEREQ_INTprim  => bwrapper (op >=)
			     | EQ_INTprim  => bwrapper (op =)
			     | NEQ_INTprim => bwrapper (not o (op =))
			     | LSHIFT_UINTprim => wiwrapper(Word32.<<)
			     | RSHIFT_UINTprim => wiwrapper(Word32.>>)
			     | AND_UINTprim => wwrapper(Word32.andb)
			     | OR_UINTprim => wwrapper(Word32.orb)
			     | _ => error "what happened to int op")
		       end
		 | ((PLUS_UINTprim | MINUS_UINTprim | MUL_UINTprim | DIV_UINTprim | MOD_UINTprim
		    | LESS_UINTprim | GREATER_UINTprim | LESSEQ_UINTprim | GREATEREQ_UINTprim 
		    | EQ_UINTprim | NEQ_UINTprim 
		    | LSHIFT_UINTprim | RSHIFT_UINTprim | AND_UINTprim | OR_UINTprim), 
		       SCON(UINT w1), SCON(UINT w2)) =>
		       let val i2 = Word32.toInt w2
			   val ws2 = Word31.fromInt i2
			   fun bwrapper booler = if booler(w1,w2) then true_exp else false_exp
			   fun wwrapper wooler = SCON(UINT(wooler(w1,w2)))
			   fun wiwrapper wooler = SCON(UINT(wooler(w1,ws2)))
		       in (case p2 of
			       PLUS_UINTprim => wwrapper (Word32.+)
			     | MINUS_UINTprim => wwrapper (Word32.-)
			     | MUL_UINTprim => wwrapper (Word32.* )
			     | DIV_UINTprim => wwrapper (Word32.div)
			     | MOD_UINTprim => wwrapper (Word32.mod)
			     | LESS_UINTprim => bwrapper (Word32.<)
			     | GREATER_UINTprim  => bwrapper (Word32.>)
			     | LESSEQ_UINTprim  => bwrapper (Word32.<=)
			     | GREATEREQ_UINTprim  => bwrapper (Word32.>=)
			     | EQ_UINTprim  => bwrapper (op =)
			     | NEQ_UINTprim => bwrapper (not o (op =))
			     | LSHIFT_UINTprim => wiwrapper(Word32.<<)
			     | RSHIFT_UINTprim => wiwrapper(Word32.>>)
			     | AND_UINTprim => wwrapper(Word32.andb)
			     | OR_UINTprim => wwrapper(Word32.orb)
			     | _ => error "what happened to uint op")
		       end
		 | (SUB1prim _,_,_)  => raise UNIMP
		 | (ARRAY1prim _,_,_)  => raise UNIMP
		 | _ => error "could not evaluate some prim2")
	 | (PRIM2 p2, _) => error "prim2 did not get 2 arguments"
	 | (PRIM3 p3, RECORD[RBND(l1,a),RBND(l2,b),RBND(l3,c)]) =>
	      (case (p3,a,b,c) of
		   (UPDATE1prim{instance},_,_,_) => raise UNIMP)
	 | (PRIM3 p3, _) => error "prim3 did not get 3 arguments")

    and eval_exp env (exp : exp) : exp = 
	let 
	    val show = (!debug andalso (not (exp_isval exp)))
	    val _ = if show
			then (print (!indent); print " eval_exp got: "; Ppil.pp_exp exp; print "\n"; 
			      indent := (!indent + 1))
		    else ()
	    val res = eval_exp' env exp 
	    handle exn_packet p => raise (exn_packet p)
		 | e => (print "\n\nError while eval_exp doing: "; Ppil.pp_exp exp; print "\n"; raise e)
	    val _ = if (show)
			then (print (!indent); print "   returning: "; Ppil.pp_exp res; print "\n"; 
			      indent := (!indent - 1))
		    else ()
	in res
	end

    and eval_mod env (module : mod) : mod =
	let 
	    val _ =  if (!debug)
			 then (print (!indent); print " eval_mod got: "; 
			       Ppil.pp_mod module; print "\n\n";
			       indent := (!indent + 1))
		     else ()
	    val res = eval_mod' env module
		handle exn_packet p => raise (exn_packet p)
		     | e => (print "\n\nError while eval_mod doing: "; Ppil.pp_mod module; print "\n"; 
			     raise e)
	in res
	end

    (* traverse an expression to reduce constructors *)
    and reduce_exp env (exp : exp) : exp = 
	((* print "********** reduce_exp called on exp:";
	 Ppil.pp_exp exp; print "\n\n";  *)
	  case exp of
	    (FIX (fbnds,v)) => let fun help (FBND(v1,v2,c1,c2,e)) = FBND(v1,v2,reduce_con env c1,
									 reduce_con env c2, 
									 reduce_exp env e)
			       in FIX(map help fbnds, v)
			       end
	  | (APP(e1,e2)) => APP(reduce_exp env e1, reduce_exp env e2)
	  | CASE (cons,arg,arms,default) => 
			       ((* print "----------- REDUCING CASE cons:\n";
				Ppil.pp_con (CON_SUM(NONE,cons)); *)
				CASE(map (reduce_con env) cons, 
						 reduce_exp env arg, 
						 map (fn NONE => NONE | SOME e => SOME(reduce_exp env e)) arms, 
						 case default of 
						     NONE => NONE
						   | SOME e => SOME(reduce_exp env e)))
	  | LET (bnds,body) => LET(map (reduce_bnd env) bnds, reduce_exp env body)
	  | HANDLE(e1,e2) => HANDLE(reduce_exp env e1, reduce_exp env e2)
	  | RAISE e => RAISE(reduce_exp env e)
	  | SUM_TAIL(c,e) => SUM_TAIL(reduce_con env c, reduce_exp env e)
	  | ROLL(c,e) => ROLL(reduce_con env c, reduce_exp env e)
	  | UNROLL(c,e) => UNROLL(reduce_con env c, reduce_exp env e)
	  | MODULE_PROJECT (m,l) => let val m' = reduce_mod env m
				    in case m' of 
					MOD_STRUCTURE sbnds => 
					    let fun loop [] = error "reduce_exp cannot perform project"
						  | loop ((SBND(ll,BND_EXP(_,e)))::rest) = if (eq_label(l,ll))
											   then reduce_exp env e
										       else loop rest
						  | loop (_::rest) = loop rest
					    in loop sbnds
					    end
				      | _ => MODULE_PROJECT(m',l)
				    end
	  | _ => exp)

    and eval_exp' env (exp : exp) : exp = 
	(case exp of
	     (OVEREXP (_,_,oe)) => (case oneshot_deref oe of
					NONE => error "uninst overloaded exp"
				      | SOME e => eval_exp env e)
	   | (SCON _ | PRIM _ | TAG _ | LOC _) => exp
	   | (FIX (fbnds,v)) => let fun help (FBND(v1,v2,c1,c2,e)) = 
		                             FBND(v1,v2,
						  reduce_con env c1,
						  reduce_con env c2, 
						  (case (subst_var(BND_EXP(fresh_var(),reduce_exp env e),env)) of
						       BND_EXP(v,e) => e
						     | _ => error "subst_var got exp, returned non-exp"))
			       in FIX(map help fbnds, v)
			       end
	   | (VAR v) => eval_exp env (exp_lookup env v)
	   | (APP (e1,e2)) => let val e1' = eval_exp env e1
				  val e2' = eval_exp env e2
			      in (case e1' of
				      FIX (fbnds,tarv) => 
					  let fun loop [] = error "ill-formed FIX"
						| loop ((FBND(v,argv,argc,resc,body))::rest) = 
					      if (eq_var(v,tarv)) then (argv,body) else loop rest
					      val (argv,body) = loop fbnds
					      val table = map (fn (FBND(v,_,_,_,_)) => (v,FIX(fbnds,v))) fbnds
					      val body' = exp_subst_var(body,(argv,e2')::table)
					  in eval_exp env body'
					  end
				    | PRIM prim => eval_prim(prim,e2')
				    | _ => error_exp exp "APP applying a non-(FIX or PRIM)")
			      end
	   | (RECORD rbnds) => RECORD(map (fn (RBND(l,e)) => RBND(l,eval_exp env e)) rbnds)
	   | (RECORD_PROJECT (e,l,c)) => let fun loop [] = error "rec projection did not find label"
					       | loop ((RBND(ll,v))::rest) = if (eq_label(l,ll)) 
										 then v else loop rest
					 in loop (case (eval_exp env e) of
						      RECORD rbnds => rbnds
						    | _ => error "projection not from record")
					 end
	   | SUM_TAIL (c,e) => (case (eval_exp env e, eval_con env c) of
					 (INJ(_,i,ee),CON_SUM(SOME i',_)) => 
					     if (i=i') then ee 
					     else error "SUM_TAIL: adornment and exp type mismatch"
				       | (e',c') => error_exp (SUM_TAIL (c',e'))
							       "SUM_TAIL: unexpected val/types")
	   | HANDLE (body,handler) => (eval_exp env body 
					 handle (exn_packet e) => eval_exp env (APP(handler,e)))
	   | RAISE e => raise (exn_packet e)
	   | LET ([],body) => eval_exp env body
	   | LET (bnd::bnds,body) => let val env' = env_bndextend(env,eval_bnd env bnd)
				     in eval_exp env' (LET(bnds,body))
				     end
	   | NEW_STAMP c => TAG(fresh_tag(),eval_con env c)
	   | EXN_INJECT (e1,e2) => EXN_INJECT(eval_exp env e1, eval_exp env e2)
	   | REF (c,e) => LOC(eval_con env c, ref (eval_exp env e))
	   | GET (c,e) => (case (eval_exp env e) of
				     LOC(_,vr) => !vr
				   | _ => error "Can't do a GET on a non-loc value")
	   | SET (c,e1,e2) => (case (eval_exp env e1) of
				     LOC(_,vr) => (vr := (eval_exp env e2); unit_exp)
				   | _ => error "Can't do a GET on a non-loc value")
	   | ROLL(c,e) => ROLL(eval_con env c, eval_exp env e)
	   | UNROLL(c,e) => let val c' = eval_con env c
				val e' = eval_exp env e
			    in case e' of
				ROLL(c'',e'') => if (eq_con(c',c'',[])) then e'' 
						 else error_exp (UNROLL(c',ROLL(c'',e''))) 
						     "UNROLL(ROLL(e)) bad"
			      | _ => error_exp e' "UNROLL did not get a ROLL arg"
			    end
	   | INJ (cs,i,e) => INJ(map (eval_con env) cs, i, eval_exp env e)
	   | PROJ(c,i,e) => (case (eval_exp env e) of
					 INJ(_,i',ee) => if (i=i') 
							     then ee
							 else error "Projection from sum type failed"
				       | _ => error "Projection not from a sum type value")
	   | CASE (cons,arg,arms,default) => 
               (case (eval_exp env arg) of
		    ee as INJ(_,i,_) => 
			let fun loop 1 [] = error "too few arms in CASE or bad sum value"
			      | loop 1 (NONE::rest) = (case default of
							   NONE => error "no matches and no def in CASE"
							 | SOME e => eval_exp env e)
			      | loop 1 ((SOME e)::rest) = eval_exp env (APP(e,ee))
			      | loop n [] = error "too few arms in CASE or bad sum value"
			      | loop n (_::rest) = loop (n-1) rest
			in loop i arms
			end
	          | v => error_exp v "CASE got a non-sum value arguments")
	   | EXN_CASE (arg,arms,eopt) => 
		    (case (eval_exp env arg) of
			 (ep as (EXN_INJECT(tag as (TAG(t,c)),value))) => 
			     let fun loop [] = (case eopt of
						    NONE => raise (exn_packet ep)
						  | SOME e => eval_exp env (APP(e,ep)))
				   | loop ((e1,_,e2)::rest) = 
				 case (eval_exp env e1) of
				     (tag' as (TAG(t',c'))) => if (eq_tag(t,t'))
								then eval_exp env (APP(e2,value))
							    else loop rest
				   | _ => error "EXN_CASE arms not labelled with tags"
			     in loop arms
			     end
		       | _ => error "EXN_CASE got a non EXN_INJECT value")
	   | MODULE_PROJECT (m,l) =>  let val m' = eval_mod env m
				      in case m' of 
					  MOD_STRUCTURE sbnds => exp_str_lookup sbnds l
					| _ => error_exp exp "MODULE_PROJECT applied to non-s"
				      end
	   | SEAL (e,c) => eval_exp env e
	   | SEQ(elist) => foldl (fn (e,cur) => eval_exp env e) (SCON (INT 0w0)) elist)

    and eval_bnd (env : env) (bnd : bnd) : bnd = 
	(case bnd of
	     BND_EXP(v,e) => BND_EXP(v,eval_exp env e)
	   | BND_MOD(v,m) => BND_MOD(v,eval_mod env m)
	   | BND_CON(v,c) => BND_CON(v,eval_con env c)
	   | _ => bnd)

    and reduce_bnd (env : env) (bnd : bnd) : bnd = 
	(case bnd of
	     BND_EXP(v,e) => BND_EXP(v,reduce_exp env e)
	   | BND_CON(v,c) => BND_CON(v,reduce_con env c)
	   | BND_MOD(v,m) => BND_MOD(v,reduce_mod env m)
	   | BND_FIXITY _ => bnd)

    and reduce_mod (env : env) (module : mod) : mod = 
	(case module of
	     MOD_STRUCTURE sbnds => MOD_STRUCTURE(map (fn (SBND(l,b)) => SBND(l,reduce_bnd env b)) sbnds)
	   | MOD_APP(m1,m2) => MOD_APP(reduce_mod env m1, reduce_mod env m2)
	   | MOD_PROJECT(m,l) => let val m' = reduce_mod env m
				    in case m' of 
					MOD_STRUCTURE sbnds => 
					    let fun loop [] = error "reduce_exp cannot perform project"
						  | loop ((SBND(ll,BND_MOD(_,m)))::rest) = if (eq_label(l,ll))
											   then reduce_mod env m
										       else loop rest
						  | loop (_::rest) = loop rest
					    in loop sbnds
					    end
				      | _ => MOD_PROJECT (m',l)
				 end
	   | MOD_FUNCTOR(v,s,m) => MOD_FUNCTOR(v,reduce_sig env s, reduce_mod env m)
	   | _ => module)

    and reduce_sig (env : env) (signat : signat) : signat = 
	(case signat of
	     SIGNAT_STRUCTURE sdec => SIGNAT_STRUCTURE sdec
	   | SIGNAT_FUNCTOR (v,s1,s2,arrow) => signat
	   | _ => signat)

    and eval_mod' (env : env) (module : mod) : mod = 
	(case module of
	     MOD_VAR v => mod_lookup env v
	   | MOD_STRUCTURE sbnds => 
		 let 
		     fun loop env [] = []
		       | loop env ((SBND(l,bnd))::rest) = 
			 let val bnd' = eval_bnd env bnd
                             val env' = env_bndextend(env,bnd')
			     fun help (SBND(l,b)) = 
				 SBND(l,case subst_var(b,[bnd']) of
				      BND_EXP(v,e) => BND_EXP(v,reduce_exp env e)
				    | BND_CON(v,c) => BND_CON(v,reduce_con env c)
				    | BND_MOD(v,m) => BND_MOD(v,reduce_mod env m)
				    | b => b)
			     val rest' = map help rest
			 in (SBND(l,bnd'))::(loop env' rest')
			 end
		     val sbnds' = loop env sbnds
		 in MOD_STRUCTURE sbnds'
		 end
	   | MOD_FUNCTOR _ => module
	   | MOD_APP (m1,m2) => let 
				    val m1' = eval_mod env m1
				    val m2' = eval_mod env m2
				in (case m1' of
					MOD_FUNCTOR (v,s,body) => eval_mod (env_modextend(env,v,m2')) body
				      | _ => error "MOD_APP applied to non-functor")
				end
	   | MOD_PROJECT (m,l) => let val m' = eval_mod env m
				  in case m' of 
				      MOD_STRUCTURE sbnds => mod_str_lookup sbnds l
				    | _ => (print "MOD_PROJECT applied to non-structure in:";
					    Ppil.pp_mod module;
					    error  "MOD_PROJECT applied to non-structure")
				  end
	   | MOD_SEAL (m,s) => eval_mod env m)

    val eval_con = fn (c : con) => eval_con [] c
    val eval_exp = fn (e : exp) => (indent := 0; eval_exp [] e)
    val eval_mod = fn (m : mod) => eval_mod [] m
  end
