functor IlEval(structure Il : IL
	       structure IlStatic : ILSTATIC
	       structure IlUtil : ILUTIL
	       structure PrimUtil : PRIMUTIL
	       structure Ppil : PPIL
	       structure IlContext : ILCONTEXT
	       sharing IlContext.Il = Ppil.Il = IlUtil.Il = IlStatic.Il = Il
	       sharing Il.Prim = PrimUtil.Prim
	       sharing type PrimUtil.con = Il.con
	       sharing type PrimUtil.exp = Il.exp)
   : ILEVAL =  
  struct

    structure Il = Il
    structure Float = Real64
    open Il IlStatic IlUtil Ppil 
    open Util Listops Name (* IlLookup *) Tyvar
    open IlContext
    open Prim

    val error = fn s => error "ileval.sml" s
    val error_exp = fn e => fn s => error_exp "ileval.sml" e s

    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()

    (*  ------------------  Functions to check for valuability ------------------------- *)

    fun exp_isval (exp : exp) = 
	(case exp of
	     (OVEREXP _ | VAR _ | APP _ | RECORD_PROJECT _ | SUM_TAIL _ 
	     | HANDLE _ | RAISE _ | LET _ | NEW_STAMP _ 
	     | CASE _ | EXN_CASE _ | MODULE_PROJECT _ 
	     | SEAL _) => false
           | (SCON (vector _)) => true (* XXX not really *)
	   | (SCON (array _)) => true  (* XXX not really *)
	   | (SCON _ | PRIM _ | FIX _ | TAG _) => true
	   | (RECORD rbnds) => andfold (fn (l,bnd) => exp_isval bnd) rbnds
	   | EXN_INJECT (tag,e) => exp_isval e
	   | ROLL (c,e) => (con_isval c) andalso (exp_isval e)
	   | UNROLL (c,e) => (con_isval c) andalso (exp_isval e)
	   | INJ (cons,i,e) => (andfold con_isval cons) andalso (exp_isval e))

    and bnd_isval (bnd : bnd) = 
	(case bnd of
	     BND_EXP (v,e) => exp_isval e
	   | BND_MOD (v,m) => mod_isval m
	   | BND_CON (v,c) => con_isval c)

    and dec_isval (dec : dec) = 
	(case dec of
	     DEC_EXP (v,c) => con_isval c
	   | DEC_MOD (v,s) => true
	   | DEC_CON (v,k,NONE) => true
	   | DEC_CON (v,k,SOME c) => con_isval c
	   | DEC_EXCEPTION (_,c) => con_isval c)

    and con_isval (con : con) =
	(case con of
	     (CON_OVAR _ | CON_APP _ 
	     | CON_TUPLE_PROJECT _ | CON_MODULE_PROJECT _) => false
	   | (CON_MUPROJECT(i,c)) => con_isval c
	   | (CON_VAR _ | CON_FUN _ | CON_INT _ | CON_UINT _ | CON_FLOAT _ | CON_ANY ) => true
	   | (CON_ARRAY c | CON_VECTOR c | CON_REF c | CON_TAG c) => con_isval c
	   | (CON_ARROW (c1,c2,_)) => (con_isval c1) andalso (con_isval c2)
	   | (CON_RECORD rdecs) => andfold (fn (l,bnd) => con_isval bnd) rdecs
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
	    (CON_INT _ | CON_UINT _ | CON_FLOAT _ | CON_ANY) => con
	  | (CON_TYVAR tv) => (case (tyvar_deref tv) of
				  SOME c => eval_con env c
				| NONE => (print "encountered unresolved "; Ppil.pp_con con;
				      print " during eval_con";
				      error "encountered unresolved CON_TYVAR during eval_con"))
	  | (CON_VAR v) => con_lookup env v
	  | (CON_OVAR oc) => eval_con env (CON_TYVAR (ocon_deref oc))
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
		      | (CON_FUN([v],body),_) => eval_con env (con_subst_convar(body,[(v,c2')]))
		      | (CON_FUN(vars,body),CON_TUPLE_INJECT (args)) => 
							       eval_con env (con_subst_convar(body,
											   zip vars args))
		      | _ => error "CON_APP applied to a first arg which is not CON_FUN")
		end
	  | CON_RECORD rdecs => CON_RECORD (map (fn (l,c) => (l,eval_con env c)) rdecs)
	  | CON_TUPLE_INJECT cs => CON_TUPLE_INJECT(map (eval_con env) cs)
	  | CON_TUPLE_PROJECT(i,c) => (case (eval_con env c) of
					   CON_TUPLE_INJECT cs => List.nth(cs,i)
					 | _ => error "Cannot project out from non CON_TUPLE_INJ")
	  | CON_MODULE_PROJECT (m,l) => (case (eval_mod env m) of
					     MOD_STRUCTURE sbnds => eval_con env (con_str_lookup sbnds l)
					   | _ => error "cannot mod_project from non MOD_STRUCT")
	  | CON_FUN _ => con
	  | CON_SUM (iopt,cs) => CON_SUM(iopt,map (eval_con env) cs)
	  | CON_MUPROJECT(i,c) => con)

    and reduce_con env (con : con) : con = 
	(case con of
	    (CON_INT _ | CON_UINT _ | CON_FLOAT _ | CON_ANY) => con
	  | (CON_VAR v) => (con_lookup env v
			    handle (NOTFOUND _) => con)
	  | (CON_TYVAR tv) => (case (tyvar_deref tv) of
				  SOME c => reduce_con env c
				| NONE => (print "encountered unresolved "; Ppil.pp_con con;
					   print " during reduce_con";
					   error "encountered unresolved CON_TYVAR during reduce_con"))
	  | (CON_OVAR oc) => reduce_con env (CON_TYVAR (ocon_deref oc))
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
		      | (CON_FUN([v],body),_) => reduce_con env (con_subst_convar(body,[(v,c2')]))
		      | (CON_FUN(vars,body),CON_TUPLE_INJECT (args)) => 
							       reduce_con env (con_subst_convar(body,
											   zip vars args))
		      | _ => CON_APP(c1',c2'))
		end
	  | CON_RECORD rdecs => CON_RECORD (map (fn (l,c) => (l,reduce_con env c)) rdecs)
	  | CON_FLEXRECORD (ref(FLEXINFO(_,true,rdecs))) => CON_RECORD (map (fn (l,c) => (l,reduce_con env c)) rdecs)
	  | CON_FLEXRECORD (ref(FLEXINFO(_,false,_))) => error "should not evaluate unresolved flex record type"
          | CON_FLEXRECORD (ref(INDIRECT_FLEXINFO rf)) => reduce_con env (CON_FLEXRECORD rf)
	  | CON_TUPLE_INJECT cs => CON_TUPLE_INJECT(map (reduce_con env) cs)
	  | CON_TUPLE_PROJECT(i,c) => (case (reduce_con env c) of
					   CON_TUPLE_INJECT cs => List.nth(cs,i)
					 | c' => CON_TUPLE_PROJECT(i,c'))
	  | CON_MODULE_PROJECT (m,l) => (case (reduce_mod env m) of
					     MOD_STRUCTURE sbnds => reduce_con env (con_str_lookup sbnds l)
					   | m' => CON_MODULE_PROJECT(m',l))
	  | CON_FUN _ => con
	  | CON_SUM (iopt,cs) => CON_SUM(iopt,map (reduce_con env) cs)
	  | CON_MUPROJECT(i,c) => con)

    and eval_prim ((prim,cons),arg) = 
	let val vals = (case arg of
			    RECORD rbnds => map (fn (_,v) => v) rbnds
			  | _ => [arg])
	in PrimUtil.apply prim cons vals
	end

    and eval_exp env (exp : exp) : exp = 
	let 
	    val show = (!debug andalso (not (exp_isval exp)))
	    val _ = if show
			then (print (Int.toString(!indent)); 
			      print " eval_exp got: "; Ppil.pp_exp exp; print "\n"; 
			      indent := (!indent + 1))
		    else ()
	    val res = eval_exp' env exp 
	    handle exn_packet p => raise (exn_packet p)
		 | e => (print "\n\nError while eval_exp doing: "; Ppil.pp_exp exp; print "\n"; raise e)
	    val _ = if (show)
			then (print (Int.toString (!indent));
			      print "   returning: "; Ppil.pp_exp res; print "\n"; 
			      indent := (!indent - 1))
		    else ()
	in res
	end

    and eval_mod env (module : mod) : mod =
	let 
	    val _ =  if (!debug)
			 then (print (Int.toString (!indent)); print " eval_mod got: "; 
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
	    (FIX (a,fbnds,v)) => let fun help (FBND(v1,v2,c1,c2,e)) = FBND(v1,v2,reduce_con env c1,
									 reduce_con env c2, 
									 reduce_exp env e)
			       in FIX(a,map help fbnds, v)
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
	  | RAISE (c,e) => RAISE(reduce_con env c, reduce_exp env e)
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
	   | (SCON _ | PRIM _ | TAG _ ) => exp
	   | (FIX (a,fbnds,v)) => let fun help (FBND(v1,v2,c1,c2,e)) = 
		                             FBND(v1,v2,
						  reduce_con env c1,
						  reduce_con env c2, 
						  (case (subst_var(BND_EXP(fresh_var(),reduce_exp env e),env)) of
						       BND_EXP(v,e) => e
						     | _ => error "subst_var got exp, returned non-exp"))
			       in FIX(a,map help fbnds, v)
			       end
	   | (VAR v) => eval_exp env (exp_lookup env v)
	   | (APP (e1,e2)) => let val e1' = eval_exp env e1
				  val e2' = eval_exp env e2
			      in (case e1' of
				      FIX (a,fbnds,tarv) => 
					  let fun loop [] = error "ill-formed FIX"
						| loop ((FBND(v,argv,argc,resc,body))::rest) = 
					      if (eq_var(v,tarv)) then (argv,body) else loop rest
					      val (argv,body) = loop fbnds
					      val table = map (fn (FBND(v,_,_,_,_)) => (v,FIX(a,fbnds,v))) fbnds
					      val body' = exp_subst_expvar(body,(argv,e2')::table)
					  in eval_exp env body'
					  end
				    | PRIM prim_cons => eval_prim(prim_cons,e2')
				    | _ => error_exp exp "APP applying a non-(FIX or PRIM)")
			      end
	   | (RECORD rbnds) => RECORD(map (fn (l,e) => (l,eval_exp env e)) rbnds)
	   | (RECORD_PROJECT (e,l,c)) => let fun loop [] = error "rec projection did not find label"
					       | loop ((ll,v)::rest) = if (eq_label(l,ll)) 
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
	   | RAISE (c,e) => raise (exn_packet e)
	   | LET ([],body) => eval_exp env body
	   | LET (bnd::bnds,body) => let val env' = env_bndextend(env,eval_bnd env bnd)
				     in eval_exp env' (LET(bnds,body))
				     end
	   | NEW_STAMP c => TAG(fresh_tag(),eval_con env c)
	   | EXN_INJECT (e1,e2) => EXN_INJECT(eval_exp env e1, eval_exp env e2)
	   | ROLL(c,e) => ROLL(eval_con env c, eval_exp env e)
	   | UNROLL(c,e) => let val c' = eval_con env c
				val e' = eval_exp env e
			    in case e' of
				ROLL(c'',e'') => if (eq_con'(empty_context,c',c'')) then e'' 
						 else error_exp (UNROLL(c',ROLL(c'',e''))) 
						     "UNROLL(ROLL(e)) bad"
			      | _ => error_exp e' "UNROLL did not get a ROLL arg"
			    end
	   | INJ (cs,i,e) => INJ(map (eval_con env) cs, i, eval_exp env e)
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
(*	   | SEQ(elist) => foldl (fn (e,cur) => eval_exp env e) (SCON (INT 0w0)) elist *)
	   | SEAL (e,c) => eval_exp env e)


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
	   | BND_MOD(v,m) => BND_MOD(v,reduce_mod env m))

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
