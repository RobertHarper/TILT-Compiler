(* This inline strategy follows the algorithm described in Tarditi's thesis 
   and implemented in the first version of TIL 

   It inlines functions which are not recursive and smaller than some constant c

   Phase 1 is done in reduce - identify recursive functions
   
   Phase 2 (analysis and transformation) - Rewrite the program from top to bottom.
   At every function binding:
       1. Rewrite the body of function so that inlining is done
       2. Calculate the size of the function
       3. If it is smaller than c and not recursive, add it to map
       4. Rewrite body of let
       
  --------------------------------------------------------------
  
  A difference between this implementation and mine is that I don't
  count the sizes of the constructors in computing the sizes of the
  expressions.

  Also, since I don't want      
  let val x = ..
  in let val y = ...
      in body
	  
  to be different than :

  let val x = ...
      val y = ..
  in body

  I don't count the lets in computing the size.....

  This could be combined with the reduce pass, but I don't know if
  that's a good idea.

 *)


functor Inline  ( structure Nil : NIL
		  structure Ppnil : PPNIL 
		  structure Squish : SQUISH
		  sharing Nil = Ppnil.Nil = Squish.Nil
		      ) : PASS = 
struct

    open Nil Name Util
    structure Nil = Nil

    exception FnNotFound
    exception BUG
    val error = fn s => Util.error "inline" s	
    
    val float_con = Prim_c (Float_c Prim.F32, [])
    val inline_click = NilOpts.make_click "Functions inlined: (general fcns)"
    val inc_click  = NilOpts.inc_click

    val max_size = 100  (* This should be part of our main fcn... *)


	(* I'm thinking that maybe we don't need this...  for one
	 recursive function, it will only get bound after the body is
	 inlined.  For multiple recursive functions, there will always
	 be a bottom. The effect will be to unroll recursive fcns one
	 step *)
	 
    val recset = ref VarSet.empty  (* Import this from reduce or find it again? *)

    val S =  Name.mk_var_hash_table(200,FnNotFound) : (var, (function * int )) HashTable.hash_table
    val SC = Name.mk_var_hash_table(200,FnNotFound) : (var, ( (var*kind) list * con * int )) HashTable.hash_table

    fun xfunc ( fnVar,   Function(eff, a, vklist, vclist, vlist, body, con )) = 
	let val (body, size) = xexp body
	    val func  = Function(eff, a, vklist, vclist, vlist, body, con )
	    val _ = 
		if (size < max_size) (*  andalso not (VarSet.member ( !recset, fnVar))  *) 
		    then  HashTable.insert S (fnVar, (func, size))
		else  ()
	in 
	    ( (fnVar, func), size ) 
	end
    
    and xbnd bnd = 
	case bnd of 
	    Con_b ( v, c, k) => (bnd, 0) 
	  | Exp_b ( v, c, exp) => 
		let val _ = ( case exp of App_e app => print "ARRRRRGH\n"
	                      | _ => ())
		    val (exp, size) = xexp exp 
		in
		    (Exp_b (v,c,exp ), size)
		end
	  | Fixopen_b vfset => 
		let val vflist = Util.set2list vfset		
		    val (vflist, sizes)  = Listops.unzip (map xfunc vflist)
		in 
		    ( Fixopen_b (Util.list2set vflist), foldl op+ 0 sizes )
		end 
	  | _ => raise BUG 
		
    (* Inline the function at the call site, alpha-converting as we go *)
    and doApp (openness, Var_e f, tactuals, actuals, elist2) =  
	let val r = HashTable.find S f
	in 
	    ( case r of 
		  SOME (Function(eff, r, vks, vcs, vs, body, con), size ) =>
		      let val _ = inc_click inline_click
			  val conbnds = Listops.map2 ( fn ((var, kind), con) =>  Con_b ( var, kind, con)) (vks, tactuals)
			  val expbnds1 = Listops.map2 (fn ((var, con), exp) => Exp_b (var, con,exp)) (vcs, actuals)
			  val expbnds2 = Listops.map2 (fn (var, exp) => Exp_b (var, float_con, exp) ) (vs, elist2)
			  val (extra, body) = case body of 
			      Let_e (_, bnds, body) => (bnds, body)
			    | _ => ([], body)
			  val bnds = conbnds @ expbnds1 @ expbnds2 @ extra
			
		      (* val Let_e (Sequential, bnds, newexp) = ALPHA_CONVERT newexp *)
		      in
			  ( bnds, body, size )
		      end
		| NONE =>  ( [], App_e (openness, Var_e f , tactuals, actuals, elist2), length actuals + length elist2))
	end 
    
    and xexp exp = 
	case exp of 
	    Var_e v => (exp, 1)
	  | Const_e c => (exp, 1)
	  | Prim_e (allp, cons, exps) =>
		( Prim_e (allp, cons, exps), length exps + 1)
	  | Switch_e sw => 
		(xswitch sw)

	  (* Change function call to new arguments *) 
	  | (App_e app) => 
		let val (bnds, app, size) = doApp app
		in 
		    if null bnds then (app, size)
		    else 
			(Let_e (Sequential, bnds, app), size)
		end 
	  | Let_e (sort, (Exp_b (v, con, App_e app) :: rest ), exp) =>
		let 
		    val (bnds, app, appsize) = doApp app
		    val (body, bodysize) =  xexp (Let_e (sort, rest, exp))
		in 
		    (Let_e  (sort, bnds @ [ Exp_b (v, con, app)], body), appsize + bodysize)
		end  
	  | Let_e (sort, (bnd :: bnds), exp ) =>
		let val (bnd, bndsize) = xbnd bnd
		    val (body, bodysize) = xexp (Let_e (sort, bnds, exp ))
		in 
		    (Let_e (sort, [bnd], body), bndsize + bodysize)
		end 
	  | Let_e (sort, [], exp ) =>
		xexp exp
		
	  | Raise_e (exp, con) => 
		let val (exp, size) = xexp exp
		in ( Raise_e (exp, con), size +1 ) end
	  | Handle_e (exp, Function(eff, r, vks, vcs, vs, expf, con) ) =>
		let val (exp, expsize) = xexp exp
		    val (expf, expfsize) = xexp expf
		in 
		    ( Handle_e (exp, Function(eff, r, vks, vcs, vs, expf, con)),
		     expsize + expfsize + 1) 
		end
		

    and xswitch  s =
	let fun id x = (x, 0)
	    fun  do_sw constr do_t {arms,default,arg,info } =
		let val (newarms, sizes) = Listops.unzip 
		    ( map (fn (t,Function(a,b,c,d,e,exp,h)) => 
			   let val (exp, size ) = xexp exp
			       val (t, tsz) = do_t t
			   in 
			       ((t, Function(a,b,c,d,e,exp,h)), size+tsz)
			   end ) arms)
		    val (newdefault, defaultsize) = case default of 
			SOME exp =>
			    let val (exp, size) = xexp exp
			    in 
				(SOME exp, size) 
			    end
		      | NONE => (NONE, 0) 
		in ( Switch_e (constr {arms = newarms, default = newdefault, arg = arg, info= info}), 
		    (foldl op+ 0 sizes) + defaultsize + 1)
		end
	in
	    case s of
		Intsw_e sw =>  (do_sw Intsw_e  id sw)
	      | Sumsw_e sw =>  (do_sw Sumsw_e id sw)
	      | Exncase_e sw => (do_sw Exncase_e xexp sw) 
	      | Typecase_e sw => (do_sw Typecase_e id sw)
	end
    
	fun doModule  debug (MODULE{bnds=bnds, imports=imports, exports=exports}) = 
	    let 
		val _ = HashTable.appi ( fn (key, item) => ignore (HashTable.remove S key)) S
		val dummy = Const_e(Prim.int(Prim.W32,TilWord64.fromInt 0))	 
		val temp =  Let_e(Sequential,bnds, dummy)
		val (exp, size) = (xexp temp)
		val _ = if debug then print ( "Size is " ^ (Int.toString size) ^ "\n") else ()
		val bnds = (case (Squish.squish exp) of
			    Let_e(Sequential,bnds,_) => bnds
			  | Const_e(Prim.int _) => []
			  | _ => error "inline got non-let")
	    in
		MODULE {bnds=bnds, imports=imports, exports=exports}
	    end
	
end
