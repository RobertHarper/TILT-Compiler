(* Unfortunately normalization of constructors pretty much ruins this *)



functor FlattenArgs (structure Nil : NIL
		     structure Ppnil : PPNIL
		     structure Subst : NILSUBST
		     structure Squish : SQUISH 
		       
		     sharing Ppnil.Nil = Nil = Squish.Nil
		     sharing type Nil.con = Subst.con
			 ) : PASS = 
struct
    structure Nil = Nil
    open Nil Name 

    exception FnNotFound
    exception BUG
    exception UNIMP
	
    val maxFnArgs = 6  (* For flattening record arguments to functions. Should be close to the 
			number of machine registers or something *)
	 
    (* Which functions have had their first record argument flattened *)
    val flattened = Name.mk_var_hash_table(200, FnNotFound)   :
	(var , (var * label list * var list * con list)) HashTable.hash_table 
	
    val flattenedc =  Name.mk_var_hash_table(200, FnNotFound)   :
	(var , (var * label list * kind * kind list)) HashTable.hash_table 

    val flatten_click = NilOpts.make_click "Flatten function args"
    val flatten_con_click = NilOpts.make_click "Flatten con fun args"
    (* If the function only takes a few arguments, (one of which is a
     record) and then we can pass the elements of the record as
     arguments to the function. This should interact nicely with known
     record projection *)

    fun extend_conbnds env bnds = 
	foldl (fn (bnd, env) => 
		   case bnd of 
		       Con_cb (v, k ,con) => VarMap.insert (env, v, con)
		     | _ => env) env bnds
    fun extend_bnds env bnds = 
	foldl (fn (bnd, env) => 
	       case bnd of 
		       Con_b (v, k ,con) => VarMap.insert (env, v, con)
		     | _ => env) env bnds
	
(*    fun record_type env (con:con) =
	case con of 
	    Prim_c (Record_c labels, cons) =>
		let val subst = VarMap.foldli (fn (var, con, subst) => Subst.add subst (var, con)) (Subst.empty()) env
		    val cons = map (Subst.substConInCon subst) cons
		in 
		    SOME (labels, cons)
		end 
	  | Var_c v => let val SOME con = (VarMap.find (env, v)) in record_type env con end 
	  | Let_c ( sort, bnds, con) => record_type (extend_conbnds env bnds) con
	  | Annotate_c (a, con) => record_type env con
	  | _ => NONE
		
	*)	

    fun flatten_cfunc env ( fnVar, [ (recordArg:var, recordKind as (Record_k lvkseq))], body, kind) = 
	let val (lvs, kinds) = Listops.unzip (Util.sequence2list lvkseq)
	    val (labels, vars) = Listops.unzip lvs
	in 
	    if ((length labels) <= maxFnArgs) then 
		let 
		    (* First rename the function and make it take more arguments *)
		    val _ = NilOpts.inc_click flatten_con_click 
		    val newFnVar = (Name.fresh_var())
		    val args = map (fn _ => (Name.fresh_var())) labels
		    val newvklist = Listops.zip args kinds 
		    val cons = map Var_c args
		    val _ = HashTable.insert flattenedc (fnVar, (newFnVar, labels, recordKind,  kinds))
			
		    val newbody = 
			Let_c (Sequential, 
			       [ Con_cb ( recordArg, recordKind,
					 Crecord_c (Listops.zip labels cons))],
			       body)
		    val newFn = (newFnVar, newvklist, xcon env newbody, kind)

		    (* Now a new version of fnVar which calls NewFnVar *)
		    val args = map (fn _ => (Name.fresh_var())) labels
		    val call = Name.fresh_var()
		    val newRecordArg = Name.fresh_var()
		    val newbnds = Listops.map3
			(fn (v, k, l) =>
			 Con_cb (v, k, (Proj_c( Var_c newRecordArg, l)))) 
			(args, kinds, labels)
		       
		    val func = 
			(fnVar, [(newRecordArg, recordKind)], 
			 Let_c (Sequential, newbnds @
				[ Con_cb (call, kind, App_c ((Var_c newFnVar), map Var_c args))],
				Var_c call), kind)
		in [Open_cb newFn, Open_cb func]
		end
	    else 
		[Open_cb( fnVar,  [(recordArg, recordKind)], xcon env body, kind)]
	end
    | flatten_cfunc env func = [ Open_cb func ] 

    and flatten_func env ( fnVar, Function(eff, a, vklist,
					  [ (recordArg, recordType) ],
					  vlist, body, con )) =
       (case recordType of 
	   Prim_c( Record_c (labels), cons) =>
	       (if ( (length vlist) + (length labels)  <= maxFnArgs ) 
	    then 
		let
		    (* First rename the function and make it take more arguments *)
		    val _ = NilOpts.inc_click flatten_click 
			
		    val newFnVar = (Name.fresh_var())
		    val args = map (fn _ => (Name.fresh_var())) labels
		    val newvclist = Listops.zip args cons
		    val exps = map Var_e args 
		    val _ = HashTable.insert flattened  (fnVar,(newFnVar, labels, (map #1 vklist), cons))
			
		    val newbody = 
			Let_e (Sequential, 
			       [Exp_b ( recordArg, 
				       recordType,
				       Prim_e (NilPrimOp (record labels), cons, exps))], 
			       body)
		    val newFn = (newFnVar, Function( eff, a, vklist, newvclist, vlist, xexp env newbody, con))
			
		    (* Now a new version of fnVar which calls NewFnVar *)
		    val args = map (fn _ => (Name.fresh_var())) labels
		    val newvlist = map (fn _ => Name.fresh_var() ) vlist
		    val newvklist = map (fn (_, k) => (Name.fresh_var(), k)) vklist
		    val call = Name.fresh_var()
		    val newRecordArg = Name.fresh_var()
			
		    val subst = Subst.fromList (Listops.zip (map #1 vklist) (map (Var_c o #1) newvklist))
		    val newbnds =  Listops.map3 (fn (v, c, l) => 
						 Exp_b (v,  Subst.substConInCon subst c, 
							Prim_e (NilPrimOp (select l), [], [Var_e newRecordArg])))
			(args, cons, labels) 
		    val newRecordType = Subst.substConInCon subst recordType
		    val newcon = Subst.substConInCon subst con
		    val func = 
			( fnVar, Function (eff, a, newvklist, [(newRecordArg, newRecordType)], newvlist, 
					   Let_e (Sequential, newbnds @
						  [ Exp_b ( call, newcon, App_e (Open, Var_e newFnVar, 
										 map (Var_c o  #1) newvklist, 
										 map Var_e args, map Var_e newvlist))], 
						  Var_e call), newcon))
			
			
			
		in 
		    [ newFn , func ] 		
		end 
	else  [ ( fnVar, Function(eff, a, vklist,
				  [ (recordArg, recordType) ], vlist, xexp env body, con)) ] )
	 | _ =>  [ ( fnVar, Function(eff, a, vklist,
					[ (recordArg, recordType) ], vlist, xexp env body, con)) ] )
     | flatten_func env ( fnVar, Function(eff, a, vklist, vclist, vlist, body, con )) =
       [ ( fnVar, Function(eff, a, vklist, vclist, vlist, xexp env body, con)) ]
    and xbnd env bnd = 
	case bnd of 
	    Con_b ( v, k, con) => Con_b (v, k, xcon env con)
	  | Exp_b ( v, c, exp) => 
		Exp_b (v,xcon env c, xexp env exp)
	  | Fixopen_b vfset => 
		let val vflist = Util.set2list vfset		
		    val vflist  = Listops.flatten (map (flatten_func env) vflist)
		in 
		    Fixopen_b (Util.list2set vflist)
		end 
	  | _ => raise BUG 

    and xconbnd env bnd = 
	case bnd of 
	 Con_cb ( v, k, con) => [Con_cb(v, k, xcon env con)]

       | Open_cb func => flatten_cfunc env func
       | _ => raise UNIMP

    and doApp_c (Var_c f , actual) = 
	let val r =  HashTable.find flattenedc f
	in 
	    ( case r of 
		  SOME (newName, labels, recordKind, kinds ) =>
		      let 
			  val args = map (fn _ => (Name.fresh_var())) labels
			  val newcons = map Var_c args
			  val (recvar, prebnd) = case actual of [Var_c v] => (Var_c v, [])
			      | [other] => let val t = Name.fresh_var()
					   in 
					       (Var_c t, [Con_cb(t, recordKind, other)])
					   end
			  val bnds = prebnd @ (Listops.map3 (fn (var, label, kind) =>
							     Con_cb (var, kind, Proj_c (recvar, label)))
					       (args, labels, kinds))
		      in 
			  (bnds, (Var_c newName, newcons))
		      end
		  | NONE => ( [], (Var_c f, actual)))
	end 
    | doApp_c app = ( [], app)

    and doApp ( openness, Var_e f, tactuals, (actual:exp list), elist2) = 
	let val r = HashTable.find flattened f
	in 
	    ( case r of 
		  SOME (newName, labels, tformals, cons ) =>
		      let val subst = Subst.fromList (Listops.zip tformals tactuals)
			  val newcons = map (Subst.substConInCon subst) cons
			  val args = map (fn _ => (Name.fresh_var())) labels
			      
			  val newexps = map Var_e args

			      (* If we are not in A-normal form, we don't want to duplicate this *)
			  val (recvar, prebnd)  = case actual of [Var_e v] => (actual, [])
			                        | [other] => let val t = Name.fresh_var() 
				     in 
					 ([Var_e t], [ Exp_b (t, Prim_c((Record_c labels), cons), other)] )
				     end 
			  val bnds =  prebnd @ (Listops.map3 (fn (var, label, con) =>
						    Exp_b (var, con, Prim_e (NilPrimOp (select label),
									     [(*Prim_c((Record_c labels), cons) *)], recvar)))) 
			      (args ,labels, newcons)
		      in 
			  (bnds, (openness, Var_e newName, tactuals, newexps, elist2))
		      end 
		| NONE =>  ( [], (openness, Var_e f , tactuals, actual, elist2)))
	end 
      | doApp app = ( [], app )
    and xexp env  exp = 
	case exp of 
	    Var_e v => exp
	  | Const_e c => exp
	  | Prim_e (allp, cons, exps) =>
		Prim_e (allp, map (xcon env) cons, map( xexp env) exps)
	  | Switch_e sw => 
		Switch_e (xswitch env sw)
	  (* Change function call to new arguments *) 
	  | (App_e app) => 
		let val (bnds, app) = doApp app
		in 
		    if null bnds  then App_e app
		    else 
			Let_e (Sequential, bnds, App_e app)
		end 
	  | Let_e (sort, (Exp_b (v, con, App_e app) :: rest ), exp) =>
		let val (bnds, app) = doApp app
		    val newbnds =  bnds @ [ Exp_b (v, con, App_e app)]
		in 
		    Let_e  (sort,newbnds, xexp (extend_bnds env newbnds) (Let_e (sort, rest, exp)))
		end 

	  | Let_e (sort, (Con_b (v, kind, App_c app) :: rest ), con) =>
		let val (bnds, app) = doApp_c app
		    val newbnds =  [ Con_b (v, kind, Let_c (sort, bnds, App_c app)) ]
		in 
		    Let_e  (sort,newbnds, xexp (extend_bnds env newbnds) (Let_e (sort, rest, con)))
		end 
 
	  | Let_e (sort, (bnd :: bnds), exp ) =>
		let val bnd' = xbnd env bnd
		in 
		    Let_e (sort, [bnd'],
			   xexp (extend_bnds env [bnd']) (Let_e (sort, bnds, exp )))
		end 
	  | Let_e (sort, [], exp ) =>
		xexp env exp
		
	  | Raise_e (exp, con) => Raise_e (xexp env exp, xcon env con)
	  | Handle_e (exp, f) =>
		Handle_e (xexp env exp, xfn env f)
		
    and xfn env (Function(eff, r, vks, vcs, vs, exp, con)) =
	Function (eff, r, vks, (map (fn (v,c) => (v, xcon env c)) vcs), vs, xexp env exp, xcon env con)

    and xcon env con = 
	case con of 
	    Var_c v => con
	  | Mu_c (bool, vcseq, v) => Mu_c (bool, Util.mapsequence (fn (v,c) => (v, xcon env c)) vcseq, v)
	  | AllArrow_c (ope, eff, vklist, clist, w32, con) =>
		AllArrow_c (ope, eff, vklist, map (xcon env) clist, w32, con)
	  | Prim_c (prim, cons) => Prim_c (prim, map (xcon env) cons)
	  | Typecase_c { arg , arms, default, kind } => 
		Typecase_c 
		{ arg = xcon env arg,
		 arms = map (fn (p, vklist, c) => (p , vklist, xcon env c)) arms,
		 default = xcon env default,
		 kind = kind }
	  | App_c app => 
		let val (bnds, app) = doApp_c app
		in 
		    if null bnds  then App_c app
		    else 
			Let_c (Sequential, bnds, App_c app)
		end 
	  | Let_c (sort, (Con_cb (v, kind, App_c app) :: rest ), con) =>
		let val (bnds, app) = doApp_c app
		    val newbnds =  bnds @ [ Con_cb (v, kind, App_c app)]
		in 
		    Let_c  (sort,newbnds, xcon (extend_conbnds env newbnds) (Let_c (sort, rest, con)))
		end 
	  | Let_c (sort, (bnd :: bnds), con) =>
		let val bnd' = xconbnd env bnd
		in 
		    Let_c (sort, bnd',
			   xcon (extend_conbnds env bnd') (Let_c (sort, bnds, con )))
		end 
	  | Let_c (sort, [], con ) =>
		xcon env con 
		
	  | Annotate_c (annot, con) => Annotate_c (annot, xcon env con)
	  | Closure_c _ => raise UNIMP
	  | Crecord_c lclist => Crecord_c (map (fn (l,c)=> (l, xcon env c)) lclist)
	  | Proj_c (con, label) => Proj_c ((xcon env con) , label)

		

    and xswitch env  s =
	let fun suminfo env (w32, cons) = 
	    (w32, map (xcon env) cons) 
	    fun id _ x = x
	    fun  do_sw  do_info do_arg do_t {arms,default,arg,info } =
		let val newarg = do_arg env arg
		    val newinfo = do_info env info
		    val newarms = map (fn (t,f) => 
				       (do_t env t, xfn env f)) arms
		    val newdefault = case default of 
			SOME exp => SOME (xexp env  exp)
		      | NONE => NONE 
		in {arms = newarms, default = newdefault, arg = newarg, info= newinfo}
		end
	in
	    case s of
		Intsw_e sw => Intsw_e (do_sw id xexp id sw)
	      | Sumsw_e sw => Sumsw_e (do_sw suminfo xexp id sw)
	      | Exncase_e sw => Exncase_e (do_sw id xexp xexp sw) 
	      | Typecase_e sw => Typecase_e (do_sw id xcon id sw)
	end
    
    
    fun reduceExport (ExportValue(l,e,c)) = ExportValue(l, xexp VarMap.empty e, c)
      | reduceExport (ExportType (l,c,k)) = ExportType(l, c, k)
	

	
    fun doModule debug (MODULE {bnds, imports, exports} ) = 
	let 
		
	    (* Clear out the hash table *)
	    val _ = HashTable.appi ( fn (key, item) => ignore (HashTable.remove  flattened key)) flattened
	    val temp =  Let_e(Sequential,bnds, Prim_e(NilPrimOp(inject {tagcount=0w2,sumtype=0w1}),[],[])  ) (* true! *)
	    val Let_e(Sequential,bnds,_) = Squish.squish (xexp VarMap.empty temp)
	    val imports = imports
	    val exports = map reduceExport exports
	in  MODULE{bnds = bnds,
		   imports = imports,
		   exports = exports}
	end
    
end


	    


