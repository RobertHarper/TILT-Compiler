(* Weird hack ... all App_e must be part of a  Let expression. 
 I've modified anormalize to account for this *)

functor FlattenArgs (structure Nil : NIL
		     structure Ppnil : PPNIL
		     structure Subst : NILSUBST
		     structure Squish : SQUISH 
		     
		     sharing Ppnil.Nil = Nil = Squish.Nil
		     sharing type Nil.con = Subst.con
			 ) = 
struct
    structure Nil = Nil
    open Nil Name 

    exception FnNotFound
    exception BUG
	
    val maxFnArgs = 6  (* For flattening record arguments to functions. Should be close to the 
			number of machine registers or something *)
	 
    (* Which functions have had their first record argument flattened *)
    val flattened = Name.mk_var_hash_table(200, FnNotFound)   :
	(var , (var * label list * var list * con list)) HashTable.hash_table 
	

	
    (* If the function only takes a few arguments, (one of which is a record) and
     then we can pass
     the elements of the record as arguments to the function. This should interact nicely with 
     known record projection *) 
	
    fun reduceModule (MODULE {bnds, imports, exports} ) = 
	let 
	    fun flatten_func ( fnVar, Function(eff, a, vklist,
					       [ (recordArg, recordType as Prim_c ( Record_c labels, cons )) ],
					       vlist, body, con )) =
		if ( (length vlist) + (length labels)  <= maxFnArgs ) 
		    then 
			let
			    (* First rename the function and make it take more arguments *)
			    (* val _ = inc_click flatten_click*) 

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
			    val newFn = (newFnVar, Function( eff, a, vklist, newvclist, vlist, xexp newbody, con))
			    
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
					  [ (recordArg, recordType) ], vlist, xexp body, con)) ]
	      | flatten_func ( fnVar, Function(eff, a, vklist, vclist, vlist, body, con )) =
		[ ( fnVar, Function(eff, a, vklist, vclist, vlist, xexp body, con)) ]
	    and xbnd bnd = 
		case bnd of 
		    Con_b ( v, c, k) => bnd
		  | Exp_b ( v, c, exp) => 
			Exp_b (v,c, xexp exp)
		  | Fixopen_b vfset => 
			let val vflist = Util.set2list vfset		
			    val vflist  = Listops.flatten (map flatten_func vflist)
			in 
			    Fixopen_b (Util.list2set vflist)
			end 
		  | _ => raise BUG 
			
	    and xexp exp = 
		case exp of 
		    Var_e v => exp
	      | Const_e c => exp
	      | Prim_e (allp, cons, exps) =>
		    Prim_e (allp, cons, map xexp exps)
	      | Switch_e sw => 
		    Switch_e (xswitch sw)
	      (* Change function call to new arguments *) 
	      | Let_e (sort, (Exp_b (v, con, App_e ( openness, Var_e f, tactuals, actual, elist2)) :: rest ), exp) =>
		(
		 let val r = HashTable.find flattened f
		 in 
		     ( case r of 
			   SOME (newName, labels, tformals, cons ) =>
			       let val subst = Subst.fromList (Listops.zip tformals tactuals)
				   val newcons = map (Subst.substConInCon subst) cons
				   val args = map (fn _ => (Name.fresh_var())) labels
				   
				   val newexps = map Var_e args
				   val bnds =  Listops.map3 (fn (var, label, con) =>
							     Exp_b (var, con, Prim_e (NilPrimOp (select label),
										      [(*Prim_c((Record_c labels), cons) *)], actual))) 
				       (args ,labels, newcons)
			       in 
				   Let_e  (sort, bnds @ [ Exp_b ( v, con, 
								 App_e (openness, Var_e newName, tactuals, newexps, elist2)) ],
					   xexp (Let_e (sort, rest, exp)))
			       end 
			 | NONE =>  Let_e  (sort, [ Exp_b ( v, con, 
							   App_e (openness, Var_e f , tactuals, actual, elist2)) ],
					    xexp (Let_e (sort, rest, exp))) )
		 end )
		      
	      | Let_e (sort, (bnd :: bnds), exp ) =>
		    let val bnd' = xbnd bnd
		    in 
		        Let_e (sort, [bnd'],
			       xexp (Let_e (sort, bnds, exp )))
		    end 
	      | Let_e (sort, [], exp ) =>
		    xexp exp

	      | Raise_e (exp, con) => Raise_e (xexp exp, con)
	      | Handle_e (exp, Function(eff, r, vks, vcs, vs, expf, con) ) =>
		    Handle_e (xexp exp, Function(eff, r, vks, vcs, vs, xexp expf, con))


	    and xswitch  s =
		let fun id x = x
		    fun  do_sw do_t {arms,default,arg,info } =
		    let val newarms = map (fn (t,Function(a,b,c,d,e,exp,h)) => 
					   (do_t t, Function(a,b,c,d,e,xexp exp,h))) arms
			val newdefault = case default of 
			    SOME exp => SOME (xexp  exp)
			  | NONE => NONE 
		    in {arms = newarms, default = newdefault, arg = arg, info= info}
		    end
		in
		    case s of
			Intsw_e sw => Intsw_e (do_sw id sw)
		      | Sumsw_e sw => Sumsw_e (do_sw id sw)
		      | Exncase_e sw => Exncase_e (do_sw xexp sw) 
		      | Typecase_e sw => Typecase_e (do_sw id sw)
		end


	    fun reduceExport (ExportValue(l,e,c)) = ExportValue(l, xexp e, c)
	      | reduceExport (ExportType (l,c,k)) = ExportType(l, c, k)
		
		(* Clear out the hash table *)
	    val _ = HashTable.appi ( fn (key, item) => ignore (HashTable.remove  flattened key)) flattened
	    val temp =  Let_e(Sequential,bnds, Prim_e(NilPrimOp(inject {tagcount=0w2,sumtype=0w1}),[],[])  ) (* true! *)
	    val Let_e(Sequential,bnds,_) = Squish.squish (xexp temp)
	    val imports = imports
	    val exports = map reduceExport exports
	in  MODULE{bnds = bnds,
		   imports = imports,
		   exports = exports}
	end
    
end


	    


