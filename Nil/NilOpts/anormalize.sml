(* 

Routine to A-normalize Nil code. The alogorithm is taken from
Flanagan, Sabry, Duba, Felleisen "The Essence of Compiling with
Continuations", and extended to handle the NIL. 

*)

signature ANORMALIZE =
    sig 
	structure Nil :NIL
	val Anormalize : Nil.module -> Nil.module
    end


functor Anormalize (structure Ppnil : PPNIL
                    (* structure Prim : PRIM *)
		    structure Nil : NIL
		    structure NilUtil : NILUTIL
		    structure Subst : NILSUBST
		    (* structure TypeCheck : TYPECHECK -- Needed to create a Let_e  *) 
		    structure NilEval: NILEVAL 
		    structure NilStatic : NILSTATIC 
		    structure NilContext : NILCONTEXT 
		    structure PrimUtil : PRIMUTIL
		    structure Squish : SQUISH 
		    sharing  Ppnil.Nil = Nil = NilStatic.Nil = NilEval.Nil = NilContext.Nil = NilUtil.Nil = Squish.Nil
		    sharing  PrimUtil.Prim = Nil.Prim
		    sharing type PrimUtil.con = Nil.con = Subst.con
		    sharing type NilContext.context = NilStatic.context
			) : ANORMALIZE =

struct 

    structure Nil = Nil

    open Nil Name Util Nil.Prim

    exception UNIMP
    exception BUG
    
(*    type context = NilContext.context *)
    val empty = NilContext.empty
(*    val insert_con = NilContext.insert_con 
    val insert_con_list = NilContext.insert_con_list *)
    val find_con = NilContext.find_con
    val remove_con = NilContext.remove_con
(*    val insert_kind = NilContext.insert_kind
    val insert_kind_list = NilContext.insert_kind_list*)
    val find_kind = NilContext.find_kind
    val remove_kind = NilContext.remove_kind

    fun insert_con (env, var, con) = 
	let  (* val newcon = NilStatic.con_reduce(env, con) *)
	(*     val _ = ( Ppnil.pp_con con ; print " reduces to " )
	    val _ = (Ppnil.pp_con newcon ; print "\n" ) *)
	in NilContext.insert_con (env, var, con )
	end
    
    fun insert_con_list  (C, defs) = 
	List.foldl (fn ((v,c),C) => insert_con (C,v,c)) C defs

    fun insert_kind (env, var, kind) = 
	let (* val kind = NilStatic.kind_reduce (env, kind) *)
	in 
	    NilContext.insert_kind (env, var, kind )
	end 
    fun insert_kind_list (C, defs) = 
	List.foldl (fn ((v,c),C) => insert_kind (C,v,c)) C defs
	
    fun extend_args  newenv (Function (eff, recurs, vklist, vclist, vlist, bocy, con)) = 
	   let val newenv = insert_kind_list (newenv, vklist)
	       val newenv = insert_con_list (newenv, vclist) 
	       val newenv = insert_con_list (newenv, map (fn v=> (v, Prim_c (Float_c Prim.F32, [])))  vlist) 
	   in newenv end 

    fun extend_functions env fcnlist = 
	foldl ( fn ((var, Function(e,r,tformals,formals,fformals,body,return)),env) =>
	       let val num_floats = Word32.fromInt (List.length fformals)
		   val con = AllArrow_c(Open, e, tformals, #2 (Listops.unzip formals),num_floats,return)
	       in 
		   insert_con (env, var, con)
	       end ) env fcnlist
	
    fun extend_bnds env bnds  =
	foldl (fn (bnd, env) =>
	       case bnd of 
		   Con_b(v,k, c) => insert_kind(env, v, k)
		 | Exp_b (v,c,e) => insert_con (env, v, c) 
		 | Fixopen_b vfset =>
		       let val fcnlist = Util.set2list vfset
		       in extend_functions env fcnlist
		       end
		 | _ => raise UNIMP) env bnds


    fun typecheck (env, exp) =
	let val ( exp, con) = NilStatic.exp_valid (env, exp) 
	in 
	    con
	end 



    
    fun nil_prim_type (env, prim, cons, exps) = 
	( case (prim, cons, exps) of
	      (record labels,cons,exps) =>
		  Prim_c (Record_c labels,cons)
	    | (select label,_,[exp]) =>
		  let val Prim_c (Record_c labels, cons) = exp_type (env, exp)
		      val SOME con =  Listops.find2 (fn (l,c) => eq_label (l,label)) (labels,cons)
		  in #2 con
		  end 
	    | (inject {tagcount,field},cons,exps as ([] | [_])) => 
		  Prim_c (Sum_c {tagcount=tagcount,known=NONE},cons)
	    | (inject_record {tagcount,field},argcons,argexps) => 
		  Prim_c (Sum_c {tagcount=tagcount,known=NONE},argcons)
	    | (project_sum {tagcount,sumtype=field},argcons,[argexp]) =>
		  List.nth (argcons,Word32.toInt (field - tagcount))
	    | (project_sum_record {tagcount,sumtype,field},argcons,[argexp]) => 
		  let val  con_i = List.nth (argcons,Word32.toInt (sumtype - tagcount))
		      val SOME (labels,cons) = NilUtil.strip_record con_i
		  in  List.nth (cons,Word32.toInt field)
		  end 
	    | (box_float floatsize,[],[exp]) => 
	      Prim_c (BoxFloat_c floatsize,[])
	    | (unbox_float floatsize,[],[exp]) => 
		  Prim_c (Float_c floatsize,[])
	    | (roll,[argcon],[exp]) => argcon      
	    | (unroll,[argcon],[exp]) =>
		  let val  SOME (set,var) =  NilUtil.strip_recursive argcon 
		      val def_list = set2list set
		      val (_,con') = valOf (List.find (fn (v,c) => eq_var (v,var)) def_list)
		      val cmap = Subst.fromList (map (fn (v,c) => (v,Mu_c (set,v))) def_list)
		      val con' = Subst.substConInCon cmap con'
		  in con'
		  end       

	    | (make_exntag,[argcon],[]) => Prim_c (Exntag_c,[argcon])
	    | (inj_exn name,[],[exp1,exp2]) => Prim_c (Exn_c,[])
(* 	    | (make_vararg (openness,effect),cons,exps) =>
		  (error "make_vararg unimplemented....punting" handle e => raise e)
	    | (make_onearg (openness,effect),cons,exps) =>  
		  (error "make_onearg unimplemented....punting" handle e => raise e)
	    | (peq,cons,exps) => 
		  (error "Polymorphic equality should not appear at this level" handle e => raise e) ) *)
	  )
     and value_type (env, value) = 
	 case value of 
	     (int (intsize,word) |
		uint (intsize,word)) => 
		Prim_c (Int_c intsize,[])
	      | float (floatsize,string) => Prim_c (Float_c floatsize,[])
	      | array (con,arr) => Prim_c (Array_c,[con])
	      | vector (con,vec) => Prim_c (Vector_c,[con])
	      | refcell expref =>
		    let val con = exp_type(env, !expref) 
		    in Prim_c (Ref_c,[con]) end 
	      | tag (atag,con) =>
		    Prim_c (Exntag_c,[con])

    and exp_type (env, exp:exp) = 
 	 case exp of 
	     Var_e v => ( case find_con (env, v) of 
			 SOME c => c 
		       | NONE => raise BUG)
	   | Const_e v => value_type (env, v)
	   | Let_e (sort, bnds, exp ) => 
		  exp_type( (extend_bnds env bnds), exp)
	   | Prim_e ( NilPrimOp prim, clist, elist) =>
		 nil_prim_type (env, prim, clist, elist)
	   | Prim_e ( PrimOp prim, cons,   exps) =>
		 let val (total,arg_types,return_type) = PrimUtil.get_type prim cons
		 in return_type end 
		     
	   | Switch_e sw => switch_type (env,sw)
	   | App_e (_, Var_e f, cons, _, _) =>
		 let val SOME con =  find_con(env, f)
		     (* val _ = ( print "%%%%%%%%%%%%%%%%%%%%%%%%" ; Ppnil.pp_con con ; "\n" ) *)
		     val (AllArrow_c(_,_,vklist,_,_,con)) = NilStatic.con_reduce(env, con)
		     val subst = Subst.fromList (Listops.zip (#1 (Listops.unzip vklist)) cons)
		 in Subst.substConInCon subst con
		 end 
	     
     and switch_type (env, sw) =
	 case sw of 
	     Intsw_e {info=intsize,arg,arms,default} =>
		 let val default_con = Util.mapopt (curry2 exp_type env) default
		 in 
		     (case (default_con,arms)
			  of (SOME con,_) => con
			| (NONE,(_,Function (_,_,_,_,_,_,con))::_) => con
			)
		 end
	   | Sumsw_e {info=(non_val,val_cons),arg,arms,default} => 
		 let val default_con = Util.mapopt (curry2 exp_type env) default
		 in 
		     (case (default_con,arms)
			  of (SOME con,_) => con
			| (NONE,(_,Function (_,_,_,_,_,_,con))::_) => con
			)
		 end
	   | Exncase_e {info=_,arg,arms,default} =>
		 let val default_con = Util.mapopt (curry2 exp_type env) default
		 in
		     (case (default_con,arms)
			  of (SOME con,_) => con
			| (NONE,(_,Function (_,_,_,_,_,_,con))::_) => con
			)
		 end 
	   | Typecase_e {info,arg=argcon,arms,default} =>
		 let val default_con = Util.mapopt (curry2 exp_type env) default
		 in 
		     (case (default_con,arms)
			  of (SOME con,_) => con
			| (NONE,(_,Function (_,_,_,_,_,_,con))::_) => con
			)
		 end 


	
     fun normalize_exp exp env (k: (exp*NilContext.context) -> exp) = 
	 ( (* p rint "normalize_exp called on : " ; Ppnil.pp_exp exp; print "\n"; *)
	case exp of
	    Var_e v => k (exp, env)
	  | Const_e c => k (exp, env)
	  | Let_e ( sort, bnds , body ) => 
		let val (bnds', newenv) = normalize_bnds  bnds env
		    val exp = normalize_exp body newenv k
		    in
			Let_e( sort, bnds', exp)
		end 
	  | Prim_e (allp, cons, exps) => 
		normalize_names exps env (fn (exps', env) => (k (Prim_e(allp, cons, exps'), env)))
	  | App_e (openness, f, cons, args, exps) =>
		(normalize_name f env ( fn (t, env)  => 			      
				      normalize_names args env 
				       ( fn (args', env) => 
				       normalize_names exps env 
				       (fn (exps', env) =>
					(name (App_e(openness, t, cons, args', exps')) env k)))))
	  | Switch_e sw  => (normalize_switch sw env k)  
	  | Raise_e(e,c) => normalize_name e env (fn (e', env) => (k (Raise_e (e',c), env) ))
	  | Handle_e (e,function as Function(eff,r,vks,vcs,vc,exp,c)) =>
		let val newenv = extend_args env function
		    val exp = normalize_exp exp newenv (fn (x, env) => x)
		in 
		    (k (Handle_e (normalize_exp e env (fn (x, env) => x) ,Function(eff,r,vks,vcs,vc,exp, c)), env ))
		end )

     and name exp env k = 
	 let val t:var = (Name.fresh_var())
	   (*  val _ = ( Ppnil.pp_var t ; print " binds" ; Ppnil.pp_exp exp ; print " \n" )  *)
	     val con = exp_type (env, exp)
		 (* echeck exp env *) 
	 in 
	     Let_e(Sequential, Exp_b( t, con, exp) :: [] , (k ((Var_e t), insert_con (env, t, con)) ))
	 end 
    and normalize_name exp env k = 
	(
	 normalize_exp exp env (fn (exp', env) => 			       
			   (case exp' of
				(Const_e _ | Var_e _) => k (exp', env)
			      | _  => name exp' env k)))

    and normalize_names ((hd :: exps) :exp list) env k = 
	normalize_name hd env (fn (t, env) => normalize_names exps env  (fn (exps', env) => k ( t :: exps', env ) ))
      | normalize_names [] env k = k ([],env)
	
    and normalize_bnds (hd :: bnds) env   = 
	normalize_bnd hd env (fn (t, newenv) =>( let val (rest, newenv) =  normalize_bnds bnds newenv
						 in (t @ rest, newenv) end ) )
      | normalize_bnds [] env  =  ([], env) 
	
     (* Returns a list of normalized bnds *) 
    and normalize_bnd bnd env k = 
	case bnd of 
	    Con_b (v, knd, c) => k ([ bnd ], insert_kind (env, v, knd))
	  | Exp_b(var, con, exp) => let val exp' = normalize_exp exp env (fn (x, env) => x)
				    in 
					case exp' of 
					    Let_e ( sort, bnds, bdy) =>
						let val Let_e(sort, bnds, bdy) = Squish.squish exp'
						    val env = extend_bnds env bnds
						in 
						    k ( bnds @ [ Exp_b  (var, con, bdy) ], insert_con (env, var, con))
						end 
					  (* Possible x^2 behavior with @ ? *)
					  | _ => k ( [ Exp_b(var, con, exp') ], insert_con (env, var, con))
				    end 
	  | Fixopen_b fcnset =>
		let val fcnlist = Util.set2list fcnset
		    (* So that types of mutually recursive functions are available *)
		    val env = extend_functions env fcnlist  
		in
		    k ( [ Fixopen_b( Util.list2set
			      (map (fn (v, (function as Function(e,r,vklist, vclist, vlist, exp, con))) =>
				    let val newenv = extend_args env function
					val exp = normalize_exp exp newenv (fn (x, env)=>x)
				    in
					(v,Function(e,r,vklist, vclist, vlist, exp, con))
				    end )
			       fcnlist)) ], env ) 
		end
	  | Fixcode_b _ => raise UNIMP
	  | Fixclosure_b _ => raise UNIMP
		
     and normalize_switch sw env k =
	 let fun id x = x
	     val do_sw = fn (do_t, { info, arg, arms, default}, env)  => 
	     { info = info, 
	      arms = map (fn ( x, (function as Function(e,r,vklist, vclist, vlist, exp, con))) =>
			  let val newenv = extend_args env function
			      val exp = normalize_exp exp newenv (fn (x, env)=>x)
			  in 
			      ( x, Function(e,r,vklist, vclist, vlist, exp, con))
			  end) arms,
	      arg = arg,
	      default = case default of 
	      SOME exp => SOME (normalize_exp exp env (fn (x, env) => x))
	    | NONE => NONE }
	     fun do_arg do_t { info, arg, arms, default } func =
		 normalize_name arg env 
		 ( fn (t, env) =>
		  k ( Switch_e (func (do_sw (do_t, {info=info, arg=t, arms=arms, default=default}, env))), env) )
		 
	 in 
	      case sw of
		 Intsw_e sw =>  do_arg id sw Intsw_e
	       | Sumsw_e sw => do_arg id sw Sumsw_e
	       | Exncase_e sw => do_arg (fn exp => normalize_exp exp env (fn(x, env) => x)) sw Exncase_e
	       | Typecase_e sw => k (Switch_e (Typecase_e (do_sw(id, sw, env))), env)
	 end

     fun Anormalize ( MODULE{bnds = bnds, imports=imports, exports = exports}) =
	 let val baseEnvFn = (fn env => 
			( foldr ( fn (entry, env) => 
			    case entry of 
				ImportValue (l, v, c) => NilContext.insert_label (insert_con (env, v, c), l, v)
			      | ImportType (l , v, k) => NilContext.insert_label (insert_kind (env, v, k), l, v)
			 env imports  ))
	     val env = (baseEnvFn (empty()))
	     val (bnds, newenv) =  normalize_bnds bnds env
	 in 
	     MODULE { bnds = bnds,
		     imports = imports,
		     exports =
		     let val env = extend_bnds env bnds
		     in  
			 map ( fn entry =>
			      case entry of 
				  ExportValue(lab, exp, con) => 
				      let val exp  = normalize_exp exp newenv (fn (x, c)=> x)
				      in 
					  ExportValue(lab, exp, con)
				      end
				| _=> entry ) exports
		     end   
		     } 
	 end 
	
end


