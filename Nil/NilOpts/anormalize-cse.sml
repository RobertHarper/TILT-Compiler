(* 

Routine to A-normalize Nil code. The alogorithm is taken from
Flanagan, Sabry, Duba, Felleisen "The Essence of Compiling with
Continuations", and extended to handle the NIL. 

Also does cse as it names each intermediate value
*)



functor Anormalize (
		    structure Ppnil : PPNIL
		    structure Nil : NIL
		    structure NilUtil : NILUTIL
		    structure Subst : NILSUBST
		    structure NilEval: NILEVAL 
		    structure NilStatic : NILSTATIC 
		    structure NilContext : NILCONTEXT 
		    structure PrimUtil : PRIMUTIL
		    structure Squish : SQUISH 
(* cse *)	    structure ExpTable : EXPTABLE 
		    sharing type Nil.exp = ExpTable.Nil.exp

		    sharing  Ppnil.Nil = Nil = NilStatic.Nil = NilEval.Nil = NilContext.Nil = NilUtil.Nil = Squish.Nil
		    sharing  PrimUtil.Prim = Nil.Prim
		    sharing type PrimUtil.con = Nil.con = Subst.con
		    sharing type NilContext.context = NilStatic.context
			) : PASS =
    
struct 

    structure Nil = Nil
    structure Expmap = ExpTable.Expmap 

    open Nil Name Util Nil.Prim

    exception FnNotFound
    exception UNIMP
    exception BUG

    val print_bind = ref false

    (* ----------- CSE aux fns ------------ *)
	
    val do_cse = NilOpts.do_cse
    val cse_click = (NilOpts.make_click "CSE")
    val normalize_click = (NilOpts.make_click "Normalization")
    val clicks = [ cse_click, normalize_click ]
    val inc_click = NilOpts.inc_click
    
    (*True if the function is total, not found or false o/w  *)
    val fntable = Name.mk_var_hash_table (200, FnNotFound):
	(var, bool) HashTable.hash_table
	
    fun is_elim_allp (NilPrimOp p) = true
      | is_elim_allp (PrimOp p) = 
	case p of
	    ( mk_ref | setref | open_in
	  | input | input1 | lookahead | open_out | close_in
	  | output | flush_out | close_out | end_of_stream ) => false 
	  | ( update _  | create_table _ ) => false
	  | ( deref | sub _ ) => false   (* ??? Keep all operations on the store *) 
	  | _ => true
	    


    fun is_elim_exp exp =
	case exp of 
	    Prim_e( allp, _, _) => is_elim_allp allp
	  | App_e ( openness, Var_e v, _, _, _) =>
		if HashTable.find fntable v = SOME true then true
		else false
	  | _ => false


    (* As we are in A-normal form, we don't need to recur
     on the arguments. Just replace exp if we can *)
    fun cse_exp exp ae D sigma =
	( if (is_elim_exp exp) then 
	      ( case Expmap.find (ae, exp) of 
		    SOME (var, tau) => 
			if NilStatic.con_equiv (D, sigma, tau) then 
			    ( inc_click cse_click ; Var_e var )
			else exp
		  | NONE => exp)
	  else exp)

    (* ------------------- Nil Typechecking stuff ----------------- *)
    
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

    fun insert_con (D, var, con) = 
	let  (* val newcon = NilStatic.con_reduce(D, con) *)
	(*     val _ = ( Ppnil.pp_con con ; print " reduces to " )
	    val _ = (Ppnil.pp_con newcon ; print "\n" ) *)
	in NilContext.insert_con (D, var, con )
	end
    
    fun insert_con_list  (C, defs) = 
	List.foldl (fn ((v,c),C) => insert_con (C,v,c)) C defs

    fun insert_kind (D, var, kind) = 
	let (* val kind = NilStatic.kind_reduce (D, kind) *)
	in 
	    NilContext.insert_kind (D, var, kind )
	end 
    fun insert_kind_list (C, defs) = 
	List.foldl (fn ((v,c),C) => insert_kind (C,v,c)) C defs
	
    fun extend_args  newD (Function (eff, recurs, vklist, vclist, vlist, bocy, con)) = 
	   let val newD = insert_kind_list (newD, vklist)
	       val newD = insert_con_list (newD, vclist) 
	       val newD = insert_con_list (newD, map (fn v=> (v, Prim_c (Float_c Prim.F32, [])))  vlist) 
	   in newD end 

    fun extend_functions D fcnlist = 
	foldl ( fn ((var, Function(e,r,tformals,formals,fformals,body,return)),D) =>
	       let val num_floats = Word32.fromInt (List.length fformals)
		   val con = AllArrow_c(Open, e, tformals, #2 (Listops.unzip formals),num_floats,return)
	       in 
		   insert_con (D, var, con)
	       end ) D fcnlist
	
    fun extend_bnds D bnds  =
	foldl (fn (bnd, D) =>
	       case bnd of 
		   Con_b(v,k, c) => insert_kind(D, v, k)
		 | Exp_b (v,c,e) => insert_con (D, v, c) 
		 | Fixopen_b vfset =>
		       let val fcnlist = Util.set2list vfset
		       in extend_functions D fcnlist
		       end
		 | _ => raise UNIMP) D bnds


    fun typecheck (D, exp) =
	let val ( exp, con) = NilStatic.exp_valid (D, exp) 
	in 
	    con
	end 


    (* --------- Code to get the type of expressions -----------*)
    
    fun nil_prim_type (D, prim, cons, exps) = 
	( case (prim, cons, exps) of
	      (record labels,cons,exps) =>
		  Prim_c (Record_c labels,cons)
	    | (select label,_,[exp]) =>
		  let val con = exp_type (D, exp)
		      (* val _ = ( Ppnil.pp_con con ; print " : nil prim type \n" ) *)
		      val Prim_c (Record_c labels, cons) = con
		      val SOME con =  Listops.find2 (fn (l,c) => eq_label (l,label)) (labels,cons)
		  in #2 con
		  end 
	    | (inject {tagcount,sumtype},cons,exps as ([] | [_])) => 
		  Prim_c (Sum_c {tagcount=tagcount,known=NONE},cons)
	    | (inject_record {tagcount,sumtype},argcons,argexps) => 
		  Prim_c (Sum_c {tagcount=tagcount,known=NONE},argcons)
	    | (project_sum {tagcount,sumtype=field},argcons,[argexp]) =>
		  List.nth (argcons,Word32.toInt (field - tagcount))
	    | (project_sum_record {tagcount,sumtype,field},argcons,[argexp]) => 
		  let val con_i = (List.nth (argcons,Word32.toInt (tagcount - sumtype)) )
		      handle Subscript => ( print "First one:" ; Ppnil.pp_exp (Prim_e ((NilPrimOp prim), cons, exps)) ; 
					   map Ppnil.pp_con argcons ; print ( Int.toString (Word32.toInt (tagcount - sumtype))) ;
					   raise Subscript )
		      val SOME (labels,cons) = NilUtil.strip_record con_i
		      val SOME con = Listops.assoc_eq (Name.eq_label, field, (Listops.zip labels cons)) 
		  in con
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
     and value_type (D, value) = 
	 case value of 
	     (int (intsize,word) |
		uint (intsize,word)) => 
		Prim_c (Int_c intsize,[])
	      | float (floatsize,string) => Prim_c (Float_c floatsize,[])
	      | array (con,arr) => Prim_c (Array_c,[con])
	      | vector (con,vec) => Prim_c (Vector_c,[con])
	      | refcell expref =>
		    let val con = exp_type(D, !expref) 
		    in Prim_c (Ref_c,[con]) end 
	      | tag (atag,con) =>
		    Prim_c (Exntag_c,[con])

    and exp_type (D, exp:exp) = 
 	 case exp of 
	     Var_e v => ( case find_con (D, v) of 
			 SOME c => c 
		       | NONE => raise BUG)
	   | Const_e v => value_type (D, v)
	   | Let_e (sort, bnds, exp ) => 
		  exp_type( (extend_bnds D bnds), exp)
	   | Prim_e ( NilPrimOp prim, clist, elist) =>
		 nil_prim_type (D, prim, clist, elist)
	   | Prim_e ( PrimOp prim, cons,   exps) =>
		 let val (total,arg_types,return_type) = PrimUtil.get_type prim cons
		 in return_type end 
		     
	   | Switch_e sw => switch_type (D,sw)
	   | App_e (_, Var_e f, cons, _, _) =>
		 let val SOME con =  find_con(D, f)
		     (* val _ = ( print "%%%%%%%%%%%%%%%%%%%%%%%%" ; Ppnil.pp_con con ; "\n" ) *)
		     val (AllArrow_c(_,_,vklist,_,_,con)) = NilStatic.con_reduce(D, con)
		     val subst = Subst.fromList (Listops.zip (#1 (Listops.unzip vklist)) cons)
		 in Subst.substConInCon subst con
		 end 
	     
     and switch_type (D, sw) =
	 case sw of 
	     Intsw_e {info=intsize,arg,arms,default} =>
		 let val default_con = Util.mapopt (curry2 exp_type D) default
		 in 
		     (case (default_con,arms)
			  of (SOME con,_) => con
			| (NONE,(_,Function (_,_,_,_,_,_,con))::_) => con
			)
		 end
	   | Sumsw_e {info=(non_val,val_cons),arg,arms,default} => 
		 let val default_con = Util.mapopt (curry2 exp_type D) default
		 in 
		     (case (default_con,arms)
			  of (SOME con,_) => con
			| (NONE,(_,Function (_,_,_,_,_,_,con))::_) => con
			)
		 end
	   | Exncase_e {info=_,arg,arms,default} =>
		 let val default_con = Util.mapopt (curry2 exp_type D) default
		 in
		     (case (default_con,arms)
			  of (SOME con,_) => con
			| (NONE,(_,Function (_,_,_,_,_,_,con))::_) => con
			)
		 end 
	   | Typecase_e {info,arg=argcon,arms,default} =>
		 let val default_con = Util.mapopt (curry2 exp_type D) default
		 in 
		     (case (default_con,arms)
			  of (SOME con,_) => con
			| (NONE,(_,Function (_,_,_,_,_,_,con))::_) => con
			)
		 end 

	 (* ---------------- Start of A-normalization code ------------------------ *)
	
     fun normalize_exp exp D ae k = 
	 ( (* p rint "normalize_exp called on : " ; Ppnil.pp_exp exp; print "\n"; *)
	case exp of
	    Var_e v => k (exp, D, ae)
	  | Const_e c => k (exp, D, ae)
	  | Let_e ( sort, bnds , body ) => 
		let val (bnds', newD, ae) = normalize_bnds bnds D ae
		    val exp = normalize_exp body newD ae k
		    in
			Let_e( sort, bnds', exp)
		end 
	  | Prim_e (allp, cons, exps) => 
		normalize_names exps D ae (fn (exps, D, ae) => 
					   let val exp = Prim_e(allp, cons, exps)
					       val exp = if !do_cse 
							     then cse_exp exp ae D (exp_type (D, exp))
							 else exp
					   in 
					       (k (exp, D, ae))
					   end )

	  | App_e (openness, f, cons, args, exps) =>
		(normalize_name f D ae ( fn (t, D, ae)  => 			      
				      normalize_names args D ae
				       ( fn (args', D, ae) => 
				       normalize_names exps D ae 
				       (fn (exps', D, ae) =>
					(k ( (App_e(openness, t, cons, args', exps')), D, ae))))))
	  | Switch_e sw  => (normalize_switch sw D ae k)  
	  | Raise_e(e,c) => normalize_name e D ae (fn (e', D, ae) => (k (Raise_e (e',c), D, ae) ))
	  | Handle_e (e,function as Function(eff,r,vks,vcs,vc,exp,c)) =>
		let val newD = extend_args D function
		    val exp = normalize_exp exp newD ae (fn (x, D, ae) => x)
		in 
		    (k (Handle_e (normalize_exp e D ae (fn (x, D, ae) => x) ,Function(eff,r,vks,vcs,vc,exp, c)), D, ae ))
		end )

     and name exp D ae k = 
	 let val t:var = (Name.fresh_var())
	     val _ = if !print_bind then ( Ppnil.pp_var t ; print " binds" ; Ppnil.pp_exp exp ; print " \n" )  
		 else ()
	     val con = exp_type (D, exp)
	     val (exp, ae) = if !do_cse then (cse_exp exp ae D con, if (is_elim_exp exp) 
									then Expmap.insert (ae, exp, (t, con))
								    else ae)
		 else (exp, ae)

	     val _ = NilOpts.inc_click normalize_click
	 in 
	     Let_e(Sequential, Exp_b( t, con, exp) :: [] , (k ((Var_e t), insert_con (D, t, con), ae) ))
	 end 
    and normalize_name exp D ae k = 
	(
	 normalize_exp exp D ae (fn (exp', D, ae) => 			       
			   (case exp' of
				(Const_e _ | Var_e _) => k (exp', D, ae)
			      | _  => name exp' D ae k)))

    and normalize_names ((hd :: exps) :exp list) D ae k = 
	normalize_name hd D ae (fn (t, D, ae) => normalize_names exps D ae (fn (exps', D, ae) => k ( t :: exps', D, ae ) ))
      | normalize_names [] D ae k = k ([],D, ae)
	
    and normalize_bnds (hd :: bnds) D ae  = 
	normalize_bnd hd D ae (fn (t, newD, ae) =>( let val (rest, newD, ae) =  normalize_bnds bnds newD ae
						 in (t @ rest, newD, ae) end ) )
      | normalize_bnds [] D ae =  ([], D, ae) 
	
     (* Returns a list of normalized bnds *) 
    and normalize_bnd bnd D ae k = 
	case bnd of 
	    Con_b (v, knd, c) => k ([ bnd ], insert_kind (D, v, knd), ae)
	  | Exp_b(var, con, exp) => let val exp' = normalize_exp exp D ae (fn (x, D,ae) => x)
					val con = exp_type (D, exp')
					val ae = if !do_cse andalso (is_elim_exp exp') then 
					    Expmap.insert (ae, exp', (var, con))
						 else 
						     ae
				    in 
					case exp' of 
					    Let_e ( sort, bnds, bdy) =>
						let val Let_e(sort, bnds, bdy) = Squish.squish exp'
						    val D = extend_bnds D bnds
						in 
						    k ( bnds @ [ Exp_b  (var, con, bdy) ], insert_con (D, var, con), ae)
						end 

					  | _ => k ( [ Exp_b(var, con, exp') ], insert_con (D, var, con), ae )
				    end 
	  | Fixopen_b fcnset =>
		let val fcnlist = Util.set2list fcnset
		    (* So that types of mutually recursive functions are available *)
		    val D = extend_functions D fcnlist  
			(***** Mark total functions for CSE ****) 
		    val _ = if !do_cse 
				then 
				    app ( fn (var, Function( eff, _, _, _, _, _, _)) =>
					 if eff = Total
					     then HashTable.insert fntable (var, true)
					 else () ) fcnlist
			    else ()
		in
		    k ( [ Fixopen_b( Util.list2set
			      (map (fn (v, (function as Function(e,r,vklist, vclist, vlist, exp, con))) =>
				    let val newD = extend_args D function
					val exp = normalize_exp exp newD ae (fn (x, D, ae)=>x)
				    in
					(v,Function(e,r,vklist, vclist, vlist, exp, con))
				    end )
			       fcnlist)) ], D, ae ) 
		end
	  | Fixcode_b _ => raise UNIMP
	  | Fixclosure_b _ => raise UNIMP
		
     and normalize_switch sw D ae k =
	 let fun id x = x
	     val do_sw = fn (do_t, { info, arg, arms, default}, D, ae)  => 
	     { info = info, 
	      arms = map (fn ( x, (function as Function(e,r,vklist, vclist, vlist, exp, con))) =>
			  let val newD = extend_args D function
			      val exp = normalize_exp exp newD ae (fn (x, D, ae)=>x)
			  in 
			      ( x, Function(e,r,vklist, vclist, vlist, exp, con))
			  end) arms,
	      arg = arg,
	      default = case default of 
	      SOME exp => SOME (normalize_exp exp D ae (fn (x, D, ae) => x))
	    | NONE => NONE }
	     fun do_arg do_t { info, arg, arms, default } func =
		 normalize_name arg D ae
		 ( fn (t, D, ae) =>
		  k ( Switch_e (func (do_sw (do_t, {info=info, arg=t, arms=arms, default=default}, D, ae))), D, ae) )
		 
	 in 
	      case sw of
		 Intsw_e sw =>  do_arg id sw Intsw_e
	       | Sumsw_e sw => do_arg id sw Sumsw_e
	       | Exncase_e sw => do_arg (fn exp => normalize_exp exp D ae (fn(x, D, ae) => x)) sw Exncase_e
	       | Typecase_e sw => k (Switch_e (Typecase_e (do_sw(id, sw, D, ae))), D, ae)
	 end

     fun doModule debug ( MODULE{bnds = bnds, imports=imports, exports = exports}) =
	 let val _ = print_bind := debug
	     
	     val baseDFn = (fn D => 
			( foldr ( fn (entry, D) => 
			    case entry of 
				ImportValue (l, v, c) => insert_con (D, v, c)
			      | ImportType (l , v, k) => insert_kind (D, v, k))
			 D imports  ))
	     val D = (baseDFn (empty()))
	     val (bnds, newD, ae) =  normalize_bnds bnds D Expmap.empty
	     val exports = let val D = extend_bnds D bnds
		     in  
			 map ( fn entry =>
			      case entry of 
				  ExportValue(lab, exp, con) => 
				      let val exp  = normalize_exp exp newD ae (fn (x, c, ae)=> x)
				      in 
					  ExportValue(lab, exp, con)
				      end
				| _=> entry ) exports
		     end
	     val _ = (print "Anormalization clicks\n***********************\n" ; 
		      NilOpts.print_round_clicks clicks)
	 in 
	     MODULE { bnds = bnds,
		     imports = imports,
		     exports = exports
		     } 
	 end 
	
end


