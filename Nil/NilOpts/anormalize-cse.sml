(* 

Routine to A-normalize Nil code. The alogorithm is taken from
Flanagan, Sabry, Duba, Felleisen "The Essence of Compiling with
Continuations", and extended to handle the NIL. 

Also does cse of constructors and expressions as it names each
intermediate value. 

A-normal form 'linearizes' the code. Actually the form I use is even
more restricted as it doesn't allow unnamed functions in the code.

sc := Var_c v | Prim_c (primcon, [])        (Variables or constant primitives)

con := Prim_c primcon * sc list
     | Mu_c bool * (var, con) seq * var            (can't make this (var, sc) seq as var will be unbound)
     | AllArrow_c openness * effect *       (Likewise, can't make con list and con be sc list and sc,
                    (var * kind) list *      as they could refer to type arguments.)
                    con list * w32 *con
     | Let_c letsort * conbnd list * con
     | Proj_c sc * label
     | Crecord_c (label*sc) list
     | App_c (Var_c f) * sc list            (con function must be named)
     | Closure_c of con * con               (Unimplemented in this version)
     | Typecase_c of { arg : sc,
                       arms : (primcon * (var*kind)list *con)list
		       default : sc,
		       kind : kind }
     | Annotate_c of annot * con 

conbnd :=                                   (same as nil) 
     Con_cb ( var * kind * con)          
     | Open_cb ( var * ( var * kind ) list * con * kind )
     | Code_cb ( var * ( var * kind ) list * con * kind )      (Unimplemented) 


se := Var_e v | Const_e c 

exp := se
     | Let_e letsort * bnd list * exp
     | Prim_e allprim * con list * se list 
     | Switch_e of switch
     | App_e openness * Var_e f * con list * se list * se list
     | Raise_e se * con
     | Handle_e exp * function              ( Note -- this is exp NOT se )

switch := 
     Intsw_e of (Prim.intsize, se, w32) sw
     | Sumsw_e of (w32 * con list, se, w32) sw
     | Exncase_e of (unit, se, se) sw
     | Typecase_e of (unit, con, primcon) sw

bnd := (same as nil )
function := (same as nil)





*)
signature ANORMALIZE =
    sig 
	structure Nil : NIL
	   
	val doModule : bool -> Nil.module -> Nil.module
	val test_exp : Nil.exp -> Nil.exp

    end 
functor Anormalize (structure Normalize : NORMALIZE
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
		    
		    sharing  Ppnil.Nil = Nil = NilStatic.Nil = NilEval.Nil = NilContext.Nil = NilUtil.Nil = Squish.Nil = ExpTable.Nil
		    sharing  PrimUtil.Prim = Nil.Prim
		    sharing type Nil.kind = Normalize.kind
		    sharing type PrimUtil.con = Nil.con = Subst.con = Normalize.con 
		    sharing type NilContext.context = NilStatic.context = Normalize.context
			) : ANORMALIZE =
    
struct 

    structure Nil = Nil
    open Nil Name Util Nil.Prim

    type context = NilContext.context  

    structure Expmap = ExpTable.Expmap                        (* Map of available expressions *)
    structure Conmap = ExpTable.Conmap                        (* Map of available constructors *)
    type avail = ((var * con) Expmap.map * var Conmap.map)    

    exception FnNotFound
    exception UNIMP
    exception BUG

    val print_bind = ref false
    val debug = ref false
    val error = fn s => Util.error "anormalize-cse" s
   
    (* ----------- CSE aux fns ------------ *)

    (* If we redo a constructor, we have to redo the kind *)	
    fun resingletonize kind con = 
	case kind of 
	    Singleton_k (phase, kind, oldcon) => Singleton_k (phase, kind, con)
	  | _ =>kind

	
    val do_cse = NilOpts.do_cse
    val agressive_cse = ref true

    val cse_exp_click = (NilOpts.make_click "CSE expressions")
    val cse_con_click = (NilOpts.make_click "CSE constructors")
    val normalize_click = (NilOpts.make_click "Normalization")
    val clicks = [ cse_exp_click, cse_con_click, normalize_click ]
    val inc_click = NilOpts.inc_click
    

    (* When we do CSE of expressions we want to keep track of which
     functions are total so that we can eliminate their applications
     if possible. We don't want to eliminate the application of non
     total functions as there could be side effects. *)

    (*True if the function is total, not found or false o/w  *)
    val fntable = Name.mk_var_hash_table (200, FnNotFound):
	(var, bool) HashTable.hash_table
	
    (* Does this primitive cause any side effects? Now as I see it
     there are several ways to do this. Right now I implement (b) as 
     it is the simplest.
     a. eliminate references and array subscripts, and clear out the
     available expressions after function calls.
     b. don't eliminate refs, but keep all avail expressions around.
     c. do both (only clear out available derefs and subs after
     function calls)
     *)

    fun is_elim_allp (NilPrimOp p) = true
      | is_elim_allp (PrimOp p) = 
	case p of
	    ( mk_ref | setref | open_in
	  | input | input1 | lookahead | open_out | close_in
	  | output | flush_out | close_out | end_of_stream ) => false 
	  | ( update _  | create_table _ ) => false
	  | ( deref | sub _ ) => false   (* ??? Keep all operations on the store *) 
	  | _ => true
	    

    (* Can we eliminate this expression -- In order to keep the
     asymptotic complexity reasonable, we don't include any expressions
     that can have full expressions as part of them. (Like switch) *)
		
    fun is_elim_exp exp =
	case exp of 
	    Prim_e( allp, _, _) => is_elim_allp allp
	  | App_e ( openness, Var_e v, _, _, _) =>
		if HashTable.find fntable v = SOME true then true
		else false 
		    
	  | _ => false

    (* We check the types here to see if they match to maintain type
    soundness. See Tarditi's thesis for more details. *)
    
    fun cse_exp exp ae D sigma =
	(if !debug then (print " ce ") else () ;
	 if (is_elim_exp exp) then 
	      ( case Expmap.find (ae, exp) of 
		    SOME (var, tau) => 
			if NilStatic.con_equiv (D, sigma, tau) then 
			    ( inc_click cse_exp_click ; Var_e var )
			else exp
		  | NONE => exp)
	  else exp)
    
	      
    fun small_con con =
	case con of 
	    Var_c v => true
	  | Prim_c (primcon, clist) => length clist = 0 
	  | _ => false

    fun is_elim_con con =
	(not (small_con con)) andalso
	(case con of 
	    ( Prim_c _ | Crecord_c _ | Proj_c _ | App_c _ ) => true
	  | ( AllArrow_c _ | Mu_c _ ) => !agressive_cse
	  | _ => false)

    (* Should really check the kinds here as well to see if they match *)
    fun cse_con con ac D =
	(if !debug then  print " cse_con: " else () ;
	 if (is_elim_con con) then  
	      ( case Conmap.find (ac, con) of 
		    SOME (var) => 
			( inc_click cse_con_click ; Var_c var )
		  | NONE => con)
	  else con)

    (* --------------------------- Nil Typechecking stuff -------------------------- *)
    
    (* Problem : When we name an expression and create a let binding,
     we have to have a constructor for its type.  Using nilstatic
     arbitrarily doesn't always work, unfortunately. So I wrote the
     following code to just read the types off of the expressions, and
     not do any type checking. The hard part is for selects, where we
     have to somehow get the type from the constructor of the record.
     *)
	      

    (* This keeps track of type variables, so when we select from a
     variable we can see what record constructor it is bound to. In
     all rights this should be a VarMap and passed around instead of a
     global hashtable. *)
    val env = Name.mk_var_hash_table(200,FnNotFound): (var, con)
     HashTable.hash_table

    
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

    (* Extends the context with the con for each recursive function *)	
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
		   Con_b(v,k, c) => 
		       ( HashTable.insert env (v, c) ; 
			insert_kind(D, v, k) )
		 | Exp_b (v,c,e) => insert_con (D, v, c) 
		 | Fixopen_b vfset =>
		       let val fcnlist = Util.set2list vfset
		       in extend_functions D fcnlist
		       end
		 | _ => raise UNIMP) D bnds

    fun avail_bnds (ae,ac) bnds = 
	foldl (fn (bnd, D) => 
	       case bnd of 
		   Con_b(v,k, c) => if (is_elim_con c) then (ae, Conmap.insert (ac, c, v))
		       else (ae, ac)
		 | Exp_b (v,c,e) => if (is_elim_exp e) then (Expmap.insert (ae, e, (v, c)), ac)
				    else (ae, ac)
		 | Fixopen_b vfset => (ae,ac)
		 | _ => raise UNIMP) (ae,ac) bnds


    fun extend_con_bnds D bnds  =
	foldl (fn (bnd, D) =>
	       case bnd of 
		   Con_cb(v,k, c) => ( HashTable.insert env (v, c) ; insert_kind(D, v, k) )
		 | Open_cb (var, vklist, con, kind) =>
		       insert_kind (D, var, Arrow_k (Open, vklist, kind))
		 | Code_cb (var, vklist, con, kind) =>
		       insert_kind (D, var, Arrow_k (Code, vklist, kind))) D bnds

    fun contype  (D, con) =
(*	let val (con, kind) = NilStatic.con_valid (D, con)
	    val kind = NilUtil.strip_singleton kind 
*)
	let val con = Normalize.con_normalize D con
	    val kind = Normalize.get_shape D con
	in ( if !debug then ( print "Kinding " ; Ppnil.pp_con con ; print " to be " ; Ppnil.pp_kind kind ; print "\n" ) else (); kind )
	end 
    (* --------- Code to get the type of expressions -----------*)

    (* Unfortunately when we do this we have to alpha convert the code as well. *)
    fun strip_record (con, label) = 
	case con of 
	    Prim_c (Record_c labels, cons) =>
		let val SOME (label,con) =  Listops.find2 (fn (l,c) => eq_label (l,label)) (labels,cons)
		in con end
	  | Var_c var => strip_record ((HashTable.lookup env var), label)
	  | Annotate_c (annot, con') => strip_record (con', label)
	  | Let_c ( sort, conbnds, con) =>
		Let_c (sort, conbnds, strip_record (con, label) )

    fun strip_arrow (con, cons) = 
	case con of 
	    AllArrow_c(_,_,vklist,_,_,con) =>
		let val subst = Subst.fromList (Listops.zip (#1 (Listops.unzip vklist)) cons)
		in Subst.substConInCon subst con
		end 
	  | Var_c var => strip_arrow ((HashTable.lookup env var), cons)
	  | Annotate_c (annot, con') => strip_arrow (con', cons)
	  | Let_c ( sort, conbnds, con) =>
		Let_c (sort, conbnds, strip_arrow (con, cons))
	  
    fun strip (D, con) = 
	case con of 
	    Var_c v => ( case  HashTable.find env v
			of SOME (con') => strip (D,con')
		      | NONE => con )
	  | Annotate_c (annot, con') => strip (D, con')
	  (* This is a real problem here *)
	  | Let_c ( sort, conbnds, exp) =>
		strip ( extend_con_bnds D conbnds, exp)	    
	  | _ => con

    (* Actually, should probably alpha-convert the output of this or something....*)
    fun nil_prim_type (D, prim, cons, exps) = 
	( case (prim, cons, exps) of
	      (record labels,cons,exps) =>
		  Prim_c (Record_c labels,cons)
	    | (select label,_,[exp]) =>
		  let val con = exp_type (D, exp)
		      val _ = (if !debug then (print "records con is " ; Ppnil.pp_con con ; print "\n" ) else ())
		      (* Now this is a real hack. I want to alpha convert, but all we can do is alpha-normalize! *)
		      val con = (strip_record (con,label))

(* This alpha_normalize can go into an infinite loop. See leroy1.sml.
		      val temp = Let_c ( Sequential, [ Con_cb(Name.fresh_var(), Type_k Runtime, con)], con) 
		      val Let_c (Sequential, bnds, return) = NilUtil.alpha_normalize_con (temp) 
*)		      
		  in con
		  end 
	    | (inject {tagcount,sumtype},cons,exps as ([] | [_])) => 
		  Prim_c (Sum_c {tagcount=tagcount,known=NONE},cons)
	    | (inject_record {tagcount,sumtype},argcons,argexps) => 
		  Prim_c (Sum_c {tagcount=tagcount,known=NONE},argcons)
	    | (project_sum {tagcount,sumtype=field},argcons,[argexp]) =>
		  List.nth (argcons,Word32.toInt (field - tagcount))
	    | (project_sum_record {tagcount,sumtype,field},argcons,[argexp]) => 
		  let val con_i = (List.nth (argcons,Word32.toInt (tagcount - sumtype)) )
		      handle Subscript => (
					   map Ppnil.pp_con argcons ; 
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
		  let val  Mu_c(bool,set,var) =  strip (D,argcon )
		      val def_list = set2list set
		      val (_,con') = valOf (List.find (fn (v,c) => eq_var (v,var)) def_list)
		      val cmap = Subst.fromList (map (fn (v,c) => (v,Mu_c (bool,set,v))) def_list)
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

(* The reason why we cant just say this is that the cons in D are in the wrong form.
   They are a-normalized instead of being fully applied. This leads to type errors. 
   
     and exp_type (D, exp:exp ) = 
	 let val (exp', con) = NilStatic.exp_valid (D, exp)
	 in con end
*)
    and exp_type (D, exp:exp) =
	(if !debug then (print "Typing" ; Ppnil.pp_exp exp ; print "\n") else ();
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
		 in 
		     strip_arrow (con, cons)
		 end )
	
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

     (* We'd like to do a little hoisting here, along with the normalization.
      Instead of leaving bindings at the con level, we'd like move them up to the term level *)

     datatype norm = EXP of exp | CON of con | BNDLIST of bnd list*context*avail | CONBNDLIST of  conbnd list*context*avail
	 | KIND of kind 

    fun TOEXP (var, kind, con, EXP body ) =
	EXP (Let_e (Sequential, [ Con_b(var, kind, con) ], body))
    fun TOCON (var, kind, con, CON body ) =
	CON (Let_c (Sequential, [ Con_cb(var, kind, con) ], body))
    fun TOBNDLIST (var, kind, con, BNDLIST (body, context,avail) ) = 
	BNDLIST ((Con_b(var, kind, con)) :: body, context,avail )
    fun TOCONBNDLIST (var, kind, con, CONBNDLIST (body, context,avail) ) = 
	CONBNDLIST ((Con_cb(var, kind, con)) :: body, context,avail )

    fun CONk (con, D, avail) = CON con
	(* k :  con * context * avail -> norm
	   bind : var * kind * con -> norm 
	 *)
	 

    fun normalize_kind (kind:kind) (D:context) (avail:avail) = 
	(if !debug then print "k" else ();
	 case kind of 
	    Type_k p => kind
	  | Word_k p => kind
	  | Singleton_k (p, kind, con) =>
		let val CON con = normalize_con con D avail (TOCON, CONk)
		    val kind =  normalize_kind kind D avail 
		in 
		   Singleton_k (p, kind, con)
		end

	  | Record_k lvkseq =>
		let val lvklist = Util.sequence2list lvkseq
		    val (lvs, kinds) = Listops.unzip lvklist
		    val kinds =  normalize_kinds kinds D avail
		in
			Record_k (Util.list2sequence (Listops.zip lvs kinds))
		end

	    
	  | Arrow_k (openn, vklist, knd) => 
		let val knd = normalize_kind knd D avail 
		    val (vs, ks) = Listops.unzip vklist
		    val ks = normalize_kinds ks D avail 
		in 
		    Arrow_k (openn, Listops.zip vs ks, knd)
		end)
	     
    and normalize_kinds kinds D avail = 
	map (fn (k) => normalize_kind k D avail) kinds

    and normalize_con con D (avail as (ae,ac)) (bind, k) = 
	(if !debug then print "c" else ();
	case con of
	    Prim_c (primcon, cons) => 
		normalize_con_names cons D avail 
		(bind, (fn (cons, D, avail) => 
		 let val con = Prim_c(primcon, cons)
		     val con = if !do_cse 
				   then cse_con con ac D 
			       else con
		 in 
		     (k (con, D, avail))
		 end ))
	  | Mu_c (bool,vcseq, var) => 
		let val vclist = sequence2list vcseq
		    fun folder((v,_),subst) = (case find_kind(D, v) of
						   NONE => subst
						 | SOME _ => Subst.add subst (v,Var_c(Name.derived_var v)))
		    val subst = foldl folder (Subst.empty()) vclist
		    fun substcon c = if (Subst.is_empty subst)
					 then c
				     else Subst.substConInCon subst c
		    fun lookup var = (case (substcon (Var_c var)) of
					  (Var_c v) => v
					| _ => error "substcon returned non Var_c")
		    val var_kinds = map (fn (var,con) => (lookup var, Word_k Runtime)) vclist
		    val newD = insert_kind_list (D, var_kinds)
		    fun do_con (var, con) = 
			let val var = lookup var
			    val con = substcon con
			    val CON con = normalize_con con newD avail (TOCON, CONk)
			in (var, con)
			end 
		in 
		    k ( Mu_c (bool,map do_con vclist, lookup var), D, avail)
		end 
	  | AllArrow_c ( openness, effect, vklist, clist, w32, con) => 
	        if null vklist then
		    normalize_con_names clist D avail 
			(bind, (fn (clist, D,avail) => 
			 normalize_con_name con D avail
			 (bind, (fn (con, D, avail) =>
				 k ( AllArrow_c( openness, effect, vklist, clist, w32, con), 
				    D, 
				    avail)))))
		
		(* The variables in the vklist are bound in the return
		 con and the CLIST, so we don't want to lift any thing
		 out as it may contain unbound variables. 
		 *)
		
		else 
		    let val (vs, ks) = Listops.unzip vklist
			val kinds = normalize_kinds ks D avail 
			val vklist = Listops.zip vs kinds
			val newD = insert_kind_list (D, vklist)
			     fun do_con con = 
				 let val CON con = normalize_con con newD avail (TOCON, CONk)
				 in con
				 end 
			     val clist = map do_con clist
			     val con = do_con con
		    in 
			k ( AllArrow_c ( openness, effect, vklist, clist, w32, con), D, avail)
		    end

	  | Var_c v => k (con, D, avail)		
	  | Let_c ( sort, bnds , body ) => 
		let val (bnds:conbnd list, bndD, bndavail ) = normalize_con_bnds bnds D avail
		    val CON con = normalize_con body bndD bndavail (TOCON, CONk) 
		in 	
		    k (Let_c (sort, bnds, con), D, avail)		
		end 

	  | Crecord_c lclist => 
		let val (labels, cons) = Listops.unzip lclist
		in 
		    normalize_con_names cons D avail (bind, 
		    ( fn (cons, D, avail) => 
		     k (Crecord_c (Listops.zip labels cons), D, avail) ))
		end

	  | Proj_c (con, label) =>
		normalize_con_name con D avail (bind,
		( fn (con, D, avail) => k ( Proj_c (con, label), D, avail)))

	  | App_c ( f, cons ) =>
		(normalize_con_name f D avail (bind, 
		 ( fn (t, D, avail)  => 			      
		  normalize_con_names cons D avail (bind,
		  (fn (cons, D, avail) =>
		   (k ( (App_c( t, cons)), D, avail)))))))

	  | Typecase_c sw  => (normalize_typecase sw D avail (bind, k))  )

    and normalize_cons ((hd :: cons) :con list) D avail (bind, k) = 
	normalize_con hd D avail 
	(bind, (fn (t, D, avail) => normalize_cons cons D avail 
		(bind, (fn (cons', D, avail) => k ( t :: cons', D, avail ) ))))
      | normalize_cons [] D avail (bind, k) = k ([],D, avail)

    and normalize_con_names ((hd :: cons) :con list) D avail (bind, k) = 
	normalize_con_name hd D avail 
	(bind, (fn (t, D, avail) => normalize_con_names cons D avail 
		(bind, (fn (cons', D, avail) => k ( t :: cons', D, avail ) ))))
      | normalize_con_names [] D avail (bind, k) = k ([],D, avail)

    and normalize_con_name con D (avail as (ae,ac)) (bind, k) = 
	(normalize_con con D avail (bind, 
	 (fn (con, D, (ae,ac)) => 
	  let val t:var = (Name.fresh_var()) 
	      val con = if !do_cse then cse_con con ac D else con
	      val ac = if (is_elim_con con) then Conmap.insert (ac, con, t) else ac
	  in 
	      if small_con con then  k (con, D, (ae,ac))
	      else 
		  let 
		      val _ = HashTable.insert env (t,con)
		      val kind =  contype (D, con) 
		      val kind =  normalize_kind kind D (ae,ac)
		      val _ = NilOpts.inc_click normalize_click
		  in
		       bind (t, kind, con, (k ((Var_c t), insert_kind (D, t, kind), (ae,ac) )))
		  end 
	  end))) 
	
     and normalize_con_bnds (hd :: bnds) D avail : conbnd list * context *avail= 
	 normalize_con_bnd hd D avail 
	 (fn (t, newD, avail) =>
	  ( let val (rest, newD, avail) =  normalize_con_bnds bnds newD avail
	    in (t @ rest, newD, avail) end))
       | normalize_con_bnds [] D avail =  ([], D, avail) 
	
     (******* ACK!!!!!!!!!!!!!!!!!!!!!!!! ***********************)
    and normalize_con_bnd bnd D (avail as (ae,ac)) (k:conbnd list*context*avail->conbnd list*context*avail ) = 
	case bnd of 
       Con_cb(var, kind, con) => 
	   let val CONBNDLIST return = normalize_con con D avail 
	       (TOCONBNDLIST, 
		(fn (con, D, (ae,ac)) =>
		 let val kind =  normalize_kind kind D (ae,ac) 
		     val _ = HashTable.insert env (var, con)
		       
		       (* We have to have this, as if it has a singleton kind 
                                      then we want it to reflect the new con *)
		       
		       val ac = if !do_cse andalso (is_elim_con con) then 
			   Conmap.insert (ac, con, var)
				else 
				    ac
		   in 
				   (*  
				    Figure something out here.... might be difficult
				    case con of 
					Let_c ( sort, bnds, bdy) =>
					    let val Let_c(sort, bnds, bdy) = Squish.squish_con con
			     val D = extend_bnds D bnds
			 in 
			     k ( bnds @ [ Con_cb  (var, kind, bdy) ], insert_kind (D, var, kind), (ae,ac))
			 end 
		   | _ => *) 
		       CONBNDLIST (k ( [ Con_cb(var,kind, con)] , insert_kind (D, var, kind), (ae,ac) ))
		 end))
	   in return 
	   end 


      | Open_cb (v, vklist, con, kind) => 
	    let val vklist = map (fn (v,k) => (v, normalize_kind k D avail)) vklist
		val kind = normalize_kind kind D avail 
		val newD = insert_kind_list (D, vklist)
		val returnD = insert_kind (D, v, Arrow_k (Open, vklist, kind))
		val CONBNDLIST return = normalize_con con newD avail 
		    (TOCONBNDLIST,
		     (fn (con, D, avail) => 
		      CONBNDLIST (k ( [Open_cb(v, vklist, con, kind) ] ,  returnD, avail ))))
	    in return
	    end 
      | Code_cb _ => raise UNIMP (* Should be the same as Open_cb *)
	
    and normalize_typecase (sw as { kind, arg, arms, default }) D avail (bind, k) =
	 let fun id x = x
	     val do_sw = fn ( { kind, arg, arms, default}, D, avail)  => 
		 let val CON default = (normalize_con default D avail (TOCON, CONk))
		 in 
		 { kind = normalize_kind kind D avail , 
		  
		  arms =  map (fn ( primcon, vklist, con) => 
			       let val vklist = map (fn (v,k) => (v, normalize_kind k D avail)) vklist
				   val newD = insert_kind_list (D, vklist)
				   val CON con = normalize_con con newD avail (TOCON, CONk)
			      in 
				  ( primcon, vklist, con)
			      end)  arms,
		  arg = arg,
		  default =  default
		  }
		 end 
	 in 
	     normalize_con_name arg D avail (bind,
	     ( fn (t, D, avail) =>
	      k ( Typecase_c (do_sw (sw, D, avail)),  D, avail)))
	 end  

    fun normalize_exp exp D (avail as (ae, ac)) (k:exp*context*avail->exp) = 
	( if !debug then ( print "e:" ; Ppnil.pp_exp exp ; print "\n----------------------------------\n") else ();
	  case exp of
	    Var_e v => k (exp, D, (ae, ac))
	  | Const_e c => k (exp, D, (ae, ac))
	  | Let_e ( sort, bnds , body ) => 
		let val (bnds, D, avail) = normalize_exp_bnds bnds D avail
		in
		    Let_e (sort, bnds, 
			   normalize_exp body D avail k )
		end 

	  | Prim_e (allp, cons, exps) => 
		let val EXP returnexp = (normalize_cons cons D (ae, ac) 
			       (TOEXP, (fn (cons, D, (ae,ac)) => 
					EXP (normalize_exp_names exps D (ae, ac) 
					(fn (exps, D, (ae, ac)) => 
					 let val exp = Prim_e(allp, cons, exps)
					     val exp = if !do_cse 
							   then 
							       ( 
								cse_exp exp ae D (exp_type (D, exp)) )
						       else exp
					 in 
					     (k (exp, D, (ae, ac)))
					 end )))))
		in returnexp end 

	  | App_e (openness, f, cons, args, exps) =>
		 (normalize_exp_name f D (ae, ac) 
		  (fn (t, D, (ae, ac))  => 
		   let val EXP returnexp = (normalize_cons cons D (ae, ac) 
				      (TOEXP,  (fn (cons, D, (ae, ac)) => 
						EXP (normalize_exp_names args D (ae, ac)
						     (fn (args', D, (ae, ac)) => 
						      normalize_exp_names exps D (ae, ac) 
						      (fn (exps', D, (ae, ac)) =>
						       (k ( (App_e(openness, t, cons, args', exps')), D, (ae, ac)))))))))
		   in returnexp end ))
	  | Switch_e sw  => (normalize_switch sw D (ae, ac) k)  
	  | Raise_e(e,c) => normalize_exp_name e D (ae, ac) 
		 (fn (e', D, (ae, ac)) =>
		  let val EXP returnexp = normalize_con c D (ae,ac) 
		      (TOEXP, (fn (con, D, (ae, ac)) => 
			       EXP (k (Raise_e (e',c), D, (ae, ac)))))
		  in returnexp end )
	  | Handle_e (e,function as Function(eff,r,vks,vcs,vc,exp,c)) =>
		let val [(v,func)] = normalize_fcn (Name.fresh_var(), function) D (ae,ac) 
		    (fn (vf, D, a) => [ vf] )
		in 
		    (k (Handle_e (normalize_exp e D (ae, ac) #1, func), D, (ae, ac)))
		end )
	 
		  
    and name_exp exp D (ae, ac) k = 
	 let val _ = if !debug then print "n" else ()
	     val t:var = (Name.fresh_var())
	     val con = exp_type (D, exp)
	     val (exp, ae) = if !do_cse then (cse_exp exp ae D con, 
					      if (is_elim_exp exp) 
						  then Expmap.insert (ae, exp, (t, con))
					      else ae)
			     else (exp, ae)

	     val _ = NilOpts.inc_click normalize_click
	     val EXP returnexp = normalize_con con D (ae,ac)
		 (TOEXP, (fn (con, D, (ae, ac)) => 
			  EXP (Let_e(Sequential, Exp_b( t, con, exp) :: [] , 
				     (k ((Var_e t), insert_con (D, t, con), (ae, ac)) )))))
	 in returnexp
	 end 
    and normalize_exp_name exp D (ae, ac) (k:exp*context*avail->exp) = 
	(
	 normalize_exp exp D (ae, ac)
	 (fn (exp', D, (ae, ac)) => 			       
	  (case exp' of
	       (Const_e _ | Var_e _) => k (exp', D, (ae, ac))
	     | _  => name_exp exp' D (ae, ac) k)))

    and normalize_exp_names ((hd :: exps) :exp list) D (ae, ac) (k:exp list*context*avail->exp) = 
	normalize_exp_name hd D (ae, ac)
	(fn (t, D, (ae, ac)) => 
	 normalize_exp_names exps D (ae, ac)
	 (fn (exps', D, (ae, ac)) => k ( t :: exps', D, (ae, ac) ) ))
      | normalize_exp_names [] D (ae, ac) k = k ([],D, (ae, ac))
	

     
    and normalize_exp_bnds (bnd::bnds) D avail = 
	normalize_exp_bnd bnd D avail 
	(fn (bndlist, D, avail) => 
	 let val (rest, newD, ae) =  normalize_exp_bnds bnds D avail
	 in (bndlist @ rest, newD, ae) end)

      | normalize_exp_bnds [] D avail = ([], D, avail)
  	
     (* k takes a list of normalized bnds *) 
    and normalize_exp_bnd bnd D (ae, ac) (k:bnd list*context*avail->bnd list*context*avail) = 
	case bnd of 
	    Con_b (v, knd, c) =>
		let val BNDLIST returnbnds = normalize_con c D (ae, ac) 
		    (TOBNDLIST, (fn (con,D, (ae,ac)) =>
				 let val knd = normalize_kind knd D (ae,ac)
				     val ac = if !do_cse andalso (is_elim_con con) then 
					 Conmap.insert (ac, con, v)
					      else ac
				 in 
				     BNDLIST ( HashTable.insert env (v, con) ; 
					      k ([ Con_b(v,knd, c) ], insert_kind (D, v, knd), (ae, ac)))
				 end ))
		in returnbnds end 
	    
	  | Exp_b(var, con, exp) => 
		let  val _ = ( if !debug then( print "Normalizing bind " ; Ppnil.pp_var var ; print "\n" ) else ())
		    val BNDLIST returnbnds = normalize_con con D (ae,ac)
		    (TOBNDLIST, (fn (con, D, (ae, ac)) => 
				 let val exp' =  normalize_exp exp D (ae, ac) #1
				     val ae = if !do_cse andalso (is_elim_exp exp') then 
					 Expmap.insert (ae, exp', (var, con))
					      else ae
				 in 
				     BNDLIST 
				     ( case exp' of 
					 Let_e ( sort, bnds, bdy) =>
					     let val Let_e(sort, bnds, bdy) = Squish.squish exp'
						 val D = extend_bnds D bnds
						 val (ae,ac) = avail_bnds (ae,ac) bnds
					     in 
						 k ( bnds @ [ Exp_b  (var, con, bdy) ], insert_con (D, var, con), (ae, ac))
					     end 
					 
					 | _ => k ( [ Exp_b(var, con, exp') ], insert_con (D, var, con), (ae, ac) ))
				 end))
		in returnbnds end
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
		    val fcnlist = 
			normalize_fcns fcnlist D (ae,ac) #1
		in 
		    k ( [ Fixopen_b( Util.list2set fcnlist) ], D,(ae,ac) )
		end
	  | Fixcode_b _ => raise UNIMP
	  | Fixclosure_b _ => raise UNIMP
 
     and normalize_fcn (v, (function as Function(e,r,vklist, vclist, vlist, exp, con))) D (ae, ac) 
	 (k:(var*function)*context*avail->(var*function)list ) =
	 let val vklist = map (fn (v,k) => (v, normalize_kind k D (ae,ac))) vklist
	     val (vars, cons) = Listops.unzip vclist
	     val D = insert_kind_list (D,vklist) 

	     (* Normalize the function argument types and return
	      types, but remember that since type args can appear in
	      them, we can't lift them out of the Function binding.
	      
	      *)
	     fun do_con con = 
		 let val CON con = normalize_con con D (ae,ac) (TOCON, CONk)
		 in con end 
	
	     val con = do_con con
	     val vclist = Listops.zip vars (map do_con cons)
	     val newD = insert_con_list (D, vclist) 
	     val newD = insert_con_list (newD, map (fn v=> (v, Prim_c (Float_c Prim.F32, [])))  vlist) 
	     val exp = normalize_exp exp newD (ae, ac) (fn (x, D, (ae, ac))=>x)

	 in ( k ((v,Function(e,r,vklist, vclist, vlist, exp, con)), D, (ae,ac)))
	 end 
    and normalize_fcns (hd :: fcns) D a (k:(var*function)list*context*avail->(var*function)list) =
	normalize_fcn hd D a (fn (hd, D, a) => 
			       normalize_fcns fcns D a (fn (fcns, D, a) => (k ((hd::fcns), D, a))))
       | normalize_fcns [] D a k = k ([], D, a)
	 
     (* arms need to be normalized ... think about that .... *) 
     and normalize_switch sw D (ae, ac) k =
	 let fun id _ _ x = x
	     fun norm_con D (ae,ac) con  = let val CON con = normalize_con con D (ae,ac) (TOCON, CONk) in con end
	     val do_sw = fn (do_t, do_info, { info, arg, arms, default}, D, (ae, ac))  =>
		 { info = do_info D (ae,ac) info, 
		  arms = map (fn ( x, (function as Function(e,r,vklist, vclist, vlist, exp, con))) =>
			      (* First extend D with vklist. Then normalize cons and return con. Then extend D with 
			       vclist. Then normalize the return exp. *)
			      	
			      let val vklist = map (fn (v,k) => (v, normalize_kind k D (ae,ac))) vklist
				  val newD = insert_kind_list (D, vklist)
				  val (vars, cons) = Listops.unzip vclist
				  val vclist = Listops.zip vars (map (norm_con newD (ae,ac)) cons)
				  val con = norm_con newD (ae,ac) con
				  val newD = insert_con_list (newD, vclist) 
				  val newD = insert_con_list (newD, map (fn v=> (v, Prim_c (Float_c Prim.F32, [])))  vlist) 
				  val exp = normalize_exp exp newD (ae, ac) (fn (x, D, (ae, ac))=>x)
			      in 
				  ( do_t D (ae,ac) x, Function(e,r,vklist, vclist, vlist, exp, con))
			      end) arms,
		  arg = arg,
		  default = case default of 
		  SOME exp => SOME (normalize_exp exp D (ae, ac) (fn (x, D, (ae, ac)) => x))
		| NONE => NONE }
	     fun do_arg do_t do_info { info, arg, arms, default } func =
		 normalize_exp_name arg D (ae, ac)
		 ( fn (t, D, (ae, ac)) =>
		  k ( Switch_e (func (do_sw (do_t, do_info, {info=info, arg=t, arms=arms, default=default}, D, (ae, ac)))), D, (ae, ac)) )
	     fun sum_do_info D avail (w32, cons) = 
		 (w32, map (norm_con D avail) cons)
	     fun exn_do_t D avail exp =  normalize_exp exp D (ae, ac) (fn(x, D, (ae, ac)) => x)
	 in 
	      case sw of
		 Intsw_e sw =>  do_arg id id sw Intsw_e
	       | Sumsw_e sw => do_arg id sum_do_info sw Sumsw_e
	       | Exncase_e sw => do_arg exn_do_t id sw Exncase_e
	       | Typecase_e sw => k (Switch_e (Typecase_e (do_sw(id, id, sw, D, (ae, ac)))), D, (ae, ac))
	 end

     (* ---- constructors ----- *)
    fun test_exp exp = 
	normalize_exp exp (empty()) (Expmap.empty, Conmap.empty)  (fn (exp, newD, (ae, ac)) => exp)

     fun doModule debug ( MODULE{bnds = bnds, imports=imports, exports = exports}) =
	 let val _ = print_bind := debug
	     fun clearTable table = 
		 HashTable.appi ( fn (key, item) => ignore (HashTable.remove  table key)) table
	     val _ = clearTable env
	     
	     val baseDFn = (fn D => 
			( foldl ( fn (entry, D) => 
			    case entry of 
				ImportValue (l, v, c) => insert_con (D, v, c)
			      | ImportType (l , v, k) => insert_kind (D, v, k))
			 D imports  ))
	     val D = (baseDFn (empty()))
	     val temp =  Let_e(Sequential,bnds, Prim_e(NilPrimOp(inject {tagcount=0w2,sumtype=0w1}),[],[])  ) (* true! *)
	     val  exp =  
		 normalize_exp temp D (Expmap.empty, Conmap.empty)  (fn (exp, newD, (ae, ac)) => exp)
	     val Let_e(Sequential,bnds,_) = Squish.squish exp
	     val _ = if debug then (print "Normalized bnds *********************************************************\n") else ()
	     val exports = let val D = extend_bnds D bnds
		     in  
			 map ( fn entry =>
			      case entry of 
				  ExportValue(lab, exp, con) => 
				      let val exp  = normalize_exp exp D (Expmap.empty, Conmap.empty) (fn (x, c, (ae, ac))=> x)
				      in 
					  ExportValue(lab, exp, con)
				      end
				  | ExportType (lab, con, kind) =>
					let val CON con = normalize_con con D (Expmap.empty, Conmap.empty) (TOCON, CONk)
					    val kind = normalize_kind kind D (Expmap.empty, Conmap.empty)
				      in 
					  ExportType(lab, con, kind) 
				      end
				) exports
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





