(* This doesn't handle Fixclosure, Fixcode or Parallel lets *)


functor Reduce  ( 
                  structure Nil : NIL
		  structure Ppnil : PPNIL 
		  structure PrimUtil : PRIMUTIL
		  structure NilEval : NILEVAL
		  structure NilUtil : NILUTIL
		  structure NilSubst : NILSUBST
		  structure Squish : SQUISH 
		  sharing NilUtil.Nil = Ppnil.Nil = NilEval.Nil = Nil
		  sharing type Squish.Nil.exp = Nil.exp
		  sharing type PrimUtil.con = Nil.con
		  sharing type PrimUtil.exp = Nil.exp
		  sharing type Nil.exp = Ppnil.Nil.exp
		  sharing type Nil.Prim.prim = PrimUtil.Prim.prim

		  sharing type Nil.exp = NilSubst.exp
	          sharing type Nil.con = NilSubst.con
	          sharing type Nil.kind = NilSubst.kind
			) : PASS =

struct 
    
    open Nil Name Util
    structure Nil = Nil

    exception FnNotFound
    exception BUG

    fun doModule  debug (MODULE{bnds=bnds, imports=imports, exports=exports}) = 
	let 

	    (* First, stuff to keep track of how many reductions have been done. *)
	     
	    val select_click = NilOpts.make_click "Selects from known records" 
	    (* How many selections from know records have been replaced *)
	    val inline_click = NilOpts.make_click "Functions Inlined" 
	    (* How many functions called only once inlined *)
	    val dead_click = NilOpts.make_click "Dead variables eliminated"
	    (* How many dead variables have  been removed *)
	    val fold_click = NilOpts.make_click "Constants folded"        
	    (* How many constants have been folded *)
	    val switch_click = NilOpts.make_click "Known switches eliminated"    
	    (* How many known switches have been eliminated *)
	    val var_click = NilOpts.make_click "Vars propagated"
		val sum_record_click = NilOpts.make_click "project_sums to project_sum_records"

	    val clicks = [ select_click, inline_click, var_click,
			  dead_click, fold_click, switch_click, sum_record_click ]

	    fun round_clicks unit = NilOpts.round_clicks clicks
	    fun print_round_clicks unit  = NilOpts.print_round_clicks clicks
	    val inc_click  = NilOpts.inc_click
	    

	    (* Now, tables to keep information about the code as
	     we walk it *)
		
	    (* Unlike Appel's paper, we specifically keep track of
	     recursive functions.  I can't really see how to extend
	     his method to handle mututally recursive functions. *)
	    val recset = ref VarSet.empty
			
	    (* Remembers  functions and records to be inlined or 
	     projected from later *)
	    datatype bind = F of function | R of label list * exp list | INLINED 
	      | S of var * {tagcount: w32, sumtype  : w32} * con list
 
	    val bind =  Name.mk_var_hash_table(200,FnNotFound):
		(var, bind) HashTable.hash_table
	
	    (* Maps variables to new variables or to constants, for
	     substituting one value for another *)
	    val sigma =  Name.mk_var_hash_table(200,FnNotFound): 
		(var, exp) HashTable.hash_table
		
	    fun insert_sigma x v = 
		HashTable.insert sigma (x, v)
		
	    fun s a  = 
		case HashTable.find sigma a of 
		    SOME x => x
		  | NONE => Var_e a

		
	    (* Counts the number of times a variable appears in the
	     function position *) 
	    val count_app =
		 Name.mk_var_hash_table(200,FnNotFound): (var, (int ref))
		 HashTable.hash_table

	    (* Counts the number of times a variable appears as an
	     argument *)
	    val count_esc = Name.mk_var_hash_table(200,FnNotFound): 
		 (var, (int ref)) HashTable.hash_table

	
	    fun update t (Var_e x) y =
		((let val (cell:int ref) = HashTable.lookup t x
		in 
		    cell := (!cell) + y
		end) 
		     handle FnNotFound => ())
	      | update t x y = () 
		
	    fun zero t (Var_e x) = 
		((let val (cell:int ref) = HashTable.lookup t x
		in 
		    cell := 0
		end)
		     handle FnNotFound => ())
	      | zero t x = ()
		
	    fun look t a =
	        ! (HashTable.lookup t a)
		handle FnNotFound => 
		    (* Anything that we can't find is not declared in this file, so we can't 
		     inline it or anything. So always return 1 *) 
		    ( (* print "Can't find " ; Ppnil.pp_var a ; print "\n" ; *) 1 ) 
		 
	    fun replace x exp = 
		( (case exp of 
		     Var_e b => 
			 ( insert_sigma x (s(b)) ;
			  update count_app (s(b)) (look count_app x) ;
			  update count_esc (s(b)) (look count_esc x)  )
		   | Const_e b =>  (* Some constant expression *)
			 insert_sigma x exp) ;
		       zero count_app (Var_e x);
		       zero count_esc (Var_e x) )

	    (* The following functions implement the census algorithm,
	     which determines for each variable how many times it is
	     used or escapes. Unlike Appel's version, this one doesn't
	     count recursive calls/uses of functions *)
		     
	   
		
	    local 
		val delta = ref 1  (* What should we change 
				    the value by (usually +1 or -1 *)
		fun inc x =  x:= (!x) + (!delta)
		    
		fun declare x =  if not ( isSome (HashTable.find count_esc x))
				     then 
					 ( HashTable.insert count_esc(x,ref 0) ;
					  HashTable.insert count_app(x, ref 0) )
				 else () 
			 
		fun insesc x = 
		    let  val y = s(x)
		    in 
			case y of 
			    Var_e y =>
				( case (HashTable.find count_esc y) of
				      NONE =>  () 
				    | SOME use =>   inc use ) 
			  | _ => ()
		    end 
		fun insapp x =
		    let  val  y = s(x) 
		    in 
			case y of 
			    Var_e y => 
				( case (HashTable.find count_app y ) of
				      NONE => () 
				    | SOME use => inc use ) 
			  |  _  => () (* Constant expression *)
		    end
    
		fun scan_exp exp = 
		    case exp of
			(Var_e v) => insesc v
		      | (Const_e v) => ()
		      | (Let_e (_, bndlist , body)) => ( app scan_bnd bndlist; scan_exp body)
		      | (Prim_e ( allp, clist, elist)) =>
			    ( app scan_exp elist)
		      | (Switch_e switch) => (scan_switch switch)
		      | (App_e ( openness, efunc, clist, elist, eflist)) =>
			    ( case efunc of 
				  Var_e v => insapp v
				| _ => scan_exp efunc ; app scan_exp elist)
		      | (Raise_e (e, c)) => (scan_exp e)
		      | (Handle_e (e, Function(_,_,_,_,_,ef,_))) =>
			    (scan_exp e; scan_exp ef)
			    
		and scan_switch s =
		    let fun nada x = ()
			fun do_sw do_arg do_t {arms,default,arg,info } =
			    ( app (fn (t,Function(_,_,_,_,_,exp,_)) => (do_t t; scan_exp exp)) arms;
			     do_arg arg;
			     case default of 
				 SOME exp => scan_exp exp
			       | NONE => () )
		    in
			case s of
			    Intsw_e sw => do_sw scan_exp nada sw
			  | Sumsw_e sw => do_sw scan_exp nada sw
			  | Exncase_e sw => do_sw scan_exp scan_exp sw 
			  | Typecase_e sw => do_sw nada nada sw 		
		    end
		and scan_bnd bnd = 
		    case bnd of
			(Con_b (v, k, con) ) => ()
		      | (Exp_b (v, c, exp)) => ( declare v; scan_exp exp )
		      | (Fixopen_b vcset | Fixcode_b vcset) => 
			    let val vflist = Util.set2list vcset
				fun dec (v,c) = declare v
				val _ = app dec vflist
				val _ = app (fn (v,Function(_,_,_,varlist,_,exp,_)) => 
					     (map dec varlist; scan_exp exp)) vflist 
				fun used t v = 
				    case HashTable.find t v of
					SOME x => not ( !x = 0 )
 				      | NONE => false
					    
				fun recursive (v,f) = 
				    used count_app v orelse used count_esc v
				    
				    
			    in
				if Listops.orfold recursive vflist then
				    let val vars = (map #1 vflist)
				    in 
					( 
					 app (fn v => (zero count_app (Var_e v) ; zero count_esc (Var_e v))) vars ;
					 recset :=  VarSet.addList ((!recset), vars))
				    end
				else 
				    ()
			    end
		      |  Fixclosure_b vcset => raise UNIMP 
	    in 
		fun census_exp ( x , exp ) = 
		    ( delta := x ;
		     scan_exp exp ) ;
		    
		fun census_bnds ( x, bnds ) = 
		    ( delta := x;
		     map scan_bnd bnds );
	    end (* Census functions *)

	val print_table = fn (var, use)=>
	    ( Ppnil.pp_var var; print " : "; print (Int.toString (!use)); print "\n")
	   
	fun print_stats unit = 
	    ( print "****************************************************\n"; 
	     print "APP TABLE\n";
	     HashTable.appi print_table count_app;
	     print "\nESC TABLE\n";
	     HashTable.appi print_table count_esc;
	     print "****************************************************\n" ) 


	    (* The following functions implement the ncontract algorithm *)

	local 
	    fun dead_var x =
		if (look count_app x = 0 andalso look count_esc x = 0 )
		    then ( (* Ppnil.pp_var x; print " is dead\n"; *) inc_click dead_click ; true )
		else false

	    fun dead_vars xs = 
		let fun dead x = (look count_app x = 0 andalso look count_esc x = 0 )
		in 
		    if ( Listops.andfold dead xs ) 
			then   ( (* app Ppnil.pp_var xs; print " are dead\n"; *) inc_click dead_click ; true )
		    else
			false
		end 
	    fun subst exp = 
		case exp of
		    Var_e v => s(v)
		  | Const_e c  => exp 
		  | _ => raise BUG
	    
	in 
	    fun xswitch s =
		let fun id x = x
		    fun do_sw constr do_arg do_t {arms,default,arg,info } =
			let val newarg = do_arg arg
			    val newarms = map (fn (t,Function(a,b,c,d,e,exp,f)) => 
					       (do_t t, Function(a,b,c,d,e,xexp exp,f))) arms
			    val newdefault = case default of 
				SOME exp => SOME (xexp exp)
			      | NONE => NONE 
			in constr {arms = newarms, default = newdefault, arg = newarg, info= info}
			end 
		    
		    fun intopt( sw as {arms,default,arg,info }) =
			case arg of
			    Const_e (Prim.int(_,w64)) =>
				let fun loop [] =(  case default of 
						  NONE => Switch_e (Intsw_e sw)    
						(* If we get here we have a Match exception so can't opt *)
						| SOME e => (inc_click switch_click; e ))
				      | loop ((t,Function(_,_,[],[],[], reducedexp, con)) :: rest ) = 
					if (TilWord64.equal(w64, TilWord64.fromUnsignedHalf t))
					    then  (inc_click switch_click; reducedexp)
					else
					    loop (rest)
				in 
				    loop arms
				end
			  | _ => Switch_e (Intsw_e sw)
		    fun sumopt ( sw as {arms,default,arg,info }) =
			(case arg of 
			     Prim_e (NilPrimOp (inject {tagcount, sumtype}), clist, elist) =>
				 let (* val _ = print "Switching on a value \n" *)
				     fun loop [] = ( case default of 
					       NONE => Switch_e (Sumsw_e sw)
					     | SOME e => (inc_click switch_click; e )) (* Replace with default *)
				       | loop ((t, Function(_,_,[],vcs,[],reducedexp, con)) :: rest ) = 
					 if ( TilWord32.equal ( sumtype, t) ) then 
					     if ( TilWord32.ult(t, tagcount))
						 then (inc_click switch_click;
						       reducedexp)
					     else
						 let val [ (var, con) ] = vcs
						 in
						     (inc_click switch_click ; 
						      insert_sigma var arg ; xexp reducedexp )
						 end
					 else loop rest
				 in 
				     loop arms
				 end 
			   | _ => ( (* print "Can't opt switch, arg is "; Ppnil.pp_exp arg ;print "\n"; *) 
				   Switch_e (Sumsw_e sw)) )
		in 
		    case s of
			Intsw_e sw => do_sw intopt xexp id sw
		      | Sumsw_e sw =>  do_sw sumopt xexp id sw

		      (* No code yet to optimize these two cases *)
		      | Exncase_e sw =>  (do_sw (fn x=> (Switch_e (Exncase_e x))) xexp xexp sw) 
		      | Typecase_e sw =>  (do_sw (fn x=> (Switch_e (Typecase_e x))) id id sw)
		end
	
	    and xexp exp = 
		( case exp of 
			  (* Record creation *)
			  Let_e ( Sequential,
				 ( Exp_b ( x, con, Prim_e (NilPrimOp (record labels), cons, exps)) :: bnds ), N ) =>
			  let val N' = Let_e ( Sequential, bnds, N)
			     
			      fun dec (Var_e a) =  update count_esc (s(a)) ~1
				| dec _ = ()
				  
			  in  
			      if ( dead_var x ) 
				  then ( app dec exps ; xexp N' )
			      else
				  let val _ = HashTable.insert bind ( x, R ( labels,  exps))
				      val N' = xexp N'
				  in
				      if (dead_var x)
					  then ( app dec exps ; N' )
				      else
					Let_e (Sequential,
					       [Exp_b ( x, con, Prim_e (NilPrimOp (record labels), cons, map subst exps) )], N')
				end
			end

		  (* Record Projection *)
		  | Let_e ( Sequential, ( Exp_b ( x, con, Prim_e (NilPrimOp (select label), 
								  cons, [ Var_e a ] )) :: bnds ), N) =>
			let val N' = Let_e ( Sequential, bnds, N)
			   
			in
			    if ( dead_var x ) then
				( update count_esc (s(a)) ~1 ; xexp N' )
			    else
				( case (s(a)) of
				      Var_e a => 
					(case ( HashTable.find bind a)  of
					     SOME ( R(labels, exps )) =>
						 let val _ = inc_click select_click 
						     val temp = Listops.zip labels exps
						     val b = ( case Listops.assoc_eq(Name.eq_label, label, temp ) of 
							      SOME v => v
							    | NONE => raise BUG )
						 in
						     ( replace x b ; update count_esc (Var_e a) ~1 ; xexp N' )
						
						 end
					     (* Change to project_sum_record *)
					   | SOME (S (r, {tagcount, sumtype}, sum_cons))  =>
						 let val _ = inc_click sum_record_click
						   						     
						     val _ = update count_esc (Var_e r) 1 
						     val _ = update count_esc (Var_e a) ~1
						 in 
						     Let_e ( Sequential,
							    [ Exp_b ( x, con, 
							     Prim_e ( (NilPrimOp 
								       (project_sum_record 
								      {tagcount=tagcount, sumtype=sumtype, field=label})),   
								     sum_cons, [ (s r) ]))], xexp N' )
						 end 
					   | _ =>  let val N' = xexp N'
						   in 
						       if (dead_var x) then 
							   ( update count_esc (s(a)) ~1 ;  N' )
						       else
							   Let_e (Sequential, 
								  [Exp_b ( x, con, Prim_e(NilPrimOp (select label),
											  cons, [ s(a) ]))], N')
						   end)
				  | _ => raise BUG )
			end
	      (* Sum projection ... perhaps it was from a record *)
		  | Let_e ( Sequential, ( Exp_b ( x, con, Prim_e (NilPrimOp (project_sum sum), 
									     sum_cons, [ Var_e a ] )) :: bnds ), N) =>
			   let val N' = Let_e ( Sequential, bnds, N)
			   in  
			       if ( dead_var x ) 
				   then ( update count_esc (s(a)) ~1 ; xexp N' )
			       else
				   let 
				       val _ = HashTable.insert bind ( x, S (a, sum, sum_cons))
				       val N' = xexp N'
				   in
				       if (dead_var x)
					   then (  update count_esc (s(a)) ~1  ; N' )
				       else
					   Let_e (Sequential, [Exp_b ( x, con, Prim_e (NilPrimOp (project_sum sum), 
										       sum_cons, [(s a)] ))], N')
				   end 
			   end 
		  (* Variables *)
		  | Let_e(Sequential,  ( Exp_b (x, con, Var_e v)  :: bnds ), N) =>
		    let val N' = Let_e ( Sequential, bnds, N)
		    in
			   ( inc_click var_click ; replace x (Var_e v); update count_esc (s(v)) ~1 ; xexp N')
		    end

		  (* Constants and other binding cases *)
		  | Let_e(Sequential,  ( Exp_b (x, con, exp)  :: bnds ), N) =>
			( case exp of 
			    Const_e c => 
				let val N' = Let_e ( Sequential, bnds, N)
				in
				    if (dead_var x) then xexp N' 
				    else
					( replace x (xexp exp) ; xexp N')
				end
			  | _ => 
				Let_e (Sequential, [Exp_b (x, con, xexp exp)],
				       xexp (Let_e (Sequential, bnds,N))) )
		  | Let_e ( Sequential,  ( (bnd as Fixopen_b vcset)  :: bnds ), N) =>
			let val N' = Let_e ( Sequential, bnds, N)
			    val vclist = Util.set2list vcset
			    val vars = map #1 vclist
			    fun possible_func  v  =
				look count_app v = 1 andalso look count_esc v = 0 
			    fun remove_func 
				( vc as ( v, Function(eff, _, vklist, vclist, vlist, exp, con))) =  
				 census_exp ( ~1, exp) 
			    fun recur_func ( v, Function(eff, a, vklist, vclist, vlist, exp, con)) = 
				( v, Function(eff, a, vklist, vclist, vlist, xexp exp, con))
			    (* Drop unused arguments from functions......
            fun drop_arg ( v, Function ( eff, a, vklist, vclist, vlist, exp, con)) = 
				let
				    fun loop ( (v,c) :: rest, x, removed ) = 
				             if look count_app v = 0 andalso look count_esc v = 0 
						 then (loop (rest, x+1, removed))
						      
					     else let val ( rest, rem ) = (loop (rest, x+1, removed))
						  in 
						      ( v :: rest, rem) 
						  end
				      | loop ( [], x, removed ) = ([], removed)
				    fun  loop2 (  v :: rest, x, removed ) = 
				             if look count_app v = 0 andalso look count_esc v = 0 
						 then (loop2 (rest, x+1, removed))
						      
					     else let val ( rest, rem ) = (loop2 (rest, x+1, removed))
						  in 
						      ( v :: rest, rem) 
						  end
				      | loop2 ( [], x, removed ) = ([], removed)
				    val (newvclist, vcrem) = loop ( map #1 vclist, []) 
					

					 *)
			      
			in
			    if VarSet.member ( (!recset) ,(#1 (hd vclist))) then
				if (dead_vars vars) then
				    ( census_bnds (~1, [Fixopen_b vcset])   ; xexp N')
				else
				    ( (* print "Recursive funcs: " ; map Ppnil.pp_var (map #1 vclist) ; print "\n"; *)
				     Let_e( Sequential,  [Fixopen_b (list2set (map recur_func vclist))], xexp N'))
			    else
				if dead_vars vars then 
				    ( map remove_func vclist ; xexp N')
				else
				    let 		
					val possible = VarSet.addList (VarSet.empty,(List.filter possible_func vars))
					val _ = app  (fn (v, f) => HashTable.insert bind (v, F f)) vclist
					val N' = xexp N'
					fun final (vc as (v:var,c:function)) = 
					    if (VarSet.member (possible,v) ) then
						case HashTable.find bind v of
						    SOME INLINED  =>   false 
						  | _ => ( remove_func vc ; false)
					    else
						case HashTable.find bind v of
						    SOME INLINED => false
						  | _ =>( if dead_var v then (remove_func vc; false)  else true )
						       
					val vclist' = map recur_func (List.filter final vclist)
					    
				    in 
					if null vclist' then N'
					else 
					    Let_e( Sequential, [Fixopen_b ( list2set  vclist')], N')
				    end
			end

			
		  | Let_e(Sequential, [], N) => xexp N

		  | Let_e(Sequential, ( Con_b (x, kind, con) :: bnds ), N) =>
			let val con = (case (NilUtil.strip_singleton kind) of
					   Record_k seq => if (null (sequence2list seq))
							       then Crecord_c []
							   else con
					 | _ => con)
			in  Let_e (Sequential, [ Con_b (x, kind, con)] , 
				   xexp (Let_e (Sequential, bnds, N)))
			end
		  | Let_e(Sequential, _ , _ ) => raise UNIMP (* Fixcode and Fixclosure *)
			
		  | Let_e (Parallel, _ , _) => raise UNIMP (* Try to get it from the old version, if we have to *)
		
		  (* Functions *)
		  (* Constant Folding -- Do it on the vay back up, instead of the way down.... *)
		
		    | Prim_e ( ap , clist, elist ) =>
			  let val elist' = map xexp elist
			      val exp' = Prim_e (ap, clist, elist')
			      fun isValue exp = case exp of Const_e v => true | _ => false
			  in
			      if Listops.andfold NilEval.exp_isval elist' andalso null clist
				  then ( (* print "Constant folding : " ; Ppnil.pp_exp exp' ; print "\n" ;  *)
					let val NilEval.VALUE(result) =  NilEval.eval_exp exp'
					in 
					    case result of 
						Prim_e (ap, clist, elist) => result
					      | _ => (inc_click fold_click; result)
					end
					    handle 
					    Div => (print "Caught Div exn\n"; exp' )
					  | Overflow => (print "Caught Overflow exn\n"; exp' )
					  | (UNIMP | Util.BUG _)=> (Ppnil.pp_exp exp' ; print " can't be folded: nileval UNIMP\n";
							     exp'))
				      
			      else 
				  case exp' of 
				      (* Unroll a  Roll *)
				      Prim_e ( NilPrimOp (unroll), [] , [ Prim_e (NilPrimOp (roll), clist2, [exp] ) ] ) =>
					  exp
					  (* box an unbox *)
				| Prim_e ( NilPrimOp (box_float (sz1)), [], 
						      [ Prim_e (NilPrimOp (unbox_float(sz2)), [], [ exp ] ) ] ) =>
				  if sz1 = sz2
				      then exp
				  else exp'
				      (* unbox a box *)
				| Prim_e ( NilPrimOp (unbox_float (sz1)), [], 
						      [ Prim_e (NilPrimOp (box_float(sz2)), [], [ exp ] ) ] ) =>
				  if sz1 = sz2
				      then exp
				  else exp'
				      (* inject a known record into a faster version *)
				| Prim_e ( NilPrimOp (inject info), [], 
					  [ Var_e v ] ) =>
				  ( case ( HashTable.find bind v)  of
					SOME ( R(labels, exps ) ) =>
					    Prim_e ( NilPrimOp (inject_record info), [], exps) 
				      | NONE =>  exp' )  
					  
				 | _ => exp' 
				      
			  end

		    (* Application, look for possible inlining *)
		  | App_e (openness, Var_e f, clist, elist, elist2) =>
			let (* val _ = print "app_e case, these are the args:";
			val _ = app Ppnil.pp_exp elist;
                        val _ = print "\n"; *)
			
			val new_app = App_e (openness, s(f), clist,
					     map subst elist, map subst elist2)
			(* val _ = (print "S(f) is "; Ppnil.pp_exp (s(f)) ; print "\n") *)
			val Var_e sf = s(f)
		    in 
			if look count_app sf = 1 andalso look count_esc sf = 0 
			    then 
				( case (HashTable.find bind sf) of
				      SOME (F (Function( effect, recur, vklist, vclist, vlist, exp, con))) =>
					  let val _ = inc_click inline_click
					      fun do_args (arg,  x ) =
					      case arg of
						  Var_e a =>
						      ( insert_sigma x (s(a)); 
						       update count_app (s(a)) (look count_app x)  ;
						       update count_esc (s(a)) ((look count_esc x) - 1 ) )
						| Const_e a => insert_sigma x arg
						| _ => raise BUG
					  in
					      (Listops.map2 do_args (elist, map #1 vclist) ; (* Regular arguments *)
					       Listops.map2 do_args (elist2, vlist) ;        (* Floating point *)
					       

					       update count_app (s(f)) ~1 ;
					       HashTable.insert bind (sf, INLINED) ;
					       (* Type arguments *)
					       let fun var_assoc ( x, (a, b) :: rest ) =
						        if Name.eq_var (x, a) then SOME b
							else var_assoc( x, rest)
						     | var_assoc (x, [] ) = NONE
						   val table = NilSubst.fromList (Listops.zip  (map #1 vklist) clist)					     
					       in
						   NilSubst.substConInExp table (xexp exp) 
					       end )
					  end
				    | NONE => new_app 
					 (*  if VarSet.member ( (!recset) , sf ) then new_app
					  else 
					      (Ppnil.pp_var sf; 
					       print " can't be inlined, though it is not recursive and used only once \n"; 
					       new_app) *)
				    | _ => new_app )
				      
			else
			    new_app
			end
	      | App_e ( _, _, _, _, _) => raise BUG
	      | Var_e v => s(v)
	      | Const_e c => exp
	      | Raise_e (exp, con) => Raise_e (xexp exp, con)
	      | Handle_e (exp,fcn) => 
		    let val Function(openness, eff, vklist, vclist, vlist, body, con) = fcn
		    in
			Handle_e (xexp exp, 
				  Function(openness, eff, vklist, vclist, vlist, xexp body, con))
		    end 
	      | Switch_e (sw) => (xswitch sw)
	      (* | _=> exp *)  ) 
			  
	end (* local for ncontract *)
        
       	(* Body of doModule *)
    
	(* In order that we don't reduce away expressions that need to
	 be exported, we put them in a record and create a Let
	 expression around it with the bindings of the module. After
	 reduction, we walk the code to take the record out and get
	 the bindings back. *)

	val exps = List.mapPartial (fn exp => case exp of 
				    ExportValue (label, e, con) => SOME e
				  | _ => NONE ) exports
	val typeexps = List.filter (fn exp => case exp of 
					    ExportType  e => true 
					  | _ => false) exports
	val cons = List.mapPartial (fn exp => case exp of
				    ExportValue (label, var, con) => SOME con
				  | _ => NONE ) exports
	val labels = List.mapPartial (fn exp => case exp of 
				      ExportValue (label, var, con) => SOME label
				    | _ => NONE ) exports
	val body = Prim_e (NilPrimOp (record labels), cons, exps)
		    
	val exp = Let_e (Sequential, bnds, body)
		
	(* Step 1, compute initial values of the tables. As the code
	 is reduced these values should be maintained. *)

	fun clearTable table = 
	    HashTable.appi ( fn (key, item) => ignore (HashTable.remove  table key)) table

	val _ = clearTable count_app
	val _ = clearTable count_esc
	val _ = clearTable bind 
	val _ = recset := VarSet.empty

	val _ =  census_exp(1, exp)
	
	(* val _ = HashTable.appi 
	    (fn ( var, count ) =>
	     if !count = 1 andalso look count_esc var = 0
		 then ( Ppnil.pp_var var ; print " can be inlined\n") 
	     else ()) count_app *)
 

	(* Now loop through the code until, we only do x reductions. *)
	val x = 0
	fun loop ( i, exp ) =
	    if ( i <= x ) then exp 
	    else 
		let 
		    val  exp' = xexp exp
		in 
		    (( if debug then 
			  (  print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
			   print "ITERATION\n";
			   print_round_clicks();
			   Ppnil.pp_exp exp';print "\n";
			   print_stats() )
		      else 
			  print_round_clicks()) ; 
			   loop ( (round_clicks()) , exp' ))
		end 
	val done = (Squish.squish (loop (x+1, exp)))

(*	val _ = (print "Bound things were : \n" ; 
		 HashTable.mapi (fn (var, bnd) => 
				 case bnd of 
				 R _ => (Ppnil.pp_var var; print " record \n")
			       | F _ =>  (Ppnil.pp_var var; print " function \n")
			       | INLINED => (Ppnil.pp_var var; print " claims to be inlined\n")) bind) *)
	val ( newbnds, newexports ) = 
	    case done of 
		Let_e(Sequential, newbnds, Prim_e (NilPrimOp (record labels), cons, exps)) =>
		    (newbnds, typeexps @ (Listops.map3 (fn (label, exp, con) => 
							ExportValue(label, exp, con))
					  (labels,exps, cons)))
	      | Prim_e (NilPrimOp (record labels), cons, exps) => 
			( [],  typeexps @ (Listops.map3 (fn (label, exp, con) => 
							 ExportValue(label, exp, con))
					   (labels,exps, cons)))
	      | _ => raise BUG
	
	in 
			MODULE { bnds= newbnds,
				imports=imports, exports=newexports}   
	end (* do_it *)
end (* struct *)


		    

