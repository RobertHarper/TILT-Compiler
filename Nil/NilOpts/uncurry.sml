functor Uncurry (structure Nil : NIL
		 structure Ppnil : PPNIL
		 sharing type Nil.exp = Ppnil.Nil.exp
		     ) : PASS =

struct
    open Nil
    structure Nil = Nil

    exception FnNotFound
    exception UNIMP

    (* Stores the arity of the function,  ... *)
    val curried =  Name.mk_var_hash_table(200,FnNotFound):
	(var, (int * var * (( var * con) list *  exp * con) * function )) HashTable.hash_table

    val partial_app = Name.mk_var_hash_table(200,FnNotFound):
	(var, (var * exp list * int )) HashTable.hash_table

    val uncurry_click = NilOpts.make_click "Functions uncurried"

    (* Phase 1 : analyze, identifying curried functions and their applications *)
    fun scan_exp exp =
	case exp of
	    (Var_e v) => ()
	  | (Const_e v) => ()
	  | (Prim_e ( allp, clist, elist)) =>
		( app scan_exp elist)
	  | (Switch_e switch) => (scan_switch switch)
	  | Let_e (_, ( Exp_b ( a, con, App_e ( openness, Var_e g, [], args, [])) :: rest), body) =>
		(* Record partial applications *)
		( case HashTable.find partial_app g of
		      SOME (f, l, n) => HashTable.insert partial_app (a, (f, l @ ((Var_e g) :: args), n+1))
		    | NONE => HashTable.insert partial_app ( a, (g, args, 1)) ;
			  scan_exp (Let_e(Sequential, rest, body)) )
	  | (App_e ( openness, Var_e g, clist, elist, elist2 )) =>
	         ( app scan_exp elist ; app scan_exp elist2)
	  | (App_e ( openness, e, clist, elist, elist2 )) =>
	         (print "WARNING: uncurry.sml encountered unnamed form\n";
		  scan_exp e;  app scan_exp elist ; app scan_exp elist2)
	  | (Let_e (_, (bnd :: rest) , body)) => ( scan_bnd bnd;  scan_exp (Let_e (Sequential, rest, body)))
	  | Let_e (_, [], body) => scan_exp body

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

    (* Args - Function,
     newname: name of new uncurried function
     newargs: renamed arguments for the function call

     Returns
     arity of function
     ( arguments, exp, con ) to create uncurried function
     new defn of curried function which calls the uncurried one.
     *)
    and scan_fcn (f,  (Function(effect,recursive,[],args,[],f_exp,con))) (newname:var) (newargs:var list) =
	let val f_arity = 1
	    val f_con = AllArrow_c (Open, effect, [], map #2 args, Word32.fromInt 0, con)   (* type of f *)
	    val f_args = (f,f_con) :: args
	    val new= ( f_args, f_exp, con)
	    val renamed_args = map (fn (v, c) => (Name.fresh_var(), c)) args
	    val oldbody =
		App_e ( Open, Var_e newname, [], tl ((map Var_e newargs) @ ((Var_e f) :: (map (Var_e o #1) renamed_args))), [])
	    val func =  Function(effect, recursive, [], renamed_args, [], oldbody, con)
	    val returnexp = Let_e ( Sequential, [Fixopen_b (Util.list2set [ (f, func) ] )], Var_e f )
	    val default = ( f_arity, new, returnexp )
	in
	    case f_exp of
		Let_e (_, [ Fixopen_b vcset ], Var_e f1') =>
		    ( case Util.set2list vcset of
			  [ (f1, func as Function(_,_,[],_,[],exp2,_))]=>
			      if Name.eq_var (f1', f1) then
				  let
				      val (arity, (args1, e, c), body) =
					  scan_fcn (f1, func) newname (newargs @ (f :: (map #1 renamed_args)))
				      (* Need to change the return type if it is now a total function *)
				      val con = (  case body of
						 Let_e  (_, [ Fixopen_b vcset ], Var_e f1') =>
						     ( case Util.set2list vcset of
							   [ (f1, func as Function(Total,_,[],_,[],exp2,_))] =>
							       ( case con of
								     AllArrow_c (openness, _, vklist, clist, w32, con2) =>
									 AllArrow_c (openness, Total, vklist, clist, w32, con2)
								   | _ => con )
							 | _ => con )
					       | _ => con)
				      val func = Function(Total, recursive, [], renamed_args, [], body, con)
				      val returnexp = Let_e ( Sequential, [ Fixopen_b (Util.list2set [ (f, func) ] )],
							     Var_e f)
				  in (f_arity + arity, (( ((f,f_con) :: args)  @ args1), e, c),  returnexp)
				  end
			      else
				  (scan_exp f_exp; scan_exp exp2 ; default)
			|  vflist =>
			       ( app (fn (v, Function(_,_,_,_,_,exp,_)) => scan_exp exp) vflist ;
				scan_exp f_exp ; default))
	      | _ => (scan_exp f_exp; default)
	end


    and scan_bnd bnd =
	case bnd of
	    (Con_b (v, k, con) ) => ()
	  | (Exp_b (v, c, exp)) => ( scan_exp exp )
	  | Fixopen_b vcset =>
	       app ( fn vfunc =>
		( case vfunc  of
		      (f1, Function(effect,recursive,[],args,[],exp,con))  =>
			  let val newname = Name.fresh_var()
			      val (arity, ( (f1'', f_con) :: args, exp, con), Let_e (Sequential, [ Fixopen_b vfset], Var_e f) ) =
				  scan_fcn vfunc newname []
			      val [ (f1', func) ] = Util.set2list vfset
			  (* f should be the same as f1 and f1'*)
			  in
			      if (arity > 1) then  ( NilOpts.inc_click uncurry_click ;
				  HashTable.insert curried (f1,(arity,  newname, (args,exp,con), func)) )
			      else
				  ()
			  end
		    | (v, Function(_,_,_,_,_,exp,_)) =>
			  scan_exp exp)) (Util.set2list vcset)
	  |  (Fixcode_b _| Fixclosure_b _) => raise UNIMP


    (* Phase 2 : transform, create uncurried versions of functions,
    and replace their applications *)

 (* According to Appel, it is not really necessart to replace the
    applications if you do general inlining. (or Beta-expansion as he
    calls it. As the size of the function depends only on the number
    of arguments uncurried, up to a certain size they will be inlined.

    Unfortunately, applications inside curried recursive functions
    won't change because the recursive call is passed in as an arg and
    can't be inlined. So it is probably a good idea to replace the
    apps. Especially since knowing when to inline is a difficult thing,
    and we always want to inline these.

    In order to handle recursive functions (which Tarditi doesn't do), we
    need to pass in the old functions as args to the new (except for the
    first one). Like Apple we implement unused arg dropping to get rid
    of them later.
 *)


	fun doFunc ( fnVar, func as Function(eff, a, vklist, vclist, vlist, body, con)) =
	    case HashTable.find curried fnVar of
		SOME ( arity, newname, (args, exp, con),func) =>
		       let val uncurried = (newname, Function(eff, a, [], args, [], xexp exp, con))
			   val curried = (fnVar, func)

		       in [ uncurried, curried ]
		       end
	      | NONE => [ (fnVar,
			   Function(eff, a, vklist, vclist, vlist, xexp body, con)) ]


	and xbnd bnd =
	    case bnd of
		Con_b ( v, c, k) => bnd
	      | Exp_b ( v, c, exp) =>
		    Exp_b (v,c, xexp exp)
	      | Fixopen_b vfset =>
		    let val vflist = Util.set2list vfset
			val vflist  = Listops.flatten (map doFunc vflist)
		    in
			Fixopen_b (Util.list2set vflist)
		    end
	      | _ => raise UNIMP


	(* Replace the curried application with uncurried one *)
	and  doApp (app as (openness, Var_e g, [] , args, [] )) =
	    ( case HashTable.find partial_app g of
		  SOME (f, l, num ) =>
		      ( case HashTable.find curried f of
			    SOME (arity, newname, _, _) =>
				( print "Found " ; Ppnil.pp_var g; print " maybe replace with ";
				  Ppnil.pp_var f; print "\n";
				 if arity = num + 1   then
				     (
				      NilOpts.inc_click uncurry_click ;
				      (openness, Var_e newname, [], l @ ((Var_e g) :: args) , []) )
				 else
				     app)
			  | NONE => app)
		| NONE => app)
	   | doApp ( app as (openness, Var_e g,c , args, e)) = app
	  | doApp app = (print "WARNING: uncurry enconutered unnamed form\n"; app)

	and xexp exp =
	    case exp of
		Var_e v => exp
	      | Const_e c => exp
	      | Prim_e (allp, cons, exps) =>
		    Prim_e (allp, cons, map xexp exps)
	      | Switch_e sw =>
		    Switch_e (xswitch sw)
	      | (App_e app) => App_e (doApp app)
	      | Let_e (sort, bnds, exp ) =>
		    let val bnds = map xbnd bnds
		    in 	Let_e (sort, bnds, xexp exp)
		    end
	      | Raise_e (exp, con) => Raise_e (xexp exp, con)
	      | Handle_e (exp, Function(eff, r, vks, vcs, vs, expf, con)) =>
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


   fun doModule debug (MODULE {bnds, imports, exports} ) =
	let

	    (* Clear out the hash tables *)
	    val _ = HashTable.appi ( fn (key, item) => ignore (HashTable.remove partial_app key)) partial_app
	    val _ = HashTable.appi ( fn (key, item) => ignore (HashTable.remove curried key)) curried
	    val temp =  Let_e(Sequential,bnds, Prim_e(NilPrimOp(inject {tagcount=0w2,sumtype=0w1}),[],[])  ) (* true! *)
	    val _ = scan_exp temp
	    (* val _ = ( print "Curried Functions: \n" ;
		     HashTable.appi (fn (key, (arity, newname, _, _)) =>
				     (Ppnil.pp_var key; print ": " ; Ppnil.pp_var newname;
				      print (" " ^ (Int.toString arity) ^ "\n" ))) curried ;
		     print "\n")
	    val _ = ( print "Partially applied Functions: \n" ;
		     HashTable.appi (fn (key, (var, exps, num)) =>
				     (Ppnil.pp_var key; print ": " ; Ppnil.pp_var var; print " ";
				      map Ppnil.pp_exp exps;
				      print "\n" )) partial_app ;
		     print "\n") *)
	    val Let_e(Sequential,bnds,_) = (xexp temp)
	    val imports = imports
	    val exports = exports
	in  MODULE{bnds = bnds,
		   imports = imports,
		   exports = exports}
	end

    end

