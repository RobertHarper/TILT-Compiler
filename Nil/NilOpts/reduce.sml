(* This doesn't handle Fixclosure, Fixcode or Parallel lets *)


functor Reduce  (
                  structure Nil : NIL
		  structure Ppnil : PPNIL
		  structure PrimUtil : PRIMUTIL
		  structure NilEval : NILEVAL
		  structure NilUtil : NILUTIL
		  structure NilSubst : NILSUBST
		  structure Squish : SQUISH
		  sharing Nil = Squish.Nil = NilEval.Nil = NilUtil.Nil = Ppnil.Nil
		      sharing type Squish.Nil.exp = Nil.exp
			      sharing type PrimUtil.con = Nil.con
				  sharing type PrimUtil.exp = Nil.exp

		    sharing type Nil.Prim.prim = PrimUtil.Prim.prim
			    sharing type Nil.exp = NilSubst.exp
				     sharing type Nil.con = NilSubst.con
			) : PASS =

struct

    val do_inline = NilOpts.do_inline_once
    val do_project_known = NilOpts.do_project_known
    val do_dead = NilOpts.do_dead

    open Nil Name Util Nil.Prim
    val error = fn s => Util.error "reduce.sml" s
    structure Nil = Nil

    exception FnNotFound
    exception BUG

    datatype body = EXP of exp | EXPORTS of export_entry list

    val filter = List.filter
    (* First, stuff to keep track of how many reductions have been done. *)

    val select_con_click = NilOpts.make_click "Selects from known con records"
    val select_click = NilOpts.make_click "Selects from known exp records"
    (* How many selections from know records have been replaced *)
    val con_inline_click = NilOpts.make_click "Constructor Functions inlined"
    val inline_click = NilOpts.make_click "Functions inlined (used only once)"
    (* How many functions called only once inlined *)
    val dead_click = NilOpts.make_click "Dead variables eliminated"
    (* How many dead variables have  been removed *)
    val fold_click = NilOpts.make_click "Constants folded"
    (* How many constants have been folded *)
    val switch_click = NilOpts.make_click "Known switches eliminated"
    (* How many known switches have been eliminated *)
    val var_click = NilOpts.make_click "Vars propagated"
    val sum_record_click = NilOpts.make_click "Project_sums to project_sum_records"
    val drop_click = NilOpts.make_click "Unused args dropped"

    val clicks = [ select_click, select_con_click, con_inline_click, inline_click, var_click,
		  dead_click, fold_click, switch_click, sum_record_click, drop_click ]

    fun round_clicks unit = NilOpts.round_clicks clicks
    fun print_round_clicks unit  = NilOpts.print_round_clicks clicks
    val inc_click  = NilOpts.inc_click


    fun small_con con =
	case con of
	    Var_c v => true
	  | Prim_c (primcon, clist) => length clist = 0
	  | _ => false


    fun doModule debug (module as MODULE{bnds=bnds, imports=imports, exports=exports}) =
	let

	    (* Now, tables to keep information about the code as
	     we walk it *)


	    (* Remembers  functions and records to be inlined or
	     projected from later *)
	    datatype bind = F of function | R of label list * exp list | INLINED
	      | S of var * {tagcount: w32, sumtype  : w32} * con list
	      (* Constructor functions and records *)
	      | FC of ((var * kind) list * con * kind)
	      | RC of label list * con list

	    val bind =  Name.mk_var_hash_table(200,FnNotFound):
		(var, bind) HashTable.hash_table


	    (* Set of functions that we have dropped the arguments of *)
	    val drop_table =  Name.mk_var_hash_table(200,FnNotFound):
		(var, (bool list * bool list) ) HashTable.hash_table


	    (* Maps variables to new variables or to constants, for
	     substituting one value for another *)
	    val sigma =  Name.mk_var_hash_table(200,FnNotFound):
		(var, exp) HashTable.hash_table

	    val sigma_c = Name.mk_var_hash_table(200,FnNotFound):
		(var, con) HashTable.hash_table

	    fun insert sigma x v =
		HashTable.insert sigma (x, v)


	    fun s a  =
		case HashTable.find sigma a of
		    SOME x => x
		  | NONE => Var_e a
	    fun sc a  =
		case HashTable.find sigma_c a of
		    SOME x => x
		  | NONE => Var_c a

	    (* Counts the number of times a variable appears in the
	     function position *)
	    val count_app =
		 Name.mk_var_hash_table(200,FnNotFound): (var, (int ref))
		 HashTable.hash_table

	    (* Counts the number of times a variable appears as an
	     argument *)
	    val count_esc = Name.mk_var_hash_table(200,FnNotFound):
		 (var, (int ref)) HashTable.hash_table


	    (* Counts the number of times a variable appears in a recursive call or esc
	     (only valid for function names) *)
	    val count_rec = Name.mk_var_hash_table(200,FnNotFound):
		 (var, (int ref)) HashTable.hash_table

	    val total_set = ref VarSet.empty  (* Set of functions encountered which are total *)

	    fun update_count t (Var_e x) y fset =
		((let  val t = if VarSet.member(fset, x) then  count_rec else t
		       val (cell:int ref) = HashTable.lookup t x
		  in
		      cell := (!cell) + y
		  end) handle FnNotFound => ())
	      | update_count t x y fset = ()

	    fun zero t (Var_e x) fset =
		((let val t = if VarSet.member(fset, x) then  count_rec else t
		      val (cell:int ref) = HashTable.lookup t x
		  in
		      cell := 0
		  end)
		      handle FnNotFound => ())
	      | zero t x fset = ()

	    fun update_count_c t (Var_c x) y fset =
		((let  val t = if VarSet.member(fset, x) then  count_rec else t
		       val (cell:int ref) = HashTable.lookup t x
		  in
		      cell := (!cell) + y
		  end) handle FnNotFound => ())
	      | update_count_c t x y fset = ()

	    fun zero_c t (Var_c x) fset =
		((let val t = if VarSet.member(fset, x) then  count_rec else t
		      val (cell:int ref) = HashTable.lookup t x
		  in
		      cell := 0
		  end)
		      handle FnNotFound => ())
	      | zero_c t x fset = ()

	    fun look t a =
	        ! (HashTable.lookup t a)
		handle FnNotFound =>
		    (* Anything that we can't find is not declared in this file, so we can't
		     inline it or anything. So always return 1 *)
		    ( (* print "Can't find " ; Ppnil.pp_var a ; print "\n" ; *) 1 )

	    fun replace x exp fset =
		( (case exp of
		     Var_e b =>
			 ( insert sigma x (s(b)) ;
			  update_count count_app (s(b)) (look count_app x) fset;
			  update_count count_esc (s(b)) (look count_esc x) fset )
		   | Const_e b =>  (* Some constant expression *)
			 insert sigma x exp) ;
		       zero count_app (Var_e x) fset;
		       zero count_esc (Var_e x) fset)

	    fun replace_c x exp fset =
		( (case exp of
		     Var_c b =>
			 ( insert sigma_c x (sc(b)) ;
			  update_count_c count_app (sc(b)) (look count_app x) fset;
			  update_count_c count_esc (sc(b)) (look count_esc x) fset )
		   | _ =>  (* Some constant expression *)
			 insert sigma_c x exp) ;
		       zero_c count_app (Var_c x) fset;
		       zero_c count_esc (Var_c x) fset)

	    (* The following functions implement the census algorithm,
	     which determines for each variable how many times it is
	     used or escapes. Unlike Appel's version, this one doesn't
	     count recursive calls/uses of functions *)



	    local
		val delta = ref 1  (* What should we change
				    the value by usually +1 or -1 *)
		fun inc x =  x:= (!x) + (!delta)

		fun declare x =  if not ( isSome (HashTable.find count_esc x))
				     then
					 ( HashTable.insert count_esc(x,ref 0) ;
					  HashTable.insert count_app(x, ref 0);
					  HashTable.insert count_rec(x,ref 0))
				 else ()

		fun insesc fset x =
		    let  val y = s(x)
		    in
			case y of
			    Var_e y =>
				let val table = if VarSet.member (fset, y) then count_rec else count_esc
				in
				    ( case (HashTable.find table y) of
					  NONE =>  ()
					| SOME use =>   inc use )
				end
			  | _ => ()
		    end

		fun insapp fset x  =
		    let  val  y = s(x)
		    in
			case y of
			    Var_e y =>
				let val table = if VarSet.member (fset, y) then count_rec else count_app
				in
				    ( case (HashTable.find table y ) of
					  NONE => ()
					| SOME use => inc use )
				end
			  |  _  => () (* Constant expression *)
		    end


		fun insesc_c fset x  =
		    let  val y = sc(x)
		    in
			case y of
			    Var_c y =>
				( case (HashTable.find count_esc y) of
				      NONE =>  ()
				    | SOME use =>   inc use )
			  | _ => ()
		    end

		fun insapp_c fset x  =
		    let  val  y = sc(x)
		    in
			case y of
			    Var_c y =>
				( case (HashTable.find count_app y ) of
					  NONE => ()
					| SOME use => inc use )
			  |  _  => () (* Constant expression *)
		    end


		fun scan_kind fset kind =
		    case kind of
			Type_k _ => ()
		      | Word_k _ => ()
		      | Singleton_k (p, k, con) =>
			    (scan_kind fset k ; scan_con fset con)
		      | Record_k lkseq =>
			Util.appsequence (fn (lv, k) =>
					  scan_kind fset k) lkseq
		      | Arrow_k (_, vklist, k) =>
			    ( app (fn (v,k) => scan_kind fset k) vklist ; scan_kind fset k)


		and scan_con fset con =
		    let
			val insesc = insesc_c fset
			val insapp = insapp_c fset
		    in
			case con of
			    Prim_c (primcon, cons) => app (scan_con fset) cons
			  | Mu_c (bool, vcset, var) =>
				let val fset = VarSet.addList (fset, map #1 (Util.sequence2list vcset))
				in
				    Util.appsequence (fn (var, con) => scan_con fset con) vcset
				end
			  | AllArrow_c (openness, effect, vklist, clist, w32, con) =>
				(app ((scan_kind fset) o #2) vklist;
				 app (scan_con fset) clist;
				 (scan_con fset) con )
			  | Var_c v => insesc v
			  | Let_c (sort, conbnds, con) =>
				(app (scan_conbnd fset) conbnds ; (scan_con fset) con)
			  | Crecord_c (lclist) =>
				(app ((scan_con fset) o #2) lclist)
			  | Proj_c (con, label) => (scan_con fset) con
			  | App_c (con, cons) => ( case con of
						  Var_c v => insapp v
						| _ => scan_con fset con ; app (scan_con fset) cons)
			  | Typecase_c { arg=arg, arms = arms, default = default, kind = kind} =>
				((scan_con fset) arg ; (scan_con fset) default ;
				 scan_kind fset kind ;
				 app (fn (primcon, vklist, con) =>
				      ( app ((scan_kind fset) o #2) vklist;
				       (scan_con fset) con)) arms )
			  | Annotate_c (annot,con) => (scan_con fset) con
			  | Closure_c (con1, con2) => raise UNIMP
		    end

		and scan_conbnd fset conbnd =
		    case conbnd of
			Con_cb (var, kind, con) => (declare var; scan_kind fset kind ; scan_con fset con)
		      | Open_cb (var, vklist, con, kind) =>
			    (declare var; app (fn (v,k) => (declare v; scan_kind fset k)) vklist; scan_con fset con; scan_kind fset kind)
		      | Code_cb _ =>raise UNIMP


		fun scan_exp fset exp  =
		    let val scan_con = scan_con fset
			val scan_exp = scan_exp fset
			val scan_bnd = scan_bnd fset
			val insesc = insesc fset
			val insapp = insapp fset
		    in
		    case exp of
			(Var_e v) => insesc v
		      | (Const_e v) =>
			  (case v
			     of Prim.array (con, arr) =>
			       (
				scan_con con;
				Array.app scan_exp arr
				)
			      | Prim.vector (con, arr) =>
			       (
				scan_con con;
				Array.app scan_exp arr
				)
			      | Prim.refcell (ref r) => scan_exp r
			      | Prim.tag (t,con) => scan_con con
			      | _ => ())
		      | (Let_e (_, bndlist , body)) => ( app scan_bnd bndlist; scan_exp body )
		 (*     | (Prim_e (NilPrimOp (select label), clist, [ Var_e r ] )) =>
			    ( insapp r ; app scan_con clist )           *)
		      | (Prim_e ( allp, clist, elist)) =>
			    ( app scan_con clist; app scan_exp elist)
		      | (Switch_e switch) => (scan_switch fset switch)
		      | (App_e ( openness, efunc, clist, elist, eflist)) =>
			    ( case efunc of
				  Var_e v => insapp v
				| _ => scan_exp efunc ;
				      app scan_con clist;
				      app scan_exp elist;
				      app scan_exp eflist)
		      | (Raise_e (e, c)) => (scan_exp e; scan_con c)
		      | (Handle_e (e, function)) =>
			    (scan_exp e; scan_function fset function)
		    end

		and scan_switch fset s =
		    let val scan_exp = scan_exp fset
			val scan_con = scan_con fset
			fun nada x = ()
			fun do_sw do_info do_arg do_t {arms,default,arg,info } =
			    ( app (fn (t,function) =>
				   (do_t t; scan_function fset function)) arms ;
			     do_info info;
			     do_arg arg;
			     case default of
				 SOME exp => scan_exp exp
			       | NONE => () )
		    in
			case s of
			    Intsw_e sw => do_sw nada scan_exp nada sw
			  | Sumsw_e sw => do_sw (fn (w32, cons) => app scan_con cons)  scan_exp nada sw
			  | Exncase_e sw => do_sw nada scan_exp scan_exp sw
			  | Typecase_e sw => do_sw nada scan_con nada sw
		    end
		and scan_function fset (Function(_,_,cvarlist,varlist,fvarlist,exp,con)) =
		    let fun dec (v,k) = (declare v; scan_kind fset k)
		    in  ( app dec cvarlist;
			 app (fn (v, con) => ignore (declare v, scan_con fset con))  varlist;
			 app declare fvarlist;
			 scan_exp fset exp;
			 scan_con fset con )
		    end

		and scan_bnd fset bnd =

		    case bnd of
			(Con_b (v, k, con) ) => (declare v; scan_kind fset k; scan_con fset con)
		      | (Exp_b (v, c, exp)) => ( case NilUtil.strip_arrow c of
						SOME (_, Total, _, _, _, _) =>
						    total_set := VarSet.add (!total_set, v)
					      | _ => () ;
						    declare v;
						    scan_con fset c;
						    scan_exp fset exp)
		      | (Fixopen_b vcset | Fixcode_b vcset) =>
			    let val vflist = Util.set2list vcset
				val newfset = VarSet.addList (fset, map #1 vflist)
				fun dec (v,c) = declare v
				val _ = app dec vflist
			    in
				app (fn (v,function as Function(effect,_,cvarlist,varlist,fvarlist,exp,_)) =>
				     (if effect = Total
					  then total_set := VarSet.add (!total_set, v)
				      else () ;
					  scan_function newfset function)) vflist

			    end
		      |  Fixclosure_b vcset => raise UNIMP
	    in
		fun census_exp fset ( x , exp ) =
		    ( delta := x ;
		     scan_exp fset exp)
		fun census_con fset ( x , con ) =
		    ( delta := x ;
		     scan_con fset con)
		fun census_kind fset (x, kind ) =
		    (delta := x;
		     scan_kind fset kind)

		fun census_bnds fset ( x, bnds ) =
		    ( delta := x;
		     map (fn x=> scan_bnd fset x) bnds )
		fun census_module (x , MODULE{imports=imports, bnds=bnds, exports=exports} ) =
		    ( delta := x;
		     map (fn x=> scan_bnd VarSet.empty x) bnds;
		     map (fn x=>
			  case x of
			      ExportValue(label, exp,con) =>
				  (scan_exp VarSet.empty exp;scan_con VarSet.empty con)
			    | ExportType (label, con, kind) =>
				  (scan_con VarSet.empty con; scan_kind VarSet.empty kind)) exports)

	    end (* Census functions *)

	fun print_table str = fn (var, use)=>
	    (if not ((!use) = 0) then
		( print str;  Ppnil.pp_var var; print " : "; print (Int.toString (!use)); print "\n")
		 else ())
	val print_exp = fn (var, newexp) =>
	    ( print "exp var "; Ppnil.pp_var var; print " maps to "; Ppnil.pp_exp newexp; print "\n")

	val print_con = fn (var, newvar) =>
	    ( print "con var "; Ppnil.pp_var var; print " maps to "; Ppnil.pp_con newvar; print "\n")

	fun print_stats unit =
	    ( print "****************************************************\n";
	     print "APP TABLE\n";
	     HashTable.appi (print_table "app ") count_app;
	     print "\nESC TABLE\n";
	     HashTable.appi (print_table "esc ") count_esc;
	     print "\nREC TABLE\n";
	     HashTable.appi (print_table "rec ")count_rec;
	     print "****************************************************\n";
	     print "\nExp var substitution\n";
	     HashTable.appi print_exp sigma;
	     print "\nCon var substitution\n";
	     HashTable.appi print_con sigma_c;
	     print "****************************************************\n"
	     )


	    (* The following functions implement the ncontract algorithm *)

	local
	    fun dead_var x =
		if (!do_dead andalso look count_app x = 0 andalso look count_esc x = 0 andalso look count_rec x = 0)
		    then ( if debug then ( Ppnil.pp_var x; print " is dead\n" ) else () ;
			  inc_click dead_click ; true )
		else false

	    fun dead_funcs xs =
		let fun dead x = (!do_dead andalso look count_app x = 0 andalso look count_esc x = 0) (* Don't count recursive apps of funcs *)
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

	    fun subst_c con =
		case con of
		    Var_c v => sc(v)
		  | _  => con

	    fun is_pure_allp (NilPrimOp p) = true
	      | is_pure_allp (PrimOp p) =
		case p of
		    ( mk_ref | setref | open_in
		  | input | input1 | lookahead | open_out | close_in
		  | output | flush_out | close_out | end_of_stream ) => false
		  | ( update _  | create_table _ ) => false
		  | ( deref | sub _ ) => true
		  | _ => true



	    fun is_pure exp =
		case exp of
		    Var_e _ => true
		  | Const_e _ => true
		  | App_e (_, Var_e f, _,_,_) =>
		        VarSet.member (!total_set, f) (* We don't have to check args because of A-normal form *)
		  | App_e _ => (print "WARNING: reduce.sml found App not in A-normal form";
				false)
		  | Prim_e (allp, _, _) =>
			is_pure_allp allp
		  | Let_e (_, bnds, exp) =>
			let fun is_pure_bnd (Exp_b (v,c,exp)) = is_pure exp
			    |  is_pure_bnd _ = true
			in
			    ( Listops.andfold is_pure_bnd bnds)
			    andalso is_pure exp
			end
		  | Raise_e _ => false
		  | Switch_e sw => false (* Punt on this for now *)
		  | Handle_e (exp, fcn) => is_pure exp

		(* ----- Have to recur on the kind here as well ----------------- *)
	in  fun xcon_project fset (t :var, kind:kind, (Var_c a):con, label:label) binder do_body do_kind =

	    let fun cleanup unit =
		(update_count_c count_esc (sc(a)) ~1 fset;
		 census_kind fset (~1, kind) )
	    in
		if (dead_var t) then
		    ( cleanup(); (do_body()))
		else
		    case sc(a) of
			Var_c a =>
			    (case HashTable.find bind a of
				 SOME ( RC (labels, cons) ) =>
				     let val _ = inc_click select_con_click
					 val temp = Listops.zip labels cons
					 val SOME b = ( Listops.assoc_eq(Name.eq_label, label, temp ) )

				     in
				     ( replace_c t b fset; cleanup(); do_body() )
				     end
			       | _ =>  let
					   val N' = (do_body())
				       in
					   if (dead_var t ) then
					       ( cleanup() ;  N' )
					   else
					       binder (t, do_kind fset kind, Proj_c (Var_c a, label), N')
				       end)
		      | _=> raise BUG
	    end
	    | xcon_project fset (t :var, kind:kind, c:con, label:label) binder do_body do_kind =
	    (print "Warning: xcon_project in reduce not anormal: given con =\n";
	     Ppnil.pp_con c; print "\n";
	     binder (t, do_kind fset kind, Proj_c (c, label), do_body()))

	    and xcon_record fset (x, kind, lclist) binder do_body do_kind =
		 let val (labels, cons) = Listops.unzip lclist
		     val cons = map subst_c cons
		     val lclist = Listops.zip labels cons
		     fun dec (Var_c a) =  update_count_c count_esc (sc(a)) ~1 fset
		       | dec _ = ()
		     fun cleanup unit =
			 ( app dec cons; census_kind fset (~1, kind) )
		 in
		     if (dead_var x)
			 then ( cleanup()  ; do_body() )
		     else
			 let val _ = if !do_project_known then HashTable.insert bind ( x, RC ( labels,  cons)) else ()
			      val N' = do_body()
			 in
			     if (dead_var x)
				 then ( cleanup()  ; N' )
			     else
				 binder (x, do_kind fset kind, Crecord_c lclist, N')
			 end
		 end
	    fun xcon_else fset (x, kind, con) binder do_body do_kind do_con =
		 if (small_con con) then
		     (replace_c x con fset;
		      census_kind fset (~1, kind);
		      (case con of
			   Var_c v => update_count_c count_esc (sc(v)) ~1 fset
			 | _ => ());
		      do_body ())
		 else
		     if (dead_var x) then
			 (census_kind fset (~1, kind);
			  census_con fset(~1, con);
			  do_body ())
		     else
			 let val N' = do_body ()
			 in
			     if (dead_var x) then
				 (census_kind fset (~1, kind);
				  census_con fset (~1, con);
				  N')
			     else
				 binder (x, do_kind fset kind, do_con fset con, N')
			 end

	    fun xkind fset kind =
		case kind of
		    Type_k p => kind
		  | Word_k p => kind
		  | Singleton_k (p, k, c) =>
			Singleton_k (p, xkind fset k, xcon fset c)
		  | Record_k lvkseq =>
			Record_k ( Util.mapsequence (fn ((l,v), kind) =>
					  ((l,v), xkind fset kind)) lvkseq)
		  | Arrow_k (openn, vklist, k) =>
			Arrow_k (openn, map (fn (v,k) => (v, xkind fset k)) vklist, xkind fset k)

	    and xcon fset con =
		case con of
		    (* Record Projection *)
		    Let_c (Sequential, ( Con_cb (t, kind, Proj_c (con, label)):: rest ), body ) =>
			let fun do_body unit = xcon fset (Let_c (Sequential, rest, body))
			    fun bind (t, kind, con, body) = Let_c (Sequential, [ Con_cb (t, kind, con)], body)
			in
			    xcon_project fset (t, kind, con, label) bind do_body xkind
			end
		  (* Record creation *)
		  | Let_c (Sequential, ( Con_cb (t, kind, Crecord_c lclist) :: rest ), body) =>
			   let fun do_body unit = xcon fset (Let_c (Sequential, rest, body))
			       fun bind (t, kind, con, body) = Let_c (Sequential, [ Con_cb (t, kind, con)], body)
			   in
			       xcon_record fset (t, kind, lclist) bind do_body xkind
			   end

		  (* Variables and other bindings *)
		  | Let_c (Sequential, (Con_cb (x, kind, con) :: rest), N) =>
			let fun do_body unit = xcon fset (Let_c (Sequential, rest, N))
			    fun bind (x,kind, con, body) = Let_c (Sequential, [ Con_cb (x,kind, con)], body)
			in
			    xcon_else fset (x, kind, con) bind do_body xkind xcon
			end


		  (* Function definitions *)
		  | Let_c (Sequential, (Open_cb (x, vklist, con, kind)) :: rest, N) =>
			let val N' = Let_c ( Sequential, rest, N)
			    fun cleanup unit =
				( app (fn (v,k) => census_kind fset (~1, k)) vklist;
				 census_con fset ( ~1, con);
				 census_kind fset (~1, kind) )
			   in if (dead_var x) then
			       (cleanup (); xcon fset N')
			      else
				  let val _ = HashTable.insert bind (x, FC (vklist, con, kind))
				      val N' = xcon fset N'
				  in case (HashTable.find bind x) of
				      (*
				       If the function was inlined, only the return kind and the kinds on the args have gone
				       away. The con was inlined.
				       *)
				      SOME INLINED =>
					  (app (fn (v,k) => census_kind fset (~1, k)) vklist;
					   census_kind fset (~1, kind);
					   N')
				    | _ => if (dead_var x) then (cleanup () ; N')
					   else let val newvklist = map (fn (v,k) => (v, xkind fset k)) vklist
						    val newcon =  xcon fset con
						    val newkind = xkind fset kind
						in
						    Let_c (Sequential, [ (Open_cb (x, newvklist, newcon, newkind)) ], N')
						end
				  end
			end

		  | Let_c (Sequential, [], N) => xcon fset N
		  | Let_c _ => raise UNIMP

		  (* Function application *)
		  | App_c (Let_c(letsort,bnds,Var_c v),cons) =>
			xcon fset (Let_c(letsort,bnds,App_c(Var_c v, cons)))

		  (* Function application *)
		  | App_c ( Var_c f, cons) =>
			let val new_app = App_c (sc(f), map (xcon fset) cons)
			    val Var_c sf = sc(f)
			in
			    if !do_inline andalso look count_app sf = 1 andalso look count_esc sf = 0
				then ( case (HashTable.find bind sf) of
				      SOME (FC ( vklist, con, kind)) =>
					  let val _ = inc_click con_inline_click
					      fun do_args (arg,  x ) =
						  case arg of
						      Var_c a =>
							  ( insert sigma_c x (sc(a));
							   update_count_c count_app (sc(a)) (look count_app x)  fset ;
							   update_count_c count_esc (sc(a)) ((look count_esc x) - 1 ) fset)
						    | _ => insert sigma_c x arg

					  in ( Listops.map2 do_args (cons, map #1 vklist);
					      update_count_c count_app (sc(f)) ~1 fset;
					      HashTable.insert bind (sf, INLINED);
					      xcon fset con)
					  end
				    | NONE => new_app )
			    else new_app
			end
		  (* unnamed application *)
		  | App_c (f, cons) =>
			(print "WARNING: reduce.sml: App_c with non-lambda/non-varaible\n";
			 App_c (xcon fset f, map (xcon fset) cons))
		  | Var_c var => sc(var)
		  | Prim_c (primcon, cons) =>
			Prim_c (primcon, map (xcon fset) cons)
		  | Mu_c (bool, vcseq, var) =>
			let val fset = VarSet.addList (fset, (map #1 (Util.sequence2list vcseq)))
			in
			    Mu_c (bool, Util.mapsequence (fn (v, c) => (v, xcon fset c)) vcseq, var)
			end
		  | AllArrow_c ( openness, effect, vklist, cons, w32, con) =>
			AllArrow_c ( openness, effect, (map (fn (v,k) => (v, xkind fset k)) vklist), map (xcon fset) cons, w32, xcon fset con)
		  | Crecord_c (lclist) =>
			Crecord_c ( map (fn  (l,c) => (l, xcon fset c) ) lclist)
		  | Proj_c (con, label) => Proj_c (xcon fset con, label)
		  | Closure_c _ => raise UNIMP
		  | Annotate_c (a,con) => Annotate_c (a, xcon fset con)
		  | Typecase_c { arg=arg, arms=arms, default=default, kind= kind} =>
		        Typecase_c { arg=xcon fset arg,
			 default = xcon fset default,
			 kind = xkind fset kind,
			 arms = map (fn (primc, vklist, con)=> (primc, (map (fn (v,k)=> (v,xkind fset k))vklist) , xcon fset con)) arms}



	    fun xswitch fset s =
		let fun id _ x   = x
		    fun do_sum_info fset (w32, cons) =
			(w32, map (xcon fset) cons)
		    fun do_sw constr do_arg do_t do_info {arms,default,arg,info } =
			let val newinfo = do_info fset info
			    val newarg = do_arg fset arg
			    val newarms = map (fn (t,function) =>
					       (do_t fset t, xfunction fset function)) arms
			    val newdefault = case default of
				SOME exp => SOME (xexp fset exp)
			      | NONE => NONE
			in constr {arms = newarms, default = newdefault, arg = newarg, info= newinfo}
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
						      insert sigma var arg ; xexp fset reducedexp )
						 end
					 else loop rest
				 in
				     loop arms
				 end
			   | _ => ( (* print "Can't opt switch, arg is "; Ppnil.pp_exp arg ;print "\n"; *)
				   Switch_e (Sumsw_e sw)) )
		in
		    case s of
			Intsw_e sw => do_sw intopt xexp id id  sw
		      | Sumsw_e sw =>  do_sw sumopt xexp id do_sum_info sw

		      (* No code yet to optimize these two cases *)
		      | Exncase_e sw =>  (do_sw (fn x => (Switch_e (Exncase_e x))) xexp xexp id sw)
		      | Typecase_e sw =>  (do_sw (fn x => (Switch_e (Typecase_e x))) xcon id id sw)
		end

	    and xfunction fset (Function( eff, recu, vklist, vclist, vlist, exp, con)) =
		let val vklist = map (fn (v,k) => (v, xkind fset k)) vklist
		    val vclist = map (fn (v,con) => (v, xcon fset con)) vclist
		    val exp = xexp fset exp
		    val con = xcon fset con
		in
		    Function( eff, recu, vklist, vclist, vlist, exp, con)
		end

	    and xbnds fset ( (bnd :: rest) : bnd list )
		( body : body ) =
		( case bnd of
 		      (* --------------------- Constructor bindings ----------------------- *)
		      (* Constructor Record Projection *)
		      Con_b (t, kind, Proj_c (con, label)) =>
			  let fun do_body unit = xbnds fset rest body
			      fun bind (t, kind, con, (rest,body) ) = (Con_b (t, kind, con)::rest, body)
			  in
			      xcon_project fset (t, kind, con, label) bind do_body xkind
			end
		    (* Constructor Record Creation *)
		    | Con_b (t, kind, Crecord_c lclist) =>
			  let fun do_body unit = xbnds fset rest body
			      fun bind (t, kind, con, (rest,body) ) = (Con_b (t, kind, con)::rest, body)
			  in
			      xcon_record fset (t, kind, lclist) bind do_body xkind
			  end


		    (* --------------- Constructor function creation --------------- *)
		    | Con_b (t, kind, conbnd as (Let_c (Sequential, [ Open_cb ( var, vklist, con, retkind) ], Var_c var2))) =>
			  if (dead_var t) then
			      (census_kind fset (~1, kind); census_con fset (~1, conbnd); xbnds fset rest body)
			  else
			      ( if Name.eq_var (var, var2) then
				    let val _ = HashTable.insert bind (t, FC(vklist, con, retkind))
					val (rest,body) = xbnds fset rest body
				    in ( case (HashTable.find bind t) of
					SOME INLINED =>
					    (app (fn (v,k) => census_kind fset (~1, k)) vklist;
					     census_kind fset (~1, retkind);
					     update_count_c count_esc (sc(var2)) ~1 fset;
					     (rest, body) )
				      | _ =>  if (dead_var t) then
					    (census_kind fset (~1, kind); census_con fset (~1, conbnd); (rest, body) )
					      else
						  let val newcon = xcon fset con
						      val newretkind = xkind fset retkind
						      val newkind = xkind fset kind
						  in
						      ( Con_b (t, newkind, Let_c (Sequential,
										  [Open_cb (var, vklist, newcon, newretkind)], Var_c var))::rest, body)
						  end )
				    end
				else
				    raise BUG)
		  (* This constructor function is dead anyways. I doubt this case will come up very often *)
		  (* xbnds fset rest body *)

		    (* Constructor  Variables and other bindings *)
		    | Con_b (x, kind, con) =>
			  let fun do_body unit = xbnds fset rest body
			      fun bind (x, kind, con, (rest,body) ) = (Con_b (x, kind, con)::rest, body)
			  in
			      xcon_else fset (x, kind, con) bind do_body xkind xcon
			  end



		  (* --------------------- Term bindings ---------------------------- *)
		  (* Term Record creation *)
		  | Exp_b ( x, con, Prim_e (NilPrimOp (record labels), cons, exps )) =>
			let fun dec (Var_e a) =  update_count count_esc (s(a)) ~1 fset
			      | dec _ = ()

			  in
			      if ( dead_var x )
				  then ( app dec exps;
					census_con fset(~1, con);
					app (fn (con) => (census_con fset (~1, con))) cons;
				        xbnds fset rest body )

			      else
				  let val _ = if !do_project_known then HashTable.insert bind ( x, R ( labels, exps)) else ()
				      val (rest,body) = xbnds fset rest body
				  in
				      if (dead_var x )
					  then ( app dec exps ;
						census_con fset (~1, con);
						app (fn (con) => (census_con fset (~1, con))) cons;
						(rest, body) )
				      else
					  ( Exp_b ( x, xcon fset con,
						   Prim_e (NilPrimOp (record labels),
							   map (xcon fset)cons, map subst exps)) :: rest, body )
				  end
		      end

	  (* ------------------------ Term Record Projection --------------------- *)
	  | Exp_b ( x, con, Prim_e (NilPrimOp (select label), cons, [ Var_e a ] )) =>
		let fun cleanup unit =
		     ( update_count count_esc (s(a)) ~1 fset;
                     census_con fset (~1, con);
                     app (fn (con) => (census_con fset (~1, con))) cons)
		in
		if ( dead_var x ) then
		    ( cleanup ();
		     xbnds fset rest body )
		else
		     ( case (s(a)) of
			   Var_e a =>
			       (case ( HashTable.find bind a)  of
				    (* Found known record *)
				    SOME ( R(labels, exps )) =>
					let val _ = inc_click select_click
					    val temp = Listops.zip labels exps
					    val b = ( case Listops.assoc_eq(Name.eq_label, label, temp ) of
						     SOME v => v
						   | NONE => raise BUG )

					in
					    ( replace x b fset;
					     cleanup ();
					     xbnds fset rest body )
					end
				  (* Can change to project_sum_record *)
				  | SOME (S (r, {tagcount, sumtype}, sum_cons))  =>
					let val _ = inc_click sum_record_click
					    val _ = update_count count_esc (Var_e r) 1 fset
					    val _ = update_count count_esc (Var_e a) ~1 fset
					    val _ = map (fn (con) => (census_con fset (1, con))) sum_cons
					    val (rest, body) = xbnds fset rest body
					in
					    (( Exp_b ( x, xcon fset con,
						     Prim_e ( (NilPrimOp
							       (project_sum_record
								{tagcount=tagcount, sumtype=sumtype, field=label})),
							     map (xcon fset) sum_cons, [ (s r) ])) :: rest), body)
					end
				  (* Must leave it alone *)
				  | _ =>  let val (rest,body) = xbnds fset rest body
					  in
					      if (dead_var x ) then
						  (cleanup ();
						   (rest,body) )
					      else
						 ( Exp_b ( x, xcon fset con, Prim_e(NilPrimOp (select label),
										    map (xcon fset) cons, [ s(a) ]))::rest, body)
					  end)
			 | _ => raise BUG )
		end
		   (* ------------- Sum projection ... perhaps it was from a record ------------ *)
		   | Exp_b ( x, con, Prim_e (NilPrimOp (project_sum sum), sum_cons, [ Var_e a ] )) =>
			 let fun cleanup unit =
			     ( census_con fset (~1, con);
			     update_count count_esc (s(a)) ~1 fset;
			     app (fn (con) => (census_con fset (~1, con))) sum_cons )
			in
			     if ( dead_var x )
			     then (cleanup();
				   xbnds fset rest body)
			 else
			     let
				 val _ = if !do_project_known then HashTable.insert bind ( x, S (a, sum, sum_cons)) else ()
				 val (rest,body) = xbnds fset rest body
			     in
				 if (dead_var x )
				     then (cleanup();
					   (rest,body) )
				 else
				     ( Exp_b ( x, xcon fset con, Prim_e (NilPrimOp (project_sum sum),
									 map (xcon fset) sum_cons, [(s a)]))::rest, body)
			     end
			 end
		    (* ----------------- Variables --------- *)
		    | Exp_b (x, con, Var_e v) =>
			  ( inc_click var_click ;
			   replace x (Var_e v) fset;
			   update_count count_esc (s(v)) ~1 fset;
			   census_con fset (~1, con);
			   xbnds fset rest body )

		    (* ----------------- Constants  ----------------- *)
		    | Exp_b (x, con, Const_e c)  =>
			( if (dead_var x ) then () else replace x (Const_e c) fset;
			  census_con fset (~1,con );
			  xbnds fset rest body )


		  (* ----------------- Function bindings ----------------- *)
		  | (bnd as Fixopen_b vcset) =>
			let val vclist = Util.set2list vcset
			    val vars = map #1 vclist
			    fun possible_func  v  =
				!do_inline andalso look count_app v = 1 andalso look count_esc v = 0

				(* Even when we inline the function, the cons & kinds on the args and the return con
				   go away, so we need to update the counts for them *)

			    fun remove_rest
				( vc as ( v, Function(eff, _, vklist, vclist, vlist, exp, con))) =
				(app (fn (var, kind) => census_kind fset (~1, kind)) vklist;
				 app (fn (var,con) => census_con fset (~1, con)) vclist;
				 census_con fset (~1, con))
			    fun remove_func
				( vc as ( v, Function(eff, _, vklist, vclist, vlist, exp, con))) =
				( census_exp fset ( ~1, exp);
				 remove_rest vc
				 )
			    fun recur_func ( v, function) =
				let val newfset = VarSet.addList (fset, vars)
				in ( v, xfunction newfset function)
				end
			    (* Drop unused arguments from functions that don't escape *)
			    (* fun drop_arg ( v, Function ( eff, a, vklist, vclist, vlist, exp, con)) =
				if look count_esc v = 0 then
				    let fun used v =
					look count_app v > 0 orelse look count_esc v > 0 orelse look count_rec v > 0

					val vclist_mask = map (fn (v, c) => used v) vclist
					val vclist = map #1 (filter #2 (Listops.zip vclist vclist_mask))
					val vlist_mask = map used vlist
					val vlist = map #1 (filter #2 (Listops.zip vlist vlist_mask))
					val _ = if not  ( (Listops.andfold (fn x=>x) vclist_mask) andalso
							 (Listops.andfold (fn x=>x) vlist_mask) )
						    then  ( inc_click drop_click ;
							   HashTable.insert drop_table (v, (vclist_mask, vlist_mask) ))
						else ()
				    in
					(v, Function ( eff, a, vklist, vclist, vlist, exp, con))
				    end
				else (v, Function ( eff, a, vklist, vclist, vlist, exp, con)) *)
			    (* val vclist = map drop_arg vclist *)
			in
			    (* If any of the  functions are recursive, we won't inline them, just check to see
			       if they are dead *)
			    if (Listops.orfold (fn v => (look count_rec v > 0)) vars ) then
				if (dead_funcs vars) then
				    ( census_bnds fset (~1, [Fixopen_b vcset])   ; xbnds fset rest body)
				else
				    let val (rest,body) = xbnds fset rest body
				    in
				    (Fixopen_b (list2set (map recur_func vclist))::rest, body)
				    end

			    else
				if dead_funcs vars  then
				    ( map remove_func vclist ; xbnds fset rest body )
				else
				    let
					val _ = app  (fn (v, f) => HashTable.insert bind (v, F f)) vclist
					val (rest,body) = xbnds fset rest body
					fun final (vc as (v:var,c:function)) =
					    case HashTable.find bind v of
						SOME INLINED => (remove_rest vc ; false )
					      | _ =>( if dead_var v then (remove_func vc; false)  else true )

					val vclist' = map recur_func (List.filter final vclist)

				    in
					if null vclist' then (rest,body)
					else
					   ( Fixopen_b ( list2set  vclist') :: rest, body)
				    end
			end

		  (* ----------- Other expression bindings ----------- *)
		  | Exp_b (x, con, exp)=>
			if is_pure exp
			    then
				if (dead_var x ) then (census_exp fset (~1, exp) ; census_con fset (~1, con); xbnds fset rest body)
				else
				    let val (rest,body) = xbnds fset rest body
				    in
					if (dead_var x ) then (census_exp fset (~1, exp);census_con fset (~1, con); (rest,body))
					else
					    ( Exp_b (x, xcon fset con, xexp fset exp) :: rest, body)
				    end
			else let val (rest, body) = xbnds fset rest body
			     in
				 (Exp_b (x, xcon fset con, xexp fset exp):: rest, body)
			     end )


	      | xbnds fset [] body =
		( case body of
		    EXP exp =>
			( [] , EXP (xexp fset exp) )
		  | EXPORTS entrys =>
			let fun do_entry ( ExportValue ( label, exp, con) ) =
			        ExportValue (label, xexp fset exp, xcon fset con)
			      | do_entry ( ExportType ( label, con, kind )) =
				ExportType (label, xcon fset con, xkind fset kind )
			in ( [], EXPORTS (map do_entry entrys) )
			end )



	    and xexp fset exp =
		( case exp of

		      Let_e(Sequential, bnds , body ) =>
			  let val (bnds, EXP body) = xbnds fset bnds (EXP body)
			  in Let_e (Sequential, bnds, body)
			  end

		    | Let_e (Parallel, _ , _) => raise UNIMP

		  | Prim_e ( ap , clist, elist ) =>
			let val clist' = map (xcon fset) clist
			    val elist' = map (xexp fset) elist
			    val exp' = Prim_e (ap, clist', elist')
			    fun isValue exp = case exp of Const_e v => true | _ => false
			in
			      if !NilOpts.do_fold_constants andalso Listops.andfold NilEval.exp_isval elist' andalso null clist
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
				      Prim_e ( NilPrimOp (unroll), con , [ Prim_e (NilPrimOp (roll), clist2, [exp] ) ] ) =>
					  (inc_click fold_click; exp)
					  (* box an unbox *)
				| Prim_e ( NilPrimOp (box_float (sz1)), [],
						      [ Prim_e (NilPrimOp (unbox_float(sz2)), [], [ exp ] ) ] ) =>
				  if sz1 = sz2
				      then (inc_click fold_click; exp)
				  else exp'
				      (* unbox a box *)
				| Prim_e ( NilPrimOp (unbox_float (sz1)), [],
						      [ Prim_e (NilPrimOp (box_float(sz2)), [], [ exp ] ) ] ) =>
				  if sz1 = sz2
				      then (inc_click fold_click; exp)
				  else exp'
				      (* inject a known record into a faster version *)
				| Prim_e ( NilPrimOp (inject info), [],
					  [ Var_e v ] ) =>
				  ( case ( HashTable.find bind v)  of
					SOME ( R(labels, exps ) ) =>
					    (inc_click fold_click; Prim_e ( NilPrimOp (inject_record info), [], exps) )
				      | NONE =>  exp' )

				 | _ => exp'

			  end

		    (* Application, look for possible inlining *)
		  | App_e (openness, Var_e f, clist, elist, elist2) =>
			let (* Drop unused fcn args *)
			    (* val (elist, elist2) = case HashTable.find drop_table f of
				SOME (elist_mask, elist2_mask) =>
				    let fun foo (v,b) = (if b then true
							 else ( update_count count_esc (subst v) ~1 fset; false ))
				    in
					( map #1 (filter foo (Listops.zip elist elist_mask)),
					 map #1 (filter foo  (Listops.zip elist2 elist2_mask)) )
				    end
			      | NONE => (elist, elist2)
				*)
			val new_app = App_e (openness, s(f), map (xcon fset) clist,
					     map subst elist, map subst elist2)
			(* val _ = (print "S(f) is "; Ppnil.pp_exp (s(f)) ; print "\n") *)
			val Var_e sf = s(f)
		    in
			if !do_inline andalso look count_app sf = 1 andalso look count_esc sf = 0
			    then
				( case (HashTable.find bind sf) of
				      SOME (F (Function( effect, recur, vklist, vclist, vlist, exp, con))) =>
					  let val _ = inc_click inline_click
					      fun do_args (arg,  x ) =
					      case arg of
						  Var_e a =>
						      ( insert sigma x (s(a));
						       update_count count_app (s(a)) (look count_app x)  fset ;
						       update_count count_esc (s(a)) ((look count_esc x) - 1 ) fset)
						| Const_e a => insert sigma x arg
						| _ => raise BUG
					      fun do_cargs (arg, x) =
						  case arg of
						  Var_c a =>
						      ( insert sigma_c x (sc(a));
						       update_count_c count_app (sc(a)) (look count_app x)  fset ;
						       update_count_c count_esc (sc(a)) ((look count_esc x) - 1 ) fset)
						| _ => insert sigma_c x arg
					  in
					      (Listops.map2 do_args (elist, map #1 vclist) ; (* Regular arguments *)
					       Listops.map2 do_args (elist2, vlist) ;        (* Floating point *)
					       Listops.map2 do_cargs (clist, map #1 vklist) ;(* Constructor arguments *)

					       update_count count_app (s(f)) ~1 fset;
					       HashTable.insert bind (sf, INLINED) ;

					       xexp fset exp )
					  end
				    | SOME _ => new_app
				    | NONE => new_app )
			else
			    new_app
			end
	      | App_e ( _, _, _, _, _) =>
			(print "WARNING: reduce.sml found app not in A-normal form\n";
			 exp)
	      | Var_e v => s(v)
	      | Const_e c =>
			Const_e (case c
			   of Prim.array (con, arr) => error "Arrays shouldn't show up ever"
			    | Prim.vector (con, arr) =>
			     let
			       val con = xcon fset con
			       val arr = Array.tabulate (Array.length arr,fn i => xexp fset (Array.sub(arr,i)))
			     in
			       Prim.vector (con, arr)
			     end
			    | Prim.refcell (ref r) => error "Refs shouldn't show up ever"
			    | Prim.tag (t,con) => error "Tags shouldn't show up ever"
			    | _ => c)
	      | Raise_e (exp, con) => Raise_e (xexp fset exp, xcon fset con)
	      | Handle_e (exp,fcn) =>
		    Handle_e (xexp fset exp, xfunction fset fcn)
	      | Switch_e (sw) => (xswitch fset sw)
	      (* | _=> exp *)  )

	end (* local for ncontract *)

       	(* Body of doModule *)

	(* In order that we don't reduce away expressions that need to
	 be exported, we put them in a record and create a Let
	 expression around it with the bindings of the module. After
	 reduction, we walk the code to take the record out and get
	 the bindings back.

	 Unfortunately this doesn't work as well for exported constructors.
	 *)

	(* Step 1, compute initial values of the tables. As the code
	 is reduced these values should be maintained. *)

	fun clearTable table =
	    HashTable.appi ( fn (key, item) => ignore (HashTable.remove table key)) table

	val _ = clearTable count_app
	val _ = clearTable count_esc
	val _ = clearTable count_rec
	val _ = total_set := VarSet.empty
	val _ =  census_module(1, module)

	val _ = if debug then print_stats() else ()

	(* Now loop through the code until, we only do x reductions. *)
	val x = 0
	fun loop ( i:int , (m as (bnds, EXPORTS entrys): (bnd list * body) )) : (bnd list * body) =
	    if ( i <= x ) then m
	    else
		let val _ = clearTable drop_table
		    val _ = clearTable bind
		    val (m as (bnds, EXPORTS entrys)) = xbnds VarSet.empty bnds (EXPORTS entrys)
		in
		    (( if debug then
			  (  print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
			   print "ITERATION\n";
			   print_round_clicks();
			   app Ppnil.pp_bnd bnds; print "\n";
			   print_stats() )
		      else
			  print_round_clicks()) ;
			   loop ( (round_clicks()) , m ))
		end
	val (newbnds,( EXPORTS newexports)) = (loop (x+1, (bnds, EXPORTS exports)))
	in
	    MODULE { bnds= newbnds,
		    imports=imports, exports=newexports}
	end (* do_it *)
end (* struct *)




