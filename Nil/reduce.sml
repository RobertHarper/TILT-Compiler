(*$import Nil NilUtil Ppnil PrimUtil NilSubst REDUCE *) 

(* This doesn't handle Fixclosure, Fixcode or Parallel lets *)

structure Reduce
  :> REDUCE 
  = 

  struct 
    val debug = ref false;
      
    val do_inline = Stats.tt("reduce_do_inline")
    val do_project_known = Stats.tt("reduce_do_project_known")
    val do_dead = Stats.tt("reduce_do_dead")

    open Nil Name Util Prim Listops
    val error = fn s => Util.error "reduce.sml" s

    structure Nil = Nil

    exception FnNotFound
    exception UNIMP

    val exportbnds = ref [] : bnd list ref

    fun nilprim_isval(np,clist,elist) = 
      (case np of
	 record _ => Listops.andfold exp_isval elist
       | inject _ => Listops.andfold exp_isval elist
       | box_float _ => Listops.andfold exp_isval elist
       | roll => Listops.andfold exp_isval elist
       | inj_exn _ => Listops.andfold exp_isval elist
       | _ => false)
(*
	 | ((make_vararg _) | (make_onearg _) | 
	    (select _) | (inject_nonrecord _) | (inject_record _) | (project_sum _) | 
	    (project_sum_nonrecord _ )| (project_sum_record _) | (unbox_float _) 
       | unroll | make_exntag) => false)
*)
	 
    and exp_isval (Var_e v) = false
      | exp_isval (Const_e v) = true
      (*      | exp_isval (Let_e (_,[bnd],Var_e v)) = 
       (case bnd of
	  (Con_b _) => false
       | (Exp_b _) => false
       | (Fixopen_b vf_set) => (member_eq(eq_var,v,Sequence.maptolist #1 vf_set))
       | (Fixcode_b vf_set) => (member_eq(eq_var,v,Sequence.maptolist #1 vf_set))
       | (Fixclosure_b (_,vc_set)) => (member_eq(eq_var,v,Sequence.maptolist #1 vc_set))) *)
      | exp_isval (Let_e _) = false
      | exp_isval (Prim_e (NilPrimOp np,clist,elist)) = nilprim_isval(np,clist,elist)
      | exp_isval (Prim_e (PrimOp _,_,_)) = false
      | exp_isval (Switch_e _) = false
      | exp_isval (App_e _) = false
      | exp_isval (ExternApp_e _ ) = false
      | exp_isval (Raise_e _) = false
      | exp_isval (Handle_e _) = false

   (* CS: Duplicates code in linearize *)
   fun exp_issmall exp =  
       (case exp of 
	    Var_e _ => true
          | Const_e (Prim.int (Prim.W64,_)) => false
          | Const_e (Prim.uint (Prim.W64,_)) => false
          | Const_e (Prim.int _) => true
          | Const_e (Prim.uint _) => true
	  | _ => false)

    (* Since we would like the code to work both on a list of bindings
     in an expression, and the list of bindings in a module, we create
	 a datatype for the "body" of the let binding, i.e. where thebound
	 variables will be used *)

    datatype body = EXP of exp | EXPORTS of export_entry list

    (* First, stuff to keep track of how many reductions have been done. *)

    val max_name_size = ref 10
    type click = { name : string , last : int ref, total : int ref}
    val clicks = ref [] : (click list) ref

    fun fprint max_size str =
	   let fun loop i = if i < 0 then () else (print " "; loop (i-1))
	   in  print str;
	       loop (max_size - (size str))
	   end

    fun make_click str = 
	let val _ = max_name_size := Int.max(size str, !max_name_size)
	    val t = Stats.int(str)
	    val click = { name = str, last = ref 0, total = t  }
	in (  clicks := click :: !clicks ;
	    click )
	end

    fun inc_click { last=r, name=n, total= t } = 
		t := ! t + 1
    fun print_clicks unit = 
	let fun pritem {name = name, last = r, total = count } = 
	    (fprint (!max_name_size) name;
	     print " : ";
	     fprint 8 (Int.toString (!count));
	     print "\n")
	in  
	    print "Optimization results\n";
	    print "-------------------------------------------\n";
	    app pritem (rev(!clicks));
	    print "-------------------------------------------\n"
	end 
 
    fun init_clicks unit =
	app (fn {last = r, total = t, name=str } =>
	     ( t:= 0 ; r := 0) ) (!clicks)
 
    fun round_clicks clicks = 
	foldl  (fn ({last = r, total = t, ... }:click, acc:int) => 
	       ( let val temp = !t - (!r)
		 in ( r := !t ; temp+acc ) end )) 0 clicks 
    fun print_round_clicks clicks = 
	app (fn {name = n, last = r, total = t } =>
	     print (n ^ " :" ^ Int.toString ((!t)-(!r)) ^ "\n") ) clicks

(*    val select_con_click = make_click "Selects from known con records" *)
    val select_click = make_click "Selects from known exp records" 
    (* How many selections from known records have been replaced *)
(*    val con_inline_click = make_click "Constructor Functions inlined" *)
    val inline_click = make_click "Functions inlined (used only once)" 
    (* How many functions called only once inlined *)
    val dead_click = make_click "Dead variables eliminated"
    (* How many dead variables have  been removed *)
    val fold_click = make_click "Constants folded"        
    (* How many constants have been folded *)
(*    val switch_click = make_click "Known switches eliminated"  *)   
    (* How many known switches have been eliminated *)
    val var_click = make_click "Vars propagated"
(*    val sum_record_click = make_click "Project_sums to project_sum_records"
    val drop_click = make_click "Unused args dropped" *)
	
    val clicks = [ select_click, (* select_con_click,  con_inline_click,*) inline_click, var_click,
		  dead_click, fold_click (*, switch_click, sum_record_click, drop_click *) ]
	
    val inc_click  = inc_click
	
   
    fun small_con con =
	case con of 
	    Var_c v => true
	  | Prim_c (primcon, clist) => length clist = 0 
	  | _ => false


    (* Now, tables to keep information about the code as
     we walk it *)
		
	   			
    (* In order to project from known records or inline functions,
     we store their bodies in the following hashtable: *)
		
    datatype bind = F of function | R of label list * exp list | INLINED 
      | S of var * w32 * con list
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
		
    (* 
     
     Hash Table for keeping statistics on the variables
     encountered in the program. The three refs are
     
     1 - app - Counts the number of times a variable appears
	       in the function position 
     2 - esc - Counts the number of times a variable appears
               as an argument 
     3 - rec_app - Counts the number of times a variable appears in
	       a recursive call (only valid for function
	       names)
     4 - rec_esc - Counts the num of times a
               function escapes inside its body
     5 - known - true if letbound, false if arg to fun 
     *)

    type info = (int ref * int ref * int ref * int ref * bool ref)
    datatype which_table = APP | ESC
    val count_app = fn (x,y,z,w,v) => x
    val count_esc = fn (x,y,z,w,v) => y
    val count_rec_app = fn (x,y,z,w,v) => z
    val count_rec_esc = fn (x,y,z,w,v) => w
    val known = fn (x,y,z,w,v) => v
    val count_table =
	Name.mk_var_hash_table(200,FnNotFound):
	(var, info) HashTable.hash_table

    val info_valid = ref false
    fun getInformationTable unit =
      if (!info_valid)
	then (info_valid := false;
	      SOME count_table)
      else
	NONE

    fun print_escape unit = 
      let val (apps, escs, kfs, both) =  HashTable.fold 
	(fn (info, (apps, escs, kfs, both) ) =>
	 let val app = ! (count_app info)
	   val esc = !(count_esc info)
	   val recval = !(count_rec_app info)
	 in 
	   if (app > 0  orelse recval > 0 ) then 
	     (apps+1,
	      if (esc > 0) then 
		escs+1 
	      else
		escs, kfs + (if (!(known info)) then 1 else 0),
		both + (if (!(known info) andalso esc > 0) then 1 else 0))
	   else
	     (apps, escs, kfs, both)
	 end)
	(0,0,0,0) count_table
	  val (apps2, escs2) = HashTable.fold 
	    (fn (info, (apps2, escs2) ) =>
	     let val app = ! (count_app info)
	       val recval = !(count_rec_app info)
	       val k = ! (known info)
	     in 
	       if (app > 0  orelse recval > 0 ) then 
		 (apps2+app + recval,
		  if k then 
		    escs2+app + recval 
		  else
		    escs2)
	       else
		 (apps2, escs2)
	     end)
	    (0,0) count_table
	    
      in 
	print "---------------- Function Information ----------------------\n";
	print ("Total number of vars applied: " ^ (Int.toString apps) ^ "\n");
	print ("           which also escape: " ^ (Int.toString escs) ^ "(" ^ 
	       (Real.toString ((Real.fromInt(escs)/Real.fromInt(apps)) * 100.0)) ^ " %)\n");
	print ("             which are known: " ^ (Int.toString kfs) ^ "\n");
	print ("                    both    : " ^ (Int.toString both) ^ "\n");
	print ("Total number of applications: " ^ (Int.toString apps2) ^ "\n");
	print("              which are known: " ^ (Int.toString  escs2) ^ "\n");
	print "-----------------------------------------------------------\n"
      end 
      

    val total_set = ref VarSet.empty  (* Set of functions encountered which are total *)

   (* Functions for manipulating the data in the hash tables *)
    fun get_table which fset x = 
      if VarSet.member(fset, x)
	then
	  (case which of
	     APP => count_rec_app
	   | ESC => count_rec_esc)
      else 
	case which of
	  APP => count_app
	| ESC => count_esc

    (* *)
    fun update_count 
	which             (* which table to look in *)
	(Var_e x)     (* the variable to look up *)
	(y:int)       (* the amount to change it by *)
	fset =        (* set of recursive functions we are in the scope of *)
	((let  val t = get_table which fset x
	       val (cell:int ref) = t (HashTable.lookup count_table x)
	  in 
	      cell := (!cell) + y
	  end) handle FnNotFound => ())
      | update_count t x y fset = () 
	
    fun zero which (Var_e x) fset = 
	((let val t = get_table which fset x
	      val (cell:int ref) = t (HashTable.lookup count_table x)
	  in 
	      cell := 0
	  end)
	      handle FnNotFound => ())
      | zero t x fset = ()
	
    fun update_count_c which (Var_c x) y fset =
	((let  val t = get_table which fset x
	       val (cell:int ref) = t (HashTable.lookup count_table x)
	  in 
	      cell := (!cell) + y
	  end) handle FnNotFound => ())
      | update_count_c t x y fset = () 
	
    fun zero_c which (Var_c x) fset = 
	((let val t = get_table which fset x
	      val (cell:int ref) = t( HashTable.lookup count_table x)
	  in 
	      cell := 0
	  end)
	      handle FnNotFound => ())
      | zero_c t x fset = ()
	
    fun look t a =
	! (t (HashTable.lookup count_table a))
	handle FnNotFound => 
	    (* Anything that we can't find is not declared in this file, so we can't 
	     inline it or anything. So always return 1 *) 
	    ( (* print "Can't find " ; Ppnil.pp_var a ; print "\n" ; *) 1 ) 
	    
    fun replace x exp fset = 
	( (case exp of 
	       Var_e b => 
		   ( insert sigma x (s(b)) ;
		    update_count APP (s(b)) (look count_app x) fset;
		    update_count ESC (s(b)) (look count_esc x) fset )
	     | _ =>  (* Some constant expression *)
		   insert sigma x exp) ;
	       zero APP (Var_e x) fset;
	       zero ESC (Var_e x) fset)
(*	       
    fun replace_c x exp fset = 
	( (case exp of 
	       Var_c b => 
		   ( insert sigma_c x (sc(b)) ;
		    update_count_c count_app (sc(b)) (look count_app x) fset;
		    update_count_c count_esc (sc(b)) (look count_esc x) fset )
	     | _ =>  (* Some constant expression *)
		   insert sigma_c x exp) ;
	       zero_c APP (Var_c x) fset;
	       zero_c ESC (Var_c x) fset) *)
	       
    (* The following functions implement the census algorithm, which
     determines for each variable how many times it is used or
     escapes. Unlike Appel's version, this one doesn't include
     recursive calls/uses of functions in the counts, but counts them
     separately*)
	       
    local 
	val delta = ref 1  (* What should we change 
			    the value by usually +1 or -1 *)
	fun inc x =  x:= (!x) + (!delta)
	    
	(* b is true for lets, false for fun args *)
	fun declare b x =  
            (case (HashTable.find count_table x ) of
	      NONE => HashTable.insert 
			 count_table (x,(ref 0, ref 0, ref 0, ref 0, ref b))
               | _ => ())
			     
	fun insesc fset x = 
	    let  val y = s(x)
	    in 
		case y of 
		    Var_e y =>
			let val table = if VarSet.member (fset, y) then count_rec_esc
					else count_esc
			in 
			    ( case (HashTable.find count_table y) of
				  NONE =>  () 
				| SOME use =>  inc (table use )) 
			end 
		  | _ => ()
	    end 
	
	fun insapp fset x  =
	    let  val  y = s(x) 
	    in 
		case y of 
		    Var_e y => 
		      let val table = if VarSet.member (fset, y) then count_rec_app 
			  else count_app 
			in 
			    ( case (HashTable.find count_table y ) of
				  NONE => () 
				| SOME use => inc (table use) ) 
			end 
			  |  _  => () (* Constant expression *)
	    end 

			 
	fun insesc_c fset x  = 
	    let  val y = sc(x)
	    in 
		case y of 
		    Var_c y =>
			( case (HashTable.find count_table y) of
			      NONE =>  () 
			    | SOME use =>   inc (count_esc use )) 
		  | _ => ()
	    end 

	fun insapp_c fset x  =
	    let  val  y = sc(x) 
	    in 
		case y of 
		    Var_c y => 
			( case (HashTable.find count_table y ) of
			      NONE => () 
					| SOME use => inc (count_app use )) 
		  |  _  => () (* Constant expression *)
	    end 

	(* ------------------------------------------------------------------ *)
	fun scan_kind fset kind = 
	  case kind of
	    Type_k => ()
	  | SingleType_k con => scan_con fset con
	  | Single_k con => scan_con fset con
	  | Record_k lkseq =>
	      Sequence.app (fn (lv, k) => scan_kind fset k) lkseq
	    | Arrow_k (_, vklist, k) =>
		( app (fn (v,k) => scan_kind fset k) vklist ; scan_kind fset k)


	and scan_con fset con =
	  case con of 
	      Prim_c (primcon, cons) => app (scan_con fset) cons 
	    | Mu_c (bool, vcset) => 
		let val fset = VarSet.addList (fset,(Sequence.maptolist #1 vcset))
		in 
		  Sequence.app (fn (var, con) => scan_con fset con) vcset
		end 
	    | AllArrow_c {tFormals, eFormals, body, ...} => 
		(app ((scan_kind fset) o #2) tFormals;
		 app ((scan_con fset) o #2) eFormals;
		 (scan_con fset) body)
	    | ExternArrow_c (cons, con) =>
		(app (scan_con fset) cons ; scan_con fset con)
	    | Var_c v => ()
	    | Let_c (sort, conbnds, con) => 
		(app (scan_conbnd fset) conbnds ; (scan_con fset) con)
	    | Typeof_c (exp) =>
		scan_exp fset exp
	    | Crecord_c (lclist) =>
		(app ((scan_con fset) o #2) lclist)
	    | Proj_c (con, label) => (scan_con fset) con
	    | Closure_c _ => raise UNIMP
	    | App_c (con, cons) => 
		( scan_con fset con ; app (scan_con fset) cons)
	    | Typecase_c { arg=arg, arms = arms, default = default, kind = kind} =>
		((scan_con fset) arg ; (scan_con fset) default ;
		 scan_kind fset kind ;
		 app (fn (primcon, vklist, con) => 
		      ( app ((scan_kind fset) o #2) vklist;
		       (scan_con fset) con)) arms )
	    | Annotate_c (annot,con) => (scan_con fset) con
		
	and scan_conbnd fset conbnd = 
	  case conbnd of 
	    Con_cb (var, con) => scan_con fset con
	  | Open_cb (var, vklist, con, kind) => 
	      (app (fn (v,k) => (scan_kind fset k)) vklist; 
	       scan_con fset con;
	       scan_kind fset kind)
	  | Code_cb _ =>raise UNIMP
			    
	(* Scan expressions, counting the number of times a variable is used
	 (escapes), or is applied *)
	      
	and scan_exp fset exp  = 
	    let val scan_con = scan_con fset
		val scan_exp = scan_exp fset
		val scan_bnd = scan_bnd fset
		val insesc = insesc fset
		val insapp = insapp fset
	    in 
		case exp of
		    (Var_e v) => insesc v 
		  | (Const_e v) => ()
		  | (Let_e (_, bndlist , body)) => 
			( app scan_bnd bndlist; scan_exp body )
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
		  | (ExternApp_e (efunc, explist)) =>
			( case efunc of 
			      Var_e v => insapp v 
			    | _ => scan_exp efunc ; 
				  app scan_exp explist) 
		  | (Raise_e (e, c)) => (scan_exp e; scan_con c)
		  | (Handle_e (e, v, e2)) =>
			(scan_exp e; insesc v; scan_exp e2)
	    end 
		     
	and scan_switch fset s =
	    let val scan_exp = scan_exp fset
		val scan_con = scan_con fset
		val scan_kind = scan_kind fset
		val ins_esc = insesc fset
	    in
		case s of
		    Intsw_e {arg,size,arms, default} =>
		       ( 
			scan_exp arg;
			map (scan_exp o #2) arms;
			Option.map (scan_exp) default;
			()
			)			
		  | Sumsw_e {arg,sumtype,bound,arms,default} =>
			(
			 scan_exp arg;
			 scan_con sumtype;
			 ins_esc bound;
			 map (scan_exp o #2) arms;
			 Option.map (scan_exp) default;
			 ()
			 )
		  | Exncase_e {arg, bound, arms, default} => 
			(
			 scan_exp arg;
			 ins_esc bound;
			 app (fn x => (scan_exp (#1 x); scan_exp (#2 x))) arms;
			 Option.map (scan_exp) default;
			 ()
			 )
		  | Typecase_e { arg, arms, default } =>
			(
			 scan_con arg;
			 map ( (map ( (declare false) o #1)) o #1) arms;
			 map ( (map (scan_kind o #2)) o #1) arms;
			 map (scan_exp o #2) arms;
			 Option.map (scan_exp) default;
			 ()
			 )
	    end
		

	and scan_function fset (Function{tFormals=cvarlist,eFormals=varlist,
					 fFormals=fvarlist,body=exp,body_type=(_,con), ...}) = 
		    let fun dec (v,k) = (declare false v; scan_kind fset k)
		    in  ( 
			 app dec cvarlist;
			 app (fn (v, tr, con) =>
			      ignore (declare false v, scan_con fset con))  varlist;
			 app (declare false) fvarlist; 
			 scan_exp fset exp;
			 scan_con fset con 
			 )
		    end 

	and scan_bnd fset bnd = 
	    let
	      fun dofix vcset =
		    let 
			val newfset = VarSet.addList 
			    (fset, Sequence.maptolist #1 vcset) 
			fun dec (v,c) = declare true v
			val _ = Sequence.app dec vcset
		    in
			Sequence.app 
			(fn (v,function as 
			     Function{effect,tFormals=cvarlist,eFormals=varlist,
				      fFormals=fvarlist,body=exp,...}) =>
			     (if effect = Total 
				  then total_set := VarSet.add (!total_set, v)
			      else () ;
				  scan_function newfset function)) vcset
			
		    end 
	    in
	      case bnd of
		(Con_b (phase, conbnd) ) => scan_conbnd fset conbnd
	      | (Exp_b (v, nt, exp)) => 
		    ( declare false v; 
		     scan_exp fset exp)
	      | Fixopen_b vcset => dofix vcset
              | Fixcode_b vcset => dofix vcset

	      |  Fixclosure_b vcset => raise UNIMP
            end
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
	fun census_module (x , MODULE{imports=imports, 
				      bnds=bnds, exports=exports} ) = 
	    ( delta := x;
	     map (fn x=> scan_bnd VarSet.empty x) bnds;
	     map (fn x=> 
		  case x of 
		      ExportValue(label, var) => 
			  insesc VarSet.empty var
		    | ExportType (label, var) =>
			  () ) exports)
	    
    end (* Census functions *)

    fun print_table str t = fn (var, use)=>
	let val use = t use 
	in 
	    (if not ((!use) = 0) then   
		 ( print str;  Ppnil.pp_var var; print " : "; 
		  print (Int.toString (!use)); print "\n")
	     else ())
	end 
    val print_exp = fn (var, newexp) =>  
	( print "exp var "; Ppnil.pp_var var; print " maps to "; 
	 Ppnil.pp_exp newexp; print "\n")
	
    val print_con = fn (var, newvar) =>
	( print "con var "; Ppnil.pp_var var; print " maps to "; 
	 Ppnil.pp_con newvar; print "\n")
	
    fun print_stats unit = 
	( print "****************************************************\n"; 
	 print "APP TABLE\n";
	 HashTable.appi (print_table "app " count_app) count_table;
	 print "\nESC TABLE\n";
	 HashTable.appi (print_table "esc " count_esc) count_table;
	 print "\nREC TABLE\n";
	 HashTable.appi (print_table "rec " count_rec_app) count_table;
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
	    if (!do_dead
		andalso look count_app x = 0 
		andalso look count_esc x = 0 
		andalso look count_rec_app x = 0
		andalso look count_rec_esc x = 0
		)
		then ( if !debug then ( Ppnil.pp_var x; print " is dead\n" ) else () ;
		      inc_click dead_click ; true )
	    else false
	      
	fun dead_funcs xs = 
	    let fun dead x = (!do_dead 
			      andalso look count_app x = 0 
			      andalso look count_esc x = 0) 
	    (* Don't count recursive apps of funcs *)
	    in 
		if ( Listops.andfold dead xs ) 
		    then   ( (* app Ppnil.pp_var xs; print " are dead\n"; *)
			    inc_click dead_click ; true )
		else
		    false
	    end 
	
	fun subst exp = 
	    case exp of
		Var_e v => s(v)
	      | Const_e c  => exp 
	      | _ => exp
		    
	fun subst_c con = 
	    case con of
		Var_c v => sc(v)
	      | _  => con
		    
	fun is_pure_allp (NilPrimOp p) = true
	  | is_pure_allp (PrimOp p) = 
	    case p of
		(*	( mk_ref | setref | deref ) => false  *)
	      (*  | input | input1 | lookahead |open_in | open_out |close_in
	      | output | flush_out | close_out | end_of_stream ) => false *)

		update _ => false
              | create_table _ => false
              | create_empty_table _  => false
	      | soft_vtrap _ => false
              | soft_ztrap _ => false
              | hard_vtrap _ => false
              | hard_ztrap _  => false
	      | ( sub _ ) => true
	      | _ => true
		   
    fun is_pure exp = 
	case exp of 
	    Var_e _ => true
	  | Const_e _ => true
	  | App_e (_, Var_e f, _,_,_) => 
		VarSet.member (!total_set, f) 
	  (* We don't have to check args because of A-normal form *)
	  | App_e _ => (print "WARNING: reduce.sml found App not in A-normal form";
			false)
	  | ExternApp_e (exp, elist) =>
		false
	  | Prim_e (allp, _, _) =>
		is_pure_allp allp
	  | Let_e (_, bnds, exp) => 
		let fun is_pure_bnd (Exp_b (v,c,exp)) = is_pure exp
		      | is_pure_bnd _ = true
		in 
		    ( Listops.andfold is_pure_bnd bnds) 
		    andalso is_pure exp
		end 
	  | Raise_e _ => false 
	  | Switch_e sw => false (* Punt on this for now *)
	  | Handle_e (exp, v, fcn) => is_pure exp
		

	in  

	    (* Perhaps we can come up with a faster way of doing this *)

	  fun xkind fset kind = 
	    case kind of 
	      Type_k => kind
	    | SingleType_k c => SingleType_k (xcon fset c)
	    | Single_k c => Single_k (xcon fset c)
	    | Record_k lvkseq =>
		Record_k ( Sequence.map (fn ((l,v), kind) =>
					 ((l,v), xkind fset kind)) lvkseq)
	    | Arrow_k (openn, vklist, k) =>
		Arrow_k (openn, map (fn (v,k) => (v, xkind fset k)) vklist, xkind fset k)

	  and xconbnd fset cb = 
	    case cb of 
	      Con_cb (var, con) =>
		(Con_cb (var, xcon fset con))
	    | Open_cb (v, vklist, con, kind) =>
		 Open_cb (v, 
			  map (fn (v,k) => (v, xkind fset k)) vklist,
			  xcon fset con,
			  xkind fset kind)
	    | Code_cb _ => raise UNIMP

	  and xcon fset c =
	    case c of 
	      Typeof_c e => Typeof_c (xexp fset e)
	    | Var_c var => sc(var)
	    | Let_c (sort, cbnds, con) =>
		Let_c (sort, map (xconbnd fset) cbnds, con)
	    | App_c (con, cons) =>
		App_c (xcon fset con, map (xcon fset) cons)
	    | Prim_c (primcon, cons) => 
		Prim_c (primcon, map (xcon fset) cons)
	    | Mu_c (bool, vcseq) =>
		let val fset = VarSet.addList (fset, (Sequence.maptolist #1 vcseq))
		in 
		  Mu_c (bool, Sequence.map (fn (v, c) => (v, xcon fset c)) vcseq)
		end 
	    | AllArrow_c {openness, effect, isDependent, tFormals, eFormals, fFormals, body} =>
		AllArrow_c {openness = openness, effect = effect, isDependent=isDependent,
			    tFormals = map (fn (v,k) => (v, xkind fset k)) tFormals,
			    eFormals = map (fn (vopt,c) => (vopt, xcon fset c)) eFormals,
			    fFormals = fFormals,
			    body = xcon fset body}
	    | ExternArrow_c (cons, con) => 
		ExternArrow_c (map (xcon fset) cons, xcon fset con)
	    | Crecord_c (lclist) =>
		Crecord_c ( map (fn  (l,c) => (l, xcon fset c) ) lclist)
	    | Proj_c (con, label) => Proj_c (xcon fset con, label)
	    | Closure_c _ => raise UNIMP
	    | Annotate_c (a,con) => Annotate_c (a, xcon fset con)
	    | Typecase_c { arg=arg, arms=arms, default=default, kind= kind} => 
		Typecase_c { arg=xcon fset arg,
			    default = xcon fset default,
			    kind = xkind fset kind, 
			    arms = map (fn (primc, vklist, con)=> 
					(primc, (map (fn (v,k)=>
						      (v,xkind fset k))vklist),
					 xcon fset con)) arms}

	    and xswitch fset s =
		case s of
		    Intsw_e { arg, size, arms, default } =>
			Intsw_e {
			 arg=xexp fset arg,
			 size = size,
			 arms = map (fn (w,e) => (w, xexp fset e)) arms,
			 default = Option.map (xexp fset) default
			 }
		  | Sumsw_e {arg, sumtype, bound, arms, default} =>
			Sumsw_e {
			 arg=xexp fset arg,
			 sumtype= xcon fset sumtype,
			 bound=bound,
			 arms = map (fn (w,e) => (w,xexp fset e)) arms,
			 default =  Option.map (xexp fset) default
			 }
		  | Exncase_e {arg, bound, arms, default} =>  
			 Exncase_e {
			 arg = xexp fset arg,
			 bound = bound,
			 arms =  map (fn (w,e) => (xexp fset w,xexp fset e)) arms,
			 default =  Option.map (xexp fset) default
			 }
		  | Typecase_e {arg, arms, default} => 
			Typecase_e 
			 {
			  arg = xcon fset arg,
			  arms = map 
			  (fn (w,e) => 
			   (map (fn (v,k) => (v, xkind fset k)) w,xexp fset e)) arms,
			  default =  Option.map (xexp fset) default
			  }
			 
	    and xfunction fset 
		(Function{effect, recursive, isDependent,
			  tFormals, eFormals, fFormals, 
			  body, body_type=(tr,con)}) =
		let val tFormals = map (fn (v,k) => (v, xkind fset k)) tFormals
		    val eFormals = map (fn (v,tr,con) => (v, tr, xcon fset con)) eFormals
		    val body = xexp fset body
		    val con = xcon fset con
		in 
		    Function{effect=effect,recursive=recursive,isDependent=isDependent,
			     tFormals=tFormals, eFormals=eFormals, fFormals=fFormals,
			     body=body, body_type=(tr,con)}
		end
	
	    and xbnds fset 
		( (bnd :: rest) : bnd list )
		( body : body ) : ( bnd list * body ) = 
		( case bnd of
		    Con_b (phase, conbnd) =>
		      let val conbnd = xconbnd fset conbnd
			val (bnds, body) = xbnds fset rest body
		      in (Con_b(phase, conbnd)::bnds, body) end 
             
		  (* --------------------- Term bindings ------------------------ *)

		  | Exp_b ( x,nt, Prim_e (NilPrimOp (record labels), cons, exps )) => 
		      let fun dec (Var_e a) =  update_count ESC (s(a)) ~1 fset
			    | dec _ = ()
			      
		      in  
			if ( dead_var x ) 
			      then (app dec exps; 
				    app (fn (con) => 
					 (census_con fset (~1, con))) cons;
				    xbnds fset rest body )
					    
			      else
				 let val _ = if !do_project_known 
						 then HashTable.insert 
						     bind ( x, R ( labels, exps)) 
					     else ()
				     val (rest,body) = xbnds fset rest body
				 in
				     if (dead_var x )
					 then ( app dec exps ;
					       app (fn (con) => (census_con fset (~1, con))) cons;
					       (rest, body) )
				     else 
					 ( Exp_b ( x, nt,
						  Prim_e (NilPrimOp (record labels), 
							  map (xcon fset)cons, map subst exps)) :: rest, body )
				 end 
			end 
		    
	  (* ------------------------ Term Record Projection --------------------- *)
	  | Exp_b ( x, nt, Prim_e (NilPrimOp (select label), cons, [ Var_e a ] )) => 
		let fun cleanup unit = 
		    ( update_count ESC (s(a)) ~1 fset;
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
						   | NONE => error "compiler bug" )
						
					in
					    ( replace x b fset;
					     cleanup ();
					     xbnds fset rest body )
					end
				  (* Can change to project_sum_record *)
				 (* | SOME (S (r, tagcount, sum_cons))  =>
					let val _ = inc_click sum_record_click
					    val _ = update_count ESC (Var_e r) 1 fset
					    val _ = update_count ESC (Var_e a) ~1 fset
					    val _ = map (fn (con) => (census_con fset (1, con))) sum_cons
					    val (rest, body) = xbnds fset rest body
					in 					  
					   ( Exp_b ( x, nt,
						    Prim_e ( (NilPrimOp 
							     (project_sum_record (tagcount, label)),   
							      [], [ (s r) ]))):: rest, body)
					end  *)
				  (* Must leave it alone *)
				  | _ =>  let val (rest,body) = xbnds fset rest body
					  in 
					      if (dead_var x ) then 
						  (cleanup ();
						   (rest,body) )
					      else
						 ( Exp_b ( x, nt, Prim_e(NilPrimOp (select label),
									 map (xcon fset) cons, [ s(a) ]))::rest, body)
					  end)
			 | _ => error "compiler error 2" )
		end
	  (* ------------- Sum projection ... perhaps it was from a record ------------ *)
	  | Exp_b ( x, nt, Prim_e (NilPrimOp (project_sum sum), sum_cons, [ Var_e a ] )) =>
		let fun cleanup unit = 
		    ( update_count ESC (s(a)) ~1 fset; 
		     app (fn (con) => (census_con fset (~1, con))) sum_cons )
		in 	   
		    if ( dead_var x ) 
			then (cleanup();
			      xbnds fset rest body) 
		    else 
			let 
			    val _ = if !do_project_known 
					then HashTable.insert bind ( x, S (a, sum, sum_cons)) else ()
			    val (rest,body) = xbnds fset rest body
			in
			    if (dead_var x )
				then (cleanup();
				      (rest,body) )
			    else
				( Exp_b ( x, nt, Prim_e (NilPrimOp (project_sum sum), 
								    map (xcon fset) sum_cons, [(s a)]))::rest, body)
			end 
		end
		    (* ----------------- Variables --------- *)
	  | Exp_b (x, nt, Var_e v) =>
		 let
		     val _ = inc_click var_click ;
		     val _ = replace x (Var_e v) fset;
		     val _ = update_count ESC (s(v)) ~1 fset;
		     val nx = Name.var2name x
		     val _ = (case s(v) of
				  Var_e v' => 
				      let
					  val nv' = Name.var2name v'
				      in
					  if (String.size(nv') = 0) then
					      Name.rename_var (v', nx)
					  else ()
				      end
				| _ => ())
		 in
		     xbnds fset rest body
		 end
		
	  (* ----------------- Constants  ----------------- *)
	  | (bnd as Exp_b (x, nt, e as Const_e c))  =>
		 if (dead_var x ) then 
		     xbnds fset rest body
		 else 
		     if (exp_issmall e) then
			 ((inc_click var_click; 
			  replace x (Const_e c) fset);
			  xbnds fset rest body)
		     else
			 let val (rest,body) = xbnds fset rest body
			 in 
			     if (dead_var x ) then (rest,body)
			     else 
				 ( bnd :: rest, body)
			 end
		      
		      
	  (* ----------------- Function bindings ----------------- *)
	  | (bnd as Fixopen_b vcset) => 
		let (* val vclist = VarSet.listItems vcset *)
		    val vars = Sequence.maptolist #1 vcset
		    fun possible_func  v  =
			!do_inline andalso look count_app v = 1 andalso look count_esc v = 0 
			
		    (* Even when we inline the function, the cons & kinds on the args and the return con
		     go away, so we need to update the counts for them *)
			
		    fun remove_rest
			( vc as (v, Function{tFormals=vklist, eFormals=vclist, body_type=(_, con),...})) =
			(app (fn (_, kind) => census_kind fset (~1, kind)) vklist; 
			 app (fn (_, _, con) => census_con fset (~1, con)) vclist; 
			 census_con fset (~1, con))
		    fun remove_func 
			( vc as (v, Function{body, ...})) =
			( census_exp fset ( ~1, body); 
			 remove_rest vc
			 )
		    fun recur_func ( v, function) = 
			let val newfset = VarSet.addList (fset, vars)
			in ( v, xfunction newfset function)
			end 
		in
		    if (Listops.orfold 
			(fn v => (look count_rec_app v > 0)
			 orelse (look count_rec_esc v > 0)) vars )
		    then 
			if (dead_funcs vars) then
			    ( census_bnds fset (~1, [Fixopen_b vcset])   ; xbnds fset rest body)
			else 
			    let val (rest,body) = xbnds fset rest body
			    in 
				(Fixopen_b (Sequence.map recur_func vcset)::rest, body)
			    end 

		    else
			if dead_funcs vars  then 
			    ( Sequence.app remove_func vcset ; xbnds fset rest body )
			else
			    let 		
				val _ = Sequence.app  (fn (v, f) => HashTable.insert bind (v, F f)) vcset
				val (rest,body) = xbnds fset rest body
				fun final (vc as (v:var,c:function)) = 
				    case HashTable.find bind v of
					SOME INLINED => (remove_rest vc ; false )
				      | _ =>( if dead_var v then (remove_func vc; false)  else true )
					    
				val vclist' = map recur_func (List.filter final (Sequence.toList vcset))
				    
			    in 
				if null vclist' then (rest,body)
				else 
				    ( Fixopen_b ( Sequence.fromList  vclist') :: rest, body)
			    end
			end
		    
	  | Fixclosure_b _ => raise UNIMP
          | Fixcode_b _ => raise UNIMP
	      
	  (* ----------- Other expression bindings ---------------------------- *)
	  | Exp_b (x, nt, exp)=> 
	      if is_pure exp
		then 
		  if (dead_var x ) then (census_exp fset (~1, exp) ; 
					 xbnds fset rest body)
		  else 
		    let val (rest,body) = xbnds fset rest body
		    in 
		      if (dead_var x ) then (census_exp fset (~1, exp);
					     (rest,body))
		      else 
			( Exp_b (x, nt, xexp fset exp) :: rest, body)
		    end 
	      else let val (rest, body) = xbnds fset rest body
		   in 
		     (Exp_b (x, nt, xexp fset exp):: rest, body)
		   end )
		      
		      
	      | xbnds fset [] body = 
		( case body of 
		    EXP exp => 
			( [] , EXP (xexp fset exp) )
		  | EXPORTS entrys =>
			let fun do_entry ( ExportValue ( label, var )) = 
			     let val newexp = xexp fset (Var_e var)
			     in
				    case newexp of 
					Var_e v => 
					    (NONE, ExportValue (label,v  ))
				      | _ => 
					    let val newname = Name.fresh_named_var "export"
					    in (SOME (Exp_b (newname, TraceUnknown, newexp)), 
						ExportValue (label, newname))
					    end 
			     end  
			      | do_entry ( ExportType (label, var)) = 
				let val Var_c v = xcon fset (Var_c var) 
				in (NONE, ExportType (label, v)) end
						
			    val (bndopts, entries) = Listops.unzip (map do_entry entrys)
			    val bnds = List.mapPartial (fn x=>x) bndopts
			    val _ = exportbnds := bnds @ (!exportbnds)
			in ( [], EXPORTS entries ) 
			end )

		

	    and xexp fset exp = 
		( case exp of 
		      
		      Let_e(Sequential, bnds , body ) =>
			  let val (bnds, EXP body) = xbnds fset bnds (EXP body) 
			  in if (null bnds)
			     then body
			     else Let_e (Sequential, bnds, body)
			  end 
			
		    | Let_e (Parallel, _ , _) => raise UNIMP 
		
		    | Prim_e ( ap , clist, elist ) =>
			  let val clist' = map (xcon fset) clist
			      val elist' = map (xexp fset) elist
			      val exp' = Prim_e (ap, clist', elist')
			  in
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
			(*	| Prim_e ( NilPrimOp (inject info), [], 
					  [ Var_e v ] ) =>
				  ( case ( HashTable.find bind v)  of
					SOME ( R(labels, exps ) ) =>
					    (inc_click fold_click; 
					     Prim_e ( NilPrimOp (inject_record info), [], exps) )
				      | NONE =>  exp' )   *)
					  
			        | _ => exp' 
				      
			  end

		    (* Application, look for possible inlining *)
		    | App_e (openness, Var_e f, clist, elist, elist2) =>
			  let
			      val new_app = App_e (openness, s(f), map (xcon fset) clist,
						   map subst elist, map subst elist2)
			      val Var_e sf = s(f)
			  in 
			      if !do_inline andalso 
				  look count_app sf = 1 andalso 
				  look count_esc sf = 0 
			       then 
				   ( case (HashTable.find bind sf) of
					 SOME (F (Function{tFormals=vklist, 
							   eFormals=vclist, 
							   fFormals=vlist, 
							   body=exp, body_type=(_,con), ...})) =>
					     let val _ = inc_click inline_click
						 fun do_args (arg,  x ) =
						     case arg of
							 Var_e a =>
							     ( insert sigma x (s(a)); 
							      update_count APP (s(a))
							      (look count_app x)  fset ;
							      update_count ESC (s(a)) 
							      ((look count_esc x) - 1 ) fset)
						       | _ => insert sigma x arg
						      
						 fun do_cargs (arg, x) = 
						     case arg of
							 Var_c a =>
							     ( insert sigma_c x (sc(a)) 
							      (* ; 
							       update_count_c count_app (sc(a))
							       (look count_app x)  fset ;
							       update_count_c count_esc (sc(a)) 
							       ((look count_esc x) - 1 ) fset *) )
						       | _ =>  insert sigma_c x arg 
					     in
						 (* Substitute the args *)
						 (Listops.map2 do_args (elist, map #1 vclist) ; (* Regular arguments *)
						  Listops.map2 do_args (elist2, vlist) ;        (* Floating point *)
						  Listops.map2 do_cargs (clist, map #1 vklist) ;(* Constructor arguments *)
						  
						  update_count APP (s(f)) ~1 fset;
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
		    |  ExternApp_e (exp, expl) =>
			   ExternApp_e (xexp fset exp, map (xexp fset) expl)
		    | Var_e v => s(v)
		    | Const_e c => exp
		    | Raise_e (exp, con) => Raise_e (xexp fset exp, xcon fset con)
		    | Handle_e (exp,v,exp2) => 
			  Handle_e (xexp fset exp,v, xexp fset exp2)
		    | Switch_e (sw) => Switch_e (xswitch fset sw) 
			  )
		      
end (* local for ncontract *)


fun doModule (module as MODULE{bnds=bnds, imports=imports, exports=exports}) = 
    let 

        
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
	val _ = exportbnds := []
	fun clearTable table = 
	    HashTable.appi ( fn (key, item) => ignore (HashTable.remove table key)) table
	val x = ref 10
	val _ = init_clicks clicks

	fun init v = 
	    let val _ = clearTable count_table
		val _ = clearTable sigma
		val _ = clearTable sigma_c
		val _ = clearTable bind
		val _ = clearTable drop_table
		  
		val _ = total_set := VarSet.empty
		val _ = census_module(1, module)
		val _ = info_valid := true
(*		val _ = print_escape () *)
	    in x := v
	    end 
	
	val _ = if !debug then print_stats() else () 
	    
	(* Now loop through the code until, we only do x reductions. *)
	       
	fun loop ( i:int , (m as (bnds, EXPORTS entrys): (bnd list * body) )) 
	  : (bnd list * body) =
	  if ( i <= !x ) then m
	  else 
	    let val _ = clearTable drop_table
	      val _ = clearTable bind 
	      val (m as (bnds, EXPORTS entrys)) = xbnds VarSet.empty bnds (EXPORTS entrys)
	    in (
		if !debug then 
		  (  
		   print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		   print "ITERATION\n";
			 print_round_clicks clicks;
			 app Ppnil.pp_bnd bnds; print "\n";
			 print_stats() 
			 )
		    else 
			( 
			 print_round_clicks clicks
			 ) ;  
			loop (round_clicks clicks, m )
			)
		end 
	val (newbnds,( EXPORTS newexports)) = 
	  (init 0 ; (loop (1, (bnds, EXPORTS exports))))
    in 
(*      print_escape () ;  *)
      MODULE { bnds= newbnds @ (!exportbnds),
	      imports=imports, exports=newexports}   
    end (* do_it *)
  end (* struct *)


		    

