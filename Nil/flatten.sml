(*$import Nil NilUtil Ppnil PrimUtil NilSubst Reduce PASS PRIMUTIL *)

(* 

 Flatten functions which take records for and argument and  
 which don't escape (get this info from Reduce). A variant of this  
 would be to flatten all functions and export both the flattened and 
 unflattened versions. 

 This pass also does primitive constant folding. (i.e. 3 + 4 replaced
 with 7) Perhaps this should be moved to a difference pass.

*)

functor Flatten( structure PrimUtil : PRIMUTIL 
		where type con = Nil.con 
		where type exp = Nil.exp)
  :> PASS = 

  struct
    structure Nil = Nil
    structure Subst = NilSubst
    open Nil Name 

    exception UNIMP
    val error = fn s => Util.error "flattenedargs" s	


    (* ----------------- Parameters ----------------------------- *)
    val debug = ref false;
    val do_flatten = Stats.tt("do_flatten")

    (* Maximum number of arguments per function. Should be close to the 
     number of machine registers or something *)
    val maxFnArgs = 6  

    (* ---------------- Local State ----------------------------- *)

    exception FnNotFound (* This exception should not propagate outside of this module *)
    
    (* Table of functions which have had their first record argument flattened *)
    val flattened = Name.mk_var_hash_table(200, FnNotFound)   :
      (var , (var * label list * var list * con list)) HashTable.hash_table 

    (* Counts of optimizations which have occured *) 
    val fold_click = Stats.int "Constants folded"
    val flatten_click = Stats.int "Functions flattened"
    fun inc_click c = 
      c := !c + 1

    (* Table of various information about each variable, created during
     the reduce pass. If it is not availble, we don't do this pass. *)
    val count_table = ref NONE :
      (var, Reduce.info) HashTable.hash_table option ref 
      
    fun non_esc_var (v:var) : bool = 
      case !count_table of 
	SOME tr =>
	  let val t =  (HashTable.lookup (tr) v)
	  in !(Reduce.count_esc t) = 0
	    andalso 
	    !(Reduce.count_rec_esc t) = 0
	  end
      | NONE => error "Can't read hashtable"
	  

    (* ----------------------------------------------------------- *)

	  
    (* In order to do a list of bindings as well as bindings in an
     expression we pull this hack *)
    datatype body = EXP of exp | NADA 

    (* In order to easily recur through cons and kinds, use the 
     rewrite mechanism from NilUtil *)
    fun make_handlers unit =
      (fn (b,e) =>
       let val newexp = xexp e
	 val _ = (print "found exp in type_of \n")
       in NilUtil.CHANGE_NORECURSE newexp
       end,
       fn (b, bnd) => 
       error "Shouldn't get here",
       fn (b, c) => NilUtil.NOCHANGE,
       fn (b, cb) => NilUtil.NOCHANGE,
       fn (b, k) => NilUtil.NOCHANGE)
      
    (* Step one: rewrite non-escaping functions *)
    and flatten_func  ( fnVar, Function{effect, recursive, isDependent,
					tFormals = vklist, eFormals = vclist,
					fFormals = vlist, body = body, 
					body_type = con} ) =
      (case vclist of 
	 (* Single record argument *)
	 [ (recordArg, _, recordType as Prim_c( Record_c (labels, vopt), cons))] =>
	   if (non_esc_var fnVar) 
	     andalso 
	     ((length vlist) + (length labels)  <= maxFnArgs ) 
	     then 
	       let
		 (* We found one ! *)
		 val _ = inc_click flatten_click 
		 (* First rename the function and make it take more arguments *)
		 val newname = (Name.var2string fnVar) ^ "flattened"
		 val newFnVar = (Name.fresh_named_var newname)
		 val _ = if !debug then 
		   print ("Flattening " ^ (Name.var2string fnVar) ^ " to "
			  ^ (Name.var2string newFnVar) ^ "\n" )
			 else ()
		 val args = map (fn _ => (Name.fresh_var())) labels
		 val newcons = map xcon cons
		 val newvclist = Listops.map2 (fn (v,c) => (v, TraceUnknown, c)) (args, newcons)
		 val exps = map Var_e args 
		 val _ = HashTable.insert flattened 
		   (fnVar,(newFnVar, labels, (map #1 vklist), newcons))
		 val newbody = 
		   Let_e (Sequential, 
			  [Exp_b (recordArg, 
				  TraceUnknown,
				  Prim_e (NilPrimOp (record labels), [], exps))],
			  body)
		   
		 (* We'll on the body later *)
		 val newFn = (newFnVar,
			      Function{effect=effect, recursive=recursive, isDependent = isDependent,
				       tFormals = vklist, eFormals = newvclist, 
				       fFormals = vlist, 
				       body = newbody, body_type = con})
		   
	       (* Now a new version of fnVar which calls NewFnVar *)
	       (*	 val args = map (fn _ => (Name.fresh_var())) labels
val newvlist = map (fn _ => Name.fresh_var() ) vlist
val newvklist = map (fn (_, k) => (Name.fresh_var(), k)) vklist
val call = Name.fresh_var()
val newRecordArg = Name.fresh_var()
		
val subst = Subst.fromList (Listops.zip (map #1 vklist) (map (Var_c o #1) newvklist))
val newbnds =  Listops.map3 (fn (v, c, l) => 
		Exp_b (v,  TraceUnknown,
		Prim_e (NilPrimOp (select l), [], [Var_e newRecordArg])))
		(args, cons, labels) 
               val newRecordType = Subst.substConInCon subst recordType
               val newcon = Subst.substConInCon subst con
               val func = 
		( fnVar, Function (eff, a, newvklist, bl, [(newRecordArg, newRecordType)], newvlist, 
		Let_e (Sequential, newbnds @
		[ Exp_b ( call, TraceUnknown, App_e 
		(Open, Var_e newFnVar, 
		map (Var_c o  #1) newvklist, 
		map Var_e args, map Var_e newvlist))], 
		Var_e call), newcon))
		*)
	       in 
		 [ newFn (*, func*) ] 		
	       end 
	   else 
	     (* Takes a record argument, but we can't flatten it *)
	     [ ( fnVar, Function{effect=effect, recursive=recursive, isDependent = isDependent,
			      tFormals = vklist, eFormals = [ (recordArg, TraceKnown TraceInfo.Trace, 
							       recordType) ],
			      fFormals = vlist, 
			      body = body, body_type = con} )]
	| _ =>  (* Argument isn't a single record *)
	  [ ( fnVar, Function{effect=effect, recursive=recursive, isDependent = isDependent,
			      tFormals = vklist, eFormals = vclist, 
			      fFormals = vlist, 
			      body = body, body_type = con}) ])
	  
    (* step two: rewrite applications of the flattened functions *)	
    and doApp ( openness, Var_e f, tactuals, (actual:exp list), elist2) = 
      let val r = HashTable.find flattened f
      in 
	( case r of 
	    SOME (newName, labels, tformals, cons ) =>
	      let 
		(* val subst = Subst.fromList (Listops.zip tformals tactuals) *)
		(* val newcons = map (Subst.substConInCon subst) cons *)

		val _ = 
		  if !debug then 
		    print ("Replacing app of " ^ (Name.var2string f) ^ " with "
			   ^ (Name.var2string newName) ^ "\n")
		  else ()
		val args = map (fn _ => (Name.fresh_var())) labels
		  
		val newexps = map Var_e args
		(* If we are not in A-normal form, we don't want to duplicate this *)
		val (recvar, prebnd)  = case actual of [Var_e v] => (actual, [])
	      | [other] => let val t = Name.fresh_var() 
			   in 
			     ([Var_e t], [ Exp_b (t, TraceUnknown, other)] )
			   end 
		val bnds =  prebnd @ (Listops.map2
				      (fn (var, label) =>
				       Exp_b (var, TraceUnknown, Prim_e 
					      (NilPrimOp (select label),
					       [], recvar))))
		  (args ,labels)
	      in 
		(bnds, (openness, Var_e newName, map xcon tactuals, newexps, map xexp elist2))
	      end 
	  | NONE =>  ( [], (openness, Var_e f , map xcon tactuals, actual, map xexp elist2)))
      end 
      | doApp app = ( [], app )
    and xexp   exp = 
      case exp of 
	Var_e v => exp
      | Const_e c => exp
      (* Constant folding of primitives *)
      | Prim_e (PrimOp p, cons, exps) => 
	  (if List.all Reduce.exp_isval exps
	     then 
	      ( let val newexp = PrimUtil.apply p cons exps
		in 
		  (inc_click fold_click; newexp )
		end 
	       handle Util.UNIMP => Prim_e (PrimOp p, map xcon cons, map (xexp ) exps) )
	  else Prim_e (PrimOp p, map xcon cons, map (xexp ) exps))
      | Prim_e (allp, cons, exps) =>
	  Prim_e (allp, map xcon cons, map( xexp ) exps)
      | Switch_e sw => 
	  Switch_e (xswitch  sw)
      (* Change function call to new arguments *) 
      | App_e app => 
	  let val (bnds, app) = doApp app
	  in 
	    if null bnds then App_e app
	    else 
	      Let_e (Sequential, bnds, App_e app)
	  end 
      | Let_e (sort, bnds, exp ) =>
	  let val (bnds, EXP body) = xbnds bnds (EXP exp)
	  in 
	    Let_e (sort, bnds, body) 
	  end 
      | ExternApp_e (exp, explist) => 
	  ExternApp_e (xexp  exp, map (xexp ) explist)
      | Raise_e (exp, con) => Raise_e (xexp  exp, xcon con)
      | Handle_e {body, bound, handler, result_type} =>
	  Handle_e {body = xexp body, bound = bound,
	            handler = xexp handler, result_type = xcon result_type}
	  
    and xfn  (Function{effect, recursive, isDependent,
		       tFormals, eFormals, fFormals, body, body_type = con}) =
      Function {effect=effect, recursive=recursive, isDependent = isDependent,
		tFormals = tFormals, eFormals = map (fn (v,tr,c) => (v, tr, xcon c)) eFormals,
		fFormals = fFormals,
		body = xexp body, body_type = xcon con}

    and xswitch s =
      case s of
	Intsw_e { arg, size, arms, default, result_type } =>
	  Intsw_e 
	  {
	   arg=xexp arg,
	   size = size,
	   arms = map (fn (w,e) => (w, xexp e)) arms,
	   default = Option.map (xexp) default,
	   result_type = xcon result_type
	   }
      | Sumsw_e {arg, sumtype, bound, arms, default, result_type} =>
	  Sumsw_e 
	  {
	   arg=xexp arg,
	   sumtype= xcon sumtype,
	   bound=bound,
	   arms = map (fn (w,tr,e) => (w,tr,xexp e)) arms,
	   default =  Option.map (xexp ) default,
	   result_type = xcon result_type
	   }
      | Exncase_e {arg, bound, arms, default, result_type} =>  
	  Exncase_e 
	  {
	   arg = xexp arg,
	   bound = bound,
	   arms =  map (fn (w,tr,e) => (xexp w,tr,xexp e)) arms,
	   default =  Option.map (xexp ) default,
	   result_type = xcon result_type
	   }
      | Typecase_e {arg, arms, default, result_type} => 
	  Typecase_e 
	  {
	   arg = xcon arg,
	   arms = map  (fn (w,e) => (w,xexp e)) arms,
	   default =  Option.map (xexp ) default,
	   result_type = xcon result_type
	   }

    and xbnds       
      ((bnd::rest):bnd list)
      (body:body) = 
      (case bnd of 
	 Con_b ( phase, conbnd) => 
	   let val conbnd = xcbnd conbnd
	     val (rest, body) = xbnds rest body
	   in (Con_b (phase, conbnd)::rest, body) end 
       | Exp_b (v, nt, App_e app) => 
	   let val (bnds1, app) = doApp app
	     val (bnds2, body) = xbnds rest body
	     val newbnds =  bnds1 @ [ Exp_b (v, nt, App_e app)] @ bnds2
	   in (newbnds, body)
	   end 
       | Exp_b ( v, nt, exp) => 
	   let val exp = xexp  exp
	     val (rest, body) = xbnds  rest body
	   in (Exp_b (v, nt, exp) :: rest , body) 
	   end 
       | Fixopen_b vfset=> 
	   let
	    
	     val vflistlist =  (Sequence.maptolist (flatten_func) vfset) 
	     val vflist = map (fn (v,f) => (v, xfn f)) (Listops.flatten vflistlist)
	     val vfseq  = Sequence.fromList vflist
	     val (rest, body) = xbnds  rest body
	   in (Fixopen_b vfseq::rest, body)
	   end 
	 
       | _ => error "Compiler bug" )
	 
      | xbnds  [] (EXP body) = 
	([], EXP  (xexp  body))
      | xbnds  [] NADA = ([], NADA)

    and xcon con = NilUtil.con_rewrite (make_handlers ()) con
    and xcbnd conbnd = NilUtil.cbnd_rewrite (make_handlers ()) conbnd
       	
    fun doModule (MODULE {bnds, imports, exports} ) = 
      let 
	
	(* import var table from reduce *)
	val _ = count_table := Reduce.getInformationTable ()
	val _ = case !count_table of
	  NONE => raise error "Reduce table not available"
	| _ => ()
	(* Clear out the hash table *)
	val _ = HashTable.appi 
	  ( fn (key, item) => ignore (HashTable.remove  flattened key)) flattened
	  
	val (bnds, NADA) = xbnds bnds NADA
      in  MODULE{bnds = bnds,
		 imports = imports,
		 exports = exports}
      end
    
  end 


	    




