(* Closure conversion is accomplished in two phases.  
   The first phase scans the program for free type and term variables of 
   all type and term functions and also notes whether a function escapes or not.  
   The second phase then rewrites the functions into codes and closures.  
   Thus, before closure conversion, there are Fixopen_b and App_e(Open,...).
   After closure conversion, there are Fixcode_b, Fixclosure_b, 
   App_e(Closure,...), and App_e(Code,...).
*)

functor ToClosure(structure Nil : NIL
		  structure NilStatic : NILSTATIC
		  structure NilContext : NILCONTEXT
		  structure NilUtil : NILUTIL
		  structure Ppnil : PPNIL
		  structure Subst : NILSUBST
		  sharing NilUtil.Nil = Ppnil.Nil = NilStatic.Nil = NilContext.Nil = Nil
		      and type NilContext.context = NilStatic.context
			 and type Subst.con = Nil.con
		         and type Subst.exp = Nil.exp
			 and type Subst.kind = Nil.kind)
    : TOCLOSURE =
struct


    val use_kind_at_bind = ref false
    val do_close_path = ref true
    val do_single_venv = ref true

    val select_carries_types = Stats.bool "select_carries_types"
    val select_has_con = select_carries_types
    val closure_print_free = Stats.bool "closure_print_free"


    open Util NilUtil Name Nil
    structure Nil = Nil

    val error = fn s => error "toclosure.sml" s
    val debug = ref false
    val debug_full = ref false
    val float64 = Prim_c(Float_c Prim.F64,[])
    structure FidSet = VarSet
    structure FidMap = VarMap
    type fid = var
    fun chat s = print s

    (* -------------- types and values manipulating free variables ------------- *)
    (* lists are kept in reverse order; map allow fast lookup/membership  *)
    type frees = {freecpaths : (var * label list) list,
		  freecpaths_map : (var * (var -> con) * kind) PathMap.map,
		  freeepaths_map : (var * (int * var -> exp) * con) PathMap.map}

    val empty_frees = {freecpaths = [],
		       freecpaths_map = PathMap.empty,
		       freeepaths_map = PathMap.empty}


    fun path_listmap_union(oldlist,oldset,newlist,newset) = 
	let fun folder(p,acc as (changed,curlist,curset)) =
	    case (PathMap.find(curset,p)) of
		SOME _ => acc
	      | NONE => (false, p::curlist, PathMap.insert(curset,p,valOf(PathMap.find(newset,p))))
	in  foldr folder (true,oldlist,oldset) newlist (* use foldr since lists are reversed *)
	end


    fun path_map_union(oldmap,newmap) = 
	let fun folder(path,item,acc as (changed,curmap)) =
	    case (PathMap.find(curmap,path)) of
		  SOME _ => acc
		| NONE => (false, PathMap.insert(curmap,path,item))
	in  PathMap.foldli folder (true,oldmap) newmap
	end

    (* returns a bool indicating whether the result is same as the first fv set;
      adds to the first free, in order, all the items only found in the second *)
    fun join_free'({freeepaths_map=ep1, freecpaths=c1, freecpaths_map=cp1} : frees,
		   {freeepaths_map=ep2, freecpaths=c2, freecpaths_map=cp2} : frees) = 
	let val (sameep,ep3) = path_map_union(ep1,ep2)
	    val (samec,c3,cp3) = path_listmap_union(c1,cp1,c2,cp2)
	in  (samec andalso sameep, 
		{freeepaths_map = ep3,
		 freecpaths = c3, 
		 freecpaths_map = cp3})
	end

    fun join_free args : frees = #2(join_free' args)


    fun show_path (v,labs) = (Ppnil.pp_var v;
			      app (fn l => (print "."; Ppnil.pp_label l)) labs)
	
    fun show_free({freeepaths_map, freecpaths, freecpaths_map, ...} : frees) =
	(print "freeepaths are: "; 
	 PathMap.appi (fn (p,_) => (show_path p; print "; ")) freeepaths_map; print "\n";
	 print "freecpaths are: "; app (fn p => (show_path p; print "; ")) freecpaths; print "\n";
	 print "freecpaths_map are: "; 
	 PathMap.appi (fn (p,_) => (show_path p; print "; ")) freecpaths_map; print "\n")


    (* labels are passed backwards *)
    fun project (base, labs) = 
	let fun loop [] = base
	      | loop (l::rest) = Prim_e(NilPrimOp(select l),[],[loop rest])
	in  loop labs 
	end
    (* labels are returned backwards *)
    fun extract_path e = 
	let fun loop acc (Prim_e(NilPrimOp(select l), _, [e])) = loop (l::acc) e
	      | loop acc e = (e,rev acc)
	in loop [] e
	end
    fun path2exp(var,labs) = project(Var_e var, labs)

    (* -------- types and values manipulating functions and related information ------ *)



    (* -------- code to perform the initial free-variable computation ---------------- *)
    local
	datatype expentry = GLOBALe | LOCALe of con * frees option ref | SHADOWe of con * frees option ref
	datatype conentry = GLOBALc | LOCALc of kind * frees option ref | SHADOWc of kind * frees option ref
	datatype state = STATE of {curfid : fid,
				   is_top : bool,
				   ctxt : NilStatic.context,
				   boundevars : expentry VarMap.map,
				   boundcvars : conentry VarMap.map,
				   boundfids : FidSet.set}
	type funentry = {static : {fid : fid, 
				   code_var : fid,
				   unpack_var : fid,
				   cenv_var : var,
				   venv_var : var},
			 escape : bool,
			 callee : (fid * state) list,
			 frees : frees}
	fun add_gboundevar (STATE{ctxt,is_top,curfid,boundevars,boundcvars,boundfids},v) = 
	    let val boundevars' = VarMap.insert(boundevars,v,GLOBALe)
	    in  STATE{is_top = is_top,
		      curfid = curfid,
		      ctxt = ctxt,
		      boundevars = boundevars',
		      boundcvars = boundcvars,
		      boundfids = boundfids}
	    end
	fun add_gboundcvar (STATE{ctxt,is_top,curfid,boundevars,boundcvars,boundfids},v,k) = 
	    let val boundcvars' = VarMap.insert(boundcvars,v,GLOBALc)
		val ctxt' = if (!use_kind_at_bind) 
				then (error "use_kind_at_bind not done here")
			    else NilContext.insert_kind(ctxt,v,k)
	    in  STATE{ctxt = ctxt',
		      is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars,
		      boundcvars = boundcvars',
		      boundfids = boundfids}
	    end

	fun add_boundevars' shadow (STATE{ctxt,is_top,curfid,boundevars,boundcvars,boundfids},vc_list) = 
	    let val wrap = if shadow then (fn c => SHADOWe(c,ref NONE))
			   else (if is_top then (fn _ => GLOBALe) 
				 else (fn c => LOCALe(c,ref NONE)))
		val boundevars' = foldl (fn ((v,c),m) => VarMap.insert(m,v,wrap c)) boundevars vc_list
	    in  STATE{ctxt = ctxt,
		      is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars',
		      boundcvars = boundcvars,
		      boundfids = boundfids}
	    end
	val add_boundevars = add_boundevars' false

	fun add_boundcvars reduced (STATE{ctxt,is_top,curfid,boundevars,boundcvars,boundfids},vck_list) = 
	    let val wrap = (if is_top then (fn _ => GLOBALc) 
			    else (fn k => LOCALc(k,ref NONE)))
		val _ = if reduced 
			    then (Stats.counter("add_boundcvars_reduced"))()
			else (Stats.counter("add_boundcvars_unreduced"))()
		fun folder ((v,copt,kopt),ctxt) =
		    ((NilContext.find_kind(ctxt,v); ctxt)
		    handle NilContext.Unbound =>
			(case (copt,kopt) of
			  (NONE,SOME k) => NilContext.insert_kind(ctxt,v,k)
		        | (SOME c, SOME k) => NilContext.insert_kind_equation(ctxt,v,c,k)
		        | (SOME c, NONE) => error "add_boundcvar with equation but no kind"
		        | (NONE,NONE) => error "add_boundcvars with no info"))
		val ctxt' =  if (!use_kind_at_bind) then ctxt 
		              else foldl folder ctxt vck_list

		fun folder ((v,NONE,SOME k),m) = VarMap.insert(m,v,wrap k)
		  | folder ((v,_,_),m) = 
		    let val k = NilContext.find_kind(ctxt',v)
		    handle NilContext.Unbound => (error "add_boundcvars internal error")
		    in  VarMap.insert(m,v,wrap k)
		    end
		val boundcvars' = foldl folder boundcvars vck_list

					 

	    in  STATE{ctxt = ctxt',
		      is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars,
		      boundcvars = boundcvars',
		      boundfids = boundfids}
	    end

val add_boundcvars = fn arg1 => fn arg2 => Stats.subtimer("toclosure_add_boundcvars",
							  add_boundcvars arg1) arg2

	fun add_boundfids shadow (STATE{ctxt,is_top,curfid,boundevars,boundcvars,boundfids},fid_types) = 
	    let val state = STATE{ctxt=ctxt,
				  curfid = curfid,
				  is_top = is_top,
				  boundevars = boundevars,
				  boundcvars = boundcvars,
				  boundfids = VarSet.addList(boundfids,map #1 fid_types)}
	    in  add_boundevars' shadow (state,fid_types)
	    end

	fun add_boundevar(s,v,c) = add_boundevars(s,[(v,c)])
	fun add_boundcvar(s,v,k) = add_boundcvars false (s,[(v,NONE, SOME k)])
	fun add_boundcvar'(s,v,c,k) = add_boundcvars true (s,[(v,SOME c, SOME k)])
	    
	fun is_boundevar(STATE{boundevars,...},evar) = 
	    (case (VarMap.find(boundevars,evar)) of
		 SOME (GLOBALe | (LOCALe _)) => true
	       | _ => false)

	fun is_boundcvar(STATE{boundcvars,...},cvar) = 
	    (case (VarMap.find(boundcvars,cvar)) of
		 SOME (GLOBALc | (LOCALc _)) => true
	       | _ => false)

	fun is_boundfid(STATE{boundfids,...}, f) = VarSet.member(boundfids,f)


	fun cvar_isfree (STATE{boundcvars,boundevars,...},{freecpaths_map,...}:frees,cvar) = 
	    if (case (PathMap.find(freecpaths_map,(cvar,[]))) of
		      NONE => false
		    | _ => true)
		then NONE
	    else (case (VarMap.find(boundcvars,cvar)) of
			  SOME (GLOBALc | (LOCALc _)) => NONE
			| SOME (SHADOWc k) => SOME k
			| NONE => error ("cvar_isfree: variable " ^
					 (var2string cvar) ^ " not bound"))

(* labs are backwards *)
	fun epath_isfree (STATE{ctxt,boundevars,...},{freeepaths_map,...}:frees,epath as (evar,labs)) = 
	    case (PathMap.find(freeepaths_map,epath)) of
		SOME _ => NONE
	      | _ => (case (VarMap.find(boundevars,evar)) of
			  SOME (GLOBALe | (LOCALe _)) => NONE
			| SOME (SHADOWe (c,f)) => 
			      let fun search l ([],[]) = error "bad projection"
				    | search l ((l1::lrest),(c1::crest)) = 
				  if (eq_label(l,l1)) then c1 else search l (lrest,crest)
				    | search l _ = error "ill-formed record type"
				  fun proj (c,[]) = c
				    | proj (c,l::rest) = 
				      let val c = case c of
					  Prim_c(Record_c labs,cons) => search l (labs,cons)
					| _ => error "projecting from non-record type"
				      in  proj(c,rest)
				end
			      in  SOME (proj(c,rev labs),f)
			      end
			| NONE => error ("evar_isfree: variable " ^
					 (var2string evar) ^ " not bound"))

	fun evar_isfree (STATE{boundcvars,boundevars,...},{freeepaths_map,...}:frees,evar) = 
	    if (case PathMap.find(freeepaths_map,(evar,[])) of
		    NONE => false
		  | _ => true)
		then NONE
	    else 
		(case (VarMap.find(boundevars,evar)) of
		     SOME (GLOBALe | (LOCALe _)) => NONE
		   | SOME (SHADOWe cf) => SOME cf
		   | NONE => error ("evar_isfree: variable " ^
				    (var2string evar) ^ " not bound"))
			    

	fun free_cvar_add (f as {freecpaths,freecpaths_map,freeepaths_map}:frees,cvar,k) = 
	    let 
		val _ = if (!debug)
			    then (print "free_cvar_add-ing "; Ppnil.pp_var cvar; print " to\n";
				  show_free f; print "\n\n")
			else ()
		fun bubble cvar [] = [(cvar,[])]
		  | bubble cvar (first::rest) = 
		    let val v = Name.fresh_named_var "dummy"
			val SOME (_,_,k') = PathMap.find(freecpaths_map,first)
			val c = AllArrow_c(Open,Total,[(v,k')],[],0w0,Var_c v)
		    in  if (NilUtil.convar_occurs_free(cvar,c))
			    then (cvar,[])::first::rest
			else first::(bubble cvar rest)
		    end
		val freecpaths = rev(bubble cvar (rev freecpaths))
		val freecpaths_map = 
		    case PathMap.find(freecpaths_map,(cvar,[])) of
			SOME _ => error "free_cvar_add given variable already in free set"
		      | _ => 
			    let val l = Name.internal_label(Name.var2string cvar)
				fun cthunk cenv_var = Proj_c(Var_c cenv_var,l)
			    in PathMap.insert(freecpaths_map,(cvar,[]),
					      (cvar,cthunk,k))
			    end
		val res = {freeepaths_map = freeepaths_map,
			   freecpaths = freecpaths, 
			   freecpaths_map = freecpaths_map}
		val _ = if (!debug)
			    then (print "free_cvar_add done "; Ppnil.pp_var cvar; print " to\n";
				  show_free f; print "\n\n\n")
			else ()
	    in  res
	    end

	fun free_epath_add ({freecpaths,freecpaths_map,
			     freeepaths_map}:frees,epath,c) = 
	    let val str = foldl (fn (l,acc) => acc ^ "_" ^ (Name.label2string l))
		(Name.var2name (#1 epath)) (#2 epath)
		val v = Name.fresh_named_var str
		val freeepaths_map = 
		(case (PathMap.find(freeepaths_map,epath)) of
		     SOME _ => error "free_epath_add given variable already in free set"
		   | NONE => let val l = Name.internal_label(Name.var2string v)
				 fun ethunk(num_efree,venv_var) = 
				     if (!do_single_venv andalso num_efree = 1)
					 then Var_e venv_var
				     else
					 Prim_e(NilPrimOp(select l), [], [Var_e venv_var])
			     in  PathMap.insert(freeepaths_map,epath,(v,ethunk,c))
			     end)
	    in  {freeepaths_map = freeepaths_map,
		 freecpaths = freecpaths, freecpaths_map = freecpaths_map}
	    end


	fun free_evar_add (frees,evar,c) = 
	    free_epath_add(frees,(evar,[]),c)

	val empty_table = VarMap.empty : (funentry ref) VarMap.map
	val fids = ref empty_table
	fun empty_fun escape new_fid : funentry = 
	    {static = {fid = new_fid,
		       code_var = fresh_named_var((var2string new_fid)
						  ^ "_code"),
		       unpack_var = fresh_named_var((var2string new_fid)
						    ^ "_unpack"),
		       cenv_var = fresh_named_var("free_cons"),
		       venv_var = fresh_named_var("free_exps")},
	     escape = escape,
	     callee = [],
	     frees = empty_frees}
	val global_escapes = ref (VarSet.empty : Name.VarSet.set)




    fun show_state(STATE{boundevars, boundcvars, ...} : state) =
	let fun show_eentry GLOBALe = print "GLOABLe"
	      | show_eentry (LOCALe _) = print "LOCALe"
	      | show_eentry (SHADOWe _) = print "SHADOWe"
	    fun show_centry GLOBALc = print "GLOABLc"
	      | show_centry (LOCALc _) = print "LOCALc"
	      | show_centry (SHADOWc _) = print "SHADOWc"
	in
	    (print "boundevars are: "; 
	     VarMap.appi (fn (v,e) => (print "  "; Ppnil.pp_var v; 
				       print " "; show_eentry e; print "\n")) boundevars; print "\n";
	     print "boundcvars are: "; 
	     VarMap.appi (fn (v,e) => (print "  "; Ppnil.pp_var v; 
				       print " "; show_centry e; print "\n")) boundcvars; print "\n")
	end


	fun copy_state (STATE{ctxt,boundevars,boundcvars,boundfids,...}) fid = 
	    let fun epromote (LOCALe cf) = SHADOWe cf
		  | epromote x = x
		fun cpromote (LOCALc kf) = SHADOWc kf
		  | cpromote x = x
		val boundevars' = VarMap.map epromote boundevars
		val boundcvars' = VarMap.map cpromote boundcvars
	    in  STATE{is_top = false,
		      curfid=fid,
		      ctxt = ctxt,
		      boundfids=boundfids,
		      boundevars = boundevars',
		      boundcvars = boundcvars'}
	    end
	
	fun get_curfid(STATE{curfid,...}) = curfid

    fun remove_free(s as (STATE{boundevars,boundcvars,...}),
		    {freecpaths_map,freecpaths,
		     freeepaths_map}) : frees = 
	let 
(*
	    val _ = (print "\n\nremove_free...\n";
		     print "\nfreeevars has ";
		     VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print ", ")) freeevars;
		     print "\nboundevars has ";
		     VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print ", ")) boundevars;
		     print "\n")
*)

	    fun cfilter (v,_) = not (is_boundcvar(s,v))
	    val freecpaths' = List.filter cfilter freecpaths
	    fun epathfilter ((v,_),_) = not (is_boundevar(s,v))
	    fun cpathfilter ((v,_),_) = not (is_boundcvar(s,v))
	    val freeepaths_map' = PathMap.filteri epathfilter freeepaths_map
	    val freecpaths_map' = PathMap.filteri cpathfilter freecpaths_map
(*
	    val _ = (print "\nfreeevars' has ";
		     VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print ", ")) freeevars';
		     print "\n")
*)
	in  {freecpaths = freecpaths',
	     freeepaths_map = freeepaths_map',
	     freecpaths_map = freecpaths_map'}
	end

    in  type state = state
	fun get_ctxt (STATE{ctxt,...}) = ctxt
	type funentry = funentry
	val copy_state = copy_state
	val show_state = show_state
	val free_cvar_add = free_cvar_add
	val free_evar_add = free_evar_add
	val free_epath_add = free_epath_add
	val cvar_isfree = cvar_isfree
	val evar_isfree = evar_isfree
	val epath_isfree = epath_isfree
	val is_boundfid = is_boundfid
	val add_gboundevar = add_gboundevar
	val add_gboundcvar = add_gboundcvar
	val add_boundevar = add_boundevar
	val add_boundcvar = add_boundcvar
	val add_boundcvar' = add_boundcvar'
	val add_boundevars = add_boundevars
	val add_boundcvars = add_boundcvars false
	val add_boundfids = add_boundfids
	val get_curfid = get_curfid
	val remove_free = remove_free
	val is_boundevar = is_boundevar
	val is_boundcvar = is_boundcvar


    (* ----- Global data structure to keep track of function information ------------ *)

	fun reset_table escapes = (fids := empty_table;
				   global_escapes := VarSet.addList(VarSet.empty,escapes))
	fun get_fids() = VarMap.foldli (fn (fid,_,acc) => VarSet.add(acc,fid)) VarSet.empty (!fids)
	fun is_fid f = (case (VarMap.find(!fids,f)) of NONE => false | _ => true)
	fun add_fun new_fid = 
	    (let val escape = VarSet.member(!global_escapes,new_fid)
	     in  fids := (VarMap.insert(!fids,new_fid,ref (empty_fun escape new_fid)))
	     end)
	    
	fun initial_state topfid = let val _ = add_fun topfid
				   in   STATE{ctxt = NilContext.empty,
					      is_top = true,
					      curfid=topfid,boundfids=VarSet.empty,
					      boundevars = VarMap.empty,
					      boundcvars = VarMap.empty}
				   end

	fun add_escape esc_fid = 
	    (case (VarMap.find(!fids,esc_fid)) of
		 NONE => error ((Name.var2string esc_fid) ^ "fid not found for add_escape")
	       | SOME (r as (ref{static,escape,callee,frees})) =>
		     r := {static = static,
			   escape = true,
			   callee = callee,
			   frees = frees})
	fun add_callee (caller,callee_fid,state) = 
	    (case (VarMap.find(!fids,caller)) of
		 NONE => error ((Name.var2string caller) ^ "fid not found for add_callee")
	       | SOME (r as (ref{static,escape,callee,frees})) =>
		     r := {static = static,
			   escape = escape,
			   callee = (callee_fid,state)::callee,
			   frees = frees})
	fun get_callee caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_callee")
				   | SOME (r as (ref{callee,...})) => callee)
	fun get_frees  caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_frees")
				   | SOME (r as (ref{frees,...})) => frees)
	fun get_static caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_static")
				   | SOME (ref{static,...}) => static)
	fun get_escape caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_escape")
				   | SOME (ref{escape,...}) => escape)

	fun add_frees (fid,f) =
	    (case (VarMap.find(!fids,fid)) of
		 NONE => error ((Name.var2string fid) ^ "fid not found for add_frees")
	       | SOME (r as (ref{static,escape,callee,frees : frees})) =>
		     r := {static = static,
			   escape = escape,
			   callee = callee,
			   frees = join_free(frees,f) : frees})

	fun augment_frees (fid,f) =
	    (case (VarMap.find(!fids,fid)) of
		 NONE => error ((Name.var2string fid) ^ "fid not found for aug_frees")
	       | SOME (r as (ref{static,escape,callee,frees})) =>
		     let (* val _ = (print "augment_frees called on fid = ";
				  Ppnil.pp_var fid; print "\nwith\n";
				  print "evars are: "; 
				  VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print " ")) evars; print "\n";
				  print "cvars are: "; 
				  VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print " ")) cvars; 
				  print "\n")  *)
			 val (same,frees) = join_free'(frees,f)
		     in  if same 
			     then false
			 else 
			     (r := {static = static,
				    escape = escape,
				    callee = callee,
				    frees = frees}; true)
		     end)
    end




(* ----------------------------------------------------------------------------
   Free variable computation.
   ---------------------------------------------------------------------------- *)

    fun bnd_find_fv (state : state, frees : frees) bnd : state * frees =
	    let
		fun vkfolder((v,k),(f,s)) = (k_find_fv (s,f) k, 
					     add_boundcvar(s,v,k))
		fun vcfolder((v,c),(f,s)) = (t_find_fv (s,f) c, 
					     add_boundevar(s,v,c))
		fun fun_type(Function(effect,recur,vklist,vclist,vflist,body,c)) = 
		    AllArrow_c(Open,effect,vklist,(map #2 vclist),TilWord32.fromInt(length vflist),c)
		fun do_fix frees var_arm_set do_arm do_add get_type =
		    let val var_arm_list = set2list var_arm_set
			val _ = app (fn (fid,_) => do_add fid) var_arm_list
			val local_fids_types = map (fn (v,pf) => (v,get_type pf)) var_arm_list
			val free = (foldl (fn (a,f) => let val f' = do_arm local_fids_types a
					               in  join_free(f,f')
						       end)
				    frees (set2list var_arm_set))
		    in  (add_boundfids false (state, local_fids_types), free)
		    end
		val (state,frees) = 
		    (case bnd of
			 Con_b(v,k,c) => 
			     let val f = c_find_fv (state,frees) c
				 val (state,f) = 
				     if (!use_kind_at_bind)
					 then let val k' = NilUtil.singletonize(NilUtil.kill_singleton k,c)
						  val state = add_boundcvar(state,v,k')
					      in (state, k_find_fv (state,f) k)
					      end
				     else
					 (* con_valid and con_reduce not quite the same! *)
					 let 
					  in  (add_boundcvar'(state,v,c,k), f)
					       (* by not adding the FV's of k,
						we are not closed WRT types
						(pk, k_find_fv (state,f) k) *)
					  end
				 val _ = if (!debug)
					     then (print "add_boundcvar ";
						   Ppnil.pp_var v; print "\n")
					 else ()
			     in (state, f)
			     end
		       | Exp_b(v,c,e) => 
					 let val f = t_find_fv (state,frees) c
					     val f = e_find_fv (state,f) e
					     val _ = if (!debug)
							 then (print "add_boundevar ";
							       Ppnil.pp_var v; print "\n")
						     else ()
					 in (add_boundevar(state,v,c), f)
					 end
		       | Fixopen_b var_fun_set =>
			     let val outer_curfid = get_curfid state
				 fun do_arm fids_types (v,Function(_,_,vklist,vclist,vflist,body,tipe)) =
				 let val outer_state = add_boundfids false (state,fids_types) 
				     val local_state = copy_state state v
				     val local_state = add_boundfids true (local_state,fids_types) 
				     val fs = (empty_frees, local_state)
				     val _ = if (!debug)
						 then (print "state before vkfolder to ";
						       Ppnil.pp_var v; print "\n";
						       show_state (#2 fs); print "\n")
					     else ()
				     val fs = foldl vkfolder fs vklist
				     val _ = if (!debug)
						 then (print "the following (after vkfolder) frees to ";
						       Ppnil.pp_var v; print "\n";
						       show_free (#1 fs); print "\n")
					     else ()
				     val (f,s) = (foldl vcfolder fs 
						  (vclist @ (map (fn v => (v,float64)) vflist)))
				     val _ = if (!debug)
						 then (print "the following (after vcfolder) frees to ";
						       Ppnil.pp_var v; print "\n";
						       show_free f; print "\n")
					     else ()
				     val f = e_find_fv (s,f) body
				     val _ = if (!debug)
						 then (print "the following (after body) frees to ";
						       Ppnil.pp_var v; print "\n";
						       show_free f; print "\n")
					     else ()
				     val f = c_find_fv (s,f) tipe
				     val _ = if (!debug)
						 then (print "adding the following frees to ";
						       Ppnil.pp_var v; print "\n";
						       show_free f; print "\n")
					     else ()
				     val _ = add_frees(v,f)
(*				     val _ = add_callee(outer_curfid,v,outer_state) *)
				 in  f
				 end
				 val (s,f) = do_fix frees var_fun_set do_arm add_fun fun_type
			     in  (s, remove_free(s,f))
			     end
		       | Fixcode_b _ => error "there can't be codes while closure-converting"
		       | Fixclosure_b _ => error "there can't be closures while closure-converting")
	    in  (state, frees)
	    end
	

    and bnds_find_fv (state : state, frees : frees) bnds : state * frees = 
	let fun folder (bnd,(state,frees)) = 
	    let val (state,frees') = bnd_find_fv (state,frees) bnd
(*
			      val _ = (print "=================\nLet_e bnd: processed  ";
				       Ppnil.pp_bnd bnd;
				       print "\nfrees = oldacc = \n";
				       show_free frees;
				       print "\nfrees' = \n";
				       show_free frees';
				       print "\nnewfree = newacc = \n";
				       show_free newfree;
				       print "\n\n\n")
*)
	    in  (state,frees')
	    end
	in  foldl folder (state,frees) bnds
	end






    and e_find_fv arg exp : frees =
	let val res = e_find_fv' arg exp
	    handle e => (print "exp_find_fv called on\n";
			 Ppnil.pp_exp exp;
			 print "\n\n"; raise e)
	in  res 
	end

    and e_find_fv' (state : state, frees : frees) exp : frees =
	(if (!debug)
	     then (print "exp_find_fv called on\n";
		   if (!debug_full)
		       then Ppnil.pp_exp exp
		   else (); print "\n\n")
	 else ();
	 case exp of
	     Var_e v => (if (is_boundfid(state,v)) then add_escape v else ();
		     (case (evar_isfree(state,frees,v)) of
			  NONE => frees
			| SOME (c,r) =>
			      let val frees = free_evar_add(frees,v,c)
				  val f = remove_free(state,
							 (case !r of
							      SOME f => f
							    | NONE => let val f = t_find_fv (state,empty_frees) c
									  val _ = r := SOME f
								      in  f
								      end))
			      in join_free(frees,f)
			      end))
           | Prim_e(NilPrimOp(select l), _, _) =>
	       let (* labels are returned backwards *)
		   val (base,labels) = extract_path exp
		   val do_path = (!do_close_path) andalso (case base of Var_e _ => true | _ => false)
	       in  if not do_path
		       then e_find_fv (state,frees) base
		   else 
		       let val (Var_e v) = base
			   val _ = if (is_boundfid(state,v)) then add_escape v else ()
		       in  (case (epath_isfree(state,frees,(v,labels))) of (* labels are backwards *)
				NONE => frees
			      | SOME (c,r) =>
				    let val fid = (get_curfid state)
					val venv_var = #venv_var(get_static fid)
					val frees = free_epath_add(frees,(v,labels),c)
					val f = (case !r of
						     SOME f => f
						   | NONE => let val f = c_find_fv (state,empty_frees) c
								 val _ = r := SOME f
							     in  f
							     end)
					val f = remove_free(state,f)
				    in join_free(frees,f)
				    end)
		       end
	       end

	   | Prim_e (NilPrimOp np,clist,elist) =>
		 let fun cfold(c,f) = c_find_fv (state,f) c
		     fun efold(e,f) = e_find_fv (state,f) e
		     val uses_carg = 
			 (case np of
			      record _ => false
			    | select _ => false
			    | roll => false
			    | unroll => false
			    | project_sum_record _ => false
			    | project_sum {sumtype, tagcount} =>
				  let 
				      val which_con = TilWord32.toInt (TilWord32.uminus(sumtype,tagcount))
				      val field_con = List.nth(clist, which_con)
				      val irreducible = not(istype_reducible(state,field_con))
				      val _ = if irreducible
					  then (Stats.counter
					       ("toclosure.irreducible-project_sum")(); ())
					      else ()
				  in  irreducible
				  end
			    | _ => true)
		     val frees = if (uses_carg)
				     then foldl cfold frees clist
				 else frees
		     val frees = foldl efold frees elist
		 in  frees
		 end
	   | Prim_e (PrimOp _,clist,elist) =>
		 let fun cfold(c,f) = c_find_fv (state,f) c
		     fun efold(e,f) = e_find_fv (state,f) e
		     val frees = foldl cfold frees clist
		     val frees = foldl efold frees elist
		 in  frees
		 end
	   | Const_e v => 
		 (case v of
		      ((Prim.int _) | (Prim.uint _) | (Prim.float _)) => frees
		    | ((Prim.array (c,array)) | (Prim.vector (c,array))) => 
			  let fun folder(e,f) = e_find_fv (state,f) e
			      val f = Array.foldl folder frees array
			  in  c_find_fv (state,f) c
			  end
		    | Prim.refcell (ref e) => e_find_fv(state,frees) e
		    | Prim.tag (_,c) => c_find_fv (state,frees) c)
	   | Let_e (_,bnds,e) => (* treat as though a sequential let *)
		      let val (state',bndfree) = bnds_find_fv (state,frees) bnds
		      in  e_find_fv (state',bndfree) e
		      end
	   | Switch_e switch => 
		 let
		     fun do_sw {info, arg, arms, default} do_info do_arg do_prearm =
			 let val f = do_info (info,frees)
			     val f = do_arg (arg,f)
			     fun do_arm((pre,Function(_,_,vklist,vclist,vflist,body,tipe)),f) =
				 let 
				     fun vkfolder((v,k),(f,s)) = (k_find_fv (s,f) k,
								  add_boundcvar(s,v,k))
				     fun vcfolder((v,c),(f,s)) =
					 let val f = t_find_fv (s,f) c
					 in (f, add_boundevar(s,v,c))
					 end
				     val (f,s) = (do_prearm (pre,f), state)
				     val (f,s) = foldl vkfolder (f,s) vklist
				     val (f,s) = foldl vcfolder (f,s) 
					 (vclist @ (map (fn v => (v,float64)) vflist))
				     val f = e_find_fv (s,f) body
				     val f = c_find_fv (s,f) tipe
				 in  f
				 end
			     val f = foldl do_arm f arms
			     val f = (case default of
					   NONE => f
					 | SOME e => e_find_fv (state,f) e)
			 in f
			 end
		     fun nothing (s,f) = f
		 in	 case switch of
		     Intsw_e sw => do_sw sw nothing (fn (e,f) => e_find_fv (state,f) e ) nothing
		     (* the info field of a sum is just type information and not constructor *)
		   | Sumsw_e sw => (do_sw sw (fn ((w,clist),f) => (map (t_find_fv (state,f)) clist; f))
				    (fn (e,f) => e_find_fv (state,f) e) nothing)
		   | Exncase_e sw => (do_sw sw nothing (fn (e,f) => e_find_fv (state,f) e) 
				      (fn (e,f) => e_find_fv (state,f) e))
		   | Typecase_e sw => do_sw sw nothing (fn (c,f) => c_find_fv (state,f) c) nothing
		 end
	   | App_e (_, e, clist, elist, eflist) =>
		 let val f = (case e of
				      Var_e v => if (is_fid v andalso eq_var(v,get_curfid state))
						     then (
(*							   print "***** adding callee ";
							   Ppnil.pp_var v; print " to ";
							   Ppnil.pp_var (get_curfid state); print "\n";
*)
							   add_callee(get_curfid state,v,state);
							   frees)
						 else (e_find_fv (state,frees) e)
				    | _ => (e_find_fv (state,frees) e))
		     val f = foldl (fn (c,f) => c_find_fv (state,f) c) f clist
		     val f = foldl (fn (e,f) => e_find_fv (state,f) e) f elist
		     val f = foldl (fn (e,f) => e_find_fv (state,f) e) f eflist
		 in  f
		 end
	   | Raise_e (e,c) => e_find_fv (state, c_find_fv (state,frees) c) e
	   | Handle_e (e,Function(_,_,[],[(v,c)],[],body,tipe)) =>
		 let val f = e_find_fv (state,frees) e
		     val f = c_find_fv (state,f) c
		     val f = e_find_fv (add_boundevar(state,v,c),f) body
		     val f = c_find_fv (state,f) tipe
		 in  f
		 end
	   | Handle_e _ => error "ill-typed Handle_e")


	and c_find_fv (state : state, frees : frees) con : frees =
	    let 
		val _ = if (!debug)
			    then (print "c_find_fv called on\n";
				  if (!debug_full)
				      then Ppnil.pp_con con
				  else (); print "\n\n")
			else ()
		val res = c_find_fv' (state,frees) con 
		handle e => 
		  (print "c_find_fv called on\n";
		   Ppnil.pp_con con; print "\n and with ctxt = ";
		   show_state state; print "\n\n"; raise e)
	    in res
	    end	 

	and t_find_fv (state : state, frees : frees) con : frees =
	    let 

		val res = t_find_fv' (state,frees) con 
		handle e => 
		  (print "t_find_fv called on\n";
		   Ppnil.pp_con con; print "\n and with ctxt = ";
		   show_state state; print "\n\n"; raise e)
	    in res
	    end	 

	and k_find_fv (state : state,frees : frees) kind : frees =
	    let val res = k_find_fv' (state,frees) kind 
		handle e => 
		  (print "k_find_fv called on\n";
		   Ppnil.pp_kind kind; print "\n and with ctxt = ";
		   show_state state; print "\n\n"; raise e)
	    in res
	    end	 


         and istype c = 
	     (case c of
		  Prim_c _ => true
		| Mu_c(_,vcset) => 
		      let val vclist = set2list vcset
		      in  (case vclist of
			       [(_,c)] => istype c
			     | _ => false)
		      end
		| AllArrow_c _ => true
		| Var_c _ => false
		| Typecase_c _ => false
		| Let_c _ => false
		| Crecord_c _ => error "t_find_fv given crecord : wrong kind"
		| Closure_c _ => error "t_find_fv given crecord : wrong kind"
		| App_c _ => false
		| Proj_c _ => false
		| Annotate_c (_,c) => istype c)

    and istype_reducible (state,con) = 
		(istype con orelse istype (NilStatic.con_reduce(get_ctxt state,con)))

	and t_find_fv' (state : state, frees : frees) con : frees =
	    let fun type_case() = (c_find_fv'(state,frees) con; frees)
		fun constructor_case() = c_find_fv'(state,frees) con
		val istype = istype_reducible(state,con)
(*
		val _ = (print "t_find_fv'  istype = "; print (Bool.toString istype); 
			 print "\nunreduced = \n";
			 Ppnil.pp_con con;
			 print "\nreduced = \n";
			 Ppnil.pp_con ((NilStatic.con_reduce(get_ctxt state,con)));
			 print "\n")
*)
	    in  if istype
		    then type_case()
		else constructor_case()
	    end

	and c_find_fv' (state : state, frees : frees) con : frees =
	    (case con of
		 Prim_c (pc,clist) => foldl (fn (c,f)=> (c_find_fv (state,f) c)) frees clist
	       | Mu_c (_,vcset) =>
		     let val vclist = set2list vcset
			 (* we need to alpha-vary since reductions may lead to duplication
			    of bound variables despite A-normal linearization *)
			 val (vclist) = alpha_mu (fn v => is_boundcvar(state,v)) (vclist)
			 val state' = add_boundcvars(state,map (fn (v,_) => (v,NONE,SOME(Word_k Runtime))) vclist)
		     in  foldl (fn ((v,c),f) => c_find_fv (state',f) c) frees (set2list vcset)
		     end
	       (* the types of some primitives like integer equality are Code arrow *)
	       | AllArrow_c((ExternCode | Code | Open | Closure),_,vklist,clist,numfloats,c) =>
		     let fun vkfolder((v,k),(f,s)) = (k_find_fv (s,f) k,
						      add_boundcvar(s,v,k))
			 fun cfolder (c,(f,s)) = (c_find_fv (s,f) c,s)
			 val fs = foldl vkfolder (frees,state) vklist
			 val (f,s) = foldl cfolder fs clist
		     in  c_find_fv (s,f) c
		     end
	       | Var_c v => 
		     (case (cvar_isfree(state,frees,v)) of
			  NONE => frees
			| SOME (k,r) => 
			      let 
(*				  val f = remove_free(state,
						       case !r of
							   SOME f => f
							 | NONE => let val f = k_find_fv (state,empty_frees) k
								       val _ = r := SOME f
								   in  f
								   end)
(* xxxxxxxxxxxx let's not addd classifiers and be not closed WRT types *)
(*				  val frees = join_free(frees,f) *)
				  (* add classifier first *)
*)
				  val k_shape = NilUtil.kill_singleton k
				  val k = Singleton_k(NilUtil.get_phase k_shape, k_shape,Var_c v)
			      in  free_cvar_add(frees,v,k)
			      end)
	       | Typecase_c {arg, arms, default, kind} => 
		     let val f = c_find_fv (state,frees) arg
			 val f = c_find_fv (state,f) default
			 val f = k_find_fv (state,f) kind
			 fun armfolder ((pc,vklist,c),f) = 
			     let fun folder((v,k),(f,s)) = (k_find_fv (s,f) k,
							    add_boundcvar(s,v,k))
				 val (f,_) = foldl folder (f,state) vklist
			     in  c_find_fv (state,f) c
			     end
		     in  foldl armfolder f arms
		     end
	       | Let_c(_,cbnds,c) => 
		     let 
			 fun cb_folder (Con_cb(v,k,c),(f,s)) =
			     let val f' = c_find_fv (s,f) c
				 val (state,f) = 
				     if (!use_kind_at_bind)
					 then let 
						  val s = add_boundcvar(s,v,k)
					      in (s, k_find_fv (s,f') k)
					      end
				     else 
					 (add_boundcvar'(s,v,c,k),f')
			     in (f,state)
			     end
			   | cb_folder (Code_cb(v,vklist,c,k), (f,s)) = (f,s)
			   | cb_folder (Open_cb(v,vklist,c,k), (f,s)) =
			     let val _ = add_fun v
				 fun folder((v,k),(f,s)) = 
				     (k_find_fv (s,f) k,
				      add_boundcvar(s,v,k))
				 val ls = copy_state s v
				 val (f,ls) = foldl folder (f,ls) vklist
				 val f' = k_find_fv (ls,f) k
				 val f'' = c_find_fv (ls,f') c
				 val _ = add_frees(v,f'')
				 val ak = Arrow_k(Open,vklist,k)
			     in  (remove_free(s,f''), add_boundcvar(s,v,ak))
			     end
			 val (f,s) = foldl cb_folder (frees,state) cbnds
			 val f' = c_find_fv (s,f) c
(*			 val f' = remove_free(s,f') *)
		     in  f'
		     end
	       | Crecord_c lclist => foldl (fn ((_,c),f) => c_find_fv (state,f) c) frees lclist
	       | Proj_c (c,l) => c_find_fv (state,frees) c
	       | Closure_c (c1,c2) => c_find_fv (state, c_find_fv (state,frees) c1) c2
	       | App_c(c,clist) => let val f = c_find_fv (state,frees) c
				   in foldl (fn (c,f) => c_find_fv (state,f) c) f clist
				   end
	       | Annotate_c (_,c) => c_find_fv (state,frees) c)
		 
	and k_find_fv' (state,frees) kind : frees =
	    (case kind of
	    ((Type_k _) | (Word_k _)) => frees
	  | (Singleton_k (p,k,c)) => (k_find_fv (state,frees) k; (c_find_fv (state,frees) c))
	  | (Record_k lv_kind_seq) =>
		 let fun folder (((l,v),k),(f,s)) = (k_find_fv (s,f) k, add_boundcvar(s,v,k))
		 in  #1(foldsequence folder (frees,state) lv_kind_seq)
		 end
	  | (Arrow_k (_,vklist,k)) =>
		let fun folder ((v,k),(f,s)) = (k_find_fv (s,f) k, add_boundcvar(s,v,k))
		    val (f,s) = foldl folder (frees,state) vklist
		in  k_find_fv (s,f) k
		end)



   (* ------- compute the final free-variable list by closing over the callgraph ------ *)

   local
       fun close_fun (curfid,nextset) = 
	   let val callees = get_callee curfid
(* val _ = VarSet.app (fn x => (print "\n    callee_fvs is: "; 
                                   show_free (get_frees x); print "\n")) callees *)
(* val callee_fvs = VarSet.foldl 
 (fn (f,fv) => join_free(get_frees f, fv)) empty_frees callees *)
	       val callee_fvs = foldl (fn ((fid,s),fv) => let val f = get_frees fid
							       val f = remove_free(s,f)
							   in  join_free(f, fv)
							   end) empty_frees callees 
(* val _ = (print "\n\nfinal callee_fvs is: "; show_free callee_fvs; print "\n") *)
	       val changed = augment_frees(curfid,callee_fvs)
	   in if changed
		  then VarSet.add(nextset,curfid)
	      else nextset
	   end
   in  fun close_funs workset = 
       let  val _ = if (!debug)
			then (print "before close_funs\n";
			      VarSet.app (fn fid => (print ("fid = ");
						     Ppnil.pp_var fid; print " --   callees are "; 
						     app (fn (fid,s) => (print "  ";
									 Ppnil.pp_var fid; print " -> ";
									 show_state s;
									 print "\n"))
						     (get_callee fid);
						     print "\n";
						     show_free(get_frees fid); print "\n\n")) (get_fids());
			      print "\n\n")
		    else ()
	    fun loop() = 
		let 
		    val nextset = (VarSet.foldl close_fun VarSet.empty workset)
		in  if (VarSet.isEmpty nextset)
			then ()
		    else loop()  (* note that we must start with the whole set again *)
		end
	    val _ = loop()
	    val _ = if (!debug)
			then (print "after all close_funs\n";
			      VarSet.app (fn fid => (print ("fid = ");
						     Ppnil.pp_var fid; print "\n"; 
						     show_free(get_frees fid))) (get_fids());
			      print "\n\n")
		    else ()
	    val _ = if !closure_print_free 
			then VarSet.app 
			    (fn fid => 
			     let val {freeepaths_map,freecpaths,...} = get_frees fid
			     in  (print ("fid = ");
				  Ppnil.pp_var fid; print "  (#free-cvars , #free-evars) =   ";
				  print (Int.toString (length freecpaths));
				  print ", ";
				  print (Int.toString (PathMap.numItems freeepaths_map));
				  print "\n")
			     end) (get_fids())
		    else ()
       in  ()
       end
   
   end





   (* ------- rewrite each term-level binding to a list of bindings ---------------------
      In general, each function is re-written to one or more functions
      and zero or more closures.  No closures are needed and generated
      if the function does not escape.

      ----- Function bindings are rewritten as follows.
      
      f == Function(vargs, fargs) = body --->
      
      [fcode == Pfunction(cargs @ cfv, vargs @ vfv, fargs) = body
       funpack == Pfunction(cargs @ [cenv], vargs @ [venv], fargs) = body
       f == Closure(fcode,cenv,venv)]

       ------------------------------------------------------------------------ *)

   fun cproj cvar i = Proj_c(Var_c cvar, generate_tuple_label(i+1))
   fun eproj (evar,rectype) i = Prim_e(NilPrimOp(select (generate_tuple_label(i+1))),
				       if (!select_has_con) then [rectype] else [],
				       [Var_e evar])


   local 
       datatype state = STATE of {curfid : var}
   in
       fun copy_state (STATE{...},fid) = STATE{curfid = fid}
       fun show_state(STATE{curfid}) = (print "state = "; Ppnil.pp_var curfid; print "\n")
       fun new_state curfid = STATE{curfid = curfid}
       fun current_fid (STATE{curfid}) = curfid
       type state = state
   end

   (* If there is one free variable, then don't bother tupling it up.
      Notice that we must do this consistently at the definition and call sites. 
      If there are no free variables, use empty record as the free environment.
      In either case, we get to not have the unpack function. 

      We perform this optimization only if both the constructor AND term level
      free variables are zero or one in number.
    *)

   fun fun_rewrite state vars (v,Function(effect,recur,vklist,vclist,vflist,body,tipe)) = 
       let 
	   val _ = if (!debug)
		       then (print "\nfun_rewrite v = "; Ppnil.pp_var v; print "\n")
		   else ()
	   val {code_var, unpack_var, cenv_var, venv_var, ...} = get_static v
	   val {freecpaths,freecpaths_map,freeepaths_map=pc_free,...} = get_frees v
	   (* was kept in reverse order *)
	   val vkl_free = map (fn p => let val SOME(v,_,k) = PathMap.find(freecpaths_map,p)
					  val l = Name.internal_label(Name.var2string v)
				      in  (v,k,l)
				      end)
	                     (rev freecpaths)
	   val vklist = map (fn (v,k) => (v,k_rewrite state k)) vklist
	   val vclist = map (fn (v,c) => (v,c_rewrite state c)) vclist
	   val escape = get_escape v
	   val vkl_free = map (fn (v,k,l) => (v,k_rewrite state k,l)) vkl_free
	   val pc_free = let val temp = PathMap.listItemsi pc_free
			     fun mapper((v,labs),(v',ethunk,c)) = 
				 let val l = Name.internal_label(Name.var2string v')
				 in ((v,labs), v', l, c_rewrite state c)
				 end
			 in  map mapper temp
			 end
	   val is_recur = Listops.orfold 
	                    (fn v => Listops.orfold (fn ((v',_),_,_,_) => Name.eq_var(v,v')) pc_free) vars

	   val _ = if (!debug)
		       then (print "fun_rewrite v = "; Ppnil.pp_var v;
			     print "\nvkl_free are "; 
			     app (fn (v,_,_) => (Ppnil.pp_var v; print ", ")) vkl_free;
			     print "\nvc_free are "; 
			     app (fn ((v,labs),_,_,_) => (Ppnil.pp_var v; app Ppnil.pp_label labs;
							print ", ")) pc_free;
			     print "\n")
		   else ()
	   val num_vkl_free = length vkl_free
	   val num_pc_free = length pc_free
	   val cenv_kind = let fun mapper(v,k,l) = ((l,Name.derived_var v),k)
			       val lvk_list = map mapper vkl_free
			   in  Record_k(Util.list2sequence lvk_list)
			   end
	   val venv_type = let val types = map #4 pc_free
			       val labs = map #3 pc_free
			   in  if (!do_single_venv andalso length labs = 1)
				   then hd types
			       else Prim_c(Record_c labs, types)
			   end
	   val vklist_code = vklist @ (map (fn (v,k,l) => (v,k)) vkl_free)
	   val vclist_code = vclist @ (map (fn (path,v',_,c) => (v',c)) pc_free)
	   val cenv_kind = 
	       let 
		   fun loop _ acc [] = Record_k (list2set (rev acc))
		     | loop subst acc (((v',_,_),((l,v),k))::rest) = 
		       let val k' = Subst.substConInKind (Subst.fromList subst) k
			   val acc' = ((l,v),k')::acc
			   val subst' = (v',Var_c v)::subst
		       in  loop subst' acc' rest
		       end
	       in  (case cenv_kind of
			Record_k lvk_set => loop [] [] (Listops.zip vkl_free (set2list lvk_set))
		      | _ => error "cenb_kind not a record_k")
	       end
	   val subst_list = 
	     let 
		 fun loop n acc [] = rev acc
		   | loop n acc ((v',_,l)::rest) =
		     let val new = (v',Proj_c(Var_c cenv_var, l))
		     in  loop (n+1) (new::acc) rest 
		     end
	     in  loop 1 [] vkl_free
	     end
	   val subst = Subst.fromList subst_list
	   val vklist_unpack_almost = map (fn (v,k) => (v,Subst.substConInKind subst k)) vklist
	   val vklist_unpack = vklist_unpack_almost @ [(cenv_var,cenv_kind)]
	   val vclist_unpack_almost = vclist @ [(venv_var,venv_type)]
	   val vclist_unpack = map (fn (v,c) => (v,Subst.substConInCon subst c)) vclist_unpack_almost
	   val codebody_tipe = c_rewrite state tipe
	   val closure_tipe = AllArrow_c(Closure,effect,
					 vklist,map #2 vclist,
					 TilWord32.fromInt(length vflist),codebody_tipe)
	   val codebody_tipe = Subst.substConInCon subst codebody_tipe

	   val code_body = (e_rewrite (copy_state(state,v)) body)

	   fun pc_mapper((v,l,_),c) = 
	       let 
		   val e = path2exp (v,l)
		   val e = e_rewrite state e
	       in  (e,c)
	       end
	   val cenv = let val lc_list = map (fn (v,_,l) => 
					     let val c = c_rewrite state (Var_c v)
					     in  (l,c)
					     end) vkl_free
		      in  Crecord_c lc_list
		      end
	   val venv = let val labels = map #3 pc_free
			  val clist = map #4 pc_free
			  val elist = map (fn (p,_,_,_) => e_rewrite state (path2exp p)) pc_free
		      in  if (!do_single_venv andalso length labels = 1)
			      then hd elist
			  else Prim_e(NilPrimOp(record labels),clist,elist)
		      end

	   val code_fun = Function(effect,recur,
				   vklist_unpack,vclist_unpack,vflist,
				   code_body, codebody_tipe)

	   val closure = {code = code_var,
			  cenv = cenv,
			  venv = venv,
			  tipe = closure_tipe}

       in if escape then (is_recur,
			  [(code_var,code_fun)],
			  [(v,closure)])
	  else (false,[(code_var,code_fun)],[])
       end



   and bnd_rewrite state bnd : bnd list =
       let
	   fun funthing_helper rewriter var_thing_set =
	       let 
		   val var_thing_list = set2list var_thing_set
		   val vars = map #1 var_thing_list
		   val pfun_close = map (rewriter vars) var_thing_list
		   val is_recur = Listops.orfold (fn (x,_,_) => x) pfun_close
		   val pfun_list = List.concat(map #2 pfun_close)
		   val pfun_bnd = Fixcode_b (list2set pfun_list)
		   val closure_bnd_list = 
		       (case (List.concat(map #3 pfun_close)) of
			    [] => []
			  | close_list => [Fixclosure_b(is_recur,list2set close_list)])
	       in  pfun_bnd :: closure_bnd_list
	       end
       in (case bnd of
		(Con_b(v,k,c)) => if (!use_kind_at_bind)
				      then [Con_b(v,k_rewrite state k, c_rewrite state c)]
				  else let val kk = NilUtil.kill_singleton k
					   val k = (k_rewrite state kk)
					       handle e => 
						   (print "k_rewrite failed on k = !\n";
						    Ppnil.pp_kind k; print "\n";
						    print "k_rewrite failed on singletonless = !\n";
						    Ppnil.pp_kind kk; print "\n";
						    raise e)
				       in  [Con_b(v,k, c_rewrite state c)]
				       end
	      | (Exp_b(v,c,e)) => [Exp_b(v,c_rewrite state c, e_rewrite state e)]
	      | (Fixclosure_b _) => error "there can't be closures while closure-converting"
	      | (Fixcode_b _) => error "there can't be codes while closure-converting"
	      | (Fixopen_b var_fun_set) => funthing_helper (fun_rewrite state) var_fun_set)
       end



   and e_rewrite state arg_exp : exp =
       let (* we do not copy_state for arms since they are not first-class *)
	   fun armfun_rewrite (Function(ef,re,vklist,vclist,vflist,e,c)) =
	   Function(ef,re,map (fn (v,k) => (v, k_rewrite state k)) vklist,
		    map (fn (v,c) => (v, c_rewrite state c)) vclist,
		    vflist, e_rewrite state e, c_rewrite state c)
	   (* there are no function definitions within e_rewrite so we use same state *)
	   val e_rewrite = e_rewrite state
	   val c_rewrite = c_rewrite state
	   fun path_case (v,labels) = 
	       let val fid = current_fid state
(*		   val _ = (print "path_case: "; Ppnil.pp_var fid; print "\n") *)
		   val {freeepaths_map,...} = get_frees fid
		   val {venv_var,...} = get_static fid
	       in  (case PathMap.find(freeepaths_map,(v,labels)) of
			NONE => arg_exp (* was not free *)
		      | SOME (_,ethunk,_) => ethunk(PathMap.numItems freeepaths_map,venv_var))
	       end

       in
       (case arg_exp of
	    Var_e v => path_case(v,[])
	  | Prim_e(NilPrimOp(select l), _, _) =>
	       let (* labels are returned backwards *)
		   val (base,labels) = extract_path arg_exp
		   val do_path = (!do_close_path) andalso (case base of Var_e _ => true | _ => false)
	       in  if not do_path
		       then project(e_rewrite base, labels)
		   else
		       let val (Var_e v) = base
		       in  path_case(v,labels)
		       end
	       end
	  | Prim_e(p,clist,elist) => Prim_e(p,map c_rewrite clist, map e_rewrite elist)

	  | Const_e v => arg_exp
	  | Switch_e switch => 
		Switch_e(case switch of 
		     Intsw_e{info,arg,arms,default} => 
			 Intsw_e{info=info, arg=e_rewrite arg,
				 arms=map (fn (w,f) => (w,armfun_rewrite f)) arms,
				 default=Util.mapopt e_rewrite default}
		   | Sumsw_e{info=(w,clist),arg,arms,default} => 
			 Sumsw_e{info=(w,map c_rewrite clist), arg=e_rewrite arg,
				 arms=map (fn (w,f) => (w,armfun_rewrite f)) arms,
				 default=Util.mapopt e_rewrite default}
		   | Exncase_e{info=info,arg,arms,default} => 
			 Exncase_e{info=info, arg=e_rewrite arg,
				 arms=map (fn (e,f) => (e_rewrite e,armfun_rewrite f)) arms,
				 default=Util.mapopt e_rewrite default}
		   | Typecase_e{info=info,arg,arms,default} => 
			 Typecase_e{info=info, arg=c_rewrite arg,
				 arms=map (fn (pc,f) => (pc,armfun_rewrite f)) arms,
				 default=Util.mapopt e_rewrite default})
			 
	  | Let_e(letsort,bnds,e) => let val bnds_list = map (bnd_rewrite state) bnds
				     in Let_e(letsort, List.concat bnds_list, e_rewrite e)
				     end
	  | App_e (Closure, _,_,_,_) => error "App_e(Code|Closure,...) during closure conversion"
	  | App_e (ar as (ExternCode | Code), e, clist, elist, eflist) => 
		let val clist' = map c_rewrite  clist
		    val elist' = map e_rewrite  elist
		    val eflist' = map e_rewrite eflist
		    val e' = e_rewrite e
		in  App_e(ar, e', clist', elist', eflist')
		end
	  | App_e (Open, e, clist, elist, eflist) => 
		let val clist' = map c_rewrite  clist
		    val elist' = map e_rewrite  elist
		    val eflist' = map e_rewrite eflist
		    fun docall (cv,{freecpaths=vk_free, freeepaths_map=pc_free, ...} : frees) = 
			let val vk_free = rev vk_free
			    val pc_free = PathMap.listItemsi pc_free
			    val {freeepaths_map,...} = get_frees(current_fid state)
			    val {cenv_var,venv_var,...} = get_static (current_fid state)
			    val clist'' = clist' @ [Var_c cenv_var] 
			    val elist'' = elist' @ [Var_e venv_var] 
			in App_e(Code, Var_e cv, clist'', elist'', eflist')
			end
		    fun default() = App_e(Closure,e_rewrite e, clist', elist', eflist')
		in  (case e of
			 Var_e v => if (is_fid v andalso eq_var(v,current_fid state))
					then let val {code_var,...} = get_static v
					     in  docall(code_var,get_frees v)
					     end
				    else default()
		       | _ => default())
		end
	  | Raise_e (e,c) => Raise_e(e_rewrite e, c_rewrite c)
	  | Handle_e (e,f) => Handle_e(e_rewrite e, armfun_rewrite f))
       end

   and cbnd_rewrite state (Con_cb(v,k,c)) : conbnd list = 
           if (!use_kind_at_bind)
	       then [Con_cb(v,k_rewrite state k, c_rewrite state c)]
	   else [Con_cb(v,k_rewrite state (NilUtil.kill_singleton k),
			c_rewrite state c)]
     | cbnd_rewrite state (Open_cb(v,vklist,c,k)) = 
         let val _ = if (!debug)
			 then (print "cbnd_rewrite v = "; Ppnil.pp_var v; print "\n")
		     else ()
	     val (code_var,vkl_free) = 
		 if (is_fid v)
		     then let val {code_var, ...} = get_static v
			      (* freeevars/freeepaths must be empty *)
			      val {freecpaths,freecpaths_map,...} = get_frees v 
			      val vkl_free = 
				  map (fn p => let val SOME(v,_,k) = PathMap.find(freecpaths_map,p)
						   val l = Name.internal_label(Name.var2string v)
					       in  (v,k,l)
					       end)
				  freecpaths
			  in  (code_var, vkl_free)
			  end
		 else  (Name.fresh_named_var "unclosed_open_cb",[])
	     val vkl_free = rev vkl_free
	     val cfv_var = fresh_named_var "free_cons"

	     val k = k_rewrite state k
	     val subst_list = 
	       let 
		 fun loop n acc [] = rev acc
		   | loop n acc ((v',_,l)::rest) =
		     let val new = (v',Proj_c(Var_c cfv_var, l))
		     in  loop (n+1) (new::acc) rest 
		     end
	       in  loop 1 [] vkl_free
	       end
	     val subst = Subst.fromList subst_list
	     val k = Subst.substConInKind subst k


	     fun get_cbnd (v,k,l) = Con_cb(v,k, Proj_c(Var_c cfv_var,l))
	     val vklist = map (fn (v,k) => (v,k_rewrite state k)) vklist
	     val vkl_free = map (fn (v,k,l) => (v,k_rewrite state k,l)) vkl_free
	     val vkl_free_kind = Record_k(map
					  (fn (v,k,l) => ((l, v), k))
					  vkl_free)
	     val cbnds = map get_cbnd vkl_free
	     val vklist' = vklist @ [(cfv_var, vkl_free_kind)]
	     val code_cb = Code_cb(code_var, vklist',
				   letc(cbnds,(c_rewrite state c)), k)
	     val con_env = Crecord_c(map (fn (v,_,l) => (l,Var_c v)) vkl_free)
	     val k' = Subst.substConInKind 
			(Subst.fromList [(cfv_var,con_env)]) k
	     val closure_cb = Con_cb(v,Arrow_k(Closure,vklist,k'),
				   Closure_c (Var_c code_var, con_env))
	 in  [code_cb, closure_cb]
	 end			
     | cbnd_rewrite state (Code_cb _) = error "found Code_cb during closure-conversion"

   and c_rewrite state arg_con : con = 
       let val c_rewrite = c_rewrite state
	   fun path_case (v,labels) = 
	   let val fid = current_fid state
(*	       val _ = (print "Var_c: path_case: "; Ppnil.pp_var fid; print ":  ") *)
	       val {freecpaths_map,...} = get_frees fid
	       val {cenv_var,...} = get_static fid
	       val res = (case PathMap.find(freecpaths_map,(v,labels)) of
			      NONE => arg_con
			    | SOME (_,cthunk,_) => cthunk cenv_var)
(*	       val _ = (show_path (v,labels); print " --> "; 
			Ppnil.pp_con res; print "\n") *)
	   in  res
	   end
       in
	(case arg_con of
            Prim_c (primcon,clist) => Prim_c(primcon, map c_rewrite clist)
	  | Mu_c (ir,vc_set) => Mu_c(ir,Util.list2set 
					(map (fn (v,c) => (v,c_rewrite c)) (Util.set2list vc_set)))
	  | Var_c v => path_case(v,[])
	  | AllArrow_c (ar as (Open | ExternCode | Code),effect,vklist,clist,numfloats,c) => 
		let val vklist' = map (fn(v,k) => (v,k_rewrite state k)) vklist
		    val ar' = (case ar of
				   Open => Closure
				 | Code => Code
				 | ExternCode => ExternCode
				 | Closure => error "control can't reach here")
		in  AllArrow_c(ar',effect,vklist',
			       map c_rewrite clist,
			       numfloats,c_rewrite c)
		end
	  | AllArrow_c (Closure,_,_,_,_,_) => error ("AllArrow_c(Closure,...) " ^ 
						     "during closure-conversion")
	  | Let_c (letsort,cbnds,c) => 
		let val cbnds' = List.concat(map (cbnd_rewrite state) cbnds)
		    val c = c_rewrite c
		in  (case (c,cbnds') of
			 (_,[]) => c
		       | (Var_c v, [Con_cb(v',_,c')]) => if (eq_var(v,v'))
							     then c'
							 else Let_c(letsort,cbnds',c)
		       | _ => Let_c(letsort,cbnds',c))
		end
	  | Typecase_c {arg,arms,default,kind} => 
		Typecase_c{arg = c_rewrite arg,
			   arms = map (fn (pc,vklist,c) => (pc,map (fn (v,k) => (v,k_rewrite state k)) vklist,
							    c_rewrite c)) arms,
			   default = c_rewrite default,
			   kind = k_rewrite state kind}
	  | Crecord_c lclist => Crecord_c(map (fn (l,c) => (l,c_rewrite c)) lclist)
	  | Proj_c (c,l) => Proj_c(c_rewrite c, l)
	  | Closure_c (c1,c2) => error "should not encounter Closure_c during closure-conversion"
	  | App_c (c,clist) => App_c(c_rewrite c, map c_rewrite clist)
	  | Annotate_c (annot,c) => Annotate_c(annot,c_rewrite c))
       end



  and k_rewrite state arg_kind : kind =
       (case arg_kind of 
	    ((Type_k _) | (Word_k _)) => arg_kind
	  | (Singleton_k (p,k,c)) =>  Singleton_k (p,k_rewrite state k, c_rewrite state c)
	  | (Record_k lvkseq) => let val lvklist = Util.sequence2list lvkseq
				     val lvklist = map (fn ((l,v),k) => ((l,v),k_rewrite state k)) lvklist
				 in  Record_k(Util.list2sequence lvklist)
				 end
	  | Arrow_k (Open,vklist,k) => 
		let val vklist' = map (fn (v,k) => (v,k_rewrite state k)) vklist
		    val k' = Arrow_k(Closure, vklist', k_rewrite state k)
		in  k'
		end
	  | Arrow_k _ => error ("cannot encounter arrow_k(Code/Closure,...) " ^ 
				"during closure-conversion"))




   fun close_mod (MODULE{bnds, imports, exports}) = 
       let val _ = reset_table []
	   val top_fid = fresh_named_var "top_fid"
	   val state = initial_state top_fid

	   (* Scan module for free variables *)
	   fun import_folder (ImportValue(l,v,c),state) = 
	       let val f = c_find_fv (state,empty_frees) c
	       in  add_gboundevar(state,v)
	       end
	     | import_folder (ImportType(l,v,k),state) = 
	       let val f = k_find_fv (state,empty_frees) k
	       in  add_gboundcvar(state,v,k)
	       end
	   val state = foldl import_folder state imports
	   val _ = chat "Closure conversion: Scanned imports\n"

	   val (state,{freeepaths_map,freecpaths,...}) = bnds_find_fv (state,empty_frees) bnds
	   val _ = chat "Closure conversion: Scanned bnds\n"

	   fun export_mapper state (ExportValue(l,e,c)) = (e_find_fv (state, empty_frees) e; 
							   c_find_fv (state, empty_frees) c)
	     | export_mapper state (ExportType(l,c,k)) = (c_find_fv (state, empty_frees) c; 
							  k_find_fv (state, empty_frees) k)
	   val _ = map (export_mapper state) exports
	   val _ = chat "Closure conversion: Scanned exports\n"


           (* Perform transitive closure computation *)
	   val _ = if (!debug)
		       then (print "Done with e_find_fv\n";
			     print "free is empty: ";
			     print (Bool.toString (PathMap.numItems freeepaths_map = 0
						   andalso length freecpaths = 0));
			     print "\n")
		   else ()
	   val _ = close_funs(get_fids())
	   val _ = if (!debug)
		       then print "close_mod: Done with close_funs\n"
		   else ()

	   val _ = chat "Closure conversion: Performed transitive closure of close funs\n"


	   (* Rewrite module *)
	   val initial_state = new_state top_fid

	   fun import_mapper (ImportValue(l,v,c)) =
	       let val c = c_rewrite initial_state c
	       in  ImportValue(l,v,c)
	       end
	     | import_mapper (ImportType(l,v,k)) =
	       let val k = k_rewrite initial_state k
	       in  ImportType(l,v,k)
	       end
	   val imports' = map import_mapper imports
	   val _ = chat "Closure conversion: Rewritten imports\n"


	   fun folder (bnd,acc) = 
	       let val bnds = bnd_rewrite initial_state bnd
	       in  acc @ bnds
	       end
	   val bnds' = (Stats.subtimer("toclosure_rewrite_bnds",foldl folder [])) bnds
	   val _ = chat "Closure conversion: Rewritten bindings\n"

	   fun export_rewrite (ExportValue(l,e,c)) = ExportValue(l,e_rewrite initial_state e,
								 c_rewrite initial_state c)
	     | export_rewrite (ExportType(l,c,k)) = ExportType(l,c_rewrite initial_state c,
							       k_rewrite initial_state k)
	   val exports' = map export_rewrite exports
	   val _ = chat "Closure conversion: Rewritten exports\n"

       in  MODULE{bnds = bnds', imports = imports', exports = exports'}
       end	   


end



