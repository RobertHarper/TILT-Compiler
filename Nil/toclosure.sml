(*$import NIL NILCONTEXT NILUTIL PPNIL NILSUBST TOCLOSURE Stats Listops Bool NORMALIZE *)

(* Closure conversion is accomplished in two phases.  
   The first phase scans the program for free type and term variables of 
   all type and term functions and also notes whether a function escapes or not.  
   The second phase then rewrites the functions into codes and closures.  
   Thus, before closure conversion, there are Fixopen_b and App_e(Open,...).
   After closure conversion, there are Fixcode_b, Fixclosure_b, 
   App_e(Closure,...), and App_e(Code,...).
*)

functor ToClosure(structure Normalize : NORMALIZE
		  structure NilContext : NILCONTEXT
		  structure NilUtil : NILUTIL
		  structure Ppnil : PPNIL
		  structure Subst : NILSUBST
		      sharing type NilContext.context = Normalize.context)
    :> TOCLOSURE =
struct


    val do_close_path = Stats.bool("Closure_TermPath")
    val do_close_cpath = Stats.bool("Closure_TypePath")
    val do_single_venv = Stats.bool("Closure_TermCompress")
    val _ = do_close_path := false
    val _ = do_close_cpath := false
    val _ = do_single_venv := true




    val closure_print_free = Stats.bool "closure_print_free"


    open Util NilUtil Name Nil Ppnil Listops
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
		  freeepaths_map : (var * (int * var -> exp) * bool * con) PathMap.map}

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
    fun eq_vpath((v1,l1),(v2,l2)) = 
	eq_var(v1,v2) andalso Listops.eq_list(Name.eq_label,l1,l2)

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


    fun projectc (base, labs) = 
	let fun loop [] = base
	      | loop (l::rest) = Proj_c(loop rest,l)
	in  loop labs 
	end
    fun extract_cpath c = 
	let fun loop acc (Proj_c(c,l)) = loop (l::acc) c
	      | loop acc c = (c,rev acc)
	in loop [] c
	end

    fun path2con(var,labs) = projectc(Var_c var, labs)



    fun reduce_hnf str = Stats.subtimer(str,Normalize.reduce_hnf)


    (* -------- code to perform the initial free-variable computation ---------------- *)
    local
	datatype expentry = GLOBALe | LOCALe of (unit -> bool) * con * frees option ref 
	                            | SHADOWe of (unit -> bool) * con * frees option ref
	datatype conentry = GLOBALc | LOCALc of (unit->kind) * frees option ref 
	                            | SHADOWc of (unit->kind) * frees option ref
	datatype state = STATE of {curfid : fid * fid list,
				   is_top : bool,
				   ctxt : Normalize.context,
				   boundevars : expentry VarMap.map,
				   boundcvars : conentry VarMap.map,
				   boundfids : FidSet.set}
	type funentry = {static : {fid : fid, 
				   fidtype_var : var,
				   code_var : fid,
				   unpack_var : fid,
				   cenv_var : var,
				   venv_var : var},
			 escape : bool,
			 callee : (fid * state) list,
			 frees : frees}


	val empty_table = VarMap.empty : (funentry ref) VarMap.map
	val fids = ref empty_table
	fun is_fid f = (case (VarMap.find(!fids,f)) of NONE => false | _ => true)

	fun get_static caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_static")
				   | SOME (ref{static,...}) => static)


	fun add_gboundevar (STATE{ctxt,is_top,curfid,boundevars,boundcvars,boundfids},v,c) = 
	    let val boundevars' = VarMap.insert(boundevars,v,GLOBALe)
		val ctxt = NilContext.insert_con(ctxt,v,c)
	    in  STATE{is_top = is_top,
		      curfid = curfid,
		      ctxt = ctxt,
		      boundevars = boundevars',
		      boundcvars = boundcvars,
		      boundfids = boundfids}
	    end
	fun add_gboundcvar (STATE{ctxt,is_top,curfid,boundevars,boundcvars,boundfids},v,k) = 
	    let val boundcvars' = VarMap.insert(boundcvars,v,GLOBALc)
		val ctxt' = NilContext.insert_kind(ctxt,v,k)
	    in  STATE{ctxt = ctxt',
		      is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars,
		      boundcvars = boundcvars',
		      boundfids = boundfids}
	    end

	fun add_boundevars' shadow (STATE{ctxt,is_top,curfid,boundevars,boundcvars,boundfids},vc_list) = 
	    let fun folder((v,c),m) = 
		let fun hnf() = #1(reduce_hnf "toclosure_hnf1" (ctxt,c))
		    val entry = if shadow then SHADOWe(hnf,c,ref NONE)
				else (if is_top then GLOBALe
				      else LOCALe(hnf,c,ref NONE))
		in  VarMap.insert(m,v,entry)
		end
		val boundevars' = foldl folder boundevars vc_list
		val ctxt' = foldl (fn ((v,c),ctxt) => NilContext.insert_con(ctxt,v,c)) 
		                  ctxt vc_list
	    in  STATE{ctxt = ctxt',
		      is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars',
		      boundcvars = boundcvars,
		      boundfids = boundfids}
	    end
	val add_boundevars = add_boundevars' false

	fun add_boundcvars (STATE{ctxt,is_top,curfid,boundevars,boundcvars,boundfids},vck_list) = 
	    let fun folder ((v,copt,k:kind),(ctxt,boundcvars)) =
		   let val ctxt = 
				((NilContext.find_kind(ctxt,v); ctxt)
				 handle NilContext.Unbound =>
				     (case copt of
					  NONE => NilContext.insert_kind(ctxt,v,k)
					| SOME c => NilContext.insert_kind_equation(ctxt,v,c,k)))
		       val entry = 
			   (if is_top 
				then GLOBALc
			    else LOCALc(memoize (fn() => Stats.subtimer("toclosure_make_shape",
						 Normalize.make_shape ctxt) k),
						 ref NONE))
		       val boundcvars = VarMap.insert(boundcvars,v,entry)
		   in  (ctxt,boundcvars)
		   end
		val (ctxt',boundcvars') =  foldl folder (ctxt,boundcvars) vck_list

	    in  STATE{ctxt = ctxt',
		      is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars,
		      boundcvars = boundcvars',
		      boundfids = boundfids}
	    end


	fun add_boundfids shadow (STATE{ctxt,is_top,curfid,boundevars,boundcvars,boundfids},fid_types) = 
	    let val state = STATE{ctxt=ctxt,
				  curfid = curfid,
				  is_top = is_top,
				  boundevars = boundevars,
				  boundcvars = boundcvars,
				  boundfids = VarSet.addList(boundfids,map #1 fid_types)}
		fun mapper(fid,t) = 
		    let val {fidtype_var,...} = get_static fid
		    in  ((fidtype_var,SOME t, Type_k), (fid, Var_c fidtype_var))
		    end
		val (vck,fid_types) = Listops.unzip(map mapper fid_types)
		val state = add_boundcvars (state,vck)
	    in  add_boundevars' shadow (state,fid_types)
	    end

	fun add_boundevar(s,v,c) = add_boundevars(s,[(v,c)])
	fun add_boundcvar(s,v,k) = add_boundcvars (s,[(v,NONE, k)])
	fun add_boundcvar'(s,v,c,k) = add_boundcvars (s,[(v,SOME c, k)])
	    
	fun is_boundevar(STATE{boundevars,...},evar) = 
	    (case (VarMap.find(boundevars,evar)) of
		 SOME GLOBALe  => true
	       | SOME(LOCALe _) => true
	       | _ => false)

	fun is_boundcvar(STATE{boundcvars,...},cvar) = 
	    (case (VarMap.find(boundcvars,cvar)) of
		 SOME GLOBALc => true
	       | SOME (LOCALc _) => true
	       | _ => false)

	fun is_boundfid(STATE{boundfids,...}, f) = VarSet.member(boundfids,f)


	fun cvar_isfree (STATE{boundcvars,boundevars,...},{freecpaths_map,...}:frees,cvar) = 
	    if (case (PathMap.find(freecpaths_map,(cvar,[]))) of
		      NONE => false
		    | _ => true)
		then NONE
	    else (case (VarMap.find(boundcvars,cvar)) of
			  SOME GLOBALc => NONE
			| SOME (LOCALc _) => NONE
			| SOME (SHADOWc (k,f)) => SOME(k(),f)
			| NONE => error ("cvar_isfree: variable " ^
					 (var2string cvar) ^ " not bound"))

	(* labels are backwards *)
	fun cpath_isfree (STATE{ctxt,boundcvars,...},{freecpaths_map,...}:frees,
			  cpath as (cvar,labs)) = 
	    case (PathMap.find(freecpaths_map,cpath)) of
		SOME _ => NONE
	      | _ => (case (VarMap.find(boundcvars,cvar)) of
			SOME GLOBALc  => NONE
		      | SOME (LOCALc _) => NONE
		      | SOME (SHADOWc (k,f)) => 
			      let fun search l [] = error "bad kind projection"
				    | search l (((l1,v1),k1)::rest) =
				  if (eq_label(l,l1)) then k1 else search l rest
				  fun proj (k,[]) = k
				    | proj (k,l::rest) = 
				      let
					  val k = 
					      (case k of
						   Record_k lvk_seq => search l (Sequence.toList lvk_seq)
						 | _ => error "projecting from non-record kind")
				      in  proj(k,rest)
				      end
			      in  SOME (proj(k(),rev labs))
			      end
		      | NONE => error ("cpath_isfree: variable " ^
				(var2string cvar) ^ "not bound"))

(* labs are backwards *)
	fun epath_isfree (STATE{ctxt,boundevars,...},{freeepaths_map,...}:frees,epath as (evar,labs)) = 
	    case (PathMap.find(freeepaths_map,epath)) of
		SOME _ => NONE
	      | _ => (case (VarMap.find(boundevars,evar)) of
			  SOME GLOBALe => NONE
			| SOME (LOCALe _) => NONE
			| SOME (SHADOWe (_,c,f)) => 
			      let fun search l ([],[]) = error "bad projection"
				    | search l ((l1::lrest),(c1::crest)) = 
				  if (eq_label(l,l1)) then c1 else search l (lrest,crest)
				    | search l _ = error "ill-formed record type"
				  fun proj (c,[]) = (#1(reduce_hnf "toclosure_hnf2" (ctxt,c)),c)
				    | proj (c,l::rest) = 
				      let val c = 
					  (case (#2(reduce_hnf "toclosure_hnf3" (ctxt,c))) of
					       Prim_c(Record_c (labs,_),cons) => search l (labs,cons)
					     | _ => error "projecting from non-record type")
				      in  proj(c,rest)
				      end
				  val (hnf,c) = proj(c,rev labs)
			      in  SOME (hnf,c,f)
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
		     SOME GLOBALe => NONE
		   | SOME (LOCALe _) => NONE
		   | SOME (SHADOWe (h,c,f)) => SOME(h(),c,f)
		   | NONE => error ("evar_isfree: variable " ^
				    (var2string evar) ^ " not bound"))
			    


	fun free_cpath_add (f as {freecpaths,freecpaths_map,freeepaths_map}:frees,
				(cvar,labs),k) = 
	    let 
		val _ = if (!debug)
			    then (print "free_cvar_add-ing "; show_path (cvar,labs);
				 print " to\n";
				  show_free f; print "\n\n")
			else ()
		val str = foldl (fn (l,acc) => acc ^ "_" ^ (Name.label2string l))
				(Name.var2name cvar) labs
		val v = Name.fresh_named_var str
		val l = Name.internal_label(Name.var2string v)
		fun bubble [] = [(cvar,labs)]
		  | bubble (first::rest) = 
		    let val v = Name.fresh_named_var "dummy"
			val SOME (_,_,k') = PathMap.find(freecpaths_map,first)
			val c = AllArrow_c(Open,Total,[(v,k')],NONE,[],0w0,Var_c v)
		    in  if (NilUtil.convar_occurs_free(cvar,c))
			    then (cvar,labs)::first::rest
			else first::(bubble rest)
		    end
		val freecpaths = rev(bubble (rev freecpaths))
		val freecpaths_map = 
		    case PathMap.find(freecpaths_map,(cvar,labs)) of
			SOME _ => error "free_cvar_add given variable already in free set"
		      | _ => 
			    let fun cthunk cenv_var = Proj_c(Var_c cenv_var,l)
			    in  PathMap.insert(freecpaths_map,(cvar,labs),
					      (v,cthunk,k))
			    end
		val res = {freeepaths_map = freeepaths_map,
			   freecpaths = freecpaths, 
			   freecpaths_map = freecpaths_map}
		val _ = if (!debug)
			    then (print "free_cvar_add done "; 
				  show_path(cvar,labs); print " to\n";
				  show_free f; print "\n\n\n")
			else ()
	    in  res
	    end

	fun free_epath_add ({freecpaths,freecpaths_map,
			     freeepaths_map}:frees,epath,hnf,c) = 
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
			     in  PathMap.insert(freeepaths_map,epath,(v,ethunk,hnf,c))
			     end)
	    in  {freeepaths_map = freeepaths_map,
		 freecpaths = freecpaths, freecpaths_map = freecpaths_map}
	    end


	fun free_evar_add (frees,evar,hnf,c) = 
	    free_epath_add(frees,(evar,[]),hnf,c)
	fun free_cvar_add (frees,cvar,k) = 
	    free_cpath_add(frees,(cvar,[]),k)


	fun empty_fun escape new_fid : funentry = 
	    {static = {fid = new_fid,
		       fidtype_var = fresh_named_var((var2string new_fid)
						  ^ "_type"),
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


	fun copy_state (STATE{ctxt,boundevars,boundcvars,boundfids,curfid,...}) (fid : var * var list) = 
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

    in  val get_static = get_static
	val is_fid = is_fid
	type state = state
	fun get_ctxt (STATE{ctxt,...}) = ctxt
	type funentry = funentry
	val copy_state = copy_state
	val show_state = show_state
	val free_cvar_add = free_cvar_add
	val free_cpath_add = free_cpath_add
	val free_evar_add = free_evar_add
	val free_epath_add = free_epath_add
	val cvar_isfree = cvar_isfree
	val evar_isfree = evar_isfree
	val cpath_isfree = cpath_isfree
	val epath_isfree = epath_isfree
	val is_boundfid = is_boundfid
	val add_gboundevar = add_gboundevar
	val add_gboundcvar = add_gboundcvar
	val add_boundevar = add_boundevar
	val add_boundcvar = add_boundcvar
	val add_boundcvar' = add_boundcvar'
	val add_boundevars = add_boundevars
	val add_boundcvars = add_boundcvars
	val add_boundfids = add_boundfids
	val get_curfid = get_curfid
	val remove_free = remove_free
	val is_boundevar = is_boundevar
	val is_boundcvar = is_boundcvar

	fun type_of(STATE{ctxt,...},e) = Stats.subtimer("toclosure_typeof",Normalize.type_of)
	                                 (ctxt,e) 

    (* ----- Global data structure to keep track of function information ------------ *)

	fun reset_table escapes = (fids := empty_table;
				   global_escapes := VarSet.addList(VarSet.empty,escapes))
	fun get_fids() = VarMap.foldli (fn (fid,_,acc) => VarSet.add(acc,fid)) VarSet.empty (!fids)
	fun add_fun new_fid = 
	    (let val escape = VarSet.member(!global_escapes,new_fid)
	     in  fids := (VarMap.insert(!fids,new_fid,ref (empty_fun escape new_fid)))
	     end)
	    
	fun get_callee caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_callee")
				   | SOME (r as (ref{callee,...})) => callee)
	fun get_frees  caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_frees")
				   | SOME (r as (ref{frees,...})) => frees)

	fun get_escape caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_escape")
				   | SOME (ref{escape,...}) => escape)


	fun initial_state topfid = let val _ = add_fun topfid
				   in   STATE{ctxt = NilContext.empty(),
					      is_top = true,
					      curfid=(topfid,[topfid]),
					      boundfids=VarSet.empty,
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
    fun bnd_find_fv arg bnd =
	let val res = bnd_find_fv' arg bnd
	    handle e => (print "bnd_find_fv called on\n";
			 Ppnil.pp_bnd bnd;
			 print "\n\n"; raise e)
	in  res 
	end

    and bnd_find_fv' (state : state, frees : frees) bnd : state * frees =
	    let
		fun do_fix frees var_arm_set do_arm =
		    let val var_arm_list = Sequence.toList var_arm_set
			val fids = map #1 var_arm_list
			val _ = app add_fun fids
			val local_fids_types = map (fn (v,pf) => 
						    (v,NilUtil.function_type Open pf)) 
			                       var_arm_list
			val free = (foldl (fn (a,f) => let val f' = do_arm local_fids_types a
					               in  join_free(f,f')
						       end)
				    frees (Sequence.toList var_arm_set))
		    in  (add_boundfids false (state, local_fids_types), free)
		    end
		val (state,frees) = 
		    (case bnd of
			 Con_b(_,cbnd) => 
			     let val (f,state) = cbnd_find_fv(cbnd,(frees,state))
			     in (state, f)
			     end
		       | Exp_b(v,e) => 
			     let 
				 val c = type_of(state,e) 
				 val f = e_find_fv (state,frees) e
				 val _ = if (!debug)
					     then (print "add_boundevar ";
						   Ppnil.pp_var v; print "\n")
					 else ()
			     in (add_boundevar(state,v,c), f)
			     end
		       | Fixopen_b var_fun_set =>
			     let val (outer_curfid,_) = get_curfid state
				 fun do_arm fids_types (v,Function(_,_,vklist,_,vclist,vflist,body,tipe)) =
				 let val outer_state = add_boundfids false (state,fids_types) 
				     val fids = map #1 fids_types
				     val local_state = copy_state state (v,fids)
				     val local_state = add_boundfids true (local_state,fids_types) 
				     val fs = (empty_frees, local_state)
				     val _ = if (!debug)
						 then (print "state before vkfolder to ";
						       Ppnil.pp_var v; print "\n";
						       show_state (#2 fs); print "\n")
					     else ()
				     val fs = vklist_find_fv (vklist,fs)
				     val _ = if (!debug)
						 then (print "the following (after vkfolder) frees to ";
						       Ppnil.pp_var v; print "\n";
						       show_free (#1 fs); print "\n")
					     else ()
				     val (f,s) = vtlist_find_fv
						  (vclist @ (map (fn v => (v,float64)) vflist),
						   fs)
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

				     val {freeepaths_map,freecpaths,freecpaths_map} = f
				     fun folder(p,(v,t,hnf,c),f) = 
					 (if (!debug)
					      then 
						  (print "for function "; pp_var (#1(get_curfid (#2 fs)));
						   print " considering "; show_path p; print "\n") else ();
					  if (not hnf orelse Listops.member_eq(eq_var,#1 p,
							  #2(get_curfid (#2 fs)))  )
					     then (if (!debug) then 
						 (if hnf 
						      then print "NOT USING TYPEOF(because member); USING "
						  else print "NOT USING TYPEOF(because not hnf); USING ";
						  Ppnil.pp_con c; print "\n") else ();
						  t_find_fv(#2 fs,free_epath_add(f,p,false,c)) c)
					 else (if (!debug) then print "USING TYPEOF\n" 
						   else ();
					       free_epath_add(f,p,true,Typeof_c(path2exp p))))
				     val lf = PathMap.foldli folder ({freecpaths=freecpaths,freecpaths_map=freecpaths_map,
							    freeepaths_map=PathMap.empty}) freeepaths_map

				     val f = {freeepaths_map = freeepaths_map,
					      freecpaths = #freecpaths lf,
					      freecpaths_map = #freecpaths_map lf}
				     val _ = add_frees(v,lf)
(*				     val _ = add_callee(outer_curfid,v,outer_state) *)
				 in  f
				 end
				 val (s,f) = do_fix frees var_fun_set do_arm
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



    and nilprim_uses_carg_conservative (np,clist) = 
	(case np of
	     record _ => false
	   | select _ => false
	   | roll => false
	   | unroll => false
	   | project_sum_record _ => false
	   | project_sum known => true
	   | _ => true)



    and nilprim_uses_carg state (np,clist) = 
	(case np of
	     record _ => false
	   | select _ => false
	   | roll => false
	   | unroll => false
	   | project_sum_record _ => false
	   | project_sum known =>
		 let 
		     val (tagcount,totalcount,carrier) = 
			 (case #2(reduce_hnf "toclosure_hnf4" (get_ctxt state, hd clist)) of
			      Prim_c(Sum_c{known,totalcount,tagcount},cons) =>
				  (tagcount,totalcount,hd cons)
			      | _ => error "project_sum decoration not reducible to sum type")
		     val cons = 
			 if (TilWord32.equal(totalcount,TilWord32.uplus(tagcount,0w1))) then [carrier]
			 else (case #2(reduce_hnf "toclosure_hnf5" (get_ctxt state, carrier)) of
				   Crecord_c lcons => map #2 lcons
				 | _ => error "project_sum decoration reduced to a bad sum type")
			     
		     val which_con = TilWord32.toInt (TilWord32.uminus(known,tagcount))
		     val field_con = List.nth(cons, which_con)
		     val irreducible = not(istype_reducible(state,field_con))
		     val _ = if irreducible
				 then (Stats.counter
				       
				       ("toclosure.irreducible-project_sum")(); 
				       if (!debug) 
					   then print "toclosure: irreducible projsum\n" 
				       else ())
			     else (if (!debug)
				       then (print "toclosure: projsum: field_con was reducible:\n"; Ppnil.pp_con field_con; print "\n") else ())
		 in  irreducible
		 end
	   | _ => true)


    and e_find_fv arg exp : frees =
	let val res = e_find_fv' arg exp
	    handle e => (print "exp_find_fv called on\n";
			 Ppnil.pp_exp exp;
			 print "\n\n"; raise e)
	in  res 
	end

    and switch_find_fv (state : state, frees : frees) switch : frees =
	(case switch of
	     Intsw_e{arg,arms,default,...} =>
		 let val frees = e_find_fv (state,frees) arg
		     val frees = foldl (fn ((_,e),f) => e_find_fv (state,f) e) frees arms 
		     val frees = (case default of
				      NONE => frees
				    | SOME e => e_find_fv (state,frees) e)
		 in  frees
		 end
	   | Sumsw_e{sumtype,arg,bound,arms,default,...} => 
		 let val frees = e_find_fv (state,frees) arg
		     val frees = t_find_fv (state,frees) sumtype
		     val frees = (case default of
				      NONE => frees
				    | SOME e => e_find_fv (state,frees) e)
		     val state = add_boundevar(state,bound,sumtype)
		     val frees = foldl (fn ((_,e),f) => e_find_fv (state,f) e) frees arms 
		 in  frees
		 end
	   | Exncase_e{arg,bound,arms,default,...} => 
		 let val frees = e_find_fv (state,frees) arg
		     val frees = (case default of
				      NONE => frees
				    | SOME e => e_find_fv (state,frees) e)
		     val frees = foldl (fn ((e1,e2),f) => 
					let val f = e_find_fv (state,f) e1
					    val tagcon = type_of(state,e1)
					    val Prim_c(Exntag_c, [con]) = tagcon
					    val state = add_boundevar(state,bound,con)
					    val f = e_find_fv (state,f) e2
					in  f
					end) frees arms 
		 in  frees
		 end
	   | Typecase_e _ => error "typecase not handled")

    and e_find_fv' (state : state, frees : frees) exp : frees =
	(if (!debug)
	     then (print "exp_find_fv called on\n";
		   if (!debug_full)
		       then Ppnil.pp_exp exp
		   else (); print "\n\n")
	 else ();
	 case exp of
	     Var_e v => 
		 (if (is_boundfid(state,v)) then add_escape v else ();
		  (case (evar_isfree(state,frees,v)) of
			   NONE => frees
			 | SOME (hnf,c,r) => free_evar_add(frees,v,hnf,c)))
           | Prim_e(NilPrimOp(select l), _, _) =>
	       let (* labels are returned backwards *)
		   val (base,labels) = extract_path exp
		   val do_path = (!do_close_path) andalso (
			case base of Var_e _ => true | _ => false)
	       in  if not do_path
		       then e_find_fv (state,frees) base
		   else 
		       let val (Var_e v) = base
			   val _ = if (is_boundfid(state,v)) then add_escape v else ()
			   (* labels are backwards *)	
		       in  (case (epath_isfree(state,frees,(v,labels))) of 
				NONE => frees
			      | SOME (hnf,c,r) => free_epath_add(frees,(v,labels),hnf,c))
		       end
	       end

	   | Prim_e (NilPrimOp np,clist,elist) =>
		 let fun tfold(t,f) = t_find_fv (state,f) t
		     fun cfold(c,f) = c_find_fv (state,f) c
		     fun efold(e,f) = e_find_fv (state,f) e
		     val frees = if (nilprim_uses_carg state (np,clist))
				     then foldl cfold frees clist
				 else foldl tfold frees clist
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
		      (Prim.int _)  => frees
		    | (Prim.uint _) => frees
		    | (Prim.float _) => frees
		    | Prim.array (c,array) =>
			  let fun folder(e,f) = e_find_fv (state,f) e
			      val f = Array.foldl folder frees array
			  in  c_find_fv (state,f) c
			  end
		    | Prim.vector (c,array) => 
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
	   | Switch_e switch => switch_find_fv (state,frees) switch

	   | App_e (_, e, clist, elist, eflist) =>
		 let val f = (case e of
				      Var_e v => if (is_fid v andalso 
						     eq_var(v,#1(get_curfid state)))
						     then (
(*							   print "***** adding callee ";
							   Ppnil.pp_var v; print " to ";
							   Ppnil.pp_var (get_curfid state); print "\n";
*)
							   add_callee(#1(get_curfid state),v,
								      state);
							   frees)
						 else (e_find_fv (state,frees) e)
				    | _ => (e_find_fv (state,frees) e))
		     val f = foldl (fn (c,f) => c_find_fv (state,f) c) f clist
		     val f = foldl (fn (e,f) => e_find_fv (state,f) e) f elist
		     val f = foldl (fn (e,f) => e_find_fv (state,f) e) f eflist
		 in  f
		 end
	   | ExternApp_e (e, elist) =>
		 let val f = e_find_fv (state,frees) e
		     val f = foldl (fn (e,f) => e_find_fv (state,f) e) f elist
		 in  f
		 end
	   | Raise_e (e,c) => e_find_fv (state, c_find_fv (state,frees) c) e
	   | Handle_e (e,v,body) =>
		 let val f = e_find_fv (state,frees) e
		     val f = e_find_fv (add_boundevar(state,v,Prim_c(Exn_c,[])),f) body
		 in  f
		 end)



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


    and istype_reducible (state,con) = #1(reduce_hnf "toclosure_hnf6" (get_ctxt state,con))

	and t_find_fv' (state : state, frees : frees) con : frees =
	    (case con of
		Var_c _ => c_find_fv'(state,frees) con
	      | _ => frees)
(*
	    let fun type_case() = (c_find_fv'(state,frees) con; frees)
		fun constructor_case() = c_find_fv'(state,frees) con
		val istype = istype_reducible(state,con)
	    in  if istype
		    then type_case()
		else constructor_case()
	    end
*)

    and vklist_find_fv (vklist,fs) =
	let fun folder((v,k),(f,s)) = 
	    (k_find_fv (s,f) k, add_boundcvar(s,v,k))
	in  foldl folder fs vklist
	end

    and vtlist_find_fv (vclist,fs) =
	let fun folder((v,c),(f,s)) = 
	    (t_find_fv (s,f) c, add_boundevar(s,v,c))
	in  foldl folder fs vclist
	end

    and cbnd_find_fv (cbnd,(f,s)) = 
	(case cbnd of
	     Con_cb(v,c) => let val f' = c_find_fv (s,f) c
				val k = Singleton_k c
				val state = add_boundcvar'(s,v,c,k)
			    in (f',state)
			    end
	   | (Code_cb(v,vklist,c,k)) => (f,s)
	   | (Open_cb(v,vklist,c,k)) =>
			    let val _ = add_fun v
				val ls = copy_state s (v,[v])
				val (f,ls) = vklist_find_fv (vklist,(f,ls))
				val f' = k_find_fv (ls,f) k
				val f'' = c_find_fv (ls,f') c
				val _ = add_frees(v,f'')
				val ak = Arrow_k(Open,vklist,k)
			    in  (remove_free(s,f''), add_boundcvar(s,v,ak))
			    end)
			
    and c_find_fv' (state : state, frees : frees) con : frees =
	     (case con of
		 Prim_c(Record_c(labs,SOME vars),clist) =>  
		     let val vclist = Listops.zip vars clist
			 val (f,s) = vtlist_find_fv (vclist,(frees,state))
		     in  f
		     end
	       | Prim_c (pc,clist) => foldl (fn (c,f)=> (c_find_fv (state,f) c)) frees clist
	       | Mu_c (_,vcset) =>
		     let val vclist = Sequence.toList vcset
			 (* we need to alpha-vary since reductions may lead to duplication
			    of bound variables despite A-normal linearization *)
			 val (vclist) = alpha_mu (fn v => is_boundcvar(state,v)) (vclist)
			 val state' = add_boundcvars(state,map (fn (v,_) => (v,NONE,Type_k)) vclist)
		     in  foldl (fn ((v,c),f) => c_find_fv (state',f) c) frees (Sequence.toList vcset)
		     end
	       (* the types of some primitives like integer equality are Code arrow *)
	       | AllArrow_c(_,_,vklist,vlist,clist,numfloats,c) =>
		     let val fs = vklist_find_fv (vklist,(frees,state))
			 val vlist = case vlist of SOME vars => vars 
		                                 | NONE => map (fn _ => Name.fresh_var()) clist
			 val (f,s) = vtlist_find_fv (Listops.zip vlist clist,fs)
		     in  c_find_fv (s,f) c
		     end
	       | ExternArrow_c(clist,c) =>
		     let fun cfolder (c,(f,s)) = (c_find_fv (s,f) c,s)
			 val (f,s) = foldl cfolder (frees,state) clist
		     in  c_find_fv (s,f) c
		     end
	       | Var_c v => 
		     (case (cvar_isfree(state,frees,v)) of
			  NONE => frees
			| SOME (k,r) => 
			      let 
				  val k = Singleton_k(Var_c v)
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
			 val (f,s) = foldl cbnd_find_fv (frees,state) cbnds
			 val f' = c_find_fv (s,f) c
(*			 val f' = remove_free(s,f') *)
		     in  f'
		     end
	       | Crecord_c lclist => foldl (fn ((_,c),f) => c_find_fv (state,f) c) frees lclist
	       | Typeof_c e => frees
	       | Proj_c (c,l) => 
		    let (* labels are returned backwards *)
			val (base,labels) = extract_cpath con
			val do_path = (!do_close_cpath) andalso
				(case base of Var_c _ => true | _ => false)
		    in  if not do_path	
			then c_find_fv (state,frees) c
			else 
			  let val (Var_c v) = base
			  in  (case cpath_isfree(state,frees,(v,labels)) of
			 	NONE => frees
			      | SOME k => 
				let val k = Singleton_k(Proj_c(c,l))
				in  free_cpath_add(frees,(v,labels),k)
				end)
			  end
		    end
	       | Closure_c (c1,c2) => c_find_fv (state, c_find_fv (state,frees) c1) c2
	       | App_c(c,clist) => let val f = c_find_fv (state,frees) c
				   in foldl (fn (c,f) => c_find_fv (state,f) c) f clist
				   end
	       | Annotate_c (_,c) => c_find_fv (state,frees) c)
		 
	and k_find_fv' (state,frees) kind : frees =
	    (case kind of
	    Type_k => frees
	  | Singleton_k c => (c_find_fv (state,frees) c)
	  | (Record_k lv_kind_seq) =>
		 let fun folder (((l,v),k),(f,s)) = (k_find_fv (s,f) k, add_boundcvar(s,v,k))
		 in  #1(Sequence.foldl folder (frees,state) lv_kind_seq)
		 end
	  | (Arrow_k (_,vklist,k)) =>
		let val (f,s) = vklist_find_fv (vklist,(frees,state))
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
				       [],[Var_e evar])


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

     local fun path_eq((v1,l1),(v2,l2)) = 
		(eq_var(v1,v2) andalso (Listops.eq_list(eq_label,l1,l2)))
	   fun chandle subst (_,c) = 
		let val (base,labels) = extract_cpath c
		in  case base of
		      Var_c v => 
			(case (Listops.assoc_eq(path_eq,(v,labels),subst)) of
				NONE => NOCHANGE
			      | SOME c => CHANGE_NORECURSE c)
		    | _ => NOCHANGE
		end
	fun handlers subst = (fn _ => NOCHANGE, fn _ => NOCHANGE, chandle subst,
			fn _ => NOCHANGE, fn _ => NOCHANGE)
     in  fun substConPathInCon (subst,c) = NilUtil.con_rewrite (handlers subst) c
         fun substConPathInKind (subst,k) = NilUtil.kind_rewrite (handlers subst) k
     end 
   fun fun_rewrite state vars (v,Function(effect,recur,vklist,dep,vclist,vflist,body,tipe)) = 
       let 
	   val _ = if (!debug)
		       then (print "\n***fun_rewrite v = "; Ppnil.pp_var v; print "\n")
		   else ()
	   val {code_var, fidtype_var, unpack_var, cenv_var, venv_var, ...} = get_static v
	   val {freecpaths,freecpaths_map,freeepaths_map=pc_free,...} = get_frees v
	   (* was kept in reverse order *)
	   val vkl_free = map (fn p => let (* the k is shape only *)
					   val SOME(v,_,k) = PathMap.find(freecpaths_map,p)
					   val k = Singleton_k(path2con p)
					   val l = Name.internal_label(Name.var2string v)
				      in  (p,v,k,l)
				      end)
	                     (rev freecpaths)
	   val vklist = map (fn (v,k) => (v,k_rewrite state k)) vklist
	   val vclist = map (fn (v,c) => (v,c_rewrite state c)) vclist
	   val escape = get_escape v
	   val vkl_free = map (fn (p,v,k,l) => (p,v,k_rewrite state k,l)) vkl_free
	   val pc_free = let val temp = PathMap.listItemsi pc_free
			     fun mapper((v,labs),(v',ethunk,_,c)) = 
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
			     app (fn ((v,labs),_,_,_) => (Ppnil.pp_var v; app Ppnil.pp_label labs; print ", ")) vkl_free;
			     print "\nvc_free are "; 
			     app (fn ((v,labs),_,_,_) => (Ppnil.pp_var v; app Ppnil.pp_label labs;
							print ", ")) pc_free;
			     print "\n")
		   else ()

	   val num_vkl_free = length vkl_free
	   val num_pc_free = length pc_free
	   val cenv_kind = let fun mapper(p,v,k,l) = ((l,Name.derived_var v),k)
			       val lvk_list = map mapper vkl_free
			   in  Record_k(Sequence.fromList lvk_list)
			   end
	   val venv_type = let val types = map #4 pc_free
			       val labs = map #3 pc_free
			   in  if (!do_single_venv andalso length labs = 1)
				   then hd types
			       else Prim_c(Record_c (labs,NONE), types)
			   end

	   val vklist_code = vklist @ (map (fn (p,v,k,l) => (v,k)) vkl_free)
	   val vclist_code = vclist @ (map (fn (path,v',_,c) => (v',c)) pc_free)
	   val (internal_subst,external_subst,cenv_kind) = 
	       let 
		   fun loop is es acc [] = (is,es,Record_k (Sequence.fromList (rev acc)))
		     | loop is es acc (((p,v',_,_),((l,v),k))::rest) = 
		       let val k' = substConPathInKind(is,k)
			   val acc' = ((l,v),k')::acc
			   val is' = (p,Var_c v)::is
			   val es' = (p,Proj_c(Var_c cenv_var,l))::es
		       in  loop is' es' acc' rest
		       end
	       in  (case cenv_kind of
			Record_k lvk_set => loop [] [] [] (Listops.zip vkl_free (Sequence.toList lvk_set))
		      | _ => error "cenv_kind not a record_k")
	       end

	   val vklist_unpack_almost = map (fn (v,k) => (v,substConPathInKind(external_subst, k))) vklist
	   val vklist_unpack = vklist_unpack_almost @ [(cenv_var,cenv_kind)]
	   val vclist_unpack_almost = vclist @ [(venv_var,venv_type)]
	   val vclist_unpack = map (fn (v,c) => (v,substConPathInCon (external_subst, c))) vclist_unpack_almost


	   val codebody_tipe = c_rewrite state tipe

	   val closure_tipe = AllArrow_c(Closure,effect,
					 vklist,
					 if dep then SOME(map #1 vclist) else NONE,
					 map #2 vclist,
					 TilWord32.fromInt(length vflist),codebody_tipe)
	   val codebody_tipe = substConPathInCon(external_subst,codebody_tipe)


	   val code_body = (e_rewrite (copy_state(state,v)) body)

	   fun pc_mapper((v,l,_),c) = 
	       let 
		   val e = path2exp (v,l)
		   val e = e_rewrite state e
	       in  (e,c)
	       end


	   val cenv = let val lc_list = map (fn (p,v,_,l) => 
					     let val c = path2con p
					     in  (l,c_rewrite state c)
					     end) vkl_free
		      in  Crecord_c lc_list
		      end


	   val venv = let val labels = map #3 pc_free
			  val clist = map #4 pc_free
			  val elist = map (fn (p,_,_,_) => e_rewrite state (path2exp p)) pc_free
		      in  if (!do_single_venv andalso length labels = 1)
			      then hd elist
			  else Prim_e(NilPrimOp(record labels),[],elist)
		      end

	   val code_fun = Function(effect,recur,
				   vklist_unpack,dep,vclist_unpack,vflist,
				   code_body, codebody_tipe)

	   val closure = {code = code_var,
			  cenv = cenv,
			  venv = venv,
			  tipe = Var_c fidtype_var}

       in if escape then (is_recur,
			  [(fidtype_var, closure_tipe)],
			  [(code_var,code_fun)],
			  [(v,closure)])
	  else (false,[],[(code_var,code_fun)],[])
       end



   and bnd_rewrite state bnd : bnd list =
       let
	   fun funthing_helper rewriter var_thing_set =
	       let 
		   val var_thing_list = Sequence.toList var_thing_set
		   val vars = map #1 var_thing_list
		   val type_pfun_close = map (rewriter vars) var_thing_list
		   val is_recur = Listops.orfold (fn (x,_,_,_) => x) type_pfun_close
		   val pfun_type = List.concat(map #2 type_pfun_close)
		   val pfun_typebnds = map (fn (v,c) => (Con_b(Compiletime,Con_cb(v,c)))) pfun_type
		   val pfun_list = List.concat(map #3 type_pfun_close)
		   val pfun_bnd = Fixcode_b (Sequence.fromList pfun_list)
		   val closure_bnd_list = 
		       (case (List.concat(map #4 type_pfun_close)) of
			    [] => []
			  | close_list => [Fixclosure_b(is_recur,Sequence.fromList close_list)])
	       in  pfun_typebnds @ (pfun_bnd :: closure_bnd_list)
	       end
       in (case bnd of
		(Con_b(p,cb)) => let val cbnds = cbnd_rewrite state cb
				 in  map (fn cb => Con_b(p,cb)) cbnds
				 end
	      | (Exp_b(v,e)) => [Exp_b(v, e_rewrite state e)]
	      | (Fixclosure_b _) => error "there can't be closures while closure-converting"
	      | (Fixcode_b _) => error "there can't be codes while closure-converting"
	      | (Fixopen_b var_fun_set) => funthing_helper (fun_rewrite state) var_fun_set)
       end



   and e_rewrite state arg_exp : exp =
       let 
(*
	   val _ = (print "  e_rewrite called on = \n";
		    pp_exp arg_exp; print "\n")
*)
	   (* we do not copy_state for arms since they are not first-class *)
	   fun armfun_rewrite (Function(ef,re,vklist,dep,vclist,vflist,e,c)) =
	   Function(ef,re,map (fn (v,k) => (v, k_rewrite state k)) vklist,
		    dep,map (fn (v,c) => (v, c_rewrite state c)) vclist,
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
		      | SOME (_,ethunk,_,_) => ethunk(PathMap.numItems freeepaths_map,venv_var))
	       end

       in
       (case arg_exp of
	    Var_e v => path_case(v,[])
	  | Prim_e(NilPrimOp(select l), _, _) =>
	       let (* labels are returned backwards *)
		   val (base,labels) = extract_path arg_exp
		   val do_path = (!do_close_path) andalso 
			(case base of Var_e _ => true | _ => false)
	       in  if not do_path
		       then project(e_rewrite base, labels)
		   else
		       let val (Var_e v) = base
		       in  path_case(v,labels)
		       end
	       end

	  | Prim_e(NilPrimOp np,clist,elist) => 
	       Prim_e(NilPrimOp np,map c_rewrite clist, map e_rewrite elist)
(*
	       if (nilprim_uses_carg_conservative (np,clist))
		   then Prim_e(NilPrimOp np,map c_rewrite clist, map e_rewrite elist)
	       else Prim_e(NilPrimOp np,clist, map e_rewrite elist)
*)

	  | Prim_e(p,clist,elist) => 
	       Prim_e(p,map c_rewrite clist, map e_rewrite elist)

	  | Const_e v => arg_exp
	  | Switch_e switch => 
		Switch_e(case switch of 
		     Intsw_e{size,arg,arms,default} => 
			 Intsw_e{size = size, 
				 arg = e_rewrite arg,
				 arms = map_second e_rewrite arms,
				 default = Util.mapopt e_rewrite default}
		   | Sumsw_e{sumtype,arg,bound,arms,default} => 
			 Sumsw_e{sumtype=c_rewrite sumtype, 
				 arg=e_rewrite arg,
				 bound = bound,
				 arms = map_second  e_rewrite arms,
				 default=Util.mapopt e_rewrite default}
		   | Exncase_e{arg,arms,bound,default} => 
			 Exncase_e{arg=e_rewrite arg,
				   bound=bound,
				 arms=map (fn (e,f) => (e_rewrite e,e_rewrite f)) arms,
				 default=Util.mapopt e_rewrite default}
		   | Typecase_e _ => error "typecase not handled")
			 
	  | Let_e(letsort,bnds,e) => let val bnds_list = map (bnd_rewrite state) bnds
				     in Let_e(letsort, List.concat bnds_list, e_rewrite e)
				     end
	  | App_e (Closure, _,_,_,_) => error "App_e(Code|Closure,...) during closure conversion"
	  | ExternApp_e (e, elist) => 
		let val elist' = map e_rewrite  elist
		    val e' = e_rewrite e
		in  ExternApp_e(e', elist')
		end
	  | App_e (Code, e, clist, elist, eflist) => 
		let val clist' = map c_rewrite  clist
		    val elist' = map e_rewrite  elist
		    val eflist' = map e_rewrite eflist
		    val e' = e_rewrite e
		in  App_e(Code, e', clist', elist', eflist')
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
	  | Handle_e (e,v,f) => Handle_e(e_rewrite e, v, e_rewrite f))
       end

   and cbnd_rewrite state (Con_cb(v,c)) : conbnd list = 
	    [Con_cb(v,c_rewrite state c)]
     | cbnd_rewrite state (Open_cb(v,vklist,c,k)) = 
         let val _ = if (!debug)
			 then (print "  cbnd_rewrite v = "; Ppnil.pp_var v; print "\n")
		     else ()
	     val (code_var,cenv_var,vkl_free) = 
		 if (is_fid v)
		     then let val {code_var, cenv_var, ...} = get_static v
			      (* freeevars/freeepaths must be empty *)
			      val {freecpaths,freecpaths_map,...} = get_frees v 
			      val vkl_free = 
				  map (fn p => let val SOME(v,_,k) = PathMap.find(freecpaths_map,p)
						   val l = Name.internal_label(Name.var2string v)
					       in  (p,v,k,l)
					       end)
				  freecpaths
			  in  (code_var, cenv_var, vkl_free)
			  end
		 else  (Name.fresh_named_var "unclosed_open_cb",
			Name.fresh_named_var "unclosed_open_cb_cenv",
			[])
	     val vkl_free = rev vkl_free


	     val k = k_rewrite state k
	     val subst = map (fn (p,_,_,l) => (p,Proj_c(Var_c cenv_var,l))) vkl_free

	     val k = substConPathInKind (subst, k)


	     fun get_cbnd (p,v,k,l) = Con_cb(v, Proj_c(Var_c cenv_var,l))
	     val vklist = map (fn (v,k) => (v,k_rewrite state k)) vklist
	     val vkl_free = map (fn (p,v,k,l) => (p,v,k_rewrite state k,l)) vkl_free
	     val vkl_free_kind = Record_k(map
					  (fn (p,v,k,l) => ((l, v), k))
					  vkl_free)
	     val cbnds = map get_cbnd vkl_free
	     val vklist' = vklist @ [(cenv_var, vkl_free_kind)]
	     val code_cb = Code_cb(code_var, vklist',
				c_rewrite (copy_state(state ,v)) c, k)
	     val con_env = Crecord_c(map (fn (p,_,_,l) => (l,c_rewrite state (path2con p))) vkl_free)
	     val k' = Subst.substConInKind 
			(Subst.fromList [(cenv_var,con_env)]) k
	     val closure_cb = Con_cb(v,Closure_c (Var_c code_var, con_env))
	 in  [code_cb, closure_cb]
	 end			
     | cbnd_rewrite state (Code_cb _) = error "found Code_cb during closure-conversion"

   and c_rewrite state arg_con : con = 
       let val c_rewrite = c_rewrite state
	   fun path_case (v,labels) = 
	   let val fid = current_fid state
	   in  if (is_fid fid)
	       then
		   let
		       val {freecpaths_map,...} = get_frees fid
		       val {cenv_var,...} = get_static fid
		       val res = (case PathMap.find(freecpaths_map,(v,labels)) of
				      NONE => arg_con
				    | SOME (_,cthunk,_) => cthunk cenv_var)
		   in  res
		   end
	       else path2con(v,labels)
	   end
       in
	(case arg_con of
            Prim_c (primcon,clist) => Prim_c(primcon, map c_rewrite clist)
	  | Mu_c (ir,vc_set) => Mu_c(ir,Sequence.fromList 
					(map (fn (v,c) => (v,c_rewrite c)) (Sequence.toList vc_set)))
	  | Var_c v => path_case(v,[])
	  | AllArrow_c (ar,effect,vklist,vlist,clist,numfloats,c) => 
		let val vklist' = map (fn(v,k) => (v,k_rewrite state k)) vklist
		    val clist' = map c_rewrite clist
		    val ar' = (case ar of
				   Open => Closure
				 | Code => Code
				 | Closure => error ("AllArrow_c(Closure,...) " ^ 
						     "during closure-conversion"))
		in  AllArrow_c(ar',effect,vklist',
			       vlist,clist',
			       numfloats,c_rewrite c)
		end
	  | ExternArrow_c (clist,c) => 
		ExternArrow_c(map c_rewrite clist,
			      c_rewrite c)

	  | Let_c (letsort,cbnds,c) => 
		let val cbnds' = List.concat(map (cbnd_rewrite state) cbnds)
		    val c = c_rewrite c
		in  (case (c,cbnds') of
			 (_,[]) => c
		       | (Var_c v, [Con_cb(v',c')]) => if (eq_var(v,v'))
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
	  | Typeof_c e => Typeof_c(e_rewrite state e)
	  | Proj_c _ => 
		let (* labels are backwards *)
		    val (base,labels) = extract_cpath arg_con
		    val do_path = (!do_close_cpath) andalso
				(case base of Var_c _ => true | _ => false)
		in  if not do_path
			then projectc(c_rewrite base,labels)
		    else let val (Var_c v) = base
			 in  path_case(v,labels)
			 end
		end
		    
	  | Closure_c (c1,c2) => error "should not encounter Closure_c during closure-conversion"
	  | App_c (c,clist) => App_c(c_rewrite c, map c_rewrite clist)
	  | Annotate_c (annot,c) => Annotate_c(annot,c_rewrite c))
       end



  and k_rewrite state arg_kind : kind =
       (case arg_kind of 
	    Type_k => arg_kind
	  | Singleton_k c =>  Singleton_k (c_rewrite state c)
	  | (Record_k lvkseq) => let val lvklist = Sequence.toList lvkseq
				     val lvklist = map (fn ((l,v),k) => ((l,v),k_rewrite state k)) lvklist
				 in  Record_k(Sequence.fromList lvklist)
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
	       in  add_gboundevar(state,v,c)
	       end
	     | import_folder (ImportType(l,v,k),state) = 
	       let val f = k_find_fv (state,empty_frees) k
	       in  add_gboundcvar(state,v,k)
	       end
	   val state = foldl import_folder state imports
	   val _ = chat "Closure conversion: Scanned imports\n"

	   val (state,{freeepaths_map,freecpaths,...}) = bnds_find_fv (state,empty_frees) bnds
	   val _ = chat "Closure conversion: Scanned bnds\n"

	   fun export_mapper state (ExportValue(l,e)) = e_find_fv (state, empty_frees) e
	     | export_mapper state (ExportType(l,c)) = c_find_fv (state, empty_frees) c

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

	   fun export_rewrite (ExportValue(l,e)) = ExportValue(l,e_rewrite initial_state e)
	     | export_rewrite (ExportType(l,c)) = ExportType(l,c_rewrite initial_state c)
	   val exports' = map export_rewrite exports
	   val _ = chat "Closure conversion: Rewritten exports\n"

	   val _ = reset_table []

       in  MODULE{bnds = bnds', imports = imports', exports = exports'}
       end	   


end



