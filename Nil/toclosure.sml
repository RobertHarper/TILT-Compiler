(* Closure conversion is accomplished in two phases.  The first phase scans the program
   for free type and term variables of all type and term functions and also notes whether
   a function escapes or not.  The second phase then rewrites the functions into codes
   and closures.  Thus, before closure conversion, there are Fixopen_b and App_e(Open,...).
   After closure conversion, there are Fixcode_b, Fixclosure_b, App_e(Closure,...), and App_e(Code,...).
*)

functor ToClosure(structure Nil : NIL
		  structure NilUtil : NILUTIL
		  structure Ppnil : PPNIL
		  structure Subst : NILSUBST
		  sharing NilUtil.Nil = Ppnil.Nil = Nil
			 and type Subst.con = Nil.con
		         and type Subst.exp = Nil.exp
			 and type Subst.kind = Nil.kind)
    : TOCLOSURE =
struct



    open Util NilUtil Name Nil
    structure Nil = Nil

    val error = fn s => error "toclosure.sml" s
    val debug = ref false
    val liftCode = ref false (* current lifting is wrong - things go out of scope when lifted *)
    val float64 = Prim_c(Float_c Prim.F64,[])
    structure FidSet = VarSet
    type fid = var

    (* -------------- types and values manipulating free variables ------------- *)
    type frees = {freecvars : kind VarMap.map,
		  freeevars : con VarMap.map,
		  boundevars : VarSet.set,
		  boundcvars : VarSet.set}
    val empty_frees = {freeevars = VarMap.empty,
		       freecvars = VarMap.empty,
		       boundevars = VarSet.empty,
		       boundcvars = VarSet.empty}
    fun map_member(m,v) = (case (VarMap.find(m,v)) of
			       NONE => false
			     | SOME _ => true)

    (* returns m1 + (m2 - s) *)
    fun map_union_diff(m1,m2,s) = 
	let fun folder(k,d,m) = if (VarSet.member(s,k))
				    then m
				else VarMap.insert(m,k,d)
	in  VarMap.foldli folder m1 m2
	end
    val set_union = VarSet.union
    fun map_set_remove(m,s) = VarMap.foldli (fn (k,d,m) => if VarSet.member(s,k) 
						then m else VarMap.insert(m,k,d)) VarMap.empty m

    fun join_free'({freeevars=e1,freecvars=c1,boundevars=be1,boundcvars=bc1},
		   {freeevars=e2,freecvars=c2,boundevars=be2,boundcvars=bc2}) = 
	let val be = set_union(be1,be2)
	    val bc = set_union(bc1,bc2)
	    val fe = map_union_diff(e1,e2,be)
	    val fc = map_union_diff(c1,c2,bc)
	    val same = (((VarMap.numItems fe) = (VarMap.numItems e1)) andalso
			((VarMap.numItems fc) = (VarMap.numItems c1)))
	in  (same, {freeevars = fe, freecvars = fc,
		    boundevars = be, boundcvars = bc})
	end

    fun join_free args = #2(join_free' args)
    fun get_freeonly({freeevars,freecvars,boundevars,boundcvars}) = 
	{freeevars = freeevars, freecvars = freecvars,
	 boundevars = VarSet.empty,
	 boundcvars = VarSet.empty}

    fun add_boundcvar_free({freeevars,freecvars,boundevars,boundcvars},v) =
	{freeevars = freeevars, freecvars = freecvars,
	 boundevars = boundevars,
	 boundcvars = VarSet.add(boundcvars,v)}
    fun add_boundevar_free({freeevars,freecvars,boundevars,boundcvars},v) =
	{freeevars = freeevars, freecvars = freecvars,
	 boundcvars = boundcvars,
	 boundevars = VarSet.add(boundevars,v)}



    fun show_free({freeevars, freecvars,boundevars,boundcvars} : frees) =
	(print "freeevars are: "; VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print " ")) freeevars; print "\n";
	 print "freecvars are: "; VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print " ")) freecvars; print "\n";
	 print "boundevars are: "; VarSet.app (fn (v) => (Ppnil.pp_var v; print " ")) boundevars; print "\n";
	 print "boundcvars are: "; VarSet.app (fn (v) => (Ppnil.pp_var v; print " ")) boundcvars; print "\n")

    (* -------------- types and values manipulating functions and related information ------- *)
    type funentry = {static : {fid : fid, 
			       code_var : fid,
			       unpack_var : fid,
			       unpack_type_var : fid},
		     escape : bool,
		     callee : FidSet.set,
		     frees : frees}


    (* ---------------- Global data structure to keep track of function information ----------------- *)
    local
	val empty_table = VarMap.empty : (funentry ref) VarMap.map
	val fids = ref empty_table
	fun empty_fun escape new_fid : funentry = 
	    {static = {fid = new_fid,
		       code_var = fresh_named_var((var2string new_fid)
						  ^ "_code"),
		       unpack_var = fresh_named_var((var2string new_fid)
						    ^ "_unpack"),
		       unpack_type_var = fresh_named_var((var2string new_fid)
							 ^ "_unpacktype")},
	     escape = escape,
	     callee = VarSet.empty,
	     frees = empty_frees}
	val global_escapes = ref (VarSet.empty : Name.VarSet.set)
    in
	fun reset_table escapes = (fids := empty_table;
				   global_escapes := VarSet.addList(VarSet.empty,escapes))
	fun get_fids() = VarMap.foldli (fn (fid,_,acc) => VarSet.add(acc,fid)) VarSet.empty (!fids)
	fun is_fid f = map_member(!fids,f)
	fun add_fun new_fid = 
	    (let val escape = VarSet.member(!global_escapes,new_fid)
	     in  fids := (VarMap.insert(!fids,new_fid,ref (empty_fun escape new_fid)))
	     end)
	    
	fun add_escape esc_fid = 
	    (case (VarMap.find(!fids,esc_fid)) of
		 NONE => error ((Name.var2string esc_fid) ^ "fid not found for add_escape")
	       | SOME (r as (ref{static,escape,callee,frees})) =>
		     r := {static = static,
			   escape = true,
			   callee = callee,
			   frees = frees})
	fun add_callee (caller,callee_fid) = 
	    (case (VarMap.find(!fids,caller)) of
		 NONE => error ((Name.var2string caller) ^ "fid not found for add_callee")
	       | SOME (r as (ref{static,escape,callee,frees})) =>
		     r := {static = static,
			   escape = escape,
			   callee = FidSet.add(callee,callee_fid),
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
	       | SOME (r as (ref{static,escape,callee,frees})) =>
		     r := {static = static,
			   escape = escape,
			   callee = callee,
			   frees = join_free(frees,f)})

	fun augment_frees (fid,evars,cvars) =
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
			 val f = {freeevars=evars,
				  freecvars=cvars,
				  boundevars=VarSet.empty,
				  boundcvars=VarSet.empty}
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



    (* ---------------- code to perform the initial free-variable computation ------------- *)
    local
	datatype expentry = GLOBALe | LOCALe of con | SHADOWe of con
	datatype conentry = GLOBALc | LOCALc of kind | SHADOWc of kind
	datatype state = STATE of {curfid : fid,
				   is_top : bool,
				   boundevars : expentry VarMap.map,
				   boundcvars : conentry VarMap.map,
				   boundfids : FidSet.set}
	fun add_gboundevar (STATE{is_top,curfid,boundevars,boundcvars,boundfids},v) = 
	    let val boundevars' = VarMap.insert(boundevars,v,GLOBALe)
	    in  STATE{is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars',
		      boundcvars = boundcvars,
		      boundfids = boundfids}
	    end
	fun add_gboundcvar (STATE{is_top,curfid,boundevars,boundcvars,boundfids},v) = 
	    let val boundcvars' = VarMap.insert(boundcvars,v,GLOBALc)
	    in  STATE{is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars,
		      boundcvars = boundcvars',
		      boundfids = boundfids}
	    end

	fun add_boundevars' shadow (STATE{is_top,curfid,boundevars,boundcvars,boundfids},vc_list) = 
	    let val wrap = if shadow then SHADOWe else (if is_top then (fn _ => GLOBALe) else LOCALe)
		val boundevars' = foldl (fn ((v,c),m) => VarMap.insert(m,v,wrap c)) boundevars vc_list
	    in  STATE{is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars',
		      boundcvars = boundcvars,
		      boundfids = boundfids}
	    end
	val add_boundevars = add_boundevars' false

	fun add_boundcvars(STATE{is_top,curfid,boundevars,boundcvars,boundfids},vk_list) = 
	    let val wrap = (if is_top then (fn _ => GLOBALc) else LOCALc)
		val boundcvars' = foldl (fn ((v,k),m) => VarMap.insert(m,v,wrap k)) boundcvars vk_list
	    in  STATE{is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars,
		      boundcvars = boundcvars',
		      boundfids = boundfids}
	    end

	fun add_boundfids shadow (STATE{is_top,curfid,boundevars,boundcvars,boundfids},fid_types) = 
	    let val state = STATE{curfid = curfid,
				  is_top = is_top,
				  boundevars = boundevars,
				  boundcvars = boundcvars,
				  boundfids = VarSet.addList(boundfids,map #1 fid_types)}
	    in  add_boundevars' shadow (state,fid_types)
	    end

	fun add_boundevar(s,v,c) = add_boundevars(s,[(v,c)])
	fun add_boundcvar(s,v,k) = add_boundcvars(s,[(v,k)])
	    
	fun is_boundevar(STATE{boundevars,...},evar) = 
	    (case (VarMap.find(boundevars,evar)) of
		 SOME (GLOBALe | (LOCALe _)) => true
	       | _ => false)

	fun is_boundcvar(STATE{boundcvars,...},cvar) = 
	    (case (VarMap.find(boundcvars,cvar)) of
		 SOME (GLOBALc | (LOCALc _)) => true
	       | _ => false)

	fun is_boundfid(STATE{boundfids,...}, f) = VarSet.member(boundfids,f)


	fun locale_map2set m = (VarMap.foldli (fn (v,LOCALe _,s) => VarSet.add(s,v) | (_,_,s) => s) 
				VarSet.empty m)
	fun localc_map2set m = (VarMap.foldli (fn (v,LOCALc _,s) => VarSet.add(s,v) | (_,_,s) => s) 
				VarSet.empty m)

	fun free_cvar (STATE{boundcvars,boundevars,...},cvar) = 
	    {boundevars = locale_map2set boundevars,
	     boundcvars = localc_map2set boundcvars,
	     freeevars = VarMap.empty,
	     freecvars = (case (VarMap.find(boundcvars,cvar)) of
			      SOME (GLOBALc | (LOCALc _)) => VarMap.empty
			    | SOME (SHADOWc k) => VarMap.insert(VarMap.empty,cvar,k)
			    | NONE => error ("free_cvar: variable " ^
					     (var2string cvar) ^ " not bound"))}

	fun free_evar (STATE{boundcvars,boundevars,...},evar) = 
	    {boundevars = locale_map2set boundevars,
	     boundcvars = localc_map2set boundcvars,
	     freeevars = (case (VarMap.find(boundevars,evar)) of
			      SOME (GLOBALe | (LOCALe _)) => VarMap.empty
			    | SOME (SHADOWe c) => VarMap.insert(VarMap.empty,evar,c)
			    | NONE => error ("free_evar: variable " ^
					     (var2string evar) ^ " not bound")),
	    freecvars = VarMap.empty}



	fun initial_state topfid = let val _ = add_fun topfid
				   in   STATE{is_top = true,
					      curfid=topfid,boundfids=VarSet.empty,
					      boundevars = VarMap.empty,
					      boundcvars = VarMap.empty}
				   end

	fun copy_state (STATE{boundevars,boundcvars,boundfids,...}) fid = 
	    let fun epromote (LOCALe c) = SHADOWe c
		  | epromote x = x
		fun cpromote (LOCALc k) = SHADOWc k
		  | cpromote x = x
		val boundevars' = VarMap.map epromote boundevars
		val boundcvars' = VarMap.map cpromote boundcvars
	    in  STATE{is_top = false,
		      curfid=fid,
		      boundfids=boundfids,
		      boundevars = boundevars',
		      boundcvars = boundcvars'}
	    end
	
	fun get_curfid(STATE{curfid,...}) = curfid

    fun remove_free(s as (STATE{boundevars,boundcvars,...}),
		    {freeevars,freecvars,boundcvars=bc,boundevars=be}) : frees = 
	let 
(*
	    val _ = (print "\n\nremove_free...\n";
		     print "\nfreeevars has ";
		     VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print ", ")) freeevars;
		     print "\nboundevars has ";
		     VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print ", ")) boundevars;
		     print "\n")
*)

	    fun efolder (v,_,map) = if (is_boundevar(s,v)) then #1(VarMap.remove(map,v)) else map
	    fun cfolder (v,_,map) = if (is_boundcvar(s,v)) then #1(VarMap.remove(map,v)) else map
	    val freeevars' = VarMap.foldli efolder freeevars freeevars
	    val freecvars' = VarMap.foldli cfolder freecvars freecvars
(*
	    val _ = (print "\nfreeevars' has ";
		     VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print ", ")) freeevars';
		     print "\n")
*)
	in  {freeevars = freeevars',
	     freecvars = freecvars',
	     boundcvars=bc,boundevars=be}
	end

    in  type state = state
	val initial_state = initial_state
	val copy_state = copy_state
	val free_cvar = free_cvar
	val free_evar = free_evar
	val is_boundfid = is_boundfid
	val add_gboundevar = add_gboundevar
	val add_gboundcvar = add_gboundcvar
	val add_boundevar = add_boundevar
	val add_boundcvar = add_boundcvar
	val add_boundevars = add_boundevars
	val add_boundcvars = add_boundcvars
	val add_boundfids = add_boundfids
	val get_curfid = get_curfid
	val remove_free = remove_free
	val is_boundevar = is_boundevar
	val is_boundcvar = is_boundcvar
    end

    fun bnd_find_fv (state : state) bnd : state * frees =
	    let
		fun vkfolder((v,k),(f,s)) = (join_free(f,k_find_fv s k), add_boundcvar(s,v,k))
		fun vcfolder((v,c),(f,s)) = (join_free(f,(c_find_fv s c)), add_boundevar(s,v,c))
		fun fun_type(Function(effect,recur,vklist,vclist,vflist,body,c)) = 
		    AllArrow_c(Open,effect,vklist,(map #2 vclist),TilWord32.fromInt(length vflist),c)
		fun do_fix var_arm_set do_arm do_add get_type =
		    let val var_arm_list = set2list var_arm_set
			val _ = app (fn (fid,_) => do_add fid) var_arm_list
			val local_fids_types = map (fn (v,pf) => (v,get_type pf)) var_arm_list
			val free = (foldl (fn (a,f) => join_free(f,do_arm local_fids_types a))
				    empty_frees (set2list var_arm_set))
		    in  (add_boundfids false (state, local_fids_types), 
			 foldl (fn ((v,_),free) => add_boundevar_free (free,v)) free var_arm_list)
		    end
		val (state,frees) = 
		    (case bnd of
			 Con_b(v,k,c) => let val f = c_find_fv state c
					     val f = join_free(f,k_find_fv state k)
					     val _ = if (!debug)
							 then (print "add_boundcvar ";
							       Ppnil.pp_var v; print "\n")
						     else ()
					 in (add_boundcvar(state,v,k), 
					     add_boundcvar_free(f,v))
					 end
		       | Exp_b(v,c,e) => let val f = e_find_fv state e
					     val f = join_free(f,c_find_fv state c)
					     val _ = if (!debug)
							 then (print "add_boundevar ";
							       Ppnil.pp_var v; print "\n")
						     else ()
					 in (add_boundevar(state,v,c), 
					     add_boundevar_free(f,v))
					 end
		       | Fixopen_b var_fun_set =>
			     let val outer_curfid = get_curfid state
				 fun do_arm fids_types (v,Function(_,_,vklist,vclist,vflist,body,tipe)) =
				 let val local_state = copy_state state v
				     val local_state = add_boundfids true (local_state,fids_types) 
				     val fs = (empty_frees, local_state)
				     val fs = foldl vkfolder fs vklist
				     val (f,s) = (foldl vcfolder fs 
						  (vclist @ (map (fn v => (v,float64)) vflist)))
				     val _ = if (!debug)
						 then (print "the following (after vcfolder) frees to ";
						       Ppnil.pp_var v; print "\n";
						       show_free f; print "\n")
					     else ()
				     val f = join_free(f,e_find_fv s body)
				     val _ = if (!debug)
						 then (print "the following (after body) frees to ";
						       Ppnil.pp_var v; print "\n";
						       show_free f; print "\n")
					     else ()
				     val f = join_free(f,c_find_fv s tipe)
				     val _ = if (!debug)
						 then (print "adding the following frees to ";
						       Ppnil.pp_var v; print "\n";
						       show_free f; print "\n")
					     else ()
				     val _ = add_frees(v,f)
				     val _ = add_callee(outer_curfid,v)
				 in  get_freeonly f
				 end
			     in  do_fix var_fun_set do_arm add_fun fun_type
			     end
		       | Fixcode_b _ => error "there can't be codes while closure-converting"
		       | Fixclosure_b _ => error "there can't be closures while closure-converting")
	    in  (state, remove_free(state,frees))
	    end
	
    and e_find_fv (state : state) exp : frees =
	let val _ = if (!debug)
			 then (print "exp_find_fv called on\n";
			       Ppnil.pp_exp exp; print "\n\n")
		     else ()
	    val res = e_find_fv' state exp
	    val _ = if (!debug)
			 then (print "exp_find_fv called on\n";
			       Ppnil.pp_exp exp; print "\nreturning:\n";
			       show_free res; print "\n\n")
		     else ()

	in  res 
	end

    and e_find_fv' (state : state) exp : frees =
	(if (!debug)
	     then (print "exp_find_fv called on\n";
		   Ppnil.pp_exp exp; print "\n\n")
	 else ();
	 case exp of
	     Var_e v => (if (is_boundfid(state,v)) then add_escape v else ();
			     free_evar(state,v))
	   | Const_e v => 
		 (case v of
		      ((Prim.int _) | (Prim.uint _) | (Prim.float _)) => empty_frees
		    | ((Prim.array (c,array)) | (Prim.vector (c,array))) => 
			  let fun folder(e,f) = join_free(f,(e_find_fv state e))
			      val f = Array.foldl folder empty_frees array
			  in  join_free(f,(c_find_fv state c))
			  end
		    | Prim.refcell (ref e) => (e_find_fv state e)
		    | Prim.tag (_,c) => (c_find_fv state c))
	   | Let_e (_,bnds,e) => (* treat as though a sequential let *)
		      let fun folder (bnd,(state,frees)) = 
			  let val (state,frees') = bnd_find_fv state bnd
			      val newfree = join_free(frees,frees')
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
			  in  (state,newfree)
			  end
			  val (state',funfree) = foldl folder (state,empty_frees) bnds
			  val bodyfree = e_find_fv state' e
			  val almost_free = join_free(funfree,bodyfree)
			  val result = remove_free(state',almost_free)
(*
			  val _ = (print "=================\nLet_e bnd: processed  ";
				   print "funfree is\n";
				   show_free funfree;
				   print "almost_free is\n";
				   show_free almost_free; print "\n";
				   print "result is\n";
				   show_free result; print "\n")
*)
		      in  result
		      end
	   | Prim_e (ap,clist,elist) =>
		 let fun cfold(c,f) = join_free(f,c_find_fv state c)
		     fun efold(e,f) = join_free(f,e_find_fv state e)
		     val free = foldl cfold empty_frees clist
		     val free = foldl efold free elist
		 in  free
		 end
	   | Switch_e switch => 
		 let
		     fun do_sw {info, arg, arms, default} do_info do_arg do_prearm =
			 let val f1 = do_info info
			     val f2 = do_arg arg
			     fun do_arm(pre,Function(_,_,vklist,vclist,vflist,body,tipe)) =
				 let 
				     fun vkfolder((v,k),(f,s)) = (join_free(f,k_find_fv s k),
								  add_boundcvar(s,v,k))
				     fun vcfolder((v,c),(f,s)) =
					 let val f' = c_find_fv s c
					 in (join_free(f,f'), add_boundevar(s,v,c))
					 end
				     val (f,s) = (do_prearm pre, state)
				     val (f,s) = foldl vkfolder (f,s) vklist
				     val (f,s) = foldl vcfolder (f,s) 
					 (vclist @ (map (fn v => (v,float64)) vflist))
				     val f = join_free(f,e_find_fv s body)
				     val f = join_free(f,c_find_fv s tipe)
				 in  f
				 end
			     val f3 = map do_arm arms
			     val f4 = (case default of
					   NONE => empty_frees
					 | SOME e => e_find_fv state e)
			 in foldl join_free f4 (f1::f2::f3)
			 end
		     fun nothing _ = empty_frees
		 in	 case switch of
		     Intsw_e sw => do_sw sw nothing (e_find_fv state) nothing
		   | Sumsw_e sw => (do_sw sw (fn (tagcount,clist) =>
					      foldl (fn (c,f) => join_free(f,c_find_fv state c)) 
					      empty_frees clist)
				    (e_find_fv state) nothing)
		   | Exncase_e sw => do_sw sw nothing (e_find_fv state) (e_find_fv state)
		   | Typecase_e sw => do_sw sw nothing (c_find_fv state) nothing
		 end
	   | App_e (_, e, clist, elist, eflist) =>
		 let val free1 = (case e of
				      Var_e v => if (is_fid v)
						     then (
(*							   print "***** adding callee ";
							   Ppnil.pp_var v; print " to ";
							   Ppnil.pp_var (get_curfid state); print "\n";
*)
							   add_callee(get_curfid state,v);
							   empty_frees)
						 else (e_find_fv state e)
				    | _ => (e_find_fv state e))
		     val free2 = map (c_find_fv state) clist
		     val free3 = map (e_find_fv state) elist
		     val free4 = map (e_find_fv state) eflist
		     val joiner = foldl join_free
		 in joiner (joiner (joiner free1 free2) free3) free4
		 end
	   | Raise_e (e,c) => join_free(e_find_fv state e, c_find_fv state c)
	   | Handle_e (e,Function(_,_,[],[(v,c)],[],body,tipe)) =>
		 let val f = e_find_fv state e
		 in  (foldl join_free empty_frees
		      [f, (c_find_fv state c),
		       (e_find_fv (add_boundevar(state,v,c)) body),
		       (c_find_fv state tipe)])
		 end
	   | Handle_e _ => error "ill-typed Handle_e")

		 
	and c_find_fv (state : state) con : frees =
	    (if (!debug)
		 then (print "c_find_fv called on\n";
		       Ppnil.pp_con con; print "\n\n")
	     else ();
		 case con of
		 Prim_c (pc,clist) => foldl (fn (c,f)=> join_free(f,(c_find_fv state c))) empty_frees clist
	       | Mu_c (vcset,v) =>
		     let val vclist = set2list vcset
			 val state' = add_boundcvars(state,map (fn (v,_) => (v,Word_k Runtime)) vclist)
		     in  foldl (fn ((v,c),f) => join_free(f,c_find_fv state' c))
			 empty_frees (set2list vcset)
		     end
	       (* the types of some primitives like integer equality are Code arrow *)
	       | AllArrow_c((ExternCode | Code | Open | Closure),_,vklist,clist,numfloats,c) =>
		     let fun vkfolder((v,k),(f,s)) = (join_free(f,k_find_fv s k),
						      add_boundcvar(s,v,k))
			 fun cfolder (c,(f,s)) = (join_free(f, (c_find_fv s c)),s)
			 val fs = foldl vkfolder (empty_frees,state) vklist
			 val (f,s) = foldl cfolder fs clist
		     in  join_free(f, c_find_fv s c)
		     end
	       | Var_c v => free_cvar(state,v)
	       | Typecase_c {arg, arms, default, kind} => 
		     let val f = c_find_fv state arg
			 val f = join_free(f,c_find_fv state default)
			 val f = join_free(f,k_find_fv state kind)
			 fun armfolder ((pc,vklist,c),f) = 
			     let fun folder((v,k),(f,s)) = (join_free(f,k_find_fv s k),
							    add_boundcvar(s,v,k))
				 val (f,_) = foldl folder (f,state) vklist
			     in join_free(f,c_find_fv state c)
			     end
		     in  foldl armfolder f arms
		     end
	       | Let_c(_,cbnds,c) => 
		     let 
			 fun cb_folder (Con_cb(v,k,c),(f,s)) =
			     let val f' = c_find_fv s c
			     in (join_free(f, f'), add_boundcvar(s,v,k))
			     end
			   | cb_folder (Code_cb(v,vklist,c,k), _) = error "code_cb during closure conversion"
			   | cb_folder (Open_cb(v,vklist,c,k), (f,s)) =
			     let val _ = add_fun v
				 fun folder((v,k),(f,s)) = 
				     (join_free(f,k_find_fv s k),
				      add_boundcvar(s,v,k))
				 val (f,s) = foldl folder (empty_frees,state) vklist
				 val f' = c_find_fv s c
			     in  (join_free(f, f'), add_boundcvar(s,v,k))
			     end
			 val (f,s) = foldl cb_folder (empty_frees,state) cbnds
			 val f' = remove_free(s,c_find_fv s c)
		     in  join_free(f, f')
		     end
	       | Crecord_c lclist => let val flist = map (fn (_,c) => c_find_fv state c) lclist
				     in foldl join_free empty_frees flist
				     end
	       | Proj_c (c,l) => c_find_fv state c
	       | Closure_c _ => error "can't have closure_c during closure-conversion"
	       | App_c(c,clist) => let val f = c_find_fv state c
				   in foldl (fn (c,f) => join_free(f,c_find_fv state c))
				       f clist
				   end
	       | Annotate_c (_,c) => c_find_fv state c)
		 
	and k_find_fv state kind : frees =
	    (case kind of
	    ((Type_k _) | (Word_k _)) => empty_frees
	  | (Singleton_k (p,k,c)) => (k_find_fv state k; (c_find_fv state c))
	  | (Record_k lv_kind_seq) =>
		 let fun folder (((l,v),k),(f,s)) = (join_free(f,k_find_fv s k), add_boundcvar(s,v,k))
		 in  #1(foldsequence folder (empty_frees,state) lv_kind_seq)
		 end
	  | (Arrow_k (Open,vklist,k)) =>
		let fun folder ((v,k),s) = (k_find_fv s k; add_boundcvar(s,v,k))
		    val state' = foldl folder state vklist
		in  k_find_fv state' k
		end
	  | (Arrow_k _) => error "can't have arrow_k(code|closure,...) during closure-conversion") 




   (* ------- compute the final free-variable list by closing over the callgraph ------ *)

   local
       fun close_fun (curfid,nextset) = 
	   let val callees = get_callee curfid
(*	       val _ = VarSet.app (fn x => (print "\n    callee_fvs is: "; show_free (get_frees x); print "\n")) callees *)
	       val callee_fvs = VarSet.foldl (fn (f,fv) => join_free(get_frees f, fv)) empty_frees callees
	       val {freeevars=fe,freecvars=fc,...} = callee_fvs
(*	       val _ = (print "\n\nfinal callee_fvs is: "; show_free callee_fvs; print "\n") *)
	       val changed = augment_frees(curfid,fe,fc)
	   in if changed
		  then VarSet.add(nextset,curfid)
	      else nextset
	   end
   in  fun close_funs workset = 
       let  val _ = if (!debug)
			then (print "before close_funs\n";
			      VarSet.app (fn fid => (print ("fid = ");
						     Ppnil.pp_var fid; print " --   callees are "; 
						     VarSet.app (fn fid => (Ppnil.pp_var fid; print " ")) 
						     (get_callee fid);
						     print "\n";
						     show_free(get_frees fid); print "\n")) (get_fids());
			      print "\n\n")
		    else ()
	    fun loop() = 
		let 
		    val nextset = (VarSet.foldl close_fun VarSet.empty workset)
		in  if (VarSet.isEmpty nextset)
			then ()
		    else loop()  (* note that we must start with the whole set again *)
		end
	   val _ = if (!debug)
		       then (print "after all close_funs\n";
			     VarSet.app (fn fid => (print ("fid = ");
						    Ppnil.pp_var fid; print "\n"; 
						    show_free(get_frees fid))) (get_fids());
			     print "\n\n")
		   else ()
       in  loop()
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
				       rectype, [Var_e evar])


   local
       val ebnds = ref ([] : bnd list)
       val cbnds = ref ([] : conbnd list)
   in
       fun reset_bnds() = (ebnds := []; cbnds := [])
       fun get_bnds() = (!ebnds,!cbnds)
       fun eadder eb = ((* print "adding ebnd\n";  *)
			ebnds := (eb :: (!ebnds)))
       fun cadder cb = ((* print "adding cbnd\n"; Ppnil.pp_conbnd cb; print " end of cbnd\n"; *)
			cbnds := (cb :: (!cbnds)))
   end

   fun fun_rewrite lift (v,Function(effect,recur,vklist,vclist,vflist,body,tipe)) = 
       let 
	   val _ = if (!debug)
		       then (print "fun_rewrite v = "; Ppnil.pp_var v; print "\n")
		   else ()
	   val {code_var, unpack_var, ...} = get_static v
	   val {freeevars,freecvars,...} = get_frees v
	   val vklist = map (fn (v,k) => (v,k_rewrite k)) vklist
	   val vclist = map (fn (v,c) => (v,c_rewrite lift c)) vclist
	   val escape = get_escape v
	   val vk_free = (VarMap.listItemsi freecvars)
	   val vc_free = (VarMap.listItemsi freeevars)
	   val vk_free = map (fn (v,k) => (v,k_rewrite k)) vk_free
	   val vc_free = map (fn (v,c) => (v,c_rewrite lift c)) vc_free
	   val _ = if (!debug)
		       then (print "fun_rewrite v = "; Ppnil.pp_var v;
			     print "\nvk_free are "; 
			     app (fn (v,_) => (Ppnil.pp_var v; print ", ")) vk_free;
			     print "\nvc_free are "; 
			     app (fn (v,_) => (Ppnil.pp_var v; print ", ")) vc_free;
			     print "\n")
		   else ()
	   val num_vk_free = length vk_free
	   val num_vc_free = length vc_free
	   val cenv_var = fresh_named_var "free_cons"
	   val venv_var = fresh_named_var "free_exp"
	   val cenv_kind = kind_tuple(map #2 vk_free)
	   val vc_free_types = map #2 vc_free
	   val venv_type = con_tuple vc_free_types
	   val vklist_code = vklist @ vk_free
	   val vclist_code = vclist @ vc_free
	   val vklist_unpack_almost = vklist @ [(cenv_var,cenv_kind)]
	   val vclist_unpack_almost = vclist @ [(venv_var,venv_type)]

	   val subst_list = 
	     let 
	       fun loop n [] = []
		 | loop n ((v',_)::rest) = 
		 (v',Proj_c(Var_c cenv_var, generate_tuple_label n))::(loop (n+1) rest)
	     in  loop 1 vk_free
	     end
	   val subst = Subst.fromList subst_list
	   val vklist_unpack = map (fn (v,k) => (v,Subst.substConInKind subst k)) vklist_unpack_almost
	   val vclist_unpack = map (fn (v,c) => (v,Subst.substConInCon subst c)) vclist_unpack_almost
	   val codebody_tipe = c_rewrite lift tipe
	   val unpackbody_tipe = Subst.substConInCon subst codebody_tipe
	   val closure_tipe = AllArrow_c(Closure,effect,
					 vklist,map #2 vclist,
					 TilWord32.fromInt(length vflist),codebody_tipe)
	   val code_fun = Function(effect,recur,
				   vklist_code,vclist_code,vflist,
				   e_rewrite lift body, codebody_tipe)
	   val unpack_body = 
	       App_e(Code,Var_e code_var,
		     (map (Var_c o #1) vklist) @ 
		     (Listops.map0count (cproj cenv_var) num_vk_free),
		     (map (Var_e o #1) vclist) @ 
		     (Listops.map0count (eproj (venv_var,vc_free_types)) num_vc_free),
		     map Var_e vflist)
	   val unpack_fun = Function(effect,recur,
				     vklist_unpack,vclist_unpack,vflist,
				     unpack_body, unpackbody_tipe)
	   val closure = {code = unpack_var,
			  cenv = con_tuple_inject(map (Var_c o #1) vk_free),
			  venv = exp_tuple(map (fn (v,c) => (Var_e v, c)) vc_free),
			  tipe = closure_tipe}
       in if escape then ([(code_var,code_fun),
			   (unpack_var,unpack_fun)],
			  [(v,closure)])
	  else ([(code_var,code_fun)],[])
       end

   and bnd_rewrite' lift (bound,bnd) : bnd list changeopt = 
       let
	   fun funthing_helper rewriter var_thing_set =
	       let 
		   val pfun_close = mapset rewriter var_thing_set
		   val pfun_list = List.concat(map #1 pfun_close)
		   val pfun_bnd = Fixcode_b (list2set pfun_list)
		   val closure_bnd_list = 
		       (case (List.concat(map #2 pfun_close)) of
			    [] => []
			  | close_list => [Fixclosure_b(list2set close_list)])
	       in  if lift
		       then ((* print "calling adder in bnd_rewrite'\n"; *)
			     eadder pfun_bnd;
			     closure_bnd_list)
		   else pfun_bnd :: closure_bnd_list
	       end
       in CHANGE_NORECURSE
	   (case bnd of
		(Con_b(v,k,c)) => [Con_b(v,k_rewrite k, c_rewrite lift c)]
	      | (Exp_b(v,c,e)) => [Exp_b(v,c_rewrite lift c, e_rewrite lift e)]
	      | (Fixclosure_b _) => error "there can't be closures while closure-converting"
	      | (Fixcode_b _) => error "there can't be codes while closure-converting"
	      | (Fixopen_b var_fun_set) => funthing_helper (fun_rewrite lift) var_fun_set)
       end

   and make_handlers lift = 
	    (e_rewrite' lift, 
	     bnd_rewrite' lift,
	     c_rewrite' lift,
	     fn (_,cb : conbnd) => NOCHANGE,
	     k_rewrite')


   and bnd_rewrite lift (arg_bnd : bnd) : bnd list =
       let val handlers = make_handlers lift
	   val bnds = NilUtil.bnd_rewrite handlers arg_bnd
       in  bnds
       end

   and e_rewrite lift (arg_exp : exp) : exp  = 
       let val handlers = make_handlers lift
	   val e = NilUtil.exp_rewrite handlers arg_exp
       in  e
       end

   and e_rewrite' lift (bound,arg_exp) : exp changeopt =
       (case arg_exp of
	    Var_e v => NOCHANGE (* closures retain their name *)
	  | Const_e v => NOCHANGE
	  | Prim_e(p,clist,elist) => NOCHANGE
	  | Switch_e switch => NOCHANGE
	  | Let_e(letsort,bnds,e) => NOCHANGE
	  | App_e (Closure, _,_,_,_) => error "App_e(Code|Closure,...) during closure conversion"
	  | App_e (ar as (ExternCode | Code), e, clist, elist, eflist) => 
		let val clist' = map (c_rewrite lift) clist
		    val elist' = map (e_rewrite lift) elist
		    val eflist' = map (e_rewrite lift) eflist
		    val e' = e_rewrite lift e
		in  CHANGE_NORECURSE(App_e(ar, e', clist', elist', eflist'))
		end
	  | App_e (Open, e, clist, elist, eflist) => 
		let val clist' = map (c_rewrite lift) clist
		    val elist' = map (e_rewrite lift) elist
		    val eflist' = map (e_rewrite lift) eflist
		    fun docall (cv,{freeevars, freecvars,...} : frees) = 
			let val clist'' = map (fn (v,_) => Var_c v) (VarMap.listItemsi freecvars)
			    val elist'' = map (fn (v,_) => Var_e v) (VarMap.listItemsi freeevars)
			in App_e(Code, Var_e cv, clist' @ clist'', elist' @ elist'', eflist')
			end
		    fun default() = App_e(Closure,e_rewrite lift e, clist', elist', eflist')
		in CHANGE_NORECURSE
		    (case e of
			 Var_e v => if (is_fid v)
					then let val {code_var,...} = get_static v
					     in  docall(code_var,get_frees v)
					     end
				    else default()
		       | _ => default())
		end
	  | Raise_e (e,c) => NOCHANGE
	  | Handle_e _ => NOCHANGE)

   and cbnd_rewrite lift (Con_cb(v,k,c)) = [Con_cb(v,k_rewrite k, c_rewrite lift c)]
     | cbnd_rewrite lift (Open_cb(v,vklist,c,k)) = 
       let val _ = if (!debug)
		       then (print "cbnd_rewrite v = "; Ppnil.pp_var v; print "\n")
		   else ()
	   val {code_var, unpack_var, ...} = get_static v
	   val {freeevars,freecvars,...} = get_frees v (* freeevars must be empty *)
	   val vk_free = (VarMap.listItemsi freecvars)
	   val cfv_var = fresh_named_var "free_cons"
	   fun get_cbnd (i,(v,k)) = Con_cb(v,k, Proj_c(Var_c cfv_var,
						       generate_tuple_label(i+1)))
	   val cbnds = Listops.mapcount get_cbnd vk_free
	   val vklist' = vklist @ [(cfv_var, kind_tuple(map #2 vk_free))]
	   val code_cb = Code_cb(code_var, vklist',
				letc(cbnds,(c_rewrite lift c)), k)
	   val con_env = con_tuple_inject(map (fn (v,k) => Var_c v) vk_free)
	   val closure_cb = Con_cb(v,Arrow_k(Closure,vklist,k),
				   Closure_c (Var_c code_var, con_env))
       in  if lift
	       then (cadder code_cb; [closure_cb])
	   else [code_cb, closure_cb]
       end			
     | cbnd_rewrite _ (Code_cb _) = error "found Code_cb during closure-conversion"

   and c_rewrite' lift (bound, arg_con) : con changeopt = 
       (case arg_con of
	    Prim_c (primcon,clist) => NOCHANGE
	  | Mu_c (vc_set,v) =>  NOCHANGE
	  | Var_c v => NOCHANGE
	  | AllArrow_c (ar as (Open | ExternCode | Code),effect,vklist,clist,numfloats,c) => 
		let val vklist' = map (fn(v,k) => (v,k_rewrite k)) vklist
		    val ar' = (case ar of
				   Open => Closure
				 | Code => Code
				 | ExternCode => ExternCode
				 | Closure => error "control can't reach here")
		in  CHANGE_NORECURSE(AllArrow_c(ar',effect,vklist',
						map (c_rewrite lift) clist,
						numfloats,c_rewrite lift c))
		end
	  | AllArrow_c (Closure,_,_,_,_,_) => error "AllArrow_c(Closure,...) during closure-conversion"
	  | Let_c (letsort,cbnds,c) => 
		let val cbnds' = List.concat(map (cbnd_rewrite lift) cbnds)
		    val c = c_rewrite lift c
		in  CHANGE_NORECURSE(case (c,cbnds') of
					 (_,[]) => c
				       | (Var_c v, [Con_cb(v',_,c')]) => if (eq_var(v,v'))
									    then c'
									else Let_c(letsort,cbnds',c)
				       | _ => Let_c(letsort,cbnds',c))
		end
	  | Typecase_c {arg,arms,default,kind} => NOCHANGE
	  | Crecord_c lclist => NOCHANGE
	  | Proj_c (c,l) => NOCHANGE
	  | Closure_c (c1,c2) => error "should not encounter Closure_c during closure-conversion"
	  | App_c (c,clist) => NOCHANGE
	  | Annotate_c (annot,c) => NOCHANGE)

   and k_rewrite arg_kind : kind = 
       let val handlers = make_handlers(false)
	   val k = NilUtil.kind_rewrite handlers arg_kind
       in  k
       end

   and c_rewrite lift arg_con : con =
       let val handlers = make_handlers lift
	   val c = NilUtil.con_rewrite handlers arg_con
       in  c
       end

   and k_rewrite' (bound,arg_kind) : kind changeopt = 
       (case arg_kind of 
	    ((Type_k _) | (Word_k _) | 
	     (Singleton_k _) | (Record_k _)) => NOCHANGE
	  | Arrow_k (Open,vklist,k) => 
		let val vklist' = map (fn (v,k) => (v,k_rewrite k)) vklist
		    val k' = Arrow_k(Closure, vklist', k_rewrite k)
		in CHANGE_NORECURSE k'
		end
	  | Arrow_k _ => error "cannot encounter arrow_k(Code/Closure,...) during closure-conversion")

(*
   fun close_exp arg_exp = 
       let val _ = reset_table []
	   val top_fid = fresh_named_var "top_fid"
	   val state = initial_state top_fid
	   val {freeevars,freecvars,...} = e_find_fv state arg_exp
	   val _ = if (!debug)
		       then (print "Done with e_find_fv\n";
			     print "free is empty: ";
			     print (Bool.toString (VarMap.numItems freeevars = 0
						   andalso VarMap.numItems freecvars = 0));
			     print "\n")
		   else ()
	   val _ = close_funs(get_fids())
	   val _ = if (!debug)
		       then print "Done with close_funs\n"
		   else ()
	   val result = e_rewrite lift arg_exp
       in  result
       end	   
*)
   fun close_con' state arg_con = 
       let 
	   val {freeevars,freecvars,...} = c_find_fv state arg_con
	   val _ = if (!debug)
		       then (print "Done with c_find_fv\n";
			     print "free is empty: ";
			     print (Bool.toString (VarMap.numItems freeevars = 0
						   andalso VarMap.numItems freecvars = 0));
			     print "\n")
		   else ()
	   val _ = close_funs(get_fids())
	   val _ = if (!debug)
		       then print "Done with close_funs\n"
		   else ()
	   val _ = reset_bnds()
	   val result = c_rewrite true arg_con
	   val (bnds,cbnds) = get_bnds()
       in  (case (bnds,cbnds) of
		([],cbnds) => letc(rev cbnds, result)
	      | _ => error "c_rewrite has bnds")
       end	   

   fun close_kind' state arg_kind = 
       let 
	   val {freeevars,freecvars,...} = k_find_fv state arg_kind
	   val _ = if (!debug)
		       then 
			   (print "Done with k_find_fv\n";
			   print "free is empty: ";
			   print (Bool.toString (VarMap.numItems freeevars = 0
						 andalso VarMap.numItems freecvars = 0));
			   print "\n")
		   else ()
	   val _ = close_funs(get_fids())
	   val _ = if (!debug)
		       then print "Done with close_funs\n"
		   else ()
	   val _ = reset_bnds()
	   val result = k_rewrite arg_kind
       in  result
       end	   

   fun close_con con = 
       let val _ = reset_table []
       in close_con' (initial_state (fresh_named_var "top_fid")) con
       end 
   fun close_kind kind = 
       let val _ = reset_table []
       in  close_kind' (initial_state (fresh_named_var "top_fid")) kind 
       end
   fun close_mod (MODULE{bnds, imports, exports}) = 
       let val _ = reset_table []
	   val top_fid = fresh_named_var "top_fid"
	   val state = initial_state top_fid
	   fun import_mapper (ImportValue(l,v,c),(imports,state)) = 
	       let 
		   val c' = close_con' state c
		   val state = add_gboundevar(state,v)
	       in   ((ImportValue(l,v,c'))::imports,state)
	       end
	     | import_mapper (ImportType(l,v,k),(imports,state)) = 
	       let val k' = close_kind' state k
		   val state = add_gboundcvar(state,v)
	       in  ((ImportType(l,v,k'))::imports,state)
	       end
	   val (rev_imports,state) = foldl import_mapper ([],state) imports
	   val imports = rev rev_imports
	   fun export_mapper (ExportValue(l,e,c)) = Exp_b(fresh_var(),c,e)
	     | export_mapper (ExportType(l,c,k)) = Con_b(fresh_var(),k,c)
	   val export_bnds = map export_mapper exports
	   val arg_exp = Let_e(Sequential, bnds, 
			       Let_e(Sequential, export_bnds, true_exp))
	   val {freeevars,freecvars,...} = e_find_fv state arg_exp
	   val _ = if (!debug)
		       then (print "Done with e_find_fv\n";
			     print "free is empty: ";
			     print (Bool.toString (VarMap.numItems freeevars = 0
						   andalso VarMap.numItems freecvars = 0));
			     print "\n")
		   else ()
	   val _ = close_funs(get_fids())
	   val _ = if (!debug)
		       then print "Done with close_funs\n"
		   else ()

	   fun folder (bnd,acc) = 
	       let val _ = reset_bnds()
		   val bnds = bnd_rewrite (!liftCode) bnd
		   val (ebnds,cbnds) = get_bnds()
		   val bnds' = (map cbnd2bnd (rev cbnds)) @ (rev ebnds) @ bnds
	       in  acc @ bnds'
	       end
	   val bnds' = foldl folder [] bnds
	   fun export_rewrite (ExportValue(l,e,c)) = ExportValue(l,e_rewrite false e,c_rewrite false c)
	     | export_rewrite (ExportType(l,c,k)) = ExportType(l,c_rewrite false c,k_rewrite k)
	   val exports' = map export_rewrite exports
       in  MODULE{bnds = bnds', imports = imports, exports = exports'}
       end	   


end