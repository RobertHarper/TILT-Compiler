(*$import HOIST Nil NilUtil ListPair Stats Name *)
(*** questions, should fixopen_b ever move? *)
(* 
 var naming convections
 bvl/bvs : (bnd * var set) list
*)
structure Hoist :> HOIST = 

struct
  open Nil Name

  exception HoistError

  val debug = Stats.ff("HoistDebug")
  val hoist_through_switch = Stats.ff("HoistThroughSwitch")
  fun diag s = if !debug then print s else ()

  (* set helper funs *)
  structure Set = Name.VarSet
  local open Set 
  in
    type hoist = bnd * set
    type set = set
    val empty = empty
    val add = add
    val set2list = listItems
    fun list2set x = addList(empty,x)
    val union = union
    val subset = isSubset
    val oneset = singleton
    fun disjoint(s1,s2) = 
	let
	    val (s1',s2') = if (numItems s1) < (numItems s2) then (s1, s2)
                           else (s2, s1)
	in
	    not (exists (fn v => member(s1',v)) s2')
	end
    val unionList = List.foldl Set.union Set.empty 
  end
  (* end set help funs *)

  datatype bndtype = STAY of bnd * set | UP of var | TOP of var
  datatype conbndtype = CSTAY of conbnd * set | CUP of var | CTOP of var


  datatype hoist_effs =
      ARROW_EFFS of Nil.effect * hoist_effs
    | REC_EFFS of (Nil.label * hoist_effs) list
    | UNKNOWN_EFFS

  structure Map = Name.VarMap
  type effect_context = hoist_effs Map.map
  val empty_fnmap = Map.empty : effect_context
  val unknown_effs = UNKNOWN_EFFS
  fun elookup (econtext:effect_context,v) =
      (case Map.find(econtext,v) of
	   NONE => unknown_effs
	 | SOME effs => effs)
  fun ereclookup (effs, l) =
      (case effs of
	   REC_EFFS lst => 
	       (case (Listops.assoc_eq (Name.eq_label,l,lst)) of
		    SOME effs => effs
		  | NONE => UNKNOWN_EFFS)
	 | _ => UNKNOWN_EFFS)

  fun con2eff (AllArrow_c{effect,body_type,...}) = 
        ARROW_EFFS (effect, con2eff body_type)
    | con2eff (Prim_c(Record_c (lbls,_), types)) =
        REC_EFFS (ListPair.zip (lbls, map con2eff types))
    | con2eff _ = UNKNOWN_EFFS

  type bvlt = bnd * var list

  (* returns set of free vars *)
  fun freeExpVars e = 
      let 
	  val (free_term_vars, free_type_vars) = 
	      NilUtil.freeExpConVarInExp (true,e)
      in 
	  list2set (free_term_vars @ free_type_vars)
      end

  (* returns set of free type vars *)
  fun freeConVars c = 
      list2set (NilUtil.freeVarInCon c)

  (* stop junk *)

  fun filter_bnds (bvl, stop_vars) =
      let
	  fun loop([],stop_vars,up,stay,freev) = (rev up, rev stay, freev)
	    | loop((bv as (eb,freevars))::rest, stop_vars, up, stay, freev) =
	      if (disjoint(freevars,stop_vars)) then
		  loop(rest, stop_vars, bv :: up, stay, freev)
	      else
		  loop(rest, Set.addList(stop_vars,NilUtil.varsBoundByBnds [eb]),
		       up, eb :: stay, Set.union(freev, freevars))
      in
	  loop(bvl, stop_vars, nil, nil, Set.empty)
      end


  fun filter_cbnds (hoists, stop_vars) =
      let
	  fun loop([], _, up, stay, freev) = (rev up, rev stay, freev)
	    | loop((bv as (cb,freevars))::rest, stop_vars, up, stay, freev) =
	      if (disjoint(freevars,stop_vars)) then
		  loop(rest, stop_vars, bv :: up, stay, freev)
	      else
		  loop(rest, Set.add(stop_vars,NilUtil.varBoundByCbnd cb),
		       up, cb :: stay, Set.union(freev, freevars))
      in
	  loop(hoists, stop_vars, nil, nil, Set.empty)
      end



  fun filter_ebnds hoists =
      let
	  fun loop([],stop_vars,up,stay, freev) = (rev up, rev stay, freev)
            | loop((bv as (eb as Con_b(_,cb),freevars))::rest, 
		   stop_vars, up, stay, freev) =
	         if (disjoint(freevars,stop_vars)) then
		     loop(rest, stop_vars, bv :: up, stay, freev)
		 else
		     loop(rest, Set.add(stop_vars,NilUtil.varBoundByCbnd cb),
			  up, eb :: stay, Set.union(freev, freevars))
 	    | loop((bv as (eb,freevars))::rest, stop_vars, up, stay, freev) =
		  loop(rest, Set.addList(stop_vars,NilUtil.varsBoundByBnds [eb]),
		       up, eb :: stay, Set.union(freev, freevars))
      in
	  loop(hoists, Set.empty, [], [], Set.empty)
      end

  (* rconbnd : conbndtype * conbnd list * (conbnd * set) list 
        returns: 1) whether or not the binding is being hoisted
                    (and if not, a rewritten variant of the binding)
                 2) 
   *)

  fun rconbnd (Con_cb(v,con), top_set, hoist_set) =
      let
	  val (con', top_cbnds, choists, freev) = 
	      rcon (con, top_set, hoist_set)
      in
	  if (subset(freev, top_set)) then
	      (CTOP v, top_cbnds @ [Con_cb(v,con')], choists)
	  else if (subset(freev, hoist_set)) then
	      (CUP v, top_cbnds, choists @ [(Con_cb(v,con'),freev)])
	  else
	      (CSTAY (Con_cb (v,con'), freev), top_cbnds, choists)
    end

    | rconbnd (Open_cb(v,vklist,con_body), top_set, hoist_set) = 
        (* We don't hoist functions *)
        (*** Morgan: if this is changed (ie to hoist open_cbs) then 
             code_cb wont work *)
	let
	  val (con_body', top_cbnds, choists, freev) = 
	      rcon (con_body,top_set,hoist_set)
	  val (vars, kinds) = Listops.unzip vklist
	  val freev = 
	      unionList (freev :: map (list2set o NilUtil.freeVarInKind) kinds)
	  val freev = Set.difference(freev, list2set vars)
	in
	  (CSTAY(Open_cb(v,vklist,con_body'),freev), top_cbnds, choists)
	end

    | rconbnd ((Code_cb stuff), top_set, hoist_set) = 
	let
	  val (CSTAY(Open_cb stuff', freev), top_cbnds, choists) = 
	      rconbnd (Open_cb stuff, top_set, hoist_set)
	in
	  (CSTAY(Code_cb stuff', freev), top_cbnds, choists)
	end

  and rconbnd' arg = 
      let val (ans, top_cbnds, choists) = rconbnd arg
      in
	  (ans, 
	   map (fn cb => Con_b(Runtime,cb)) top_cbnds,
	   map (fn (cb,s) => (Con_b(Runtime,cb), s)) choists)
      end

  and rconbnds (conbnds, top_set, hoist_set) = 
    let
      fun loop ([],top_set,hoist_set,top,up,stay,freev) = 
	   (top_set, hoist_set,
	    List.concat(rev top), List.concat(rev up), rev stay, freev)
	| loop (cb::rest,top_set,hoist_set,top,up,stay,freev) = 
	   (case rconbnd (cb,top_set,hoist_set) of 
		(CSTAY (cb',freev'),top_cbnds, choists) => 
                    loop (rest, top_set, hoist_set,
			  top_cbnds :: top, choists :: up, cb'::stay,
			  Set.union(freev,freev'))
	      | (CUP v, top_cbnds, choists) => 
		    let
			val hoist_set' = Set.add(hoist_set, v)
		    in
			loop(rest, top_set, hoist_set',
			     top_cbnds :: top, choists :: up, stay,
			     freev)
		    end
	      | (CTOP v, top_cbnds, choists) => 
		    let
			val hoist_set' = Set.add(hoist_set, v)
			val top_set' = Set.add(top_set, v)
		    in
			loop(rest, top_set', hoist_set',
			     top_cbnds :: top, choists :: up, stay,
			     freev)
		    end)
      val (top_set, hoist_set, top, up, stay, freev) = 
	  loop (conbnds, top_set, hoist_set, [], [], [], Set.empty)
	  
      val boundv = list2set (map NilUtil.varBoundByCbnd stay)
      val freev = Set.difference(freev, boundv)
    in
      (top_set, hoist_set, top, up, stay, freev)
    end

  and rcon (Prim_c (primcon,conlist), top_set, hoist_set) =
      let
	  val (conlist', top_cbnds, choists, freev) = 
	      rcons (conlist, top_set, hoist_set)
      in
	  (Prim_c(primcon,conlist'), top_cbnds, choists, freev)
      end

    | rcon (Mu_c (isRecursive,vcseq), top_set, hoist_set) = 
      let
	  val vclist = Sequence.toList vcseq
	  fun mapper (v,c) = 
	      let
		  val (c', top_cbnds, hoists, freev) = 
		      rcon(c, top_set, hoist_set)
	      in
		  ((v,c'), top_cbnds, hoists, freev)
	      end
	  val (vclist'',top_cbnds_list, hoists_list, freev_list) = 
	      Listops.unzip4 (map mapper vclist)
	  val top_cbnds = List.concat top_cbnds_list
	  val hoists = List.concat hoists_list
	  val freev = 
	      Set.difference(unionList freev_list,
			     list2set (map #1 vclist))
      in
	  (Mu_c (isRecursive,Sequence.fromList vclist''),
	   top_cbnds, hoists, freev)
      end

    | rcon (con as AllArrow_c {openness, effect, isDependent, tFormals,
			       eFormals, fFormals, body_type}, 
	    top_set, hoist_set) =
      let
	  val (eNames,eTypes) = ListPair.unzip eFormals
	  val (eTypes', top_cbnds, choists, freev) = 
	      rcons(eTypes, top_set, hoist_set)
	  val eFormals' = ListPair.zip (eNames,eTypes')

	  val (body_type', top_cbnds', choists', freev') = 
	      rcon (body_type, top_set, hoist_set)
	  val (tvars, kinds) = Listops.unzip tFormals
	  val kinds_freev = 
	      unionList (map (list2set o NilUtil.freeVarInKind) kinds)
	  val freev = 
	      Set.difference
	      (Set.difference
	       (unionList[kinds_freev, freev, freev'],
		list2set tvars),
	       list2set (List.mapPartial (fn vopt => vopt) eNames))

      in
	  (AllArrow_c{openness=openness,
		      effect=effect,
		      isDependent = isDependent,
		      tFormals = tFormals,
		      eFormals = eFormals',
		      fFormals = fFormals,
		      body_type = body_type'},
	   top_cbnds @ top_cbnds',
	   choists @ choists',
	   freev)
      end

    | rcon (c as ExternArrow_c (conlist,con),_, _) = 
      (con, [], [], list2set (NilUtil.freeVarInCon c))

    | rcon (c as Var_c v,_, _) = (c, [], [], Set.singleton v)
      
    | rcon (Let_c(Sequential,bndlist,bodcon),top_set,hoist_set) = 
      let
	  val (top_set', hoist_set', bnd_top_cbnds, bnd_hoists, 
	       bnd_stay_cbnds, stay_freev) = 
	      rconbnds(bndlist, top_set, hoist_set) 

          (* we can hoist anything out of the body that depends on
             anything bound here *)
	  val bound_var_set = list2set (map NilUtil.varBoundByCbnd bndlist)
          val body_hoist_set' = union (bound_var_set, hoist_set)

	  val (bodcon',body_top_cbnds, body_hoists, body_freev) = 
	      rcon (bodcon,top_set',hoist_set') 
	      
	  (* But, bindings with free variables whose bindings
	     are in this let must stay *)
	  val stop_vars = list2set (map NilUtil.varBoundByCbnd bnd_stay_cbnds)
	  val (body_hoists, body_stay_cbnds, body_stay_freev) = 
	      filter_cbnds(body_hoists, stop_vars)
	      
	  val top_cbnds = bnd_top_cbnds @ body_top_cbnds
          val hoists = bnd_hoists @ body_hoists
          val stay_cbnds= bnd_stay_cbnds @ body_stay_cbnds

	  val boundv = list2set (NilUtil.varsBoundByCbnds stay_cbnds)
	  val freev = 
	      Set.difference
	      (unionList [stay_freev, body_stay_freev, body_freev], boundv)

      in  (NilUtil.makeLetC stay_cbnds bodcon', top_cbnds, hoists, freev)
      end

    | rcon (Let_c(Parallel,_,_),_,_) = (print "rcon: Parallel Let_c found";
					raise HoistError)

    | rcon (Crecord_c lclist, top_set, hoist_set) = 
      let
	  fun mapper (l,con) =
	      let
		  val (con', top_cbnds, hoists, freev) = 
		      rcon (con, top_set, hoist_set)
	      in
		  ((l,con'), top_cbnds, hoists, freev)
	      end

	  val (lclist', top_cbnds_list, hoists_list, freev_list) =
	      Listops.unzip4 (map mapper lclist)
      in
	  (Crecord_c lclist', 
	   List.concat top_cbnds_list, 
	   List.concat hoists_list,
	   unionList freev_list)
      end

    | rcon (Proj_c (con,lab), top_set, hoist_set) = 
      let
	  val (con', top_cbnds, hoists, freev) = 
	      rcon (con, top_set, hoist_set)
      in
	  (Proj_c(con',lab), top_cbnds, hoists, freev)
      end

(*XXX Can't float out top_bnds because they are bnds and not cbnds.
     Perhaps rcon should instead return term bindings too? *)
    | rcon (Typeof_c e, top_set, hoist_set) = 
      let
	  val econtext = empty_fnmap (* should be no applications in a typeof *)
	  val (e', top_bnds, hoists,_: hoist_effs, true, freev) = 
	      rexp (e, top_set, hoist_set, econtext)

	  val (hoist_bnds, freev_list) = ListPair.unzip hoists
	  val bnds = top_bnds @ hoist_bnds
	  val e'' = NilUtil.makeLetE Sequential bnds e'
	  val freev = unionList freev_list
      in
	  (Typeof_c e'', [], [], freev)
      end
  
    | rcon (Closure_c(codecon,envcon), top_set, hoist_set) = 
      let
	  val (codecon', top_cbnds1, hoists1, freev1) = 
	      rcon (codecon, top_set, hoist_set)
	  val (envcon', top_cbnds2, hoists2, freev2) = 
	      rcon (envcon, top_set, hoist_set)
      in
	  (Closure_c(codecon',envcon'), 
	   top_cbnds1 @ top_cbnds2,
	   hoists1 @ hoists2,
	   Set.union(freev1, freev2))
      end

    | rcon (App_c(con,conlist), top_set, hoist_set) = 
      let
	  val (con', top_cbnds1, choists1, freev1) = 
	      rcon (con,top_set,hoist_set)
	  val (conlist', top_cbnds2, choists2, freev2) = 
	      rcons(conlist,top_set,hoist_set)
      in
	  (App_c(con',conlist'), 
	   top_cbnds1 @ top_cbnds2, 
	   choists1 @ choists2,
	   Set.union(freev1, freev2))
      end

    | rcon (Typecase_c {arg,arms,default,kind},_, _) = 
      (Util.error "hoist.sml" "rcon of typecase unimplemented")

    | rcon (Annotate_c (annot,con), top_set, hoist_set) = 
      let
	  val (con', top_cbnds, choists, freev) = 
	      rcon (con, top_set, hoist_set)
      in
	  (Annotate_c (annot,con'), top_cbnds, choists, freev)
      end

  and rcon' arg = 
      let val (c', top_cbnds, choists, freev) = rcon arg
	  val top_bnds = map (fn cb => Con_b(Runtime,cb)) top_cbnds
	  val hoists = map (fn (cb,s) => (Con_b(Runtime,cb),s)) choists
      in
	  (c', top_bnds, hoists, freev)
      end

  and rcons (conlist, top_set, hoist_set) =
      let 
	  val (conlist', top_cbnds_list, choists_list, freev_list) = 
	      Listops.unzip4 (map (fn a => rcon (a,top_set,hoist_set)) conlist)
	  val top_cbnds = List.concat top_cbnds_list
	  val choists = List.concat choists_list
	  val freev = unionList freev_list
      in
	  (conlist', top_cbnds, choists, freev)
      end

  and rcons' (conlist, top_set, hoist_set) =
      let 
	  val (conlist', top_bnds_list, hoists_list, freev_list) = 
	      Listops.unzip4 (map (fn a => rcon'(a,top_set,hoist_set)) conlist)
	  val top_bnds = List.concat top_bnds_list
	  val hoists = List.concat hoists_list
	  val freev = unionList freev_list
      in
	  (conlist', top_bnds, hoists, freev)
      end

  and rbnds (bnds,top_set,hoist_set,econtext) =
      let
      fun loop ([],top_set,hoist_set,rev_top_bnds_list,
		rev_hoists_list,rev_stay_bnds, econtext, valuable, freev) = 
	   (top_set, hoist_set,
	    List.concat(rev rev_top_bnds_list), 
	    List.concat(rev rev_hoists_list), 
	    rev rev_stay_bnds,
	    econtext, valuable, freev)
	| loop (bnd::rest, top_set, hoist_set, rev_top_bnds_list,
		rev_hoists_list, rev_stay_bnds, econtext, valuable, freev) = 
	   (case rbnd (bnd,top_set,hoist_set,econtext) of 
		(STAY (bnd',freev'), top_bnds, hoists, econtext', valuable') =>
                    loop (rest, top_set, hoist_set,
			  top_bnds :: rev_top_bnds_list, 
			  hoists :: rev_hoists_list, 
			  bnd' :: rev_stay_bnds,
			  econtext, valuable andalso valuable',
			  Set.union(freev,freev'))
	      | (UP v, top_bnds, hoists, econtext', valuable') => 
		    let
			val hoist_set' = Set.add(hoist_set, v)
		    in
			loop(rest, top_set, hoist_set',
			     top_bnds :: rev_top_bnds_list, 
			     hoists :: rev_hoists_list, 
			     rev_stay_bnds,
			     econtext, valuable andalso valuable',
			     freev)
		    end
	      | (TOP v, top_bnds, hoists, econtext', valuable') => 
		    let
			val hoist_set' = Set.add(hoist_set, v)
			val top_set' = Set.add(top_set, v)
		    in
			loop(rest, top_set', hoist_set',
			     top_bnds :: rev_top_bnds_list, 
			     hoists :: rev_hoists_list, 
			     rev_stay_bnds,
			     econtext, valuable andalso valuable',
			     freev)
		    end)

      val (top_set, hoist_set, top_bnds, hoists, stay_bnds, 
	   econtext, valuable, freev) =
	  loop (bnds, top_set, hoist_set, [], [], [], 
		econtext, true, Set.empty)

      val boundv = list2set (NilUtil.varsBoundByBnds stay_bnds)
      val freev = Set.difference(freev, boundv)

    in
	(top_set, hoist_set, top_bnds, hoists, stay_bnds, 
	 econtext, valuable, freev)
    end

  and rbnd (Con_b(p,cb), top_set, hoist_set, econtext)  =
      let
	  val v = NilUtil.varBoundByCbnd cb
      (*
          val vstr = var2string v 
          val _ = (pprint ("rewriting bnd for con var: "^vstr^"\n");ppin 2) 
          val _ = ppout 2 
       *)
      in
	  case rconbnd' (cb,top_set,hoist_set) of
	      (CUP v, top_bnds, hoists) => 
		  (UP v, top_bnds, hoists, econtext, true)
	    | (CSTAY (cb',freev), top_bnds, hoists) => 
		  (STAY (Con_b (p,cb'), freev), top_bnds, hoists, 
		   econtext, true)
            | (CTOP v, top_bnds, hoists) => 
		  (TOP v, top_bnds, hoists, econtext, true)
      end

    | rbnd (Exp_b(v,niltrace,e), top_vars, hoist_vars, econtext) = 
      let

	  val (e', top_bnds, hoists, effs, valuable, freev) = 
	      rexp(e, top_vars, hoist_vars, econtext)
	  
	  val newbnd = Exp_b(v,niltrace,e')
	  val econtext' = Map.insert(econtext,v,effs)
      in
	  if valuable then
	      if (Set.isSubset(freev, top_vars)) then
		  (TOP v, top_bnds @ [newbnd], hoists, econtext', valuable)
	      else 
		  if (Set.isSubset(freev, hoist_vars)) then
		      (UP v, top_bnds, hoists @ [(newbnd,freev)], 
		       econtext', valuable)
		  else
		      (STAY (newbnd, freev), top_bnds, hoists, econtext', 
		       valuable)
	  else 
	      (STAY (newbnd, freev), top_bnds, hoists, econtext', valuable)
    end

    | rbnd (Fixopen_b(vfs), top_set, hoist_set, econtext_orig) = 
	let
	    fun loop ([],vfl,top_bnds,hoists,econtext,freev) = 
		(vfl,top_bnds,hoists,econtext,freev)
	      | loop ((v,f)::rest,vfl,top_bnds,hoists,econtext,freev) = 
		let
		    val (f',top_bnds',hoists',effs, freev') = 
			rfun (f,top_set,hoist_set,econtext_orig)
		    val econtext' = Map.insert(econtext, v, effs)
		in
		    loop (rest, (v,f')::vfl, top_bnds @ top_bnds',
			  hoists @ hoists', econtext',
			  Set.union(freev, freev'))
		end
	    
	    val (vfs', top_bnds, hoists, econtext', freev) = 
		loop (Sequence.toList vfs,[],[],[],econtext_orig, Set.empty)
	in
	    (STAY(Fixopen_b(Sequence.fromList vfs'), freev),
	     top_bnds, hoists, econtext',true)
	end	

    | rbnd (Fixcode_b(vfs), top_set, hoist_set, econtext_orig) = 
	let
	    val (STAY(Fixopen_b vf_seq, freev), 
		 top_bnds, hoists, econtext', true) =
		rbnd(Fixopen_b vfs, top_set, hoist_set, econtext_orig)
	in
	    (STAY(Fixcode_b vf_seq, freev),
	        top_bnds, hoists, econtext', true)
	end
    
    | rbnd _ = raise HoistError

  and rexp (Var_e v,_,_,econtext) = 
         (Var_e v, [], [], elookup(econtext,v), true, Set.singleton v)

    | rexp (Const_e c,_,_,econtext) = 
	 (Const_e c, [], [], UNKNOWN_EFFS, true, Set.empty)

    | rexp (Let_e (seq, bndlst, bodexp), top_set, hoist_set, econtext) = 
      let

	  val (top_set', hoist_set',
	       bnd_top_bnds, bnd_hoists, bnd_stay_bnds,
	       econtext', bnd_valuable, stay_vfree) = 
	      rbnds(bndlst,top_set,hoist_set,econtext)

	  val bound_var_set = list2set (NilUtil.varsBoundByBnds bndlst)
	  val body_hoist_set = union (bound_var_set,hoist_set)
	  val (bodexp',body_top_bnds, body_hoists, effs, 
	       body_valuable, body_vfree) = 
	      rexp(bodexp,top_set',body_hoist_set,econtext')

	  val stop_vars = list2set (NilUtil.varsBoundByBnds bnd_stay_bnds)
	  val (body_hoists, body_stay_bnds, body_stay_vfree) = 
	      filter_bnds(body_hoists, stop_vars)

	  val top_bnds = bnd_top_bnds @ body_top_bnds
	  val hoists = bnd_hoists @ body_hoists
	  val stay_bnds = bnd_stay_bnds @ body_stay_bnds

	  val vbound = Set.addList(stop_vars, NilUtil.varsBoundByBnds body_stay_bnds)
	      
	  val vfree = 
	      Set.difference
	       (unionList [stay_vfree, body_stay_vfree, body_vfree],
		vbound)
      in
	  (NilUtil.makeLetE seq stay_bnds bodexp',
	   top_bnds, hoists, effs, bnd_valuable andalso body_valuable,
	   vfree)
      end

    | rexp (exp as Prim_e (allp,conlst,explst), top_set, hoist_set, econtext) =
      let
	  val (conlst', con_top_bnds, con_hoists, con_vfree) = 
	      rcons' (conlst, top_set, hoist_set)

	  val (explst', exp_top_bnds, exp_hoists, effs_list, 
	       valuable_list, exps_vfree) =
	      rexps (explst, top_set, hoist_set, econtext)
          
	  (* we don't try very hard to track effect types through primops.
	   *)
	  val effs =
	      (case (allp, effs_list) of
		   (NilPrimOp (record lbls),  _) =>
		       REC_EFFS(ListPair.zip (lbls, effs_list))
                 | (NilPrimOp (select lbl), [effs]) =>
		       ereclookup (effs, lbl)
		 | _ => UNKNOWN_EFFS)
	      
	  val valuable = (not (NilUtil.effect exp)) andalso
	      List.all (fn (b:bool)=>b) valuable_list

	  val vfree = Set.union(con_vfree, exps_vfree)

      in
	  (Prim_e (allp, conlst', explst'), 
	   con_top_bnds @ exp_top_bnds,
	   con_hoists @ exp_hoists,
	   effs, valuable, vfree)
      end

    | rexp (Switch_e s, top_set, hoist_set, econtext) = 
      let
	  val (ns, top_bnds, hoists, effs, valuable, vfree) = 
	      rswitch (s, top_set, hoist_set, econtext)
      in
	  (Switch_e ns, top_bnds, hoists, effs, valuable, vfree)
      end

    | rexp (exp0 as App_e (opn, exp, conlist, explist1, explist2), 
	    top_set, hoist_set, econtext) = 
      let
	  val (exp', exp_top_bnds, exp_hoists, exp_effs, 
	       exp_valuable, exp_vfree) = 
	      rexp (exp, top_set, hoist_set, econtext)

	  val (conlist', con_top_bnds, con_hoists, con_vfree) = 
	      rcons' (conlist, top_set, hoist_set)
	  val (explist1', top_bnds1, hoists1, effs1, valuable1, vfree1) =
	      rexps (explist1, top_set, hoist_set, econtext)
	  val (explist2', top_bnds2, hoists2, effs2, valuable2, vfree2) =
	      rexps (explist2, top_set, hoist_set, econtext)

	  val top_bnds = exp_top_bnds @ con_top_bnds @ top_bnds1 @ top_bnds2
          val hoists = exp_hoists @ con_hoists @ hoists1 @ hoists2
	  val vfree = unionList [exp_vfree, con_vfree, vfree1, vfree2]

	  fun id (b:bool) = b
	  val components_valuable = 
	      exp_valuable andalso
	      (List.all id valuable1) andalso (List.all id valuable2)
	      
	  val (valuable, effs) = 
	      (case exp_effs of
		   (ARROW_EFFS (Total,rest)) => (components_valuable, rest)
		 | (ARROW_EFFS (Partial,rest)) => (false, rest)
		 | _ => (false, unknown_effs))

      in
	  (App_e (opn, exp', conlist', explist1', explist2'), 
	   top_bnds, hoists, effs, valuable, vfree)
      end
  
    | rexp (ExternApp_e (exp,explist), top_set, hoist_set, econtext) = 
      let
	  val (exp', exp_top_bnds, exp_hoists, _, _, exp_vfree) = 
	      rexp (exp, top_set, hoist_set, econtext)
	  val (explist',exps_top_bnds, exps_hoists, _, _, exps_vfree) = 
	      rexps(explist, top_set, hoist_set, econtext)

	  val top_bnds = exp_top_bnds @ exps_top_bnds
          val hoists = exp_hoists @ exps_hoists
	  val valuable = false
	  val effs = unknown_effs
	  val vfree = Set.union(exp_vfree, exps_vfree)
      in
	(ExternApp_e (exp',explist'), top_bnds, hoists, effs, valuable, vfree)
      end

    | rexp (Raise_e(exp,con), top_set, hoist_set, econtext) = 
      let
	  val (exp',top_bnds1, hoists1,_,_, vfree1) = 
	      rexp (exp, top_set, hoist_set, econtext)
	  val (con',top_bnds2, hoists2, vfree2) = 
	      rcon' (con, top_set, hoist_set) 
	  val top_bnds = top_bnds1 @ top_bnds2
	  val hoists = hoists1 @ hoists2
	  val effs = unknown_effs
	  val valuable = false
	  val vfree = Set.union(vfree1, vfree2)
      in
	  (Raise_e(exp',con'), top_bnds, hoists, effs, valuable, vfree)
      end
  
    | rexp (Handle_e {body,bound,handler,result_type},
	    top_set, hoist_set, econtext) = 
      let
	  val (body',top_bnds1,hoists1,_,_, vfree1) = 
	      rexp(body, top_set, hoist_set, econtext)
	  val (handler',top_bnds2,hoists2,_,_, vfree2) = 
	      rexp(handler, top_set, hoist_set, econtext)
	  val (result_type', top_bnds3, hoists3, vfree3) = 
	      rcon'(result_type, top_set, hoist_set)
	  val top_bnds = top_bnds1 @ top_bnds2 @ top_bnds3
	  val hoists = hoists1 @ hoists2 @ hoists3
	  val effs = unknown_effs
	  val valuable = false	
	  val vfree = Set.union(Set.union(vfree1, vfree3),
				Set.difference(vfree2, Set.singleton bound))
      in
	  (Handle_e {body=body', bound = bound,
		     handler=handler', result_type= result_type'},
	   top_bnds, hoists, effs, valuable, vfree)
      end

  and rexps (explst, top_set, hoist_set, econtext) = 
      let
	  fun unzip6 abcdef_list = 
	      let fun unzip6_loop [] (aa,bb,cc,dd,ee,ff) = 
		  (rev aa, rev bb, rev cc, rev dd, rev ee, rev ff)
		    | unzip6_loop ((a,b,c,d,e,f)::rest) (aa,bb,cc,dd,ee,ff) = 
		  unzip6_loop rest (a::aa,b::bb,c::cc,d::dd,e::ee,f::ff)
	      in unzip6_loop abcdef_list ([],[],[],[],[],[])
	      end
	  
	  val (explst', top_bnd_list, hoists_list, effs_list, 
	       valuable_list, vfree_list) = 
	      unzip6 
	        (map (fn e => rexp(e, top_set, hoist_set, econtext))
		 explst)
      in
	  (explst',
	   List.concat top_bnd_list,
	   List.concat hoists_list,
	   effs_list,
	   valuable_list,
	   unionList vfree_list)
      end

  and rswitch (Intsw_e{arg,size,arms,default,result_type},
	       top_set, hoist_set, econtext) =
      let
	  val (arg', arg_top_bnds, arg_hoists, _, _, arg_vfree) = 
	      rexp (arg, top_set, hoist_set,econtext)

	  fun mapper (w,e) = 
	      let 
		  val (e',top_bnds,hoists,_,_, vfree) = 
		      rexp (e,top_set,hoist_set,econtext)
	      in
		  if (!hoist_through_switch) then
		      ((w,e'), top_bnds, hoists, vfree)
		  else
		      let 
			  val (hoists, stay_bnds,vfree') = filter_ebnds hoists
		      in
			  ((w, NilUtil.makeLetE Sequential stay_bnds e'), 
			   top_bnds, hoists, Set.union(vfree,vfree'))
		      end
	      end
	      
	  val (arms',arms_top_bnds_list, arms_hoists_list, vfree_list) = 
	      Listops.unzip4 (map mapper arms)
	  val arms_top_bnds = List.concat arms_top_bnds_list
	  val arms_hoists = List.concat arms_hoists_list

	  val (default',default_top_bnds,default_hoists,_,_, default_vfree) = 
	      rexpopt(default, top_set,
		      (*cheat*)
		      if (!hoist_through_switch) then hoist_set else top_set,
		      econtext)


	  val effs = con2eff result_type
	  val valuable = false
	  val vfree = unionList (arg_vfree :: default_vfree :: vfree_list)
      in
	  (Intsw_e {arg=arg', size=size, arms=arms', default=default',
		    result_type = result_type},
	   arg_top_bnds @ arms_top_bnds @ default_top_bnds,
	   arg_hoists @ arms_hoists @ default_hoists,
	   effs, valuable, vfree)
      end

    | rswitch (Sumsw_e {arg,sumtype,bound,arms,default,result_type},
	       top_set, hoist_set, econtext) = 
      let

	  val (arg', arg_top_bnds, arg_hoists, _, _, arg_vfree) = 
	      rexp (arg, top_set, hoist_set, econtext)

	  val boundset = Set.singleton bound
	  val arm_hoist_set = Set.add(hoist_set,bound)
	  fun mapper (w,tr,e) = 
	      let 
		  val (e',top_bnds,hoists,_,_, vfree) = 
		      rexp (e, top_set, arm_hoist_set, econtext)
	      in
		  if (!hoist_through_switch) then
		      let
			  val (up,stay,vfree') = filter_bnds (hoists, boundset)
			  val e'' = NilUtil.makeLetE Sequential stay e'
		      in
			  ((w,tr,e''), top_bnds, up, Set.union(vfree,vfree'))
		      end
		  else
		      let
			  val (hoists, stay_bnds2, vfree2) = 
			      filter_ebnds hoists
			  val (hoists, stay_bnds1, vfree1) = 
			      filter_bnds (hoists, boundset) 
			  val stay_bnds = stay_bnds1 @ stay_bnds2
		      in
			  ((w, tr, NilUtil.makeLetE Sequential stay_bnds e'), 
			   top_bnds, hoists,
			   unionList [vfree, vfree1, vfree2])
		      end
	      end
	      
	  val (arms',arms_top_bnds_list, arms_hoists_list, arms_vfree_list) = 
	      Listops.unzip4 (map mapper arms)
	  val arms_top_bnds = List.concat arms_top_bnds_list
	  val arms_hoists = List.concat arms_hoists_list

	  val (default',default_top_bnds,default_hoists,_,_,default_vfree) = 
	      rexpopt(default, top_set,
		      (*cheat*)
		      if (!hoist_through_switch) then hoist_set else top_set,
		      econtext)
	      
	  val effs = con2eff result_type
	  val valuable = false
	  val vfree = 
	      Set.difference
	      (unionList (arg_vfree :: default_vfree :: arms_vfree_list),
	       boundset)
      in
	  (Sumsw_e {arg = arg',
		    sumtype = sumtype,
		    bound = bound,
		    arms = arms',
		    default = default',
		    result_type = result_type},
	   arg_top_bnds @ arms_top_bnds @ default_top_bnds,
	   arg_hoists @ arms_hoists @ default_hoists,
	   effs, valuable, vfree)
      end

    | rswitch (Exncase_e {arg,bound,arms,default,result_type},
	       top_set, hoist_set, econtext) = 
      let
	  val (arg', arg_top_bnds, arg_hoists, _, _, arg_vfree) = 
	      rexp (arg, top_set, hoist_set, econtext)

	  val boundset = Set.singleton bound
	  val arm_hoist_set = add(hoist_set,bound)

	  (* e1 is supposed to be a path, so no point traversing it *)
	  fun mapper (e1,tr,e) = 
	      let 
		  val (e',top_bnds,hoists,_,_,vfree) = 
		      rexp (e, top_set, arm_hoist_set, econtext)
	      in
		 if (!hoist_through_switch) then
		      let  
			  val (up,stay,vfree') = filter_bnds (hoists, boundset)
			  val e'' = NilUtil.makeLetE Sequential stay e'
		      in
			  ((e1,tr,e''), top_bnds, up, Set.union(vfree,vfree'))
		      end
		 else
		      let
			  val (hoists, stay_bnds2, vfree2) = 
			      filter_ebnds hoists
			  val (hoists, stay_bnds1, vfree1) = 
			      filter_bnds (hoists, boundset) 
			  val stay_bnds = stay_bnds1 @ stay_bnds2
		      in
			  ((e1, tr, NilUtil.makeLetE Sequential stay_bnds e'), 
			   top_bnds, hoists,
			   unionList [vfree, vfree1, vfree2])
		      end
	      end	
      
	  val (arms',arms_top_bnds_list, arms_hoists_list, arms_vfree_list) = 
	      Listops.unzip4 (map mapper arms)
	  val arms_top_bnds = List.concat arms_top_bnds_list
	  val arms_hoists = List.concat arms_hoists_list

	  val (default',default_top_bnds,default_hoists,_,_, default_vfree) = 
	      rexpopt(default, top_set,
		      (*cheat*)
		      if (!hoist_through_switch) then hoist_set else top_set,
		      econtext)
	      
	  val effs = con2eff result_type
	  val valuable = false
	  val vfree = 
	      Set.difference
	      (unionList (arg_vfree :: default_vfree :: arms_vfree_list),
	       boundset)
      in
	  (Exncase_e {arg=arg',
		      bound=bound,
		      arms=arms',
		      default=default',
		      result_type = result_type},
	   arg_top_bnds @ arms_top_bnds @ default_top_bnds,
	   arg_hoists @ arms_hoists @ default_hoists,
	   effs, valuable, vfree)
    end

    | rswitch (Typecase_e {arg,arms,default,result_type},_,_,econtext) = 
    (Util.error "hoist.sml" "Typecase_e not implemented yet")

  and rexpopt(NONE,top_set, hoist_set,econtext) = 
      (NONE,[],[],unknown_effs,true, Set.empty)
    | rexpopt(SOME(e), top_set, hoist_set, econtext) = 
      let 
	  val (e', top_bnds, hoists, effs, valuable, vfree) = 
	      rexp (e, top_set, hoist_set, econtext) 
      in 
	  (SOME(e'), top_bnds, hoists, effs, valuable, vfree) 
      end

  and rfun (Function{effect=eff,recursive=isrec,isDependent=isdep,
		     tFormals=tFormals, eFormals=eformals,
		     fFormals=fformals, body=bod, body_type=ret},
	    top_set, hoist_set, econtext) = 
      let

	  val econtext' = 
	      List.foldr (fn ((v,_,c),ectx) => Map.insert(ectx, v, con2eff c))
              econtext eformals
	      
	  val (cvars, kinds) = Listops.unzip tFormals
	  val kinds_freev = 
	      unionList (map (list2set o NilUtil.freeVarInKind) kinds)

	  val (evars, nts, etypes) = Listops.unzip3 eformals
	  val (etypes', arg_top_bnds, arg_hoists, cons_freev) = 
	         rcons' (etypes, top_set, hoist_set)
	  val eformals' = Listops.zip3 evars nts etypes'
          val boundvar_set = list2set (cvars @ evars @ fformals)
	  val body_hoist_set = Set.union (hoist_set, boundvar_set)

	  val (bod',body_top_bnds, body_hoists,effs,_, body_freev) = 
	      rexp(bod,top_set, body_hoist_set, econtext)
          val (bod_up, bod_stay, bod_stay_freev) = 
	      filter_bnds (body_hoists, boundvar_set)
	  val newbod = NilUtil.makeLetE Sequential bod_stay bod'

	  val (ret',retcon_top_cbnds, retcon_choists, retcon_freev) = 
	      rcon (ret,top_set,body_hoist_set)
          val (ret_up, ret_stay, ret_stay_freev) = 
	      filter_cbnds (retcon_choists, boundvar_set)
          val newret = NilUtil.makeLetC ret_stay ret'

          val hoists = arg_hoists @ 
	                (map (fn (cb,s) => (Con_b(Runtime,cb), s)) ret_up) @ 
			bod_up

	  val top_bnds = arg_top_bnds @ 
	      (map (fn cb => Con_b(Runtime,cb)) retcon_top_cbnds) @ 
	      body_top_bnds

	  val freev = 
	      Set.difference
	      (Set.difference
	       (unionList[kinds_freev, cons_freev, body_freev, 
			  retcon_freev, ret_stay_freev],
		list2set cvars),
	       list2set evars)

      in
	  (Function{effect=eff,recursive=isrec,isDependent=isdep,
		    tFormals=tFormals,eFormals=eformals', fFormals=fformals,
		    body=newbod,body_type=newret},
	   top_bnds, hoists, ARROW_EFFS(eff, effs), freev) 
      end
  
  fun optimize (MODULE {bnds, imports, exports}) = 
      let
	  (* collect the 'top level' var names *)
	  fun split ([],cvs,ectx) = (cvs,ectx)
	    | split (ImportValue(l,v,_,c)::rest,cvs,ectx) = 
	      split(rest,Set.add(cvs,v),Map.insert(ectx,v,con2eff c))
	    | split (ImportType(l,v,k)::rest,cvs,ectx) = 
		    split(rest,add(cvs,v), ectx)
		    
	  val (top_set, econtext) = split (imports, Set.empty, empty_fnmap)
	  val hoist_set = top_set

	  val (_,_,top_bnds,hoists,stay_bnds,_, _, _) = 
	      rbnds(bnds, top_set, hoist_set, econtext)
		    
	  val bnds' = top_bnds @ (map #1 hoists) @ stay_bnds
		    
      in
	  MODULE {bnds=bnds',imports=imports,exports=exports}  
      end

end
