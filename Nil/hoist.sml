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

  datatype bndtype = STAY of bnd | UP of var | TOP of var
  datatype conbndtype = CSTAY of conbnd | CUP of var | CTOP of var

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
	not (exists (fn v => member(s1,v)) s2)
  end
  (* end set help funs *)


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

  (* We can't hoist a kind past a binding of its free variable.
     (although technically this binding can always be expanded) *)
  val look_in_kinds = true

  (* returns set of free vars *)
  fun freeExpVars e = 
      let 
	  val (free_term_vars, free_type_vars) = 
	      NilUtil.freeExpConVarInExp (look_in_kinds,e)
      in 
	  Set.addList (list2set free_term_vars, free_type_vars)
      end

  (* returns set of free type vars *)
  fun freeConVars c = 
      list2set (NilUtil.freeConVarInCon (look_in_kinds,c))

  (* returns set of free vars *)
  fun freeKindVars k = 
      list2set (NilUtil.freeVarInKind k)

  fun freeConbndVars cb =
      list2set (NilUtil.freeVarInCbnd cb)

    
  (* get the bound var from a conbnd *)
  fun get_conbnd_var (Con_cb (v,c)) = v
    | get_conbnd_var (Open_cb (v,_,_)) = v
    | get_conbnd_var (Code_cb (v,_,_)) = v

  (* get the bound vars from a list of bnds *)
  fun getBoundVars bnd_list = 
    let
      fun gv ([],l) = rev l
	| gv (Con_b (p,cb)::rest,l) = gv(rest,(get_conbnd_var cb)::l)
	| gv (Exp_b (v,_,e)::rest,l) = gv(rest,v::l)
	| gv (Fixopen_b(vfs)::rest,l) = gv(rest,(map #1 (Sequence.toList vfs))@l)
	| gv (Fixcode_b(vfs)::rest,l) = gv(rest,(map #1 (Sequence.toList vfs))@l)
	| gv (Fixclosure_b(_)::rest,l) = raise HoistError
    in
      gv (bnd_list,[])
    end

  (* stop junk *)

  fun filter_bnds (bvl, stop_vars) =
      let
	  fun loop([],stop_vars,up,stay) = (rev up, rev stay)
	    | loop((bv as (eb,freevars))::rest, stop_vars, up, stay) =
	      if (disjoint(freevars,stop_vars)) then
		  loop(rest, stop_vars, bv :: up, stay)
	      else
		  loop(rest, Set.addList(stop_vars,getBoundVars [eb]),
		       up, eb :: stay)
      in
	  loop(bvl, stop_vars, nil, nil)
      end


  fun filter_cbnds (hoists, stop_vars) =
      let
	  fun loop([], _, up, stay) = (rev up, rev stay)
	    | loop((bv as (cb,freevars))::rest, stop_vars, up, stay) =
	      if (disjoint(freevars,stop_vars)) then
		  loop(rest, stop_vars, bv :: up, stay)
	      else
		  loop(rest, Set.add(stop_vars,get_conbnd_var cb),
		       up, cb :: stay)
      in
	  loop(hoists, stop_vars, nil, nil)
      end



  fun filter_ebnds hoists =
      let
	  fun loop([],stop_vars,up,stay) = (rev up, rev stay)
            | loop((bv as (eb as Con_b(_,cb),freevars))::rest, stop_vars, up, stay) =
	         if (disjoint(freevars,stop_vars)) then
		     loop(rest, stop_vars, bv :: up, stay)
		 else
		     loop(rest, Set.add(stop_vars,get_conbnd_var cb),
			  up, eb :: stay)
 	    | loop((bv as (eb,freevars))::rest, stop_vars, up, stay) =
		  loop(rest, Set.addList(stop_vars,getBoundVars [eb]),
		       up, eb :: stay)
      in
	  loop(hoists, Set.empty, [], [])
      end

  (* rconbnd : conbndtype * conbnd list * (conbnd * set) list 
        returns: 1) whether or not the binding is being hoisted
                    (and if not, a rewritten variant of the binding)
                 2) 
   *)

  fun rconbnd (Con_cb(v,con), top_set, hoist_set) =
      let
	  val (con' : con, top_cbnds : conbnd list, choists : (conbnd*set) list) = 
	      rcon (con, top_set, hoist_set)
	  val freev = freeConVars con'
      in
	  if (subset(freev, top_set)) then
	      (CTOP v, top_cbnds @ [Con_cb(v,con')], choists)
	  else if (subset(freev, hoist_set)) then
	      (CUP v, top_cbnds, choists @ [(Con_cb(v,con'),freev)])
	  else
	      (CSTAY (Con_cb (v,con')), top_cbnds, choists)
    end

    | rconbnd (Open_cb(v,vklist,con_body), top_set, hoist_set) = 
        (* We don't hoist functions *)
        (*** if this is changed (ie to hoist open_cbs) then code_cb wont work *)
	let
	  val (con_body', top_cbnds, choists) = rcon (con_body,top_set,hoist_set)
	in
	  (CSTAY(Open_cb(v,vklist,con_body')), top_cbnds, choists)
	end

    | rconbnd ((Code_cb stuff), top_set, hoist_set) = 
	let
	  val (CSTAY(Open_cb stuff'), top_cbnds, choists) = 
	      rconbnd (Open_cb stuff, top_set, hoist_set)
	in
	  (CSTAY(Code_cb stuff'), top_cbnds, choists)
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
      fun loop ([],top_set,hoist_set,top,up,stay) = 
	   (top_set, hoist_set,
	    List.concat(rev top), List.concat(rev up), rev stay)
	| loop (cb::rest,top_set,hoist_set,top,up,stay) = 
	   (case rconbnd (cb,top_set,hoist_set) of 
		(CSTAY cb',top_cbnds, choists) => 
                    loop (rest, top_set, hoist_set,
			  top_cbnds :: top, choists :: up, cb'::stay)
	      | (CUP v, top_cbnds, choists) => 
		    let
			val hoist_set' = Set.add(hoist_set, v)
		    in
			loop(rest, top_set, hoist_set',
			     top_cbnds :: top, choists :: up, stay)
		    end
	      | (CTOP v, top_cbnds, choists) => 
		    let
			val hoist_set' = Set.add(hoist_set, v)
			val top_set' = Set.add(top_set, v)
		    in
			loop(rest, top_set', hoist_set',
			     top_cbnds :: top, choists :: up, stay)
		    end)
    in
	loop (conbnds, top_set, hoist_set, [], [], [])
    end

  and rcon (Prim_c (primcon,conlist), top_set, hoist_set) =
      let
	  val (conlist', top_cbnds, choists) = rcons (conlist, top_set, hoist_set)
      in
	  (Prim_c(primcon,conlist'), top_cbnds, choists)
      end

    | rcon (Mu_c (isRecursive,vcseq), top_set, hoist_set) = 
      let
	  val vclist = Sequence.toList vcseq
	  fun mapper (v,c) = 
	      let
		  val (c', top_cbnds, hoists) = rcon(c, top_set, hoist_set)
	      in
		  ((v,c'), top_cbnds, hoists)
	      end
	  val (vclist'',top_cbnds_list, hoists_list) = 
	      Listops.unzip3 (map mapper vclist)
	  val top_cbnds = List.concat top_cbnds_list
	  val hoists = List.concat hoists_list
      in
	  (Mu_c (isRecursive,Sequence.fromList vclist''),
	   top_cbnds, hoists)
      end

    | rcon (con as AllArrow_c {openness, effect, isDependent, tFormals,
			       eFormals, fFormals, body_type}, top_set, hoist_set) =
      let
	  val (eNames,eTypes) = ListPair.unzip eFormals
	  val (eTypes', top_cbnds, choists) = rcons(eTypes, top_set, hoist_set)
	  val eFormals' = ListPair.zip (eNames,eTypes')

	  val (body_type', top_cbnds', choists') = rcon (body_type, top_set, hoist_set)
      in
	  (AllArrow_c{openness=openness,
		      effect=effect,
		      isDependent = isDependent,
		      tFormals = tFormals,
		      eFormals = eFormals',
		      fFormals = fFormals,
		      body_type = body_type'},
	   top_cbnds @ top_cbnds',
	   choists @ choists')
      end

    | rcon (ExternArrow_c (conlist,con),_, _) = (con, [], [])
(*
      let
	  val (conlist',bvll) = 
	      Listops.unzip (map (fn c => rcon (c,cvs)) conlist)
	  val (con',bvl') = rcon (con,cvs)
      in
	  (ExternArrow_c(conlist',con'),
	   (List.concat bvll) @ bvl')
      end
*)
    | rcon (c as Var_c(var'),_, _) = (c, [], [])
      
    | rcon (Let_c(Sequential,bndlist,bodcon),top_set,hoist_set) = 
      let
	  val (top_set', hoist_set', bnd_top_cbnds, bnd_hoists, bnd_stay_cbnds) = 
	      rconbnds(bndlist, top_set, hoist_set) 

          (* we can hoist anything out of the body that depends on
             anything bound here *)
	  val bound_var_set = list2set (map get_conbnd_var bndlist)
          val body_hoist_set' = union (bound_var_set, hoist_set)

	  val (bodcon',body_top_cbnds, body_hoists) = 
	      rcon (bodcon,top_set',hoist_set') 
	      
	  (* But, bindings with free variables whose bindings
	     are in this let must stay *)
	  val stop_vars = list2set (map get_conbnd_var bnd_stay_cbnds)
	  val (body_hoists, body_stay_cbnds) = 
	      filter_cbnds(body_hoists, stop_vars)
	      
	  val top_cbnds = bnd_top_cbnds @ body_top_cbnds
          val hoists = bnd_hoists @ body_hoists
          val stay_cbnds= bnd_stay_cbnds @ body_stay_cbnds

      in  (NilUtil.makeLetC stay_cbnds bodcon', top_cbnds, hoists)
      end

    | rcon (Let_c(Parallel,_,_),_,_) = (print "rcon: Parallel Let_c found";
					raise HoistError)

    | rcon (Crecord_c lclist, top_set, hoist_set) = 
      let
	  fun mapper (l,con) =
	      let
		  val (con', top_cbnds, hoists) = rcon (con, top_set, hoist_set)
	      in
		  ((l,con'), top_cbnds, hoists)
	      end

	  val (lclist', top_cbnds_list, hoists_list) =
	      Listops.unzip3 (map mapper lclist)
      in
	  (Crecord_c lclist', 
	   List.concat top_cbnds_list, 
	   List.concat hoists_list)
      end

    | rcon (Proj_c (con,lab), top_set, hoist_set) = 
      let
	  val (con', top_cbnds, hoists) = rcon (con, top_set, hoist_set)
      in
	  (Proj_c(con',lab), top_cbnds, hoists)
      end

    | rcon (Typeof_c e, top_set, hoist_set) = 
      let
	  val econtext = empty_fnmap (* should be no applications in a typeof *)
	  val (e',[],[],_: hoist_effs,true) = 
	      rexp (e, top_set, hoist_set, econtext)
      in
	  (Typeof_c e', [], [])
      end
  
    | rcon (Closure_c(codecon,envcon), top_set, hoist_set) = 
      let
	  val (codecon', top_cbnds1, hoists1) = rcon (codecon, top_set, hoist_set)
	  val (envcon', top_cbnds2, hoists2) = rcon (envcon, top_set, hoist_set)
      in
	  (Closure_c(codecon',envcon'), 
	   top_cbnds1 @ top_cbnds2,
	   hoists1 @ hoists2)
      end

    | rcon (App_c(con,conlist), top_set, hoist_set) = 
      let
	  val (con', top_cbnds, choists) = rcon (con,top_set,hoist_set)
	  val (conlist', top_cbnds, choists) = rcons(conlist,top_set,hoist_set)
      in
	  (App_c(con',conlist'), top_cbnds, choists)
      end

    (**** clean me till i shine *)
    | rcon (Typecase_c {arg,arms,default,kind},_, _) = 
      (Util.error "hoist.sml" "rcon of typecase unimplemented")
(*
    let
      fun proc (primc,vkl,con) =
	let
	  val (con',bvl') = rcon (con,cvs)
	in
	  ((primc,vkl,con'),bvl')
	end
      
      val (arg',bvl:bvs list) = rcon (arg,cvs)
      val (default',bvl':bvs list) = rcon (default,cvs)	
      val (arms',bvl'') = Listops.unzip (map proc arms)
      val bvl'' = List.concat bvl''
 
     in
      (Typecase_c {arg=arg',arms=arms',default=default',kind=kind},(bvl@bvl'@bvl''))
    end
*)
    | rcon (Annotate_c (annot,con), top_set, hoist_set) = 
      let
	  val (con', top_cbnds, choists) = rcon (con, top_set, hoist_set)
      in
	  (Annotate_c (annot,con'), top_cbnds, choists)
      end

  and rcon' arg = 
      let val (c', top_cbnds, choists) = rcon arg
	  val top_bnds = map (fn cb => Con_b(Runtime,cb)) top_cbnds
	  val hoists = map (fn (cb,s) => (Con_b(Runtime,cb),s)) choists
      in
	  (c', top_bnds, hoists)
      end

  and rcons (conlist, top_set, hoist_set) =
      let 
	  val (conlist', top_cbnds_list, choists_list) = 
	      Listops.unzip3 (map (fn a => rcon (a,top_set,hoist_set)) conlist)
	  val top_cbnds = List.concat top_cbnds_list
	  val choists = List.concat choists_list
      in
	  (conlist', top_cbnds, choists)
      end

  and rcons' (conlist, top_set, hoist_set) =
      let 
	  val (conlist', top_bnds_list, hoists_list) = 
	      Listops.unzip3 (map (fn a => rcon' (a,top_set,hoist_set)) conlist)
	  val top_bnds = List.concat top_bnds_list
	  val hoists = List.concat hoists_list
      in
	  (conlist', top_bnds, hoists)
      end

  and rbnds (bnds,top_set,hoist_set,econtext) =
      let
      fun loop ([],top_set,hoist_set,rev_top_bnds_list,
		rev_hoists_list,rev_stay_bnds, econtext, valuable) = 
	   (top_set, hoist_set,
	    List.concat(rev rev_top_bnds_list), 
	    List.concat(rev rev_hoists_list), 
	    rev rev_stay_bnds,
	    econtext, valuable)
	| loop (bnd::rest, top_set, hoist_set, rev_top_bnds_list,
		rev_hoists_list, rev_stay_bnds, econtext, valuable) = 
	   (case rbnd (bnd,top_set,hoist_set,econtext) of 
		(STAY bnd', top_bnds, hoists, econtext', valuable') => 
                    loop (rest, top_set, hoist_set,
			  top_bnds :: rev_top_bnds_list, 
			  hoists :: rev_hoists_list, 
			  bnd' :: rev_stay_bnds,
			  econtext, valuable andalso valuable')
	      | (UP v, top_bnds, hoists, econtext', valuable') => 
		    let
			val hoist_set' = Set.add(hoist_set, v)
		    in
			loop(rest, top_set, hoist_set',
			     top_bnds :: rev_top_bnds_list, 
			     hoists :: rev_hoists_list, 
			     rev_stay_bnds,
			     econtext, valuable andalso valuable')
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
			     econtext, valuable andalso valuable')
		    end)
    in
	loop (bnds, top_set, hoist_set, [], [], [], econtext, true)
    end

  and rbnd (Con_b(p,cb), top_set, hoist_set, econtext)  =
      let
	  val v = get_conbnd_var cb
      (*
          val vstr = var2string v 
          val _ = (pprint ("rewriting bnd for con var: "^vstr^"\n");ppin 2) 
          val _ = ppout 2 
       *)
      in
	  case rconbnd' (cb,top_set,hoist_set) of
	      (CUP v, top_bnds, hoists) => 
		  (UP v, top_bnds, hoists, econtext, true)
	    | (CSTAY cb', top_bnds, hoists) => 
		  (STAY (Con_b (p,cb')), top_bnds, hoists, econtext, true)
            | (CTOP v, top_bnds, hoists) => 
		  (TOP v, top_bnds, hoists, econtext, true)
      end

    | rbnd (Exp_b(v,niltrace,e), top_vars, hoist_vars, econtext) = 
      let
(*
         val vstr = var2string v
         val _ = (pprint ("rewriting bnd for exp var: "^vstr^"\n"); ppin 2)  
*)
	  val (e', top_bnds, hoists, effs, valuable) = 
	      rexp(e, top_vars, hoist_vars, econtext)
	  
	  val free_vars = freeExpVars e'
	      
	  (*
	      val _ = print ("Free vars for " ^ vstr ^ " are [ ")
	      val _ = print " are [ "
	      val _ = Set.app Ppnil.pp_var free_vars
	      val _ = print "]\n"
	      val _ = Ppnil.pp_exp e'
	      val _ = print "***\n"
	   *)
	
(*
          val _ = if hoistable then pprint ("E-moving "^vstr^" up\n") 
        	      else pprint ("E-keeping "^vstr^" here\n") 
          val _ = ppout 2 
*)

	  val newbnd = Exp_b(v,niltrace,e')
	  val econtext' = Map.insert(econtext,v,effs)
      in
	  if valuable then
	      if (Set.isSubset(free_vars, top_vars)) then
		  (TOP v, top_bnds @ [newbnd], hoists, econtext', valuable)
	      else 
		  if (Set.isSubset(free_vars, hoist_vars)) then
		      (UP v, top_bnds, hoists @ [(newbnd,free_vars)], 
		       econtext', valuable)
		  else
		      (STAY newbnd, top_bnds, hoists, econtext', valuable)
	  else 
	      (STAY newbnd, top_bnds, hoists, econtext', valuable)
    end

    | rbnd (Fixcode_b(vfs), top_set, hoist_set, econtext_orig) = 
	let
(*
	  val fvstr = (sl2s (map var2string (map #1 vfs)))
	  val _ = (pprint ("rewriting bnd for code funs: "^fvstr^"\n");ppin 2) 
*)
	  fun loop ([],vfl,top_bnds,hoists,econtext) = 
	         (vfl,top_bnds,hoists,econtext)
	    | loop ((v,f)::rest,vfl,top_bnds,hoists,econtext) = 
	         let
		     val (f',top_bnds',hoists',effs) = 
			 rfun (f,top_set,hoist_set,econtext_orig)
		     val econtext' = Map.insert(econtext, v, effs)
		 in
		     loop (rest, (v,f')::vfl, top_bnds @ top_bnds',
			   hoists @ hoists', econtext')
		 end

	  val (vfs', top_bnds, hoists, econtext') = 
	      loop (Sequence.toList vfs,[],[],[],econtext_orig)
(*	  val _ = ppout 2  *)
	in
	    (STAY(Fixcode_b(Sequence.fromList vfs')),
	     top_bnds, hoists, econtext',true)
	end

    | rbnd (Fixopen_b(vfs), top_set, hoist_set, econtext_orig) = 
	let
(*
	  val fvstr = (sl2s (map var2string (map #1 (Sequence.toList vfs))))
	  val _ = (pprint ("rewriting bnd for open funs: "^fvstr^"\n");ppin 2) 
*)

  fun loop ([],vfl,top_bnds,hoists,econtext) = 
	         (vfl,top_bnds,hoists,econtext)
	    | loop ((v,f)::rest,vfl,top_bnds,hoists,econtext) = 
	         let
		     val (f',top_bnds',hoists',effs) = 
			 rfun (f,top_set,hoist_set,econtext_orig)
		     val econtext' = Map.insert(econtext, v, effs)
		 in
		     loop (rest, (v,f')::vfl, top_bnds @ top_bnds',
			   hoists @ hoists', econtext')
		 end

	  val (vfs', top_bnds, hoists, econtext') = 
	      loop (Sequence.toList vfs,[],[],[],econtext_orig)
(*	  val _ = ppout 2  *)
	in
	    (STAY(Fixopen_b(Sequence.fromList vfs')),
	     top_bnds, hoists, econtext',true)
	end	
    
    | rbnd _ = raise HoistError

  and rexp (Var_e v,_,_,econtext) = 
         (Var_e v, [], [], elookup(econtext,v), true)

    | rexp (Const_e c,_,_,econtext) = (Const_e c, [], [], UNKNOWN_EFFS, true)

    | rexp (Let_e (seq, bndlst, bodexp), top_set, hoist_set, econtext) = 
      let
(*
          val t = newtag "let" 
          val _ = (plist ["start",t];ppin(3)) 
*)
	  val (top_set', hoist_set',
	       bnd_top_bnds, bnd_hoists, bnd_stay_bnds,
	       econtext', bnd_valuable) = 
	      rbnds(bndlst,top_set,hoist_set,econtext)

	  val bound_var_set = list2set (getBoundVars bndlst)
	  val body_hoist_set = union (bound_var_set,hoist_set)
	  val (bodexp',body_top_bnds, body_hoists, effs, body_valuable) = 
	      rexp(bodexp,top_set',body_hoist_set,econtext')

	  val stop_vars = list2set (getBoundVars bnd_stay_bnds)
	  val (body_hoists, body_stay_bnds) = filter_bnds(body_hoists, stop_vars)

(*
      val _ = diag ("bndlst: "^(bl2s bndlst)^"\n") 
      val _ = diag ("bvl': "^bvl2s (List.concat bvll')^", bndlst': "^(bl2s bndlst')^"\n") 
*)
	  val top_bnds = bnd_top_bnds @ body_top_bnds
	  val hoists = bnd_hoists @ body_hoists
	  val stay_bnds = bnd_stay_bnds @ body_stay_bnds

(*
      val _ = pprint ("laying down: "^(bl2s stay')^"\n") 
      val _ = pprint ("moving up: "^(bl2s (map #1 up')^"\n"))  
      val _ = (ppout 3;plist ["end",t]) 
*)
      in
	  (NilUtil.makeLetE seq stay_bnds bodexp',
	   top_bnds, hoists, effs, bnd_valuable andalso body_valuable)
      end

    | rexp (exp as Prim_e (allp, conlst, explst), top_set, hoist_set, econtext) = 
      let
	  val (conlst', con_top_bnds, con_hoists) = 
	      rcons' (conlst, top_set, hoist_set)

	  val (explst', exp_top_bnds, exp_hoists, effs_list, valuable_list) =
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
	      
      in
	  (Prim_e (allp, conlst', explst'), 
	   con_top_bnds @ exp_top_bnds,
	   con_hoists @ exp_hoists,
	   effs, valuable)
      end

    | rexp (Switch_e s, top_set, hoist_set, econtext) = 
      let
	  val (ns, top_bnds, hoists, effs, valuable) = 
	      rswitch (s, top_set, hoist_set, econtext)
      in
	  (Switch_e ns, top_bnds, hoists, effs, valuable)
      end

    | rexp (exp0 as App_e (opn, exp, conlist, explist1, explist2), 
	    top_set, hoist_set, econtext) = 
      let
	  val (exp', exp_top_bnds, exp_hoists, exp_effs, exp_valuable) = 
	      rexp (exp, top_set, hoist_set, econtext)

	  val (conlist', con_top_bnds, con_hoists) = 
	      rcons' (conlist, top_set, hoist_set)
	  val (explist1', top_bnds1, hoists1, effs1, valuable1) =
	      rexps (explist1, top_set, hoist_set, econtext)
	  val (explist2', top_bnds2, hoists2, effs2, valuable2) =
	      rexps (explist2, top_set, hoist_set, econtext)

	  val top_bnds = exp_top_bnds @ con_top_bnds @ top_bnds1 @ top_bnds2
          val hoists = exp_hoists @ con_hoists @ hoists1 @ hoists2

	  fun id (b:bool) = b
	  val components_valuable = 
	      exp_valuable andalso
	      (List.all id valuable1) andalso (List.all id valuable2)
	      
	  val (valuable, effs) = 
	      (case exp_effs of
		   (ARROW_EFFS (Total,rest)) => (components_valuable, rest)
		 | (ARROW_EFFS (Partial,rest)) => (false, rest)
		 | _ => (false, unknown_effs))

(*
	       val _ = (print "Processing application:\n ";
			Ppnil.pp_exp exp0;
			print " components_valuable = ";
			print (Bool.toString components_valuable);
			print " valuable = ";
			print (Bool.toString valuable);
			print "\n")
*)

      in
	  (App_e (opn, exp', conlist', explist1', explist2'), 
	   top_bnds, hoists, effs, valuable)
      end
  
    | rexp (ExternApp_e (exp,explist), top_set, hoist_set, econtext) = 
      let
	  val (exp', exp_top_bnds, exp_hoists, _, _) = 
	      rexp (exp, top_set, hoist_set, econtext)
	  val (explist',exps_top_bnds, exps_hoists, _, _) = 
	      rexps(explist, top_set, hoist_set, econtext)

	  val top_bnds = exp_top_bnds @ exps_top_bnds
          val hoists = exp_hoists @ exps_hoists
	  val valuable = false
	  val effs = unknown_effs
      in
	(ExternApp_e (exp',explist'), top_bnds, hoists, effs, valuable)
      end

    | rexp (Raise_e(exp,con), top_set, hoist_set, econtext) = 
      let
	  val (exp',top_bnds1, hoists1,_,_) = rexp (exp, top_set, hoist_set, econtext)
	  val (con',top_bnds2, hoists2) = rcon' (con, top_set, hoist_set) 
	  val top_bnds = top_bnds1 @ top_bnds2
	  val hoists = hoists1 @ hoists2
	  val effs = unknown_effs
	  val valuable = false
      in
	  (Raise_e(exp',con'), top_bnds, hoists, effs, valuable)
      end
  
    | rexp (Handle_e (exp1,var,exp2) ,top_set, hoist_set, econtext) = 
      let
	  val (exp1',top_bnds1,hoists1,_,_) = rexp(exp1, top_set, hoist_set, econtext)
	  val (exp2',top_bnds2,hoists2,_,_) = rexp(exp2, top_set, hoist_set, econtext)
	  val top_bnds = top_bnds1 @ top_bnds2
	  val hoists = hoists1 @ hoists2
	  val effs = unknown_effs
	  val valuable = false	
      in
	  (Handle_e (exp1',var,exp2'), top_bnds, hoists, effs, valuable)
      end

  and rexps (explst, top_set, hoist_set, econtext) = 
      let
	  val (explst', top_bnd_list, hoists_list, effs_list, valuable_list) = 
	      Listops.unzip5 (map (fn e => rexp(e, top_set, hoist_set, econtext))
			      explst)
      in
	  (explst',
	   List.concat top_bnd_list,
	   List.concat hoists_list,
	   effs_list,
	   valuable_list)
      end
	      

  and rswitch (Intsw_e{arg,size,arms,default,result_type},top_set,hoist_set,econtext)=
      let
	  val (arg', arg_top_bnds, arg_hoists, _, _) = 
	      rexp (arg, top_set, hoist_set,econtext)

	  fun mapper (w,e) = 
	      let 
		  val (e',top_bnds,hoists,_,_) = rexp (e,top_set,hoist_set,econtext)
	      in
		  if (!hoist_through_switch) then
		      ((w,e'), top_bnds, hoists)
		  else
		      let 
			  val (hoists, stay_bnds) = filter_ebnds hoists
		      in
			  ((w, NilUtil.makeLetE Sequential stay_bnds e'), 
			   top_bnds, hoists)
		      end
	      end
	      
	  val (arms',arms_top_bnds_list, arms_hoists_list) = 
	      Listops.unzip3 (map mapper arms)
	  val arms_top_bnds = List.concat arms_top_bnds_list
	  val arms_hoists = List.concat arms_hoists_list

	  val (default',default_top_bnds,default_hoists,_,_) = 
	      rexpopt(default, top_set,
		      (*cheat*)
		      if (!hoist_through_switch) then hoist_set else top_set,
		      econtext)
	      
	  val effs = con2eff result_type
	  val valuable = false
      in
	  (Intsw_e {arg=arg', size=size, arms=arms', default=default',
		    result_type = result_type},
	   arg_top_bnds @ arms_top_bnds @ default_top_bnds,
	   arg_hoists @ arms_hoists @ default_hoists,
	   effs, valuable)
      end

    | rswitch (Sumsw_e {arg,sumtype,bound,arms,default,result_type},
	       top_set, hoist_set, econtext) = 
      let

	  val (arg', arg_top_bnds, arg_hoists, _, _) = 
	      rexp (arg, top_set, hoist_set, econtext)

	  val boundset = Set.singleton bound
	  val arm_hoist_set = Set.add(hoist_set,bound)
	  fun mapper (w,tr,e) = 
	      let 
		  val (e',top_bnds,hoists,_,_) = 
		      rexp (e, top_set, arm_hoist_set, econtext)
	      in
		  if (!hoist_through_switch) then
		      let
			  val (up,stay) = filter_bnds (hoists, boundset) 
			  val e'' = NilUtil.makeLetE Sequential stay e'
		      in
			  ((w,tr,e''), top_bnds, up)
		      end
		  else
		      let
			  val (hoists, stay_bnds2) = filter_ebnds hoists
			  val (hoists, stay_bnds1) = filter_bnds (hoists, boundset) 
			  val stay_bnds = stay_bnds1 @ stay_bnds2
		      in
			  ((w, tr, NilUtil.makeLetE Sequential stay_bnds e'), 
			   top_bnds, hoists)
		      end
	      end
	      
	  val (arms',arms_top_bnds_list, arms_hoists_list) = 
	      Listops.unzip3 (map mapper arms)
	  val arms_top_bnds = List.concat arms_top_bnds_list
	  val arms_hoists = List.concat arms_hoists_list

	  val (default',default_top_bnds,default_hoists,_,_) = 
	      rexpopt(default, top_set,
		      (*cheat*)
		      if (!hoist_through_switch) then hoist_set else top_set,
		      econtext)
	      
	  val effs = con2eff result_type
	  val valuable = false
      in
	  (Sumsw_e {arg = arg',
		    sumtype = sumtype,
		    bound = bound,
		    arms = arms',
		    default = default',
		    result_type = result_type},
	   arg_top_bnds @ arms_top_bnds @ default_top_bnds,
	   arg_hoists @ arms_hoists @ default_hoists,
	   effs, valuable)
      end

    | rswitch (Exncase_e {arg,bound,arms,default,result_type},
	       top_set, hoist_set, econtext) = 
      let
	  val (arg', arg_top_bnds, arg_hoists, _, _) = 
	      rexp (arg, top_set, hoist_set, econtext)

	  val boundset = Set.singleton bound
	  val arm_hoist_set = add(hoist_set,bound)

	  (* e1 is supposed to be a path, so no point traversing it *)
	  fun mapper (e1,tr,e) = 
	      let 
		  val (e',top_bnds,hoists,_,_) = 
		      rexp (e, top_set, arm_hoist_set, econtext)
	      in
		 if (!hoist_through_switch) then
		      let  
			  val (up,stay) = filter_bnds (hoists, boundset) 
			  val e'' = NilUtil.makeLetE Sequential stay e'
		      in
			  ((e1,tr,e''), top_bnds, up)
		      end
		 else
		      let
			  val (hoists, stay_bnds2) = filter_ebnds hoists
			  val (hoists, stay_bnds1) = filter_bnds (hoists, boundset) 
			  val stay_bnds = stay_bnds1 @ stay_bnds2
		      in
			  ((e1, tr, NilUtil.makeLetE Sequential stay_bnds e'), 
			   top_bnds, hoists)
		      end
	      end	
      
	  val (arms',arms_top_bnds_list, arms_hoists_list) = 
	      Listops.unzip3 (map mapper arms)
	  val arms_top_bnds = List.concat arms_top_bnds_list
	  val arms_hoists = List.concat arms_hoists_list

	  val (default',default_top_bnds,default_hoists,_,_) = 
	      rexpopt(default, top_set,
		      (*cheat*)
		      if (!hoist_through_switch) then hoist_set else top_set,
		      econtext)
	      
	  val effs = con2eff result_type
	  val valuable = false
      in
	  (Exncase_e {arg=arg',
		      bound=bound,
		      arms=arms',
		      default=default',
		      result_type = result_type},
	   arg_top_bnds @ arms_top_bnds @ default_top_bnds,
	   arg_hoists @ arms_hoists @ default_hoists,
	   effs, valuable)
    end

    | rswitch (Typecase_e {arg,arms,default,result_type},_,_,econtext) = 
    (Util.error "hoist.sml" "Typecase_e not implemented yet")
(*
    let
      val (arg',bvl') = rcon' (arg,cvs)

      fun procarm(arms,tprocfun,cvs) = 
	let
	  val l = map (fn (a,b) => (tprocfun (a,cvs),rexp (b,cvs))) arms
	  val f = fn (((a,b),(c,d)),(e,f)) => ((a,c)::e,b@d@f)
	in
	  foldr f ([],[]) l
	end
      val (arms',bvl'') = procarm(arms,fn (x,b) => (x,[]),cvs)
      val (default',bvl''') = rexpopt(default,cvs)
    in
      (Typecase_e {arg=arg',
		   arms=arms',
		   default=default},
       bvl'@bvl''@bvl''')
    end
*)
  and rexpopt(NONE,top_set, hoist_set,econtext) = (NONE,[],[],unknown_effs,true)
    | rexpopt(SOME(e), top_set, hoist_set, econtext) = 
      let 
	  val (e',top_bnds,hoists,effs,valuable) = 
	      rexp (e, top_set, hoist_set, econtext) 
      in 
	  (SOME(e'), top_bnds, hoists, effs, valuable) 
      end

  and rfun (Function{effect=eff,recursive=isrec,isDependent=isdep,
		     tFormals=typelist, eFormals=eformals,
		     fFormals=fformals, body=bod, body_type=ret},
	    top_set, hoist_set, econtext) = 
      let

(*
        val t = newtag "fun" 
        val _ = (plist ["start",t];ppin(3)) 
*)
	  val econtext' = 
	      List.foldr (fn ((v,_,c),ectx) => Map.insert(ectx, v, con2eff c))
              econtext eformals
	      
	  val cvars = map #1 typelist
	  val (evars, nts, etypes) = Listops.unzip3 eformals
	  val (etypes', arg_top_bnds, arg_hoists) = 
	         rcons' (etypes, top_set, hoist_set)
	  val eFormals' = Listops.zip3 evars nts etypes'
          val boundvar_set = list2set (cvars @ evars @ fformals)
	  val body_hoist_set = Set.union (hoist_set, boundvar_set)

	  val (bod',body_top_bnds, body_hoists,effs,_) = 
	      rexp(bod,top_set, body_hoist_set, econtext)
          val (bod_up, bod_stay) = filter_bnds (body_hoists, boundvar_set)
	  val newbod = NilUtil.makeLetE Sequential bod_stay bod'

	  val (ret',retcon_top_cbnds, retcon_choists) = 
	      rcon (ret,top_set,body_hoist_set)
          val (ret_up, ret_stay) = filter_cbnds (retcon_choists, boundvar_set)
          val newret = NilUtil.makeLetC ret_stay ret'

          val hoists = arg_hoists @ 
	                (map (fn (cb,s) => (Con_b(Runtime,cb), s)) ret_up) @ bod_up
	  val top_bnds = arg_top_bnds @ 
	      (map (fn cb => Con_b(Runtime,cb)) retcon_top_cbnds) @ body_top_bnds
(*
        val _ = pprint ("laying down at fun: "^(bl2s stay)^"\n") 
        val _ = ppout 3 
        val _ = plist ["end",t]
*)
      in
	  (Function{effect=eff,recursive=isrec,isDependent=isdep,
		    tFormals=typelist,eFormals=eformals, fFormals=fformals,
		    body=newbod,body_type=newret},
	   top_bnds, hoists, ARROW_EFFS(eff, effs)) 
      end
  
  fun optimize (MODULE {bnds, imports, exports}) = 
      let
(*    val _ = diag("we are hoisting\n") *)

	  (* collect the 'top level' var names *)
	  fun split ([],cvs,ectx) = (cvs,ectx)
	    | split (ImportValue(l,v,_,c)::rest,cvs,ectx) = 
	      split(rest,Set.add(cvs,v),Map.insert(ectx,v,con2eff c))
	    | split (ImportType(l,v,k)::rest,cvs,ectx) = 
		    split(rest,add(cvs,v), ectx)
		    
	  val (top_set, econtext) = split (imports, Set.empty, empty_fnmap)
	  val hoist_set = top_set

	  val (_,_,top_bnds,hoists,stay_bnds,_, _) = 
	      rbnds(bnds, top_set, hoist_set, econtext)
		    
	  val bnds' = top_bnds @ (map #1 hoists) @ stay_bnds
		    
      in
	  MODULE {bnds=bnds',imports=imports,exports=exports}  
      end

end
