(*$import HOIST Nil NilUtil ListPair Stats Name *)
(*** questions, should fixopen_b ever move? *)
(* 
 var naming convections
 cvs - XXX var set
 bvl/bvs : (bnd * var set) list
*)
structure Hoist :> HOIST = 

struct
  open Nil Name

  exception HoistError

  datatype bndtype = STAY of bnd | UP of var
  datatype conbndtype = CSTAY of conbnd | CUP of var

  val debug = Stats.ff("HoistDebug")
  fun diag s = if !debug then print s else ()

  (* set helper funs *)
  structure Set = Name.VarSet
  local open Set 
  in
    type bvs = bnd * set
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

(*
  (* start junk *)
 fun cat_app_zip l = foldr (fn ((a,b),(c,d)) => (a::c,b@d)) ([],[]) l
 fun cat_app_zip' (a,b) = foldr (fn ((a,b),(c,d)) => (a::c,b@d)) ([],[]) (ListPair.zip (a,b))

*)

  local val count = ref 0 
     in fun newtag i = let val _ = count := !count + 1 
		    in "_" ^ i ^ (Int.toString(!count)) end 
  end

  val indent = ref 0

  fun ppin(a) = indent:=(!indent+a)
  fun ppout(a) = if (!indent < a) then indent:=0 else indent:=(!indent-a)

  fun len l = (Int.toString (length l))

  fun pprint s = 
    let
      fun space (0,ss) = ss
	| space (a,ss) = space(a-1,ss^" ")
    in
      (diag (space(!indent,""));diag s)
    end

  fun plist l = 
    let
      val _ = pprint ""
      fun pl [] = diag "\n"
	| pl (h::t) = (diag (h^" ");pl t)
    in
      pl l
    end

  fun sl2s l =
    let
      fun loop ([],s) = s^"]"
	| loop (h::[],s) = s^h^"]"
	| loop (h::t,s) = loop(t,s^h^",")
    in
      loop (l,"[")
    end


  fun vl2s vl = 
    let
      open Name
    in
      sl2s (map var2string vl)
    end

 fun vs2s vs = vl2s (set2list vs) 

 (* in/out correct order *)
 fun bl2s bl = 
   let
     open Ppnil Name
     val vl = getBoundVars bl
   in
     (sl2s (map var2string vl))
   end

 fun bvl2s (bvl:bvs list) = bl2s (map #1 bvl)

 fun bvs2s (bvl:bvs list) = bl2s (map #1 bvl)
 fun bvsl2s (bvs:bvs list) = bl2s (map #1 bvs)

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

  fun filter_cbnds (bvl, stop_vars) =
      let
	  fun loop([],stop_vars,up,stay) = (rev up, rev stay)
	    | loop((bv as (cb,freevars))::rest, stop_vars, up, stay) =
	      if (disjoint(freevars,stop_vars)) then
		  loop(rest, stop_vars, bv :: up, stay)
	      else
		  loop(rest, Set.add(stop_vars,get_conbnd_var cb),
		       up, cb :: stay)
      in
	  loop(bvl, stop_vars, nil, nil)
      end

  (* CS 2/21/99
     The set cvs appears to be the set of variables which are
     bound either at top level or at least at a higher level.
     A binding can be hoisted iff its free variables are included
     in this set. 
   *)

  fun rconbnd (Con_cb(v,con), cvs) : conbndtype * (conbnd * set) list = 
    let
      val (con',bvl : (conbnd * set) list) = rcon (con,cvs)
      val freev = freeConVars con'
    in
      case subset(freev,cvs) of
	true  => (CUP v, bvl @ [(Con_cb(v,con'),freev)])
      | false => (CSTAY (Con_cb (v,con')), bvl)
    end

    (*** should these move? *)
    (*** if this is changed (ie to hoist open_cbs) then code_cb wont work *)
    | rconbnd (cb as Open_cb(v,vklist,con'), cvs) = 
	let
	  val (con'',bvl') = rcon (con',cvs)
	in
	  (CSTAY(Open_cb(v,vklist,con'')),bvl')
	end

    | rconbnd (cb as (Code_cb goo), cvs) = 
	let
	  val (CSTAY(Open_cb goo'),bvl) = rconbnd (Open_cb goo, cvs)
	in
	  (CSTAY(Code_cb goo'),bvl)
	end

  and rconbnd' arg = 
      let val (ans, bs) = rconbnd arg
      in
	  (ans, map (fn (cb,s) => (Con_b(Runtime,cb), s)) bs)
      end


  (*
     If we hoist a binding, it's variable gets added to the cvs set
     so that we can hoist later bindings depending on this one.
   *)


  (* bvsll is inorder and conbnd list is inorder *)
  and rconbnds (conbnds, cvs) : ((conbnd * set) list * conbnd list) = 
    let
      fun loop ([],cvs,up,stay) = (List.concat(rev up), rev stay)
	| loop (cb::rest,cvs,up,stay) = 
	   (case rconbnd (cb,cvs) of 
		(CSTAY cb',bvl) => loop (rest,cvs, bvl :: up, cb'::stay)
	      | (CUP v, bvl) => loop(rest, add(cvs,v), bvl :: up, stay))
    in
      loop (conbnds,cvs,[],[])
    end

  and rcon (Prim_c (primcon,conlist), cvs) : (con * (conbnd * set) list) = 
      let
	  val (conlist', bvl_list) = 
	      Listops.unzip (map (fn a => rcon (a,cvs)) conlist)
	  val bvl = List.concat bvl_list
      in
	  (Prim_c(primcon,conlist'), bvl)
      end

    | rcon (Mu_c (isRecursive,vcseq), cvs) = 
      let
	  val vclist = Sequence.toList vcseq
	  val vclist' = map (fn (v,c) => (v,rcon(c,cvs))) vclist
	  val l = map (fn (v,(c,bvl)) => ((v,c),bvl)) vclist'
	  val (vclist'',bvll) = ListPair.unzip l
      in
	  (Mu_c (isRecursive,Sequence.fromList vclist''),
	   List.concat bvll)
      end

    | rcon (con as AllArrow_c _, cvs) = (con, [])

(*
      let
	  val (eNames,eTypes) = ListPair.unzip eFormals
	  val (eTypes', bvll) = ListPair.unzip (map (fn c => rcon (c,cvs)) eTypes)
	  val eFormals' = ListPair.zip (eNames,eTypes')
	  val (con',bvl') = rcon (con,cvs)
      in
	  (AllArrow_c(openness=openness,
		      effect=effect,
		      isDependent = isDependent,
		      tFormals = tFormals,
		      vkl,vlist,conlist',w32,con'),
	   (List.concat bvll) @ bvl')
      end
*)
    | rcon (ExternArrow_c (conlist,con),cvs) = 
      let
	  val (conlist',bvll) = 
	      Listops.unzip (map (fn c => rcon (c,cvs)) conlist)
	  val (con',bvl') = rcon (con,cvs)
      in
	  (ExternArrow_c(conlist',con'),
	   (List.concat bvll) @ bvl')
      end

    | rcon (c as Var_c(var'),cvs) = (c,[])
      
    | rcon (Let_c(letsort,bndlist,bodcon),cvs) = 
      let
	  val (up,stay) = rconbnds(bndlist,cvs) 

	  val bound_var_set = list2set (map get_conbnd_var bndlist)
          val cvs' = union (bound_var_set,cvs)

	  val (bodcon',body_up) = rcon (bodcon,cvs') 
	      
	  (* bindings that have free variables whose bindings
           are in this let must stay *)
	  val stop_vars = list2set (map get_conbnd_var stay)
	  val (body_up', body_stay') = filter_cbnds(body_up, stop_vars)
	      
	  val up'' = up @ body_up'
	  (* XXX what happened to the letsort *)
      in  (NilUtil.makeLetC (stay @ body_stay') bodcon',
	   up'')
      end

    | rcon (Crecord_c lclist, cvs) = 
      let
	  val lcu_list = map (fn (l,con') => (l,rcon(con',cvs))) lclist
	  val (lclist', up_list) = 
	      Listops.unzip (map (fn (l,(c,bvl)) => ((l,c),bvl)) lcu_list)
      in
	  (Crecord_c lclist', List.concat up_list)
      end

    | rcon (Proj_c (con,lab),cvs) = 
      let
	  val (con',bvl) = rcon (con,cvs)
      in
	  (Proj_c(con',lab),bvl)
      end

    | rcon (Typeof_c e,cvs) = 
      let
	  val econtext = empty_fnmap (* should be no applications in a typeof *)
	  val (e',[],_: hoist_effs,true) = rexp (e, cvs, econtext)
      in
	  (Typeof_c e',[])
      end
  
    | rcon (Closure_c(codecon,envcon),cvs) = 
      let
	  val (codecon',bvl) = rcon (codecon, cvs)
	  val (envcon',bvl') = rcon (envcon, cvs)
      in
	  (Closure_c(codecon',envcon'), bvl@bvl')
      end

    | rcon (App_c(con,conlist),cvs) = 
      let
	  val (con',bvl') = rcon (con,cvs)
	  val (conlist',bvll) = 
	      Listops.unzip (map (fn c => rcon (c,cvs)) conlist)
      in
	  (App_c(con',conlist'),
	   List.concat (bvl' :: bvll))
      end

    (**** clean me till i shine *)
    | rcon (Typecase_c {arg,arms,default,kind},cvs) = 
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
    | rcon (Annotate_c (annot,con),cvs) = 
      let
	  val (con',bvl) = rcon (con,cvs)
      in
	  (Annotate_c (annot,con'),bvl)
      end

  and rcon' arg = 
      let val (c', bs) = rcon arg
      in
	  (c', map (fn (cb,s) => (Con_b(Runtime,cb), s)) bs)
      end

  and rbnds (bnd_list,cvs,econtext) : (bvs list * bnd list * effect_context * bool) = 
      let
	  fun loop ([],cvs,up,stay,econtext,valuable) = 
	      (List.concat (rev up),rev stay,econtext,valuable)
	    | loop (b::rest,cvs,up,stay,econtext,valuable) = 
	      (case rbnd (b,cvs,econtext) of 
		   (STAY b, bvsl,econtext',valuable') => 
		       loop (rest, cvs, bvsl::up, b::stay,
			     econtext', valuable andalso valuable')
		 | (UP v, bvsl, econtext', valuable') => 
		       loop (rest, Set.add(cvs,v), bvsl::up, stay,
			     econtext', valuable andalso valuable'))
      in
	  loop (bnd_list,cvs,[],[],econtext,true)
      end

  and rbnd (Con_b(p,cb),cvs,econtext) : (bndtype * bvs list * effect_context * bool) = 
      let
	  val v = get_conbnd_var cb
      (*
          val vstr = var2string v 
          val _ = (pprint ("rewriting bnd for con var: "^vstr^"\n");ppin 2) 
          val _ = ppout 2 
       *)
      in
	  case rconbnd' (cb,cvs) of
	      (CUP v,bvl) => (UP v,bvl,econtext,true)
	    | (CSTAY cb', bvl) => (STAY (Con_b (p,cb')),bvl,econtext,true)
      end

    | rbnd (Exp_b(v,niltrace,e), cvs, econtext) = 
      let
(*
         val vstr = var2string v
         val _ = (pprint ("rewriting bnd for exp var: "^vstr^"\n"); ppin 2)  
*)
	  val (e',bvl',effs,valuable) = rexp(e,cvs,econtext)
	  
	  val free_vars = freeExpVars e'

(*
	      val _ = print ("Free vars for " ^ vstr ^ " are [ ")
	      val _ = print " are [ "
	      val _ = Set.app Ppnil.pp_var free_vars
	      val _ = print "]\n"
	      val _ = Ppnil.pp_exp e'
	      val _ = print "***\n"
*)
	  val hoistable = 
	      valuable andalso Set.isSubset(free_vars,cvs)

(*
          val _ = if hoistable then pprint ("E-moving "^vstr^" up\n") 
        	      else pprint ("E-keeping "^vstr^" here\n") 
          val _ = ppout 2 
*)

	  val newbnd = Exp_b(v,niltrace,e')
	  val econtext' = Map.insert(econtext,v,effs)
    in
      if hoistable then 
	  (UP v, bvl'@[(newbnd,free_vars)],econtext',valuable)
      else 
	  (STAY newbnd,bvl',econtext',valuable)
    end

    | rbnd (Fixcode_b(vfs), cvs, econtext_orig) = 
	let
(*
	  val fvstr = (sl2s (map var2string (map #1 vfs)))
	  val _ = (pprint ("rewriting bnd for code funs: "^fvstr^"\n");ppin 2) 
*)
	  fun loop ([],vfl,bvl',econtext) = (vfl,bvl',econtext)

	    | loop ((v,f)::rest,vfl,bvl',econtext) = 
	      let
		  val (f',bvl'',effs) = rfun (f,cvs,econtext_orig)
		  val econtext' = Map.insert(econtext,v, effs)
	      in
		  loop (rest,(v,f')::vfl,bvl''@bvl',econtext')
	      end

	  val (vfs',bvl''',econtext') = 
	      loop (Sequence.toList vfs,[],[],econtext_orig)
(*	  val _ = ppout 2  *)
	in
	    (STAY(Fixcode_b(Sequence.fromList vfs')),bvl''',econtext',true)
	end

    | rbnd (Fixopen_b(vfs), cvs, econtext_orig) = 
	let
(*
	  val fvstr = (sl2s (map var2string (map #1 (Sequence.toList vfs))))
	  val _ = (pprint ("rewriting bnd for open funs: "^fvstr^"\n");ppin 2) 
*)

	  fun loop ([],vfl,bvl',econtext) = (vfl,bvl',econtext)

	    | loop ((v,f)::rest,vfl,bvl',econtext) = 
	      let
		  val (f',bvl'',effs) = rfun (f,cvs,econtext_orig)
		  val econtext' = Map.insert(econtext,v, effs)
	      in
		  loop (rest,(v,f')::vfl,bvl''@bvl',econtext')
	      end

	  val (vfs',bvl''',econtext') = 
	      loop (Sequence.toList vfs,[],[],econtext_orig)
(*	  val _ = ppout 2  *)
	in
	    (STAY(Fixopen_b(Sequence.fromList vfs')),bvl''',econtext',true)
	end
    
    | rbnd _ = raise HoistError

  and rexp (Var_e v,cvs,econtext) = (Var_e v,[],elookup(econtext,v),true)

    | rexp (Const_e c,cvs,econtext) = (Const_e c, [], UNKNOWN_EFFS, true)

    | rexp (Let_e (seq, bndlst, bodexp),cvs,econtext) = 
      let
(*
          val t = newtag "let" 
          val _ = (plist ["start",t];ppin(3)) 
*)

	  val bound_var_set = list2set (getBoundVars bndlst)
	      
	  val (up,stay,econtext',valuable') = rbnds(bndlst,cvs,econtext)

	  val cvs' = union (bound_var_set,cvs)
	  val (bodexp',body_up,effs,valuable) = rexp(bodexp,cvs',econtext')

	  val stop_vars = list2set (getBoundVars stay)
	  val (body_up', body_stay') = filter_bnds(body_up, stop_vars)

(*
      val _ = diag ("bndlst: "^(bl2s bndlst)^"\n") 
      val _ = diag ("bvl': "^bvl2s (List.concat bvll')^", bndlst': "^(bl2s bndlst')^"\n") 
*)
	  val up' = up @ body_up
	  val stay' = stay @ body_stay'

(*
      val _ = pprint ("laying down: "^(bl2s stay')^"\n") 
      val _ = pprint ("moving up: "^(bl2s (map #1 up')^"\n"))  
      val _ = (ppout 3;plist ["end",t]) 
*)
      in
	  (NilUtil.makeLetE seq stay' bodexp',
	   up', effs, valuable' andalso valuable)
      end

    | rexp (exp as Prim_e (allp, conlst, explst), cvs, econtext) = 
      let
	  val (conlst', con_bvll) = 
	      Listops.unzip (map (fn c => rcon'(c, cvs)) conlst)
	  val con_bvl = List.concat con_bvll
	      
	  val (explst', exp_bvll, effs_list, valuable_list ) = 
	      Listops.unzip4 (map (fn e => rexp(e, cvs, econtext)) explst)
	  val exp_bvl = List.concat exp_bvll

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
      (Prim_e (allp, conlst', explst'), con_bvl @ exp_bvl,
       effs, valuable)
    end

    | rexp (Switch_e s, cvs, econtext) = 
      let
	  val (ns, bvl, effs, valuable) = rswitch (s, cvs, econtext)
      in
	  (Switch_e ns, bvl, effs, valuable)
      end

    | rexp (exp0 as App_e (opn, exp, conlist, explist1, explist2), cvs, econtext) = 
      let
	  val (exp', exp_bvl, exp_effs, exp_valuable) = rexp (exp,cvs, econtext)
	  val (conlist', conlist_bvll) = Listops.unzip (map (fn a => rcon'(a,cvs)) conlist)
	  val (explist1',explist1_bvll, explist1_effs, explist1_valuable) = 
	      Listops.unzip4 (map (fn e => rexp(e, cvs, econtext)) explist1)
	  val (explist2',explist2_bvll, explist2_effs, explist2_valuable) = 
	      Listops.unzip4 (map (fn e => rexp(e, cvs, econtext)) explist2)
	      
	  val bvl = List.concat (exp_bvl :: conlist_bvll @ explist1_bvll @ explist2_bvll)
	      
	  fun id (b:bool) = b
	      
	  val components_valuable = 
	      exp_valuable andalso
	      (List.all id explist1_valuable) andalso
	      (List.all id explist2_valuable)
	      
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
	  (App_e (opn, exp', conlist', explist1', explist2'), bvl, effs, valuable)
      end
  
    | rexp (ExternApp_e (exp,explist), cvs, econtext) = 
      let
	  val (exp', exp_bvl, _, _) = rexp (exp,cvs, econtext)
	  val (explist',explist_bvll, _, _) = 
	      Listops.unzip4 (map (fn e => rexp(e, cvs, econtext)) explist)

	  val bvl = List.concat (exp_bvl :: explist_bvll)
	  val valuable = false
	  val effs = unknown_effs
      in
	(ExternApp_e (exp',explist'), bvl, effs, valuable)
      end

    | rexp (Raise_e(exp,con),cvs,econtext) = 
      let
	  val (exp',bvl,_,_) = rexp (exp,cvs,econtext)
	  val (con',bvl') = rcon' (con,cvs)
	  val effs = unknown_effs
	  val valuable = false
      in
	  (Raise_e(exp',con'), bvl @ bvl', effs, valuable)
      end
  
    | rexp (Handle_e (exp,var,exp'),cvs,econtext) = 
      let
	  val (nexp,bvl,_,_) = rexp (exp,cvs,econtext)
	  val (nexp',bvl',_,_) = rexp (exp',cvs,econtext)
 	  val effs = unknown_effs
	  val valuable = false
      in
	  (Handle_e (nexp,var,nexp'), bvl@bvl', effs, valuable)
      end

  and rswitch (Intsw_e {arg,size,arms,default,result_type},cvs,econtext) = 
      let
	  
	  fun mapper (w,e) = 
	      let 
		  val (e',bvl',_,_) = rexp (e,cvs,econtext)
	      in
		  ((w,e'), bvl')
	      end
	      
	  val (arg', bvl', _, _) = rexp (arg,cvs,econtext)
	  val (arms',bvll) = Listops.unzip (map mapper arms)
	  val bvl'' = List.concat bvll
	  val (default',bvl''',_,_) = rexpopt(default,cvs,econtext)
	      
	  val effs = unknown_effs
	  val valuable = false
      in
	  (Intsw_e {arg=arg', size=size, arms=arms', default=default',
		    result_type = result_type},
	   bvl'@bvl''@bvl''', effs, valuable)
      end

    | rswitch (Sumsw_e {arg,sumtype,bound,arms,default,result_type},cvs, econtext) = 
      let

	  val boundset = Set.singleton bound

	  val cvs' = Set.add(cvs,bound)

	  fun mapper (w,tr,e) = 
	      let 
		  val (e',bvl,_,_) = rexp (e,cvs',econtext)
		  val (up,stay) = filter_bnds (bvl, boundset) 
		  val e'' = NilUtil.makeLetE Sequential stay e'
	      in
		  ((w,tr,e''), up)
	      end
	      
	  val (arg', bvl', _, _) = rexp (arg,cvs,econtext)
	  val (arms',bvll) = Listops.unzip (map mapper arms)
	  val bvl'' = List.concat bvll
	  val (default',bvl''',_,_) = rexpopt(default,cvs,econtext)
	      
	  val effs = unknown_effs
	  val valuable = false
      in
	  (Sumsw_e {arg = arg',
		    sumtype = sumtype,
		    bound = bound,
		    arms = arms',
		    default = default',
		    result_type = result_type},
	   bvl' @ bvl'' @ bvl''', 
	   effs, valuable)
      end

    | rswitch (Exncase_e {arg,bound,arms,default,result_type},cvs,econtext) = 
      let
	  val boundset = Set.singleton bound

	  val cvs' = add(cvs,bound)

	  (* e1 is supposed to be a path, so no point traversing it *)
	  fun mapper (e1,tr,e) = 
	      let 
		  val (e',bvl,_,_) = rexp (e,cvs',econtext)
		  val (up,stay) = filter_bnds (bvl, boundset) 
		  val e'' = NilUtil.makeLetE Sequential stay e'
	      in
		  ((e1,tr,e''), up)
	      end
	      
	  val (arg', bvl', _, _) = rexp (arg,cvs,econtext)
	  val (arms',bvll) = Listops.unzip (map mapper arms)
	  val bvl'' = List.concat bvll
	  val (default',bvl''',_,_) = rexpopt(default,cvs,econtext)
	      
	  val effs = unknown_effs
	  val valuable = false
      in
	  (Exncase_e {arg=arg',
		      bound=bound,
		      arms=arms',
		      default=default',
		      result_type = result_type},
	   bvl'@bvl''@bvl''', effs, valuable)
    end

    | rswitch (Typecase_e {arg,arms,default,result_type},cvs,econtext) = 
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
  and rexpopt(NONE,cvs,econtext) = (NONE,[],unknown_effs,true)
    | rexpopt(SOME(a),cvs,econtext) = 
      let 
	  val (e',bvl,effs,valuable) = rexp (a,cvs,econtext) 
      in 
	  (SOME(e'),bvl,effs,valuable) 
      end

  and rfun (Function{effect=eff,recursive=isrec,isDependent=isdep,
		     tFormals=typelist, eFormals=eformals,
		     fFormals=fformals, body=bod, body_type=ret},cvs,econtext) = 
      let

(*
        val t = newtag "fun" 
        val _ = (plist ["start",t];ppin(3)) 
*)
	  val cvars = map #1 typelist
	  val evars = map #1 eformals
          val boundvar_set = list2set (cvars @ evars @ fformals)
	  val cvs' = Set.union (cvs, boundvar_set)

	  val econtext' = 
	      List.foldr (fn ((v,_,c),ectx) => Map.insert(ectx, v, con2eff c))
              econtext eformals
	      
	  val (bod',bvl,effs,_) = rexp(bod,cvs',econtext)
	  val (ret',cbvl) = rcon (ret,cvs')
          val (ret_up, ret_stay) = filter_cbnds (cbvl, boundvar_set)
          val (bod_up, bod_stay) = filter_bnds (bvl, boundvar_set)
	  val newbod = NilUtil.makeLetE Sequential bod_stay bod'
          val newret = NilUtil.makeLetC ret_stay ret'

          val up = (map (fn (cb,s) => (Con_b(Runtime,cb), s)) ret_up) @ bod_up
(*
        val _ = pprint ("laying down at fun: "^(bl2s stay)^"\n") 
        val _ = ppout 3 
        val _ = plist ["end",t]
*)
      in
	  (Function{effect=eff,recursive=isrec,isDependent=isdep,
		    tFormals=typelist,eFormals=eformals, fFormals=fformals,
		    body=newbod,body_type=newret},
	   up, ARROW_EFFS(eff, effs)) 
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
		    
	  val (cvs, econtext) = split (imports,Set.empty,empty_fnmap)
		    
	  val (bvl, bnds, _, _) = rbnds(bnds, cvs, econtext)
		    
	  val bnds' = (map #1 bvl) @ bnds
		    
      in
	  MODULE {bnds=bnds',imports=imports,exports=exports}  
      end

end
