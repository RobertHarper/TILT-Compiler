(*$import HOIST Nil NilUtil ListPair Stats *)
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

  (**** in general should hoisting look in kinds? *)
  (**** for nilutul.free[expcon/con]varin[exp/con] should look in kind be true or false? *)
  val LOOK_IN_KIND = false

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
  end
  (* end set help funs *)

  type bvlt = bnd * var list


  (*** should look-in-kinds flag be true? *)
  (* returns set of free vars *)
  fun freeExpVars e = let val (a,b) = (NilUtil.freeExpConVarInExp (true,e)) in union (list2set a,list2set b) end(* free term/type vars *)

  (* returns set of free vars *)
  fun freeConVars c = list2set (NilUtil.freeConVarInCon (true,c)) (* free type vars *)

  (* returns set of free vars *)
  fun freeKindVars k = 
    let
      fun loop (Type_k, s) = s
 	| loop (Singleton_k c, s) = union(freeConVars c,s)
	| loop (Record_k r,s) = foldr (fn (a,b) => union(a,b)) s (map freeKindVars (map #2 (Sequence.toList r)))
	| loop (Arrow_k (oness,vkl,k),s) = union((foldr (fn (a,b) => union(a,b)) s (map freeKindVars (map #2 vkl))),
						 (freeKindVars k))
    in
      loop (k,empty)
    end

  (* returns set of free vars *)
  fun freeConbndVars cb' = 
    let
      fun free' vkl = foldr (fn (freeset,freeset') => union (freeset,freeset')) empty (map (fn (v,k) => freeKindVars k) vkl)

      fun free (Con_cb (v,c)) = list2set (NilUtil.freeConVarInCon(LOOK_IN_KIND,c))
	| free (Open_cb (v,vkl,c,k)) = 
	let
	  val freeset = free' vkl
	  val freeset' = freeConVars c
	  val freeset'' = freeKindVars k
	in
	  union (union (freeset,freeset'),freeset'')
	end

    in
      free cb'
    end


    
  (* get the bound var from a conbnd *)
  fun get_conbnd_var (Con_cb (v,c)) = v
    | get_conbnd_var (Open_cb (v,_,_,_)) = v
    | get_conbnd_var (Code_cb (v,_,_,_)) = v

  (* get the bound vars from a list of bnds *)
  fun getBoundVars bnd_list = 
    let
      fun gv ([],l) = rev l
	| gv (Con_b (p,cb)::rest,l) = gv(rest,(get_conbnd_var cb)::l)
	| gv (Exp_b (v,e)::rest,l) = gv(rest,v::l)
	| gv (Fixopen_b(vfs)::rest,l) = gv(rest,(map (fn (a,b) => a) (Sequence.toList vfs))@l)
	| gv (Fixcode_b(vfs)::rest,l) = gv(rest,(map (fn (a,b) => a) (Sequence.toList vfs))@l)
	| gv (Fixclosure_b(_)::rest,l) = raise HoistError
    in
      gv (bnd_list,[])
    end

  (* start junk *)
 fun cat_app_zip l = foldr (fn ((a,b),(c,d)) => (a::c,b@d)) ([],[]) l
 fun cat_app_zip' (a,b) = foldr (fn ((a,b),(c,d)) => (a::c,b@d)) ([],[]) (ListPair.zip (a,b))

(*
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
*)    
  (* stop junk *)

  fun rconbnd cvs (Con_cb(v,con)) : conbndtype * bvs list = 
    let
      val (con',bvl) = rcon(con,cvs)
      val freev = freeConVars con'
    in
      case subset (freev,cvs) of
	true => (CUP v, bvl@[(Con_b (Runtime,Con_cb(v,con')),freev)])
      | false => (CSTAY (Con_cb (v,con')), bvl)
    end

    (*** should these move? *)
    (*** if this is changed (ie to hoist open_cbs) then code_cb wont work *)
    | rconbnd cvs (Open_cb(v,vklist,con',kind')) = 
	let
	  val l = map (fn (a,b) => (a,rkind(b,cvs))) vklist
	  val (vkl,bvl) = foldr (fn ((a,(b,c)),(d,e)) => ((a,b)::d,c@e)) ([],[]) l
	  val (con'',bvl') = rcon(con',cvs)
	  val (kind'',bvl'') = rkind(kind',cvs)
	in
	  (CSTAY(Open_cb(v,vkl,con'',kind'')),bvl@bvl'@bvl'')
	end

    | rconbnd cvs (Code_cb goo) = 
	let
	  val (CSTAY(Open_cb goo'),bvl) = rconbnd cvs (Open_cb goo)
	in
	  (CSTAY(Code_cb goo'),bvl)
	end

  (* bvsll is inorder,inorder and conbnd list is inorder *)
  and rconbnds (conbnd,cvs:set) : (bvs list list * conbnd list) = 
    let
      fun loop ([],cvs,(up,stay)) = (rev up,rev stay)
	| loop (cb::rest,cvs,(up,stay)) = 
	case rconbnd cvs cb of 
	  (CSTAY cb',bvl) => loop (rest,cvs,(bvl::up,cb'::stay))
	| (CUP v, bvl) => loop(rest,add(cvs,v),(bvl::up,stay))
    in
      loop (conbnd,cvs,([[]],[]))
    end

  (*** rewrite kinds at all? *)
  and rkind (k,cvs:set) = (k,[])

  and rcon (Prim_c (primcon,conlist),cvs) : (con * bvs list) = 
    let
      val (conlist', bvl) = cat_app_zip (map (fn a => rcon (a,cvs)) conlist)
    in
      (Prim_c(primcon,conlist'),bvl)
    end

    | rcon (Mu_c (bool',vcseq),cvs) = 
    let
      val vclist = Sequence.toList vcseq
      val vclist' = map (fn (a,b) => (a,rcon(b,cvs))) vclist
      val l = map (fn (a,(b,c)) => ((a,b),c)) vclist'
      val (vclist'',bvll) = ListPair.unzip l
    in
      (Mu_c (bool',Sequence.fromList vclist''),List.concat bvll)
    end

    (*** what to do with this? *)
    | rcon (AllArrow_c (openness,effect,vkl,vlist,conlist,w32,con),cvs) = 
    let
      val (conlist',bvl) = cat_app_zip (map (fn a => rcon (a,cvs)) conlist)
      val (con',bvl') = rcon (con,cvs)
    in
      (AllArrow_c(openness,effect,vkl,vlist,conlist',w32,con'),bvl@bvl')
    end

    | rcon (ExternArrow_c (conlist,con),cvs) = 
    let
      val (conlist',bvl) = cat_app_zip (map (fn a => rcon (a,cvs)) conlist)
      val (con',bvl') = rcon (con,cvs)
    in
      (ExternArrow_c(conlist',con'),bvl@bvl')
    end

    | rcon (Var_c(var'),cvs) = (Var_c(var'),[])

    | rcon (Let_c(letsort,bndlist,bodcon),cvs) = 
    let
      val (up,stay) = rconbnds(bndlist,cvs) 

      (* collect all the bnd vars that are const *)
      val cvs' = union(cvs,list2set (List.concat (map getBoundVars (map (fn bvl => (map #1 bvl)) up))))

      val (bodcon',up') = rcon (bodcon,cvs') 

      val up'' : bvs list = (List.concat up) @ up'
    in
      (Let_c(letsort,stay,bodcon'),up'')
    end

    | rcon (Crecord_c lclist, cvs) = 
    let
      val lclist' = map (fn (l,con') => (l,rcon(con',cvs))) lclist
      val (lclist'',bvl) = foldr (fn ((l,(con',bvl)),(cvl',bvl')) => ((l,con')::cvl',bvl@bvl')) ([],[]) lclist'
    in
      (Crecord_c lclist'',bvl)
    end

    | rcon (Proj_c (con,lab),cvs) = 
    let
      val (con',bvl) = rcon (con,cvs)
    in
      (Proj_c(con',lab),bvl)
    end

    | rcon (Typeof_c e,cvs) = 
    let
      val (e',bvl) = rexp (e,cvs)
    in
      (Typeof_c e',bvl)
    end

    | rcon (Closure_c(codecon,envcon),cvs) = 
    let
      val (codecon',bvl) = rcon (codecon,cvs)
      val (envcon',bvl') = rcon (envcon,cvs)
    in
      (Closure_c(codecon',envcon'),bvl@bvl')
    end

    | rcon (App_c(con,conlist),cvs) = 
    let
      val (con',bvl) = rcon (con,cvs)
      val conlist' = map (fn a => rcon(a,cvs)) conlist
      val (conlist'',bvl') = cat_app_zip conlist'
    in
      (App_c(con',conlist''),bvl@bvl')
    end

    (**** clean me till i shine *)
    | rcon (Typecase_c {arg,arms,default,kind},cvs) = 
    let
      fun proc (primc,vkl,con) =
	let
	  val m = map (fn (v,k) => 
		       let 
			 val (k',bvl) = rkind (k,cvs) 
		       in 
			 ((v,k'),bvl)
		       end) vkl
	    
	  val (vkl',bvl) = ListPair.unzip m
	  val (con',bvl') = rcon (con,cvs)
	in
	  ((primc,vkl',con'),(List.concat bvl)@bvl')
	end
      
      val (arg',bvl:bvs list) = rcon (arg,cvs)
      val (default',bvl':bvs list) = rcon (default,cvs)	
      val (arms',bvl'') = ListPair.unzip (map proc arms)
      val bvl'' = List.concat bvl''
      val (kind',bvl''':bvs list) = rkind (kind,cvs)
      val a = (bvl @ bvl') @ bvl''
    in
      (Typecase_c {arg=arg',arms=arms',default=default',kind=kind'},(bvl@bvl'@bvl''@bvl'''))
    end

    | rcon (Annotate_c (annot,con),cvs) = 
    let
      val (con',bvl) = rcon (con,cvs)
    in
      (Annotate_c (annot,con'),[])
    end

  and rbnds (bnd_list,cvs) : (bvs list list * bnd list) = 
    let
      fun loop ([],cvs,(up,stay)) (*: bvs list * bnd list *)= (rev up,rev stay)
	| loop (b::rest,cvs,(up,stay)) = 
	  case rbnd cvs b of 
	    (STAY b,bvsl) => loop (rest,cvs,(bvsl::up,b::stay))
	  | (UP v, bvsl) => loop (rest,add(cvs,v),(bvsl::up,stay))
    in
      loop (bnd_list,cvs,([],[]))
    end

  and rbnd cvs (Con_b(p,cb)) : (bndtype * bvs list) = 
    let
      val v = get_conbnd_var cb
(*
      val vstr = var2string v 
      val _ = (pprint ("rewriting bnd for con var: "^vstr^"\n");ppin 2) 
      val _ = ppout 2 
*)
    in
      case rconbnd cvs cb of
	(CUP v,bvl) => (UP v,bvl)
      | (CSTAY cb', bvl) => (STAY (Con_b (p,cb')),bvl)
    end

    | rbnd cvs (Exp_b(v,e)) = 
    let
(*
      val vstr = var2string v
      val _ = (pprint ("rewriting bnd for exp var: "^vstr^"\n");ppin 2)  
*)
      val (e',bvl') = rexp(e,cvs)
      val fv = freeExpVars e
      (* hack, can't have floats at top-level *)
      val up = subset(fv,cvs) andalso (not (NilUtil.effect e))
	                      andalso (case e of
					   Prim_e(NilPrimOp(unbox_float _),_,_) => false
					 | _ => true)
(*
      val _ = if up then pprint ("E-moving "^vstr^" up\n") 
      	      else pprint ("E-keeping "^vstr^" here\n") 
      val _ = ppout 2 
*)
      val newbnd = Exp_b(v,e')
    in
      if up then (UP v,bvl'@[(newbnd,fv)])
      else (STAY newbnd,bvl')
    end

    (*** need to do more here? pass in fvars as being constant? *)
    | rbnd cvs (Fixcode_b(vfs)) = 
	let
(*
	  val fvstr = (sl2s (map var2string (map #1 vfs)))
	  val _ = (pprint ("rewriting bnd for code funs: "^fvstr^"\n");ppin 2) 
*)
	  fun loop ([],vfl,bvl') = (vfl,bvl')

	    | loop ((v,f)::rest,vfl,bvl') = 
	    let
	      val (f',bvl'') = rfun (f,cvs)
	    in
	      loop (rest,(v,f')::vfl,bvl''@bvl')
	    end

	  val (vfs',bvl''') = loop (Sequence.toList vfs,[],[])
(*	  val _ = ppout 2  *)
	in
	  (STAY(Fixcode_b(Sequence.fromList vfs')),bvl''')
	end

    (*** need to do more here? pass in fvars as being constant? *)
    | rbnd cvs (Fixopen_b(vfs)) = 
	let
(*
	  val fvstr = (sl2s (map var2string (map #1 vfs)))
	  val _ = (pprint ("rewriting bnd for open funs: "^fvstr^"\n");ppin 2) 
*)

	  fun loop ([],vfl,bvl') = (vfl,bvl')

	    | loop ((v,f)::rest,vfl,bvl') = 
	    let
	      val (f',bvl'') = rfun (f,cvs)
	    in
	      loop (rest,(v,f')::vfl,bvl''@bvl')
	    end

	  val (vfs',bvl''') = loop (Sequence.toList vfs,[],[])
(*	  val _ = ppout 2  *)
	in
	  (STAY(Fixopen_b(Sequence.fromList vfs')),bvl''')
	end

    | rbnd _ _ = raise HoistError

  and rexp (Var_e v,cvs) = (Var_e v,[])

    | rexp (Const_e c,cvs) = (Const_e c, [])

    | rexp (Let_e (seq, bndlst, bodexp),cvs) = 
    let
(*
      val t = newtag "let" 
      val _ = (plist ["start",t];ppin(3)) 
*)
      val fvset = list2set (getBoundVars bndlst)

      val u = union (fvset,cvs)

      val (bodexp',bvl) = rexp(bodexp,u)
      val (bvll',bndlst') = rbnds(bndlst,cvs)
(*
      val _ = diag ("bndlst: "^(bl2s bndlst)^"\n") 
      val _ = diag ("bvl': "^bvl2s (List.concat bvll')^", bndlst': "^(bl2s bndlst')^"\n") 
*)
      val (up,stay) = splitbnds (bvl@(List.concat bvll'),cvs)
      val stay' = bndlst'@(stay)

(*
      val _ = pprint ("laying down: "^(bl2s stay')^"\n") 
      val _ = pprint ("moving up: "^(bl2s (map #1  up)^"\n"))  
      val _ = (ppout 3;plist ["end",t]) 
*)
    in
      (NilUtil.makeLetE seq stay' bodexp',up)
    end

    | rexp (Prim_e (allp, conlst, explst),cvs) = 
    let
      val (nconlst,bvl)  = foldr (fn ((c,bvl),(cl,bvll)) => (c::cl,bvl@bvll)) ([],[]) (map (fn c => rcon(c,cvs)) conlst)
      val (nexplst,bvl') = foldr (fn ((e,bvl),(el,bvll)) => (e::el,bvl@bvll)) ([],[]) (map (fn e => rexp(e,cvs)) explst)
    in
      (Prim_e (allp, nconlst, nexplst),bvl@bvl')
    end

    | rexp (Switch_e s,cvs) = 
    let
      val (ns,bvl) = rswitch (s,cvs)
    in
      (Switch_e ns,bvl)
    end

    | rexp (App_e (opn,exp,conlist, explist, explist'),cvs) = 
    let
      val (nexp,bvl) = rexp (exp,cvs)
      val (nconlist,bvl') = cat_app_zip (map (fn a => rcon(a,cvs)) conlist)
      val (nexplist,bvl'') = cat_app_zip (map (fn a => rexp(a,cvs)) explist)
      val (nexplist',bvl''') = cat_app_zip (map (fn a => rexp(a,cvs)) explist')
      val bvl = bvl@bvl'@bvl''@bvl'''
    in
      (App_e (opn, nexp,nconlist, nexplist, nexplist'),bvl)
    end

    | rexp (ExternApp_e (exp,explist),cvs) = 
    let
      val (nexp,bvl) = rexp (exp,cvs)
      val (nexplist,bvl') = cat_app_zip (map (fn a => rexp(a,cvs)) explist)
      val bvl = bvl@bvl'
    in
      (ExternApp_e (nexp,nexplist),bvl)
    end

    | rexp (Raise_e(exp,con),cvs) = 
    let
      val (nexp,bvl) = rexp (exp,cvs)
      val (ncon,bvl') = rcon (con,cvs)
    in
      (Raise_e(nexp,ncon),bvl@bvl')
    end

    | rexp (Handle_e (exp,var,exp'),cvs) = 
    let
      val (nexp,bvl) = rexp (exp,cvs)
      val (nexp',bvl') = rexp (exp',cvs)
    in
      (Handle_e (nexp,var,nexp'),bvl@bvl')
    end

  and rswitch (Intsw_e {arg,size,arms,default},cvs) = 
    let

      fun procarm(arms,tprocfun,cvs) = 
	let
	  val l = map (fn (a,b) => (tprocfun (a,cvs),rexp (b,cvs))) arms
	  val f = fn (((a,b),(c,d)),(e,f)) => ((a,c)::e,b@d@f)
	in
	  foldr f ([],[]) l
	end

      val (arg',bvl') = rexp (arg,cvs)
      val (arms',bvl'') = procarm(arms,fn (x,b:set) => (x,[]),cvs)
      val (default',bvl''') = rexpopt(default,cvs)

    in
      (Intsw_e {arg=arg',
		size=size,
		arms=arms',
		default=default'},
       bvl'@bvl''@bvl''')
    end

    | rswitch (Sumsw_e {arg,sumtype,bound,arms,default},cvs) = 
    let

      fun procarm(arms,tprocfun,cvs) = 
	let
	  val l = map (fn (a,b) => (tprocfun (a,cvs),rexp (b,cvs))) arms
	  val f = fn (((a,b),(c,d)),(e,f)) => ((a,c)::e,b@d@f)
	in
	  foldr f ([],[]) l
	end

      val (arg',bvl') = rexp (arg,cvs)
      val (sumtype',bvl'') = rcon (sumtype,cvs)
      val (arms',bvl''') = procarm(arms,fn (x,b) => (x,[]),cvs)
      val (default',bvl'''') = rexpopt(default,cvs)
    in
      (Sumsw_e {arg = arg',
		sumtype = sumtype',
		bound = bound,
		arms = arms',
		default = default'},
       bvl'@bvl''@bvl'''@bvl'''')
    end

    | rswitch (Exncase_e {arg,bound,arms,default},cvs) = 
    let
      val (arg',bvl') = rexp (arg,cvs)

      fun procarm(arms,tprocfun,cvs) = 
	let
	  val l = map (fn (a,b) => (tprocfun (a,cvs),rexp (b,cvs))) arms
	  val f = fn (((a,b),(c,d)),(e,f)) => ((a,c)::e,b@d@f)
	in
	  foldr f ([],[]) l
	end 

      val (arms',bvl') = procarm(arms,rexp,cvs)

      val (default',bvl'') = rexpopt(default,cvs)
    in
      (Exncase_e {arg=arg',
		  bound=bound,
		  arms=arms',
		  default=default'},
       bvl'@bvl'')
    end

    | rswitch (Typecase_e {arg,arms,default},cvs) = 
    let
      val (arg',bvl') = rcon (arg,cvs)

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

  and rexpopt(NONE,cvs) = (NONE,[])
    | rexpopt(SOME(a),cvs) = let val (e',bvl) = rexp (a,cvs) in (SOME(e'),bvl) end

  and rfun (Function(eff,isrec,typelist:(var * kind) list,
		     vlist,formlist,vl,bod,ret),cvs:set) = 
    let
(*
      val t = newtag "fun" 
      val _ = (plist ["start",t];ppin(3)) 
*)
      val cvl = map (fn (a,b) => a) typelist
      val cvl' = map (fn (a,b) => a) formlist
      val cvs' = cvs
      val (bod',bvl) = rexp(bod,cvs')
      val (ret',bvl') = rcon (ret,cvs')
      val (up,stay) = splitbnds(bvl@bvl',cvs)
      val newbod = NilUtil.makeLetE Sequential stay bod'
(*
      val _ = pprint ("laying down at fun: "^(bl2s stay)^"\n") 
      val _ = ppout 3 
      val _ = plist ["end",t]
*)
    in
      (Function(eff,isrec,typelist,vlist,formlist,vl,newbod,ret'),up) 
    end

  and splitbnds (bvl:bvs list,cvs:set) : (bvs list * bnd list) = 
    let
(*      val _ = diag ("splitbnds("^(Int.toString (length bvl))^" start\n")  *)
      (*** both u and s should be reversed. find out where the other rev is happening *)
      fun loop ([],cvs,(u,s)) = (rev u,rev s)

	| loop ((b,vs:set)::rest,cvs,(u,s)) = 
	let
(*
	  val bname = var2string (hd (getBoundVars [b])) 
	  val vsname = sl2s (map var2string (set2list vs)) 
	  val cvsname = sl2s (map var2string (set2list cvs)) 
	  val _ = diag ("bnd: "^bname^", vs: "^vsname^", cvs: "^cvsname^"\n") 
*)
	in
	  if subset (vs,cvs) 
	    then loop(rest,union (cvs, list2set (getBoundVars([b]))),((b,vs)::u,s))
	  else loop(rest,cvs,(u,b::s))
	end
      val (u,s) = loop (bvl,cvs,([],[]))
(*      val _ = diag ("splitting: "^(bvsl2s bvl)^" into up: "^(bvsl2s u)^
                       " and stay: "^(bl2s s)^"\n") 
*)
    in
      (u,s)
    end

  fun optimize (MODULE {bnds, imports, exports}) = 
    let
(*    val _ = diag("we are hoisting\n") *)

      (* remove the bindings from empty lets *)
      fun stripLet (Let_e(_,bnds,bod)) = bnds@(stripLet bod)
	| stripLet _ = []
    
      (* collect the 'top level' var names *)
      fun split ([],vl,tl,cvs) = (vl,tl,cvs)
	| split (ImportValue(l,v,c)::rest,vl,tl,cvs) = split(rest,(v,c)::vl,tl,add(cvs,v))
	| split (ImportType(l,v,k)::rest,vl,tl,cvs) = split(rest,vl,(v,k)::tl,add(cvs,v))

      val (exp_args,type_args,cvs) = split (imports,[],[],empty)

      val lete = Let_e (Sequential,bnds,NilUtil.unit_exp)

      (******** check 3rd and 4th args *)
      val startfun = Function(Total,Leaf,type_args,
			      false,exp_args,[],lete,NilUtil.unit_con)

      val (f,bvl) = rfun(startfun,cvs)

      val Function(_,_,_,_,_,_,some_let,_) = f

      val bnds' = stripLet some_let

      val bnds'' = (map #1 bvl)@bnds'

    in
      MODULE {bnds=bnds'',imports=imports,exports=exports}  
    end

end






