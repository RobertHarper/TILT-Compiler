structure LilContext
   :> LILCONTEXT = 
 struct


   (* IMPORTS *)

   open Lil 
   open Prim

   structure LD = LilDefs
   structure LU = LilUtil
   structure LO = Listops
   structure LS = LilSubst
   structure Dec = Deconstruct.Dec
   structure Elim = Deconstruct.Elim

   (* Stats **********************************************)

   fun error s = Util.error "lilcontext.sml" s

   structure V = Name.VarMap
   structure L = Name.LabelMap
   structure IM = IntBinaryMap

   (*END OF IMPORTS     *)


   exception Unbound of string
   exception Rebound of string

   val () = 
     let
       fun print_exn (Unbound s) = SOME s
	 | print_exn (Rebound s) = SOME s
	 | print_exn _ = NONE
     in UtilError.register_exn_printer print_exn
     end

   fun unbound (s : string) : 'a = raise (Unbound ("lilcontext.sml:Unbound var/label "^s))

   datatype parity = Pos | Neg | Any
   
   (* Extensive use of vcase makes contexts substitutions a *huge*
    * bottleneck.  Note that without a special context representation
    * we have to copy the whole damn thing every time.  This version
    * attempts to alleviate this by subsituting lazily on the way out 
    * of the context.  Note: no memoization yet.
    *
    * With every type, we keep the index of the first substitution
    * that applies to it.  
    *
    * numItems(substs) is the implicit current index.  When we insert a
    * new type, we give it this index + 1, since only subsequent substitutions
    * will apply to it.
    *
    * subst[i] contains all of the substitutions made since i was the current 
    * index, indexed from 0 (so subst[0] contains all substitutions).
    *)
   type context = 
     {kvars   : parity V.map,
      cvars   : kind V.map,
      vars32  : (con * int) V.map,
      vars64  : (con * int) V.map,
      labels  : (con * int) L.map,
      index   : int,
      substs  : LS.con_subst list}

   (* VarMap utilities ******************)
   fun Vfind (map,v) = V.find(map,v)

   fun Vcontains map var = Option.isSome (Vfind (map,var))

   fun Vinsert (map,v,value) = 
       if Vcontains map v then 
	 (print ("Variable already occurs in context: "^(Name.var2string v)^"\n");
	  raise Rebound (Name.var2string v))
       else V.insert (map,v,value)

   fun Vinsert_list (map,l) = List.foldl (fn ((v,value),map) => Vinsert (map,v,value)) map l

   (* LabelMap utilities ******************)
   fun Lfind (map,l) = L.find(map,l)

   fun Lcontains map lbl = Option.isSome (Lfind (map,lbl))

   fun Linsert (map,l,value) = 
       if Lcontains map l then 
	 (print ("Label already occurs in context: "^(Name.label2string l)^"\n");
	  raise Rebound (Name.label2string l))
       else L.insert (map,l,value)

   fun Linsert_list (map,l) = List.foldl (fn ((lbl,value),map) => Linsert (map,lbl,value)) map l

   (****** map delay helpers  ******)

   (********** Main Functions ********************************)

   (* Empty context 
    *)
   fun empty () : context = 
     {kvars = V.empty, cvars = V.empty, vars32 = V.empty, vars64 = V.empty,labels = L.empty,index = 0,substs = [LS.C.empty()]}


   (* Is a given variable already bound? 
    *)
   fun bound_cvar (ctx as {cvars,...}:context,var)    = Vcontains cvars var
   fun bound_var32 (ctx as {vars32,...} :context,var) = Vcontains vars32 var
   fun bound_var64 (ctx as {vars64,...} :context,var) = Vcontains vars64 var

   (*****Term level functions. ******)

   (* Adding a new variable.  Shift the substitution index by 1.
    * If the current substitution is empty we can avoid shifting.
    *)
   fun shift (i,substs) = if LS.C.is_empty (hd substs) then (i,substs) else (i+1,(LS.C.empty())::substs)
     
   fun do_subst(index,substs,(c,i)) = 
     let
       val subst = List.nth (substs,index - i)
     in LS.substConInCon subst c
     end

   fun varConConSubst a c substs = List.map (fn s => LS.C.addl(a,c,s)) substs

   fun bind_var32 (ctx as {vars32,cvars,kvars,vars64,labels,index,substs}:context,(var,con:con)) :context= 
     let
       val (index,substs) = shift (index,substs)
     in
       {vars32 = Vinsert(vars32,var,(con,index)),
	vars64 = vars64,
	labels = labels,
	kvars = kvars,
	cvars = cvars,
	index = index,
	substs = substs}
     end

   fun bind_var32s (ctx :context,vcs : (var * con) list) :context = 
     foldl (fn (ac,ctx) => bind_var32 (ctx,ac)) ctx vcs

   fun find_var32 ({vars32,index,substs,...}:context,var) = 
     (case Vfind(vars32, var) of
	SOME ci => do_subst(index,substs,ci)
      | NONE => unbound (Name.var2string var))
	
   fun bind_label (ctx as {vars32,cvars,kvars,vars64,labels,index,substs}:context,(lbl,con:con)) :context= 
     let
       val (index,substs) = shift (index,substs)
     in
       {vars32 = vars32,
	vars64 = vars64,
	labels = Linsert(labels, lbl, (con,index)), 
	kvars = kvars,
	cvars = cvars,
	index = index,
	substs = substs}
     end

   fun bind_labels (ctx :context,lcs) :context= 
     foldl (fn (lc,ctx) => bind_label (ctx,lc)) ctx lcs
     
   fun find_label ({labels,index,substs,...}:context,lbl) = 
     (case Lfind (labels, lbl) of
	SOME cs => do_subst(index,substs,cs)
      | NONE => unbound (Name.label2string lbl))


   fun bind_var64 (ctx as {vars32,cvars,kvars,vars64,labels,index,substs}:context,(var,con:con)) :context= 
     let
       val (index,substs) = shift (index,substs)
     in
       {vars32 = vars32,
	vars64 = Vinsert (vars64, var, (con,index)), 
	labels = labels,
	kvars = kvars,
	cvars = cvars,
	index = index,
	substs = substs}
     end

   fun bind_var64s (ctx:context,vcs : (var * con) list) :context= 
     foldl (fn (ac,ctx) => bind_var64 (ctx,ac)) ctx vcs
     
     
   fun find_var64 ({vars64,index,substs,...}:context,var) = 
     (case Vfind (vars64, var) of
	SOME cs => do_subst(index,substs,cs)
      | NONE => unbound (Name.var2string var))
	

   (**** Type level functions *****)
   fun bind_cvar (ctx as {vars32,cvars,kvars,vars64,labels,index,substs}:context,(v,k) : (var* kind) ) :context= 
     let
     in
       {vars64 = vars64,
	labels = labels,
	vars32 = vars32,
	kvars = kvars,
	cvars = Vinsert (cvars, v, k),
	index = index,
	substs = substs}
     end

   (**** Type level functions *****)
   fun bind_cvars (ctx as {vars32,cvars,kvars,vars64,labels,index,substs}:context,vks : (var* kind) list) :context= 
     let
     in
       {vars64 = vars64,
	labels = labels,
	vars32 = vars32,
	kvars = kvars,
	cvars = Vinsert_list (cvars, vks),
	index = index,
	substs = substs}
     end


   fun find_cvar ({cvars,...}:context,var) = 
     (case Vfind (cvars, var) of
	SOME kind => kind
      | NONE => unbound (Name.var2string var))



   (**** Kind level functions *****)
   fun bind_kvar (ctx as {vars32,cvars,kvars,vars64,labels,index,substs}:context,var,parity) :context= 
     let
     in
       {vars64 = vars64,
	labels = labels,
	vars32 = vars32,
	cvars = cvars,
	kvars = Vinsert (kvars, var, parity),
	index = index,
	substs = substs}
     end


   fun bind_kvars (ctx as {vars32,cvars,kvars,vars64,labels,index,substs}:context,vs : var list,parity) :context = 
     let
     in
       {vars64 = vars64,
	labels = labels,
	vars32 = vars32,
	cvars = cvars,
	kvars = Vinsert_list (kvars, map (fn v => (v,parity)) vs),
	index = index,
	substs = substs}
     end


   fun find_kvar ({kvars,...}:context,var) = 
     (case Vfind (kvars, var)
	of SOME p => p
	 | NONE => unbound (Name.var2string var))


   (* replace (D1,a::k,D2,G) a aks c => (D1,aks,D2,G[c/a]) *)
   fun replace (ctx as {vars32,vars64,labels,cvars,kvars,index,substs} : context,a,aks,c) = 
     let
       val (cvars,_) = V.remove (cvars,a)
       val cvars = Vinsert_list(cvars,aks)
       val substs = varConConSubst a c substs
     in
       ({vars64 = vars64,
	 labels = labels,
	 vars32 = vars32,
	 cvars = cvars,
	 kvars = kvars,
	 index = index,
	 substs = substs},LS.C.simFromList [(a,c)])
     end

   fun unfold_cvar (ctx,(a,b)) = 
     let
       val k = find_cvar (ctx,a)
       val c = LD.C.fold k (mk_con (Var_c b))
       val k = Elim.K.unfold k
     in replace (ctx,a,[(b,k)],c)
     end

   fun split_cvar (ctx,(a,(b,g))) = 
     let
       val k = find_cvar (ctx,a)
       val c = LD.C.pair (mk_con (Var_c b)) (mk_con (Var_c g))
       val (k1,k2) = Dec.K.pair k
     in replace (ctx,a,[(b,k1),(g,k2)],c)
     end

   fun vcase_cvar (ctx,(a,b)) = 
     let
       val k = find_cvar (ctx,a)
       val ks = (Dec.K.sum k) handle _ => error "vcase_var not of sum kind"
       val cs = LO.mapcount (fn (i,ki) => LD.C.inj (LU.i2w i) k (mk_con (Var_c b))) ks

       val ctxts = LO.map2 (fn (ki,ci) => replace (ctx,a,[(b,ki)],ci)) (ks,cs)
     in ctxts
     end

   fun clear_vars ({vars32,cvars,kvars,vars64,labels,index,substs} : context) : context = 
     {kvars = kvars, cvars = cvars, vars32 = V.empty, vars64 = V.empty,labels = labels,index = index,substs = substs}

 end

