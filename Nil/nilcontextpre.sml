(*$import Stats Option Ppnil NilSubst NilUtil ListMergeSort NILCONTEXTPRE *)

structure NilContextPre
   :> NILCONTEXTPRE = 
 struct


   open Nil 
   open Prim


   (* IMPORTS *)
   val profile = Stats.ff "nil_profile"
   val local_profile = Stats.ff "nilcontext_profile"

   val timer = Stats.subtimer'
   val subtimer = fn args => fn args2 => if !profile orelse !local_profile then Stats.subtimer' args args2 else #2 args args2

   val do_selfify = Stats.ff "nilcontext:do_selfify"
     
   val substConInKind = fn s => subtimer("Ctx:substConInKind",NilSubst.substConInKind s)
   val substConInCon  = fn s => subtimer("Ctx:substConInCon", NilSubst.substConInCon s)

   val add = NilSubst.C.sim_add
   val addr = NilSubst.C.addr
   val varConKindSubst = NilSubst.varConKindSubst
   val empty_subst = NilSubst.C.empty

   val var2string = Name.var2string

   val eq_label = Name.eq_label
   val fresh_named_var = Name.fresh_named_var
   val eq_var = Name.eq_var
   val derived_var = Name.derived_var

   val zip = Listops.zip
   val unzip = Listops.unzip
   val map2 = Listops.map2
   val split = Listops.split
   val map_second = Listops.map_second
   val foldl_acc = Listops.foldl_acc
   val foldl2 = Listops.foldl2
   val foldl3 = Listops.foldl3

   val curry2 = Util.curry2
   val printl = Util.printl
   val lprintl = Util.lprintl
   val lprint = Util.lprint
   val eq_opt = Util.eq_opt
   val error = Util.error
   val mapopt = Util.mapopt

   val locate = NilError.locate  "nilcontext.sml"
   val assert = NilError.assert

   fun error s s' = Util.error s s'

   val c_all = NilError.c_all
   val perr_k_k = NilError.perr_k_k
   val perr_c_k_k = NilError.perr_c_k_k

   val strip_arrow = NilUtil.strip_arrow
   val strip_var = NilUtil.strip_var
   val generate_tuple_label = NilUtil.generate_tuple_label
   val selfify = subtimer("Ctx:selfify",NilUtil.selfify)
   val project_from_kind_nondep = NilUtil.project_from_kind_nondep


   (*END OF IMPORTS     *)

   val profile = Stats.ff "nil_profile"
   val debug = Stats.ff "nil_debug"
   val eager = Stats.ff "nil_eager"
   val memoize = Stats.tt "nilcontext_memoize"
   val transitive = Stats.tt "nilcontext_transitive"
   val found_trans = Stats.counter "nilcontext_found_trans"

   val print_std_kinds = ref false
   val print_std_cons = ref false

   exception Unimplemented

   structure V = Name.VarMap

   type 'a map = 'a V.map

   exception Unbound

   (*Possibly uncomputed data.*)
   datatype 'a thunk = FROZEN of (unit -> 'a) | THAWED of 'a
   type 'a delay = 'a thunk ref

   fun delay thunk = ref(if (!eager)
			 then THAWED (thunk())
			 else FROZEN thunk)
   fun immediate value = ref (THAWED value)
   fun thaw (ref (THAWED v)) = v
     | thaw (r as ref(FROZEN t)) = let val v = t()
				       val _ = if !memoize then r := (THAWED v) else ()
				   in  v
				   end
   fun delayed (ref (FROZEN _)) = true
     | delayed (ref (THAWED _)) = false


   fun valOf str NONE = (print "valOf failed at "; print str; print "\n";
			 raise Option.Option)
     | valOf str (SOME x) = x


   type k_entry = {eqn: con option,
		   kind : kind,
		   std_kind : kind delay,
		   max_kind : kind delay,
		   index : int}
     
   type c_entry = {con : con delay,
		   std_con : con delay}

   type context = 
     {kindmap : k_entry map,
      conmap  : c_entry map,
      counter : int}

   fun empty () : context = 
     {kindmap = V.empty,
      conmap = V.empty,
      counter = 0}


   fun contains map var = Option.isSome (V.find (map,var))

   (*Adding a function to Varmap to avoid the double call only saves 
    * about 1/12 of the total time.  I think Splay Maps make repeated
    * splays on the same value cheap.
    *)
   fun Vinsert (map,v,value) = 
       if contains map v then 
	 error (locate "Vinsert") ("Constructor variable already occurs in context: "^(Name.var2string v))
       else V.insert (map,v,value)

   val Vinsert = fn args => subtimer("Ctx:Vinsert",Vinsert) args

   (**** Printing functions *****)
   local
     fun print_con (var,{con,std_con}:c_entry) =
       (print (Name.var2string var);
	print ":";
	Ppnil.pp_con (thaw con);
	if (!print_std_cons) then 
	  (lprint ":";
	   Ppnil.pp_con (thaw std_con))
	else
	  print "\n")
       
     fun print_entry (var,{eqn,kind,std_kind,max_kind,index}:k_entry) =
       (print (Name.var2string var);
	(case eqn
	   of SOME c => (print " = ";
			 Ppnil.pp_con c)
	    | NONE => ());
	   print "::";
	   Ppnil.pp_kind kind;
	   if (!print_std_kinds) then
	     (lprint "::";
	      Ppnil.pp_kind (thaw std_kind))
	   else 
	     print "\n")
       
     fun lt ((_,{index=a,...}:k_entry),(_,{index=b,...}:k_entry)) = a < b

   in
     fun print_kinds ({kindmap,...}:context) = 
       (print "\n Constructor variables, kinds, and equations are :\n";
	List.app print_entry (ListMergeSort.sort lt (V.listItemsi kindmap)))
       
     fun print_cons ({conmap,...}:context) = 
	(print "\n Expression variables and constructors are :\n";
	 V.appi print_con conmap)

     fun print_context (context:context) = 
	 (print_kinds context;
	  print_cons context)
	 
     fun print_context' (context:context) = 
	 (print_kinds context;
	  print_cons context)
   end

   local
     type 'item renamer = (var -> bool) * (var -> bool) -> 'item -> bool
     fun isRenamedXXX (isRenamedXXXWRT : 'item renamer) ({kindmap,conmap,counter} : context) (item : 'item) = 
       isRenamedXXXWRT (contains conmap,contains kindmap) item
   in
     val isRenamedExp  = isRenamedXXX NilRename.isRenamedExpWRT
     val isRenamedCon  = isRenamedXXX NilRename.isRenamedConWRT
     val isRenamedKind = isRenamedXXX NilRename.isRenamedKindWRT
   end
   (*****Term level context functions. ******)

   fun insert_con (ctx as {conmap,kindmap,counter}:context,var,con) = 
     let
       val _ = 
	 if !debug then
	   assert (locate "insert_con") []
	 else ()
       val thunk = fn () => raise Unimplemented (*type_standardize(ctx,con)*)
       val c_entry = 
	 {con = immediate con,
	  std_con = delay thunk}
     in
       {conmap = Vinsert (conmap, var, c_entry), 
	kindmap = kindmap,
	counter = counter}
     end

   fun insert_exp_pre (typeof) (ctx as {conmap,kindmap,counter}:context,var,con) = 
     let
       val _ = 
	 if !debug then
	   assert (locate "insert_con") []
	 else ()

       val cthunk = fn () => typeof(ctx,con)
       val sthunk = fn () => raise Unimplemented (*type_standardize(ctx,con)*)
       val c_entry = 
	 {con = delay cthunk,
	  std_con = delay sthunk}
     in
       {conmap = Vinsert (conmap, var, c_entry), 
	kindmap = kindmap,
	counter = counter}
     end

   fun insert_con_list (C:context,defs : (var * con) list) =
     List.foldl (fn ((v,c),C) => insert_con (C,v,c)) C defs

   fun find_con ({conmap,...}:context,var) = 
       (case V.find (conmap, var) of
	    SOME {con,std_con} => thaw con
	  | NONE => raise Unbound)


   fun find_std_con_pre normalize (D as {conmap,...}:context,var) = 
       (case V.find (conmap, var) of
	    SOME {con,std_con} => (std_con := (THAWED (normalize (D,thaw con)));
				   thaw std_con)
	  | NONE => raise Unbound)

   (*****Constructor level context functions. ******)

   fun vprint v = (lprintl (var2string v);false)
   fun allBound_c kindmap con = c_all (contains kindmap) vprint  (NilUtil.freeConVarInCon (true,con))
   fun allBound_k kindmap kind = c_all (contains kindmap) vprint (NilUtil.freeConVarInKind kind)

   fun find_std_kind (context as {kindmap,...}:context,var) = 
     (case (V.find (kindmap, var)) of
	   SOME {std_kind,...} => thaw std_kind
	 | NONE => raise Unbound)

   fun find_max_kind (context as {kindmap,...}:context,var) = 
     (case (V.find (kindmap, var)) of
	   SOME {max_kind,...} => thaw max_kind
	 | NONE => raise Unbound)

   fun find_kind (context as {kindmap,...}:context,var) = 
     (case (V.find (kindmap, var)) of
	   SOME {kind,...} => kind
	 | NONE => raise Unbound)


   fun name_proj (SOME name,l) = SOME (Proj_c(name,l))
     | name_proj (NONE,_)      = NONE
     
   fun name_app (SOME name,formals) = 
     let val (formal_vars,_) = ListPair.unzip formals
         val actuals = List.map Var_c formal_vars
     in SOME (App_c(name,actuals))
     end
     | name_app (NONE,_)            = NONE
     
   fun name_eqn (SOME name,_) = SingleType_k name
     | name_eqn (_,kind)      = kind

   local	
     fun selfify (con,kind,subst) =
       (case kind of
          Type_k => SingleType_k con
	| SingleType_k _ => SingleType_k(con) 
	| Single_k _ => Single_k con 
	| Record_k entries => 
	    let
	      fun mapper ((l,v),k) = 
		let
		  val proj = Proj_c (con,l)
		  val kres = selfify (proj,k,subst)
		in
		  ((l,v),kres)
		end
	    in
	      Record_k (Sequence.map mapper entries)
	    end
	| Arrow_k (openness,args,return) => 
	    let
	      val (formal_vars,kinds) = ListPair.unzip args
	      val kinds = map (substConInKind subst) kinds
	      val args = ListPair.zip (formal_vars,kinds)
	      val actuals = List.map Var_c formal_vars
	    in
	      Arrow_k (openness,args,selfify(App_c (con,actuals),return,subst))
	    end)
   in
     fun name_self(SOME name,kind,subst) = 
       (empty_subst(),
	if NilSubst.C.is_empty subst then NilUtil.selfify (name,kind)
	else selfify (name,kind,subst))
       | name_self (NONE,kind,subst)     = (subst,kind)
   end

   val name_self = subtimer("Ctx:name_self",name_self)

(*   val KSTtrace = Trace.newtrace "kind_standarize"
   val KOFtrace = Trace.newtrace "kind_of"
*)
   fun kind_standardize(D : context, kind : kind) : kind = kind_standardize'(D,kind,NONE)
   and kind_standardize'(D : context, kind : kind, name : con option) : kind = 
     let 
(*       val _ = Trace.enter KSTtrace*)

       fun insert_std_kind (context as {conmap,kindmap,counter}: context,var,std_kind) = 
	 let
	   val entry = {eqn = NONE,
			kind = std_kind,
			std_kind = immediate std_kind, 
			max_kind = delay (fn () => selfify (Var_c var,std_kind)),
			index = counter}
	 in
	   {conmap = conmap, 
	    counter = counter+1,
	    kindmap = Vinsert (kindmap, var, entry)}
	 end
      val res = 
	(case kind of
	   Type_k => name_eqn(name,kind)
	 | SingleType_k con => name_eqn(name,kind)
	 | Single_k con => let val (s,k) = kind_of'(D,con,name) in substConInKind s k end
	 | Record_k elts => 
	     let
	       fun folder (((label,var),kind),D) = 
		 let
		   val std_kind = kind_standardize'(D,kind,name_proj (name,label))
		   val D = insert_std_kind(D,var,std_kind)
		 in
		   (((label,var),std_kind),D)
		 end
	       val (elts,D) = Sequence.foldl_acc folder D elts
	     in
	       Record_k elts
	     end
	 | Arrow_k (openness, formals, return) => 
	     let
	       fun folder ((var,kind),D) = 
		 let
		   val std_kind = kind_standardize'(D,kind,NONE)
		   val D = insert_std_kind(D,var,std_kind)
		 in
		   ((var,std_kind),D)
		 end
	       val (formals,D) = foldl_acc folder D formals
	       val return = kind_standardize'(D,return,name_app (name,formals))
	     in
	       Arrow_k(openness, formals, return)
	     end)
(*      val _ = Trace.exit KSTtrace*)
    in
      res 
    end
   and kind_of (D : context,constructor : con) : kind = let val (subst,kind) = kind_of' (D,constructor,NONE)
							in substConInKind subst kind
							end
   and kind_of' (D : context,constructor : con,name : con option) : NilSubst.con_subst * kind = 
     let
(*       val _ = Trace.enter KOFtrace*)

       fun insert_std_kind (context as {conmap,kindmap,counter}: context,var,std_kind) = 
	 let
	   val entry = {eqn = NONE,
			kind = std_kind,
			std_kind = immediate std_kind, 
			max_kind = delay (fn () => selfify (Var_c var,std_kind)),
			index = counter}
	 in
	   {conmap = conmap, 
	    counter = counter+1,
	    kindmap = Vinsert (kindmap, var, entry)}
	 end

       val res = 
	 (case constructor 
	    of Prim_c _        => (empty_subst(),name_eqn(name,SingleType_k(constructor)))
	     | (AllArrow_c _)  => (empty_subst(),name_eqn(name,SingleType_k(constructor)))
	     | ExternArrow_c _ => (empty_subst(),name_eqn(name,SingleType_k(constructor)))
	     | Typeof_c _      => (empty_subst(),name_eqn(name,SingleType_k(constructor)))
	      
	     | (Mu_c (recur,defs)) => 
	      let 
		val len = Sequence.length defs
	      in  (empty_subst(),
		   if len = 1
		     then name_eqn(name,SingleType_k(constructor))
		   else 
		     let 
		       fun mapper (i,(v,c)) = 
			 let val label = generate_tuple_label(i+1)
			   val kind = name_eqn(name_proj(name,label),SingleType_k(Proj_c(constructor,label)))
			 in((label,Name.derived_var v),kind)
			 end
		       val entries = Sequence.mapcount mapper defs
		     in  Record_k(entries)
		     end)
	      end
	    
	     | (v as (Var_c var)) => 
	      let
		val kind = (find_max_kind (D,var)  
			    handle Unbound =>
			      (print_context D;
			       error  (locate "kind_of") ("variable "^(var2string var)^" not in context")))
	      in name_self(name,kind,empty_subst())
	      end
	    
	     | Let_c (sort,bnds,let_body) =>
	      let
		fun Pi(D,subst,Tag,formals,body) = 
		  let 
		    fun folder ((v,k),D) = 
		      let
			val k = kind_standardize(D,k)  (*No subst?*)
			val D = insert_std_kind(D,v,k)
		      in 
			((v,k),D)
		      end
		    val (formals,D) = foldl_acc folder D formals
		    val (subst2,return) = kind_of'(D,body,name_app (name,formals))
		    val subst = NilSubst.C.compose (subst,subst2)
		    val kind = Arrow_k(Tag,formals,return)
		  in
		    (subst,kind)
		  end		  
		
		fun add_bnd (D,subst) maker (var,formals,body) = 
		  let
		    val var' = derived_var var
		    val bnd = maker (var',formals,body)
		    val con = Let_c (sort,[bnd],Var_c var')
		    (*		      val con = substConInCon subst con*)
		    val subst = NilSubst.C.addr (subst,var,con)
		    val D = insert_kind(D,var,Single_k(con))
		  in (D,subst)
		  end
		
		val body_var_opt = strip_var let_body
		  
		fun loop ([],(D,subst)) = 
		  let
		    val (subst2,kind) = kind_of'(D,let_body,name)
		  in
		    (NilSubst.C.compose (subst,subst2),kind)
		  end
		  | loop (bnd::rest,state as (D,subst)) =
		  (case bnd
		     of Open_cb (args as (var,formals,body)) =>
		       if (null rest) andalso eq_opt(eq_var,SOME var,body_var_opt) then
			 Pi(D,subst,Open,formals,body)
		       else loop(rest,add_bnd state Open_cb args)
			 
		      | Code_cb (args as (var,formals,body)) => 
		       if (null rest) andalso eq_opt(eq_var,SOME var,body_var_opt) then
			 Pi(D,subst,Code,formals,body)
		       else loop(rest,add_bnd state Code_cb args)
		      | Con_cb (var,con) => 
		       let
			 val kind = kind_of(D,con)
			 val D = insert_std_kind(D,var,kind)
			 val subst = NilSubst.C.addr (subst,var,con)
		       in
			 loop(rest,(D,subst))
		       end)
	      in loop (bnds,(D,empty_subst()))
	      end
	    	      
	     | (Closure_c (code,env)) => 
	      let val (subst,code_kind) = kind_of' (D,code,name) 
		  val (vklist,body_kind) = 
		    (case code_kind
		       of Arrow_k (Code,vklist,body_kind) => (vklist,body_kind)
			| Arrow_k (ExternCode,vklist,body_kind) => (vklist,body_kind)
			| _ => (error  (locate "kind_of") "Invalid closure: code component does not have code kind" ))
		val (first,(v,klast)) = split vklist
	      in 
		(NilSubst.C.addl (v,env,subst),Arrow_k(Closure,first,body_kind))
	      end
	    
	     | (Crecord_c entries) => 
	      let 
		fun mapper (l,c) = 
		  let val var = Name.fresh_named_var "crec_kind_of"
		      val (subst,kind) = kind_of'(D,c,name_proj (name,l))
		  in ((l,var),substConInKind subst kind)
		  end
		val k_entries =  map mapper entries
	      in (empty_subst(),Record_k (Sequence.fromList k_entries))
	      end
	     | (Proj_c (rvals,label)) => 
	      let val (subst,k) = kind_of'(D,rvals,NONE)  
	      in name_self(name,project_from_kind_nondep(k,label),subst)
	      end
	    | (App_c (cfun,actuals)) => 
	      (case kind_of' (D,cfun,NONE) of   
		 (subst,Arrow_k (_,formals,body_kind)) => 
		   let 
		     fun folder ((v,k),c,subst) = add subst (v,c)
		     val subst2 = foldl2 folder (empty_subst()) (formals,actuals)
		     val subst = NilSubst.C.compose (subst2,subst)
		   in  
		     name_self(name,body_kind,subst)
		   end
	       | (_,cfun_kind) => 
		   (print "Invalid kind for constructor application\n";
		    Ppnil.pp_kind cfun_kind; print "\n";
		    error (locate "kind_of")  "Invalid kind for constructor application"))
	    | (Typecase_c {arg,arms,default,kind}) => (empty_subst(),kind_standardize'(D,kind,name))
	    | (Annotate_c (annot,con)) => kind_of'(D,con,name))
(*       val _ = Trace.exit KOFtrace*)
     in res
     end
   and insert_kind (context as {conmap,kindmap,counter}:context,var,kind) = 
     let
       val _ =  
	   if !debug then
	     assert (locate "insert_kind")
	     [
	      (allBound_k kindmap kind,
	       fn () => (Ppnil.pp_kind kind;
			 lprintl "Kind contains variables not found in context"))
	      ]
	   else (); 
	 val kind_standardize' = subtimer("Ctx:find_xxx:KST",kind_standardize')
	 val selfify = subtimer("Ctx:find_xxx:Selfify",NilUtil.selfify)
	 val std_kind = delay (fn () => kind_standardize' (context,kind,NONE))
	 val max_kind = delay (fn () => if delayed std_kind 
					  then kind_standardize' (context,kind,SOME (Var_c var))
					else selfify(Var_c var,thaw std_kind))

	 val entry = {eqn = NONE,
		      kind = kind,
		      std_kind = std_kind,
		      max_kind = max_kind,
		      index = counter}
       in
	 {conmap = conmap, 
	  counter = counter+1,
	  kindmap = Vinsert (kindmap, var, entry)}
       end     


  fun insert_kind_equation (context as {conmap,kindmap,counter}:context,var,con,kind) = 
     let
       val _ =  
	 if !debug then
	   assert (locate "insert_kind_equation")
	   [
	    (allBound_c kindmap con,
		 fn () => (Ppnil.pp_con con;
			   print "Constructor contains variables not found in context")),
	    (allBound_k kindmap kind,
		 fn () => (Ppnil.pp_kind kind;
			   print "Kind contains variables not found in context"))
	    ]
	 else ();
       val kind_of = subtimer("Ctx:find_xxx:kind_of",kind_of)
       val kind_of' = subtimer("Ctx:find_xxx:kind_of'",kind_of')
       val selfify = subtimer("Ctx:find_xxx:Selfify",NilUtil.selfify)
       val std_kind = delay (fn () => kind_standardize(context,kind))
       val max_kind = delay (fn () => if delayed std_kind 
					then kind_standardize'(context,kind,SOME (Var_c var))
				      else selfify(Var_c var,thaw std_kind))


       val entry = {eqn = SOME con,
		    kind = kind,
		    std_kind = std_kind,
		    max_kind = max_kind,
		    index = counter}
     in
       {conmap = conmap, 
	counter = counter+1,
	kindmap = Vinsert (kindmap, var, entry)}
     end

  fun insert_equation (context,var,con) = 
    insert_kind_equation(context,var,con,Single_k con)

  fun insert_kind_list (C:context,vklist) = 
    foldl (fn ((v,k),C) => insert_kind (C,v,k)) C vklist


  (*PRE:  con :: T or W *)
  fun find_kind_equation(s as {kindmap,...}:context,con) : con option = 
    let open Nil
      datatype result = CON of con | KIND of (kind * NilSubst.con_subst) | NON_PATH
      
      val substConInCon = fn s => subtimer ("Ctx:FKEQ:substConInCon",substConInCon s)

      fun project_kind(k,c,l,subst) = 
	(case k of
	   Record_k lvk_seq => 
	     let 
	       val lvk_list = Sequence.toList lvk_seq
	       fun loop _ [] = error (locate "find_kind_equation.project_kind") "missing label"
		 | loop subst (((l',v),k)::rest) = 
		 if (Name.eq_label(l,l'))
		   then (KIND (k,subst))
		 else loop (add subst (v,Proj_c(c,l'))) rest
	     in  loop subst lvk_list
	     end
	 | Single_k c => CON(substConInCon subst (Proj_c(c,l)))
	 | _ => 
	     (print "bad kind to project from = \n";
	      Ppnil.pp_kind k;
	      print "\n\n";
	      error (locate "find_kind_equation.project_kind") 
	      "bad kind to project_kind"))

      val project_kind = subtimer("Ctx:project_kind",project_kind)

      fun app_kind(k,c1,clist,subst) = 
	(case k 
	   of Arrow_k (openness,vklist, k) => 
	     let 
	       fun folder ((v,k),c,subst) = add subst (v,c)
	     in  
	       KIND (k, foldl2 folder subst (vklist,clist))
	     end
	    | Single_k c => CON(substConInCon subst (App_c(c,clist)))
	    | _ => error (locate "app_kind") "bad kind to app_kind")

      val app_kind = subtimer("Ctx:app_kind",app_kind)

      fun closure_kind (Single_k c,env,subst) = CON(substConInCon subst (Closure_c(c,env)))
	| closure_kind (k,env,subst) = 
	(let 
	   val (vklist,body_kind) = 
	     (case k
		of Arrow_k (Code,vklist,body_kind) => (vklist,body_kind)
		 | Arrow_k (ExternCode,vklist,body_kind) => (vklist,body_kind)
		 | _ => (error  (locate "find_kind_equation") "Invalid closure: code component does not have code kind" ))
	   val (first,(v,klast)) = split vklist
	   val body_kind = varConKindSubst v env body_kind
	 in 
	   KIND (Arrow_k(Closure,first,body_kind),subst)
	 end)

      fun kind_eqn k = 
	(case k 
	   of Single_k c => SOME c
	    | SingleType_k c => SOME c
	    | _ => NONE)

      fun get_eqn c = 
	(case c
	   of CON c => SOME c
	    | KIND(k,s) => mapopt (substConInCon s) (kind_eqn k)
	    | NON_PATH => NONE)

      fun trans c = 
	if !transitive then
	  (case get_eqn(traverse c)
	     of SOME c => (found_trans();trans c)
	      | NONE => c)
	else c
      and traverse c : result = 
	(case c 
	   of (Var_c v) => 
	     (case V.find(kindmap,v) 
		of SOME {eqn,kind,...} => 
		  (case (eqn,kind_eqn kind)
		     of (SOME c,_) => CON(trans c)   (*Try and get a transitive closure of it*)
		      | (_,SOME c)  => CON(trans c)
		      | _ => KIND (kind, empty_subst()))
		 | NONE => (print "Traverse:Variable ";print (var2string v);print " not found in context!\n";
			    raise Unbound))
	    | (Proj_c (c,l)) => 
		let 
		  val res = traverse c
		in  
		  case res of
		    CON c => CON(Proj_c(c,l))
		  | KIND (k,subst) => project_kind(k,c,l,subst)
		  | NON_PATH => NON_PATH
		end
	    | (App_c(c1,clist)) =>
		let 
		  val (res) = traverse c1
		in  
		  case res of
		    CON c => CON(App_c(c,clist))
		  | KIND (k,subst) => 
		      app_kind(k,c1,clist,subst)
		  | NON_PATH => NON_PATH
		end
	    | (Closure_c(c1,c2)) => 
		let
		  val res = traverse c1
		in
		  case res
		    of CON c1 => CON(Closure_c(c1,c2))
		     | KIND (k,subst) => closure_kind(k,c2,subst)
		     | NON_PATH => NON_PATH
		end
	    | (Annotate_c (_,c)) => traverse c
	    | _ => NON_PATH)

     in 
       (*Try and get an equation.  If successful, transitively find
	* as deep an equation as possible
	*)
       mapopt trans (get_eqn (traverse con))
     end

  val find_kind_equation = subtimer("Ctx:find_kind_equation",find_kind_equation)

   fun is_well_formed (kind_valid : context * kind -> unit,
		       con_valid : context * con -> kind,
		       subkind : context * kind * kind -> bool) ({kindmap,...}:context) : bool =
     let
       fun compare ((_,a):var*k_entry,(_,b):var*k_entry) = (#index a) > (#index b)
       val entries = V.listItemsi kindmap
       val entries =ListMergeSort.sort compare entries
       val error : string -> bool = error (locate "is_well_formed") 
	 
       fun folder ((var,entry as {eqn,kind,std_kind,max_kind,index}),D as {kindmap,conmap,counter}) = 
	 let
	   val _ = 
	     (
	      kind_valid (D,kind);

	      (case eqn
		 of SOME con => 
		   let val kind' = con_valid (D,con)
		   in
		     (
		      (subkind(D,kind',kind)) orelse
		      (perr_c_k_k (con,kind,kind');
		       error ("Invalid equation/kind for entry: "^(var2string var)));

		      ()
		     )
		   end
		  | _ => ());
		 
	      ((index = counter) orelse
	       (error "Indexing error"))
	      )
	   val D = 
	     {kindmap = Vinsert (kindmap,var,entry),
	      conmap = conmap,
	      counter = counter+1}
	 in
	   D
	 end
       val _ = List.foldl folder (empty()) entries
     in
       true
     end

   fun generate_error_context (orig,context,[]) = context
     | generate_error_context (orig,context,fvlist) = 
     let

       fun k_get ({kindmap,...}:context,v) = 
	 (case V.find (kindmap,v)
	    of NONE => raise Unbound
	     | SOME {eqn,kind,...} => (eqn,kind))

       fun k_insert (v,k,copt,D) = 
	 if contains (#kindmap D) v then D else
	   (case copt of SOME c => insert_kind_equation (D,v,c,k) | NONE => insert_kind (D,v,k))

       val (ev_list,cv_list) = Listops.unzip fvlist
       val free_cvs = List.concat cv_list
       val free_evs = List.concat ev_list

       (*Get the equations opts and kinds, and then filter out the non-equations *)
       val (eqn_opts,kinds) = Listops.unzip (map (fn v => k_get(orig,v)) free_cvs)
       val eqns = List.mapPartial (fn x => x) eqn_opts

       (*Get the types*)
       val types = map (fn v => find_con(orig,v)) free_evs

       (*Get the context that covers the kinds, equations, and types*)
       val context = generate_kind_error_context' (orig,context,kinds)
       val context = generate_con_error_context' (orig,context,eqns)
       val context = generate_con_error_context' (orig,context,types)

       (*Insert the equations, kinds, and types*)
       val context = foldl3 k_insert context (free_cvs,kinds,eqn_opts)
       val context = foldl2 (fn (v,c,D) => insert_con (D,v,c)) context (free_evs,types)
     in
       context
     end
   and generate_con_error_context' (orig,context,cons) = 
     let
       val fvlist = map (fn c => NilUtil.freeExpConVarInCon (true,c)) cons
     in
       generate_error_context (orig,context,fvlist)
     end
   and generate_kind_error_context' (orig,context,kinds) = 
     let
       val fvlist = map (fn c => NilUtil.freeExpConVarInKind (true,c)) kinds
     in
       generate_error_context (orig,context,fvlist)
     end
   and generate_exp_error_context' (orig,context,exps) = 
     let
       val fvlist = map (fn c => NilUtil.freeExpConVarInExp (true,c)) exps
     in
       generate_error_context (orig,context,fvlist)
     end

   fun exps_error_context (orig,exps)   = generate_exp_error_context'  (orig,empty(),exps)
   fun cons_error_context (orig,cons)   = generate_con_error_context'  (orig,empty(),cons)
   fun kinds_error_context (orig,kinds) = generate_kind_error_context' (orig,empty(),kinds)

   fun exp_error_context (orig,exp)   = generate_exp_error_context'  (orig,empty(),[exp])
   fun con_error_context (orig,con)   = generate_con_error_context'  (orig,empty(),[con])
   fun kind_error_context (orig,kind) = generate_kind_error_context' (orig,empty(),[kind])
 end

