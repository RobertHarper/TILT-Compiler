(*$import Stats Option Ppnil NilSubst NilUtil ListMergeSort NILCONTEXTPRE *)

structure NilContextPre
   :> NILCONTEXTPRE = 
 struct


   open Nil 
   open Prim


   (* IMPORTS *)
   val subtimer = Stats.subtimer

   val substConInKind = fn s => subtimer("Ctx:substConInKind",NilSubst.substConInKind s)
   val substConInCon = fn s => subtimer("Ctx:substConInCon",NilSubst.substConInCon s)
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



   (*END OF IMPORTS     *)

   val profile = Stats.ff "nil_profile"
   val debug = Stats.ff "nil_debug"
   val eager = Stats.ff "nil_eager"
   val memoize = Stats.tt "nilcontext_memoize"

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
		   index : int}
     
   type c_entry = {con : con,
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

   (**** Printing functions *****)
   local
     fun print_con (var,{con,std_con}:c_entry) =
       (print (Name.var2string var);
	print ":";
	Ppnil.pp_con con;
	if (!print_std_cons) then 
	  (lprint ":";
	   Ppnil.pp_con (thaw std_con))
	else
	  print "\n")
       
     fun print_entry (var,{eqn,kind,std_kind,index}:k_entry) =
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
	   assert (locate "insert_con")
	   [
	    (not (contains conmap var),
	     fn () => print ("Term variable already occurs in context: "^(Name.var2string var)))
(*            ,
	    (isRenamedCon ctx con, 
	     fn () => (Ppnil.pp_con con;
		       print ("Type not properly renamed when inserted into context"))) *)
	    ]
	 else ()
       val thunk = fn () => raise Unimplemented (*type_standardize(ctx,con)*)
       val c_entry = 
	 {con = con,
	  std_con = delay thunk}
     in
       {conmap = V.insert (conmap, var, c_entry), 
	kindmap = kindmap,
	counter = counter}
     end

   fun insert_con_list (C:context,defs : (var * con) list) =
     List.foldl (fn ((v,c),C) => insert_con (C,v,c)) C defs

   fun find_con ({conmap,...}:context,var) = 
       (case V.find (conmap, var) of
	    SOME {con,std_con} => con
	  | NONE => raise Unbound)


   fun find_std_con_pre normalize (D as {conmap,...}:context,var) = 
       (case V.find (conmap, var) of
	    SOME {con,std_con} => (std_con := (THAWED (normalize (D,con)));
				   thaw std_con)
	  | NONE => raise Unbound)

   (*****Constructor level context functions. ******)

   fun vprint v = (lprintl (var2string v);false)
   fun allBound_c kindmap con = c_all (contains kindmap) vprint  (NilUtil.freeConVarInCon (true,con))
   fun allBound_k kindmap kind = c_all (contains kindmap) vprint (NilUtil.freeConVarInKind kind)

   fun var_error v () = error (locate "var_error") ("Constructor variable already occurs in context: "^(Name.var2string v))

   fun find_std_kind (context as {kindmap,...}:context,var) = 
     (case (V.find (kindmap, var)) of
	   SOME {std_kind,...} => thaw std_kind
	 | NONE => raise Unbound)

   fun find_kind (context as {kindmap,...}:context,var) = 
     (case (V.find (kindmap, var)) of
	   SOME {kind,...} => kind
	 | NONE => raise Unbound)

	
   fun kind_standardize(D : context, kind : kind) : kind = 
     let 
       
       fun insert_std_kind (context as {conmap,kindmap,counter}: context,var,kind,std_kind) = 
	 let
	   val entry = {eqn = NONE,
			kind = kind,
(*			std_kind = immediate std_kind, *)
			std_kind = delay std_kind,
			index = counter}
	 in
	   {conmap = conmap, 
	    counter = counter+1,
	    kindmap = V.insert (kindmap, var, entry)}
	 end
      
      val res = 
	(case kind of
	   Type_k => Type_k
	 | SingleType_k con => kind
	 | Single_k con => kind_of(D,con)
	 | Record_k elts => 
	     let
	       fun folder (((label,var),kind),D) = 
		 let
		   val std_kind = kind_standardize(D,kind)
		   val D = insert_std_kind(D,var,kind,fn() => selfify(Var_c var,std_kind))
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
		   val std_kind = kind_standardize(D,kind)
		   val D = insert_std_kind(D,var,kind,fn() => selfify (Var_c var,std_kind))
		 in
		   ((var,std_kind),D)
		 end
	       val (formals,D) = foldl_acc folder D formals
	       val return = kind_standardize(D,return)
	     in
	       Arrow_k(openness, formals, return)
	     end)

    in
      res 
    end
   and kind_of (D : context,constructor : con) : kind = 
     let 

       val res = 
	 (case constructor 
	    of Prim_c _ => SingleType_k(constructor)
	     | (Mu_c (recur,defs)) => 
	      let 
		val len = Sequence.length defs
	      in  if len = 1
		    then SingleType_k(constructor)
		  else 
		    let 
		      fun mapper (i,(v,c)) = 
			let val label = generate_tuple_label(i+1)
			in((label,derived_var v),SingleType_k(Proj_c(constructor,label)))
			end
		      val entries = Sequence.mapcount mapper defs
		    in  Record_k(entries)
		    end
	      end

	     | (AllArrow_c _) => SingleType_k(constructor)
	      
	     | ExternArrow_c _ => SingleType_k(constructor)

	     | Typeof_c _ => SingleType_k(constructor)

	     | (v as (Var_c var)) => 
	      let
		val kind = (find_std_kind (D,var)
			    handle Unbound =>
			      (print_context D;
			       error  (locate "kind_of") ("variable "^(var2string var)^" not in context")))
	      in
		kind
	      end
	    
	     | Let_c (sort,bnds,let_body) =>
	      let
		local 

		  fun Pi(D,Tag,formals,body) = 
		    let 
		      fun folder ((v,k),D) = 
			let
			  val k = kind_standardize(D,k)
			  val D = insert_kind(D,v,k)
			in 
			  ((v,k),D)
			end
		      val (formals,D) = foldl_acc folder D formals
		      val return = kind_of(D,body)
		      val kind = Arrow_k(Tag,formals,return)
		    in
		      kind
		    end		  

		  fun add_bnd (D,subst) maker (var,formals,body,body_kind) = 
		    let
		      val var' = derived_var var
		      val bnd = maker (var',formals,body,body_kind)
		      val con = Let_c (sort,[bnd],Var_c var')
		      val con = substConInCon subst con
		      val subst = add subst (var,con)
		      val D = insert_kind(D,var,Single_k(con))
		    in (D,subst)
		    end
		  
		  val body_var_opt = strip_var let_body

		in
		  fun loop ([],(D,subst)) = (subst,kind_of(D,let_body))
		    | loop (bnd::rest,state as (D,subst)) =
		    (case bnd
		       of Open_cb (args as (var,formals,body,body_kind)) =>
			 if (null rest) andalso eq_opt(eq_var,SOME var,body_var_opt) then
			   (subst,Pi(D,Open,formals,body))
			 else loop(rest,add_bnd state Open_cb args)

			| Code_cb (args as (var,formals,body,body_kind)) => 
			   if (null rest) andalso eq_opt(eq_var,SOME var,body_var_opt) then
			     (subst,Pi(D,Code,formals,body))
			   else loop(rest,add_bnd state Code_cb args)

			| Con_cb (var,con) => 
			  let
			    val kind = kind_of(D,con)
			    val D = insert_kind(D,var,kind)
			    val subst = addr (subst,var,con)
			  in
			    loop(rest,(D,subst))
			  end)
		end
	      val (subst,kind) = loop (bnds,(D,empty_subst()))
	      val kind = substConInKind subst kind
	      in
		kind
	      end
	    	      
	    | (Closure_c (code,env)) => 
	      let 
		val (vklist,body_kind) = 
		  (case kind_of (D,code)
		     of Arrow_k (Code,vklist,body_kind) => (vklist,body_kind)
		      | Arrow_k (ExternCode,vklist,body_kind) => (vklist,body_kind)
		      | _ => (error  (locate "kind_of") "Invalid closure: code component does not have code kind" ))
	       val (first,(v,klast)) = split vklist
	       val body_kind = varConKindSubst v env body_kind
	      in 
		Arrow_k(Closure,first,body_kind)
	      end
	    
	    | (Crecord_c entries) => 
	      let
		val k_entries = 
		  map (fn (l,c) => ((l,fresh_named_var "crec_kind_of"),kind_of(D,c))) entries
	      in Record_k (Sequence.fromList k_entries)
	      end
	    
	    | (Proj_c (rvals,label)) => 
	      (case kind_of(D,rvals)
		 of Record_k lvk_seq => (*NilUtil.project_from_kind (lvk_seq,rvals,label) Claim is that this is non-dependent*)
		   (case Sequence.find (fn (l,_) => (Name.eq_label(label,l))) lvk_seq
		      of SOME k => k
		       | NONE => error  (locate "kind_of") "Field not in record kind")
		  | other => 
		   (print "Non-record kind returned from kind_of in projection:\n";
		    Ppnil.pp_kind other; print "\n";
		    error  (locate "kind_of") "Non-record kind returned from kind_of in projection"))
		 
	    | (App_c (cfun,actuals)) => 
	      (case kind_of (D,cfun) of
		 (Arrow_k (_,formals,body_kind)) => 
		   let 
		     fun folder ((v,k),c,subst) = add subst (v,c)
		     val subst = foldl2 folder (empty_subst()) (formals,actuals)
		   in  
		     substConInKind subst body_kind
		   end
	       | cfun_kind => (print "Invalid kind for constructor application\n";
			       Ppnil.pp_kind cfun_kind; print "\n";
			       error (locate "kind_of")  "Invalid kind for constructor application"))
		 
	    | (Typecase_c {arg,arms,default,kind}) => kind_standardize(D,kind)
	    | (Annotate_c (annot,con)) => kind_of(D,con))

     in 
       res
     end
   and insert_kind (context as {conmap,kindmap,counter}:context,var,kind) = 
     let
       val _ =  
	 if !debug then
	   assert (locate "insert_kind")
	   [
	    (not (contains kindmap var),
		 var_error var),
	    (allBound_k kindmap kind,
		 fn () => (Ppnil.pp_kind kind;
			   lprintl "Kind contains variables not found in context"))
	    ]
	 else (); 
       fun thunk() = selfify(Var_c var,subtimer ("Ctx:kind_standardize",kind_standardize) (context,kind))
       val entry = {eqn = NONE,
		    kind = kind,
		    std_kind = delay thunk,
		    index = counter}
     in
       {conmap = conmap, 
	counter = counter+1,
	kindmap = V.insert (kindmap, var, entry)}
     end

  fun insert_kind_equation (context as {conmap,kindmap,counter}:context,var,con,kind) = 
     let
       val _ =  
	 if !debug then
	   assert (locate "insert_kind_equation")
	   [
	    (not (contains kindmap var),
		 var_error var),
	    (allBound_c kindmap con,
		 fn () => (Ppnil.pp_con con;
			   print "Constructor contains variables not found in context")),
	    (allBound_k kindmap kind,
		 fn () => (Ppnil.pp_kind kind;
			   print "Kind contains variables not found in context"))
	    ]
	 else ();
       fun thunk() = subtimer("Ctx:kind_standardize",kind_standardize)(context,Single_k(con))
       val entry = {eqn = SOME con,
		    kind = kind,
		    std_kind = delay thunk,
		    index = counter}
     in
       {conmap = conmap, 
	counter = counter+1,
	kindmap = V.insert (kindmap, var, entry)}
     end

  fun insert_equation (context as {conmap,kindmap,counter}:context,var,con) =  
     let
       val _ =  
	 if !debug then
	   assert (locate "insert_equation")
		  [
		   (not (contains kindmap var),
			var_error var),
		   (allBound_c kindmap con,
			fn () => (Ppnil.pp_con con;
				  print "Constructor contains variables not found in context"))
		   ]
	 else ();
       val kind = Single_k (con)
       fun sthunk() = subtimer("Ctx:kind_standardize",kind_standardize) (context,kind)
       val entry = {eqn = SOME con,
		    kind = kind,
		    std_kind = delay sthunk,
		    index = counter}
     in
       {conmap = conmap, 
	counter = counter+1,
	kindmap = V.insert (kindmap, var, entry)}
     end

  fun insert_kind_list (C:context,vklist) = 
    foldl (fn ((v,k),C) => insert_kind (C,v,k)) C vklist


  (*PRE:  con :: T or W *)
  fun find_kind_equation(s as {kindmap,...}:context,con) : con option = 
    let open Nil
      datatype result = CON of con | KIND of (kind * NilSubst.con_subst)
      exception Opaque
      
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

      fun traverse c : result = 
	(case c 
	   of (Var_c v) => 
	     (case V.find(kindmap,v) 
		of SOME {eqn,kind,...} => 
		  (case eqn
		     of SOME c => CON c
		      | NONE => 
		       (case kind 
			  of Single_k c => CON c
			   | SingleType_k c => CON c
			   | _ =>  KIND (kind, empty_subst())))
		 | NONE => (print "Variable not found in context!\n";
			    raise Unbound))
	    | (Proj_c (c,l)) => 
		let 
		  val res = traverse c
		in  
		  case res of
		    CON c => CON(Proj_c(c,l))
		  | KIND (k,subst) => project_kind(k,c,l,subst)
		end
	    | (App_c(c1,clist)) =>
		let 
		  val (res) = traverse c1
		in  
		  case res of
		    CON c => CON(App_c(c,clist))
		  | KIND (k,subst) => 
		      app_kind(k,c1,clist,subst)
		end
	    | (Closure_c(c1,c2)) => 
		let
		  val res = traverse c1
		in
		  case res
		    of CON c1 => CON(Closure_c(c1,c2))
		     | KIND (k,subst) => closure_kind(k,c2,subst)
		end
	    | _ => ((* print "traverse given non-path = \n";
		     Ppnil.pp_con c; print "\n"; *)
		    raise Opaque))

     in 
       (case traverse con 
	  of (CON c) => SOME c
	   | KIND (SingleType_k c,subst) => SOME (substConInCon subst c)
	   | KIND (Single_k c,subst) => SOME (substConInCon subst c)
	   | _ => NONE)

	  handle Opaque => NONE
	       | e => (print "find_kind_equation given = \n";
		       Ppnil.pp_con con; print "\n"; raise e)
     end

  val find_kind_equation = Stats.subtimer("Ctx:find_kind_equation",find_kind_equation)

   fun is_well_formed (kind_valid : context * kind -> unit,
		       con_valid : context * con -> kind,
		       subkind : context * kind * kind -> bool) ({kindmap,...}:context) : bool =
     let
       fun compare ((_,a):var*k_entry,(_,b):var*k_entry) = (#index a) > (#index b)
       val entries = V.listItemsi kindmap
       val entries =ListMergeSort.sort compare entries
       val error : string -> bool = error (locate "is_well_formed") 
	     
       fun folder ((var,entry as {eqn,kind,std_kind,index}),D as {kindmap,conmap,counter}) = 
	 let
	   val std_kind = thaw std_kind
	   val _ = 
	     (
	      kind_valid (D,kind);
	      kind_valid (D,std_kind);

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
	     {kindmap = V.insert (kindmap,var,entry),
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

       fun k_insert (v,k,SOME c,D) = insert_kind_equation (D,v,c,k) 
	 | k_insert (v,k,NONE,D)   = insert_kind (D,v,k)

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

