(*$import Stats Option Ppnil NilSubst NilUtil NILCONTEXT *)

structure NilContext
   :> NILCONTEXT where type 'a subst = 'a NilSubst.subst = 
 struct


   open Nil 
   open Prim

   type 'a subst = 'a NilSubst.subst

   (* IMPORTS *)
   val substConInKind = NilSubst.substConInKind

   val var2string = Name.var2string

   val eq_label = Name.eq_label
   val fresh_named_var = Name.fresh_named_var
   val eq_var = Name.eq_var

   val zip = Listops.zip
   val unzip = Listops.unzip
   val map2 = Listops.map2
   val split = Listops.split
   val map_second = Listops.map_second

   val curry2 = Util.curry2
   val printl = Util.printl
   val lprintl = Util.lprintl

   val is_shape = NilUtil.is_shape  
   val strip_arrow = NilUtil.strip_arrow

   val error = Util.error
   val locate = NilError.locate  "nilcontext.sml"
   val assert = NilError.assert
   val c_all = NilError.c_all
   val perr_k_k = NilError.perr_k_k
   val perr_c_k_k = NilError.perr_c_k_k

   (*END OF IMPORTS     *)

   val profile = Stats.ff "nil_profile"
   val debug = Stats.ff "nil_debug"
   val eager = Stats.ff "nil_eager"

   val (nilcontext_kinds_bound,
	nilcontext_kinds_renamed) =
     (Stats.counter "nilcontext_kinds_bound",
      Stats.counter "nilcontext_kinds_renamed")

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
				       val _ = r := (THAWED v)
				   in  v
				   end
   fun delayed (ref (FROZEN _)) = true
     | delayed (ref (THAWED _)) = false


   fun valOf str NONE = (print "valOf failed at "; print str; print "\n";
			 raise Option.Option)
     | valOf str (SOME x) = x


   type k_entry = {eqn: con option,
		   kind : kind,
		   shape : kind delay,
		   index : int}

   type context = 
     {kindmap : k_entry map,
      conmap  : con map,
      counter : int}
   (* kindmap(v) => (con,kind,shape,i)
     * Invariants: Not all of con, kind, shape are ref NONE
     *             v == con :: kind
     *             con :: kind
     *             kind <: shape
     *             
     *             Forall(v1,2), if v1 => (c1,k1,s1,i1)
     *              and v2 => (c2,k2,s2,i2), then
     *              i1 < i2 iff v1 inserted before v2.
     *             Forall(v) s.t. v => (c,k,s,i), counter > i
     *)

   fun empty () : context = 
     {kindmap = V.empty,
      conmap = V.empty,
      counter = 0}


   fun contains map var = Option.isSome (V.find (map,var))

   (**** Printing functions *****)

   fun print_con (var,con) =
     (print (Name.var2string var);
      print ":";
      Ppnil.pp_con con;
      print "\n")

   fun print_entry (var,{eqn,kind,shape,index}:k_entry) =
     (print (Name.var2string var);
      (case eqn
	 of SOME c => (print " = ";
		       Ppnil.pp_con c)
	  | NONE => ());
      print "::";
      Ppnil.pp_kind kind;
      print "\n")

   fun print_kinds ({kindmap,...}:context) = 
     (print "\n Constructor variables, kinds, and equations are :\n";
      V.appi print_entry kindmap)

   fun print_cons ({conmap,...}:context) = 
     (print "\n Expression variables and constructors are :\n";
      V.appi print_con conmap)

   fun print_context (context:context) = 
     (print_kinds context;
      print_cons context)

   fun print_context' (context:context) = 
     (print_kinds context;
      print_cons context)

   local
     type 'item renamer = (var -> bool) * (var -> bool) -> 'item -> bool
     fun isRenamedXXX (isRenamedXXXWRT : 'item renamer) ({kindmap,conmap,counter} : context) (item : 'item) = 
       isRenamedXXXWRT (contains conmap,contains kindmap) item
   in
     val isRenamedExp  = isRenamedXXX NilSubst.isRenamedExpWRT
     val isRenamedCon  = isRenamedXXX NilSubst.isRenamedConWRT
     val isRenamedKind = isRenamedXXX NilSubst.isRenamedKindWRT
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
     in
       {conmap = V.insert (conmap, var, con), 
	kindmap = kindmap,
	counter = counter}
     end

   fun insert_con_list (C:context,defs : (var * con) list) =
     List.foldl (fn ((v,c),C) => insert_con (C,v,c)) C defs

   fun find_con ({conmap,...}:context,var) = 
       (case V.find (conmap, var) of
	    SOME con => con
	  | NONE => raise Unbound)


   (*****Constructor level context functions. ******)

   fun vprint v = (lprintl (var2string v);false)
   fun allBound_c kindmap con = c_all (contains kindmap) vprint  (NilUtil.freeConVarInCon (true,con))
   fun allBound_k kindmap kind = c_all (contains kindmap) vprint (NilUtil.freeConVarInKind kind)

   fun var_error v () = error (locate "var_error") ("Constructor variable already occurs in context: "^(Name.var2string v))

   fun insert_shape (ctx as {conmap,kindmap,counter}:context,var,shape:kind) = 
     let
       val _ =  
	 if !debug then
	   assert (locate "insert_shape")
		  [
		   (not (contains kindmap var), 
		       var_error var),
		   (allBound_k kindmap shape, 
		       fn () => (Ppnil.pp_kind shape;
				 print "Kind contains variables not found in context")),
		   (is_shape shape, 
		       fn () => (Ppnil.pp_kind shape;
				 print "Kind is not a shape"))
(*               ,
		   (isRenamedKind ctx shape, 
		    fn () => (Ppnil.pp_kind shape;
			      print ("Kind not properly renamed when inserted into context"))) *)
		   ]
	 else ();

       val entry = {eqn = NONE,
		    kind = shape,  (*can never get better info*)
		    shape = immediate shape,
		    index = counter}
     in
       {conmap = conmap, 
	counter = counter+1,
	kindmap = V.insert (kindmap, var, entry)}
     end

   (* Turn a kind into a shape *)
   fun make_shape (D:context,kind:kind):kind = 
     (case kind 
	of Type_k => kind
	 | SingleType_k con => Type_k
	 | Single_k con => shape_of (D,con)  
	 | Record_k elts => 
	  let 
	    fun folder(((l,v),k),D) = let val k' = make_shape(D,k)
				      in  (((l,v),k'),insert_shape(D,v,k'))
				      end
	    val (entries,_) = (Sequence.foldl_acc folder D elts)
	  in  Record_k entries
	  end
	 | Arrow_k (openness, formals, return) => 
	  let 
(* CS: Was destroying subkind relationship
 	    fun folder((v,k),D) = let val k' = make_shape(D,k)
				  in  ((v,k'), insert_shape(D,v,k'))
				  end
	    val (formals,D) = Listops.foldl_acc folder D formals
*)
 	    fun folder((v,k),D) = insert_kind(D,v,k)
	    val D = List.foldl folder D formals

	    val return = make_shape (D,return)
	  in  (Arrow_k (openness, formals,return))
	  end)

   and shape_of (D : context,constructor : con) : kind = 
     let 
       val _ = 
	 if !debug then
	   assert (locate "shape_of") 
		  [
(*		   (isRenamedCon D constructor, 
		    fn () => (Ppnil.pp_con constructor;
			      print ("Type not properly renamed when passed to shape_of"))) *)
		   ]
	 else ()
     in
       (case constructor 
	  of Prim_c (Int_c W64,_) => Type_k
	   | Prim_c (Float_c F32,_) => Type_k (* error? *)
	   | Prim_c (Float_c F64,_) => Type_k (* error? *)
	   | Prim_c _ => Type_k

	   | (Mu_c (recur,defs)) => 
	    let val len = Sequence.length defs
	    in  if len = 1
		  then Type_k
		else NilUtil.kind_tuple(Listops.copy(len,Type_k))
	    end

	 | (AllArrow_c _) => Type_k

	 | ExternArrow_c _ => Type_k

	 | (v as (Var_c var)) => 
	    (find_shape (D,var)
	     handle Unbound =>
	       (print_context D;
		error (locate "shape_of") ("variable "^(var2string var)^" not in context")))

	 | Let_c (sort,bnds,body) => 
	    let
	      val D = shape_of_bnds (D,bnds) 
	    in
	      shape_of (D,body)
	    end
	 | Typeof_c _ => Type_k
	 | (Closure_c (code,env)) => 
	    let 
	      val (vklist,body_kind) = 
		(case shape_of (D,code) of
		      Arrow_k (Code,vklist,body_kind) => (vklist,body_kind)
		    | Arrow_k (ExternCode,vklist,body_kind) => (vklist,body_kind)
		    | _ => error (locate "shape_of") "Invalid closure: code component does not have code kind")
	      val shape = Arrow_k(Closure,Listops.butlast vklist,body_kind)
	    in shape
	    end

	 | (Crecord_c entries) => 
	  let
	    val (labels,cons) = unzip entries
	    val kinds = (map (curry2 shape_of D) cons)
	    val k_entries = 
		   map2 (fn (l,k) => ((l,fresh_named_var "crec_norm"),k)) (labels,kinds)
	    val entries = zip labels cons
	  in Record_k (Sequence.fromList k_entries)
	  end

	 | (Proj_c (rvals,label)) => 
	  let
	    val kind = 
	      (case (shape_of (D,rvals)) of
		 Record_k kinds => valOf "2" (Sequence.find (fn (l,v) => eq_label(l,label)) kinds)
	       | other => 
		 (print "Non-record kind returned from shape_of in projection:\n";
		  Ppnil.pp_kind other; print "\n";
		  error (locate "shape_of") "Non-record kind returned from shape_of in projection"))

	    (*No dependencies, since shape is all we get*)
	  in kind
	  end	 
	 | (App_c (cfun,actuals)) => 
	     let
	       val (formals,body_kind) = 
		 case shape_of (D,cfun) of
		   (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		 | cfun_kind => (print "Invalid kind for constructor application\n";
			 Ppnil.pp_kind cfun_kind; print "\n";
			 error (locate "shape_of") "Invalid kind for constructor application")
	     in body_kind
	     end
	 | (Typecase_c {arg,arms,default,kind}) => make_shape (D,kind)
	 | (Annotate_c (annot,con)) => shape_of (D,con))
       end 

   and shape_of_bnds (D : context, bnds : conbnd list) : context = 
     let
       fun folder (bnd,D) = 
	 (case bnd 
	    of Open_cb (var,formals,body,body_kind) => 
	      insert_shape (D,var,make_shape (D,Arrow_k(Open,formals,body_kind)))
	     | Code_cb (var,formals,body,body_kind) => 
	      insert_shape (D,var,make_shape (D,Arrow_k(Code,formals,body_kind)))
	     | Con_cb (var,con) =>
	      insert_shape (D,var,shape_of (D,con)))
       val D = List.foldl folder D bnds
     in
       D
     end
   and find_shape (context as {kindmap,...}:context,var) = 
     (case (V.find (kindmap, var)) of
	SOME {eqn,kind,shape,index} => thaw shape
      | NONE => raise Unbound)


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
(*            ,
	    (isRenamedKind context kind, 
	     fn () => (Ppnil.pp_kind kind;
		       lprintl ("Kind not properly renamed when inserted into context"))) *)
	    ]
	 else (); 
       fun thunk() = make_shape (context,kind)
       val entry = {eqn = NONE,
		    kind = kind,
		    shape = delay thunk,
		    index = counter}
     in
       {conmap = conmap, 
	counter = counter+1,
	kindmap = V.insert (kindmap, var, entry)}
     end

   fun insert_kind_shape (context as {conmap,kindmap,counter}:context,var,kind,shape) = 
     let
       val _ =  
	 if !debug then
	   assert (locate "insert_kind_shape")
	   [
	    (not (contains kindmap var),
		 var_error var),
	    (allBound_k kindmap kind,
		 fn () => (Ppnil.pp_kind kind;
			   print "Kind contains variables not found in context")),
	    (allBound_k kindmap shape,
		 fn () => (Ppnil.pp_kind shape;
			   print "Shape kind contains variables not found in context")),
	    (is_shape shape, 
	     fn () => (Ppnil.pp_kind shape;
		       print "Shape kind is not a shape"))
(*            ,
	    (isRenamedKind context kind, 
	     fn () => (Ppnil.pp_kind kind;
		       print ("Kind not properly renamed when inserted into context"))),
	    (isRenamedKind context shape, 
	     fn () => (Ppnil.pp_kind shape;
		       print ("Shape not properly renamed when inserted into context"))) *)
	    ]
	 else ();

       val entry = {eqn = NONE,
		    kind = kind,
		    shape = immediate shape,
		    index = counter}
     in
       {conmap = conmap, 
	counter = counter+1,
	kindmap = V.insert (kindmap, var, entry)}
     end

   fun insert_kind_shape_equation (ctx as {conmap,kindmap,counter}:context,var,con,kind,shape) = 
     let
       val _ =  
	 if !debug then
	   assert (locate "insert_kind_shape_equation")
	   [
	    (not (contains kindmap var),
		 var_error var),
	    (allBound_c kindmap con,
		 fn () => (Ppnil.pp_con con;
			   print "Constructor contains variables not found in context")),
	    (allBound_k kindmap kind,
		 fn () => (Ppnil.pp_kind kind;
			   print "Kind contains variables not found in context")),
	    (allBound_k kindmap shape,
		 fn () => (Ppnil.pp_kind shape;
			   print "Shape kind contains variables not found in context")),
	    (is_shape shape, 
	     fn () => (Ppnil.pp_kind shape;
		       print "Shape kind is not a shape"))
(*  ,
	    (isRenamedKind ctx kind, 
	     fn () => (Ppnil.pp_kind kind;
		       print ("Kind not properly renamed when inserted into context"))),
	    (isRenamedKind ctx shape, 
	     fn () => (Ppnil.pp_kind shape;
		       print ("Shape not properly renamed when inserted into context"))),
	    (isRenamedCon ctx con, 
	     fn () => (Ppnil.pp_con con;
		       print ("Type not properly renamed when inserted into context"))) *)
	    ]
	 else ();

       val entry = {eqn = SOME con,
		    kind = kind,
		    shape = immediate shape,
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
(*	 ,   (isRenamedCon context con, 
	     fn () => (Ppnil.pp_con con;
		       print ("Type not properly renamed when inserted into context"))) *)
	    ]
	 else ();
       fun thunk() = make_shape (context,kind)
       val entry = {eqn = SOME con,
		    kind = kind,
		    shape = delay thunk,
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
(*	,	   (isRenamedCon context con, 
		    fn () => (Ppnil.pp_con con;
			      print ("Type not properly renamed when inserted into context"))) *)
		   ]
	 else ();
       val kind = Single_k (con)
       fun sthunk() = make_shape (context,kind)
       val entry = {eqn = SOME con,
		    kind = kind,
		    shape = delay sthunk,
		    index = counter}
     in
       {conmap = conmap, 
	counter = counter+1,
	kindmap = V.insert (kindmap, var, entry)}
     end

   and insert_kind_list (C:context,vklist) = 
     foldl (fn ((v,k),C) => insert_kind (C,v,k)) C vklist

   and find_kind (context as {kindmap,...}:context,var) = 
     (case (V.find (kindmap, var)) of
	   SOME {kind,...} => kind
	 | NONE => raise Unbound)

   (*PRE:  con :: T or W *)
   fun find_kind_equation(s as {kindmap,...}:context,con) : con option = 
     let open Nil
	 datatype result = CON of con | KIND of kind
	 exception Opaque

	 val _ = 
	   if !debug then 
	     assert (locate "find_kind_equation")
		    [
(*		     (isRenamedCon s con,
		      fn () => (Ppnil.pp_con con;
				print ("Type not properly renamed passed to function"))) *)
		     ]
	   else
	     ()

	 fun project_kind(k,c,l,subst) = 
	   (case k of
	      Record_k lvk_seq => 
		 let val lvk_list = Sequence.toList lvk_seq
		     fun loop _ [] = error (locate "find_kind_equation.project_kind") "missing label"
		       | loop subst (((l',v),k)::rest) = 
			   if (Name.eq_label(l,l'))
			     then (KIND k, subst)
			   else loop (NilSubst.add subst (v,Proj_c(c,l'))) rest
		 in  loop subst lvk_list
		 end
	    | Single_k c => (CON(Proj_c(c,l)), subst)
	    | _ => 
		 (print "bad kind to project from = \n";
		  Ppnil.pp_kind k;
		  print "\n\n";
		  error (locate "find_kind_equation.project_kind") 
		  "bad kind to project_kind"))

	 fun app_kind(k,c1,clist,subst) = 
	   (case k 
	      of Arrow_k (openness,vklist, k) => 
		let 
		  fun folder (((v,k),c),subst) = NilSubst.add subst (v,c)
		in  
		  (KIND k, foldl folder subst (Listops.zip vklist clist))
		end
	       | Single_k c => (CON(App_c(c,clist)), subst)
	       | _ => error (locate "app_kind") "bad kind to app_kind")

	 fun traverse c : result * con subst = 
	   (case c 
	      of (Var_c v) => 
		(case V.find(kindmap,v) 
		   of SOME {eqn,kind,...} => 
		     (case eqn
			of SOME c => (CON c, NilSubst.empty())
			 | NONE => 
			  (KIND kind, NilSubst.empty()))
		    | NONE => raise Unbound)
	       | (Proj_c (c,l)) => 
		   let 
		     val (res,subst) = traverse c
		   in  
		     case res of
		       CON c => (CON(Proj_c(c,l)), subst)
		     | KIND k => project_kind(k,c,l,subst)
		   end
	       | (App_c(c1,clist)) =>
		   let 
		     val (res,subst) = traverse c1
		   in  
		     case res of
		       CON c => (CON(App_c(c,clist)), subst)
		     | KIND k => app_kind(k,c1,clist,subst)
		   end
	       | _ => ((* print "traverse given non-path = \n";
			Ppnil.pp_con c; print "\n"; *)
		       raise Opaque))

     in 
       (case traverse con 
	  of (CON c, _) => SOME c
	   | (KIND (SingleType_k c),subst) => SOME (NilSubst.substConInCon subst c)
	   | (KIND (Single_k c),subst) => SOME (NilSubst.substConInCon subst c)
	   | _ => NONE)

	  handle Opaque => NONE
	       | e => (print "find_kind_equation given = \n";
		       Ppnil.pp_con con; print "\n"; raise e)
     end


   fun is_well_formed (kind_valid : context * kind -> unit,
		       con_valid : context * con -> kind,
		       subkind : context * kind * kind -> bool) ({kindmap,...}:context) : bool =
     let
       fun compare ((_,a):var*k_entry,(_,b):var*k_entry) = (#index a) > (#index b)
       val entries = V.listItemsi kindmap
       val entries =ListMergeSort.sort compare entries
       val error : string -> bool = error (locate "is_well_formed") 
	     
       fun folder ((var,entry as {eqn,kind,shape,index}),D as {kindmap,conmap,counter}) = 
	 let
	   val shape = thaw shape
	   val _ = 
	     (
	      kind_valid (D,kind);
	      kind_valid (D,shape);

	      (case eqn
		 of SOME con => 
		   let val kind' = con_valid (D,con)
		   in
		     (
		      (subkind(D,kind',kind)) orelse
		      (perr_c_k_k (con,kind,kind');
		       error ("Invalid equation/kind for entry: "^(var2string var)));

(*		      (subkind(D,kind',shape)) orelse
		      (perr_c_k_k (con,shape,kind');
		       error ("Invalid equation/shape for entry: "^(var2string var))); *)
		      ()
		     )
		   end
		  | _ => ());
		 
(*	      ((subkind(D,kind,shape)) orelse
	       (perr_k_k (shape,kind);
		error ("Kind not a subkind of shape in entry: "^(var2string var)))); *)
 
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
 end
