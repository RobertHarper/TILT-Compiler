(*$import NIL PPNIL NILUTIL NILSUBST Stats NILCONTEXT PRIMUTIL Option *)
functor NilContextFn(structure PpNil : PPNIL
		     structure NilUtil : NILUTIL
		     structure PrimUtil : PRIMUTIL
		     structure Subst : NILSUBST)
   :> NILCONTEXT where type 'a subst = 'a Subst.subst = 
 struct


   open Nil 
   open Prim

   type 'a subst = 'a Subst.subst

   val substConInKind = Subst.substConInKind

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


   val is_shape = NilUtil.is_shape  
   val strip_arrow = NilUtil.strip_arrow

   val error = Util.error

   val profile = Stats.bool "nil_profile"
   val debug = Stats.bool "nil_debug"
   val eager = Stats.bool "nil_eager"
   val _ = eager := false
   val (nilcontext_kinds_bound,
	nilcontext_kinds_renamed) =
     (Stats.counter "nilcontext_kinds_bound",
      Stats.counter "nilcontext_kinds_renamed")

   structure V = Name.VarMap

   type 'a map = 'a V.map

   exception Unbound

   fun locate fn_name = "nilcontext.sml::"^fn_name

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
		   kind : kind delay,
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
     *             Forall(k) s.t. c::k, k<:shape
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


   exception Assert of string 

   fun assert location checkl = 
     let
       val loc_string = "\nAssertion violated in "^location^": "

       fun check (b,f) = if b then () 
			 else (f() handle any => ();
			       print loc_string;
			       raise Assert location)
     in
       List.app check checkl
     end

   (**** Printing functions *****)

   fun print_con (var,con) =
     (print (Name.var2string var);
      print ":";
      PpNil.pp_con con;
      print "\n")

   fun print_entry (var,{eqn,kind,shape,index}:k_entry) =
     (print (Name.var2string var);
      (case eqn
	 of SOME c => (print " = ";
		       PpNil.pp_con c)
	  | NONE => ());
      print "::";
      (if delayed kind then
	 print "Delayed"
       else
	 PpNil.pp_kind (thaw kind));
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

   (*****Term level context functions. ******)

   fun insert_con ({conmap,kindmap,counter}:context,var,con) = 
     let
       val _ = 
	 if !debug then
	   assert (locate "insert_con")
	   [
	    (not (contains conmap var),fn () => print ("Term variable already occurs in context: "^(Name.var2string var)))
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

   fun allBound_c kindmap con = List.all (contains kindmap) (NilUtil.freeConVarInCon (true,con))
   fun allBound_k kindmap kind = List.all (contains kindmap) (NilUtil.freeConVarInKind kind)

   fun var_error v () = error (locate "var_error") ("Constructor variable already occurs in context: "^(Name.var2string v))

   fun insert_shape ({conmap,kindmap,counter}:context,var,shape:kind) = 
     let
       val _ =  
	 if !debug then
	   assert (locate "insert_shape")
		  [
		   (not (contains kindmap var), 
		       var_error var),
		   (allBound_k kindmap shape, 
		       fn () => (PpNil.pp_kind shape;
				 print "Kind contains variables not found in context")),
		   (is_shape shape, 
		       fn () => (PpNil.pp_kind shape;
				 print "Kind is not a shape"))
		   ]
	 else ();

       val entry = {eqn = NONE,
		    kind = immediate shape,  (*can never get better info*)
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
	 | Singleton_k con => shape_of (D,con)  
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
	    fun folder((v,k),D) = let val k' = make_shape(D,k)
				  in  ((v,k'), insert_shape(D,v,k'))
				  end
	    val (formals,D) = Listops.foldl_acc folder D formals
	    val return = make_shape (D,return)
	  in  (Arrow_k (openness, formals,return))
	  end)

   and shape_of (D : context,constructor : con) : kind = 
     let 
       val _ = 
	 if !debug then
	   assert (locate "shape_of") 
		  [
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

	 | (v as (Var_c var)) => 
	    (find_shape (D,var)
	     handle Unbound =>
	       (print_context D;
		error (locate "shape_of") ("variable "^(var2string var)^" not in context")))

	 | Let_c (sort,bnds,body) => 
	    let
	      val D = kind_of_bnds (D,bnds)
	    in
	      shape_of (D,body)
	    end

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
		  PpNil.pp_kind other; print "\n";
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
			 PpNil.pp_kind cfun_kind; print "\n";
			 error (locate "shape_of") "Invalid kind for constructor application")
	     in body_kind
	     end
	 | (Typecase_c {arg,arms,default,kind}) => make_shape (D,kind)
	 | (Annotate_c (annot,con)) => shape_of (D,con))
       end 
   and kind_of_bnds (D : context, bnds : conbnd list) : context = 
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


   fun insert_kind (context as {conmap,kindmap,counter}:context,var,kind) = 
     let
       val _ =  
	 if !debug then
	   assert (locate "insert_kind")
	   [
	    (not (contains kindmap var),
		 var_error var),
	    (allBound_k kindmap kind,
		 fn () => (PpNil.pp_kind kind;
			   print "Kind contains variables not found in context"))
	    ]
	 else (); 
       fun thunk() = make_shape (context,kind)
       val entry = {eqn = NONE,
		    kind = immediate kind,
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
	   assert (locate "insert_kind")
	   [
	    (not (contains kindmap var),
		 var_error var),
	    (allBound_k kindmap kind,
		 fn () => (PpNil.pp_kind kind;
			   print "Kind contains variables not found in context")),
	    (allBound_k kindmap shape,
		 fn () => (PpNil.pp_kind shape;
			   print "Shape kind contains variables not found in context")),
	    (is_shape shape, 
	     fn () => (PpNil.pp_kind shape;
		       print "Shape kind is not a shape"))
	    ]
	 else ();

       val entry = {eqn = NONE,
		    kind = immediate kind,
		    shape = immediate shape,
		    index = counter}
     in
       {conmap = conmap, 
	counter = counter+1,
	kindmap = V.insert (kindmap, var, entry)}
     end

   fun insert_kind_shape_equation ({conmap,kindmap,counter}:context,var,con,kind,shape) = 
     let
       val _ =  
	 if !debug then
	   assert (locate "insert_kind")
	   [
	    (not (contains kindmap var),
		 var_error var),
	    (allBound_c kindmap con,
		 fn () => (PpNil.pp_con con;
			   print "Constructor contains variables not found in context")),
	    (allBound_k kindmap kind,
		 fn () => (PpNil.pp_kind kind;
			   print "Kind contains variables not found in context")),
	    (allBound_k kindmap shape,
		 fn () => (PpNil.pp_kind shape;
			   print "Shape kind contains variables not found in context")),
	    (is_shape shape, 
	     fn () => (PpNil.pp_kind shape;
		       print "Shape kind is not a shape"))
	    ]
	 else ();

       val entry = {eqn = SOME con,
		    kind = immediate kind,
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
		 fn () => (PpNil.pp_con con;
			   print "Constructor contains variables not found in context")),
	    (allBound_k kindmap kind,
		 fn () => (PpNil.pp_kind kind;
			   print "Kind contains variables not found in context"))
	    ]
	 else ();
       fun thunk() = make_shape (context,kind)
       val entry = {eqn = SOME con,
		    kind = immediate kind,
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
			fn () => (PpNil.pp_con con;
				  print "Constructor contains variables not found in context"))
		   ]
	 else ();
       fun kthunk() = kind_of (context,con)
       val kind = delay kthunk
       fun sthunk() = make_shape (context,thaw kind)
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


   and strip_singleton (D : context,kind : kind) : kind = 
     (case kind
	of Singleton_k con => kind_of (D,con)
	 | _ => kind)
   and kind_of (D : context,constructor : con) : kind = 
     let 
       val _ = 
	 if !debug then
	   assert (locate "kind_of") 
		  [
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

	 | (v as (Var_c var)) => 
	    (find_kind (D,var)
	     handle Unbound =>
	       (print_context D;
		error (locate "kind_of") ("variable "^(var2string var)^" not in context")))

	 | Let_c (sort,bnds,body) => 
	    let
	      fun folder (bnd,D) = 
		(case bnd 
		   of Open_cb (var,formals,body,body_kind) => 
		     insert_shape (D,var,Arrow_k(Open,formals,body_kind))
		    | Code_cb (var,formals,body,body_kind) => 
		     insert_shape (D,var,Arrow_k(Code,formals,body_kind))
		    | Con_cb (var,con) =>
		     insert_shape (D,var,kind_of (D,con)))
	      val D = List.foldl folder D bnds
	    in
	      kind_of (D,body)
	    end

	 | (Closure_c (code,env)) => 
	    let 
	      val (vklist,body_kind) = 
		(case strip_singleton (D,kind_of (D,code))
		   of Arrow_k (Code,vklist,body_kind) => (vklist,body_kind)
		    | Arrow_k (ExternCode,vklist,body_kind) => (vklist,body_kind)
		    | _ => (error (locate "kind_of") "Invalid closure: code component does not have code kind" ))
	    in 
	      Arrow_k(Closure,Listops.butlast vklist,body_kind)
	    end

	 | (Crecord_c entries) => 
	  let
	    val (labels,cons) = unzip entries
	    val kinds = (map (curry2 kind_of D) cons)
	    val k_entries = 
		   map2 (fn (l,k) => ((l,fresh_named_var "crec_norm"),k)) (labels,kinds)
	    val entries = zip labels cons
	  in Record_k (Sequence.fromList k_entries)
	  end

	 | (Proj_c (rvals,label)) => 
	  let
	    val kind = 
	      (case strip_singleton (D,kind_of (D,rvals))
		 of Record_k lvk_seq => 
		   let 
		     val lvk_list = Sequence.toList lvk_seq
		     fun loop (subst,[]) = error (locate "kind_of") "missing label"
		       | loop (subst,((l,v),k)::rest) = 
		       if (Name.eq_label(label,l))
			 then substConInKind subst k
		       else loop (Subst.add subst (v,Proj_c(rvals,l)),rest)
		   in  loop (Subst.empty(),lvk_list)
		   end
		  | other => 
		   (print "Non-record kind returned from shape_of in projection:\n";
		    PpNil.pp_kind other; print "\n";
		    error (locate "kind_of") "Non-record kind returned from shape_of in projection"))

	  in kind
	  end
	 | (App_c (cfun,actuals)) => 
	     let
	       val body_kind = 
		 (case strip_singleton (D,kind_of (D,cfun)) of
		    (Arrow_k (_,formals,body_kind)) => 
		      let 
			fun folder ((v,k),c,subst) = Subst.add subst (v,c)
			val subst = Listops.foldl2 folder (Subst.empty()) (formals,actuals)
		      in  
			substConInKind subst body_kind
		      end
		  | cfun_kind => (print "Invalid kind for constructor application\n";
				  PpNil.pp_kind cfun_kind; print "\n";
				  error (locate "kind_of") "Invalid kind for constructor application"))
	     in body_kind
	     end
	 | (Typecase_c {arg,arms,default,kind}) => kind
	 | (Annotate_c (annot,con)) => kind_of (D,con))
     end
   and find_kind (context as {kindmap,...}:context,var) = 
     (case (V.find (kindmap, var)) of
	   SOME {eqn,kind,shape,index} => thaw kind
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
			   else loop (Subst.add subst (v,Proj_c(c,l'))) rest
		 in  loop subst lvk_list
		 end
	    | Singleton_k c => (CON(Proj_c(c,l)), subst)
	    | _ => 
		 (print "bad kind to project from = \n";
		  PpNil.pp_kind k;
		  print "\n\n";
		  error (locate "find_kind_equation.project_kind") 
		  "bad kind to project_kind"))

	 fun app_kind(k,c1,clist,subst) = 
	   (case k 
	      of Arrow_k (openness,vklist, k) => 
		let 
		  fun folder (((v,k),c),subst) = Subst.add subst (v,c)
		in  
		  (KIND k, foldl folder subst (Listops.zip vklist clist))
		end
	       | Singleton_k c => (CON(App_c(c,clist)), subst)
	       | _ => error (locate "app_kind") "bad kind to app_kind")

	 fun traverse c : result * con subst = 
	   (case c 
	      of (Var_c v) => 
		(case V.find(kindmap,v) 
		   of SOME {eqn,kind,shape,index} => 
		     (case eqn
			of SOME c => (CON c, Subst.empty())
			 | NONE => 
			  if delayed kind then
			    raise Opaque
			  else
			    (KIND (thaw kind), Subst.empty()))
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
			PpNil.pp_con c; print "\n"; *)
		       raise Opaque))

     in 
       (case traverse con 
	  of (CON c, _) => SOME c
	   | (KIND (Singleton_k c),subst) => SOME (Subst.substConInCon subst c)
	   | _ => NONE)

	  handle Opaque => NONE
	       | e => (print "find_kind_equation given = \n";
		       PpNil.pp_con con; print "\n"; raise e)
     end


end
