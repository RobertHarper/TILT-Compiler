(*$import Stats Option Ppnil NilSubst NilUtil NILCONTEXT *)

structure NilContext
   :> NILCONTEXT where type 'a subst = 'a NilSubst.subst = 
 struct


   open Nil 
   open Prim

   type 'a subst = 'a NilSubst.subst

   (* IMPORTS *)
   val substConInKind = NilSubst.substConInKind
   val substConInCon = NilSubst.substConInCon

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

   val curry2 = Util.curry2
   val printl = Util.printl
   val lprintl = Util.lprintl
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
		   std_kind : kind delay,
		   index : int}

   type context = 
     {kindmap : k_entry map,
      conmap  : con map,
      counter : int}

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

   fun print_entry (var,{eqn,kind,std_kind,index}:k_entry) =
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

   fun find_std_kind (context as {kindmap,...}:context,var) = 
     (case (V.find (kindmap, var)) of
	   SOME {std_kind,...} => thaw std_kind
	 | NONE => raise Unbound)

   fun find_kind (context as {kindmap,...}:context,var) = 
     (case (V.find (kindmap, var)) of
	   SOME {kind,...} => kind
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
		 fn () => (Ppnil.pp_kind kind;
			   lprintl "Kind contains variables not found in context"))
	    ]
	 else (); 
       fun thunk() = kind_standardize (context,kind)
       val entry = {eqn = NONE,
		    kind = kind,
		    std_kind = delay thunk,
		    index = counter}
     in
       {conmap = conmap, 
	counter = counter+1,
	kindmap = V.insert (kindmap, var, entry)}
     end

  and kind_standardize(D : context, kind : kind) : kind = 
    let 
      
      val res = 
	(case kind of
	   Type_k => Type_k
	 | SingleType_k con => kind
	 | Single_k con => kind_of(D,con)
	 | Record_k elts => 
	     let
	       fun folder (((label,var),kind),D) = 
		 let
		   val kind = kind_standardize(D,kind)
		   val D = insert_kind(D,var,kind)
		 in
		   (((label,var),kind),D)
		 end
	       val (elts,D) = Sequence.foldl_acc folder D elts
	     in
	       Record_k elts
	     end
	 | Arrow_k (openness, formals, return) => 
	     let
	       fun folder ((var,kind),D) = 
		 let
		   val kind = kind_standardize(D,kind)
		   val D = insert_kind(D,var,kind)
		 in
		   ((var,kind),D)
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
		NilUtil.selfify(v,kind)
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
		      val subst = NilSubst.add subst (var,con)
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
			    val subst = NilSubst.add subst (var,substConInCon subst con)
			  in
			    loop(rest,(D,subst))
			  end)
		end
	      val (subst,kind) = loop (bnds,(D,NilSubst.empty()))
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
	       val body_kind = NilSubst.varConKindSubst v env body_kind
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
		 of Record_k lvk_seq => NilUtil.project_from_kind (lvk_seq,rvals,label)
		  | other => 
		   (print "Non-record kind returned from kind_of in projection:\n";
		    Ppnil.pp_kind other; print "\n";
		    error  (locate "kind_of") "Non-record kind returned from kind_of in projection"))
		 
	    | (App_c (cfun,actuals)) => 
	      (case kind_of (D,cfun) of
		 (Arrow_k (_,formals,body_kind)) => 
		   let 
		     fun folder ((v,k),c,subst) = NilSubst.add subst (v,c)
		     val subst = Listops.foldl2 folder (NilSubst.empty()) (formals,actuals)
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
       fun thunk() = kind_standardize (context,kind)
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
       fun sthunk() = kind_standardize (context,kind)
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
      datatype result = CON of con | KIND of kind
      exception Opaque
      

      fun project_kind(k,c,l,subst) = 
	(case k of
	   Record_k lvk_seq => 
	     let 
	       val lvk_list = Sequence.toList lvk_seq
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
 end
