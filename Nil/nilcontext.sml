functor NilContextFn'(structure ArgNil : NIL
		      structure PpNil : PPNIL
		      structure Cont : CONT_SIG
		      structure NilUtil : NILUTIL
		      structure Subst : NILSUBST
		      sharing PpNil.Nil = NilUtil.Nil = ArgNil
		        and type Subst.con = ArgNil.con
		        and type Subst.exp = ArgNil.exp
		        and type Subst.kind = ArgNil.kind) :(*>*)
  NILCONTEXT' where Nil = ArgNil 
	      and type 'a subst = 'a Subst.subst = 
struct
  structure Nil = ArgNil

  type kind = Nil.kind
  type con = Nil.con
  type var = Nil.var
  type 'a subst = 'a Subst.subst

  val var2string = Name.var2string
  val mapsequence = Util.mapsequence
  val get_phase = NilUtil.get_phase
  val selfify = NilUtil.selfify    
  val pull = NilUtil.pull

  val Var_c = Nil.Var_c

  fun error s = Util.error "nilcontext.sml" s

  val profile = Stats.bool "nil_profile"
  val (nilcontext_kinds_bound,
       nilcontext_kinds_renamed) =
    (Stats.counter "nilcontext_kinds_bound",
     Stats.counter "nilcontext_kinds_renamed")
    
  structure V = Name.VarMap
    
  type 'a map = 'a V.map
    
  structure S = Name.VarSet

  type set = S.set

  (*The con in the kindmap is the result of pulling
   * and normalizing the kind.  
   *)
  type context = {kindmap : (con * kind) map,
		  conmap : con map,
		  top_level : bool,
		  c_binds_top : set,
		  e_binds_top : set}

  fun empty ():context = 
    {kindmap = V.empty,
     conmap = V.empty,
     top_level = true,
     c_binds_top = S.empty,
     e_binds_top = S.empty}

  fun leave_top_level ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context) : context = 
    {conmap = conmap,kindmap = kindmap,
     top_level = false,
     c_binds_top = c_binds_top,e_binds_top = e_binds_top}

  fun code_context ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context) : context = 
    let 
      fun filter set (v,a) = S.member (set,v)
    in
      {conmap = V.filteri (filter e_binds_top) conmap,
       kindmap = V.filteri (filter c_binds_top) kindmap,
       top_level = false,
       c_binds_top = c_binds_top,
       e_binds_top = e_binds_top}
    end


  fun insert_con ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context,var,con) = 
    {conmap = V.insert (conmap, var, con), 
     kindmap = kindmap,
     top_level = top_level,
     c_binds_top = c_binds_top,
     e_binds_top = if top_level then S.add (e_binds_top, var) else e_binds_top}

  fun insert_code_con ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context,code_var,con) = 
    {conmap = V.insert (conmap, code_var, con), 
     kindmap = kindmap,
     top_level = top_level,
     c_binds_top = c_binds_top,
     e_binds_top = S.add (e_binds_top, code_var)}

  fun insert_con_list (C:context,defs : (var * con) list) =
    List.foldl (fn ((v,c),C) => insert_con (C,v,c)) C defs

  fun insert_code_con_list (C:context,defs : (var * con) list) =
    List.foldl (fn ((v,c),C) => insert_code_con (C,v,c)) C defs

  fun find_con ({conmap,...}:context,var) = V.find (conmap, var)
    
  fun remove_con ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context,var) = 
    let
      val conmap = #1 (V.remove (conmap, var))
      val e_binds_top = (S.delete (e_binds_top, var)
			 handle LibBase.NotFound => e_binds_top)
    in
      {conmap = conmap, kindmap = kindmap,
       top_level = top_level,
       c_binds_top = c_binds_top,
       e_binds_top = e_binds_top}
    end

  fun inject_kind ({kindmap,conmap,top_level,c_binds_top,e_binds_top}:context,var,con,kind) = 
    {kindmap = V.insert (kindmap,var,(con,kind)),
     conmap = conmap,
     top_level = top_level,
     c_binds_top = (if top_level then S.add (c_binds_top, var)
		    else c_binds_top),
     e_binds_top = e_binds_top}

  (* given two well-formed contexts,
    add the entries kindmap of ctxt2 to to the ones in ctxt1;
    if key is is present in both maps, that entry is not added *)
  fun context_addkindmap ({kindmap = km1, conmap,top_level,c_binds_top = cb1,e_binds_top} : context,
			  {kindmap = km2, c_binds_top = cb2, ...} : context) = 
      {kindmap = V.unionWith (fn (e1,e2) => e1) (km1,km2),
       conmap = conmap,
       top_level = top_level,
       c_binds_top = S.union (cb1,cb2) ,
       e_binds_top = e_binds_top}

  (* NOTE RE: kind insertion -
   *         The context maintains with every kind the
   * most precisely defined constructor that can be maintained for the
   * variable, based on it's kind.  The is created by normalizing the
   * result of "pulling" with the variables from the kind.  Note
   * though that the call to normalize must be made with the variable
   * bound, since the variable being bound may occur in the pulled
   * version.  Therefore, the normalization of the pulled constructor
   * is done with var = Var_c var :: selfify(Var_c var,kind) bound in
   * the context.  It can be shown that if the variable is looked up
   * in the process of normalizing it's pulled version, then there can
   * be no extra information extracted by trying to recursively pull,
   * and it is sufficient to return the variable itself.  (All
   * occurrences of a variable in it's pulled version will be at
   * Singleton's that were opaque.  Therefore we do not lose
   * information by simply returning the variable itself in this case.)
   * Trying to normalize and pull on demand in this will cause a loop.
  *)

  val empty_subst = Subst.empty
  val subst_compose = Subst.con_subst_compose
  val subst_add = Subst.add

  fun bind_kind normalizer (D:context,var,kind) =
    let
      val _ = if !profile then (nilcontext_kinds_renamed();
				nilcontext_kinds_bound ()) else ()
      val var' = Name.derived_var var
      val var_con = Var_c var'
      val kind = selfify(var_con,kind)
      val D' = inject_kind (D,var',var_con,kind)
      val con = normalizer D' (pull(var_con,kind))
    in
      (inject_kind(D,var',con,kind),
       var',
       subst_add (empty_subst ()) (var,Var_c var'))
    end

  fun insert_kind normalizer (D as {kindmap,...}:context,var,kind) = 
    (case V.find (kindmap, var)
       of NONE => 
	 let
	   val var_con = Var_c var
	   val kind = selfify(var_con,kind)
	   val D' = inject_kind (D,var,var_con,kind)
	   val con = normalizer D' (pull(var_con,kind))
	 in
	   inject_kind(D,var,con,kind)
	 end
	| _ => error ("Constructor variable "^(var2string var)^" already in context"))

  fun insert_kind_list normalizer = 
    let 
      val insert_kind = insert_kind normalizer
      fun insert_list' (C:context,defs : (var * kind) list) =
	List.foldl (fn ((v,k),C) => insert_kind (C,v,k)) C defs
    in
      insert_list'
    end

  fun find_kind ({kindmap,...}:context,var) = 
    (case (V.find (kindmap, var))
       of SOME(c,k) => SOME k
	| NONE => NONE)
    
  fun find_kind' (D as {kindmap,...}:context,var) = V.find (kindmap, var)
	 
  fun remove_kind ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context,var) = 
    let
      val kindmap = #1 (V.remove (kindmap, var))
      val c_binds_top = (S.delete (c_binds_top, var)
			 handle LibBase.NotFound => c_binds_top)
    in
      {conmap = conmap, 
       kindmap = kindmap,
       top_level = top_level,
       c_binds_top = c_binds_top,
       e_binds_top = e_binds_top}
    end

  fun bind_kind_list normalizer = 
    let
      val bind_kind = bind_kind normalizer
      fun bind_kind_list (C:context,defs : (var * kind) list) 
	: (context * ((var * kind) list) * con subst) =
	let
	  fun folder ((var,kind),(C,rev_acc,subst)) = 
	    let
	      val kind = Subst.substConInKind subst kind
	      val (C,var,subst_one) = bind_kind (C,var,kind)
	    in
	      (C,(var,kind)::rev_acc,subst_compose(subst_one,subst))
	    end
	  
	  val (C,rev_acc,subst) = 
	    List.foldl folder (C,[],empty_subst()) defs
	in
	  (C,rev rev_acc,subst)
	end
    in
      bind_kind_list
    end

  fun c_insert_con (context,var,con,k) = 
      k (insert_con(context,var,con))

  fun c_insert_kind normalizer (context,var,kind,k) = 
      k (insert_kind normalizer (context,var,kind))

  fun c_insert_con_list (context,nil,k) = k context
    | c_insert_con_list (context,(v,c)::cs,k) = 
      c_insert_con_list(insert_con(context, v, c), cs, k)

  fun c_insert_kind_list normalizer (context,nil,k) = k context
    | c_insert_kind_list normalizer (context,(v,knd)::cs,k) = 
      c_insert_kind_list normalizer (insert_kind normalizer (context, v, knd), cs, k)

  fun foldli_kind f acc ({kindmap,...} : context) = 
    let
      fun f' (v,(c,k),acc) = f (v,k,acc)
    in
      V.foldli f' acc kindmap
    end

  fun print_con_kind (var,(con,kind)) =
    (print (Name.var2string var);
     print "=";
     PpNil.pp_con con;
     print "::";
     PpNil.pp_kind kind;
     print "\n")

  fun print_con (var,con) =
    (print (Name.var2string var);
     print ":";
     PpNil.pp_con con;
     print "\n")

  fun print_context ({kindmap,conmap,top_level,c_binds_top,e_binds_top}:context) = 
    (print (if top_level then "\nTOPLEVEL CONTEXT\n" else "\nCONTEXT\n");
     print "\n Constructor variables and kinds are :\n";
     V.appi print_con_kind kindmap;
     print "\n Expression variables and constructors are :\n";
     V.appi print_con conmap)

  fun print_kinds ({kindmap,...}:context) = 
    (print "\n Constructor variables and kinds are :\n";
     V.appi print_con_kind kindmap)

  fun print_cons ({conmap,...}:context) = 
    (print "\n Expression variables and constructors are :\n";
     V.appi print_con conmap)

end 


functor NilContextFn(structure NilContext' : NILCONTEXT'
		     structure Normalize : NORMALIZE
		     sharing type NilContext'.subst = Normalize.subst
			 and type Normalize.context = NilContext'.context
			 and type Normalize.con = NilContext'.Nil.con 
		         and type Normalize.exp = NilContext'.Nil.exp
			 and type Normalize.kind = NilContext'.Nil.kind) :(*>*)
   NILCONTEXT where Nil = NilContext'.Nil
	      and type 'a subst = 'a NilContext'.subst = 
struct
  open NilContext'
  val con_norm = Normalize.con_normalize
  val insert_kind = insert_kind con_norm
  val insert_kind_list = insert_kind_list con_norm
  val bind_kind = bind_kind con_norm
  val bind_kind_list = bind_kind_list con_norm
  fun c_insert_kind args = NilContext'.c_insert_kind con_norm args
  fun c_insert_kind_list args = NilContext'.c_insert_kind_list con_norm args
end