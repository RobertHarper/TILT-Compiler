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

  (*The con option in the kindmap is the result of pulling
   * and normalizing the kind.  It is an option, because the
   * call to normalize must be made with the variable bound,
   * since the variable being bound may occur in the pulled version
   * If a variable is looked up, and it's con is NONE, then it must be
   * the case (by construction) that we are in the process of normalizing
   * pull of that variable and it's kind.  All occurrences of a variable 
   * in it's pulled version will be at Singleton's that were opaque.
   * Therefore we do not lose information by simply returning the variable 
   * itself in this case.  Trying to normalize and pull on demand in this 
   * will cause a loop.
   *)
  type context = {kindmap : (con option*kind) map,
		  conmap : con map,
		  top_level : bool,
		  c_binds_top : (con option*kind) map,
		  e_binds_top : con map}

  fun empty ():context = 
    {kindmap = V.empty,
     conmap = V.empty,
     top_level = true,
     c_binds_top = V.empty,
     e_binds_top = V.empty}

  fun leave_top_level ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context) : context = 
    {conmap = conmap,kindmap = kindmap,
     top_level = false,
     c_binds_top = c_binds_top,e_binds_top = e_binds_top}

  fun code_context ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context) : context = 
    {conmap = e_binds_top,kindmap = c_binds_top,
     top_level = false,
     c_binds_top = c_binds_top,e_binds_top = e_binds_top}

  fun insert_con ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context,var,con) = 
    {conmap = V.insert (conmap, var, con), 
     kindmap = kindmap,
     top_level = top_level,
     c_binds_top = c_binds_top,
     e_binds_top = if top_level then V.insert (e_binds_top, var, con) else e_binds_top}

  fun insert_code_con ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context,code_var,con) = 
    {conmap = V.insert (conmap, code_var, con), 
     kindmap = kindmap,
     top_level = top_level,
     c_binds_top = c_binds_top,
     e_binds_top = V.insert (e_binds_top, code_var, con)}

  fun insert_con_list (C:context,defs : (var * con) list) =
    List.foldl (fn ((v,c),C) => insert_con (C,v,c)) C defs

  fun insert_code_con_list (C:context,defs : (var * con) list) =
    List.foldl (fn ((v,c),C) => insert_code_con (C,v,c)) C defs

  fun find_con ({conmap,...}:context,var) = V.find (conmap, var)
    
  fun remove_con ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context,var) = 
    let
      val conmap = #1 (V.remove (conmap, var))
      val e_binds_top = (#1 (V.remove (e_binds_top, var))
			 handle LibBase.NotFound => e_binds_top)
    in
      {conmap = conmap, kindmap = kindmap,
       top_level = top_level,
       c_binds_top = c_binds_top,
       e_binds_top = e_binds_top}
    end

  fun insert_kind normalizer (D as {kindmap,conmap,top_level,c_binds_top,e_binds_top}:context,var,kind) = 
    (case V.find (kindmap, var)
       of NONE => 
	 let
	   val kind = selfify(Var_c var,kind)
	   val D' = {kindmap = V.insert (kindmap,var,(NONE,kind)),
		     conmap = conmap,top_level = top_level,
		     c_binds_top = (if top_level then V.insert (c_binds_top, var, (NONE,kind))
				    else c_binds_top),
		     e_binds_top = e_binds_top}
	   val con = normalizer D' (pull(Var_c var,kind))
	 in
	   {kindmap = V.insert (kindmap, var, (SOME con,kind)),
	    conmap = conmap,
	    top_level = top_level,
	    c_binds_top = (if top_level then V.insert (c_binds_top, var, (SOME con,kind)) else c_binds_top),
	    e_binds_top = e_binds_top}
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
    
  fun find_kind' (D as {kindmap,...}:context,var) = 
    (case V.find (kindmap, var)
       of SOME(SOME c,k) => SOME (c,k)
	| SOME(NONE,k) => SOME(Var_c var,k)
	| NONE => NONE)
	 
  fun remove_kind ({conmap,kindmap,top_level,c_binds_top,e_binds_top}:context,var) = 
    let
      val kindmap = #1 (V.remove (kindmap, var))
      val c_binds_top = (#1 (V.remove (c_binds_top, var))
			 handle LibBase.NotFound => c_binds_top)
    in
      {conmap = conmap, kindmap = kindmap,
       top_level = top_level,
       c_binds_top = c_binds_top,
       e_binds_top = e_binds_top}
    end

  val empty_subst = Subst.empty
  val subst_compose = Subst.con_subst_compose
  val subst_add = Subst.add

  fun bind_kind normalizer (D as {kindmap,conmap,top_level,c_binds_top,e_binds_top}:context,var,kind) =
    let
      val var' = Name.derived_var var
      val _ = if !profile then (nilcontext_kinds_renamed();
				nilcontext_kinds_bound ()) else ()
      val kind = selfify(Var_c var',kind)
      val D' = {kindmap = V.insert (kindmap,var',(NONE,kind)),
		conmap = conmap,top_level = top_level,
		c_binds_top = (if top_level then V.insert (c_binds_top, var', (NONE,kind)) 
			       else c_binds_top),
		e_binds_top = e_binds_top}
      val con = normalizer D' (pull (Var_c var',kind))
      val kindmap = V.insert (kindmap, var', (SOME con,kind))
      val c_binds_top = if top_level then V.insert (c_binds_top, var', (SOME con,kind)) else c_binds_top
    in
      ({kindmap = kindmap, conmap = conmap,
	top_level = top_level,
	c_binds_top = c_binds_top, e_binds_top = e_binds_top},
       var',
       subst_add (empty_subst ()) (var,Var_c var'))
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

  fun print_con_kind (var,(con_opt,kind)) =
    (print (Name.var2string var);
     (case con_opt 
	of SOME con => (print "=";
			PpNil.pp_con con)
	 | NONE => ());
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