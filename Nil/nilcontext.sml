functor NilContextFn(structure ArgNil : NIL
		     structure PpNil : PPNIL
		     structure Cont : CONT_SIG
		     structure NilUtil : NILUTIL
		     structure Subst : NILSUBST
		       sharing PpNil.Nil = NilUtil.Nil = ArgNil
		       and type Subst.con = ArgNil.con
		       and type Subst.exp = ArgNil.exp
		       and type Subst.kind = ArgNil.kind) :(*>*)
   NILCONTEXT where Nil = ArgNil 
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
    
  fun error s = Util.error "nilcontext.sml" s

  local fun id () = () 
  in
    val profile = Stats.bool "nil_profile"
    val (nilcontext_kinds_bound,
	 nilcontext_kinds_renamed) =
      (Stats.counter "nilcontext_kinds_bound",
       Stats.counter "nilcontext_kinds_renamed")
  end

  structure V = Name.VarMap

  type 'a map = 'a V.map

  type context = {kindmap : kind map,
		  conmap : con map,
		  top_level : bool,
		  c_binds_top : kind map,
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

  fun foldli_kind f acc ({kindmap,...} : context) = V.foldli f acc kindmap
    
  local
    open Nil
  in
    fun selfify (con,kind) =
      (case kind 
	 of Type_k phase => Singleton_k(phase,Type_k phase,con)
	  | Word_k phase => Singleton_k(phase,Word_k phase,con)
	  | Singleton_k(_) => kind
	  | Record_k entries => Singleton_k(get_phase kind,kind,con)
	  | Arrow_k (openness,args,return) => 
	   Singleton_k(get_phase kind,kind,con))
  end

  fun insert_kind ({kindmap,conmap,top_level,c_binds_top,e_binds_top}:context,var,kind) = 
    (case V.find (kindmap, var)
       of NONE => 
	 let
	   val kind = selfify(Nil.Var_c var,kind)
	 in
	   {kindmap = V.insert (kindmap, var, kind),
	    conmap = conmap,
	    top_level = top_level,
	    c_binds_top = (if top_level then V.insert (c_binds_top, var, kind) else c_binds_top),
	    e_binds_top = e_binds_top}
	 end
	| _ => error ("Constructor variable "^(var2string var)^" already in context"))

  fun insert_kind_list (C:context,defs : (var * kind) list) =
      List.foldl (fn ((v,k),C) => insert_kind (C,v,k)) C defs

  fun find_kind ({kindmap,...}:context,var) = V.find (kindmap, var)
    
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

  fun bind_kind ({kindmap,conmap,top_level,c_binds_top,e_binds_top}:context,var,kind) =
(*    let
      val kind = selfify(Nil.Var_c var,kind)
      val _ = nilcontext_kinds_bound ()
    in*)
(* Unfortunately, the way I handle code means that must always rename to avoid 
 * accidental capture.
 *)
  (*    case V.find (kindmap, var)
	 of NONE => 
	   ({kindmap = V.insert (kindmap, var, kind), 
	     conmap = conmap,
	     top_level = top_level,
	     c_binds_top = if top_level then V.insert (c_binds_top, var, kind) else c_binds_top,
	     e_binds_top = e_binds_top},
	    var,empty_subst())
	  | SOME k => *)
	   let
	     val var' = Name.derived_var var
	     val _ = if !profile then (nilcontext_kinds_renamed();
				       nilcontext_kinds_bound ()) else ()
	     val kind = selfify(Nil.Var_c var',kind)
	     val kindmap = V.insert (kindmap, var', kind)
	     val c_binds_top = if top_level then V.insert (c_binds_top, var', kind) else c_binds_top
	   in
	     ({kindmap = kindmap, conmap = conmap,
	       top_level = top_level,
	       c_binds_top = c_binds_top, e_binds_top = e_binds_top},
	      var',
	      subst_add (empty_subst ()) (var,Nil.Var_c var'))
	   end

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

  fun c_insert_con (context,var,con,k) = 
      k (insert_con(context,var,con))

  fun c_insert_kind (context,var,kind,k) = 
      k (insert_kind(context,var,kind))

  fun c_insert_con_list (context,nil,k) = k context
    | c_insert_con_list (context,(v,c)::cs,k) = 
      c_insert_con_list(insert_con(context, v, c), cs, k)

  fun c_insert_kind_list (context,nil,k) = k context
    | c_insert_kind_list (context,(v,knd)::cs,k) = 
      c_insert_kind_list(insert_kind(context, v, knd), cs, k)

  fun c_foldl fbase fcont init list = 
      let 
	  fun iterate ([],state) = fbase state
	    | iterate ((fst::rest),state) = 
	      fcont (fst,state,(fn state => iterate (rest,state)))
      in
	  iterate(list,init)
      end

  fun c_fold_acc ins_fun fbase fsplit tbl = 
    let
      fun base (tbl,elts) = fbase (tbl,rev elts) 
      fun step (cur,(tbl,elts),k) = 
	let
	  val (acc_elt,(key,value)) = fsplit (tbl,cur)
	in
	  ins_fun (tbl,key,value,fn tbl => k (tbl,acc_elt::elts))
	end
    in
      c_foldl base step (tbl,[]) 
    end

  fun c_fold_kind_acc fbase fsplit tbl = c_fold_acc c_insert_kind fbase fsplit tbl 
  fun c_fold_con_acc fbase fsplit tbl = c_fold_acc c_insert_con fbase fsplit tbl

  fun print_kind (var,kind) =
    (print (Name.var2string var);
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
     V.appi print_kind kindmap;
     print "\n Expression variables and constructors are :\n";
     V.appi print_con conmap)

  fun print_kinds ({kindmap,...}:context) = 
    (print "\n Constructor variables and kinds are :\n";
     V.appi print_kind kindmap)

  fun print_cons ({conmap,...}:context) = 
    (print "\n Expression variables and constructors are :\n";
     V.appi print_con conmap)

end 
