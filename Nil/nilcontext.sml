functor NilContextFn(structure ArgNil : NIL
		     structure PpNil : PPNIL
		     structure Cont : CONT_SIG
		     structure NilUtil : NILUTIL
		     sharing PpNil.Nil = NilUtil.Nil = ArgNil) :(*>*)
   NILCONTEXT where structure Nil = ArgNil = 
struct
  structure Nil = ArgNil

  type kind = Nil.kind
  type con = Nil.con
  type var = Nil.var
  val var2string = Name.var2string
  val mapsequence = Util.mapsequence
  val get_phase = NilUtil.get_phase

  fun error s = Util.error "nilcontext.sml" s

  exception NotFound

  structure V = Name.VarMap

  type 'a map = 'a V.map

  type context = {kindmap : kind map,
		  conmap : con map}

  fun empty ():context = 
    {kindmap = V.empty,
     conmap = V.empty}

  fun insert_con ({conmap,kindmap}:context,var,con) = 
    (case V.find (conmap, var)
       of NONE => {conmap = V.insert (conmap, var, con), kindmap = kindmap}
	| _ => error ("Expression variable "^(var2string var)^" already in context"))

  fun insert_con_list (C:context,defs : (var * con) list) =
    List.foldl (fn ((v,c),C) => insert_con (C,v,c)) C defs

  fun find_con ({conmap,...}:context,var) = V.find (conmap, var)
    
  fun remove_con ({conmap,kindmap}:context,var) = 
      {conmap = #1 (V.remove (conmap, var)), kindmap = kindmap}

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
(*	   Record_k (mapsequence (fn ((l,v),k) => ((l,v),selfify (Proj_c (con,l),k))) entries)*)
	  | Arrow_k (openness,args,return) => 
	   Singleton_k(get_phase kind,kind,con)
    (*	   let
	     val (formal_vars,_) = ListPair.unzip args
	     val actuals = List.map Var_c formal_vars
	   in
	     Arrow_k (openness,args,selfify(App_c (con,actuals),return))
	   end
*))

    fun insert_kind ({kindmap,conmap}:context,var,kind) = 
      (case V.find (kindmap, var)
	 of NONE => {kindmap = V.insert (kindmap, var, selfify(Var_c var,kind)),
		     conmap = conmap}
	  | _ => error ("Constructor variable "^(var2string var)^" already in context"))
  end
       
  fun insert_kind_list (C:context,defs : (var * kind) list) =
    List.foldl (fn ((v,k),C) => insert_kind (C,v,k)) C defs

  fun find_kind ({kindmap,...}:context,var) = V.find (kindmap, var)
    
  fun remove_kind ({kindmap,conmap}:context,var) = 
      {kindmap = #1 (V.remove (kindmap, var)), conmap = conmap}

  fun c_insert_con (context,var,con,k) = 
      k (insert_con(context,var,con))

  fun c_remove_con (context,var,k) = 
      k (remove_con(context,var))

  fun c_insert_kind (context,var,kind,k) = 
      k (insert_kind(context,var,kind))

  fun c_remove_kind (context,var,k) = 
      k (remove_kind(context,var))

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

  fun print_context ({kindmap,conmap}:context) = 
    (print "\n Constructor variables and kinds are :\n";
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
