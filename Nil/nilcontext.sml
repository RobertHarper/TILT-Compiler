functor NilContextFn(structure ArgNil : NIL
		     structure PPNil : PPNIL
		     structure Cont : CONT
		     sharing PPNil.Nil = ArgNil) :> 
  sig include NILCONTEXT sharing Nil = ArgNil end = 
struct
  structure Nil = ArgNil

  type kind = Nil.kind
  type con = Nil.con
  type var = Nil.var
  val find = HashTable.find
  val remove = HashTable.remove
  val insert = HashTable.insert
  val lookup = HashTable.lookup
  val appi = HashTable.appi
  val var2string = Name.var2string
  val c_insert = Cont.c_insert
  val c_remove = Cont.c_remove
  val c_foldl = Cont.c_foldl
  val c_insert_list = Cont.c_insert_list
  val mapsequence = Util.mapsequence

  fun error s = Util.error "nilcontext.sml" s

  exception NotFound


  type 'a map = (Nil.var,'a) HashTable.hash_table

  type context = {kindmap : kind map,
		  conmap : con map}

  fun empty ():context = 
    {kindmap = Name.mk_var_hash_table (1000,NotFound),
     conmap = Name.mk_var_hash_table (1000,NotFound)}

  fun insert_con ({conmap,...}:context,var,con) = 
    (case find conmap var
       of NONE => insert conmap (var,con)
	| _ => error ("Expression variable "^(var2string var)^"already in context"))

  fun find_con ({conmap,...}:context,var) = find conmap var
    
  fun remove_con ({conmap,...}:context,var) = ignore (remove conmap var)


  local
    open Nil
  in
    fun selfify (con,kind) =
      (case kind 
	 of Type_k phase => Singleton_k(phase,Type_k phase,con)
	  | Word_k phase => Singleton_k(phase,Word_k phase,con)
	  | Singleton_k(_) => kind
	  | Record_k entries => 
	   Record_k (mapsequence (fn ((l,v),k) => ((l,v),selfify (Proj_c (con,l),k))) entries)
	  | Arrow_k (openness,args,return) => 
	   let
	     val (formal_vars,_) = ListPair.unzip args
	     val actuals = List.map Var_c formal_vars
	   in
	     Arrow_k (openness,args,selfify(App_c (con,actuals),return))
	   end)

    fun insert_kind ({kindmap,...}:context,var,kind) = 
      (case find kindmap var
	 of NONE => insert kindmap (var,selfify(Var_c var,kind))
	  | _ => error ("Constructor variable "^(var2string var)^"already in context"))
  end
       
  fun find_kind ({kindmap,...}:context,var) = find kindmap var
    
  fun remove_kind ({kindmap,...}:context,var) = ignore (remove kindmap var)

  fun c_insert_con ({conmap,kindmap}:context,var,con,k) = 
    c_insert (conmap,var,con,fn conmap => k {conmap=conmap,kindmap=kindmap})

  fun c_remove_con ({conmap,kindmap}:context,var,k) = 
    c_remove (conmap,var,fn conmap => k {conmap=conmap,kindmap=kindmap})

  fun c_insert_kind ({kindmap,conmap}:context,var,kind,k) = 
    c_insert (kindmap,var,kind,fn kindmap => k {conmap=conmap,kindmap=kindmap})

  fun c_remove_kind ({kindmap,conmap}:context,var,k) = 
    c_remove (kindmap,var,fn kindmap => k {conmap=conmap,kindmap=kindmap})

  fun c_insert_con_list ({conmap,kindmap}:context,cons,k) = 
    c_insert_list (conmap,cons,fn conmap => k {conmap=conmap,kindmap=kindmap})

  fun c_insert_kind_list ({kindmap,conmap}:context,kinds,k) = 
    c_insert_list (kindmap,kinds,fn kindmap => k {conmap=conmap,kindmap=kindmap})

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
     PPNil.pp_kind kind;
     print "\n")

  fun print_con (var,con) =
    (print (Name.var2string var);
     print ":";
     PPNil.pp_con con;
     print "\n")

  fun print_context ({kindmap,conmap}:context) = 
    (print "\n Constructor variables and kinds are :\n";
     appi print_kind kindmap;
     print "\n Expression variables and constructors are :\n";
     appi print_con conmap)

  fun print_kinds ({kindmap,...}:context) = 
    (print "\n Constructor variables and kinds are :\n";
     appi print_kind kindmap)

  fun print_cons ({conmap,...}:context) = 
    (print "\n Expression variables and constructors are :\n";
     appi print_con conmap)

end 
