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
  val c_insert = Cont.c_insert
  val c_remove = Cont.c_remove
  val c_foldl = Cont.c_foldl
  val c_fold_acc = Cont.c_fold_acc
  val c_insert_list = Cont.c_insert_list

  exception NotFound

  type 'a map = (Nil.var,'a) HashTable.hash_table

  type context = {kindmap : kind map,
		  conmap : con map}


  fun empty ():context = 
    {kindmap = Name.mk_var_hash_table (1000,NotFound),
     conmap = Name.mk_var_hash_table (1000,NotFound)}

  fun insert_con ({conmap,...}:context,var,con) = insert conmap (var,con)

  fun find_con ({conmap,...}:context,var) = find conmap var
    
  fun remove_con ({conmap,...}:context,var) = ignore (remove conmap var)

  fun insert_kind ({kindmap,...}:context,var,kind) = 
    insert kindmap (var,kind)

  fun find_kind ({kindmap,...}:context,var) = find kindmap var
    
  fun remove_kind ({kindmap,...}:context,var) = ignore (remove kindmap var)

  fun c_insert_con ({conmap,kindmap}:context,var,con,k) = 
    c_insert (conmap,(var,con),fn conmap => k {conmap=conmap,kindmap=kindmap})

  fun c_remove_con ({conmap,kindmap}:context,var,k) = 
    c_remove (conmap,var,fn conmap => k {conmap=conmap,kindmap=kindmap})

  fun c_insert_kind ({kindmap,conmap}:context,var,kind,k) = 
    c_insert (kindmap,(var,kind),fn kindmap => k {conmap=conmap,kindmap=kindmap})

  fun c_remove_kind ({kindmap,conmap}:context,var,k) = 
    c_remove (kindmap,var,fn kindmap => k {conmap=conmap,kindmap=kindmap})

  fun c_insert_con_list ({conmap,kindmap}:context,cons,k) = 
    c_insert_list (conmap,cons,fn conmap => k {conmap=conmap,kindmap=kindmap})

  fun c_insert_kind_list ({kindmap,conmap}:context,kinds,k) = 
    c_insert_list (kindmap,kinds,fn kindmap => k {conmap=conmap,kindmap=kindmap})


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
