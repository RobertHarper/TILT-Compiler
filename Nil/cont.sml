(*   Leaf Petersen, January 1997 *)

structure Cont :> CONT =
  struct

    type ('key,'val) table = ('key,'val) HashTable.hash_table

    val find = HashTable.find
    val remove = HashTable.remove
    val insert = HashTable.insert
    val lookup = HashTable.lookup

    (*Applies cont to the hash table with (k,v) inserted.  When cont *)
    (* returns, restores the original state of the tbl, and returns*)
    (* the value returned by cont *)
    fun c_insert (tbl,k,v,cont) = 
      let 
	val old = find tbl k
	fun undo (SOME old_v) = insert tbl (k,old_v)
	  | undo (NONE) = ((ignore (remove tbl k))
			   handle _ => ())
	val ans = (insert tbl (k,v);
		   ((cont tbl) 
		    handle any => (undo (old); 
				   raise any)))
      in
	undo(old);
	ans
      end
			 
    (*Applies cont to the hash table with k removed.  When cont *)
    (* returns, restores the original state of the tbl, and returns*)
    (* the value returned by cont.  raises the table's exception *)
    (* if k was not in tbl*)
    fun c_remove (tbl,k,cont) = 
      let 
	val oldVal = remove tbl k;
	val ans = (cont tbl 
		   handle any => (insert tbl (k,oldVal);raise any))
      in
	insert tbl (k,oldVal);
	ans
      end
    
    fun c_foldl fbase fcont init list = 
      let 
	fun iterate ([],state) = fbase state
	  | iterate ((fst::rest),state) = 
	  fcont (fst,state,(fn state => iterate (rest,state)))
      in
	iterate(list,init)
      end
    
    fun c_fold_acc fbase fsplit tbl = 
      let
	fun base (tbl,elts) = fbase (tbl,rev elts) 
	fun step (cur,(tbl,elts),k) = 
	  let
	    val (acc_elt,(key,value)) = fsplit (tbl,cur)
	  in
	    c_insert (tbl,key,value,fn tbl => k (tbl,acc_elt::elts))
	  end
      in
	c_foldl base step (tbl,[]) 
      end

    fun c_insert_list (tbl,[],cont) = cont tbl
      | c_insert_list (tbl,(key,value)::rest,cont) = 
      c_insert (tbl,key,value,fn nu_tbl => c_insert_list (nu_tbl,rest,cont))

  end (* Cont *)
