signature CONT = 
  sig

    val c_insert : 
      ('key,'val) HashTable.hash_table * ('key * 'val) 
             * (('key,'val) HashTable.hash_table -> 'a) -> 'a

    val c_remove : 
      ('key,'val) HashTable.hash_table * 'key 
             * (('key,'val) HashTable.hash_table -> 'a) -> 'a

    val c_foldl : ('state -> 'a) -> ('elt * 'state * ('state -> 'a) -> 'a) 
      -> 'state -> 'elt list -> 'a

    val c_fold_acc : 
      ((('key,'val) HashTable.hash_table * 'acc_elt list) -> 'a)
      -> ((('key,'val) HashTable.hash_table * 'elt) 
	  -> ('acc_elt * ('key * 'val)))
      -> ('key,'val) HashTable.hash_table -> 'elt list -> 'a

    val c_insert_list : 
      ('key,'val) HashTable.hash_table * ('key * 'val) list
      * (('key,'val) HashTable.hash_table -> 'a) -> 'a
      
  end
