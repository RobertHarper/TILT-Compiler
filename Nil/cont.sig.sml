signature CONT = 
  sig

    type ('key,'val) table = ('key,'val) HashTable.hash_table

    val c_insert : ('key,'val) table * 'key * 'val * (('key,'val) table -> 'a) -> 'a

    val c_remove : ('key,'val) table * 'key * (('key,'val) table -> 'a) -> 'a

    val c_foldl : ('state -> 'a) -> ('elt * 'state * ('state -> 'a) -> 'a) 
                                                     -> 'state -> 'elt list -> 'a
    val c_fold_acc : 
      ((('key,'val) table * 'acc_elt list) -> 'a)
      -> ((('key,'val) table * 'elt) -> ('acc_elt * ('key * 'val)))
         -> ('key,'val) table -> 'elt list -> 'a

    val c_insert_list : ('key,'val) table * ('key * 'val) list * (('key,'val) table -> 'a) -> 'a
      
  end
