signature SSA_CONSTANT_FOLDING =
sig

   structure SSA : SSA

   type valnum = int

   (* special value numbers *)
   val bot      : valnum 
   val top      : valnum  (* uninitialized *)
   val volatile : valnum  (* volatile value *)
   val zero     : valnum  (* integer zero *)
   val one      : valnum  (* integer one *)

   (* constant folding and algebraic simplification *)
   val constantFolding : SSA.ssa -> 
        (SSA.exp * valnum list * 'a -> valnum) ->
         SSA.exp * valnum list * 'a -> valnum

   (* create a exp/operand hash table *)
   val hashTable : int * exn -> (SSAExp.exp * valnum list, 'a) HashTable.table  

end
