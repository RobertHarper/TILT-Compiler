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

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:44  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:14:49  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:18  pscheng
# *** empty log message ***
#
 *)
