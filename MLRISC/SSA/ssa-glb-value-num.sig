signature SSA_GLOBAL_VALUE_NUMBERING =
sig

   structure SSA : SSA

   val computeValueNumbers : SSA.ssa -> int Array.array
   val top : int

end
