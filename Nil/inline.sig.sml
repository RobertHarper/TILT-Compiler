(*$import Nil *)
signature INLINE =
 sig
 val debug : bool ref
 val threshold : int ref (* max size of functions to be inlined *)
 val occur_threshold : int ref (* max # of occurrences *)
 val optimize : Nil.module -> (int * Nil.module)
 end
