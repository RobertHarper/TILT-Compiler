(*$import Prelude Nil *)
signature INLINE =
 sig
     val debug : bool ref
     val inline : {sizeThreshold : int,
		   occurThreshold : int} -> Nil.module -> Nil.module
 end
