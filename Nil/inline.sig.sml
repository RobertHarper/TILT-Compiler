(* Inline functions that are non-recursive and either are called once
   or else are sufficiently small and called a sufficiently small number
   of times.
*)

signature INLINE =
 sig
     val InlineDiag : bool ref
     val debug : bool ref
     (* Print debug information *)

     val inline_once : (bool * bool) -> Nil.module -> Nil.module
     (* Inline all functions called only once.  
      * inlince_once (inline_cons,iterate) nilmod
      * If inlince_cons is true, do con functions.
      * If iterate is true, then iterate until a fixed point is reached.
      *)

     val inline : {iterate : bool,
		   inlinecons : bool,
		   tinyThreshold : int,
		   sizeThreshold : int,
		   occurThreshold : int} -> Nil.module -> Nil.module
     (* Inline functions that don't escape and either:
      *    1) Are not recursive and are called only once
      * or 2) Have body size no more than sizeThreshold and no more than occurThreshold calls
      *
      * In addition, inline any applications of tiny functions (functions whose body is
      * smaller than tinyThreshold), whether they escape or not. At some point, the size
      * of the application is comparable to the size of the function body, in which
      * case inlining is pretty much always a win.   This should not be iterated
      * however, since this can cause exponential blowup.
      *
      * If iterate is true and any inlining was done in the first pass, call inline_once
      * iteratively to clean up.
      *
      *)


 end
