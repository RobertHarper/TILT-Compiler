structure Vectorops :> VECTOROPS = 
  struct
    fun maplist f v = Vector.foldr (fn (a,l) => (f a)::l) [] v
    fun mapPartialList f v = 
      Vector.foldr (fn (a,l) => (case (f a) of SOME v => v::l | NONE => l)) [] v
    fun mapilist f v = Vector.foldri (fn (i,a,l) => (f (i,a))::l) [] (v,0,NONE)
    fun mapiPartialList f v = 
      Vector.foldri (fn (i,a,l) => (case (f (i,a)) of SOME v => v::l | NONE => l)) [] (v,0,NONE)

  end