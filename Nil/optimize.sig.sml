(*$import Nil *)
signature OPTIMIZE = 
  sig
      val debug : bool ref
      val optimize :  {dead : bool,
		       projection : bool,
		       uncurry : bool,
		       cse : bool} -> Nil.module -> Nil.module
  end