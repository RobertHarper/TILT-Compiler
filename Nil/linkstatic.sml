
structure Prim = Prim()

structure Nil = Nil(structure Annotation = Annotation
		    structure Prim = Prim)

structure NilUtil = NilUtilFn(structure Nil = Nil)
  
structure NilStatic = NilStaticFn(structure Annotation = Annotation
				  structure Prim = Prim
				  structure Nil = Nil
				  structure NilUtil = NilUtil
				  structure Cont = Cont)
