(*$import *)
(* infix 4 = *)
	signature S_dt = 
	  sig 
	    datatype foo = FOO
	  end

	functor F (structure A : S_dt
		   structure B : S_dt
		     sharing type A.foo = B.foo) = 
	  struct
	    val y = A.FOO = B.FOO
	  end
