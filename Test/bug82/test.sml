(*$import *)
	fun g {a,b} = a + b + 3

	fun f thunk = 
	  let val {a, ...} = thunk  
	  in  g thunk
	  end
