signature GML =
sig

  datatype exp =
      Var of string
		| Binder of string
		| Bool of bool
		| Int of int
		| Real of real
		| String of string
		| Fun of exp list
		|	Array of exp vector

end
