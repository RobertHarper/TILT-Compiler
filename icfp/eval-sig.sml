
signature EVAL =
sig

		type ? = int

		type v3 = real * real * real
		type color = v3

		datatype light =
				Sunlight of v3 * color
			| Pointlight of v3 * color
			| Spotlight of { pos : v3,
											 dir : v3,
											 color : color,
											 cutoff : real, (* half of the cone in degrees *)
											 att : real }

		datatype value =
				Int of int
			| Bool of bool
			| String of string
			| Real of real
			| Array of value vector
			| Point of v3
			| Closure of closure
			| Object of obj * closure
			| Light of light
			| MLfun of (env * stack) -> (env * stack)

		withtype closure = env * Gml.exp list
		withtype env = value Envmap.map 
		withtype stack = value list

end
