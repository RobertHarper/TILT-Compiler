
signature EVAL =
sig

    type m4 = Matrix.m4
    type v3 = Matrix.v3

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
      | Object of obj 
      | Light of light
      | MLfun of (env * stack) -> (env * stack)
        
    and obj = 
        Sphere of m4 * closure
      | Cube of m4 * closure
      | Cone of m4 * closure
      | Cylinder of m4 * closure
      | Plane of m4 * closure
      | Union of obj * obj
      | Difference of obj * obj
      | Intersection of obj * obj
        
    withtype env = value Envmap.map 
    and closure = env * Gml.exp list
    and stack = value list

    structure T :
	sig
	    val opers : (stack -> stack) Envmap.map
	end

    val eval : Gml.exp list -> stack
        
end
