
structure Eval :> EVAL =
struct

    exception Unimplemented
    exception Eval of string

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

structure T :> 
    sig

	val opers : (stack -> stack) Envmap.map

    end =
struct

    fun u (s : stack) = 
	(raise Unimplemented; s)

    val opers = 
	foldl (fn ((oo,dd), m) => Envmap.insert (m, oo, dd)) Envmap.empty

[ 
  ("apply", u),
  ("if", u),
  ("addi", u),
  ("addf", u),
  ("acos", u),
  ("asin", u),
  ("clampf", u),
  ("cos", u),
  ("divi", u),
  ("divf", u),
  ("eqi", u),
  ("equif", u),
  ("floor", u),
  ("frac", u),
  ("lessi", u),
  ("lessf", u),
  ("modi", u),
  ("muli", u),
  ("mulf", u),
  ("negi", u),
  ("negf", u),
  ("real", u),
  ("sin", u),
  ("sqrt", u),
  ("subi", u),
  ("subf", u),
  ("getx", u),
  ("gety", u),
  ("getz", u),
  ("point", u),
  ("get", u),
  ("length", u),
  ("sphere", u),
  ("cube", u),
  ("cylinder", u),
  ("cone", u),
  ("plane", u),
  ("translate", u),
  ("scale", u),
  ("uscale", u),
  ("rotatex", u),
  ("rotatey", u),
  ("rotatez", u),
  ("light", u),
  ("pointlight", u),
  ("spotlight", u),
  ("union", u),
  ("intersect", u),
  ("difference", u),
  ("render", u)
  ]


end


    infix ++ ??
    fun G ++ (var, v) = Envmap.insert (G, var, v)
    fun G ?? var = case Envmap.find (G, var) of
	NONE => 
	    let
		val _ = print "environment:\n"
		val _ = Envmap.appi (fn (a, _) => print (a^"\n"))
	    in
		raise Eval ("error looking up '" ^ var ^ "'")
	    end
      | SOME v => v

    val empty_context = Envmap.empty

    fun eval (el) =
	let

	    fun step (G, s, (Gml.Int i) :: c) = step (G, (Int i) :: s, c)
	      | step (G, s, (Gml.Bool b) :: c) = step (G, (Bool b) :: s, c)
	      | step (G, s, (Gml.Real r) :: c) = step (G, (Real r) :: s, c)
	      | step (G, s, (Gml.String ss) :: c) = 
		step (G, (String ss) :: s, c)
	      | step (G, v :: s, (Gml.Binder var) :: c) =
		step (G ++ (var, v), s, c)
	      | step (G, s, (Gml.Oper p) :: c ) = 
		step ((G, (T.opers ?? p) s, c)
		      handle Match => 
			  raise Eval ("inappropriate stack for " ^ p ^
				      "(caught Match)"))
              | step (G, s, (Gml.Var v) :: c) = step (G, (G ?? v) :: s, c)
	      | step (G, s, (Gml.Fun f) :: c) = 
		step (G, (Closure (G, f)) :: s, c)
	      | step (G, s, (Gml.Array a) :: c) =
		let val (_, out) = step (G, nil, a)
		(* might be missing a rev here XXX *)
		in step (G, (Array (Vector.fromList out)) :: s, c)
		end
	      | step (G, s, nil) = (G, s)
	      | step _ = raise Eval ("Eval error")
	in
            #2 (step (empty_context, nil, el))
	end
        
end
