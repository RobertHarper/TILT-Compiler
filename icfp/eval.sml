
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

    fun addi ((Int n2) :: (Int n1) :: s) = (Int (n2 + n1)) :: s
    fun addf ((Real n2) :: (Real n1) :: s) = (Real (n2 + n1)) :: s

    fun getx ((Point (x,_,_)) :: s) = (Real x) :: s
    fun gety ((Point (_,y,_)) :: s) = (Real y) :: s
    fun getz ((Point (_,_,z)) :: s) = (Real z) :: s

    fun point ((Real z) :: (Real y) :: (Real x) :: s) =
	(Point (x, y, z)) :: s

    fun alength ((Array v) :: s) =
	(Int (Vector.length v)) :: s
    fun aget ((Int i) :: (Array v) :: s) =
	(Vector.sub (v, i)) :: s

(* transformations *)

    structure M4 = Matrix


fun translate ((Real rtz) :: (Real rty) :: (Real rtx) :: (Object obj) :: s) =
    let
	fun helper (Sphere(m,c)) = Sphere(M4.translateM(rtx, rty, rtz, m), c)
	  | helper (Cube(m,c))  = Cube(M4.translateM(rtx, rty, rtz, m), c)
	  | helper (Cone(m,c)) = Cone(M4.translateM(rtx, rty, rtz, m), c)
	  | helper (Cylinder(m,c)) = Cylinder(M4.translateM(rtx, rty, rtz, m), c)
	  | helper (Plane (m,c)) = Plane(M4.translateM(rtx, rty, rtz, m), c)
	  | helper (Union(o1, o2)) = Union(helper(o1), helper(o2))
	  | helper (Difference(o1,o2)) = Difference(helper(o1), helper(o2))
	  | helper (Intersection(o1,o2)) = Intersection(helper(o1), helper(o2))
    in
	Object (helper obj)::s
    end

fun scale ((Real rsz) :: (Real rsy) :: (Real rsx) :: (Object obj) :: s) =
    let
	fun helper (Sphere(m,c)) = Sphere(M4.scaleM(rsx, rsy, rsz, m), c)
	  | helper (Cube(m,c))  = Cube(M4.scaleM(rsx, rsy, rsz, m), c)
	  | helper (Cone(m,c)) = Cone(M4.scaleM(rsx, rsy, rsz, m), c)
	  | helper (Cylinder(m,c)) = Cylinder(M4.scaleM(rsx, rsy, rsz, m), c)
	  | helper (Plane (m,c)) = Plane(M4.scaleM(rsx, rsy, rsz, m), c)
	  | helper (Union(o1, o2)) = Union(helper(o1), helper(o2))
	  | helper (Difference(o1,o2)) = Difference(helper(o1), helper(o2))
	  | helper (Intersection(o1,o2)) = Intersection(helper(o1), helper(o2))
    in
	Object (helper obj)::s
    end

fun uscale ((Real rs) :: (Object obj) :: s) =
    let
	fun helper (Sphere(m,c)) = Sphere(M4.uScaleM(rs, m), c)
	  | helper (Cube(m,c))  = Cube(M4.uScaleM(rs, m), c)
	  | helper (Cone(m,c)) = Cone(M4.uScaleM(rs, m), c)
	  | helper (Cylinder(m,c)) = Cylinder(M4.uScaleM(rs, m), c)
	  | helper (Plane (m,c)) = Plane(M4.uScaleM(rs, m), c)
	  | helper (Union(o1, o2)) = Union(helper(o1), helper(o2))
	  | helper (Difference(o1,o2)) = Difference(helper(o1), helper(o2))
	  | helper (Intersection(o1,o2)) = Intersection(helper(o1), helper(o2))
    in
	Object (helper obj)::s
    end

fun rotatex ((Real t) :: (Object obj) :: s) =
    let
	fun helper (Sphere(m,c)) = Sphere(M4.rotxM(t, m), c)
	  | helper (Cube(m,c))  = Cube(M4.rotxM(t, m), c)
	  | helper (Cone(m,c)) = Cone(M4.rotxM(t, m), c)
	  | helper (Cylinder(m,c)) = Cylinder(M4.rotxM(t, m), c)
	  | helper (Plane (m,c)) = Plane(M4.rotxM(t, m), c)
	  | helper (Union(o1, o2)) = Union(helper(o1), helper(o2))
	  | helper (Difference(o1,o2)) = Difference(helper(o1), helper(o2))
	  | helper (Intersection(o1,o2)) = Intersection(helper(o1), helper(o2))
    in
	Object (helper obj)::s
    end

fun rotatey ((Real t) :: (Object obj) :: s) =
    let
	fun helper (Sphere(m,c)) = Sphere(M4.rotyM(t, m), c)
	  | helper (Cube(m,c))  = Cube(M4.rotyM(t, m), c)
	  | helper (Cone(m,c)) = Cone(M4.rotyM(t, m), c)
	  | helper (Cylinder(m,c)) = Cylinder(M4.rotyM(t, m), c)
	  | helper (Plane (m,c)) = Plane(M4.rotyM(t, m), c)
	  | helper (Union(o1, o2)) = Union(helper(o1), helper(o2))
	  | helper (Difference(o1,o2)) = Difference(helper(o1), helper(o2))
	  | helper (Intersection(o1,o2)) = Intersection(helper(o1), helper(o2))
    in
	Object (helper obj)::s
    end

fun rotatez ((Real t) :: (Object obj) :: s) =
    let
	fun helper (Sphere(m,c)) = Sphere(M4.rotzM(t, m), c)
	  | helper (Cube(m,c))  = Cube(M4.rotzM(t, m), c)
	  | helper (Cone(m,c)) = Cone(M4.rotzM(t, m), c)
	  | helper (Cylinder(m,c)) = Cylinder(M4.rotzM(t, m), c)
	  | helper (Plane (m,c)) = Plane(M4.rotzM(t, m), c)
	  | helper (Union(o1, o2)) = Union(helper(o1), helper(o2))
	  | helper (Difference(o1,o2)) = Difference(helper(o1), helper(o2))
	  | helper (Intersection(o1,o2)) = Intersection(helper(o1), helper(o2))
    in
	Object (helper obj)::s
    end

fun union ((Object o2) :: (Object o1) :: s) = Object(Union(o1, o2))::s
    
fun intersect ((Object o2) :: (Object o1) :: s) = Object(Intersection(o1, o2))::s
    
fun difference ((Object o2) :: (Object o1) :: s) = Object (Difference(o1, o2))::s
    
(* Math functions [not addi/addf, since tom did those] in reverse alphabetic order *)
    
fun subi((Int i2) :: (Int i1) :: s) = Int (i1-i2) :: s
    
fun subf((Real r2) :: (Real r1) :: s) = Real (r1-r2) :: s
    
fun sqrt ((Real r1) :: s) = Real (Math.sqrt r1) :: s
    
(* d2r converts degrees to radians *)
fun d2r (angle) = angle * (Math.pi / 180.0)
(* r2d converts radians to degrees *)
fun r2d (rad) = rad * (180.0 / Math.pi)
    
fun sin ((Real r1) :: s) = Real (Math.sin (d2r (r1))) :: s
    
fun real ((Int i1) :: s) = Real (Real.fromInt(i1)) :: s
    
fun negi ((Int i1) :: s) = Int (~ i1) :: s
    
fun negf ((Real r1) :: s) = Real (~ r1) :: s
    
fun muli ((Int i2) :: (Int i1) :: s) = Int(i1 * i2) :: s
    
fun mulf ((Real r2) :: (Real r1) :: s) = Real (r1 * r2) :: s
    
fun modi ((Int i2) :: (Int i1) :: s) = Int (i1 mod i2) :: s
    
fun lessi ((Int i2) :: (Int i1) :: s) = Bool(i1 < i2) :: s
fun lessf ((Real r2) :: (Real r1) :: s) = Bool (Real.< (r1, r2)) :: s
    
fun frac ((Real r1) :: s) = Real (Real.realMod r1) :: s
    
fun floor ((Real r1) :: s) = Int (Real.trunc r1) :: s
    
fun eqi ((Int i2) :: (Int i1) :: s) = Bool (i1 = i2) :: s
fun eqf ((Real i2) :: (Real i1) :: s) = Bool (Real.== (i1,i2)) :: s
    
fun divi ((Int i2) :: (Int i1) :: s) = Int (i1 div i2) :: s
fun divf ((Real r2) :: (Real r1) :: s) = Real (r1 / r2) :: s
    
fun cos ((Real r1) :: s) = Real (Math.cos (d2r r1)) :: s
    
fun clampf ((Real r1) :: s) = 
    Real(if (r1 < 0.0) then 0.0 else if (r1 > 1.0) then 1.0 else r1) :: s
	 
fun asin((Real r1) :: s) = Real(r2d (Math.asin r1)) :: s
    
fun acos((Real r1) :: s) = Real(r2d (Math.acos r1)) :: s
    
(* Geom primitaves *)
    
fun sphere ((Closure c) :: s) = Object (Sphere(M4.ident, c)) :: s
fun cube ((Closure c) :: s) = Object (Cube(M4.ident, c)) :: s
fun cylinder ((Closure c) :: s) = Object (Cylinder(M4.ident, c)) :: s
fun cone ((Closure c) :: s) = Object (Cone(M4.ident, c)) :: s
fun plane ((Closure c) :: s) = Object (Plane(M4.ident, c)) :: s



(* FIXME *)

fun light ((Point c) :: (Point dir) :: s) =
    Light (Sunlight (dir, c)) :: s
fun pointlight ((Point c) :: (Point pos) :: s) =
    Light (Pointlight (pos, c)) :: s
fun spotlight ((Real exp) :: (Real cutoff) :: (Point c) :: 
	   (Point dest) :: (Point src) :: s) =
    Light (Spotlight { pos = src,
		       dir = Vect.makeDir (src, dest),
		       color = c,
		       cutoff = cutoff,
		       att = exp }) :: s
    
    val opers = 
	foldl (fn ((oo,dd), m) => Envmap.insert (m, oo, dd)) Envmap.empty

[ ("addi", addi),
  ("addf", addf),
  ("acos", acos),
  ("asin", asin),
  ("clampf", clampf),
  ("cos", cos),
  ("divi", divi),
  ("divf", divf),
  ("eqi", eqi),
  ("eqf", eqf),
  ("floor", floor),
  ("frac", frac),
  ("lessi", lessi),
  ("lessf", lessf),
  ("modi", modi),
  ("muli", muli),
  ("mulf", mulf),
  ("negi", negi),
  ("negf", negf),
  ("real", real),
  ("sin", sin),
  ("sqrt", sqrt),
  ("subi", subi),
  ("subf", subf),
  ("getx", getx),
  ("gety", gety),
  ("getz", getz),
  ("point", point),
  ("get", aget),
  ("length", alength),
  ("sphere", sphere),
  ("cube", cube),
  ("cylinder", cylinder),
  ("cone", cone),
  ("plane", plane),
  ("translate", translate),
  ("scale", scale),
  ("uscale", uscale),
  ("rotatex", rotatex),
  ("rotatey", rotatey),
  ("rotatez", rotatez),
  ("light", light),
  ("pointlight", pointlight),
  ("spotlight", spotlight),
  ("union", union),
  ("intersect", intersect),
  ("difference", difference),
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
		raise Eval ("unbound variable/operator '" ^ var ^ "'")
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
	      | step (G, 
		      (Closure (G'f, elf)) :: 
		      (Closure (G't, elt)) :: 
		      (Bool b) :: s, Gml.If :: c) =
		let val (_, stk) = step (if b then G't else G'f, s, 
					     if b then elt else elf)
		in step (G, stk, c)
		end
	      | step (G, (Closure (G', el)) :: s, Gml.Apply :: c) =
		let val (_, stk) = step (G', s, el)
		in step (G, stk, c)
		end
              | step (G, s, (Gml.Var v) :: c) = step (G, (G ?? v) :: s, c)
	      | step (G, s, (Gml.Fun f) :: c) = 
		step (G, (Closure (G, f)) :: s, c)
	      | step (G, s, (Gml.Array a) :: c) =
		let val (_, out) = step (G, nil, a)
		(* might be missing a rev here XXX *)
		in step (G, (Array (Vector.fromList (rev out))) :: s, c)
		end
	      | step (G, s, nil) = (G, s)
	      | step _ = raise Eval ("Eval error")
	in
            #2 (step (empty_context, nil, el))
	end
        
end
