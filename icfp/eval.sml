
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

    val real_render : (stack -> stack) ref = ref (fn _ => raise Unimplemented)

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

fun render (s : stack) = !real_render s

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
  ("render", render)
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

    local
      fun eval' (G, s, (Gml.Int i) :: c) = eval' (G, (Int i) :: s, c)
	| eval' (G, s, (Gml.Bool b) :: c) = eval' (G, (Bool b) :: s, c)
	| eval' (G, s, (Gml.Real r) :: c) = eval' (G, (Real r) :: s, c)
	| eval' (G, s, (Gml.String ss) :: c) = 
	eval' (G, (String ss) :: s, c)
	| eval' (G, v :: s, (Gml.Binder var) :: c) =
	eval' (G ++ (var, v), s, c)
	| eval' (G, s, (Gml.Oper p) :: c ) = 
	eval' ((G, (T.opers ?? p) s, c)
	       handle Match => 
		 raise Eval ("inappropriate stack for " ^ p ^
			     "(caught Match)"))
	| eval' (G, 
		 (Closure (G'f, elf)) :: 
		 (Closure (G't, elt)) :: 
		 (Bool b) :: s, Gml.If :: c) =
	let val (_, stk) = eval' (if b then G't else G'f, s, 
				    if b then elt else elf)
	in eval' (G, stk, c)
	end
	| eval' (G, (Closure (G', el)) :: s, Gml.Apply :: c) =
	let val (_, stk) = eval' (G', s, el)
	in eval' (G, stk, c)
	end
	| eval' (G, s, (Gml.Var v) :: c) = eval' (G, (G ?? v) :: s, c)
	| eval' (G, s, (Gml.Fun f) :: c) = 
	eval' (G, (Closure (G, f)) :: s, c)
	| eval' (G, s, (Gml.Array a) :: c) =
	let val (_, out) = eval' (G, nil, a)
	(* might be missing a rev here XXX *)
	in eval' (G, (Array (Vector.fromList (rev out))) :: s, c)
	end
	| eval' (G, s, nil) = (G, s)
	| eval' _ = raise Eval ("Eval error")

      fun render ((Point amb)      
		  :: (Array v)  
		  :: (Object obj) 
		  :: (Int depth)    
		  :: (Real hfov) 
		  :: (Int wid) 
		  :: (Int ht) 
		  :: (String fname) 
		  :: s) = 
	let 
	  val lights = Vector.foldr (fn (Light light,ls) => (light :: ls)) [] v

	  val sref = ref s

	  fun apply ((G,exps),face,u,v) = 
	    let 
	      val stack = (Real v) :: (Real u) :: (Int face) :: (!sref)
	      val (_,stack) = eval' (G,stack,exps)
	      val ((Real n) :: (Real ks) :: (Real kd) :: (Point color) :: stack) = stack
	      val _ = sref := stack
	    in (color,kd,ks,n)
	    end

	  val ppm = Render.render (apply,
				   {amb    = amb,
				    lights = lights,
				    scene  = obj,
				    depth  = depth,
				    hfov   = hfov,
				    hres   = wid,
				    vres   = ht})
	  val _ = Ppm.write (ppm,fname)
	in !sref
	end
    in
      val _ = real_render := render
      fun eval (el) =   #2 (eval' (empty_context, nil, el))
    end

        
end
