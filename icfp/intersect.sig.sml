(* Intersect basic shapes
 *)
signature INTERSECT = 
  sig
    type m4 = Matrix.m4
    type v4 = Matrix.v4
    type v3 = Matrix.v3

    type ans = {u:real,v:real,face:int, 
		N : v3,         (*Normal vector*)
		S : v3,         (*Reflection *)
		theta : real,   (*Reflection angle in degrees *)
		hit : v3,       (*Point of intersection in world coordinates *)
		dist : real     (*Distance to viewer *)
		}

    datatype result = ZERO | ONE of ans | TWO of ans * ans 

    (*Transformation matrix
     * Ray endpoints in world coordinates
     *)
    val sphere   : m4 * v3 * v3 -> result
    val plane    : m4 * v3 * v3 -> result
    val cylinder : m4 * v3 * v3 -> result
    val cube     : m4 * v3 * v3 -> result
    val cone     : m4 * v3 * v3 -> result

    (*Transformation matrix
     * Ray endpoints in world coordinates
     *)
    val hits_sphere   : m4 * v3 * v3 -> bool
    val hits_plane    : m4 * v3 * v3 -> bool
    val hits_cylinder : m4 * v3 * v3 -> bool
    val hits_cube     : m4 * v3 * v3 -> bool
    val hits_cone     : m4 * v3 * v3 -> bool
  end