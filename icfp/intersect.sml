structure Intersect : INTERSECT = 
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

    fun dummy _ = ZERO
    val sphere   : m4 * v3 * v3 -> result = dummy
    val plane    : m4 * v3 * v3 -> result = dummy
    val cylinder : m4 * v3 * v3 -> result = dummy
    val cube     : m4 * v3 * v3 -> result = dummy
    val cone     : m4 * v3 * v3 -> result = dummy

    val hits_sphere   : m4 * v3 * v3 -> bool = dummy
    val hits_plane    : m4 * v3 * v3 -> bool = dummy
    val hits_cylinder : m4 * v3 * v3 -> bool = dummy
    val hits_cube     : m4 * v3 * v3 -> bool = dummy
    val hits_cone     : m4 * v3 * v3 -> bool = dummy

  end
