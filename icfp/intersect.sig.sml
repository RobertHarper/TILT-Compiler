(* Intersect basic shapes
 *)
signature INTERSECT = 
  sig
    type m4 = Matrix.m4
    type v4 = Matrix.v4
    type v3 = Matrix.v3

    type l1 = bool

    type l2 = {hit : v3,dist : real} list

    type l3info = {u:real,v:real,face:int, 
		   N : v3,         (*Normal vector*)
		   hit : v3,       (*Point of intersection in world coordinates *)
		   dist : real     (*Distance to viewer *)
		   } 
    type l3 = l3info list

    type result = l1 * (unit -> l2) * (unit -> l3)

    (*Transformation matrix,
     *   Origin and normalized direction vector
     *)
    val sphere   : m4 * v3 * v3 -> result
    val plane    : m4 * v3 * v3 -> result
    val cylinder : m4 * v3 * v3 -> result
    val cube     : m4 * v3 * v3 -> result
    val cone     : m4 * v3 * v3 -> result
  end
