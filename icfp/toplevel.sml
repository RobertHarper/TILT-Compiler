structure Toplevel :> TOPLEVEL =
struct

    exception Unimplemented

    fun u (e : Eval.env,
	   s : Eval.stack) = 
	(raise Unimplemented; (e,s))

    val opers = 
	foldl (fn ((oo,dd), m) => Envmap.insert (m, oo, dd)) Envmap.empty

[ ("addi", u),
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
