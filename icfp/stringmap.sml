
structure Envmap =
struct
    structure M = SplayMapFn(type ord_key = string
			     val compare = String.compare)
    open M
end