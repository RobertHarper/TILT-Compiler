(*$import Setup Bool *)

local
    structure S' =
    struct
	type t' = bool
	val init = true
	val toString = Bool.toString
    end
in
    structure S2 = F(S')
end
