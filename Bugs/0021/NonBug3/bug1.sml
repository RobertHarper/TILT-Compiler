(*$import Setup Int Bool *)

local
    structure S' =
    struct
	type t' = int
	val init = 3
	val toString = Int.toString
    end
in
    structure S1 = F(S')
end

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
