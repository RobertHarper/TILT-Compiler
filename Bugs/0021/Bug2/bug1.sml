(*$import Setup Int *)

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

