(*$import Setup Word32 Time *)

local
    structure T' =
    struct
	type t' = Word32.word
	val init = 0w3
	val toString = Word32.toString
    end
in
    structure T1 = F(T')
end

local
    structure T' =
    struct
	type t' = Time.time
	val init = Time.now()
	val toString = Time.toString
    end
in
    structure T2 = F(T')
end
