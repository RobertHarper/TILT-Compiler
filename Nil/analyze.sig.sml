(*$import Prelude Nil Name *)

signature ANALYZE = 
    sig
	type funinfo = {definition : Nil.function,
			size : int,
			occurs : (bool * int) list}
	val analyze : Nil.module -> funinfo Name.VarMap.map
    end

