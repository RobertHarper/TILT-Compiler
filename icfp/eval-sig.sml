
signature EVAL =
sig

    type m4 = Matrix.m4
    type v3 = Matrix.v3
    type color = Base.color

    structure T :
	sig
	    val opers : (Base.stack -> Base.stack) Envmap.map
	end

    val eval : Gml.exp list -> Base.stack
        
end
