signature S1 =
sig
	structure A :
	sig
		datatype a = A
	end
	type b = A.a
end

signature S2 =
sig
	structure B : S1
end

functor F(structure Arg : S2) :> S2 where B = Arg.B =
struct
	structure B = Arg.B
end
