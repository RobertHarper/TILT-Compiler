functor F (Arg : S1) :> S2
	where B.C = Arg =
struct
	structure A = Arg
	structure B =
	struct
		structure C = Arg
	end
end
