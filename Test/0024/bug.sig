signature S1 =
sig
	type t
end
signature S2 =
sig
	structure A : S1
	structure B :
	sig
		structure C : S1
	end
	sharing type A.t = B.C.t
end
