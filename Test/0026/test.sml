(*
	The phase splitter once bombed on this code because
	of a mistake in updating the splitting context.
*)
signature SIG =
sig
	datatype a = A of b
	and b = B of a
end

functor F (structure Arg : SIG) =
struct
	structure S = Arg
	datatype c = C of S.a
end
