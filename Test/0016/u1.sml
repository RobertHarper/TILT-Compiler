functor F (Arg : sig type t end) =
struct
	type a = Arg.t
	type b = a
end
