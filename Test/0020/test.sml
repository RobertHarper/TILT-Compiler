(*
	The compiler used to bomb in Ilstatic.Reduce rather than
	reject this code (because P.t has the wrong kind).
*)
signature MONO =
sig
    type t
    val x : t
end

signature POLY =
sig
    type 'a t
    val x : 'a t
end

functor F (structure P : POLY) :> MONO =
struct
	open P
end
