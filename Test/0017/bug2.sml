(*
	This unit failed to pass the phase splitter given the
	malformed signature created by bug1.sml.
*)

functor F (structure X : sig end) :> sig end = struct end
structure TextIO = F (structure X = B)
