(*
	Test various cases involving eqytpe specs and
	types that admit equality.
*)
signature m = sig eqtype t end
signature p = sig eqtype 'a t end
structure mt :> m = struct type t = unit end
structure md :> m = struct datatype t = T end
structure pt' :> p = struct type 'a t = unit end
structure pd' :> p = struct datatype 'a t = T end
structure pt :> p = struct type 'a t = 'a end
structure pd :> p = struct datatype 'a t = T of 'a end
(*
	TILT had problems with the next two structures that were fixed by
	changing the coercion compiler to always invoke the equality
	compiler to generate equality functions rather than relying on a
	name-based lookup in the target module.  In both cases, TILT was
	trying to coerce *Eqtype.+Et to match the +Et spec.
*)

(*
	This code was rejected under EqPayAsYouGo because the module
	does not have a pre-computed +Et.
*)
structure A :> sig eqtype t end =
struct
	structure Eqtype :> sig eqtype 'a t end = struct type 'a t = 'a end
	open Eqtype
	type t = unit
end

(*
	(*
		This code was rejected for the wrong reason with or without
		EqPayAsYouGo.  Rather than complaining that "t does not admit
		equality", TILT was complaining that *Eqtype.+Et did not match
		the +Et spec.
	*)
	structure B :> sig eqtype t end =
	struct
		structure Eqtype :> sig eqtype t end = struct type t = unit end
		open Eqtype
		type t = int -> int
	end
*)
