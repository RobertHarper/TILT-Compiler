(*
	TILT used to support eqtype specs with definitions.
	This tests that the support is really gone.
*)
signature S =
sig
	eqtype 'a t = 'a option
end
