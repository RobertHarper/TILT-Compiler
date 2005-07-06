(* see ../../Basis/ccall.sml *)
(* see ../../Basis/externtys.sml *)

type uct = word

(*
	Must agree with ../Runtime/port/ccall.c
*)
type cerr = int ref * uct ref	(* tag, carried *)

(*
	These types belong in extern.sml.  Due to a deficiency in the
	phase splitter, externs cannot use types defined within the
	same unit.

	These types must agree with the runtime.
*)

(* ../Runtime/port/timer.c *)
type rusagerep =
	int *	(* ru_utime.tv_sec *)
	int *	(* ru_utime.tv_usec *)
	int *	(* ru_stime.tv_sec *)
	int	(* ru_stime.tv_usec *)

