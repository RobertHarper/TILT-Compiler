(*$import *)

extern raise_an_exception : (exn, unit) -->
extern my_openf : (word, unit) -->

datatype open_mode = O_RDWR

fun omodeToWord O_RDWR = 0w0

fun safe_openf () = Ccall(my_openf,0w0)

fun unsafe_openf () = Ccall (my_openf,omodeToWord O_RDWR)

fun die () =
    let

	(* The exception must be raised from the runtime.  Replacing
	   "Ccall(raise_an_exception, Div)" with "(raise Div) : unit"
	   avoids the error.

	   The identity of the exception does not seem to matter.  The
	   error occurs with Div, Overflow, and SysErr constructed by
	   the runtime and with Div and TiltExn.SysErr constructed in
	   ML.
	*)
	val _ = (Ccall(raise_an_exception, Div)
		 handle _ => ())

	val _ = unsafe_openf ()		(* illegal instruction *)
(*
	val _ = safe_openf ()		(* no illegal instruction *)
*)

    in
	()
    end

val _ = die()
