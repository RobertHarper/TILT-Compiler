(*
	Must agree with runtime's struct cresult.
*)
datatype 'a cresult =
	Normal of 'a	(* Normal v => v *)
|	Error of exn	(* Error e => raise e *)
(*
The following functions should be copied into any compilation unit
that needs them.

fun ccall (f : ('a, 'b cresult) -->, a:'a) : 'b =
	(case (Ccall(f,a)) of
		Normal r => r
	|	Error e => raise e)

fun ccall2 (f : ('a, 'b, 'c cresult) -->, a:'a, b:'b) : 'c =
	(case (Ccall(f,a,b)) of
		Normal r => r
	|	Error e => raise e)

fun ccall3 (f : ('a, 'b, 'c, 'd cresult) -->, a:'a, b:'b, c:'c) : 'd =
	(case (Ccall(f,a,b,c)) of
		Normal r => r
	|	Error e => raise e)

fun ccall4 (f : ('a, 'b, 'c, 'd, 'e cresult) -->, a:'a, b:'b, c:'c, d:'d) : 'e =
	(case (Ccall(f,a,b,c,d)) of
		Normal r => r
	|	Error e => raise e)
*)
