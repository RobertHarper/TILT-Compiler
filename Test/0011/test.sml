(*
	IO is not in scope.
	Toil was generating an internal error
	rather than a user error.
*)
fun apply f =
	(f() handle (e as IO.Io _) => (print (exnMessage e)))
