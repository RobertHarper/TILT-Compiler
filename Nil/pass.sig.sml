(* A generic signature for a pass of the optimizer *)
signature PASS =
    sig
	val debug : bool ref
	val doModule :  Nil.module -> Nil.module
    end
