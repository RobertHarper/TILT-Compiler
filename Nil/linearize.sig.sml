(*$import Nil *)

(*
 Tools for putting Nil code into A-normal form
*)

signature LINEARIZE = 
    sig
	val debug : bool ref
	(* Turn debug prints on *)

	val linearize_mod : Nil.module -> Nil.module
        (* Convert a module to A-normal form *)

	val linearize_exp : Nil.exp -> Nil.exp
	(* Convert an expression (which may be open) to A-normal form *)
    end
