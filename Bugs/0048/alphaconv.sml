(*$import *)

(* Fails due to arity problems. *)

datatype 'a foo = A of 'a bar
     and 'b bar = B of 'b foo
