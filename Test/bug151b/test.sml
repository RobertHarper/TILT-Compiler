(*$import A B *)

(* The imports conflict; that is, both A and B export a value x.
   Rather than impose an order on imports, TILT rejects this code.  *)
