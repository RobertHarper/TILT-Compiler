(*$import *)

(* TILT is failing to detect duplicate tyvar bindings. *)
(* Compilation should fail. *)
 
datatype ('a,'a) foo = A of 'a
