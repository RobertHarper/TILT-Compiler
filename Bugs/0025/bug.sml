(*$import *)
(* exception A  ---  as if from a missing import *)
exception B
val bug = (()
	   handle A => ()
		| B => ())

