(*$import Plus *)

(* This unit directly imports op+ from three sources:

 	Prelude and TopLevel each provide some overloads for op+.
	Plus provides op+ : string.

  The compiler correctly resolves the conflict between Prelude and
  TopLevel by merging the overloads.

  The compiler does not detect the conflict between the overloads and
  op+ : string.  It should reject this unit.

*)
