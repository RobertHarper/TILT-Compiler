(*$import Prelude *)

signature STATISTICS =
sig
    type seq				(* of reals *)
	
    val min      : seq -> real		(* raises Domain if |seq| = 0 *)
    val median   : seq -> real		(* raises Domain if |seq| = 0 *)
    val max      : seq -> real		(* raises Domain if |seq| = 0 *)
    val mean     : seq -> real		(* raises Domain if |seq| = 0 *)
    val variance : seq -> real		(* raises Domain if |seq| <= 1 *)
    val stddev   : seq -> real		(* raises Domain if |seq| <= 1 *)
    val absdev   : seq -> real		(* raises Domain if |seq| = 0 *)
end
