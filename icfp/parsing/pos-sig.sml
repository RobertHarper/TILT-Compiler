(* Chris Okasaki / Robert Harper
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
*)

signature POS =
  (* positions within a file (for reporting errors) *)
sig

  type T

  val markstream : char Stream.T -> (char * T) Stream.T

  val initpos  : T
  val nextchar : T -> T
  val nextline : T -> T
	(* to roll your own markstream for non-strings *)

  val rightedge : T -> T

  val union  : T * T -> T
  val max    : T * T -> T
  val min    : T * T -> T
        (* if positions are ranges (start,finish) :          *)
        (*   union ((s,f),(s',f')) = (min (s,s'),max (f,f')) *)
        (*   max   ((s,f),(s',f')) = (max (s,s'),max (f,f')) *)
        (*   min   ((s,f),(s',f')) = (min (s,s'),min (f,f')) *)

  val toString : T -> string

end
