(*$import Prelude *)

fun find ("" :: r) = find r
  | find _ = ()				(* Not necessary but simplifies generated code *)
