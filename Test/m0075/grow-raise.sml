(*$import *)

exception E
val rec loop =
   fn 0 => raise E
    | n => 1 + loop(n - 1)

val _ = loop 100000 handle E => 13

