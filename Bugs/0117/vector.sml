(*$import Int Vector *)

val b = Vector.fromList [44,55,66];

fun chkiteri g f vec reslast =
    g (fn (i, x) => f x) vec = reslast

val test11e =
    chkiteri Vector.mapi (fn x => 2*x) (b, 3, NONE) (Vector.fromList [])
