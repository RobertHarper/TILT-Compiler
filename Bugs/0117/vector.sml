(*$import Int Vector *)

fun chck' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

val b = Vector.fromList [44,55,66];

fun chkiteri g f vec reslast =
    chck'(fn _ =>
	   let val last = ref ~1
	       val res = g (fn (i, x) => (last := i; f x)) vec
	   in (res, !last) = reslast end)

val test11e =
    chkiteri Vector.mapi (fn x => 2*x) (b, 3, NONE) (Vector.fromList [], ~1)


