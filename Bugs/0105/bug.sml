(*$import Word8 Word8Vector *)

fun chck' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst' s f = tst0 s (chck' f);

val b = Word8Vector.fromList (map Word8.fromInt [44,55,66]);

fun chkiter iter f vec =
    tst' "test_chkiter" (fn _ =>
	   let val res = iter f vec
	   in  true  end)

val test10a:unit = chkiter Word8Vector.map (fn x => 0w2*x) b
