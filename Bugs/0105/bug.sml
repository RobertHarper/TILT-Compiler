(*$import Word8 Word8Vector *)

fun chck' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst' s f = tst0 s (chck' f);

val b = Word8Vector.fromList (map Word8.fromInt [44,55,66]);

fun chkiter iter f vec reslast =
    tst' "test_chkiter" (fn _ =>
	   let val last = ref (0w255:Word8.word)
	       val res = iter (fn x => (last := x; f x)) vec
	   in (res, !last) = reslast end)

val test10a:unit = chkiter Word8Vector.map (fn x => 0w2*x) b (Word8Vector.fromList [0w88,0w110,0w132], 0w66)
