(*$import Word8Array Word8 *)

val a = Word8Array.array (0, 0w42)
val sum = Word8Array.foldr (fn (e,a) => Word8.toInt e + a) 0 a
    