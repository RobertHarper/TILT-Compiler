structure Arith :> ARITH =
struct
    structure P = PrimArith
    structure F = FlooredArith
    structure I32 = IntArith (structure I = Int32
			      val name = "Int32")
    structure W8 = WordArith (structure W = Word8
			      val name = "Word8")
    structure W32 = WordArith (structure W = Word32
			       val name = "Word32")
    structure R = RealArith (structure R = Real
			     val name = "Real")
    structure O = OrdArith (structure C = Char
			    val name = "Char")
    val tests = [P.test, F.test, I32.test, W8.test, W32.test, R.test, O.test]
		 
    fun test () = app (fn f => f()) tests
end
