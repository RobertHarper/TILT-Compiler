signature BLASTER = sig
(*
  type t
  val blastOut : (string * t) -> unit
  val blastIn  : string -> t
*)
  type 'a blastin = BinIO.instream -> 'a
  type 'a blastout = BinIO.outstream -> 'a -> unit

  val useOldBlast : bool ref

  val blastOutInt : BinIO.outstream -> int -> unit
  val blastInInt : BinIO.instream -> int
  val blastOutWord64 : BinIO.outstream -> TilWord64.word -> unit
  val blastInWord64 : BinIO.instream -> TilWord64.word
  val blastOutString : BinIO.outstream -> string -> unit
  val blastInString : BinIO.instream -> string
  val blastOutBool : BinIO.outstream -> bool -> unit
  val blastInBool : BinIO.instream -> bool
  val blastOutPair : 'a blastout -> 'b blastout -> BinIO.outstream -> ('a * 'b) -> unit
  val blastInPair : 'a blastin -> 'b blastin -> BinIO.instream -> ('a * 'b)
  val blastOutTriple : 'a blastout -> 'b blastout -> 'c blastout -> BinIO.outstream -> ('a * 'b * 'c) -> unit
  val blastInTriple : 'a blastin -> 'b blastin -> 'c blastin -> BinIO.instream -> ('a * 'b * 'c)
  val blastOutList : 'a blastout -> BinIO.outstream -> 'a list -> unit
  val blastInList : 'a blastin -> BinIO.instream -> 'a list
  val blastOutOption : 'a blastout -> BinIO.outstream -> 'a option -> unit
  val blastInOption : 'a blastin -> BinIO.instream -> 'a option

end



structure Blaster : BLASTER = struct

  val error = fn s => Util.error "blast.sml" s
  val useOldBlast = ref false

  type 'a blastin = BinIO.instream -> 'a
  type 'a blastout = BinIO.outstream -> 'a -> unit

    local
	open BinIO
    in
	val input1 = fn is => (case input1 is of
				   SOME c => c
				 | NONE => error "premature end of file in input1")

	fun string2vector str = 
	    Word8Vector.tabulate(size str, 
				 fn i => Word8.fromInt(ord(String.sub(str,i))))
	fun vector2string vect = 
	    let val chars = Word8Vector.foldr (fn (w,acc) => (chr(Word8.toInt w))::acc) [] vect
	    in  implode chars
	    end
	    
	fun blastOutString os str = 
	    (output1(os, Word8.fromInt (size str));
	     output(os, string2vector str))
	    
	fun blastInString is = 
	    let val sz = Word8.toInt(input1 is)
		val str = vector2string(inputN(is, sz))
	    in  str
	    end

	fun blastOutWord32 os w = 
	    let val a = Word32.andb(w,0w255)
		val w = Word32.>>(w,0w8)
		val b = Word32.andb(w,0w255)
		val w = Word32.>>(w,0w8)
		val c = Word32.andb(w,0w255)
		val w = Word32.>>(w,0w8)
		val d = w
		val a = Word8.fromInt(Word32.toInt a)
		val b = Word8.fromInt(Word32.toInt b)
		val c = Word8.fromInt(Word32.toInt c)
		val d = Word8.fromInt(Word32.toInt d)
	    in  output1(os, a);
		output1(os, b);
		output1(os, c);
		output1(os, d)
	    end

	fun blastInWord32 is =
	    let val a' = Word32.fromInt(Word8.toInt(input1 is))
		val b' = Word32.fromInt(Word8.toInt(input1 is))
		val c' = Word32.fromInt(Word8.toInt(input1 is))
		val d' = Word32.fromInt(Word8.toInt(input1 is))
		val w = Word32.orb(c',Word32.<<(d',0w8))
	    in  Word32.orb(a',Word32.<<(Word32.orb(b',Word32.<<(w,0w8)),0w8))
	    end
	
	fun blastOutInt' os i = blastOutString os (Int.toString i)
	fun blastInInt' is = (case Int.fromString(blastInString is) of
				 NONE => error "blastInInt failed\n"
			       | SOME n => n)

	fun blastOutInt'' os i = blastOutWord32 os (Word32.fromInt i)
	fun blastInInt'' is = Word32.toInt(blastInWord32 is)

	fun blastOutInt os i = if (!useOldBlast)
				   then blastOutInt' os i
			       else blastOutInt'' os i
	fun blastInInt is = if (!useOldBlast)
				then blastInInt' is
			    else blastInInt'' is


	fun blastOutWord64 os i = blastOutString os (TilWord64.toDecimalString i)
	fun blastInWord64 is = TilWord64.fromDecimalString(blastInString is) 
	    
	fun blastOutBool os true = output1(os, Word8.fromInt 1)
	  | blastOutBool os false = output1(os, Word8.fromInt 0)
	    
	fun blastInBool is = (input1 is) = (Word8.fromInt 1)
    end

    fun blastOutPair blaster1 blaster2 os (x,y) = (blaster1 os x; blaster2 os y)
    fun blastInPair blaster1 blaster2 is = let val x = blaster1 is
	                                       val y = blaster2 is
					   in  (x,y)
					   end
    fun blastOutTriple blaster1 blaster2 blaster3 os (x,y,z) = (blaster1 os x; blaster2 os y; blaster3 os z)
    fun blastInTriple blaster1 blaster2 blaster3 is = let val x = blaster1 is
							val y = blaster2 is
							val z = blaster3 is
						    in  (x,y,z)
						    end

    fun blastOutOption blaster os NONE = blastOutInt os 0
      | blastOutOption blaster os (SOME x) = (blastOutInt os 1; blaster os x)
    fun blastInOption blaster is = 
	(case (blastInInt is) of
	     0 => NONE
	   | 1 => SOME(blaster is)
	   | _ => error "bad blastInOption")


    fun blastOutList blaster os ls = (blastOutInt os (length ls);
				      app (blaster os) ls)
    fun blastInList blaster is = let val len = blastInInt is
				     fun loop 0 = []
				       | loop n = (blaster is)::(loop (n-1))
				 in  loop len
				 end
end

