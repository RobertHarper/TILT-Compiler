signature BLASTER = sig
(*
  type t
  val blastOut : (string * t) -> unit
  val blastIn  : string -> t
*)
  type 'a blastin = TextIO.instream -> 'a
  type 'a blastout = TextIO.outstream -> 'a -> unit

  val useOldBlast : bool ref

  val blastOutInt : TextIO.outstream -> int -> unit
  val blastInInt : TextIO.instream -> int
  val blastOutWord64 : TextIO.outstream -> TilWord64.word -> unit
  val blastInWord64 : TextIO.instream -> TilWord64.word
  val blastOutString : TextIO.outstream -> string -> unit
  val blastInString : TextIO.instream -> string
  val blastOutBool : TextIO.outstream -> bool -> unit
  val blastInBool : TextIO.instream -> bool
  val blastOutPair : 'a blastout -> 'b blastout -> TextIO.outstream -> ('a * 'b) -> unit
  val blastInPair : 'a blastin -> 'b blastin -> TextIO.instream -> ('a * 'b)
  val blastOutTriple : 'a blastout -> 'b blastout -> 'c blastout -> TextIO.outstream -> ('a * 'b * 'c) -> unit
  val blastInTriple : 'a blastin -> 'b blastin -> 'c blastin -> TextIO.instream -> ('a * 'b * 'c)
  val blastOutList : 'a blastout -> TextIO.outstream -> 'a list -> unit
  val blastInList : 'a blastin -> TextIO.instream -> 'a list
  val blastOutOption : 'a blastout -> TextIO.outstream -> 'a option -> unit
  val blastInOption : 'a blastin -> TextIO.instream -> 'a option

end



structure Blaster : BLASTER = struct

  val error = fn s => Util.error "blast.sml" s
  val useOldBlast = ref false

  type 'a blastin = TextIO.instream -> 'a
  type 'a blastout = TextIO.outstream -> 'a -> unit

    local
	open TextIO
    in
	val input1 = fn is => (case input1 is of
				   SOME c => c
				 | NONE => error "premature end of file in input1")

	fun blastOutString os str = 
	    (output1(os, chr (size str));
	     output(os, str))
	    
	fun blastInString is = 
	    let val sz = ord(input1 is)
		val str = inputN(is, sz)
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
		val a' = Word32.toInt a
		val b' = Word32.toInt b
		val c' = Word32.toInt c
		val d' = Word32.toInt d
	    in  output1(os, chr a');
		output1(os, chr b');
		output1(os, chr c');
		output1(os, chr d')
	    end

	fun blastInWord32 is =
	    let val a' = Word32.fromInt(ord(input1 is))
		val b' = Word32.fromInt(ord(input1 is))
		val c' = Word32.fromInt(ord(input1 is))
		val d' = Word32.fromInt(ord(input1 is))
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
	    
	fun blastOutBool os true = output1(os, chr 1)
	  | blastOutBool os false = output1(os, chr 0)
	    
	fun blastInBool is = (input1 is) = (chr 1)
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

