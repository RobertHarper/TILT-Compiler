structure BinIO_Util : BIN_IO_UTIL =
  struct

    type instream = BinIO.instream
    type outstream = BinIO.outstream

    val use_non_blocking = ref false
    fun non_blocker(is,i) f =
	if (!use_non_blocking)
	    then (case BinIO.canInput(is,i)
		      of SOME 0 => NONE
		    | NONE => NONE
		    | SOME i' => if i=i' then f() else NONE)
	else f()

    val elemToChar = Char.chr o Word8.toInt
    val charToElem = Word8.fromInt o Char.ord

    fun input_string (is : BinIO.instream, i:int) : string option =  (* non-blocking *)
      non_blocker(is,i)
      (fn () => let val v = BinIO.inputN(is,i)
		    val cs = Word8Vector.foldl (fn (e,a) => elemToChar e::a) [] v
		in SOME (implode (rev cs))
		end)

    fun lookahead (is : BinIO.instream) : char option =
      non_blocker(is,1)
      (fn () => case BinIO.lookahead(is)
		  of SOME elem => SOME(elemToChar elem)
		   | NONE => NONE)

    fun input_char (is : BinIO.instream) : char option =
      non_blocker(is,1)
      (fn () => case BinIO.input1(is)
		  of SOME elem => SOME(elemToChar elem)
		   | NONE => NONE)

    fun output_string (os : BinIO.outstream, s) =
      app (fn c => BinIO.output1(os,charToElem c))
      (explode s)


    fun copy (is : instream, os : outstream) : unit =
      let val n = 1024
	  fun loop () =
	      let
(*
	    case BinIO.canInput(is, n)
	      of SOME 0 => ()
	       | NONE => ()
	       | SOME n' =>
*)
		  val n' = n
		     val v = BinIO.inputN(is,n')
		  in BinIO.output(os,v);
		      if (Word8Vector.length v > 0) then loop() else ()
		  end
      in loop ()
      end

  end

