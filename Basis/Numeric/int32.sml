(*$import Prelude StringCvt PreInt General NumScan NumFormat INTEGER *)
(* int32.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Int32 :> INTEGER where type int = int =
  struct
    val quot = TiltPrim.iquot
    val andb = TiltPrim.andb
    val xorb = TiltPrim.xorb
(*
    structure I32 = InlineT.Int32

    type int = int32
*)
    type int = int

    val precision = SOME 32

    val minIntVal : int = ~2147483648
    val minInt : int option = SOME minIntVal
    val maxInt : int option = SOME 2147483647
(*
    val op *    : int * int -> int  = I32.*
    val op quot : int * int -> int  = I32.quot
    val op +    : int * int -> int  = I32.+
    val op -    : int * int -> int  = I32.-
    val ~       : int -> int = I32.~
    val op <    : int * int -> bool = I32.<
    val op <=   : int * int -> bool = I32.<=
    val op >    : int * int -> bool = I32.>
    val op >=   : int * int -> bool = I32.>=
    val op =    : int * int -> bool = I32.=
    val op <>   : int * int -> bool = I32.<>
*)

    val op *    : int * int -> int  = *
    val op quot : int * int -> int  = quot
    val op +    : int * int -> int  = +
    val op -    : int * int -> int  = -
    val ~       : int -> int = ~
    val op <    : int * int -> bool = <
    val op <=   : int * int -> bool = <=
    val op >    : int * int -> bool = >
    val op >=   : int * int -> bool = >=
    val op =    : int * int -> bool = =
    val op <>   : int * int -> bool = <>
    nonfix * quot + - < <= > >= = <>

  (* min, max, abs, rem, div, and mod should be inlined. 
   *     ... but this is not the time!
   *)
    fun min(a:int, b:int):int = if <(a,b) then a else b
    fun max(a:int, b:int):int = if >(a,b) then a else b
    fun op rem(a:int,b:int):int =  -(a, *(b, quot(a, b)))
(*
    fun abs(a:int):int = if >(a, 0) then ~(a) else a
    fun op div(a:int, b:int):int = if >=(b, 0)
	  then if >=(a, 0)
	    then quot(a, b) 
	    else -(quot(+(a, 1), b), 1)
	  else if >(a,0)
	    then -(quot(-(a, 1), b), 1)
	    else quot(a, b)

    fun op mod(a:int, b:int):int = if >=(b, 0)
	  then if >=(a, 0)
	    then -(a, *(quot(a, b), b))
	    else -(a, +( *(quot(+(a,1), b), b), b))
	  else if >(a, 0)
	    then -(a, +( *(quot(-(a,1), b), b), b))
	  else if =(a, ~2147483648) andalso =(b, ~1)
	    then 0
	    else -(a, * (quot(a, b), b))
*)
    val abs = PreInt.iabs
    val op div = PreInt.idiv
    val op mod = PreInt.imod
	
    fun sign(0) = 0
      | sign i = if <(i, 0) then ~1 else 1

    fun sameSign(i, j) = let val x : int = andb(xorb(i, j), minIntVal)
			 in  =(x,0)
			 end


    fun compare (i:int, j:int) =
	  if (<(i, j)) then General.LESS
	  else if (>(i, j)) then General.GREATER
	  else General.EQUAL

    val scan = NumScan.scanInt
    val fmt = NumFormat.fmtInt
    val toString = fmt StringCvt.DEC
    val fromString = StringCvt.scanString (scan StringCvt.DEC) 

(*
    val toInt : int -> Int.int = toInt
    val fromInt : Int.int -> int = fromInt
    val toLarge : int -> LargeInt.int = toLarge
    val fromLarge : LargeInt.int -> int = fromLarge
*)
    fun toInt (x : int) : PreInt.int = x
    fun fromInt (x : PreInt.int) : int = x
    fun toLarge (x : int) : PreLargeInt.int = x
    fun fromLarge (x : PreInt.int) : int = x

  end

