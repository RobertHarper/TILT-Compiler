(*$import Prelude StringCvt PreInt General NumScan NumFormat INTEGER *)
(* int32.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Int32 :> INTEGER where type int = int =
  struct
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

structure Position = Int32
structure Int = Int32
structure LargeInt = Int32
structure SysInt = Int32

(*
 * $Log$
# Revision 1.3  2000/09/12  18:54:29  swasey
# Changes for cutoff compilation
# 
 * Revision 1.2  2000/01/20 13:31:54  pscheng
 * *** empty log message ***
 *
# Revision 1.1  1998/03/09  19:52:36  pscheng
# added basis
#
 * Revision 1.2  1997/02/11  15:15:43  george
 * got rid of structure rebinding, since inlining is now preserved
 *
 * Revision 1.1.1.1  1997/01/14  01:38:15  george
 *   Version 109.24
 *
 *)
