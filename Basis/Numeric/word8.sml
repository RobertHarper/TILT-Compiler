(* word8.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Word8 :> WORD
    where type word = TiltPrim.uint8 =
struct
    type word = TiltPrim.uint8
    type w32 = TiltPrim.uint32

    val !! : w32 -> w32 = TiltPrim.!!	(* notb *)
    val && : w32 * w32 -> w32 = TiltPrim.&& (* andb *)
    val << : w32 * int -> w32 = TiltPrim.<<
    val >> : w32 * int -> w32 = TiltPrim.>>
    val ^^ : w32 * w32 -> w32 = TiltPrim.^^ (* xorb *)
    val || : w32 * w32 -> w32 = TiltPrim.|| (* orb *)
    val ~>> : int * int -> int = TiltPrim.~>>

    val int32touint32 : int -> w32 = TiltPrim.int32touint32
    val uint8toint32 : word -> int = TiltPrim.uint8toint32
    val toi32 : w32 -> int = TiltPrim.uint32toint32
    val tow32 : word -> w32 = TiltPrim.uint8touint32
    val tow8 : w32 -> word = TiltPrim.uint32touint8

    val ugt : w32 * w32 -> bool = TiltPrim.ugt
    val ugte : w32 * w32 -> bool = TiltPrim.ugte
    val ult : w32 * w32 -> bool = TiltPrim.ult
    val ulte : w32 * w32 -> bool = TiltPrim.ulte

    val udiv : w32 * w32 -> w32 = TiltPrim.udiv
    val uminus : w32 * w32 -> w32 = TiltPrim.uminus
    val umod : w32 * w32 -> w32 = TiltPrim.umod
    val uplus : w32 * w32 -> w32 = TiltPrim.uplus
    val umult : w32 * w32 -> w32 = TiltPrim.umult

    fun adapt (w : w32) : word =
	tow8(&& (w, 0wxFF))

    fun sextend (w : word) : w32 =
	let val w32 = tow32 w
	    val neg = &&(w32, 0w128)
	    val mask = int32touint32(~>>(toi32(<<(neg,24)),23))
	in  ||(w32,mask)
	end

    val wordSize = 8

    val toInt : word -> int = uint8toint32
    val toIntX : word -> int = toi32 o sextend
    val fromInt : int -> word = adapt o int32touint32

    val toLargeInt : word -> int = toInt
    val toLargeIntX : word -> int = toIntX
    val fromLargeInt : int -> word = fromInt

    val toLargeWord : word -> w32 = tow32
    val toLargeWordX : word -> w32 = sextend
    val fromLargeWord : w32 -> word = adapt

    fun lshift (w : word, k : w32) : word =
	if ulte(0w8,k) then 0w0
	else adapt(<<(tow32 w, toi32 k))

    fun rshiftl (w : word, k : w32) : word =
	if ulte(0w8,k) then 0w0
	else tow8(>>(tow32 w, toi32 k))

    fun rshifta (w : word, k : w32) : word =
	let val h : int = toi32(<<(tow32 w,24))
	    val k : int =
		if ulte(0w8,k) then 31
		else 24 + (toi32 k)
	in  adapt(int32touint32(~>>(h,k)))
	end

    nonfix << >> ~>> + - * div mod <= = >= < >

    val << = lshift
    val >> = rshiftl
    val ~>> = rshifta

    fun orb (x : word, y : word) : word = tow8(||(tow32 x, tow32 y))
    fun andb (x : word, y : word) : word = tow8(&&(tow32 x, tow32 y))
    fun xorb (x : word, y : word) : word = tow8(^^(tow32 x, tow32 y))
    fun notb (x : word) : word = adapt(!!(tow32 x))

    fun + (x : word, y : word) : word = adapt(uplus(tow32 x, tow32 y))
    fun - (x : word, y : word) : word = adapt(uminus(tow32 x, tow32 y))
    fun * (x : word, y : word) : word = adapt(umult(tow32 x, tow32 y))
    fun div (x : word, y : word) : word = tow8(udiv(tow32 x, tow32 y))
    fun mod (x : word, y : word) : word = tow8(umod(tow32 x, tow32 y))

    fun compare (x : word, y : word) : order =
	let val x = tow32 x
	    val y = tow32 y
	 in if ult(x,y) then LESS
	    else if ugt(x,y) then GREATER
	    else EQUAL
	end

    fun > (x : word, y : word) : bool = ugt(tow32 x, tow32 y)
    fun >= (x : word, y: word) : bool = ugte(tow32 x, tow32 y)
    fun < (x : word, y: word) : bool = ult(tow32 x, tow32 y)
    fun <= (x : word, y: word) : bool = ulte(tow32 x, tow32 y)

    fun min (x : word, y : word) : word = if <(x,y) then x else y
    fun max (x : word, y : word) : word = if <(x,y) then y else x

    fun fmt (radix : StringCvt.radix) : word -> string =
	(NumFormat.fmtWord radix) o tow32

    val toString : word -> string =
	fmt StringCvt.HEX

    type ('ty,'stream) scanner =
	(char,'stream) StringCvt.reader -> ('ty,'stream) StringCvt.reader

    fun scan (radix : StringCvt.radix) : (word,'stream) scanner =
	let val scanLarge : (w32,'stream) scanner =
		NumScan.scanWord radix
	in  fn (getc : 'stream -> (char * 'stream) option) =>
	    fn (s : 'stream) =>
		(case (scanLarge getc s) of
		    NONE => NONE
		|   SOME(w,s') =>
			if ugt(w,0w255) then raise Overflow
			else SOME(tow8 w,s'))
	end

    val fromString : string -> word option =
	StringCvt.scanString (scan StringCvt.HEX)

end
