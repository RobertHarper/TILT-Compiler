functor TilWordFromWord (W:WORD) :> TILWORD
    where type word = W.word =
struct

    val error = fn s => Util.error "tilwordfromword.sml" s

    type halfword = W.word  (* kept abstract *)
    type word = W.word

    val wordsize : int = W.wordSize

    val halfsize : int =
	if wordsize mod 8 = 0 then
	    wordsize div 2
	else
	    error "word size is not a multiple of 8"

    val zero : word = W.fromInt 0
    val one : word = W.fromInt 1

    val neg_one : word =    (* eg, 0wxFFFF *)
	W.fromInt ~1

    val low_mask : word =   (* eg, 0wxFF *)
	W.>>(neg_one, Word.fromInt halfsize)

    val high_mask : word =  (* eg, 0wxFF00 *)
	W.<<(neg_one, Word.fromInt halfsize)

    val most_neg : word =   (* eg, 0wx8000 *)
	W.<<(one, Word.fromInt (wordsize - 1))

    val most_pos : word =   (* eg, 0wx7FFF *)
	W.andb(W.notb most_neg, neg_one)

    val low_byte : word = W.fromInt 255

    (* ------ EQUALITY OPERATIONS --------- *)
    val equal : word * word -> bool = op=
    val nequal : word * word -> bool = op<>

    (* ---------- LOGICAL OPERATIONS ---------- *)
    val notb : word -> word = W.notb
    val orb : word * word -> word = W.orb
    val andb : word * word -> word = W.andb
    val xorb : word * word -> word = W.xorb
    fun lshift (w:word, i:int) : word = W.<<(w,Word.fromInt i)
    fun rshiftl (w:word, i:int) : word = W.>>(w,Word.fromInt i)
    fun rshifta (w:word, i:int) : word = W.~>>(w,Word.fromInt i)

    fun tohalf (w:word) : halfword * halfword =
	let val high = rshiftl(w, halfsize)
	    val low = andb(low_mask,w)
	in  (high,low)
	end

    fun fromhalf (high:halfword, low:halfword) : word =
	orb(lshift(high,halfsize), low)

    (* ------ UNSIGNED OPERATIONS --------- *)
    val uplus : word * word -> word = W.+
    val uminus : word * word -> word = W.-
    val umult : word * word -> word = W.*
    val udiv : word * word -> word = W.div
    val umod : word * word -> word = W.mod
    val ult : word * word -> bool = W.<
    val ugt : word * word -> bool = W.>
    val ulte : word * word -> bool = W.<=
    val ugte : word * word -> bool = W.>=

    fun uplus' (a:word, b:word) : word * word =
	let val low = uplus(a,b)
	    val high = if ult(low,a) then one else zero
	in  (high,low)
	end

    (* umult' is long multiplication in base 2**halfsize *)

    fun umulthalf (high:halfword, low:halfword, m:halfword) : halfword * halfword * halfword =
	let val (carry,low) = tohalf(W.*(m,low))
	    val (high,med) = tohalf(W.+(W.*(m,high),carry))
	in  (high,med,low)
	end

    fun umult' (a:word, b:word) : word * word =
	let val (ahigh,alow) = tohalf a
	    val (bhigh,blow) = tohalf b
	    val (d2',d1', d0) = umulthalf(ahigh,alow,blow)
	    val (d3',d2'',d1'') = umulthalf(ahigh,alow,bhigh)
	    val (carry,d1) = tohalf(W.+(d1',d1''))
	    val (carry,d2) = tohalf(W.+(W.+(d2',d2''),carry))
	    val d3 = W.+(carry,d3')
	    val low = fromhalf(d1,d0)
	    val high = fromhalf(d3,d2)
	in  (high,low)
	end

    (* ------ SIGNED OPERATIONS --------- *)

    fun sign (x:word) : int =
	if x = zero then 0
	else if W.andb(x,most_neg) = most_neg then ~1
	else 1

    fun splus (x:word, y:word) : word =
	let val z = W.+(x,y)
	    (*
		negative overflow when x, y < 0 and z >= 0
		positive overflow when x, y > 0 and z < 0
	    *)
	    val sx = sign x
	in  if sx = 0 orelse sx <> sign y orelse sx = sign z then z
	    else raise Overflow
	end

    fun unsafe_snegate (x:word) : word =
	uplus(one,notb x)

    fun unsafe_absolute (x:word) : word =
	if sign x >= 0 then x else unsafe_snegate x

    fun snegate (a:word) : word =
	if a=most_neg then raise Overflow
	else unsafe_snegate a

    fun sminus (a:word, b:word) : word =
	if nequal(b,most_neg) then
	    splus(a,snegate b)
	else if sign a <= 0 then
	    uplus(a,b)
	else raise Overflow

    fun absolute (x:word) : word =
	if sign x >= 0 then x else snegate x

    fun smult (x:word, y:word) : word =
	let val ux = unsafe_absolute x
	    val uy = unsafe_absolute y
	    val (high,low) = umult'(ux,uy)
	in  if high = zero then
		if sign x * sign y < 0 then
		    unsafe_snegate low
		else if sign low >= 0 then
		    low
		else raise Overflow
	    else raise Overflow
	end

    (*
	squot(x,y) =
	    = floor(x/y)    if x/y >= 0
	    = ceil(x/y) if x/y < 0
	srem(x,y) = x - y*squot(x,y)
    *)
    fun squot (x:word, y:word) : word =
	if x <> most_neg orelse y <> neg_one then
	    let val uq = udiv(unsafe_absolute x,unsafe_absolute y)
		val sign = sign x * sign y
	    in	if sign < 0 then unsafe_snegate uq else uq
	    end
	else raise Overflow

    fun srem (x:word, y:word) : word =
	sminus(x,smult(squot(x,y),y))

    (*
	sdiv(x,y) = floor(x/y)
	smod(x,y) = x - y*sdiv(x,y)
    *)
    fun sdiv (x:word, y:word) : word =
	let val q = squot(x,y)
	in  if sign q >= 0 orelse equal(srem(x,y),zero) then
		q
	    else
		sminus(q,one)
	end

    fun smod (x:word, y:word) : word =
	sminus(x,smult(sdiv(x,y),y))

    fun slt (x:word, y:word) : bool =
	let val nx = sign x < 0
	    val ny = sign y < 0
	in  if nx <> ny then nx else ult(x,y)
	end

    fun slte (arg:word * word) : bool =
	slt arg orelse equal arg
    val sgt : word * word -> bool = not o slte
    val sgte : word * word -> bool = not o slt

    (* ----- Conversion Operations ------ *)

    fun fromSignedHalf (low:halfword) : word =
	if rshiftl(low,halfsize - 1) = one then
	    fromhalf(low_mask,low)
	else
	    low

    fun fromUnsignedHalf (low:halfword) : word =
	low

    fun toSignedHalf (w:word) : halfword =
	let val (high,low) = tohalf w
	    val low_isneg = equal(rshiftl(low,halfsize-1),one)
	    val high' = if low_isneg then low_mask else zero
	in  if equal(high,high') then low else raise Overflow
	end

    fun toUnsignedHalf (w:word) : halfword =
	andb(low_mask,w)

    val fromInt : int -> word = W.fromInt
    val toInt : word -> int = W.toIntX
    val toIntU : word -> int = W.toInt

    (* Raises Overflow if the number is too large. *)
    fun ufromstring (radix:StringCvt.radix, ss:substring) : word =
	let fun fail () = error ("ufromstring: " ^ Substring.string ss)
	in  (case (W.scan radix Substring.getc ss) of
		SOME (w,ss) => if Substring.isEmpty ss then w else fail()
	    |	NONE => fail())
	end

    fun forbidprefix (ss:substring) (prefix:string) : unit =
	if Substring.isPrefix prefix ss then
	    error ("illegal prefix: " ^ prefix)
	else
	    ()

    fun fromHexString (s:string) : word =
	let val ss = Substring.all s
	    val _ = app (forbidprefix ss) ["0wx","0wX","0x","0X"]
	in  ufromstring(StringCvt.HEX, ss)
	end

    fun fromDecimalString (s:string) : word =
	let val ss = Substring.all s
	    fun negate (x:word) : word =
		if ugt(x,most_neg) then raise Overflow
		else unsafe_snegate x
	    val (fix,ss) =
		(case (Substring.getc ss) of
		    SOME(#"~",ss) => (negate,ss)
		|   _ => (fn w => w,ss))
	    val _ = forbidprefix ss "0w"
	    val u = ufromstring(StringCvt.DEC,ss)
	in  fix u
	end

    val toHexString : word -> string =
	W.toString

    fun toDecimalString (w:word) : string =
	let val uw = unsafe_absolute w
	    val s = W.fmt StringCvt.DEC uw
	in  if sign w < 0 then "-" ^ s else s
	end

end
