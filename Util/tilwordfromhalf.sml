functor TilWordFromHalf (W:TILWORD) :> TILWORD
    where type halfword = W.word =
struct
    val error = fn s => Util.error "tilwordfromhalf.sml" s

    type halfword = W.word
    type word = halfword * halfword (* high, low *)
    val wordsize : int = W.wordsize * 2
    val halfsize : int = W.wordsize
    val zero : word = (W.zero,W.zero)
    val one : word = (W.zero,W.one)
    val neg_one : word = (W.neg_one,W.neg_one)
    val low_mask : word = (W.zero,W.neg_one)
    val high_mask : word = (W.neg_one,W.zero)
    val most_neg : word = (W.most_neg, W.zero)
    val most_pos : word = (W.most_pos, W.neg_one)

    val sixteen : word = (W.zero, W.fromInt 16)
    val ten : word = (W.zero, W.fromInt 10)

    (* ------ EQUALITY OPERATIONS --------- *)
    fun equal ((high,low):word, (high',low'):word) : bool =
	W.equal(high,high') andalso W.equal(low,low')
    val nequal : word * word -> bool = not o equal

    (* ----------- LOGICAL OPERATIONS  --------------- *)

    fun lift1 (f:W.word -> W.word) : word -> word =
	fn (high,low) => (f high, f low)

    fun lift2 (f:W.word * W.word -> W.word) : word * word -> word =
	fn ((high,low), (high',low')) => (f(high,high'), f(low,low'))

    val notb : word -> word = lift1 W.notb
    val andb : word * word -> word = lift2 W.andb
    val orb : word * word -> word = lift2 W.orb
    val xorb : word * word -> word = lift2 W.xorb

    fun lshift ((high,low):word, shift:int) : word =
	if shift >= halfsize then
	    lshift((low,W.zero), shift-halfsize)
	else
	    let val lowlow = W.lshift(low,shift)
		val lowhigh = W.rshiftl(low,halfsize - shift)
		val highhigh = W.lshift(high,shift)
	    in	(W.orb(highhigh,lowhigh), lowlow)
	    end

    fun rshifta ((high,low):word, shift:int) : word =
	if shift >= halfsize then
	    rshifta((W.rshifta(high,halfsize), high), shift-halfsize)
	else
	    let val highhigh = W.rshifta(high,shift)
		val highlow = W.lshift(high,halfsize - shift)
		val lowlow = W.rshiftl(low,shift)
	    in	(highhigh, W.orb(highlow,lowlow))
	    end

    fun rshiftl ((high,low):word, shift:int) : word =
	if shift >= halfsize then
	    rshiftl((W.zero,high), shift-halfsize)
	else
	    let val highhigh = W.rshiftl(high,shift)
		val highlow = W.lshift(high,halfsize - shift)
		val lowlow = W.rshiftl(low,shift)
	    in	(highhigh, W.orb(highlow,lowlow))
	    end

    (* ------- UNSIGNED OPERATIONS ------------- *)

    fun uplus' ((high,low):word, (high',low'):word) : word * word =
	let val (carry,low) = W.uplus'(low,low')
	    val (c1,high) = W.uplus'(high,high')
	    val (c2,high) = W.uplus'(carry,high)
	    val c = W.uplus(c1,c2)
	in  ((W.zero,c), (high,low))
	end
    val uplus : word * word -> word = #2 o uplus'

    fun unsafe_snegate (n:word) : word = uplus(one,notb n)

    fun uminus (a:word, b:word) : word = uplus(a,unsafe_snegate b)

    fun umulthalf (high:halfword, low:halfword, m:halfword) : halfword * halfword * halfword =
	let val (carry,low) = W.umult'(m,low)
	    val (high,med) = uplus(W.umult'(m,high), (W.zero,carry))
	in  (high,med,low)
	end

    fun umult' ((ahigh,alow):word, (bhigh,blow):word) : word * word =
	let val (d2',d1',d0) = umulthalf(ahigh,alow,blow)
	    val (d3',d2'',d1'') = umulthalf(ahigh,alow,bhigh)
	    val (carry,d1) = W.uplus'(d1',d1'')
	    val (carry,d2) = uplus(W.uplus'(d2',d2''), (W.zero,carry))
	    val d3 = W.uplus(carry,d3')
	    val low = (d1,d0)
	    val high = (d3,d2)
	in  (high,low)
	end

    val umult : word * word -> word = #2 o umult'

    fun ult ((high,low):word, (high',low'):word) : bool =
	W.ult(high,high') orelse
	(W.equal(high,high') andalso W.ult(low,low'))

    fun ulte (arg:word * word) : bool =
	ult arg orelse equal arg
    val ugt : word * word -> bool = not o ulte
    val ugte : word * word -> bool = not o ult

    fun udiv (a:word, b:word) : word =
	let fun high_bit ((high,_):word) : bool =
		W.equal(W.one,W.rshiftl (high, halfsize - 1))
	    fun find_shift (n1:word, n2:word) : int =
		if (ult(n1,n2) orelse high_bit n2)
		then 0
		else 1 + find_shift (n1, lshift (n2, 1))
	    (* div_loop invariant: rshiftl (n2, shifts) = divisor. *)
	    fun div_loop (n1:word, n2:word, shifts:int) : word =
		if shifts = 0 then
		    if ult(n1,n2) then zero else one
		else
		    if ult(n1,n2) then
			div_loop (n1, rshiftl (n2, 1), shifts-1)
		     else
			let val n1 = uminus(n1,n2)
			    val n2 = rshiftl(n2,1)
			    val q' = div_loop(n1,n2,shifts-1)
			    val q = uplus(lshift(one,shifts), q')
			in  q
			end
	in
	    if equal(b,zero) then raise Div
	    else
		let val shift = find_shift (a,b)
		in  div_loop (a, lshift (b, shift), shift)
		end
	end

    fun umod (a:word, b:word) : word =
	uminus(a,umult(b,udiv(a,b)))

    (* ------- SIGNED OPERATIONS ------------- *)

    fun sign (x as (high,_):word) : int =
	if equal(x,zero) then 0
	else if W.sign high < 0 then ~1 else 1

    fun slt (x:word, y:word) : bool =
	let val nx = sign x < 0
	    val ny = sign y < 0
	in  if nx <> ny then nx else ult(x,y)
	end

    fun slte (arg:word * word) : bool =
	slt arg orelse equal arg
    val sgt : word * word -> bool = not o slte
    val sgte : word * word -> bool = not o slt

    fun splus (x:word, y:word) : word =
	let val z = uplus(x,y)
	    val sx = sign x
	    (*
		negative overflow when x, y < 0 and z >= 0
		positive overflow when x, y > 0 and z < 0
	    *)
	in  if sx = 0 orelse sx <> sign y orelse sx = sign z then z
	    else raise Overflow
	end

    fun snegate (a:word) : word =
	if equal(a,most_neg) then raise Overflow
	else unsafe_snegate a

    fun unsafe_absolute (x:word) : word =
	if sign x >= 0 then x else unsafe_snegate x

    fun absolute (x:word) : word =
	if sign x >= 0 then x else snegate x

    fun sminus (a:word, b:word) : word =
	if nequal(b,most_neg) then
	    splus(a, snegate b)
	else if sign a <= 0 then
	    uplus(a,b)
	else raise Overflow

    fun smult (x:word, y:word) : word =
	let val ux = unsafe_absolute x
	    val uy = unsafe_absolute y
	    val (high,low) = umult'(ux,uy)
	in  if equal(high,zero) then
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
	if nequal(x,most_neg) orelse nequal(y,neg_one) then
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

    (* ----- Conversion Operations ------ *)

    fun fromUnsignedHalf (low:halfword) : word =
	(W.zero,low)

    fun fromSignedHalf (low:halfword) : word =
	let val high = if W.sign low < 0 then W.neg_one else W.zero
	in  (high,low)
	end

    fun toUnsignedHalf ((_,low):word) : halfword = low

    fun toSignedHalf ((high,low):word) : halfword =
	let val high' = if W.sign low < 0 then W.neg_one else W.zero
	in  if W.equal(high,high') then low else raise Overflow
	end

    (* Used when intsize <= halfsize *)
    val fromSmallInt : int -> word = fromSignedHalf o W.fromInt
    val toSmallInt : word -> int = W.toInt o toSignedHalf
    val toSmallIntU : word -> int = W.toIntU o toSignedHalf

    (* Used when intsize > halfsize.  *)
    (* pow(n,x) = n*2^x *)
    fun pow (n:int, x:int) : int =
	if x = 0 then n
	else pow(n*2, x-1)

    (* scale() = 2^halfsize *)
    val scale : unit -> int = Util.memoize(fn () => pow(1,halfsize))

    fun fromLargeInt (i:int) : word =
	let val s = scale()
	    val high = W.fromInt (i div s)
	    val low = W.fromInt (i mod s)
	in  (high,low)
	end

    fun toLargeIntU ((high,low):word) : int =
	let val s = scale()
	    val high = W.toIntU high
	    val low = W.toIntU low
	in  high * s + low
	end

    fun toLargeInt (w:word) : int =
	if sign w < 0 then
	    ~1 * (toLargeIntU(unsafe_absolute w))
	else
	    toLargeIntU w

    val (fromInt : int -> word, toInt : word -> int, toIntU : word -> int) =
	let val small = (fromSmallInt,toSmallInt,toSmallIntU)
	    val large = (fromLargeInt,toLargeInt,toLargeIntU)
	in
	    (case Int.precision of
		SOME intsize =>
		    if intsize <= halfsize then small
		    else large
	    |	NONE => large)
	end

    fun fromstring (neg:bool, base:word, todigit:char -> int, ss:substring) : word =
	let fun checkoverflow (high:word,low:word) : word =
		if equal(high,zero) then low
		else raise Overflow
	    fun add (a:word, b:word) : word = checkoverflow(uplus'(a,b))
	    fun mult (a:word, b:word) : word = checkoverflow(umult'(a,b))
	    fun folder (c:char, (m,w):(word * word) * word) : (word * word) * word =
		let val digit = fromInt(todigit c)
		    val m = checkoverflow m
		    val w = add(w,mult(m,digit))
		    val m = umult'(m,base)
		in  (m,w)
		end
	    (* Skip leading zeros *)
	    fun skip (ss:substring) : substring =
		if Substring.isEmpty ss then ss
		else
		    if Substring.sub(ss,0) = #"0" then
			skip(Substring.triml 1 ss)
		    else ss
	    val (_,w) = Substring.foldr folder ((zero,one),zero) (skip ss)
	in  if neg then
		if ugt(w,most_neg) then raise Overflow
		else unsafe_snegate w
	    else w
	end

    fun fromHexString (s:string) : word =
	let fun todigit (c:char) : int =
		if Char.isHexDigit c then
		    let val n = ord c
		    in	if n < 65 then n - 48
			else if n < 97 then n - 55
			else n - 87
		    end
		else error "illegal character in hex string"
	    val ss = Substring.all s
	in  fromstring (false,sixteen,todigit,ss)
	end

    fun fromDecimalString (s:string) : word =
	let fun todigit (c:char) : int =
		if Char.isDigit c then ord c - 48
		else error "illegal character in decimal string"
	    val ss = Substring.all s
	    val (ss,neg) =
		if Substring.size ss > 0 andalso Substring.sub(ss,0) = #"~" then
		    (Substring.triml 1 ss, true)
		else
		    (ss,false)
	in  fromstring (neg,ten,todigit,ss)
	end

    fun tostring (neg:bool, base:word, fromdigit:int -> char, w:word) : string =
	let fun digits (w:word, acc:char list) : char list =
		if equal(w,zero) then acc
		else
		    let val d = umod(w,base)
			val w = udiv(w,base)
			val c = fromdigit (toInt d)
		    in	digits(w,c::acc)
		    end
	    val digits = digits(w,nil)
	    val num =
		if neg then #"-" :: digits
		else if null digits then #"0" :: digits
		else digits
	in  String.implode num
	end

    fun toHexString (w:word) : string =
	let fun fromdigit (digit:int) : char =
		if digit < 10 then chr(digit + 48)
		else chr(digit + 87)
	in  tostring(false,sixteen,fromdigit,w)
	end

    fun toDecimalString (w:word) : string =
	let fun fromdigit (digit:int) : char =
		chr(digit + 48)
	in  tostring(sign w < 0,ten,fromdigit,unsafe_absolute w)
	end

end
