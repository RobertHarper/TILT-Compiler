(*$import TILWORD Word32 Util Char Listops String *)

(* TIL2/ML Compiler Source, 
 * Copyright (c) 1997:  Perry Cheng, Chris Stone, Greg Morrisett,
 * Robert Harper, and Peter Lee.
 *)

structure TilWord32 :> TILWORD where type word = Word32.word =
struct

    type halfword = unit
    type word = Word32.word
    val wordsize = 32
    val zero = 0w0 : word
    val one = 0w1 : word
    val neg_one = 0wxffffffff : word
    val low_mask = 0wx0000ffff : word
    val high_mask = 0wxffff0000 : word
    val most_neg = 0wx80000000 : word
    val error = fn s => Util.error "tilword32.sml" s
    val ten = 0w10 : word

    (* ------ EQUALITY OPERATIONS --------- *)
    fun equal(x:word,y) = x = y
    fun nequal(x:word,y) = not(equal(x,y))

    (* ---------- LOGICAL OPERATIONS ---------- *)
    val andb = Word32.andb
    val orb = Word32.orb
    val xorb = Word32.xorb
    val notb = Word32.notb
    fun lshift(w,i) = Word32.<<(w, Word32.fromInt i)
    fun rshiftl(w,i) = Word32.>>(w, Word32.fromInt i)
    fun rshifta(w,i) = Word32.~>>(w, Word32.fromInt i)
	
    (* ------ UNSIGNED OPERATIONS --------- *)
    val uplus = Word32.+
    val uminus = Word32.-
    fun unegate a = uplus(one,notb(a))
    val umult = Word32.*
    val udiv = Word32.div
    val ult = Word32.<
    val ugt = Word32.>
    val ulte = Word32.<=
    val ugte = Word32.>=
    val umod = Word32.mod
    fun umult'(a,b) = 
	let 
	  val half_size = wordsize div 2
	  fun get_low x = andb(low_mask,x)
	  fun get_high x = rshiftl(andb(high_mask,x),half_size)
	  val alow = get_low a
	  val blow = get_low b
	  val ahigh = get_high a
	  val bhigh = get_high b
	  val low = Word32.*(alow,blow)
	  val med = Word32.+(Word32.*(alow,bhigh),Word32.*(ahigh,blow))
	  val high = Word32.*(ahigh,bhigh)
	  val low' = get_low low
	  val low_carry = get_high low
	  val med_temp = Word32.+(low_carry,med)
	  val med' = get_low med_temp
	  val high' = Word32.+(get_high med_temp,high)
      in  (high',orb(lshift(med',half_size),low'))
      end
  fun uplus'(a,b) =
      let 
	  val low = uplus(a,b)
	  val high = if (ugte(low,a) andalso ugte(low,b))
			 then zero else one
      in (high,low)
      end


  (* ------ SIGNED OPERATIONS --------- *)
  fun sign (x:word):int = 
      if x= 0wx0 then 0 
      else if (Word32.>>(x,0w31) = 0wx0) then 1 else ~1
  fun isneg (x:word):bool = (sign x) = ~1
  fun iszero (x:word):bool = (sign x) = 0
  fun ispos (x:word):bool = (sign x) = 1
  fun splus(x,y) =
      let val z = uplus(x,y)
	  val sign_z = sign z
	  val sign_x = sign x
	  val sign_y = sign y
	  val nooverflow = not (sign_x = sign_y andalso (sign_x <> 0) andalso
				(sign_x <> sign_z))
      in
	  if nooverflow then z else raise Overflow
      end
  fun sminus (x,y) =
      let val z = uminus(x,y)
	  val sign_z = sign z
	  val sign_x = sign x
	  val sign_y = sign y
	  val nooverflow = 
	      not((sign_x <> sign_y) andalso (sign_x <> 0) andalso 
		  (sign_x <> sign_z))
      in
	  if nooverflow then z else raise Overflow
      end
  fun snegate x = sminus(0wx0,x)
  fun absolute x = if (equal(x,most_neg)) 
		       then raise Overflow
		   else if (sign x >= 0) then x else snegate x
  fun smult (x,y) =
      let val z = umult(x,y)
	  val nooverflow = (x = 0wx0) orelse udiv(z,x) = y
      in
	  if nooverflow then z else raise Overflow
      end
  fun sdiv (x,y : word) =
      if x = uminus(0wx0,0wx1) andalso y=neg_one then raise Overflow
      else 
	  let
	      val signx = sign x
	      val signy = sign y
	      val signres = signx * signy
	      val ux = if (signx<0) then snegate x else x
	      val uy = if (signy<0) then snegate y else y
	      val ures = udiv(ux,uy)   (* udiv raises Div when uy is zero, 
					so we don't have to worry about that *)
	      val res = if (signres<0) then snegate ures else ures
	  in res
	  end
  fun smod(x,y) = sminus(x,(smult(sdiv(x,y),y)))
  fun squot arg = raise Util.UNIMP
  fun srem arg = raise Util.UNIMP

  fun slt(x:word,y:word) = 
      case (isneg x,isneg y) of
	  (false,false) => ult(x,y)
	| (true,false) => true
	| (false,true) => false
	| (true,true) => ult(x,y)
  fun sgt(x:word,y:word) =
      case (isneg x,isneg y) of
	  (false,false) => ugt(x,y)
	| (true,false) => false
	| (false,true) => true
	| (true,true) => ult(x,y)
  fun slte(x,y) = not(sgt(x,y))
  fun sgte(x,y) = not(slt(x,y))

  (* ----- Conversion Operations ------ *)
  fun fromSignedHalf () = zero
  fun fromUnsignedHalf () = zero
  fun toSignedHalf _ = ()
  fun toUnsignedHalf _ = ()
  val fromInt = Word32.fromInt
  fun toInt i =
      let val i' = Word32.toIntX i
	  val j = Word32.fromInt i'
      in if equal(i,j) then i' else raise Overflow
      end
  fun fromHexString arg = (case (Word32.fromString arg) of
			       SOME res => res
			     | NONE => error "fromHexString: got a non Hex-String")

  fun fromDecimalString str = 
      let val ten = fromInt 10
	  val (sign,chars) = (case (String.explode str) of
				  (#"~" :: rest) => (neg_one,rest)
				| all => (one,all))
	  fun digit d = if (Char.isDigit d) 
			    then fromInt(ord d - ord #"0")
			else error "fromDecimalString called with non-decimal string"
	  fun loop acc [] = acc
	    | loop acc (a::b) = loop (splus(smult(ten,acc),digit a)) b
      in smult(sign,loop zero chars)
      end

  fun fromWordStringLiteral ws = if (size ws > 3 andalso
				    substring(ws,0,3) = "0wx")
				    then fromHexString (substring(ws,3,(size ws) - 3))
				else if (size ws > 2 andalso
					 substring(ws,0,2) = "0w")
					 then fromDecimalString (substring(ws,2,(size ws) - 2))
				     else error ("fromWordStringLiteral got an illegal string: " ^ ws)

  fun toHexString num = 
      let 
	  val pos = rev(Listops.count 8)
	  fun help i = 
	      let val x = toInt(andb(fromInt 15, rshiftl(num,i*4)))
	      in if (x < 10) 
		     then chr(ord #"0" + x)
		 else chr(ord #"a" + (x - 10))
	      end
	  val chars = map help pos
      in implode chars
      end
  fun toDecimalString w =
      let
	  val is_neg = sign w = ~1
	  val (is_least,w) = if (equal(w,most_neg)) 
				 then (true,absolute(splus(w,one))) else (false,absolute w)
	  fun loop w = if (equal(w,zero)) then []
		       else let val q = udiv(w,ten)
				val r = umod(w,ten)
				val digit = chr(ord #"0" + (toInt r))
			    in digit :: (loop q)
			    end
	  val res = if (equal(w,zero)) then [#"0"] else rev(loop w)
      in
	  implode (if is_neg then #"-"::res else res)
      end


(*
  val uwordToString = fn (x:word) => Word32.toString x
  fun uwordToRealString w = 
    let 
      val d = Word32.fromInt(1000000)
      val upper = Word32.div(w,d)
      val lower = Word32.mod(w,d)
      val upper_str = Int.toString(Word32.toInt upper)
      val lower_str = Int.toString(Word32.toInt lower)
      fun loop 0 = ""
	| loop n = "0" ^ (loop (n-1))
    in (if (upper = 0w0) 
	  then lower_str
	else upper_str ^ (loop (6 - size lower_str) ^ lower_str))
      ^ ".0"
    end

  fun log_2 (i:word) : word * bool = 
      let fun loop(pos:int,j:word,highest:int,first) = 
	  if pos > 31 then (highest,first)
	  else 
	  let val (highest',first') = 
	      if andb(j,0wx1)= 0wx1 then 
		  if first = ~1 then 
		      (pos,pos)
		  else (pos,first)
	      else (highest,first)
	      val j' = rshift(j,1)
	  in
	      loop(pos+1,j',highest',first')
	  end
	  val (highest,first) = loop(0,i,~1,~1)
      in
	  (intToWord highest,first=highest andalso (first <> ~1))
      end
  (* taken from rtloper.sml ... *)

(*  If we compile this code, it causes lots of problems.  Try
 *  compiling it without the signature match above and you'll
 *  see the problem.
 local
  val wmod' : word = Word32.fromInt (256*256)
  val wmod_real : real = real (256 * 256)
 in
  fun sword_to_real w : real = 
	let 

            val bottom : int = Word32.toInt(Word32.mod(w,wmod'));
            val top : int = Word32.toInt(Word32.div(w,wmod'));
            val neg : real = if (top > 128 * 256) 
			then (wmod_real) * (wmod_real)
			else 0.0
         in (real bottom) + wmod_real * (real top) - neg
         end
  end
*)

  fun sword_to_real _ = raise UNIMP

  (* be careful when redefining standard operators *)

  val intlt = (op <) : int * int -> bool




      



  exception Word32mod





  fun wordToString x =
      if x = (intToWord ~1) then "~1" else 
	  let val (sign,num) = 
	      if lessThanZero(x) then ("~",uminus(0wx0,x))
	      else ("",x)
	  in
	      sign ^ (uwordToString x)
	  end
  fun wordToRealString x =
	  let val (sign,num) = 
	      if lessThanZero(x) then ("-",uminus(0wx0,x))
	      else ("",x)
	  in
	      sign ^ (uwordToRealString x)
	  end
(*
  (* depends upon little endian *)
  fun sub_word_little(s,i:int) = 
      let val w = intToWord
	  val b0 = w(ord(String.sub(s,i))) handle Subscript => 0wx0
	  val b1 = w(ord(String.sub(s,i+1))) handle Subscript => 0wx0
	  val b2 = w(ord(String.sub(s,i+2))) handle Subscript => 0wx0
	  val b3 = w(ord(String.sub(s,i+3))) handle Subscript => 0wx0
      in
	  orb(orb(orb(b0,lshift(b1,8)),lshift(b2,16)),lshift(b3,24))
      end

  fun sub_word_big(s,i:int) = 
      let val w = intToWord
	  val b3 = w(ord(String.sub(s,i))) handle Subscript => 0wx0
	  val b2 = w(ord(String.sub(s,i+1))) handle Subscript => 0wx0
	  val b1 = w(ord(String.sub(s,i+2))) handle Subscript => 0wx0
	  val b0 = w(ord(String.sub(s,i+3))) handle Subscript => 0wx0
      in
	  orb(orb(orb(b0,lshift(b1,8)),lshift(b2,16)),lshift(b3,24))
      end

  val little_endian = let val a = Array.array(1,4)
			  val a' : Word8Array.array = System.Unsafe.cast a
		      in
			  Word8Array.sub(a',0) <> 0w0
		      end

  val sub_word = if little_endian then sub_word_little else sub_word_big
*)

*)
end
