(*$import MATH Prelude *)

structure Math64 : MATH =
  struct
    (* div and mod will eventually be overloaded to work at multiple types *)
    val div = idiv
    val mod = imod

    type real = real

    val pi = 3.14159265358979323846
    val e  = 2.7182818284590452354

    (* eta expansion to distingush between ML arrows and external arrows *)
    val sqrt  : real -> real = fn arg => Ccall(sqrt, arg)
    val sin   : real -> real = fn arg => Ccall(sin, arg)
    val cos   : real -> real = fn arg => Ccall(cos, arg)
    val tan   : real -> real = fn arg => Ccall(tan, arg)
    val asin  : real -> real = fn arg => Ccall(asin, arg)
    val acos  : real -> real = fn arg => Ccall(acos, arg)
    val atan  : real -> real = fn arg => Ccall(atan, arg)
    val exp   : real -> real = fn arg => Ccall(exp, arg)
    val ln    : real -> real = fn arg => Ccall(ln, arg)
    val log10 : real -> real = fn arg => Ccall(log10, arg)
    val sinh  : real -> real = fn arg => Ccall(sinh, arg)
    val cosh  : real -> real = fn arg => Ccall(cosh, arg)
    val tanh  : real -> real = fn arg => Ccall(tanh, arg)

    (* the following copied from math64.sml -- we neeed atan2 and pow since
       we don't have multi-arg call to C yet *)
    infix 4 ==
    val (op ==) = float_eq
    local
	val one = 1.0
	val PIo2   =  1.5707963267948966192E0
	val PI = pi
	fun atanpy y = (* y>=0 *)
	    if y>one then PIo2 - atan(one/y) else atan(y)

	fun atan2pypx(x,y) = 
	    if y>x then PIo2 - atan(x/y) else atan(y/x)

	fun atan2py(x,y) = 
	    if x >= 0.0 then atan2pypx(x,y) 
	    else if x == 0.0 andalso y == 0.0 then 0.0
		 else PI - atan2pypx(~x,y)

    in  fun atan y = (* miraculously handles inf's and nan's correctly *)
	if y<=0.0 then ~(atanpy(~y)) else atanpy y
	fun atan2(y,x) = (* miraculously handles inf's and nan's correctly *)
	    if y>=0.0 then atan2py(x,y) else ~(atan2py(x,~y))
    end

    local
	val zero = 0.0
	fun copysign(a,b) = (case (a<zero, b<zero)
				 of (true,true) => a
			       | (false,false) => a
			       | _ => ~a)
	fun isNaN x = not(x==x)
	val plusInfinity = 1E300 * 1E300
	val minusInfinity = ~plusInfinity
	val NaN = 0.0 / 0.0
	
        (* This is the IEEE double-precision maxint; won't work accurately on VAX *)
	val maxint = 4503599627370496.0

	(* realround(x) returns x rounded to some nearby integer, almost always
	 * the nearest integer.
	 *  May be applied to inf's and nan's.
	 *)
	fun realround x = if x>=0.0 then x+maxint-maxint else x-maxint+maxint

	fun isInt y = realround(y)-y == 0.0
	fun isOddInt(y) = isInt((y-1.0)*0.5)
	fun intpow(x,0) = 1.0
	  | intpow(x,y) = let val h = y div 2
			      val z = intpow(x,h)
			      val zz = z*z
			  in if y=(h+h) then zz else x*zz
			  end
	(* may be applied to inf's and nan's *)
	fun abs x = if x < zero then ~x else x
    in
	fun pow(x : real,y : real) = 
	    if y>0.0
		then if y<plusInfinity 
			 then if x > minusInfinity
			 then if x > 0.0
				then exp(y*ln(x))
				else if x == 0.0
			          then if isOddInt(y)
				       then x
				       else 0.0
			          else if isInt(y)
				       then intpow(x,floor(y+0.5))
				       else NaN
			 else if isNaN x
			  then x
			  else if isOddInt(y)
				then x
				else plusInfinity
		   else let val ax = abs(x)
			 in if ax>1.0 then plusInfinity
			    else if ax<1.0 then 0.0
			    else NaN
                        end
               else if y < 0.0
	         then if y>minusInfinity
		   then if x > minusInfinity
			then if x > 0.0
		             then exp(y*ln(x))
			     else if x==0.0 
			          then if isOddInt(y)
		  		     then copysign(plusInfinity,x)
			             else plusInfinity
				  else if isInt(y)
				       then 1.0 / intpow(x, floor(~y+0.5))
				       else NaN
			else if isNaN x
			 then x
			 else if isOddInt(y)
			     then ~0.0
			     else 0.0
		   else let val ax = abs(x)
			 in if ax>1.0 then 0.0
			    else if ax<1.0 then plusInfinity
			    else NaN
                        end
               else if isNaN y
		 then y
	       else 1.0
    end
  end
