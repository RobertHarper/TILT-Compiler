(*$import *)
(* At the time the prelude is elaborated, it may assume that
 * compiler primitives are available.  Of special interest
 * is the identifier littleEndian which is a boolean value.
 *)
    

(* standard fixity *)
infix  0 before
infix  3 := o
infix  4 = <> < > <= >= 
infix  5 @
infixr 5 ::
infix  6 + - ^
infix  7 div mod quot rem / * 
infix  9 << >> ~>> && || 

(* standard types *)

datatype 'a list = nil | :: of 'a * 'a list
datatype 'a susp = Susp of unit -> 'a 
datatype 'a option = NONE | SOME of 'a
datatype order = GREATER | LESS | EQUAL 
type int32 = int
type word = word32


(* standard exceptions *)
exception Match and Bind                   (* pattern related *)
exception Overflow and Div and Mod and Quot (* arithmetic *)
      and Floor and Sqrt and Exp and Ln 
exception Ord and Chr and Substring       (* character/string *)
exception Hd and Tl and NthTail and Nth   (* list related *)
exception Subscript and Size              (* array related *)
exception Interrupt            
exception Io of char vector
exception Domain
exception Span
exception Fail of char vector

(* must have vector_eq for vector types to be used: may change if elaborator changes *)
fun vector_eq (equaler : 'a * 'a -> bool) (x : 'a vector, y : 'a vector) = 
    let val lx = vector_length x
	val ly = vector_length y
	fun vector_eq_loop n = 
	    ugte(n,lx) orelse (equaler(unsafe_vsub(x,n),unsafe_vsub(y,n))
			       andalso (vector_eq_loop (uplus(n,0w1))))
    in  (lx = ly) andalso vector_eq_loop 0w0
    end
type string = char vector
datatype substring = SS of (string * int * int)

(* Predefined external functions *)
(* we cannot use types defined in this module for the externs 
   because of a deficiency in the phase-splitter *)
extern exnNameRuntime : (exn, char vector) -->
extern exnMessageRuntime : (exn, char vector) -->
extern real_logb : (float, int) -->
extern real_scalb : (float * int, float) -->
extern sqrt : (float, float) -->
extern exp : (float, float) -->
extern ln : (float, float) -->
extern log10 : (float, float) -->
extern sin : (float, float) -->
extern cos : (float, float) -->
extern tan : (float, float) -->
extern atan : (float, float) -->
extern asin : (float, float) -->
extern acos : (float, float) -->
extern tanh : (float, float) -->
extern sinh : (float, float) -->
extern cosh : (float, float) -->
extern setRoundingMode : (int, int) -->
extern getRoundingMode : (int, int) -->
extern ml_timeofday : (unit, (int * int)) -->

    fun f o g = fn x => f(g x)
    fun a before b = a
    fun ignore _ = ()

    fun exnName exn = exnNameRuntime exn
    fun exnMessage exn = exnMessageRuntime exn

	fun rev l = 
	    let fun revappend([],x) = x
		  | revappend(hd::tl,x) = revappend(tl,hd::x)
	    in  revappend(l,[])
	    end

	fun length l = 
	    let fun length' (n,[]) = n
		  | length' (n,_::rest) = length' (n+1,rest)
	    in  length' (0,l)
	    end

	fun array (s:int, e : 'a) : 'a array =
	    if s<0 then raise Size
	    else unsafe_array(int32touint32 s,e) 



	fun vsub (a : 'a vector, index :int) : 'a =
	    let val index = int32touint32 index
	    in  if (ugte(index, vector_length a))
		    then raise Subscript
		else unsafe_vsub(a,index)
	    end





	fun ord(x : char) : int = uint8toint32 x
	fun chr(x : int) : char = int32touint8 x

val stringmaxsize = 1024 * 1024
val vectormaxlength = 1024 * 1024
val arraymaxlength = 1024 * 1024



	fun size(x : string) : int = uint32toint32(vector_length x)
	fun explode(x:string): char list =
	    let val sz = vector_length x
		fun explode_loop(i,accum) = 
		    if ult(i,sz)
			then explode_loop (uplus(i,0w1),(unsafe_vsub(x,i))::accum)
		    else rev accum
	    in
		explode_loop(0w0,[])
	    end
	
	fun sub(x : string, i : int) = vsub(x,i)

	local
    (* copies len words from vector x starting at x_start to array y starting at y_start *)
	    fun wordcopy(len : uint,
			 x:uint vector, x_start:uint,
			 y:uint array, y_start:uint) =
		let val y_stop = uplus(y_start,len)
		    fun wordcopyLoop (x_cur,y_cur) = 
			if ult(y_cur,y_stop)
			    then (unsafe_update(y,y_cur,unsafe_vsub(x,x_cur)); 
				  wordcopyLoop(uplus(x_cur,0w1),uplus(y_cur,0w1)))
			else ()
		in  wordcopyLoop(x_start,y_start)
		end
    (* same as before with with bytes *)	    
	    fun bytecopy(len : uint,
			 x:char vector, x_start:uint,
			 y:char array, y_start:uint) =
		let val y_stop = uplus(y_start,len)
		    fun bytecopyLoop (x_cur,y_cur) = 
			if ult(y_cur,y_stop)
			    then (unsafe_update(y,y_cur,unsafe_vsub(x,x_cur)); 
				  bytecopyLoop(uplus(x_cur,0w1),uplus(y_cur,0w1)))
			else ()
		in  bytecopyLoop(x_start,y_start)
		end
	in
	    fun (x : string) ^ (y : string) = 
		let val x_sz = vector_length x
		    val y_sz = vector_length y
		    val a_sz = uplus(x_sz,y_sz)
		    val c = chr 0
		    val a : char array = unsafe_array(a_sz,c)
		    val aw = uinta8touinta32 a
		    val xw = uintv8touintv32 x
		    val _ = wordcopy((uplus(x_sz,0w3))>>2, xw,0w0, aw,0w0)
		    val _ = bytecopy(y_sz,y,0w0,  a,x_sz)
		in  unsafe_array2vector a
		end
		
	    fun implode(x : char list) : string = 
		let val sz = int32touint32 (length x)
		    val c = chr 0
		    val a : char array = unsafe_array(sz,c)
		    fun loop i [] = ()
		      | loop i (c::rest) = 
			(unsafe_update(a,i,c);
			 loop (uplus(i,0w1)) rest)
		    val _ = loop 0w0 x
		in  unsafe_array2vector a
		end

	    fun revImplode(len : int, x : char list) : string = 
		let val sz = int32touint32 len
		    val c = chr 0
		    val a : char array = unsafe_array(sz,c)
		    fun loop i [] = ()
		      | loop i (c::rest) = 
			(unsafe_update(a,i,c);
			 loop (uminus(i,0w1)) rest)
		    val _ = loop (uminus(sz,0w1)) x
		in  unsafe_array2vector a
		end
	    
	    fun substring (a : string, start : int, len : int) : string =
		let val size = vector_length a
		    val start = if start<0 then raise Substring else (int32touint32 start)
		    val len = if len<0 then raise Substring else (int32touint32 len)
		    val stop = uplus(len,start)
		    val c = chr 0
		    val res : char array = unsafe_array(len,c)
		    val _ = if (ugt(stop,size))
				then raise Substring
			    else
				bytecopy(len,a,start,res,0w0)
		in  unsafe_array2vector res
		end
	end
    
	fun char_eq (cx:char,cy:char) = cx = cy
	val string_eq = vector_eq char_eq

	(* create returns a character array filled with any character it likes *)
	fun create sz : char array = if (sz>0)
					 then unsafe_array(int32touint32 sz,#"\000")
				     else raise Size





	fun (a : int) mod (b : int) =
	    let val temp = a rem b
	    in if ((b>0 andalso temp>=0) orelse
		   (b<0 andalso temp<=0))
		   then temp
	       else temp+b
	    end
	fun (a : int) div (b : int) =
	    let val temp = a quot b
	    in  (* same if sign of a and b agree *)
		if ((a>=0 andalso b>0) orelse (a<=0 andalso b<0))
		    then temp
		else 
		    if (b * temp = a)   (* same if exact div *)
			then temp
		    else temp - 1       (* here's where they differ *)
	    end
	val imod = op mod
	val idiv = op div

(* unimped parts of the standard basis - or exception indicating internal error in basis *)
exception LibFail of string