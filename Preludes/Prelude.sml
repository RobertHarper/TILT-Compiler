(* At the time the prelude is elaborated, it may assume that
 * truly basic primitives are available.  Of special interest
 * is the identifier littleEndian which is a boolean value.
 *)
    

(* standard fixity *)
infix  3 := o
infix  4 = <> < > <= >= 
infix  5 @
infixr 5 ::
infix  6 + - ^
infix  7 div mod quot rem / * 
infix  9 << >> && || 

(* standard types *)
type string = char vector
datatype 'a list = nil | :: of 'a * 'a list
datatype 'a susp = Susp of unit -> 'a 


(* standard exceptions *)
exception Match and Bind                   (* pattern related *)
exception Overflow and Div and Mod and Quot (* arithmetic *)
      and Floor and Sqrt and Exp and Ln 
exception Ord and Chr and Substring       (* character/string *)
exception Hd and Tl and NthTail and Nth   (* list related *)
exception Subscript and Size              (* array related *)
exception Interrupt            
exception Io of string
  

structure List = 
    struct
	fun [] @ y = y
	  |  (x::xs) @ y = x :: (xs @ y)
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
	fun foldl f acc [] = acc
	  | foldl f acc (first::rest) = foldl f (f(first,acc)) rest
	fun map f [] = []
	  | map f (a::b) = (f a)::(map f b)
    end
open List



structure Array = 
    struct
	type 'a array1 = 'a array
	fun array1 (s:int, e : 'a) : 'a array =
	    if s<0 then raise Size
	    else unsafe_array(int32touint32 s,e) 
		
	fun sub1 (a : 'a array, index :int) : 'a =
	    let val index = int32touint32 index
	    in  if (ugte(index, array_length a))
		    then raise Subscript
		else unsafe_sub(a,index)
	    end
	
	fun update1 (a : 'a array, index :int, e : 'a) : unit =
	    let val index = int32touint32 index
	    in  if (ugte(index, array_length a))
		    then raise Subscript
		else unsafe_update(a,index,e)
	    end
	
	(* 2D arrays are in column major order *)
	type 'a array2 = {rows : uint, columns : uint, data : 'a array}
	    
	fun array2(rows : int, columns : int, e : 'a) : 'a array2 = 
	    if rows<0 orelse columns<0 then raise Size
	    else let val rows = int32touint32 rows
		     val columns = int32touint32 columns
		 in  {rows=rows, columns=columns,
		      data=unsafe_array(umult(rows,columns),e)}
		 end
	     
	fun sub2 ({rows,columns,data} : 'a array2, s :int, t:int) : 'a =
	    let val s = int32touint32 s
		  val t = int32touint32 t
	    in  if (ugte(s,rows) orelse ugte(t,columns))
		    then raise Subscript
		else unsafe_sub(data,uplus(umult(s,columns),t))
	    end
	
	fun update2 ({rows,columns,data} : 'a array2, s : int, t:int, e) : unit =
	    let val s = int32touint32 s
		val t = int32touint32 t
	    in  if (ugte(s,rows) orelse ugte(t,columns))
		    then raise Subscript
		else unsafe_update(data,uplus(umult(s,columns),t),e)
	    end
	
	fun length2 ({rows,columns,data} : 'a array2) : int * int = (uint32toint32 rows,
								     uint32toint32 columns)
	      
    end
open Array

structure Vector =
    struct
	fun vector1 (s:int, e : 'a) : 'a vector =
	    if s<0 then raise Size
	    else unsafe_vector(int32touint32 s,e)
		
	fun vsub1 (a : 'a vector, index :int) : 'a =
	    let val index = int32touint32 index
	    in  if (ugte(index, vector_length a))
		    then raise Subscript
		else unsafe_vsub(a,index)
	    end
	fun vector_eq (equaler : 'a * 'a -> bool) (x : 'a vector, y : 'a vector) = 
	    let val lx = vector_length x
		val ly = vector_length y
		fun vector_eq_loop n = 
		    (n = 0w0) orelse (equaler(unsafe_vsub(x,n),unsafe_vsub(y,n))
					 andalso (vector_eq_loop (uminus(n,0w1))))
	    in  (lx = ly) andalso vector_eq_loop (uminus(lx,0w1))
	    end
    end
open Vector

structure Misc = 
    struct
	fun (f o g) x = f(g x)
	fun app f =
	    let fun app_loop [] = ()
		  | app_loop (hd::tl) = (f hd; app_loop tl)
	    in
		app_loop
	    end
	
	(* refs *)
	fun inc r = r := !r + 1
	fun dec r = r := !r - 1
	(* misc *)
	fun min(a:int,b) = if a<b then a else b
	fun max(a:int,b) = if a>b then a else b
      end
  
open Misc
  
structure Char = 
    struct
	fun ord(x : char) : int = uint8toint32 x
	fun chr(x : int) : char = int32touint8 x
    end
open Char

structure String = 
    struct
	fun size(x : string) : int = uint32toint32(vector_length x)
	fun explode(x:string): char list =
	    let val sz = size x
		fun explode_loop(i,accum) = 
		    if (i < sz)
			then explode_loop (i+1,(vsub1(x,i))::accum)
		    else List.rev accum
	    in
		explode_loop(0,[])
	    end
	
	
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
	    fun concat(x : string, y : string) = 
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
	    val op ^ = concat
		
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
    end
open String

structure Integer = 
    struct
	fun toString (i:int):string =
	    let fun ord' (i : uint) = chr(uint32toint32(uplus(i,0w48)))
		fun loop (i : uint) = 
		    if ulte(i,0w9) 
			then [ord' i]
		    else let val d = udiv(i,0w10)
			     val digit = uminus(i,umult(d,0w10))
			 in (ord' digit)::(loop d)
			 end
		val chars = if (i>=0)
				then List.rev(loop (int32touint32 i))
			    else #"~"::(rev(loop (int32touint32 (~i))))
	    in  implode chars
	    end
    end


structure Bool = 
    struct
	fun toString (b:bool) : string = if b then "true" else "false"
    end


structure Real = 
    struct
	fun toString (r:real):string =
	    let val five = 5.0
		val ten  = 10.0
		val one = 1.0
		val zero = 0.0
		val itoa = Integer.toString
		fun chr'(x:int) = chr(x+48)
		fun scistr(a::b::tl,e) : string =
		    let val tail = [#"E", chr e]  (* XXX what if e > 9 *)
			fun trail nil : char list = tail
			  | trail (0::tl) =
			    let val rest = trail tl
			    in  (case rest of 
				     nil => nil
				   | _ => #"0" :: rest)
			    end
			  | trail (hd::tl) = (chr' hd) :: (trail tl)
			val chars = (chr' a) :: #"." :: (chr' b) :: (trail tl)
		    in  implode chars
		    end
		  | scistr _ = "" (* prevents non-exhaustive match *)
		fun normstr(digits : int list,e) : string =
		    let fun n(nil,_) : char list = nil
			  | n(hd::nil,0) = chr' hd :: [#".", #"0"]
			  | n(hd::tl,0) = chr' hd :: #"." :: n(tl,~1)
			  | n(0::tl,d) = let val rest = n(tl,d - 1)
					 in  case (d < ~1,rest) of
					     (true,nil) => rest
					   | _ => #"0" :: rest
					 end
			  | n(hd::tl,d) = chr' hd :: n(tl,d - 1)
			val acc : char list = n(digits,e)
			fun header n : char list = 
			    let fun zeros 1 = acc
				  | zeros n = #"0" :: zeros(n - 1)
			    in   (#"0" :: #"." :: zeros n)
			    end
			val sl = implode(if e < 0
					     then header(~e)
					 else acc)
		    in sl
		    end
		fun mkdigits(f : real, 0) : (int list * int) = (nil,if f < five then 0 else 1)
		  | mkdigits(f,i) =
		    let val digit = floor f
			val new = ten * (f - real digit)
			val (digits,carry) = mkdigits(new,i - 1)
			val (digit,carry) = (case (digit,carry) of
						 (9,1) => (0,1)
					       | _ => (digit + carry,0))
		    in  (digit::digits,carry)
		    end
		(* should eventually speed this up by using log10 *)
		fun mkstr(f,e) : string =
		    if f >= ten then mkstr(f/ten,e +1)
		    else if f < one then mkstr(f*ten,e - 1)
			 else let val (digits,carry) = mkdigits(f,15)
				  val (digits,e) = (case carry of
							0 => (digits,e)
						      | _ => (1::digits,e + 1))
			      in  if e > ~5 andalso e < 15
				      then normstr(digits,e)
				  else scistr(digits,e)
			      end
	    in  if r < zero 
		    then concat("~",mkstr(~r,0))
		else if r > zero 
			 then mkstr(r,0)
		     else "0.0"
	    end (* makestring_real *)
    end


structure IO = 
    struct
	
	(* --------------------- streams --------------- *)
	type instream = int
	type outstream = int
	val std_in : instream =  0
	val std_out : outstream =  1
	val std_err : outstream = 2
	    
	(* open_in, open_out, input, output, lookahead, 
	 close_in, close_out, flush_out, end_of_stream *)
	fun input_line (ins : instream) =
	    let fun loop prev : char list = 
		if end_of_stream ins 
		    then (rev prev)
		else let val c = input1 ins 
		     in
			 case c of
			     #"\n" => rev(c :: prev)
			   | _ => loop (c::prev)
		     end
	    in  implode (loop [])
	    end
	fun print (x:char vector):unit = output(std_out, x)
    end
open IO

