(*$Tillude: *)
structure Tillude = 
  struct
    exception Tillude of (string * int * int);

    type '1a array2 = int * int * '1a Array.Array
    type '1a array1 = int * int * '1a Array.Array

    fun array2 (w,h,v) = (w,h,Array.create (w * h) v)
    fun sub2 ((w,h,raw),x,y) = 
      let 
	val _ = if (x<0 orelse x>=w) then raise Tillude("sub2",x,w) else ()
	val _ = if (y<0 orelse y>=h) then raise Tillude("sub2",y,h) else ()
	val pos = x + y * w
      in
	Array.sub(raw,pos)
      end

    fun update2 ((w,h,raw),x,y,v) = 
      let 
	val _ = if (x<0 orelse x>=w) then raise Tillude("update2",x,w) else ()
	val _ = if (y<0 orelse y>=h) then raise Tillude("update2",y,h) else ()
	val pos = x + y * w
      in
	Array.update(raw,pos,v)
      end
    
    fun array1 (s,v) = array2(s,1,v)
    fun sub1   (A,x) =  sub2(A,x,0)
    fun update1 (A,s,v) = update2(A,s,0,v)
    fun length1 ((w,1,raw)) = w
      | length1 _ = raise Tillude("length1",0,0)
	
    fun length2 ((w,h,raw)) = (w,h)
      
    val makestring_int = makestring : int -> string
    val makestring_real = makestring : real -> string
    fun makestring_bool true = "true"
      | makestring_bool false = "false"

    val op || = Bits.orb
    val op && = Bits.andb
    val op << = Bits.lshift
    val op >> = Bits.rshift
  end
infix 9 || && << >>;