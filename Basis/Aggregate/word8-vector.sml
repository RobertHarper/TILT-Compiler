(*$import Prelude General Array MONO_VECTOR Vector List *)
(* word8-vector.sml specialized copy of vector.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

structure Word8Vector :> MONO_VECTOR where type elem = char
				       and type vector = char vector =
  struct

    val int32touint32 = TiltPrim.int32touint32
    val uint32toint32 = TiltPrim.uint32toint32
	
    val empty_vector = TiltPrim.empty_vector
    val unsafe_array = TiltPrim.unsafe_array
    val unsafe_update = TiltPrim.unsafe_update
    val unsafe_vsub = TiltPrim.unsafe_vsub
    val vector_length = TiltPrim.vector_length
	
    val unsafe_array2vector = TiltPrim.unsafe_array2vector
    val unsafe_vector2array = TiltPrim.unsafe_vector2array
	
    val ugt = TiltPrim.ugt
    val ugte = TiltPrim.ugte
    val ult = TiltPrim.ult
	
    val uminus = TiltPrim.uminus
    val uplus = TiltPrim.uplus
	
    type elem = char
    type vector = char vector

    val maxLen = Vector.maxLen

    fun checkLen n = if maxLen < n then raise General.Size else ()
    val vector0 : vector = empty_vector

    fun fromList'(n,l) = 
	let val _ = checkLen n
	in
	    if (n = 0)
		then vector0
	    else let val ar = unsafe_array(int32touint32 n, List.hd l)
		     fun loop [] _ = ()
		       | loop (a::b) n = (unsafe_update(ar,n,a);
					  loop b (uplus(n,0w1)))
		     val _ = loop l 0w0
		 in  unsafe_array2vector ar
		 end
	end

    fun fromList l = unsafe_array2vector(Array.fromList l)
    fun tabulate(n,f) = unsafe_array2vector(Array.tabulate(n,f))

    fun length (a : vector) : int = uint32toint32(vector_length a)
    fun sub (a : vector, index :int) =
	let val index = int32touint32 index
	in  if (ugte(index, vector_length a))
		then raise Subscript
	    else unsafe_vsub(a,index)
	end


  (* a utility function *)
    fun rev ([], l) = l
      | rev (x::r, l) = rev (r, x::l)

    fun extract (v : vector, base : int, optLen : int option) = 
	Array.extract (unsafe_vector2array v, base, optLen)

    fun concat [v] = v
      | concat vl = let
	(* get the total length and flatten the list *)
	  fun len ([], n, l) = (checkLen n; (n, rev(l, [])))
	    | len (v::r, n, l) = let
		val n' = length v
		fun explode (i, l) = if (i < n')
		      then explode(i+1, unsafe_vsub(v, int32touint32 i)::l)
		      else l
		in
		  len (r, n + n', explode(0, l))
		end
	  in
	    case len (vl, 0, [])
	     of (0, _) => vector0
	      | (n, l) => fromList'(n, l)
	    (* end case *)
	  end

    fun map f vec = let
          val len = vector_length vec
          fun mapf (i, l) = if ult(i,len)
                then mapf (uplus(i,0w1), f (unsafe_vsub(vec, i)) :: l)
                else fromList'(uint32toint32 len, rev(l, []))
          in
            if ugt(len,0w0)
              then mapf (0w0, [])
              else vector0
          end

    fun chkSlice (vec, i, NONE) = let val len = length vec
	  in
	    if (len < i)
	      then raise Subscript
	      else (vec, int32touint32 i, int32touint32 len)
	  end
      | chkSlice (vec, i, SOME n) = let val len = length vec
	  in
	    if ((0 <= i) andalso (0 <= n) andalso (i+n <= len))
	      then (vec, int32touint32 i, int32touint32(i+n))
	      else raise Subscript
	  end
     fun mapi f slice = let
          val (vec, start, stop) = chkSlice slice
          val len = uminus(stop,start)
          fun mapf (i, l) = if ult(i,stop)
                then mapf (uplus(i,0w1), f (uint32toint32 i, unsafe_vsub(vec, i)) :: l)
                else fromList'(uint32toint32 len, rev(l, []))
          in
            if ugt(len,0w0)
              then mapf (start, [])
              else vector0
          end

    fun app f vec = Array.app f (unsafe_vector2array vec)
    fun foldl f init vec = Array.foldl f init (unsafe_vector2array vec)
    fun foldr f init vec = Array.foldr f init (unsafe_vector2array vec)
    fun appi f (vec,start,len) = Array.appi f (unsafe_vector2array vec,start,len)
    fun foldli f acc (vec,start,len) = Array.foldli f acc (unsafe_vector2array vec,start,len)
    fun foldri f acc (vec,start,len) = Array.foldri f acc (unsafe_vector2array vec,start,len)




  end  (* Vector *)


(*
 * $Log$
# Revision 1.5  2001/12/13  16:31:18  swasey
# *** empty log message ***
# 
# Revision 1.4  2000/11/27  22:36:22  swasey
# *** empty log message ***
# 
 * Revision 1.3  2000/09/12 18:54:11  swasey
 * Changes for cutoff compilation
 *
# Revision 1.2  98/05/14  16:38:59  pscheng
# result types on sme functions were wrong
# 
# Revision 1.1  1998/03/09  19:50:22  pscheng
# added basis
#
 * Revision 1.3  1997/05/29  14:44:30  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.2  1997/02/11  15:16:12  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
