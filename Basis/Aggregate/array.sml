(*$import ARRAY General List *)
(* array.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

structure Array : ARRAY =
  struct
    type 'a array = 'a array
    type 'a vector = 'a vector

    val maxLen = arraymaxlength

    val array0 : 'a array = empty_array
    val vector0 : 'a vector = empty_vector

    fun array (0, _) = array0
      | array (n, init) = 
	if (maxLen < n) 
	    then raise General.Size 
	else unsafe_array(int32touint32 n, init)

    fun checkLen n = if maxLen < n then raise General.Size else ()
    fun rev ([], l) = l
      | rev (x::r, l) = rev (r, x::l)
    fun fromList'(n,l) = 
	let val _ = checkLen n
	in
	    if (n = 0)
		then array0
	    else let val ar = unsafe_array(int32touint32 n, List.hd l)
		     fun loop [] _ = ()
		       | loop (a::b) n = (unsafe_update(ar,n,a);
					  loop b (uplus(n,0w1)))
		     val _ = loop l 0w0
		 in  ar
		 end
	end
    fun fromList l = 
	let
	    fun len ([], n) = n
	      | len ([_], n) = n+1
	      | len (_::_::r, n) = len(r, n+2)
	    val n = len (l, 0)
	in  fromList'(n,l)
	end

    fun tabulate (0, _) = array0
      | tabulate (n, f) : 'a array = 
          let val a = array(n, f 0)
	      val n = int32touint32 n
              fun tab i = 
                if ult(i,n) then (unsafe_update(a, i, f (uint32toint32 i)); 
				  tab(uplus(i,0w1)))
                else a
           in tab 0w1
          end

    fun length (ar : 'a array) : int = uint32toint32(array_length ar)
    fun sub (a : 'a array, index :int) : 'a =
	let val index = int32touint32 index
	in  if (ugte(index, array_length a))
		then raise Subscript
	    else unsafe_sub(a,index)
	end
    fun update (a, index :int, e : 'a) : unit =
	let val index = int32touint32 index
	in  if (ugte(index, array_length a))
		then raise Subscript
	    else unsafe_update(a,index,e)
	end

    fun extract (v, base : int, optLen : int option) = let
	  val len = length v
	  fun newVec (n : int) = let
		fun tab (~1, l) = unsafe_array2vector(fromList'(n,l))
		  | tab (i, l) = tab(i-1, (unsafe_sub(v, int32touint32(base+i)))::l)
		in  tab (n-1, [])
		end
	  in
	    case (base, optLen)
	     of (0, NONE) => if (0 < len) then newVec len else vector0
	      | (_, SOME 0) => if ((base < 0) orelse (len < base))
		  then raise General.Subscript
		  else vector0
	      | (_, NONE) => if ((base < 0) orelse (len < base))
		    then raise General.Subscript
		  else if (len = base)
		    then vector0
		    else newVec (len - base)
	      | (_, SOME n) =>
		  if ((base < 0) orelse (n < 0) orelse (len < (base+n)))
		    then raise General.Subscript
		    else newVec n
	    (* end case *)
	  end

    fun copy {src, si=si', len, dst, di} = let
	  val (sstop', dstop') = 
	      let val srcLen = length src
	      in  case len
		  of NONE => if ((si' < 0) orelse (srcLen < si'))
				 then raise Subscript
			     else (srcLen, di+srcLen-si')
		| (SOME n) => if ((n < 0) orelse (si' < 0) orelse (srcLen < si'+n))
				  then raise Subscript
			      else (si'+n, di+n)
	      (* end case *)
	      end
	  val sstop = int32touint32 sstop'
	  val dstop = int32touint32 dstop'
	  val si = int32touint32 si'
	  fun copyUp (j, k) = if ult(j,sstop)
		then (unsafe_update(dst, k, unsafe_sub(src, j));
		      copyUp (uplus(j,0w1),uplus(k,0w1)))
		else ()
	  fun copyDown (j, k) = if ulte(si,j)
				    then (unsafe_update(dst, k, unsafe_sub(src, j));
					  copyDown (uminus(j,0w1), uminus(k,0w1)))
				else ()
	  in  if ((di < 0) orelse (length dst < dstop'))
		  then raise Subscript
	      else if (si' < di)
		       then copyDown (uminus(sstop,0w1), uminus(dstop,0w1))
		   else copyUp (si, int32touint32 di)
	  end

    fun copyVec {src, si, len, dst, di} = let
	  val (sstop', dstop') = let
		val srcLen = uint32toint32(vector_length src)
		in
		  case len
		   of NONE => if ((si < 0) orelse (srcLen < si))
		        then raise Subscript
		        else (srcLen, di+srcLen-si)
		    | (SOME n) => if ((n < 0) orelse (si < 0) orelse (srcLen < si+n))
		        then raise Subscript
		        else (si+n, di+n)
		  (* end case *)
		end
	  val sstop = int32touint32 sstop'
	  val dstop = int32touint32 dstop'
	  fun copyUp (j, k) = if ult(j,sstop)
		then (unsafe_update(dst, k, unsafe_vsub(src, j));
		      copyUp (uplus(j,0w1),uplus(k,0w1)))
		else ()
	  in
	    if ((di < 0) orelse (length dst < dstop'))
	      then raise Subscript
	      else copyUp (int32touint32 si, int32touint32 di)
	  end

    fun app f arr = let
	  val len = array_length arr
	  fun app i = if ult(i,len)
			  then (f (unsafe_sub(arr, i)); app(uplus(i,0w1)))
		      else ()
	  in
	    app 0w0
	  end

    fun foldl f init arr = let
	  val len = array_length arr
	  fun fold (i, accum) = if ult(i,len)
				    then fold (uplus(i,0w1), f (unsafe_sub(arr, i), accum))
				else accum
	  in
	    fold (0w0, init)
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

    fun foldr f init arr = 
	let
	    fun fold (i, accum) = 
		let val accum' = f (unsafe_sub(arr, i), accum)
		in  if (i = 0w0)
			then accum'
		    else fold (uminus(i,0w1), accum')
		end
	    val len = length arr
	in  if len = 0 then init else fold (int32touint32(len - 1), init)
	end


    fun modify f arr = let
	  val len = array_length arr
	  fun modify' i = if ult(i,len)
			      then (unsafe_update(arr, i, f (unsafe_sub(arr, i)));
				    modify'(uplus(i,0w1)))
		else ()
	  in
	    modify' 0w0
	  end



    fun appi f slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun app i = if ult(i,stop)
		then (f (uint32toint32 i, unsafe_sub(vec, i)); app(uplus(i,0w1)))
		else ()
	  in
	    app start
	  end

    fun mapi f slice = let
	  val (vec, start, stop) = chkSlice slice
	  val len = uminus(stop,start)
	  fun mapf (i, l) = if ult(i,stop)
		then mapf (uplus(i,0w1), f (uint32toint32 i, unsafe_sub(vec, i)) :: l)
		else fromList'(uint32toint32 len, rev(l, []))
	  in
	    if ugt(len,0w0)
	      then mapf (start, [])
	      else array0
	  end

    fun foldli f init slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun fold (i, accum) = if ult(i,stop)
				    then fold (uplus(i,0w1), f (uint32toint32 i, 
								unsafe_sub(vec, i), accum))
				else accum
	  in  fold (start, init)
	  end

    fun foldri f init slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun fold (i, accum) = if ugt(i,start)
		then let val i' = uminus(i,0w1)
		     in fold (i', f (uint32toint32 i', unsafe_sub(vec, i), accum))
		     end
		else accum
	  in
	    fold (stop, init)
	  end

    fun modifyi f slice = let
	  val (arr, start, stop) = chkSlice slice
	  fun modify' i = if ult(i,stop)
		then (unsafe_update(arr, i,
				    f (uint32toint32 i, unsafe_sub(arr, i)));
		      modify'(uplus(i,0w1)))
		else ()
	  in
	    modify' start
	  end


  end (* structure Array *)


(*
 * $Log$
# Revision 1.3  98/05/14  16:38:56  pscheng
# result types on sme functions were wrong
# 
# Revision 1.2  1998/03/11  20:08:14  pscheng
# bug in foldr
#
# Revision 1.1  1998/03/09  19:50:09  pscheng
# added basis
#
 * Revision 1.3  1997/05/05  19:59:57  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.2  1997/02/11  15:15:32  george
 * got rid of structure rebinding, since inlining is now preserved
 *
 * Revision 1.1.1.1  1997/01/14  01:38:12  george
 *   Version 109.24
 *
 *)
