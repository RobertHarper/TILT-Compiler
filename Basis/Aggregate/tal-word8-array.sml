(* array.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

structure Word8Array :> MONO_ARRAY where type elem = char
				     and type array = TiltPrim.word8array
				     and type Vector.vector = string
				     and type Vector.elem = char =
  struct

    val int32touint32 = TiltPrim.int32touint32
    val uint32toint32 = TiltPrim.uint32toint32

    val array_length = TiltPrim.array_length8
    val empty_array = TiltPrim.empty_array8
    val empty_vector = TiltPrim.empty_vector8
    val unsafe_array = TiltPrim.unsafe_array8
    val unsafe_sub = TiltPrim.unsafe_sub8
    val unsafe_update = TiltPrim.unsafe_update8
    val unsafe_vsub = TiltPrim.unsafe_vsub8
    val vector_length = TiltPrim.vector_length8

    val unsafe_array2vector = TiltPrim.unsafe_array2vector8

    val uminus = TiltPrim.uminus
    val uplus = TiltPrim.uplus

    val ugt = TiltPrim.ugt
    val ugte = TiltPrim.ugte
    val ult = TiltPrim.ult
    val ulte = TiltPrim.ulte

    type elem = char
    type array = TiltPrim.word8array
    structure Vector = Word8Vector

    val maxLen = PreVector8.maxLen

    val array0 : array = empty_array
    val vector0 : Vector.vector = empty_vector

    fun array (0, _) = array0
      | array (n, init) =
	if (maxLen < n)
	    then raise Size
	else unsafe_array(int32touint32 n, init)

    fun checkLen n = if maxLen < n then raise Size else ()
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
      | tabulate (n, f) : array =
          let val _ = if (n < 0) then raise Size else ()
	      val a = array(n, f 0)
	      val n = int32touint32 n
              fun tab i =
                if ult(i,n) then (unsafe_update(a, i, f (uint32toint32 i));
				  tab(uplus(i,0w1)))
                else a
           in tab 0w1
          end

    fun length (ar : array) : int = uint32toint32(array_length ar)
    fun sub (a : array, index :int) =
	let val index = int32touint32 index
	in unsafe_sub(a,index)
	end
    fun update (a, index :int, e : char) : unit =
	let val index = int32touint32 index
	in unsafe_update(a,index,e)
	end

    fun extract (v, base : int, optLen : int option) = let
	  val len = length v
	  fun newVec (n : int) : Vector.vector = let
		fun tab (~1, l) = unsafe_array2vector(fromList'(n,l))
		  | tab (i, l) = tab(i-1, (unsafe_sub(v, int32touint32(base+i)))::l)
		in  tab (n-1, [])
		end
	  in
	    case (base, optLen)
	     of (0, NONE) => if (0 < len) then newVec len else vector0
	      | (_, SOME 0) => if ((base < 0) orelse (len < base))
		  then raise Subscript
		  else vector0
	      | (_, NONE) => if ((base < 0) orelse (len < base))
		    then raise Subscript
		  else if (len = base)
		    then vector0
		    else newVec (len - base)
	      | (_, SOME n) =>
		  if ((base < 0) orelse (n < 0) orelse (len < (base+n)))
		    then raise Subscript
		    else newVec n
	    (* end case *)
	  end

    fun copy {src, si=si', len, dst, di} =
        let

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

        (* check to continue *after* update; otherwise we might underflow j  - Tom 7 *)
            fun copyDown (j, k) =
                let in
                    unsafe_update(dst, k, unsafe_sub(src, j));
                    if ult(si,j) then copyDown (uminus(j, 0w1),
                                                uminus(k, 0w1))
                    else ()
                end

        in  if ((di < 0) orelse (length dst < dstop'))
                then raise Subscript
            else if (si' < di) then
                if (0w0 = sstop) then ()
                else copyDown (uminus(sstop,0w1), uminus(dstop,0w1))
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
	    val n = length arr
	in  if n = 0 then init
	    else fold (int32touint32(n - 1), init)
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
		     in fold (i', f (uint32toint32 i', unsafe_sub(vec, i'), accum))
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


