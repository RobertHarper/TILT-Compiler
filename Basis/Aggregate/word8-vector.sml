(* word8-vector.sml specialized copy of vector.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

structure Word8Vector :> MONO_VECTOR where type elem = char
				       and type vector = string =
  struct

    val int32touint32 = TiltPrim.int32touint32
    val uint32toint32 = TiltPrim.uint32toint32

    val empty_vector = TiltPrim.empty_vector8
    val unsafe_array = TiltPrim.unsafe_array8
    val unsafe_update = TiltPrim.unsafe_update8
    val unsafe_vsub = TiltPrim.unsafe_vsub8
    val vector_length = TiltPrim.vector_length8

    val unsafe_array2vector = TiltPrim.unsafe_array2vector8
    val unsafe_vector2array = TiltPrim.unsafe_vector2array8

    val ugt = TiltPrim.ugt
    val ugte = TiltPrim.ugte
    val ult = TiltPrim.ult

    val uminus = TiltPrim.uminus
    val uplus = TiltPrim.uplus

    type elem = char
    type vector = word8vector

    val maxLen = PreVector8.maxLen

    fun checkLen n = if maxLen < n then raise General.Size else ()
    val vector0 : vector = empty_vector

    val fromList' = PreVector8.vectorFromList'
    val fromList = PreVector8.vectorFromList

    fun tabulate (0, _) = vector0
      | tabulate (n, f) : word8vector =
      let 
	val _ = if (n < 0) orelse (maxLen < n) then raise Size else ()
	val n = int32touint32 n
	val a = unsafe_array(n, f 0)

	fun tab i =
	  if ult(i,n) then (unsafe_update(a, i, f (uint32toint32 i));
			    tab(uplus(i,0w1)))
	  else a
	val arr = tab 0w1
      in unsafe_array2vector arr
      end

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

    fun extract (v, base : int, optLen : int option) = 
      let
	val len = length v
	fun newVec (n : int) = 
	  let
	    fun tab (~1, l) = fromList'(n,l)
	      | tab (i, l) = tab(i-1, (unsafe_vsub(v, int32touint32(base+i)))::l)
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
    fun app f vec = let
	  val len = vector_length vec
	  fun app i = if ult(i,len)
			  then (f (unsafe_vsub(vec, i)); app(uplus(i,0w1)))
		      else ()
	  in
	    app 0w0
	  end

    fun foldl f init vec = let
	  val len = vector_length vec
	  fun fold (i, accum) = if ult(i,len)
				    then fold (uplus(i,0w1), f (unsafe_vsub(vec, i), accum))
				else accum
	  in
	    fold (0w0, init)
	  end

    fun foldr f init vec =
	let
	    fun fold (i, accum) =
		let val accum' = f (unsafe_vsub(vec, i), accum)
		in  if (i = 0w0)
			then accum'
		    else fold (uminus(i,0w1), accum')
		end
	    val len = length vec
	in  if len = 0 then init else fold (int32touint32(len - 1), init)
	end


    fun appi f slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun app i = if ult(i,stop)
		then (f (uint32toint32 i, unsafe_vsub(vec, i)); app(uplus(i,0w1)))
		else ()
	  in
	    app start
	  end

    fun foldli f init slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun fold (i, accum) = if ult(i,stop)
				    then fold (uplus(i,0w1), f (uint32toint32 i,
								unsafe_vsub(vec, i), accum))
				else accum
	  in  fold (start, init)
	  end

    fun foldri f init slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun fold (i, accum) = if ugt(i,start)
		then let val i' = uminus(i,0w1)
		     in fold (i', f (uint32toint32 i', unsafe_vsub(vec, i), accum))
		     end
		else accum
	  in
	    fold (stop, init)
	  end




  end  (* Vector *)


