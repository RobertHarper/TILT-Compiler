(* pre-string.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Some common operations that are used by both the String and
 * Substring structures.
 *
 *)

structure PreString =
  struct

    val int32touint32 = TiltPrim.int32touint32
    val int32touint8 = TiltPrim.int32touint8
    val uint32toint32 = TiltPrim.uint32toint32
    val uint8touint32 = TiltPrim.uint8touint32

    val ugte = TiltPrim.ugte
    val ult = TiltPrim.ult

    val uminus = TiltPrim.uminus
    val uplus = TiltPrim.uplus

    val unsafe_vsub = TiltPrim.unsafe_vsub8
    val vector_length = TiltPrim.vector_length8
    val unsafe_sub = TiltPrim.unsafe_sub8
    val unsafe_update = TiltPrim.unsafe_update8
    val unsafe_array = TiltPrim.unsafe_array8
    val unsafe_array2vector = TiltPrim.unsafe_array2vector8

    val unsafe_vsub32 = TiltPrim.unsafe_vsub
    val unsafe_update32 = TiltPrim.unsafe_update
    val unsafe_array32 = TiltPrim.unsafe_array
    val unsafe_array2vector32 = TiltPrim.unsafe_array2vector


    local
	val unsafeSub = unsafe_sub
	val unsafeUpdate = unsafe_update
	val unsafeUpdate32 = unsafe_update32
	fun unsafeCreate (sz : int) : TiltPrim.word8array = unsafe_array(int32touint32 sz,#"\000")
	val maxOrd = 255
    in

    datatype substring = SS of (string * int * int)

  (* allocate an uninitialized string of given length (with a size check) *)
    fun create n = if n > 0 then unsafeCreate n else raise Size

  (* a vector of single character strings *)
    val chars : string vector =
	let
	    val a = unsafe_array32 (int32touint32 (maxOrd+1),"")
	    fun next i = if (i <= maxOrd)
			     then let val s = unsafeCreate 1
				      val _ = unsafeUpdate(s, 0w0, int32touint8 i)
				      val _ = unsafeUpdate32(a, int32touint32 i, unsafe_array2vector s)
				  in  next(i+1)
				  end
			 else unsafe_array2vector32 a
	in  next 0
	end

    fun unsafeSubstring (_, _, 0w0) = ""
      | unsafeSubstring (s : string, i, 0w1) = unsafe_vsub32 (chars, uint8touint32 (unsafe_vsub (s, i)))
      | unsafeSubstring (s, i, n) =
	let
	    val ss = unsafeCreate (uint32toint32 n)
	    fun copy j = if (j = n)
			     then unsafe_array2vector ss
			 else (unsafeUpdate(ss, j, unsafe_vsub(s, uplus(i,j))); copy(uplus(j,0w1)))
	in  copy 0w0
	end

    fun size (x : string) : int = TiltPrim.uint32toint32(TiltPrim.vector_length8 x)

  (* concatenate a pair of non-empty strings *)
    fun concat2 (x, y) =
	let
	    val xl = size x
	    val yl = size y
	    val ss = create(xl+yl)
	    val xl = int32touint32 xl
	    val yl = int32touint32 yl
	    fun copyx n = if (n = xl)
			      then ()
			  else (unsafeUpdate(ss, n, unsafe_vsub(x, n)); copyx(uplus(n,0w1)))
	    fun copyy n = if (n = yl)
			      then ()
			  else (unsafeUpdate(ss, uplus(xl,n), unsafe_vsub(y,n)); copyy(uplus(n,0w1)))
	in
	    copyx 0w0; copyy 0w0;
	    unsafe_array2vector ss
	end

  (* given a reverse order list of strings and a total length, return
   * the concatenation of the list.
   *)
    fun revConcat (0, _) = ""
      | revConcat (1, lst : string list) = let
	  fun find ("" :: r) = find r
	    | find (s :: _) = s
	    | find _ = "" (** impossible **)
	  in
	    find lst
	  end
      | revConcat (totLen : int, lst : string list) =
	  let val ss = create totLen
	      fun copy ([], _) = ()
		| copy (s::r, i) =
		  let
		      val len = vector_length s
		      val i = uminus(i,len)
		      fun copy' j = if (j = len)
					then ()
				    else (
					  unsafeUpdate(ss, uplus(i,j), unsafe_vsub(s, j));
					  copy'(uplus(j,0w1)))
		  in
		      copy' 0w0;
		      copy (r, i)
		  end
	  in  copy (lst, int32touint32 totLen);
	      unsafe_array2vector ss
	  end

  (* map a translation function across the characters of a substring *)
    fun translate (tr, s, i, n) = let
	  val stop = uplus(i,n)
	  fun mkList (j, totLen, lst) = if ult(j,stop)
		then let val s' = tr (unsafe_vsub (s, j))
		  in
		    mkList (uplus(j,0w1), totLen + size s', s' :: lst)
		  end
		else revConcat (totLen, lst)
	  in
	    mkList (i, 0, [])
	  end

  (* implode a non-empty list of characters into a string where both the
   * length and list are given as arguments.
   *)
    fun implode (len, cl) =
	let
	    val ss = create len
	    fun copy ([], _) = unsafe_array2vector ss
	      | copy (c::r, i) = (unsafe_update(ss, i, c); copy(r, uplus(i,0w1)))
	in  copy (cl, 0w0)
	end

  (* implode a reversed non-empty list of characters into a string where both the
   * length and list are given as arguments.
   *)
    fun revImplode (len, cl) = let
	  val ss = create len
	  fun copy ([], _) = unsafe_array2vector ss
	    | copy (c::r, i) = (unsafe_update(ss, i, c); copy(r, uminus(i,0w1)))
	  in  copy (cl, int32touint32(len-1))
	  end

    fun isPrefix (s1, s2, i2, n2) = let
	  val n1 = vector_length s1
	  fun eq (i, j) =
		(ugte(i,n1))
		orelse ((unsafe_vsub(s1, i) = unsafe_vsub(s2, j)) andalso eq(uplus(i,0w1), uplus(j,0w1)))
	  in
	      ugte(n2,n1) andalso eq (0w0, i2)
	  end

    fun collate cmpFn (s1 : string, i1, n1, s2 : string, i2, n2) = let
	  val (n, order) =
		if (n1 = n2) then (n1, EQUAL)
		else if (n1 < n2) then (n1, LESS)
		else (n2, GREATER)
	  val n = int32touint32 n
	  fun cmp' i = if (i = n)
		then order
		else let
		  val c1 = unsafe_vsub(s1, uplus(i1,i))
		  val c2 = unsafe_vsub(s2, uplus(i2,i))
		  in
		    case (cmpFn(c1, c2))
		     of EQUAL => cmp' (uplus(i,0w1))
		      | order => order
		    (* end case *)
		  end
	  in  cmp' 0w0
	  end

    fun cmp (s1, i1, n1, s2, i2, n2) = let
	  fun cmpFn (c1, c2) =
		if (c1 = c2) then EQUAL
		else if ((c1 > c2)) then GREATER
		else LESS
	  in
	    collate cmpFn (s1, i1, n1, s2, i2, n2)
	  end

    end (* local *)

    (* getNChars : (char, 'a) reader -> ('a * int) -> (char list * 'a) option *)
    fun getNChars (getc : 'a -> (char * 'a) option) (cs, n) = let
          fun rev ([], l2) = l2
            | rev (x::l1, l2) = rev(l1, x::l2)
          fun get (cs, 0, l) = SOME(rev(l, []), cs)
            | get (cs, i, l) = (case getc cs
                 of NONE => NONE
                  | (SOME(c, cs')) => get (cs', i-1, c::l)
                (* end case *))
          in
            get (cs, n, [])
          end

  end
