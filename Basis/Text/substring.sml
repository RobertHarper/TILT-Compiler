(* substring.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Substring :> SUBSTRING where type substring = PreString.substring
				   and type String.string = string
				   and type String.Char.char = char
				   and type String.Char.string = string =
  struct

    val int32touint32 = TiltPrim.int32touint32
    val uint32toint32 = TiltPrim.uint32toint32

    val unsafe_vsub8 = TiltPrim.unsafe_vsub8

    val ugte = TiltPrim.ugte
    val ult = TiltPrim.ult

    val uminus = TiltPrim.uminus
    val uplus = TiltPrim.uplus
(*
    val op + = InlineT.DfltInt.+
    val op - = InlineT.DfltInt.-
    val op < = InlineT.DfltInt.<
    val op <= = InlineT.DfltInt.<=
    val op > = InlineT.DfltInt.>
    val op >= = InlineT.DfltInt.>=
    val op = = InlineT.=
    val unsafeSub = InlineT.CharVector.sub
*)

    val i2w = int32touint32
    val w2i = uint32toint32

  (* list reverse *)
    fun rev ([], l) = l
      | rev (x::r, l) = rev (r, x::l)

    structure String = String
    datatype substring = datatype PreString.substring
    fun base (PreString.SS arg) = arg

    fun string (PreString.SS(s,st,sz)) = PreString.unsafeSubstring(s,i2w st,i2w sz)


    fun substring (s, i, n) =
	  if ((0 <= i) andalso (0 <= n) andalso (i+n <= String.size s))
	    then PreString.SS(s, i, n)
	    else raise Subscript

    fun extract (s, i, NONE) = if ((0 <= i) andalso (i <= String.size s))
				   then PreString.SS(s, i, String.size s - i)
			       else raise Subscript
      | extract (s, i, SOME n) = substring (s, i, n)

    fun all s = PreString.SS(s, 0, String.size s)

    fun isEmpty (PreString.SS(_, _, 0)) = true
      | isEmpty _ = false

    fun getc (PreString.SS(s, i, 0)) = NONE
      | getc (PreString.SS(s, i, n)) = SOME(unsafe_vsub8(s, i2w i), PreString.SS(s, i+1, n-1))
    fun first (PreString.SS(s, i, 0)) = NONE
      | first (PreString.SS(s, i, n)) = SOME(unsafe_vsub8(s, i2w i))
    fun triml k (PreString.SS(s, i, n)) =
	  if (k < 0) then raise Subscript
	  else if (k >= n) then PreString.SS(s, i+n, 0)
	  else PreString.SS(s, i+k, n-k)
    fun trimr k (PreString.SS(s, i, n)) =
	  if (k < 0) then raise Subscript
	  else if (k >= n) then PreString.SS(s, i, 0)
	  else PreString.SS(s, i, n-k)

    fun sub (PreString.SS(s, i, n), j) =
	  if (j >= n)
	    then raise Subscript
	    else unsafe_vsub8(s, i2w(i+j))
    fun size (PreString.SS(_, _, n)) = n
    fun slice (PreString.SS(s, i, n), j, SOME m) =
	  if ((0 <= j) andalso (0 <= m) andalso (j+m <= n))
	    then PreString.SS(s, i+j, m)
	    else raise Subscript
      | slice (PreString.SS(s, i, n), j, NONE) =
	  if (0 <= j) andalso (j <= n)
	    then PreString.SS(s, i+j, n-j)
	    else raise Subscript

  (* concatenate a list of substrings together *)
    fun concat ssl = let
	fun length (len, sl, []) = (len, sl)
	  | length (len, sl, (PreString.SS(s, i, n)::rest)) =
	      length(len+n, PreString.unsafeSubstring(s, i2w i, i2w n)::sl, rest)
	in
	  PreString.revConcat (length (0, [], ssl))
	end

  (* explode a substring into a list of characters *)
    fun explode (PreString.SS(s, i, n)) = let
	  fun f(l, j) = if ult(j,i2w i)
		then l
		else f(unsafe_vsub8(s, j) :: l, uminus(j,0w1))
	  in
	    f(nil, i2w((i + n) - 1))
	  end

  (* Substring comparisons *)
    fun isPrefix s1 (PreString.SS(s2, i2, n2)) = PreString.isPrefix (s1, s2,i2w  i2, i2w n2)
    fun compare (PreString.SS(s1, i1, n1), PreString.SS(s2, i2, n2)) =
	  PreString.cmp (s1, i2w i1, n1, s2, i2w i2, n2)
    fun collate cmpFn (PreString.SS(s1, i1, n1), PreString.SS(s2, i2, n2)) =
	  PreString.collate cmpFn (s1, i2w i1, n1, s2, i2w i2, n2)

    fun splitAt (PreString.SS(s, i, n), k) =
	  if (n < k)
	    then raise Subscript
	    else (PreString.SS(s, i, k), PreString.SS(s, i+k, n-k))

    local
      fun scanl chop pred (PreString.SS(s, i, n)) = let
	    val stop = i2w(i+n)
	    fun scan j = if ((j <> stop) andalso pred(unsafe_vsub8(s, j)))
		  then scan(uplus(j,0w1))
		  else j
	    in
	      chop (s, i, n, w2i(scan (i2w(i - i))))
	    end
      fun scanr chop pred (PreString.SS(s, i, n)) = let
	    val stop = i2w(i-1)
	    fun scan j = if ((j <> stop) andalso pred(unsafe_vsub8(s, j)))
		  then scan(uminus(j,0w1))
		  else j
	    val k : int = (w2i (scan (i2w (i+n-1)))) - i + 1
	    in
	      chop (s, i, n, k)
	    end
    in
    val splitl = scanl (fn (s, i, n, k) => (PreString.SS(s, i, k), PreString.SS(s, i+k, n-k)))
    val splitr = scanr (fn (s, i, n, k) => (PreString.SS(s, i, k), PreString.SS(s, i+k, n-k)))
    val dropl  = scanl (fn (s, i, n, k) => PreString.SS(s, i+k, n-k))
    val dropr  = scanr (fn (s, i, n, k) => PreString.SS(s, i, k))
    val takel  = scanl (fn (s, i, n, k) => PreString.SS(s, i, k))
    val taker  = scanr (fn (s, i, n, k) => PreString.SS(s, i+k, n-k))
    end (* local *)




  (* find the position of the first occurrence of s in the substring.
   * NOTE: some day we might want to implement KMP matching for this
   *)
    fun position s (PreString.SS (s', i, n)) = let
	  val len = String.size s
	  val len' = i2w len
	  fun eq (j, k) = ugte(j,len') orelse
		((unsafe_vsub8(s, j) = unsafe_vsub8(s', k)) andalso
		 eq (uplus(j,0w1),uplus(k,0w1)))
	  val stop = i+n-len
	  fun cmp k =
		if (k > stop) then i+n (* failure *)
		else if eq(0w0, i2w k) then k
		else cmp(k+1)
	  val indx = cmp i
	  in
	    (PreString.SS(s', i, indx-i), PreString.SS(s', indx, i+n-indx))
	  end

    fun span (PreString.SS(s1, i1, n1), PreString.SS(s2, i2, n2)) =
	  if ((s1 = s2) andalso (i1 < i2+n2))
	    then PreString.SS(s1, i1, (i2+n2)-i1)
	    else raise Span

    fun translate tr (PreString.SS(s, i, n)) =
	  PreString.translate (tr, s, i2w i, i2w n)

    fun tokens isDelim (PreString.SS(s, i, n)) = let
	  val stop = i+n
	  fun substr (i, j, l) =
		if (i = j) then l else PreString.SS(s, i, j-i)::l
	  fun scanTok (i, j, toks) = if (j < stop)
		  then if (isDelim (unsafe_vsub8 (s, i2w j)))
		    then skipSep(j+1, substr(i, j, toks))
		    else scanTok (i, j+1, toks)
		  else substr(i, j, toks)
	  and skipSep (j, toks) = if (j < stop)
		  then if (isDelim (unsafe_vsub8 (s, i2w j)))
		    then skipSep(j+1, toks)
		    else scanTok(j, j+1, toks)
		  else toks
	  in
	    rev (scanTok (i, i, []), [])
	  end
    fun fields isDelim (PreString.SS(s, i, n)) = let
	  val stop = i+n
	  fun substr (i, j, l) = PreString.SS(s, i, j-i)::l
	  fun scanTok (i, j, toks) = if (j < stop)
		  then if (isDelim (unsafe_vsub8 (s, i2w j)))
		    then scanTok (j+1, j+1, substr(i, j, toks))
		    else scanTok (i, j+1, toks)
		  else substr(i, j, toks)
	  in
	    rev (scanTok (i, i, []), [])
	  end

    fun foldl f init (PreString.SS(s, i, n)) = let
	  val stop = i+n
	  fun iter (j, accum) = if (j < stop)
		then iter (j+1, f (unsafe_vsub8(s, i2w j), accum))
		else accum
	  in
	    iter (i, init)
	  end
    fun foldr f init (PreString.SS(s, i, n)) = let
	  fun iter (j, accum) = if (j >= i)
		then iter (j-1, f (unsafe_vsub8(s, i2w j), accum))
		else accum
	  in
	    iter (i+n-1, init)
	  end
    fun app f (PreString.SS(s, i, n)) = let
	  val stop = i+n
	  fun iter j = if (j < stop)
		then (f (unsafe_vsub8(s, i2w j)); iter (j+1))
		else ()
	  in
	    iter i
	  end

  end;
