(* string.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure String :> STRING where type string = string
			     and type Char.char = char
			     and type Char.string = string =
  struct

    val int32touint32 = TiltPrim.int32touint32
    val uint32toint32 = TiltPrim.uint32toint32
    val uint8touint32 = TiltPrim.uint8touint32

    val unsafe_array8 = TiltPrim.unsafe_array8
    val unsafe_update8 = TiltPrim.unsafe_update8
    val unsafe_vsub8 = TiltPrim.unsafe_vsub8
    val vector_length8 = TiltPrim.vector_length8
    val unsafe_array2vector8 = TiltPrim.unsafe_array2vector8

    val unsafe_vsub32 = TiltPrim.unsafe_vsub

    val ugte = TiltPrim.ugte
    val ult = TiltPrim.ult

    val uplus = TiltPrim.uplus

    type string = string
    structure Char = Char

    val maxSize = 1024 * 1024

  (* the length of a string *)
    val i2w = int32touint32
    val w2i = uint32toint32
    fun size(x : string) : int = uint32toint32(vector_length8 x)


  (* allocate an uninitialized string of given length *)
    fun create sz : TiltPrim.word8array = if (sz>0)
				   then unsafe_array8(i2w sz,#"\000")
				 else raise Size


  (* convert a character into a single character string *)
    fun str (c : Char.char) : string =
	  unsafe_vsub32(PreString.chars, uint8touint32 c)

  (* get a character from a string *)
    fun sub(x : string, i : int) =
	let val index = int32touint32 i
	in  if (ugte(index, vector_length8 x))
		then raise Subscript
	    else unsafe_vsub8(x,index)
	end

    fun substring (s, i, n) =
	  if ((i < 0) orelse (n < 0) orelse (size s < i+n))
	    then raise Subscript
	    else PreString.unsafeSubstring (s, i2w i, i2w n)

    fun extract (v : string, base, optLen) = let
	  val len = size v
	  val base' = i2w base
	  fun newVec n = let
		val newV : TiltPrim.word8array = create n
		val n = i2w n
		fun fill (i : word) = if ult(i,n)
		      then let val temp : word = uplus(base',i)
			       val c : char = unsafe_vsub8(v,temp)
			       val i' : word = uplus(i,0w1)
			  in  (unsafe_update8(newV, i, c);
			       fill i')
			   end
		      else ()
		in  fill 0w0; unsafe_array2vector8 newV
		end
	  in
	    case (base, optLen)
	     of (0, NONE) => v
	      | (_, SOME 0) => if ((base < 0) orelse (len < base))
		    then raise Subscript
		    else ""
	      | (_, NONE) => if ((base < 0) orelse (len < base))
		      then raise Subscript
		    else if (base = len)
		      then ""
		      else newVec (len - base)
	      | (_, SOME 1) =>
		  if ((base < 0) orelse (len < base+1))
		    then raise Subscript
		    else str(unsafe_vsub8(v, i2w base))
	      | (_, SOME n) =>
		  if ((base < 0) orelse (n < 0) orelse (len < (base+n)))
		    then raise Subscript
		    else newVec n
	    (* end case *)
	  end

    fun op ^ ("", s) = s
      | op ^ (s, "") = s
      | op ^ (x, y) = PreString.concat2 (x, y)

  (* concatenate a list of strings together *)
    fun concat [s] = s
      | concat (sl : string list) = let
	fun length (i, []) = i
	  | length (i, s::rest) = length(i+size s, rest)
	in
	  case length(0, sl)
	   of 0 => ""
	    | 1 => let
		fun find ("" :: r) = find r
		  | find (s :: _) = s
		  | find _ = "" (** impossible **)
		in
		  find sl
		end
	    | totLen => let
		val ss = create totLen
		fun copy ([], _) = ()
		  | copy (s::r, i) = let
		      val len = i2w(size s)
		      fun copy' j = if (j = len)
			    then ()
			    else (
			      unsafe_update8(ss, uplus(i,j),
					    unsafe_vsub8(s, j));
			      copy'(uplus(j,0w1)))
		      in
			copy' 0w0;
			copy (r, uplus(i,len))
		      end
		in
		  copy (sl, 0w0);  unsafe_array2vector8 ss
		end
	  (* end case *)
	end (* concat *)

  (* implode a list of characters into a string *)
    fun implode [] = ""
      | implode cl =  let
	  fun length ([], n) = n
	    | length (_::r, n) = length (r, n+1)
	  in
	    PreString.implode (length (cl, 0), cl)
	  end

  (* explode a string into a list of characters *)
    fun explode s = let
	  fun f(l, ~1) = l
	    | f(l,  i) = f(unsafe_vsub8(s, i2w i) :: l, i-1)
	  in
	    f(nil, size s - 1)
	  end

    fun map f vec = (case (size vec)
	   of 0 => ""
	    | len => let
		val newVec = create len
		val len = i2w len
		fun mapf i = if ult(i,len)
		      then (unsafe_update8(newVec, i,
					 f(unsafe_vsub8(vec, i)));
			    mapf(uplus(i,0w1)))
		      else ()
		in  mapf 0w0; unsafe_array2vector8 newVec
		end
	  (* end case *))

  (* map a translation function across the characters of a string *)
    fun translate tr s = PreString.translate (tr, s, 0w0, i2w (size s))

  (* tokenize a string using the given predicate to define the delimiter
   * characters.
   *)
    fun tokens isDelim s = let
	  val n = size s
	  fun substr (i, j, l) = if (i = j)
		then l
		else PreString.unsafeSubstring(s, i2w i, i2w(j-i))::l
	  fun scanTok (i, j, toks) = if (j < n)
		  then if (isDelim (unsafe_vsub8 (s, i2w j)))
		    then skipSep(j+1, substr(i, j, toks))
		    else scanTok (i, j+1, toks)
		  else substr(i, j, toks)
	  and skipSep (j, toks) = if (j < n)
		  then if (isDelim (unsafe_vsub8 (s, i2w j)))
		    then skipSep(j+1, toks)
		    else scanTok(j, j+1, toks)
		  else toks
	  in
	    rev (scanTok (0, 0, []))
	  end
    fun fields isDelim s = let
	  val n = size s
	  fun substr (i, j, l) = PreString.unsafeSubstring(s, i2w i, i2w(j-i))::l
	  fun scanTok (i, j, toks) = if (j < n)
		  then if (isDelim (unsafe_vsub8 (s, i2w j)))
		    then scanTok (j+1, j+1, substr(i, j, toks))
		    else scanTok (i, j+1, toks)
		  else substr(i, j, toks)
	  in
	    rev (scanTok (0, 0, []))
	  end

  (* String comparisons *)
    fun isPrefix s1 s2 = PreString.isPrefix (s1, s2, 0w0, i2w(size s2))
    fun compare (a, b) = PreString.cmp (a, 0w0, size a, b, 0w0, size b)
    fun collate cmpFn (a, b) = PreString.collate cmpFn (a, 0w0, size a, b, 0w0, size b)

  (* String greater or equal *)
    fun sgtr (a, b) = let
	  val al = size a and bl = size b
	  val n = if (al < bl) then al else bl
	  fun cmp i = if (i = n)
		then (al > bl)
		else let
		  val ai = unsafe_vsub8(a,i2w i)
		  val bi = unsafe_vsub8(b,i2w i)
		  in
		    Char.>(ai, bi) orelse ((ai = bi) andalso cmp(i+1))
		  end
	  in
	    cmp 0
	  end

    fun op <= (a,b) = if sgtr(a,b) then false else true
    fun op < (a,b) = sgtr(b,a)
    fun op >= (a,b) = b <= a
    val op > = sgtr

    fun fromString s = let
	  val len = i2w(size s)
	  fun getc i = if ult(i,len)
		then SOME(unsafe_vsub8(s, i), uplus(i,0w1))
		else NONE
	  val scanChar = Char.scan getc
	  fun accum (i, chars) = (case (scanChar i)
		 of NONE => if ult(i,len)
		      then NONE (* bad format *)
		      else SOME(implode(rev chars))
		  | (SOME(c, i')) => accum(i', c::chars)
		(* end case *))
	  in
	    accum (0w0, [])
	  end
    val toString = translate Char.toString

    fun fromCString s = raise TiltExn.LibFail "String.fromCString not implemented"
    val toCString = translate Char.toCString

  end (* structure String *)
