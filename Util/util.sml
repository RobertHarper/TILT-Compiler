structure Util : UTIL = 
  struct
    exception UNIMP
    exception BUG of string
    fun real_error filename str = let val s = "Error: " ^  filename ^ ": " ^ str
				  in print s; print "\n"; raise (BUG s)
				  end

    fun loop a b = if (a>b) then [] else a::(loop (a+1) b)
    fun count n = loop 0 (n-1)
    val error = fn s => real_error "util.sml" s
			      

    fun mapopt f NONE = NONE
      | mapopt f (SOME x) = SOME(f x)
    fun eq_opt (f, NONE, NONE) = true
      | eq_opt (f, SOME a, SOME b) = f(a,b)
      | eq_opt _ = false

(*
    fun IntStr2word s = (case (Int.fromString s) of
			SOME i => Word32.fromInt i
		      | NONE => error "IntStr2word failed")
    fun WordStr2word s = (case (String.sub(s,2)) of
			    #"x" => (case Word32.fromString (String.substring(s,3,size s - 3)) of
				       SOME w => w
				     | NONE => error "WordStr2word failed")
			  | _ => IntStr2word (String.substring(s,2,size s - 2)))
    val IntStr2word64 = Word64.fromWord32 o IntStr2word
    val WordStr2word64 = Word64.fromWord32 o WordStr2word
*)
    fun CharStr2char s = String.sub(s,0)


    datatype '1a oneshot = ONESHOT of int * '1a option ref
    val oneshot_count = ref 0
    fun inc r = (r := (!r) + 1; !r)
    fun oneshot() = ONESHOT(inc oneshot_count, ref NONE)
    fun oneshot_init item = ONESHOT(inc oneshot_count, ref (SOME item))
    fun oneshot_set(ONESHOT(_,ref (SOME _)),_) = error "oneshot_set called on something already set"
      | oneshot_set(ONESHOT(_,r as (ref NONE)),item) = r := (SOME item)
    fun oneshot_deref (ONESHOT (_,r)) = !r
    fun eq_oneshot (ONESHOT a,ONESHOT b) = a = b

    type ('a,'b) sequence = ('a*'b) list
    type ('a,'b) set = ('a*'b) list
    fun list2sequence x = x
    fun sequence2list x = x
    fun sequence2set x = x
    fun set2sequence x = x
    fun list2set x = x
    fun set2list x = x
    val foldsequence = foldl
    val mapsequence = map
    val mapset = map
    val appsequence = app
    val appset = app
    val allsequence = List.all

    fun set_lookup pred set key = 
	let fun loop [] = NONE
	      | loop ((a,b)::rest) = if (pred(a,key)) then SOME b else loop rest
	in loop set
	end
    val sequence_lookup = set_lookup

    val error = real_error
  end
