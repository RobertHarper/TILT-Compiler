(*$Util : UTIL*)
functor Util() 
  : UTIL = 
  struct
    exception UNIMP
    exception BUG of string
    fun real_error filename str = let val s = "Error: " ^  filename ^ ": " ^ str
				  in print s; print "\n"; raise (BUG s)
				  end

    fun loop a b = if (a>b) then [] else a::(loop (a+1) b)
    fun count n = loop 1 n
    val error = real_error "util.sml"
			      
    fun zip [] [] = []
      | zip (a::arest) (b::brest) = (a,b) :: (zip arest brest)
      | zip _ _ = error "zip got lists of different lengths"
    fun zip3 a b c = (map (fn ((a,b),c) => (a,b,c)) (zip (zip a b) c))
    fun zip4 a b c d = (map (fn ((a,b),(c,d)) => (a,b,c,d)) (zip (zip a b) (zip c d)))
    fun zip5 a b c d e = (map (fn ((a,b,c),(d,e)) => (a,b,c,d,e)) (zip (zip3 a b c) (zip d e)))
    fun zip6 a b c d e f = (map (fn ((a,b,c),(d,e,f)) => (a,b,c,d,e,f)) (zip (zip3 a b c) (zip3 d e f)))
    fun zip7 a b c d e f g = (map (fn ((a,b,c),(d,e,f,g)) => (a,b,c,d,e,f,g)) (zip (zip3 a b c) (zip4 d e f g)))
    fun map2 F (a,b) = map F (zip a b)
    fun map3 F (a,b,c) = map F (zip3 a b c)
    fun map4 F (a,b,c,d) = map F (zip4 a b c d)
    fun map5 F (a,b,c,d,e) = map F (zip5 a b c d e)
    fun map6 F (a,b,c,d,e,f) = map F (zip6 a b c d e f)
    fun map7 F (a,b,c,d,e,f,g) = map F (zip7 a b c d e f g)
    fun mapmap F alistlist = map (fn alist => map F alist) alistlist
    fun mapmapmap F alistlistlist = map (fn alistlist => mapmap F alistlist) alistlistlist
    fun map0count F len = map F (count len)
    fun mapcount F a = map2 F (count(length a),a)
    fun map2count F (a,b) = map3 F (count(length a),a,b)
    fun map3count F (a,b,c) = map4 F (count(length a),a,b,c)
    fun map4count F (a,b,c,d) = map5 F (count(length a),a,b,c,d)
    fun map5count F (a,b,c,d,e) = map6 F (count(length a),a,b,c,d,e)
    fun map6count F (a,b,c,d,e,f) = map7 F (count(length a),a,b,c,d,e,f)
    fun flatten arg = foldl (op @) [] arg
    fun member (elem,[]) = false
      | member (elem,a::rest) = (elem = a) orelse member(elem,rest)
    fun member_eq (eq,elem,[]) = false
      | member_eq (eq,elem,a::rest) = eq(elem,a) orelse member_eq(eq,elem,rest)
    fun assoc (aa,[]) = NONE
      | assoc (aa,(a,b)::rest) = if (a = aa) then SOME b else assoc(aa,rest)
    fun assoc_eq (eq,aa,[]) = NONE
      | assoc_eq (eq,aa,((a,b)::rest)) = if eq(a,aa) then SOME b else assoc_eq(eq,aa,rest)
    fun list_diff ([],_) = []
      | list_diff (a::rest,b) = if (member(a,b)) then list_diff(rest,b) else a::(list_diff (rest,b))
    fun list_diff_eq (_,[],_) = []
      | list_diff_eq (p,a::rest,b) = if (member_eq(p,a,b)) then list_diff_eq(p,rest,b) 
				     else a::(list_diff_eq (p,rest,b))
    fun list_inter ([],_) = []
      | list_inter (a::rest,b) = if (member(a,b)) then a::(list_inter(rest,b)) else list_inter(rest,b) 


    fun eq_list (f, [], []) = true
      | eq_list (f, a::arest, b::brest) = f(a,b) andalso eq_list(f,arest,brest)
      | eq_list (f, _, _) = false
    fun eq_listlist  (f, a, b) = eq_list(fn (x,y) => eq_list(f,x,y), a,b)

    fun butlast [] = error "but_last got empty list"
      | butlast [a] = []
      | butlast (a::rest) = a :: (butlast rest)

    fun andfold pred [] = true
      | andfold pred (a::rest) = (pred a) andalso (andfold pred rest)
    fun orfold pred [] = false
      | orfold pred (a::rest) = (pred a) orelse (orfold pred rest)

    fun IntStr2word s = (case (Int.fromString s) of
			SOME i => Word32.fromInt i
		      | NONE => error "IntStr2word failed")
    fun WordStr2word s = (case (String.sub(s,2)) of
			    #"x" => (case Word32.fromString (String.substring(s,3,size s - 3)) of
				       SOME w => w
				     | NONE => error "WordStr2word failed")
			  | _ => IntStr2word (String.substring(s,2,size s - 2)))
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



    val error = real_error
  end