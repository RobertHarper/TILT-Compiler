structure Listops : LISTOPS = 
  struct

    fun loop a b = if (a>b) then [] else a::(loop (a+1) b)
    fun count n = loop 0 (n-1)
    val error = fn s => Util.error "util.sml" s
			      
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
    fun flatten arg = foldr (op @) [] arg
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
    fun andfold' pred acc [] = true
      | andfold' pred acc (a::rest) = let val (b,acc') = pred(a,acc)
				      in b andalso (andfold' pred acc' rest)
				      end
    fun orfold' pred acc [] = false
      | orfold' pred acc (a::rest) = let val (b,acc') = pred(a,acc)
				     in b orelse (orfold' pred acc' rest)
				     end


  end
