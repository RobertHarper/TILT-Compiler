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
    fun unzip3 abc_list = 
	let fun unzip3_loop [] (aa,bb,cc) = (rev aa, rev bb, rev cc)
	      | unzip3_loop ((a,b,c)::rest) (aa,bb,cc) = unzip3_loop rest (a::aa,b::bb,c::cc)
	in unzip3_loop abc_list ([],[],[])
	end

    fun unzip4 abcd_list = 
	let fun unzip4_loop [] (aa,bb,cc,dd) = (rev aa, rev bb, rev cc, rev dd)
	      | unzip4_loop ((a,b,c,d)::rest) (aa,bb,cc,dd) = 
	  unzip4_loop rest (a::aa,b::bb,c::cc,d::dd)
	in unzip4_loop abcd_list ([],[],[],[])
	end

    fun all3 pred = 
      let
	fun check ([],[],[]) = true
	  | check (a::arest,b::brest,c::crest) = 
	  (pred (a,b,c)) andalso check (arest,brest,crest)
	  | check _ = false
      in
	check 
      end
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
      
    fun app2 f = 
      let
	fun loop ([],[]) = ()
	  | loop (a::aa,b::bb) = (ignore (f (a,b));loop (aa,bb))
	  | loop _ = error "app2 passed lists of different lengths"
      in
	loop
      end

    fun app3 f = 
      let
	fun loop ([],[],[]) = ()
	  | loop (a::aa,b::bb,c::cc) = (ignore (f (a,b,c));loop (aa,bb,cc))
	  | loop _ = error "app3 passed lists of different lengths"
      in
	loop
      end

    fun flatten arg = foldr (op @) [] arg
    fun transpose [] = []
      | transpose ([]::_) = []
      | transpose lists = let fun split [] = error "transpose failed"
				| split (a::b) = (a,b)
			      val pairs = map split lists
			  in (map #1 pairs) :: (transpose (map #2 pairs))
			  end
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

    fun map_second f = List.map (fn (x,v) => (x,f v)) 
    fun map_first f = List.map (fn (x,v) => (f x,v)) 

    fun foldl_acc ffun init list = 
      let 
	fun loop (state,[]) = ([],state)
	  | loop (state,fst::rest) =
	  let
	    val (fst',state') = ffun (fst,state)
	    val (rest',state'') = loop (state',rest)
	  in
	    (fst'::rest',state'')
	  end
      in
	loop (init,list)
      end
    
    fun eq_len (l1,l2) = ((List.length l1) = (List.length l2))

    fun eq_len3 ([],[],[]) = true
      | eq_len3 (a::arest,b::brest,c::crest) = eq_len3 (arest,brest,crest)
      | eq_len3 _ = false
  end
