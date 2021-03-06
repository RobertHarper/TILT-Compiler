structure Listops :> LISTOPS =
  struct

    fun loop a b = if (a>b) then [] else a::(loop (a+1) b)
    fun count n = loop 0 (n-1)
    fun count1 n = loop 1 n
    fun copy (0,_) = []
      | copy (n,x) = x::(copy(n-1,x))
    val error = fn s => Util.error "util.sml" s

    fun zip [] [] = []
      | zip (a::arest) (b::brest) = (a,b) :: (zip arest brest)
      | zip _ _ = error "zip got lists of different lengths"
    fun zip3 a b c = (map (fn ((a,b),c) => (a,b,c)) (zip (zip a b) c))
    fun zip4 a b c d = (map (fn ((a,b),(c,d)) => (a,b,c,d)) (zip (zip a b) (zip c d)))
    fun zip5 a b c d e = (map (fn ((a,b,c),(d,e)) => (a,b,c,d,e)) (zip (zip3 a b c) (zip d e)))
    fun zip6 a b c d e f = (map (fn ((a,b,c),(d,e,f)) => (a,b,c,d,e,f)) (zip (zip3 a b c) (zip3 d e f)))
    fun zip7 a b c d e f g = (map (fn ((a,b,c),(d,e,f,g)) => (a,b,c,d,e,f,g)) (zip (zip3 a b c) (zip4 d e f g)))


    fun unzip ab_list =
	let fun unzip_loop [] (aa,bb) = (rev aa, rev bb)
	      | unzip_loop ((a,b)::rest) (aa,bb) = unzip_loop rest (a::aa,b::bb)
	in unzip_loop ab_list ([],[])
	end

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

    fun unzip5 abcde_list =
	let fun unzip5_loop [] (aa,bb,cc,dd,ee) =
	          (rev aa, rev bb, rev cc, rev dd, rev ee)
	      | unzip5_loop ((a,b,c,d,e)::rest) (aa,bb,cc,dd,ee) =
	  unzip5_loop rest (a::aa,b::bb,c::cc,d::dd,e::ee)
	in unzip5_loop abcde_list ([],[],[],[],[])
	end

    fun map_unzip f ls = unzip(map f ls)

    fun map_concat (f : 'a -> 'b list) (l : 'a list) : 'b list = 
      let
	fun loop (l,acc) = 
	  (case l 
	     of [] => rev acc
	      | a::aa => loop(aa,List.revAppend (f a,acc)))
      in loop(l,[])
      end

    val all = List.all

    (* nb. NOT the same as ListPair.all, because
       (ListPair.all op= ([], [1])) = true,
       but (all2 op= ([], [1])) = false
      *)
    fun all2 pred =
      let
	fun check ([],[]) = true
	  | check (a::arest,b::brest) =
	  (pred (a,b)) andalso check (arest,brest)
	  | check _ = false
      in
	check
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
    val map = List.map
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
	  | loop (a::aa,b::bb) = (f (a,b);loop (aa,bb))
	  | loop _ = error "app2 passed lists of different lengths"
      in
	loop
      end

    fun app3 f =
      let
	fun loop ([],[],[]) = ()
	  | loop (a::aa,b::bb,c::cc) = (f (a,b,c);loop (aa,bb,cc))
	  | loop _ = error "app3 passed lists of different lengths"
      in
	loop
      end

    fun flatten arg = foldr (op @) [] arg

    fun rev_flatten arg = foldl (op @) [] arg

    fun transpose [] = []
      | transpose ([]::_) = []
      | transpose lists = let fun split [] = error "transpose failed"
				| split (a::b) = (a,b)
			      val pairs = map split lists
			  in (map #1 pairs) :: (transpose (map #2 pairs))
			  end
    fun member (elem,[]) = false
      | member (elem,a::rest) = (elem = a) orelse member(elem,rest)
    fun member_eq (eq,elem:'a,[]:'b list) = false
      | member_eq (eq,elem,a::rest) = eq(elem,a) orelse member_eq(eq,elem,rest)

    fun assoc (aa,[]) = NONE
      | assoc (aa,(a,b)::rest) = if (a = aa) then SOME b else assoc(aa,rest)
    fun assoc_eq (eq:'a*'a->bool, aa,[]) = NONE
      | assoc_eq (eq,aa,((a:'a,b:'b)::rest)) = if eq(a,aa) then SOME b else assoc_eq(eq,aa,rest)
    fun list_sum (a,[]) = a
      | list_sum (a,b::rest) = if (member(b,a)) then list_sum(a,rest) else list_sum(b::a,rest)
    fun list_sum_eq (_,a,[]) = a
      | list_sum_eq (_,[],b) = b
      | list_sum_eq (p,a,b::rest) = if (member_eq(p,b,a)) then list_sum_eq(p,a,rest) else list_sum_eq(p,b::a,rest)
    fun list_diff ([],_) = []
      | list_diff (a::rest,b) = if (member(a,b)) then list_diff(rest,b) else a::(list_diff (rest,b))
    fun list_diff_eq (_,[],_) = []
      | list_diff_eq (p,a::rest,b) = if (member_eq(p,a,b)) then list_diff_eq(p,rest,b)
				     else a::(list_diff_eq (p,rest,b))
    fun list_inter ([],_) = []
      | list_inter (a::rest,b) = if (member(a,b)) then a::(list_inter(rest,b)) else list_inter(rest,b)
    fun list_inter_eq (_,[],_) = []
      | list_inter_eq (p,a::rest,b) = if (member_eq(p,a,b)) then a::(list_inter_eq(p,rest,b)) else list_inter_eq(p,rest,b)


    fun eq_list (f, [] : 'a list, [] : 'b list) = true
      | eq_list (f, a::arest, b::brest) = f(a,b) andalso eq_list(f,arest,brest)
      | eq_list (f, _, _) = false
    fun eq_listlist  (f, a, b) = eq_list(fn (x,y) => eq_list(f,x,y), a,b)

    fun subset_eq eq nil _ = true
      | subset_eq eq (r1::s1) s2 = member_eq (eq, r1, s2) andalso subset_eq eq s1 s2

    fun sameset_eq eq a b = subset_eq eq a b andalso subset_eq eq b a

    fun butlast [] = error "but_last got empty list"
      | butlast [a] = []
      | butlast (a::rest) = a :: (butlast rest)

    val andfold = List.all
    val orfold = List.exists

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

    fun app_second f = List.app (fn (x,v) => f v)
    fun app_first f = List.app (fn (x,v) => f x)

    fun firsts  (l : ('a * 'b) list) = List.map #1 l
    fun seconds (l : ('a * 'b) list) = List.map #2 l

    fun foldl_acc ffun init list =
      let
	fun loop (state,[],acc) = (rev acc,state)
	  | loop (state,fst::rest,acc) =
	  let
	    val (fst',state') = ffun (fst,state)
	  in
	    loop (state',rest,fst'::acc)
	  end
      in
	loop (init,list,[])
      end

    fun foldl_acc2 ffun init (list1,list2) =
      let
	fun loop (state,[],[],acc1,acc2) = (rev acc1,rev acc2,state)
	  | loop (state,fst1::rest1,fst2::rest2,acc1,acc2) =
	  let
	    val (fst1,fst2,state) = ffun (fst1,fst2,state)
	  in
	    loop (state,rest1,rest2,fst1::acc1,fst2::acc2)
	  end
	  | loop _ = error "foldl_acc2 given lists of different length"
      in
	loop (init,list1,list2,[],[])
      end

    fun foldl2 f init (l1,l2) =
      let
	fun loop ([],[],state) = state
	  | loop (a::aa,b::bb,state) = loop(aa,bb,f (a,b,state))
	  | loop _ = error "foldl2 given lists of different length"
      in
	loop (l1,l2,init)
      end

    fun foldl3 f init (l1,l2,l3) =
      let
	fun loop ([],[],[],state) = state
	  | loop (a::aa,b::bb,c::cc,state) = loop(aa,bb,cc,f (a,b,c,state))
	  | loop _ = error "foldl3 given lists of different length"
      in
	loop (l1,l2,l3,init)
      end

    fun foldl4 f init (l1,l2,l3,l4) =
      let
	fun loop ([],[],[],[],state) = state
	  | loop (a::aa,b::bb,c::cc,d::dd,state) = loop(aa,bb,cc,dd,f (a,b,c,d,state))
	  | loop _ = error "foldl4 given lists of different length"
      in
	loop (l1,l2,l3,l4,init)
      end

    fun eq_len (l1,l2) = ((List.length l1) = (List.length l2))

    fun eq_len3 ([],[],[]) = true
      | eq_len3 (a::arest,b::brest,c::crest) = eq_len3 (arest,brest,crest)
      | eq_len3 _ = false


    fun split ls =
      let fun split' _ [] = error "split given empty list"
	    | split' acc [x] = (rev acc,x)
	    | split' acc (a::rest) = split' (a::acc) rest
      in split' [] ls
      end

    fun opt_cons a (SOME aa) = (a::aa)
      | opt_cons a (NONE) = [a]

    fun find2 p listpair =
      let
	fun find_one (a,b,NONE) = if p(a,b) then SOME (a,b) else NONE
	  | find_one (_,_,state) = state
      in
	ListPair.foldl find_one NONE listpair
      end

    fun insertion_sort compare =
      let
	fun insert (a,[]) = [a]
	  | insert (a,bs as b::bb) =
	  case compare (a,b)
	    of GREATER => (b::(insert (a,bb)))
	     | _ => a::bs
	fun sort [] = []
	  | sort (a::aa) = insert (a, sort aa)
      in sort
      end

    fun exist_pair pred =
      let
	fun loop [] = false
	  | loop [_] = false
	  | loop (a::b::rest) = pred(a,b) orelse loop(b::rest)
      in
	loop
      end


    fun index_of p l = 
      let
	fun loop (n,[]) = error "index_of: item not in list"
	  | loop (n,a::aa) = if p a then n else loop (n+1,aa)
      in loop (0,l)
      end

    fun no_dups compare list = 
      let
	val list = insertion_sort compare list
	fun eq (a,b) = case compare (a,b) of EQUAL => true | _ => false
      in
	not (exist_pair eq list)
      end

   datatype 'a catlist =
       LIST of 'a list
     | CONS of 'a * 'a catlist
     | APPEND of 'a catlist list
     | SNOC of 'a catlist * 'a
     | SINGLETON of 'a
     | NIL

   fun flattenCatlist ps =
       let
	   fun flatten' (ps as LIST lst, accum) = lst @ accum
             | flatten' (APPEND (ps::pss), accum) =
	       let
		   val accum' = flatten' (APPEND pss, accum)
	       in
		   flatten' (ps,accum')
	       end
	     | flatten' (APPEND nil, accum) = accum
	     | flatten' (CONS (x, ps), accum) = x :: flatten' (ps, accum)
	     | flatten' (SNOC (ps, x), accum) = flatten' (ps, x :: accum)
             | flatten' (NIL, accum) = accum
             | flatten' (SINGLETON x, accum) = x :: accum

       in
	   flatten' (ps, nil)
       end

   fun join s [] = []
     | join s [a] = [a]
     | join s (a::aa) = a::s::(join s aa)

   fun concatWith s pieces  = String.concat (join s pieces)

   (* toString : ('a -> string) -> 'a list -> string *)
   fun toString f L =
       let
	   fun int i = Int.toString i ^ "."
	   fun string s = int (size s) ^ s
       in
	   foldl (fn (v, acc) => acc ^ string (f v)) (int (length L)) L
       end

   (* fromString' : (string -> 'a option) -> string -> ('a list * substring) option *)
   fun fromString' f s =
       let
	   fun int ss = case Int.scan StringCvt.DEC Substring.getc ss
			  of NONE => NONE
			   | SOME (i, ss') =>
			      if (Substring.isEmpty ss' orelse i < 0 orelse
				  Substring.sub (ss', 0) <> #".")
				  then NONE
			      else SOME (i, Substring.triml 1 ss')

	   fun string ss = case int ss
			     of NONE => NONE
			      | SOME (size, ss') =>
				 SOME (Substring.string (Substring.slice (ss', 0, SOME size)),
				       Substring.triml size ss')

	   fun value ss = case string ss
			    of NONE => NONE
			     | SOME (s, ss') =>
				(case f s
				   of NONE => NONE
				    | SOME v => SOME (v, ss'))

	   fun values (ss, 0, acc) = SOME (rev acc, ss)
	     | values (ss, i, acc) = case value ss
				       of NONE => NONE
					| SOME (v, ss') =>
					   values (ss', i-1, v::acc)

	   fun list ss = case int ss
			   of NONE => NONE
			    | SOME (length, ss') => values (ss', length, nil)
       in  list (Substring.all s)
       end

   (* fromString : (string -> 'a option) -> string -> 'a list option *)
   fun fromString f s = case fromString' f s
			  of NONE => NONE
			   | SOME (result, ss) =>
			      if Substring.isEmpty ss then SOME result
			      else NONE

  end
