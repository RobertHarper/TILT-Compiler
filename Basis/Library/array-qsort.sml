(*$import Prelude ARRAY_SORT Word32 Array TopLevel *)
(* array-qsort.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Structure for in-place sorting of polymorphic arrays.
 * Uses an engineered version of quicksort due to 
 * Bentley and McIlroy.
 *
 *)

structure ArrayQSort : ARRAY_SORT =
  struct

    structure A = Array

    type 'a array = 'a A.array

    val sub = unsafe_sub
    val update = unsafe_update

    fun isort (array, start : word, n : word, cmp) = let
          fun item i = sub(array,i)
          fun swap (i,j) = let 
                val tmp = sub(array,i)
                in update(array,i,sub(array,j)); update(array,j,tmp) end
          fun vecswap (i,j,0w0) = ()
            | vecswap (i,j,n) = (swap(i,j);vecswap(i+0w1,j+0w1,n-0w1))
          fun insertSort (start : word, n : word) = let
                val limit = start+n
                fun outer i =
                      if i >= limit then ()
                      else let
                        fun inner j =
                              if j = start then outer(i+0w1)
                              else let
                                val j' = j - 0w1
                                in
                                  if cmp(item j',item j) = GREATER
                                    then (swap(j,j'); inner j')
                                    else outer(i+0w1)
                                end
                        in inner i end
                in
                  outer (start+0w1)
                end
          in insertSort (start, n); array end

    fun sortRange (array, start : word, n : word, cmp) = let
          fun item i = sub(array,i)
          fun swap (i,j) = let 
                val tmp = sub(array,i)
                in update(array,i,sub(array,j)); update(array,j,tmp) end
          fun vecswap (i,j,0w0) = ()
            | vecswap (i,j,n) = (swap(i,j);vecswap(i+0w1,j+0w1,n-0w1))
          fun insertSort (start, n) = let
                val limit = start+n
                fun outer i =
                      if i >= limit then ()
                      else let
                        fun inner j =
                              if j = start then outer(i+0w1)
                              else let
                                val j' = j - 0w1
                                in
                                  if cmp(item j',item j) = GREATER
                                    then (swap(j,j'); inner j')
                                    else outer(i+0w1)
                                end
                        in inner i end
                in
                  outer (start+0w1)
                end

          fun med3(a,b,c) = let
		val a' = item a and b' = item b and c' = item c
		in
		  case (cmp(a', b'),cmp(b', c'))
		   of (LESS, LESS) => b
		    | (LESS, _) => (
			case cmp(a', c') of LESS => c | _ => a)
		    | (_, GREATER) => b
                    | _ => (case cmp(a', c') of LESS => a | _ => c)
		  (* end case *)
		end

          fun getPivot (a,n) = 
                if n <= 0w7 
		    then a + n div 0w2
                else let
                  val p1 = a
                  val pm = a + n div 0w2
                  val pn = a + n - 0w1
                  in
                    if n <= 0w40 then med3(p1,pm,pn)
                    else let
                      val d = n div 0w8
                      val p1 = med3(p1,p1+d,p1+0w2*d)
                      val pm = med3(pm-d,pm,pm+d)
                      val pn = med3(pn-0w2*d,pn-d,pn)
                      in
                        med3(p1,pm,pn)
                      end
                  end
          
          fun quickSort (arg as (a, n)) = let
                fun bottom limit = let
                      fun loop (arg as (pa,pb)) =
                            if pb > limit then arg
                            else case cmp(item pb,item a) of
                              GREATER => arg
                            | LESS => loop (pa,pb+0w1)
                            | _ => (swap arg; loop (pa+0w1,pb+0w1))
                      in loop end
      
                fun top limit = let
                      fun loop (arg as (pc,pd)) =
                            if limit > pc then arg
                            else case cmp(item pc,item a) of
                              LESS => arg
                            | GREATER => loop (pc-0w1,pd)
                            | _ => (swap arg; loop (pc-0w1,pd-0w1))
                      in loop end

                fun split (pa,pb,pc,pd) = let
                      val (pa,pb) = bottom pc (pa,pb)
                      val (pc,pd) = top pb (pc,pd)
                      in
                        if pb > pc then (pa,pb,pc,pd)
                        else (swap(pb,pc); split(pa,pb+0w1,pc-0w1,pd))
                      end

                val pm = getPivot arg
                val _ = swap(a,pm)
                val pa = a + 0w1
                val pc = a + (n-0w1)
                val (pa,pb,pc,pd) = split(pa,pa,pc,pc)
                val pn = a + n
                val r = Word.min(pa - a, pb - pa)
                val _ = vecswap(a, pb-r, r)
                val r = Word.min(pd - pc, pn - pd - 0w1)
                val _ = vecswap(pb, pn-r, r)
                val n' = pb - pa
                val _ = if n' > 0w1 then sort(a,n') else ()
                val n' = pd - pc
                val _ = if n' > 0w1 then sort(pn-n',n') else ()
                in () end

          and sort (arg as (_, n)) = if n < 0w7 then insertSort arg 
                                     else quickSort arg
          in sort (start,n) end

    fun sort cmp array = sortRange(array, 0w0, int32touint32(A.length array), cmp)

    fun sorted cmp array = let
          val len = int32touint32(A.length array)
          fun s (v,i : word) = let
                val v' = sub(array,i)
                in
                  case cmp(v,v') of
                    GREATER => false
                  | _ => if i+0w1 = len then true else s(v',i+0w1)
                end
          in
            if len = 0w0 orelse len = 0w1 then true
            else s(sub(array,0w0),0w1)
          end

  end (* ArraySort *)

