(*$import UREF *)
(* simple-uref.sml
 *
 * UNIONFIND DATA STRUCTURE WITH PATH COMPRESSION
 *
 * Author:
 *    Fritz Henglein
 *    DIKU, University of Copenhagen
 *    henglein@diku.dk
 *)

structure SimpleURef : UREF =
  struct

    exception UnionFind of string

    datatype 'a urefC
      = ECR of 'a
      | PTR of 'a uref
    withtype 'a uref = 'a urefC ref

    fun find (p as ref(ECR _)) = p
      | find (p as ref(PTR p')) = let
	  val p'' = find p'
          in
	    p := PTR p''; p''
          end

    fun uRef x = ref (ECR x)

    fun !! p = let val (ECR x) = !(find p) in x end
      
    fun equal (p, p') = (find p = find p')

    fun update (p, x) = let val p' = find p
	  in
	    p' := ECR x
	  end

    fun link (p, q) = let
	  val p' = find p
          val q' = find q
	  in
	    if p' = q' then () else p' := PTR q'
	  end
 
    val union = link

    fun unify f (p, q) = let
	  val v = f(!!p, !!q)
	  in
	    union (p, q); update (q, v)
	  end

  end (* SimpleURef *)

