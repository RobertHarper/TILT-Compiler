(* uref.sml
 *
 * UNIONFIND DATA STRUCTURE WITH PATH COMPRESSION AND RANKED UNION
 *
 * Author:
 *    Fritz Henglein
 *    DIKU, University of Copenhagen
 *    henglein@diku.dk
 *)

structure URef :> UREF =
  struct

    datatype 'a urefC
      = ECR of 'a * int
      | PTR of 'a uref
    withtype 'a uref = 'a urefC ref

    fun find (p as ref (ECR _)) = p
      | find (p as ref (PTR p')) = let
	  val p'' = find p'
          in
	    p := PTR p''; p''
          end

    fun uRef x = ref (ECR(x, 0))

    fun !! p = let val ECR(x, _) = !(find p) in x end

    fun equal (p, p') = (find p = find p')

    fun update (p, x) = let val (p' as ref(ECR(_, r))) = find p
	  in
	    p' := ECR(x, r)
	  end

    fun link (p, q) = let
	  val p' = find p
          val q' = find q
	  in
	    if (p' = q') then () else p' := PTR q
	  end

    fun unify f (p, q) = let
	  val (p' as ref(ECR(pc, pr))) = find p
          val (q' as ref(ECR(qc, qr))) = find q
	  val newC = f (pc, qc)
          in
	    if p' = q'
	      then p' := ECR(newC, pr)
	    else if pr = qr
	      then (
		q' := ECR(newC, qr+1);
		p' := PTR q')
	    else if pr < qr
	      then (
		q' := ECR(newC, qr);
		p' := PTR q')
	      else ((* pr > qr *)
                p' := ECR(newC, pr);
                q':= PTR p')
          end

    fun union (p, q) = let
	  val p' = find p
          val q' = find q
          in
	    if p' = q'
              then ()
              else let
		val ECR(pc, pr) = !p' and ECR(qc, qr) = !q'
		in
		  if pr = qr
		    then (
		      q' := ECR(qc, qr+1);
		      p' := PTR q')
		  else if pr < qr
		    then p' := PTR q'
		    else q':= PTR p'
		end
          end

  end
