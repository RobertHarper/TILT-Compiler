(*$import Prelude *)

(* 2-d matrix multiply *)

local
    exception Shape
    open Array
    open Array2

    fun outerdot (i,j,A : int array2, B : int array2) =
      let val (ar,ac) = length2 A
	  val (br,bc) = length2 B
	  val _ = if ac<>br then raise Shape else ()
	  fun innerdot(k,sum) = 
	      if k<ac then
                  let val sum'=sum + sub2(A,i,k) * sub2(B,k,j)
                  in innerdot(k+1,sum')
                  end
	      else sum
      in innerdot(0,0)
      end

   fun check (A : int array2) (v : int) : bool =
       let val (ar,ac) = length2 A
           fun inner (row,~1) = true
	     | inner (row,count) = (sub2(A,row,count) = v) andalso inner (row,count-1)
           fun outer (~1) = true
	     | outer count = inner(count,ac-1) andalso outer (count-1)
       in  outer (ar-1)
       end

   fun pr (A : int array2) =
       let val (ar,ac) = length2 A
           val _ = for (0, ar-1, fn i =>
			(for (0, ac-1, fn j =>
			      (print(Int.toString(sub2(A,i,j)));
			       print " "));
			 print "\n"))
      in   ()
      end


  fun mult (A : int array2, B : int array2) =
      let val (ar,ac) = length2 A
          val (br,bc) = length2 B
	  val _ = if ac<>br then raise Shape else ()
	  val C = array2(ar,bc,0)
	  val _ = forP(0, ar-1, fn i =>
		      for (0, bc-1, fn j =>
			   update2(C,i,j,outerdot(i,j,A,B))))
      in  C
      end

  val dim = 100
  val v = 1
  val a = array2(dim,dim,v)
  val b = mult(a,a)
in
  val matmultResult = if (check b (dim * v))
			  then print "matrix product correct\n"
		      else print "matrix product incorrect\n"

end


