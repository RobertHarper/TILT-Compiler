(*$import TopLevel Array Array2 Help *)

(* 2-d matrix multiply *)

local
    exception Shape
    open Array
    open Array2

    val _ = print "just started\n"

    fun outerdot (i,j,A : int array2, B : int array2) =
      let val (ar,ac) = dimensions A
	  val (br,bc) = dimensions B
	  val _ = if ac<>br then raise Shape else ()
	  fun innerdot(k,sum) = 
	      if k<ac then
                  let val sum'=sum + Array2.sub(A,i,k) * Array2.sub(B,k,j)
                  in innerdot(k+1,sum')
                  end
	      else sum
      in innerdot(0,0)
      end

   fun check (A : int array2) (v : int) : bool =
       let val (ar,ac) = dimensions A
           fun inner (row,~1) = true
	     | inner (row,count) = (Array2.sub(A,row,count) = v) andalso inner (row,count-1)
           fun outer (~1) = true
	     | outer count = inner(count,ac-1) andalso outer (count-1)
       in  outer (ar-1)
       end

   fun pr (A : int array2) =
       let val (ar,ac) = dimensions A
           val _ = for (0, ar-1, fn i =>
			(for (0, ac-1, fn j =>
			      (print(Int.toString(Array2.sub(A,i,j)));
			       print " "));
			 print "\n"))
      in   ()
      end


  fun mult (A : int array2, B : int array2) =
      let val (ar,ac) = dimensions A
          val (br,bc) = dimensions B
	  val _ = if ac<>br then raise Shape else ()
	  val C = Array2.array(ar,bc,0)
	  val _ = for(0, ar-1, fn i =>
		      (for (0, bc-1, fn j =>
			    Array2.update(C,i,j,outerdot(i,j,A,B)))))
      in  C
      end

  val dim = 100
  val v = 1
  val _ = print "about to make array\n"
  val a = Array2.array(dim,dim,v)
  val _ = print "done making array\n"
  val b = mult(a,a)
in
  val matmultResult = if (check b (dim * v))
			  then print "matrix product correct\n"
		      else print "matrix product incorrect\n"

end


