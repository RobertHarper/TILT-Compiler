(*$import Prelude *)

(* 2-d matrix multiply *)

local
exception Shape
val dim = 100

  fun outerdot (i,j,A : int array2,B : int array2) =
      let val (ar,ac) = length2 A
	  val (br,bc) = length2 B
      in if ac<>br then raise Shape
         else let fun innerdot(k,sum) = 
                if k<ac then
                  let val sum'=sum + sub2(A,i,k) * sub2(B,k,j)
                  in innerdot(k+1,sum')
                  end
                else sum
              in innerdot(0,0)
              end
      end

   fun check (A : int array2) (v : int) : bool =
       let val (ar,ac) = length2 A
           fun inner (row,~1) = true
	     | inner (row,count) = (sub2(A,row,count) = v) andalso inner (row,count-1)
           fun outer (~1) = true
	     | outer count = inner(count,ac-1) andalso outer (count-1)
       in   outer (ar-1)
       end

   fun pr (A : int array2) =
       let val (ar,ac) = length2 A
           fun f(i) =
	        if i<ar then
		    let fun g(j) =
			if j<ac then (print(Int.toString(sub2(A,i,j)));
				      print " "; g(j+1))
			else (print "\n"; f(i+1))
		    in g 0
		    end
               else ()
      in f 0
      end


in
  fun mult (A : int array2, B : int array2) =
      let val (ar,ac) = length2 A
          val (br,bc) = length2 B
      in if ac<>br then raise Shape
         else
	    let val c = array2(ar,bc,0)
                fun f(i) =
                  if i<ar then
                    let fun g(j) =
	                if j<bc then (update2(c,i,j,outerdot(i,j,A,B));
				      g(j+1))
                        else ()
                    in (g 0; f(i+1))
                    end
                  else ()
            in f 0; c
            end
      end

  val v = 1
  val a = array2(dim,dim,v)
  val b = mult(a,a)
  val _ = if (check b (dim * v))
	      then print "matrix product correct\n"
	  else print "matrix product incorrect\n"

end


