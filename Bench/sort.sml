(*$import Prelude *)


(* simple quadratic insertion sort; space consumption is also quadratic;
   the generator i am sure is quite unrandom but that is immaterial; the key
   is to make it generate the data quickly so that we are mostly measuring
   the running time of the sorting *)



local 
    fun insert (a:int) nil = (a::nil)
      | insert a (l::lr) = if (a < l) then (a::(l::lr))
			   else (l::(insert a lr))

    fun sort nil = nil
      | sort (a::ar) = insert a (sort ar);

    fun printlist (a::ar) = (print (Int.toString a);
			     print ",";
			     printlist ar)
      | printlist [] = ()

    fun generator seed 0 acc = acc
      | generator seed count acc = 
	generator ((17 * seed + 345) mod 1234567) (count-1) (seed::acc)

    fun checkit [] = true
      | checkit [a:int] = true
      | checkit (a::b::rest) = (a<=b) andalso (checkit (b::rest))

in 
    val _ = print "Started\n"
    val data = generator 1 5000 [] 
    val sorted = sort data
    val _ = if (checkit(sorted)) 
		then print "\nSorted correctly\n\n" 
	    else print "\nSorted incorrectly\n\n"

end 

