(* Thread safety guaranteed by lack of mutable types *)

(* Merge sort *)

structure Msort :> RUN = 
  struct

fun revappend([],l2) = l2
  | revappend(hd::tl,l2) = revappend(tl,hd::l2)

fun sort (gt:int*int->bool) l = 
    let fun split l = foldl (fn (x,(a,b)) => (b,x::a)) ([],[]) l
	fun merge (l1,l2) = 
	    let fun m(l1,[],prev) = revappend(prev,l1)
		  | m([],l2,prev) = revappend(prev,l2)
		  | m(l1 as (x1::y1),l2 as (x2::y2),prev) = 
		if gt(x1,x2) then
		    m(l1,y2,x2::prev)
		else 
		    m(y1,l2,x1::prev)
	    in
			m(sort gt l1,sort gt l2,[])
	    end 
    in
	case l of
	    [] => l
	  | [_] => l
	  | _ => merge(split l) 
    end


fun gen (prev,i) = if (i = 0) then prev else gen(i::prev,i-1)
fun square(l,i) = if (i=0) then l else square(l @ l,i-1)

(* Goes from [start,stop] *)
fun for(start,stop,f) = 
  let fun loop i = if i > stop then () else (f i; loop (i+1))
  in
    loop start
  end



    fun run () = (print "Msort Started\n";
		      for(1,10,fn _ => sort (op >) (square(gen([],10),9)));
		      print "Msort Done\n")

end
