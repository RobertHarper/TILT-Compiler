(*$import Prelude TopLevel *)

(* Thread safety automatic since no mutable types are used *)

(*
extern fastTimerOn : (unit) -->
extern fastTimerOff : (unit) -->
*)

local
    fun (f o g) x = f(g x)
    exception ex_undefined of string
    fun error str = raise ex_undefined str
	
    fun accumulate f = let fun foldf a [] = a
			     | foldf a (b::x) = foldf (f a b) x
		       in foldf 
		       end
		   
    fun filter p = let fun consifp x a = if p a then a::x else x
		   in rev o accumulate consifp [] 
		   end
	       
    fun equal (a:int*int) b = a=b

    fun exists p = let fun existsp [] = false
			 | existsp (a::x) = if p a then true else existsp x
		   in existsp 
		   end

    fun member x a = let (* val _ = Ccall(fastTimerOn) *)
			 val res =  exists (equal a) x
			 (* val _ = Ccall(fastTimerOff) *)
		     in  res
		     end
	
    fun C f x y = f y x
	
    fun cons a x = a::x
	
    val revonto = accumulate (C cons)
	
    val length = let fun count n a = n+1 in accumulate count 0 end

    fun repeat f = let fun rptf n x = if n=0 then x else rptf(n-1)(f x)
		       fun check n = if n<0 then error "repeat<0" else n
		   in rptf o check 
		   end

    fun copy n x = repeat (cons x) n []
	
    fun spaces n = implode (copy n #" ")
	
    fun lexless(a1:int,b1:int)(a2,b2) = 
	if a2<a1 then true else if a2=a1 then b2<b1 else false
    fun lexgreater pr1 pr2 = lexless pr2 pr1
    fun lexordset [] = []
      | lexordset (a::x) = lexordset (filter (lexless a) x) @ [a] @
	lexordset (filter (lexgreater a) x)
	
    fun collect f list =
	let fun accumf sofar [] = sofar
	      | accumf sofar (a::x) = accumf (revonto sofar (f a)) x
	in accumf [] list
	end
    fun flatten [] = []
      | flatten (first::rest) = 
	let fun loop [] = flatten rest
	      | loop (a::rest) = a::(loop rest)
	in  loop first
	end

    fun occurs3 x = 
    (* finds coords which occur exactly 3 times in coordlist x *)
    let fun diff x y = filter (not o member y) x
	fun f xover x3 x2 x1 [] = diff x3 xover
	  | f xover x3 x2 x1 (a::x) = 
	    if member xover a then f xover x3 x2 x1 x else
		if member x3 a then f (a::xover) x3 x2 x1 x else
		    if member x2 a then f xover (a::x3) x2 x1 x else
			if member x1 a then f xover x3 (a::x2) x1 x else
			    f xover x3 x2 (a::x1) x
(*	val _ = Ccall(fastTimerOn) 	     *)
	val res =  f [] [] [] [] x 
(*	val _ = Ccall(fastTimerOff) 	    *)
    in  res
    end

    datatype generation = GEN of (int*int) list


    fun alive (GEN livecoords) = livecoords
    fun mkgen coordlist = GEN (lexordset coordlist)
    fun mk_nextgen_fn neighbours gen =
	let 
	    fun twoorthree 2 = true
	      | twoorthree 3 = true
	      | twoorthree _ = false
	    val living = alive gen
	    val isalive = member living
	    val liveneighbours = length o filter isalive o neighbours
	    val survivors = filter (twoorthree o liveneighbours) living
(*	    val _ = Ccall(fastTimerOn)  *)
	    val newnbrlist = flatten(map (filter (not o isalive) o neighbours) living)
(*	    val _ = Ccall(fastTimerOff) *)
	    val newborn = occurs3 newnbrlist
	    val nextGen = mkgen(survivors @ newborn)
	in  nextGen
	end
    
    
    fun neighbours (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),
			    (i,j-1),(i,j+1),
			    (i+1,j-1),(i+1,j),(i+1,j+1)]

    val xstart = 0 
    val ystart = 0
    
    fun markafter n string = string ^ spaces n ^ "0"
    fun plotfrom (x,y) (* current position *)
	str   (* current line being prepared -- a string *)
	((x1,y1)::more)  (* coordinates to be plotted *)
	= if x=x1
	      then (* same line so extend str and continue from y1+1 *)
                  plotfrom(x,y1+1)(markafter(y1-y)str)more
	  else (* flush current line and start a new line *)
	      str :: plotfrom(x+1,ystart)""((x1,y1)::more)
      | plotfrom (x,y) str [] = [str]
    fun good (x,y) = x>=xstart andalso y>=ystart
    fun plot coordlist = plotfrom(xstart,ystart) "" 
	(filter good coordlist)


infix 6 at
fun coordlist at (x:int,y:int) = let fun move(a,b) = (a+x,b+y) 
                                  in map move coordlist end

val rotate = map (fn (x:int,y:int) => (y,~x))

val glider = [(0,0),(0,2),(1,1),(1,2),(2,1)]
val bail = [(0,0),(0,1),(1,0),(1,1)]
fun barberpole n =
   let fun f i = if i=n then (n+n-1,n+n)::(n+n,n+n)::nil
                   else (i+i,i+i+1)::(i+i+2,i+i+1)::f(i+1)
    in (0,0)::(1,0):: f 0
   end

val genB = mkgen(glider at (2,2) @ bail at (2,12)
		 @ rotate (barberpole 4) at (5,20))

val show = app (fn s => (print s; print "\n")) o plot o alive
fun nthgen g 0 = g 
  | nthgen g i = 
    let 
(* 	val _ = Ccall(fastTimerOn)   *)
	val res = mk_nextgen_fn neighbours g
(*	val _ = Ccall(fastTimerOff)  *)
    in  nthgen res (i-1)
    end

(*
fun run g = (show g;
             input_line std_in;
	     run(mk_nextgen_fn neighbours g))

fun read filename = 
  let val f = open_in filename
      fun g(x,y) = case input(f,1)
                    of "." => g(x,y+1)
                     | "O" => (x,y)::g(x,y+1)
                     | "\n" => g(x+1,0)
                     | "" => nil
   in mkgen(g(0,0))
  end
*)

val gun = mkgen
 [(2,20),(3,19),(3,21),(4,18),
  (4,22),(4,23),(4,32),(5,7),
  (5,8),(5,18),(5,22),(5,23),
  (5,29),(5,30),(5,31),(5,32),
  (5,36),(6,7),(6,8),(6,18),
  (6,22),(6,23),(6,28),(6,29),
  (6,30),(6,31),(6,36),(7,19),
  (7,21),(7,28),(7,31),(7,40),
  (7,41),(8,20),(8,28),(8,29),
  (8,30),(8,31),(8,40),(8,41),
  (9,29),(9,30),(9,31),(9,32)
]

  fun doit start step thing = 
      let fun loop 0 thing = ()
	    | loop count thing = let 
				     val _ = show thing 
(*				     val _ = Ccall(fastTimerOn)  *)
				     val nextThing =  nthgen thing step
(*				     val _ = Ccall(fastTimerOff) *)
				 in loop (count-1) nextThing
				 end
      in  loop start thing
      end

in
    fun runLife() = doit 5 30 gun
end

