(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Good test for loops. Best compiled with unsafe libraries. *)


val andb = fn(x,y) => uint32toint32((int32touint32 x) && (int32touint32 y))
val (op &&) = andb
local

fun qsort lo hi (a : int array) =
  if lo < hi then 
  let val i = lo
      val j = hi
      val pivot = sub(a,hi) 
      fun loop(i,j) = 
	  if i < j then
	      let fun fi i = 
		      if i < hi andalso sub(a,i) <= pivot then fi(i+1) else i
		  val i = fi i
		  fun fj j = 
		      if j > lo andalso sub(a,j) >= pivot then fj(j-1) else j
		  val j = fj j
	      in
		  if i < j then
		      let val temp = sub(a,i)
		      in
			  update(a,i,sub(a,j)); 
			  update(a,j,temp)
		      end
		  else ();
		  loop(i,j)
	      end
	  else (i,j)
      val (i,j) = loop(i,j)
      val temp = sub(a,i)
  in
      update(a,i,sub(a,hi));
      update(a,hi,temp);
      qsort lo (i-1) a;
      qsort (i+1) hi a
  end
  else ()


(* Same but abstract over the comparison to force spilling *)

fun cmp (i:int) j = i - j

fun qsort2 lo hi (a : int array) =
  if lo < hi then 
  let val i = lo
      val j = hi
      val pivot = sub(a,hi) 
      fun loop(i,j) = 
	  if i < j then
	      let fun fi i = 
		      if i < hi andalso cmp (sub(a,i)) pivot <= 0 then 
			  fi(i+1) else i
		  val i = fi i
		  fun fj j = 
		      if j > lo andalso cmp (sub(a,j)) pivot >= 0 then 
			  fj(j-1) else j
		  val j = fj j
	      in
		  if i < j then
		      let val temp = sub(a,i)
		      in
			  update(a,i,sub(a,j)); 
			  update(a,j,temp)
		      end
		  else ();
		  loop(i,j)
	      end
	  else (i,j)
      val (i,j) = loop(i,j)
      val temp = sub(a,i)
  in
      update(a,i,sub(a,hi));
      update(a,hi,temp);
      qsort lo (i-1) a;
      qsort (i+1) hi a
  end
  else ()

(* Test *)

val seed = ref 0

fun random() = (seed := (!seed * 25173 + 17431) && 4095; !seed)

exception Failed

fun for(start,stop,f) = 
    let fun loop i = if i > stop then () else (f i; loop (i+1))
    in
	loop start
    end

fun test_sort sort_fun size =
  let val a = array(size, 0) 
      val check = array(4096,0)
  in
      for(0,size-1,fn i =>
	  let val n = random() 
	  in 
	      update(a,i,n); 
	      update(check,n,sub(check,n)+1)
	  end);
      sort_fun 0 (size-1) a;
      (update(check,sub(a,0),sub(check,sub(a,0))-1);
       for (1,size-1,fn i =>
         (if sub(a,i-1) > sub(a,i) then raise Failed else ();
          update(check,sub(a,i),sub(check,sub(a,i)) - 1)));
       for(0,4095,fn i => if sub(check,i) <> 0 then raise Failed else ());
       print "OK"; print "\n") handle Failed =>
      (print "failed"; print "\n")
  end

fun main () =
  (test_sort qsort 50000;
   test_sort qsort2 50000)
in
  val _ = main()
end