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

(*
type 'a array1 = 'a Array.array
val array1 = Array.array
val sub1 = Array.sub
val update1 = Array.update
infix 9 &&
val op && = Bits.andb
*)
val andb = fn(x,y) => uint32toint32((int32touint32 x) && (int32touint32 y))
val (op &&) = andb
local

fun qsort lo hi (a : int array1) =
  if lo < hi then 
  let val i = lo
      val j = hi
      val pivot = sub1(a,hi) 
      fun loop(i,j) = 
	  if i < j then
	      let fun fi i = 
		      if i < hi andalso sub1(a,i) <= pivot then fi(i+1) else i
		  val i = fi i
		  fun fj j = 
		      if j > lo andalso sub1(a,j) >= pivot then fj(j-1) else j
		  val j = fj j
	      in
		  if i < j then
		      let val temp = sub1(a,i)
		      in
			  update1(a,i,sub1(a,j)); 
			  update1(a,j,temp)
		      end
		  else ();
		  loop(i,j)
	      end
	  else (i,j)
      val (i,j) = loop(i,j)
      val temp = sub1(a,i)
  in
      update1(a,i,sub1(a,hi));
      update1(a,hi,temp);
      qsort lo (i-1) a;
      qsort (i+1) hi a
  end
  else ()


(* Same but abstract over the comparison to force spilling *)

fun cmp (i:int) j = i - j

fun qsort2 lo hi (a : int array1) =
  if lo < hi then 
  let val i = lo
      val j = hi
      val pivot = sub1(a,hi) 
      fun loop(i,j) = 
	  if i < j then
	      let fun fi i = 
		      if i < hi andalso cmp (sub1(a,i)) pivot <= 0 then 
			  fi(i+1) else i
		  val i = fi i
		  fun fj j = 
		      if j > lo andalso cmp (sub1(a,j)) pivot >= 0 then 
			  fj(j-1) else j
		  val j = fj j
	      in
		  if i < j then
		      let val temp = sub1(a,i)
		      in
			  update1(a,i,sub1(a,j)); 
			  update1(a,j,temp)
		      end
		  else ();
		  loop(i,j)
	      end
	  else (i,j)
      val (i,j) = loop(i,j)
      val temp = sub1(a,i)
  in
      update1(a,i,sub1(a,hi));
      update1(a,hi,temp);
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
  let val a = array1(size, 0) 
      val check = array1(4096,0)
  in
      for(0,size-1,fn i =>
	  let val n = random() 
	  in 
	      update1(a,i,n); 
	      update1(check,n,sub1(check,n)+1)
	  end);
      sort_fun 0 (size-1) a;
      (update1(check,sub1(a,0),sub1(check,sub1(a,0))-1);
       for (1,size-1,fn i =>
         (if sub1(a,i-1) > sub1(a,i) then raise Failed else ();
          update1(check,sub1(a,i),sub1(check,sub1(a,i)) - 1)));
       for(0,4095,fn i => if sub1(check,i) <> 0 then raise Failed else ());
       print "OK"; print "\n") handle Failed =>
      (print "failed"; print "\n")
  end

fun main () =
  (test_sort qsort 50000;
   test_sort qsort2 50000)
in
  val _ = main()
end