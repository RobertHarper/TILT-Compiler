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


structure Quicksort :> RUN = 
struct


val int32touint32 = TiltPrim.int32touint32
val uint32toint32 = TiltPrim.uint32toint32

val && = TiltPrim.&&

val andb = fn(x,y) => uint32toint32(&&(int32touint32 x,int32touint32 y))
val && = andb

fun inc x = x := (!x) + 1
fun dec x = x := (!x) - 1
open Array

fun qsort lo hi (a : int array) =
  if lo < hi then 
  let val i = ref lo 
      val j = ref hi 
      val pivot = sub(a,hi) 
  in
      while !i < !j do
	  (while !i < hi andalso sub(a,!i) <= pivot do inc i;
	   while !j > lo andalso sub(a,!j) >= pivot do dec j;
	   if !i < !j then 
	       let val temp = sub(a,!i) 
	       in 
		   update(a,!i,sub(a,!j)); 
		   update(a,!j,temp)
	       end
	   else ());
      let val temp = sub(a,!i) 
      in 
	  update(a,!i,sub(a,hi)); 
	  update(a,hi,temp)
      end;
      qsort lo (!i-1) a;
      qsort (!i+1) hi a
  end
 else ()

(* Same but abstract over the comparison to force spilling *)

fun cmp (i:int) j = i - j

fun qsort2 lo hi (a : int array) =
  if lo < hi then 
  let val i = ref lo 
      val j = ref hi 
      val pivot = sub(a,hi) 
  in
      while !i < !j do
	  (while !i < hi andalso cmp (sub(a,!i)) pivot <= 0 do inc i;
	   while !j > lo andalso cmp (sub(a,!j)) pivot >= 0 do dec j;
	   if !i < !j then 
	       let val temp = sub(a,!i) 
	       in 
		   update(a,!i,sub(a,!j)); 
		   update(a,!j,temp)
	       end
	   else ());
      let val temp = sub(a,!i) 
      in 
	  update(a,!i,sub(a,hi)); 
	  update(a,hi,temp)
      end;
      qsort lo (!i-1) a;
      qsort (!i+1) hi a
  end
 else ()
(* Test *)

val seed = ref 0

fun random() = (seed := &&(!seed * 25173 + 17431,4095); !seed)

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
       print "sort OK"; print "\n") handle Failed =>
      (print "failed"; print "\n")
  end

fun main () =
  (test_sort qsort 50000;
   test_sort qsort 50000;
   test_sort qsort 50000;
   test_sort qsort2 50000;
   test_sort qsort2 50000;
   test_sort qsort2 50000)

  fun run () = main()
(*  val quicksortResult = main()*)
end
