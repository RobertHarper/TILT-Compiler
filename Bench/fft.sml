(*$import Prelude Int TopLevel Math64 Real64 Array *)

(* Thread safety automatic since no top-level mutable values are present *)

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
(* compatability *)


local


open Math64
open Array
fun abs_real(x:real) = if x < 0.0 then ~x else x
fun print_real (x:real) = print(Real.toString x)



(* $Id$ *)

val pi = 3.14159265358979323846

val tpi = 2.0 * pi

(* Goes from [start,stop] *)
fun for(start,stop,f) = 
    let fun loop i = if i > stop then () else (f i; loop (i+1))
    in
	loop start
    end
(* Goes from [start,stop) with steps of step *)
fun stride(start,stop,step:int,f) =
    let fun loop i = if (i < stop) then (f i; loop (i+step)) else ()
    in	loop start
    end
fun strideP(start,stop,step:int,f) =
    let val count = (stop - start) div step
	val bigStep = step * (1 + (count div 10))
	fun loop cur = if (cur < stop) 
			 then let val next = cur + bigStep
				  val next = if (next > stop) then stop else next
			          val _ = stride(cur, next, step, f)
                                  and _ = loop next
			      in  ()
		              end
		     else ()
    in	loop start
    end

val print_string : string -> unit = print
val print_int : int -> unit = print_string o Int.toString
val print_newline : unit -> unit = fn _ => print_string "\n"

fun dump pxr pxi = 
  for(0,15,fn i => 
      (print_int i; 
       print " ";
       print_real (sub(pxr,i+1)); 
       print " "; 
       print_real (sub(pxi,i+1));
       print_newline()))


fun bitReverse px py n = 
  let val j = ref 1 
  in
    for (1,n-1,fn i =>
    (if i < !j then (
      let val xt1 = sub(px,!j) 
	  val xt2 = sub(px,i)
	  val _ = update(px,!j,xt2)
	  val _ = update(px,i,xt1)
	  val yt1 = sub(py,!j) 
	  val yt2 = sub(py,i)
      in
	  update(py,!j,yt2);
	  update(py,i,yt1)
      end)
     else ();
     let val k = ref(n div 2) 
     in
        while !k < !j do (j := !j - !k; k := !k div 2);
        j := !j + !k
     end))
  end

(* Given x, return (y,z) where y = ceiling(log2 y) and z = 2 ^ y *)
fun binaryLogCeiling np = 
    let val i = ref 2 
	val m = ref 1 
	val _ = 
	    while (!i < np) do
		(i := !i + !i; 
		 m := !m + 1)
	val n = !i 
    in  (!m,n)
    end


(************************************)
(*  Last stage, length=2 butterfly  *)
(************************************)

fun butterfly px py n = 
  let val is = ref 1 
      val id = ref 4 
  in
      while !is < n do
	  let val i0r = ref (!is)
	  in
	      while !i0r <= n do
		  let val i0 = !i0r 
		      val i1 = i0 + 1 
		      val r1 = sub(px,i0) : real
		      val _ = update(px,i0,r1 + sub(px,i1))
		      val _ = update(px,i1,r1 - sub(px,i1))
		      val r1 = sub(py,i0) : real
		  in
		      update(py,i0,r1 + sub(py,i1));
		      update(py,i1,r1 - sub(py,i1));
		      i0r := i0 + !id
		  end;
	      is := 2 * !id - 1; 
	      id := 4 * !id
	  end
  end

fun fft px py np =
let
    (* Let m by log2(np) and n be 2^m. *)
    val (m,n) = binaryLogCeiling np

    (* Pad the array with zeroes if np < n. *)
    val _ = if n <> np 
		then (for (np+1,n,fn i=>
			   (update(px,i,0.0); 
			    update(py,i,0.0)));
		      print_string "Use "; print_int n;
		      print_string " point fft"; print_newline())
	    else ();
in  

  let val n2 = ref(n+n) 
  in
    for(1,m-1,fn k =>
    let val _ = n2 := !n2 div 2
	val n4 = !n2 div 4 
	val e  = tpi / (real (!n2 ))
    in
      for(1,n4,fn j =>
      let val a = e * real(j - 1) 
	  val a3 = 3.0 * a 
	  val cc1 = cos a
	  val ss1 = sin a 
	  val cc3 = cos a3
	  val ss3 = sin a3
	  val is = ref j 
	  val id = ref(2 * !n2) 
      in
        while !is < n do
	    (strideP 
	        (!is, n, !id, 
		 fn i0 =>
		 let 
		     val i1 = i0 + n4 
		     val i2 = i1 + n4 
		     val i3 = i2 + n4 
		     val r1 = sub(px,i0) - sub(px,i2) 
		     val _ = update(px,i0,sub(px,i0) + sub(px,i2))
		     val r2 = sub(px,i1) - sub(px,i3) 
		     val _ = update(px,i1,sub(px,i1) + sub(px,i3))
		     val s1 = sub(py,i0) - sub(py,i2) 
		     val _ = update(py,i0,sub(py,i0) + sub(py,i2))
		     val s2 = sub(py,i1) - sub(py,i3) 
		     val _ = update(py,i1,sub(py,i1) + sub(py,i3))
		     val s3 = r1 - s2 
		     val r1 = r1 + s2 
		     val s2 = r2 - s1 
		     val r2 = r2 + s1 
		  in
		      update(px,i2,r1*cc1 - s2*ss1);
		      update(py,i2,~s2*cc1 - r1*ss1);
		      update(px,i3,s3*cc3 + r2*ss3);
		      update(py,i3,r2*cc3 - s3*ss3)
		  end);
		is := 2 * !id - !n2 + j; 
		id := 4 * !id)
      end)
    end)
  end;


  butterfly px py n;
  bitReverse px py n;
  n

end

fun test np =
  (print_int np; print_string "... "; 
  let val enp = real np 
      val npm = np div 2 - 1
      val pxr = array ((np+2), 0.0)
      val pxi = array ((np+2), 0.0)
      val t = pi / enp
      val _ = update(pxr,1,(enp - 1.0) * 0.5)
      val _ = update(pxi,1,0.0)
      val n2 = np div 2 
      val _ = update(pxr,n2+1,~0.5)
      val _ = update(pxi,n2+1,0.0)
  in
      for (1,npm,fn i =>
      let val j = np - i 
	  val _ = update(pxr,i+1,~0.5)
	  val _ = update(pxr,j+1,~0.5)
	  val z = t * (real i)
	  val y = ~0.5 * ((cos z)/(sin z))
      in
	   update(pxi,i+1,y);
	   update(pxi,j+1,~y)
      end);
(*
  print "\n"; print "before fft: \n";
  dump pxr pxi;
*)
  fft pxr pxi np;
(*
  print "\n"; print "after fft: \n";
  dump pxr pxi;
*)
  let val zr = ref 0.0 
      val zi = ref 0.0 
      val kr = ref 0 
      val ki = ref 0 
  in
      for (0,np-1,fn i =>
      let val a = abs_real(sub(pxr,i+1) - (real i))
      in
	   if !zr < a then 
	       (zr := a; 
		kr := i)
	   else ();
	   let val a = abs_real(sub(pxi,i+1))
	   in
	       if !zi < a then 
		   (zi := a; 
		    ki := i)
	       else ()
	   end
      end);
      let val zm = if abs_real (!zr) < abs_real (!zi) then !zi else !zr 
      in
	  print_real zm; print_newline()
      end
  end
 end)

in
    fun runFft() = 
	let 
	    val np = ref 16 
	in for(1,14,fn i => (test (!np); np := (!np)*2))
	end

end
