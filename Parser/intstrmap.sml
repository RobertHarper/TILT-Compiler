(*$import Prelude INTSTRMAP Array List Word Int Control *)

(* Copyright 1989 by AT&T Bell Laboratories *)
structure IntStrMap : INTSTRMAP =
struct
  open Array List
  infix 9 sub
  val itow = Word.fromInt
  val wtoi = Word.toIntX
  datatype 'a bucket = NIL | B of (int * string * 'a * 'a bucket)
  datatype 'a intstrmap =
    H of {table: 'a bucket array ref,elems: int ref,exn: exn,name: string option}
  fun bucketapp f =
      let fun loop NIL = ()
	    | loop(B(i,s,j,r)) = (f(i,s,j); loop r)
      in loop
      end
  fun roundsize size = 
      let fun f x = if x >= size then x else f (x*2)
      in f 1
      end
  fun namednew(name, size, exn) =
      H {table=ref(array(roundsize size,NIL)),elems=ref 0,exn=exn,
	 name=SOME name}
  fun new(size, exn) =
      H {table=ref(array(roundsize size,NIL)),elems=ref 0,exn=exn,name=NONE}
  val elems = fn (H{elems,...}) => !elems
  fun index(a, i) = wtoi (Word.andb(itow i, itow(Array.length a - 1)))
  fun map (H{table,exn,...}) =
      let fun find(i,s,NIL) = raise exn
            | find(i,s,B(i',s',j,r)) = if i=i' andalso s=s' then j else find(i,s,r)
	  fun map' (i,s) = let val ref a = table
			   in find (i,s,a sub (index(a, i)))
			   end
      in map'
      end
  fun rmv (H{table=ref a,elems,...}) (i,s) =
      let fun f(B(i',s',j,r)) =
	        if i=i' andalso s=s' then (elems := !elems-1; r) else B(i',s',j,f r)
	    | f x = x
	  val indx = index(a, i)
      in  update(a, indx, f(a sub indx))
      end
  fun app f (H{table=ref a,...}) =
      let fun zap 0 = ()
	    | zap n = let val m = n-1 in bucketapp f (a sub m); zap m end
      in  zap(Array.length a)
      end
  fun add (m as H{table as ref a, elems, name, ...}) (v as (i,s,j)) =
      let val size = Array.length a
       in if !elems <> size
	  then let val index = wtoi (Word.andb(itow i, itow(size-1)))
		   fun f(B(i',s',j',r)) =
		         if i=i' andalso s=s' then B(i,s,j,r) else B(i',s',j',f r)
		     | f x = (elems := !elems+1; B(i,s,j,x))
	       in update(a,index,f(a sub index))
	       end
	  else let val newsize = size+size
		   val newsize1 = newsize-1
		   val new = array(newsize,NIL)
		   fun bucket n =
		       let fun add'(a,b,B(i,s,j,r)) =
			       if wtoi (Word.andb(itow i, itow newsize1)) = n
			       then add'(B(i,s,j,a),b,r)
			       else add'(a,B(i,s,j,b),r)
			     | add'(a,b,NIL) = 
			       (update(new,n,a);
				update(new,n+size,b);
				bucket(n+1))
		       in add'(NIL,NIL,a sub n)
		       end
	       in (case name of
		     NONE => ()
		   | SOME name =>
		     List.app Control.Print.say[
		         "\nIncreasing size of intstrmap ", name, " to: ",
		         Int.toString newsize, "\n"]);
		  bucket 0 handle Subscript => ();
		  table := new;
		  add m v
	       end
      end
  fun intStrMapToList(H{table,...})=
      let val a = !table;
	  val last = Array.length a - 1
	  fun loop (0, NIL, acc) = acc
	  |   loop (n, B(i,s,j,r), acc) = loop(n, r, (i,s,j)::acc)
	  |   loop (n, NIL, acc) = loop(n-1, a sub (n-1), acc)
       in loop(last,a sub last,[])
      end
  fun transform (f:'a -> '2b) (H{table=ref a, elems=ref n, exn, name}) =
      let val newa = array(Array.length a,NIL)
	  fun mapbucket NIL = NIL
	    | mapbucket(B(i,s,x,b)) = B(i,s,f x,mapbucket b)
	  fun loop i = (update(newa,i,mapbucket(a sub i)); loop(i+1))
       in loop 0 handle Subscript => ();
	  H{table=ref newa, elems=ref n, exn=exn, name=name}
      end
end

(*
 * $Log$
# Revision 1.5  2000/11/27  22:37:01  swasey
# *** empty log message ***
# 
 * Revision 1.4  2000/09/12 18:56:53  swasey
 * Changes for cutoff compilation
 *
 * Revision 1.3  1999/09/22 15:46:07  pscheng
 * *** empty log message ***
 *
# Revision 1.2  1998/01/21  20:40:19  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.1  97/03/26  14:12:29  pscheng
# added copy of SMLNJ parser files
# 
 *)
