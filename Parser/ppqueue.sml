(*$import Prelude TopLevel Array *)

(* ppqueue.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature PPQUEUE =
sig
   type 'a  queue
   exception QUEUE_FULL
   exception QUEUE_EMPTY
   datatype  Qend = Qback | Qfront
   val is_empty : 'a queue -> bool
   val mk_queue : int -> '2a -> '2a queue
   val clear_queue : 'a queue -> unit
   val queue_at : Qend -> 'a queue -> 'a
   val en_queue : Qend -> 'a -> 'a queue -> unit
   val de_queue : Qend -> 'a queue -> unit
end

structure PPQueue: PPQUEUE =
struct

  open Array
  infix 9 sub

  datatype Qend = Qfront | Qback

  exception QUEUE_FULL
  exception QUEUE_EMPTY
  exception REQUESTED_QUEUE_SIZE_TOO_SMALL

  fun ++ i n = (i + 1) mod n
  fun -- i n = (i - 1) mod n

  datatype 'a queue = QUEUE of {elems: 'a array, (* the contents *)
			       front: int ref,
			       back: int ref,
			       size: int}  (* fixed size of element array *)

    fun is_empty (QUEUE{front=ref ~1, back=ref ~1,...}) = true
      | is_empty _ = false

    fun mk_queue n init_val = 
	if (n < 2)
	then raise REQUESTED_QUEUE_SIZE_TOO_SMALL
	else QUEUE{elems=array(n, init_val), front=ref ~1, back=ref ~1, size=n}

    fun clear_queue (QUEUE{front,back,...}) = (front := ~1; back := ~1)

    fun queue_at Qfront (QUEUE{elems,front,...}) = elems sub !front
      | queue_at Qback (QUEUE{elems,back,...}) = elems sub !back

    fun en_queue Qfront item (Q as QUEUE{elems,front,back,size}) =
	  if (is_empty Q)
	  then (front := 0; back := 0;
		update(elems,0,item))
	  else let val i = --(!front) size
	       in  if (i = !back)
		   then raise QUEUE_FULL
		   else (update(elems,i,item); front := i)
	       end
      | en_queue Qback item (Q as QUEUE{elems,front,back,size}) = 
	  if (is_empty Q)
	  then (front := 0; back := 0;
		update(elems,0,item))
	  else let val i = ++(!back) size
	       in  if (i = !front)
		   then raise QUEUE_FULL
		   else (update(elems,i,item); back := i)
	       end

    fun de_queue Qfront (Q as QUEUE{front,back,size,...}) = 
	  if (!front = !back) (* unitary queue *)
	  then clear_queue Q
	  else front := ++(!front) size
      | de_queue Qback (Q as QUEUE{front,back,size,...}) =
	  if (!front = !back)
	  then clear_queue Q
	  else back := --(!back) size


end (* structure PPQueue *)

(*
 * $Log$
# Revision 1.3  2000/09/12  18:57:08  swasey
# Changes for cutoff compilation
# 
# Revision 1.2  98/01/21  20:40:42  pscheng
# moved the .sig files to .sig.sml file
# 
# Revision 1.1  97/03/26  14:12:34  pscheng
# added copy of SMLNJ parser files
# 
 *)
