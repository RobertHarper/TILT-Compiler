(*
 *  A reference that allows undo.
 *)

signature UNDOABLE_REF =
sig
   eqtype 'a uref 
   val uref : 'a -> 'a uref
   val !   : 'a uref -> 'a
   val :=  : 'a uref * 'a -> unit
end

functor UndoableRefFn (Log : TRANSACTION_LOG) : UNDOABLE_REF =
struct

   type 'a uref = 'a ref * Log.version ref 

   fun uref a = (ref a, ref(!Log.version))

   fun !! (r,_) = !r

   fun commit (x,v) = fn ver => v := ver

   fun rollback (x,v) = 
   let val x' = !x
   in  fn ver => (x := x'; v := ver)
   end

   fun ::= (r as (x,v),y) = 
   let val ver = !Log.version
   in  if !v <> ver then (Log.add_object{rollback = rollback r,
					 commit   = commit r
					}; 
			  v := ver)
       else ();
       x := y
   end

   val !  = !!
   val op := = ::=
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:17:19  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:14  pscheng
# *** empty log message ***
#
 *)
