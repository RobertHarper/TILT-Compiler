functor TransactionFn(Log : TRANSACTION_LOG) : TRANSACTION =
struct

   exception Abort

   fun transaction default func =
   let
       val _ = Log.begin()
       val x = func()
       val _ = Log.commit()
   in
       x
   end
   handle Abort => (Log.abort(); default)
	| e     => (Log.abort(); raise e)

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:17:15  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:11  pscheng
# *** empty log message ***
#
 *)
