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

