signature TRANSACTION_LOG =
sig

   exception TransactionLog

   type version = int
   val version      : version ref
   val add_object   : { rollback : version -> unit, 
			commit   : version -> unit } -> unit   
   val begin        : unit -> unit
   val commit       : unit -> unit
   val abort        : unit -> unit
   val init         : unit -> unit
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:17:14  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:07  pscheng
# *** empty log message ***
#
 *)
