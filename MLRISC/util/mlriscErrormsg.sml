structure MLRiscErrorMsg = struct
  exception Error
  val print = fn s => TextIO.output(TextIO.stdOut, s)
  fun impossible msg =
      (app print ["Error: MLRisc bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)
end


(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:29  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:17:51  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:55  pscheng
# *** empty log message ***
#
 *)
