structure MLRiscErrorMsg = struct
  exception Error
  val print = fn s => TextIO.output(TextIO.stdOut, s)
  fun impossible msg =
      (app print ["Error: MLRisc bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)
end


