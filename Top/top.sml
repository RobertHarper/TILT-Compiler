(*$import Prelude List TextIO TopLevel Manager *)


fun top() = 
  let val _ = print "TILT Compiler: "
      val _ = TextIO.flushOut TextIO.stdOut
      val line = TextIO.inputLine TextIO.stdIn
      val rev_chars = rev(explode line)
      val mapfile = implode(rev(tl rev_chars))
      val _ = (print "Processing mapfile: '"; print mapfile; print "'\n")
  in  Manager.tilc(mapfile,false,NONE,[])
  end

val _ = top()
