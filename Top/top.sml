(*$import Prelude List TextIO TopLevel Manager Stats *)


fun top() = 
  let val _ = print "TILT Compiler.  Please type one of the following: \n"
      val _ = print "(1) make <mapfile>\n"
      val _ = print "(2) master <mapfile>\n"
      val _ = print "(3) slave\n"
      val _ = print "(4) set <flag>\n"
      val _ = print "(5) clear <flag>\n"
      val _ = TextIO.flushOut TextIO.stdOut
      val line = TextIO.inputLine TextIO.stdIn
      val line = String.substring(line, 0, (size line) - 1)  (* drop the return *)
      val words = String.fields Char.isSpace line
      val _ = (case words of 
		   ["make",mapfile] => Manager.make mapfile
		 | ["master",mapfile] => Manager.master mapfile
		 | ["slave"] => Manager.slave()
		 | ["set",flag] => Stats.bool(flag) := true
		 | ["clear",flag] => Stats.bool(flag) := false
		 | _ => print "Command not understood.\n\n")
  in  top()
  end

val _ = top()
