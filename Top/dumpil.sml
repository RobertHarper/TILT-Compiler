(*$import Prelude TopLevel CommandLine BinIO OS LinkIl Name Run *)

structure Dumpil
    :> sig val main : string * string list -> OS.Process.status end =
struct

    fun readPartialContextRaw file = 
	let 
	    val is = BinIO.openIn file
	    val res = LinkIl.IlContextEq.blastInPartialContext is
	    val _ = BinIO.closeIn is
	in  res
	end
    
    fun printPartialContextFile file =
	let
	    val _ = (print "----------------------------------------\n";
		     print file; print "\n")
	    val (c,u) = readPartialContextRaw file
	    val _ = (print "context:\n";
		     LinkIl.Ppil.pp_context c;
		     print "\n")
	    fun printFV (var, label) = (LinkIl.Ppil.pp_var var;
					print "\t";
					LinkIl.Ppil.pp_label label;
					print "\n")
	    val _ = (print "free variables:\n";
		     Name.VarMap.appi printFV u)
	in  ()
	end

    fun main (_, files) = (LinkIl.IlContextEq.blast_debug := true;
			   app printPartialContextFile files;
			   OS.Process.success)
end
val _ = Run.run Dumpil.main
