use "getopt.sig.sml";
use "getopt.sml";

fun printOpt (c, NONE) = print (Char.toString c ^ " ")
  | printOpt (c, SOME s) = print (Char.toString c ^ "<<" ^ s ^ ">> ");
fun printOpts opts = (print "options: "; app printOpt opts; print "\n");

fun printArg arg = print ("<<" ^ arg ^ ">> ");
fun printArgs args = (print "arguments: "; app printArg args; print "\n");

fun printErr msg = print ("error: " ^ msg ^ "\n");
    
fun test (cmd, nil) = (print ("usage: " ^ cmd ^ ": optstring parameters");
		       OS.Process.failure)
  | test (cmd, optstring::args) =
    (case Getopt.getopt (Getopt.optstring optstring, args)
       of Getopt.Error msg => (printErr msg; OS.Process.failure)
	| Getopt.Success (options,args) => (printOpts options;
					    printArgs args;
					    OS.Process.success));

(* SMLofNJ.exportFn ("getopt", test); *)
