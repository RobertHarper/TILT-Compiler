
structure Top :>

    sig

	val runFile : string -> unit
	val testAll : unit -> unit
	val run     : unit   -> unit

	val tests1 : string list
	val tests2 : string list
	val tests3 : string list
	val testGroup : string list -> unit
    end

    = 

    struct

	fun goahead s drop =
	    let val SOME ast = ParseString.parse s
		
		val _ = Eval.eval ast 
		    handle e as Base.Eval s => (print "Evaluation Error: "; print s; 
						print "\n"; raise e)
	    in  if drop then Base.exit_ok () else ()
	    end handle _ => if drop then Base.exit_error () else ()
		
	(* from a file - don't exit *)	
	fun runFile file = goahead (ParseString.file2string file) false

	(* from standard in - then exit *)
	fun run () = goahead (Stdin.stdintostring ()) true

	val tests1 = 
	    ["scene1.gml",
	     "scene2.gml",
	     "scene3.gml",
	     "scene4.gml",
	     "scene5.gml",
	     "scene6.gml",
	     "scene7.gml"]

	val tests2 = 
	    ["spheres.gml" ,
	     "spheres2.gml",
	     "reflect.gml",
	     "checked-cube.gml",
	     "fib.gml",
	     "dice.gml",
	     "spotlight.gml",
	     "golf.gml"]
	     
	val tests3 = ["cone2.gml",
		      "cylinder.gml",
		      "cylinders.gml"]

	val failed = ref ([] : string list)

	fun testGroup tests = 
	    (app (fn s => (ignore (runFile s) handle _ => failed := (s :: !failed))) tests;
	     app (fn s => print ("Failed test "^s^"\n")) (!failed);
	     failed := [])

	fun testAll() = testGroup (tests1 @ tests2 @ tests3)

    end



