(*
 Harness to run and time the benchmarks, individually or as a group.
 Usage:  btimes [-r n] [ benchmarks ]
   -r n, if present, indicates that n runs should be done, and the results
         averaged before reporting.  Defaults to 1.
   benchmarks is an optional list of which benchmarks to run.  Defaults
         to all of them.
   *)
structure Benchmarks :> RUN = 
  struct
    
    val benchmarks = 
      [(Life.run,"Life"),
       (Leroy.run,"Leroy"),
(*       (Fft.run,"Fft"),
       (Boyer.run,"Boyer"),*)
       (Simple.run,"Simple"),
       (Tyan.run,"Tyan"),
       (*   (Tyan1.run,"Tyan1"),
	(Tyan2.run,"Tyan2"),
	(Tyan3.run,"Tyan3"),*)
       (Msort.run,"Msort"),
       (Pia.run,"Pia"),
       (* Data files don't exist.
	* (Pia1.run,"Pia1"),
	*   (Pia2.run,"Pia2"),
	*  (Pia3.run,"Pia3"),
	*) 
       (Lexgen.run,"Lexgen"),
       (*  Data files don't exist
	*  (Lexgen1.run,"Lexgen1"),
	*  (Lexgen2.run,"Lexgen2"),
	*  (Lexgen3.run,"Lexgen3"),
	*)
(*       (Frank.run,"Frank"),*)
       (Arithmetic.run,"Arithmetic"),
       (Arithmetic32.run,"Arithmetic32"),
       (F_Arithmetic.run,"F_Arithmetic"),
(*       (BarnesHut.run,"BarnesHut"),*)
       (PQueens.run,"PQueens"),
(*       (Quicksort.run,"Quicksort"),
       (Quicksort2.run,"Quicksort2"),*)
       (Takc.run,"Takc"),
       (Taku.run,"Taku")
       ]
      

    fun eprint (s : string) : unit =
      TextIO.output(TextIO.stdErr, s)

    fun FAIL msg = raise Fail msg
      
    fun findall [] _ = []
      | findall (a::aa) l = 
      let
	fun isb b (_,b') = b = b'

	val bench = 
	  (case List.find (isb a) l
	     of SOME bench => bench
	      | NONE => (print "Benchmarks are \n";
			 List.app (fn (_,name) => print ("\t"^name^"\n")) l;
			 FAIL (a^" is not a known benchmark\n")))
      in bench :: findall aa l
      end
    
    fun main () : unit =
      (let
	 fun usage msg = 
	   (msg ^ "\nusage: " ^ CommandLine.name() ^  " [-r n] [ benchmarks ]\n")

	 fun fail msg = FAIL (usage msg)

	 fun parseInt s = 
	   (case Int.fromString s
	      of SOME i => i
	       | NONE => fail "argument to -r is not an integer")

	 val args = CommandLine.arguments() 

	 fun parse_options ({argc,eargf,acc,...} : int Arg.argument) : int = 
	   (case argc 
	      of #"r" => parseInt (eargf (fn () => fail "-r requires argument"))
	       | _ => fail ("-" ^ str argc ^" not a valid option"))


	 val (args,runs) = Arg.arguments parse_options (args,1)
	 val torun = case args 
		       of [] => benchmarks
			| _ => findall args benchmarks
       in TimeAndRun.report (runs,torun)
       end)
	 handle e => (eprint (CommandLine.name());
		      eprint ": ";
		      eprint (case e
				of Fail msg => msg
				 | _ => exnMessage e);
		      eprint "\n";
		      OS.Process.exit OS.Process.failure)

    val run = main
  end

  val _ = Benchmarks.run()
