signature LINKALL = 
sig
    val compile_prelude : bool * string -> string (* use_cache * input filename -> asm filename *)
    val compile : string -> string (* input filename -> executable filename *)
    val compiles : string list -> string (* input filename -> executable filename *)
    val test : string -> string (* input filename -> executable filename *)
end

structure Linkall : LINKALL = 
struct

    val error = fn s => Util.error "linkall.sml" s
    val debug = ref false

    datatype platform = ALPHA | ALPHA_MLRISC | PPC
    val cur_platform = ref ALPHA
    fun specific_link_file arg = 
	(case (!cur_platform) of
	     ALPHA => Linkalpha.link arg
	   | ALPHA_MLRISC => AlphaLink.link arg
	   | PPC => error "no PPC") (* Linkppc.comp_file arg *)
    fun specific_comp_files (debug,args) = 
	(case (args,!cur_platform) of
	     ([arg],ALPHA) => [(if debug then Linkalpha.test else Linkalpha.compile) arg]
	   | (_,ALPHA) => (if debug then error "no test" else Linkalpha.compiles) args
	   | ([arg],ALPHA_MLRISC) => [(if debug then AlphaLink.test else AlphaLink.compile) arg]
	   | (_,ALPHA_MLRISC) => (if debug then error "no test" else AlphaLink.compiles) args
	   | (_,PPC) => error "no PPC") (* Linkppc.comp_file arg *)
    val cached_prelude = ref (NONE : (string * Rtl.label) option)
    fun specific_reparse_prelude arg = 
	let val (littleEndian,compile_prelude) = 
	    (case (!cur_platform) of
		 ALPHA => (true,Linkalpha.compile_prelude)
	       | ALPHA_MLRISC => (true,AlphaLink.compile_prelude)
	       | PPC => (false,error "no PPC"))
		 (* Linkppc.reparse_prelude arg *)
	    val _ = (Stats.bool "littleEndian") := littleEndian
            val (prelude_file,prelude_label) = compile_prelude arg
	    val _ = cached_prelude := SOME(prelude_file,prelude_label)
	in  prelude_file
	end
	  
    exception Transcript
    local
        structure T = TextIO
	val curout = ref (NONE : T.StreamIO.outstream option)
    in
	fun fileon(s) = 
	      (curout := SOME (T.getOutstream T.stdOut);
               T.setOutstream (T.stdOut, 
                               T.getOutstream(T.openOut s)))
	fun fileoff() = 
	    let 
		val old_stdOut =
	              (case !curout of
			NONE => raise Transcript
		       | SOME x => x)
	    in
               T.setOutstream (T.stdOut, old_stdOut);
               curout := NONE
	    end
    end

    val atom = ref false
    val runtime_dir = ref (OS.FileSys.getDir() ^ "/Runtime")
	
	
    fun link (outname,asm_files,opt) =
	let 
	    fun getflags () =
		if !atom then "LDFLAGS='-r -N' CFLAGS='-O -Wl,-r, -nonshared'"
		else ""
	    (*
             val _ = (print "Done creating assemblies:";
	     app (fn f => (print f; print " ")) asm_files; 
			     print "\n")
		*)
	    val current_dir = OS.FileSys.getDir()
	    val _ = ((OS.FileSys.chDir (!runtime_dir)) handle exn => 
		     (print "bad Runtime directory.\n"; raise exn))
	    val asm_files = map (fn s => (current_dir^"/"^s)) asm_files
	    fun folder (str,acc) = acc ^ " " ^ str
	    val target = foldl folder " target='" (asm_files @ ["'"])
	    val out = " out="^current_dir^"/"^outname
	    val makecom = ("gmake " ^ getflags() ^ target ^ " " ^ opt ^
			   (if false  (* (!Linkrtl.Tortl.HeapProfile) *)
				then "hprof=1"
			    else "")
				^ out)
	    val _ = print ("Executing: " ^ makecom ^ "\n")
	    val _ = (Stats.timer("Linking",OS.Process.system)) makecom
	    val _ = OS.FileSys.chDir current_dir;
	    val _ = print "Linking complete\n"
	    val _ = if (!debug)
			then print ("Done creating final executable: " ^ outname ^ "\n")
		    else ()
	in
	    outname
	end (* link *)

    fun wrapper string command = Stats.timer(string,command)
    val link          = wrapper "linking" link

    

    fun compile_help _ [] = error "compile given no files"
      | compile_help debug src_files =
	let val last_srcfile = List.last src_files
	    val outname = last_srcfile ^ ".exe"
	    val asm_labels = specific_comp_files (debug,src_files)
	    val (asm_files,local_labels) = 
		(case (!cached_prelude) of
		     SOME (filename,label) => (filename::(map #1 asm_labels),
					       label::(map #2 asm_labels))
		   | _ => error "link failed: no prelude")
	    val _ = (print "---- there are "; print (Int.toString (length local_labels));
		     print " local_labels\n")
	    val _ = specific_link_file(last_srcfile,local_labels) 
	    val _ = link(outname,asm_files,"")
	in  outname
	end

    fun test filename = compile_help true [filename]
    fun compile filename = compile_help false [filename]
    fun compiles filenames = compile_help false filenames
    fun compile_prelude arg = specific_reparse_prelude arg
	

    fun reset_wrapper f arg = let val _ = Stats.clear_stats()
				  val res = f arg
				  val _ = Stats.print_stats()
			      in  res
			      end

    val compile_prelude = reset_wrapper compile_prelude
    val compile = reset_wrapper compile
    val compiles = reset_wrapper compiles
    val test = reset_wrapper test


    fun nj_process(infile,outfile) =
      let 
	val inSTR = TextIO.openIn infile
	val inData = 
	  let 
	    fun loop() = 
	      if (TextIO.endOfStream inSTR)
		then nil
	      else (TextIO.inputLine inSTR)::loop()
	  in
	    loop()
	  end
	val outData = 
	  let 
	    fun loop [] = nil
	      | loop (a::b) = 
		if ((String.size a > 3) andalso
		    (String.substring(a,0,3) = "val"))
		  then nil
		else a::(loop b)
	  in
	    loop (tl inData)
	  end
	val _ = TextIO.closeIn inSTR
	val outSTR = TextIO.openOut outfile
	val _ = app (fn s => TextIO.output(outSTR,s)) outData;
	val _ = TextIO.closeOut outSTR
      in
	()
      end

    fun time_alpha(progs,logname) = 
      let 
	fun logentry s = OS.Process.system("echo \"" ^ s ^ "\" >> " ^ logname);
        val _ = TextIO.closeOut(TextIO.openOut logname)
	fun test (ss) = 
	  let 
	    val _ = print ("About to start TIL compiling " 
			   ^ ss ^ "\n");
	    val outname = compile ss
	    val _ = print ("Done compiling with output: " 
			   ^ outname ^ "\n");
	    val alphaout = ss ^ ".alphaout"
	    val com = outname ^ " > " ^  alphaout
	    val _ = print ("About to run: " ^ com ^ "\n");
	    val _ = OS.Process.system com;
	    val _ = print ("Done running: " ^ com ^ "\n");
	    val _ = logentry ss
	    val com = "grep time "^alphaout^ ">>" ^ logname
	    val _ = OS.Process.system com
	  in
	    print "test done\n"
	  end
	val res = app test progs
      in
	res
      end
(*
    fun test_alpha(progs) = 
      let 
	val logname = "test_alpha.log";
	fun logentry s = OS.Process.system("echo \"" ^ s ^ "\" >> " ^ logname);
	fun test (ss) = 
	  let
	    val tmpname = "testalpha.tmp";
	    val _ = print ("About to call NJ and redirect to: " ^ 
			   tmpname ^ "\n");
	    val _ = (fileon(tmpname); use ss; fileoff()) 
	      handle _ => (fileoff(); 
			   print "Error occurred\n"; 
			   logentry "Error occurred while compiling in SML/NJ";
			   ())
	    val _ = print ("Done calling NJ, post-processing output file\n");
	    val njout = ss ^ ".njout" 
	    val _ = nj_process(tmpname,njout);
	    val _ = print ("Post-processing done with result in " 
			   ^ njout ^".\n\n\n");
	      

	    val _ = print ("About to start TIL compiling " 
			   ^ ss ^ "\n");
	    val outname = comp_file ss 
	    val _ = print ("Done compiling with output: " 
			   ^ outname ^ "\n");
	    val alphaout = ss ^ ".alphaout"
	    val com = outname ^ " > " ^ alphaout;
	    val _ = print ("About to run: " ^ com ^ "\n");
	    val _ = OS.Process.system com;
	    val _ = print ("Done running: " ^ com ^ "\n");
	      
	    val diffcom = ("diff " ^ alphaout ^ " " ^ njout 
			   ^ " >> " ^ logname)
	    val _ = logentry diffcom
	    val _ = OS.Process.system(diffcom)
	  in
	    print "test done\n"
	  end
	val res = app test progs
      in
	res
      end
*)


    val testprogs = 
      map (fn s => "tests/" ^ s)
      (["fact.sml", "fib.sml", 
	"maptest.sml", "printint.sml",
	"dotprod.sml", "matmult.sml"] @
       (map (fn s => "test" ^ s ^ ".sml") 
	["2a", "4a", "5a", "6a", "13", "12",
	 "8", "3", "7", "9", "10", "11"]));

    val pldiprogs = map (fn s => "bench/"^s)
      ["checksum.sml","fft.sml","leroy.sml", "lexgen.sml",
	"life.sml","matmult.sml", "pia.sml","simple.sml"];


    val benchprogs = map (fn s => "bench/"^s)
      ["bignum.sml","checksum.sml","fft.sml","frank.sml",
       "leroy.sml", "lexgen.sml","life.sml","matmult.sml", "msort.sml",
       "pia.sml","quicksort.sml","sieve.sml","simple.sml", 
       "soli.sml","sort.sml","takc.sml","taku.sml"];
(* boyer, cw, fox, nucleic, vliw, yacc *)


  end; (* struct *)


