structure Linkall = 
struct

    val error = fn s => Util.error "linkall.sml" s

    datatype platform = ALPHA | PPC
    val cur_platform = ref ALPHA
    fun specific_comp_file arg = 
	(case (!cur_platform) of
	     ALPHA => Linkalpha.comp_file arg
	   | PPC => error "no PPC") (* Linkppc.comp_file arg *)
(*
    fun specific_reparse_prelude arg = 
	(case (!cur_platform) of
	     ALPHA => Linkalpha.reparse_prelude arg
	   | PPC => error "no PPC") (* Linkppc.reparse_prelude arg *)
*)
	  
	exception Transcript
	local
	    open Posix.IO
	    open Posix.FileSys
	    val curout = ref (NONE : file_desc option)
	in
	    fun fileon(s) = 
		let 
		    val _ = curout := SOME(dup stdout)
		    val _ = close stdout
		    val _ = createf (s,O_RDWR,O.trunc,S.flags[S.irusr,S.iwusr])
		in ()
		end
	    fun fileoff() = 
		let 
		    val _ = close stdout
		    val _ = dup (case !curout of
				     NONE => raise Transcript
				   | SOME x => x)
		    val _ = curout := NONE
		in
		    ()
		end
	end
(*    
	fun test_rtl(progs) = 
	    let 
		fun test (ss) = 
		    let 
			val outname = ss ^ ".rtlout"
			val outstream = TextIO.openOut outname;
			val _ = Linkrtl.I.outstream := SOME outstream
		    in
			(Linkrtl.comp_file ss
			 handle e => (print "Uncaught exception while running "; 
				      print ss;
				      print "\n";
				      raise e));
			Linkrtl.I.outstream := NONE;
			TextIO.closeOut outstream
		    end
	    in
		app test progs
	    end
*)	
	val atom = ref false
	val runtime_dir = ref (OS.FileSys.getDir() ^ "/Runtime")
	    
	local
	    fun getflags () =
		if !atom then "LDFLAGS='-r -N' CFLAGS='-O -Wl,-r, -nonshared'"
		else ""
	in
	    fun link (infile,asm_files,opt) =
		let 
		    val outname = infile ^ ".exe"
		    val _ = (print "Done creating assemblies:";
			     app (fn f => (print f; print " ")) asm_files; 
			     print "\n")
		    val current_dir = OS.FileSys.getDir()
		    val _ = ((OS.FileSys.chDir (!runtime_dir)) handle exn => 
			     (print "bad Runtime directory.\n"; raise exn))
		    val targetfile = hd(rev asm_files)
		    val preludefiles = rev(tl(rev asm_files))
		    val target = " target="^current_dir^"/"^targetfile
		    val out = " out="^current_dir^"/"^outname
		    val makecom = ("gmake " ^ getflags() ^ target ^ " " ^ opt ^
				   (if false  (* (!Linkrtl.Tortl.HeapProfile) *)
				      then "hprof=1"
				    else "")
				      ^ out)
                    val _ = print ("Executing: " ^ makecom ^ "\n")
		    val _ = OS.Process.system makecom
		    val _ = OS.FileSys.chDir current_dir;
		    val _ = print ("Done creating final executable: " ^ outname ^ "\n");
		in
		    outname
		end
	    

    
      (* compilation of an entire file *)
    
      fun comp_file infile = link(infile,specific_comp_file infile,"");
      fun comp_file_opt (infile,opt) = link(infile,specific_comp_file infile,opt);
      fun assemble_file infile = specific_comp_file infile

      fun wrapper string command = Stats.timer(string,command)


      val link          = wrapper "linking" link
      val comp_file     = wrapper "total" comp_file
      val comp_file_opt = wrapper "total" comp_file_opt
      val assemble_file = wrapper "toasm" assemble_file

(*      val reparse_prelude = specific_reparse_prelude *)
    end


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
	    val outname = comp_file ss
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


