signature LINKALL = 
sig
    val compile : string -> string (* input filename -> executable filename *)
    val compiles : string list -> string (* input filename -> executable filename *)
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
(*	   | ALPHA_MLRISC => AlphaLink.link arg *)
	   | PPC => error "no PPC") (* Linkppc.comp_file arg *)
    fun specific_comp_file file =
	let 
	    val (_,fp,_,dec) = LinkParse.parse_impl file
	    val il_mod = (case LinkIl.elab_dec(LinkIl.plus_context[], fp, dec) of
			      NONE => error (file ^ " failed to elaborate")
			    | SOME il_mod => il_mod)
	    val nil_mod = Linknil.il_to_nil (file,il_mod)
	    val rtl_mod = Linkrtl.nil_to_rtl (file,nil_mod)
	in
	    (case (!cur_platform) of
		 ALPHA => Linkalpha.rtl_to_asm (file, rtl_mod)
(*	       | ALPHA_MLRISC => AlphaLink.rtl_to_asm (file, rtl_mod) *)
	       | PPC => error "no PPC")
	end
    val specific_comp_files = map specific_comp_file 	  

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


    fun compiles [] = error "compile given no files"
      | compiles src_files =
	let val last_srcfile = List.last src_files
	    val outname = last_srcfile ^ ".exe"
	    val asm_labels = specific_comp_files src_files
	    val (files, labels) = Listops.unzip asm_labels
	    val last_file = List.last files
	    val _ = (print "---- there are "; print (Int.toString (length labels));
		     print " local_labels\n")
	    val _ = specific_link_file(last_file,labels) 
	    val _ = link(outname,files,"")
	in  outname
	end

    fun compile filename = compiles [filename]

    fun reset_wrapper f arg = let val _ = Stats.clear_stats()
				  val res = f arg
				  val _ = Stats.print_stats()
			      in  res
			      end

    val compile = reset_wrapper compile
    val compiles = reset_wrapper compiles


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


