structure Toplevel : 
  sig 
    val toplevel : (string * string list -> OS.Process.status)
    val export_toplevel : string -> unit
  end  =
  struct

      open Compstream
      open Linkub
      open Linkb.Linkopt
      structure Parse = TopdecParsing.Parse
	
      exception ERROR of string
      fun error (s : string) = raise (ERROR s)
      fun atoi s = 
	let val reader = Int.scan StringCvt.DEC
	in  (case StringCvt.scanString reader s of
	       NONE => (error "atoi cannot parse string")
	     | (SOME x) => x)
	end

      fun atob s = 
	let val reader = Bool.scan 
	in  (case StringCvt.scanString reader s of
	       NONE => (error "atob cannot parse string")
	     | (SOME x) => x)
	end

      val _ = (flat := false; messages := false; 
	       closure_optimize := true; 
	       sink := true; 
	       reduce_always := true)

      val bool_flags : (string * string * bool ref) list = 
	[("do_tailopt", "controls whether the dps optimization is done",
	  Linkub.do_tailopt),
	 ("do_writelist", "controls whether set! add to writelist",
	  Linkrtl.Tortl.do_writelist),
	 ("HeapProfile", "controls whether client programs have heap profiling",
	  Linkrtl.Tortl.HeapProfile),
	 ("closure_opt", "optimize closure representation",
	  closure_optimize),
	 ("gcreify_debug","debug gc reify info",Gcreify.DEBUG),
	 ("tc_lmli", "typecheck lmli form", Linklmli.typecheck_lmli),
	 ("tc_box_real","typecheck boxed real lmli",
	  Linklmli.typecheck_box_real),
	 ("tc_flat_arg","typecheck flat arg lmli",
	  Linklmli.typecheck_flat_args),
	 ("tc_flat_cons","typecheck flat cons lmli",
	  Linklmli.typecheck_flat_cons),
	 ("tc_b","typecheck bform",Linkb.typecheck_b),
	 ("tc_alpha","typecheck alpha converted bform",tc_alpha),
	 ("tc_cmpelim","typecheck comparison elim",tc_cmpelim),
	 ("tc_cse","typecheck common sub expression elim",tc_cse),
	 ("tc_dead","typecheck dead code elim",tc_deadcode),
	 ("tc_hoist","typecheck hoist",tc_hoist),
	 ("tc_inline","typecheck inline expand",tc_inline_expand),
	 ("tc_inline","typecheck inline once",tc_inline_once),
	 ("tc_invariant","typecheck invariant",tc_invariant),
	 ("tc_minfix","typecheck minimize fix",tc_minfix),
	 ("tc_sink","typecheck sink",tc_sink),
	 ("tc_switch","typecheck switch",tc_switch),
	 ("tc_switchfail","typecheck switch fail",tc_switchfail),
	 ("tc_uncurry","typecheck uncurry",tc_uncurry),
	 ("tc_reduce","typecheck reduce",tc_reduce),
	 ("tc_transparent","typecheck transparent",tc_transparent),
	 ("tc_simple_reduce","typecheck simple reduce",tc_simple_reduce),
	 ("tc_opt","typecheck optimized bform",Linkb.typecheck_opt),
	 ("print_lambda", "print lambda form", Linklmli.print_lambda),
	 ("print_lmli","print lmli form",Linklmli.print_lmli),
	 ("print_box_real","print boxed real lmli",Linklmli.print_box_real),
	 ("print_flat_cons","print flat cons lmli",Linklmli.print_flat_cons),
	 ("print_flat_args","print flat args lmli",Linklmli.print_flat_args),
	 ("print_bform","print starting bform",Linkb.print_start),
	 ("print_alpha","",print_alpha),
	 ("print_cmpelim","",print_cmpelim),
	 ("print_cse","",print_cse),
	 ("print_deadcode","",print_deadcode),
	 ("print_gcreify","",print_gcreify),
	 ("print_hoist","",print_hoist),
	 ("print_inline expand","",print_inline_expand),
	 ("print_inline once","",print_inline_once),
	 ("print_invariant","",print_invariant),
	 ("print_minfix","",print_minfix),
	 ("print_sink","",print_sink),
	 ("print_switch","",print_switch),
	 ("print_switchfail","",print_switchfail),
	 ("print_uncurry","",print_uncurry),
	 ("print_transparent","",print_transparent),
	 ("print_simple_reduce","",print_simple_reduce),
	 ("print_ubform", "print ubform defn", print_ubform),
	 ("print_opt", "print optimized bform defn", Linkb.print_bform),
(*	 ("print_closure", "print closure defn", Linkclosure.print_closure), *)
	 ("tapp_pure","consider type applications pure",Linkb.consider_tapp_pure),
	 ("alpha","alpha convert",alpha),
	 ("closed","check to see if bform terms are closed",debug),
	 ("cmpelim","eliminate redundant comparisons",cmpelim),
	 ("cse","eliminate common sub expressions",cse),
	 ("dead","eliminate dead code",deadcode),
	 ("flat","non-type directed argument flattening",flat),
	 ("hoist","hoist expressions",hoist),
	 ("inline","inline expansion of functions",inline_expand),
	 ("inline_once","inline functions applied only once",inline_once),
	 ("invariant","hoist loop invariant computations",hoist),
	 ("reduce","reduce primitive operations",reduce),
	 ("reduce_always","reduce primitive operations on every shrink",reduce_always),
	 ("simple_reduce","reduce simple selects & value bindings",simple_reduce),
	 ("pr_simple","print results of simple reduce",Simplereduce.DEBUG),
	 ("alpha_after_reduce","alpha convert after reduce",alpha_after_reduce),
	 ("minfix","minimize fix definitions",minfix),
	 ("sink","sink expressions into switches",sink),
	 ("switch","eliminate redundant switches",switch),
	 ("switchfail","switchfail",switchfail),
	 ("uncurry","uncurry functions",uncurry),
	 ("transparent","consider imported types transparent",transparent),
	 ("typeapp","do profitable type applications",do_type_app),
	 ("conapp","do constructor function applications",do_confn_app),
	 ("iterate","iterate shrinking optimizations until convergance",iterate),
	 ("messages","print optimizer messages",messages),
	 ("preduce","print results of reduce phase",Reduce.print_results),
	 ("box_reals", "box real values", Linklmli.box_reals),
	 ("flatcons","flatten tag-or-rec constructors",Linklmli.flatten_cons),
	 ("flatargs","flatten function arguments (type directed)",Linklmli.flatten_args),
	 ("elideTypes","don't print types when printing lmli/bform",Linklmli.Pplmli.elideTypes),
	 ("elideKinds","don't print kinds when printing lmli/bform",Linklmli.Pplmli.elideKinds),
	 ("elideSigs","don't print signatures when printing lmli/bform",Linklmli.Pplmli.elideSigs),
	 ("flatrealarrays","don't box arrays of reals",Linklmli.flatten_real_arrays),
	 ("atom","compile with support for atom tracing",Linkall.atom),
	 ("rtl_optimize","optimize the rtl representation",Linkrtl.rtl_optimize),
	 ("rtl_gcoptimize","optimize gc checks in the rtl representation",Linkrtl.rtl_gcoptimize),
	 ("print_rtl","print the rtl representation",Linkrtl.print_rtl),
	 ("close_handlers","generate closures for exn handler",close_handlers),
	 ("force_gc","force a gc during optimization",Linkb.Linkopt.gc)
	 ]
	   
      val int_flags : (string * string * int ref) list = 
	[
(*
       ("closure_inline_size",
	  "size of functions to inline after closure conversion",closure_inline_size),

	 ("closure_rounds",
	  "# of optimization rounds to do on closure converted code",closure_rounds),
*)
	 ("inline_size",
	  "maximum size of functions to inline",inline_size),
	 ("rounds",
	  "# of optimization rounds to do on b-form",rounds),
	 ("max_trans",
	  "maximum size of an imported type to inline",Transparent.max_transparent)
	 ]
	
      (* supported values are alpha_osf, rs_aix, and C *)
      val platform = ref "alpha_osf"
      val string_flags : (string * string * string ref) list = 
	[("Runtime","directory of runtime",Linkall.runtime_dir),
	 ("platform","Platform to operate on", platform),
	 ("cflags","flags to pass to the C compiler",Linkc.cflags)]

      exception Find

      fun find (s:string) l = 
	let fun loop [] = raise Find
	      | loop ((hd as (s',_,_))::rest) = if s = s' then hd else loop rest
	in
	  loop l
	end

      fun find_bool s = find s (bool_flags)
      fun find_int s = find s (int_flags)
      fun find_string s = find s (string_flags)
	
      fun get find s = let val (_,_,x) = find s
		       in
			 !x
		       end
      val get_bool = get find_bool
      val get_int = get find_int
      val get_string = get find_string
	
      fun set find s v = let val (_,_,x) = find s
			 in x := v
			 end
      val set_bool = set find_bool
      val set_int = set find_int
      val set_string = set find_string
	
      fun print_flags (l,printer) = 
	let fun loop ([]) = ()
	      | loop ((k:string,d:string,x)::rest) = 
	  (print k; print ": "; printer x; 
	   print "     ("; print d; print ")\n"; loop rest)
	in
	  loop l
	end
      
      fun print_bools () = 
	print_flags (bool_flags,fn (x:bool ref) => print (makestring (!x)))
      fun print_ints () = 
	print_flags (int_flags,fn (x:int ref) => print (makestring (!x)))
      fun print_strings () = 
	print_flags (string_flags,fn (x:string ref) => print (!x))
	
      fun skip_white(#" "::rest) = skip_white rest
	| skip_white(#"\t"::rest) = skip_white rest
	| skip_white(rest) = rest
	
      fun get_word l = 
	let fun loop([],accum) = (accum,[])
	      | loop(#" "::rest,accum) = (accum,rest)
	      | loop(#"\t"::rest,accum) = (accum,rest)
	      | loop(#"\n"::rest,accum) = (accum,rest)
	      | loop(#"\""::rest,accum) = get_string (rest,accum) (* "\"" *)
	      | loop(c::rest,accum) = loop(rest,c::accum)
	    and get_string(#"\\"::(#"\""::rest),accum) = 
				   get_string(rest,#"\""::accum)
	      | get_string(#"\""::rest,accum) = loop(rest,accum)   (* "\"" *)
	      | get_string(c::rest,accum) = get_string(rest,c::accum)
	      | get_string([],accum) = 
					      get_string(explode(input_line std_in),accum)
	    val (ac,rest) = loop(l,[])
	in
					      (implode(rev ac),rest)
	end
					      
					      
      fun do_command (c::rest) : unit = 
      (case c of
	 #"s" => set_command rest
       | #"g" => get_command rest
       | #"c" => ((compile_command rest)
		  handle exn => (print "exception raised: ";
				 print (General.exnName exn);
				 print "\n"; print "continuing...\n";
				 do_command []))
       | #"a" => ((assemble_command rest)
		  handle exn => (print "exception raised: ";
				 print (General.exnName exn);
				 print "\n"; print "continuing...\n";
				 do_command []))
       | #"h" => help_command rest
       | #"?" => help_command rest
       | #"l" => list_command rest
       | #"p" => reparse_prelude_command rest
       | #"r" => runtime_command rest
       | #"x" => exit_command rest
       | #"q" => exit_command rest
       | #"e" => export_command rest
       | #"\n" => do_command rest
       | #"%" => do_command []
       | #" " => do_command rest
       | #"\t" => do_command rest
       | _ => bad ("bad command: " ^ (str c) ^ "  (? for help)\n"))
	| do_command [] = 
	 (print "> "; flush_out(std_out); 
	  do_command (explode (input_line std_in)))
	 
      and bad s = (print s; do_command [])
	
      and export_command rest = 
	let val (filename,rest) = get_word(skip_white rest)
	in
	  exportFn(filename,toplevel)
	end
      
      and runtime_command rest = 
	let val (runtimedir,rest) = get_word(skip_white rest)
	in
	  Linkc.runtimedir := runtimedir;
	  do_command rest
	end
      
      and C_compile_command (#"f"::rest) = 
	let val (out_filename,rest) = get_word(skip_white rest)
	  val (in_filename,rest) = get_word(skip_white rest)
	in
	  Linkc.comp_file (out_filename,in_filename); do_command rest
	end
	| C_compile_command (#"d"::rest) = 
	let val (dir,_) = get_word(skip_white rest)
	in
	  OS.FileSys.chDir dir; do_command []
	end
	| C_compile_command rest = bad "bad compile command"
					  
      and compile_command arg = 
	let val temp = (case (get_string "platform") of
			  "C" => C_compile_command
			| "alpha_osf" => native_compile_command Linkall.ALPHA
			| "rs_aix" => native_compile_command Linkall.PPC
			| unk => error ("Unknown platform: " ^ unk))
        in temp arg
        end

      and native_compile_command plat ((#"f") :: (#" ") :: rest) = 
	let val (filename,rest) = get_word(skip_white rest)
	in
	  Linkall.cur_platform := plat;
	  Linkall.comp_file filename; do_command rest
	end
	| native_compile_command plat ((#"f") :: (#"o") :: rest) = 
	let val (filename,rest) = get_word(skip_white rest)
	  val (opt,rest) = get_word(skip_white rest)
	in
	  Linkall.cur_platform := plat;
	  Linkall.comp_file_opt (filename,opt); do_command rest
	end
	| native_compile_command _ (#"d"::rest) = 
	let val (dir,_) = get_word(skip_white rest)
	in
	  Posix.FileSys.chdir dir; do_command []
	end
	| native_compile_command _ rest = bad "bad compile command"
	
   and assemble_command (#"s"::rest) = 
					  let val (filename,rest) = get_word(skip_white rest)
					    val temp = (case (get_string "platform") of
			    "alpha_osf" => Linkall.ALPHA
			  | "rs_aix" => Linkall.PPC
                          | _ => error "bad platform for assemble")
      in
	Linkall.cur_platform := temp;
	Linkall.assemble_file filename; 
	do_command rest
      end
    | assemble_command rest = bad "bad assemble command"

  and set_command (rest) : unit = 
      let val (flagname,rest) = get_word(skip_white rest)
	  val (value,rest) = get_word(skip_white rest)
      in
	((((set_bool flagname (atob value))
	  handle _ => 
	      (set_int flagname (atoi value)));
	 do_command [])
	 handle _ => (set_string flagname value; do_command []))
	handle Find => bad ("bad flagname: "^flagname)
      end

  and get_command (rest) : unit = 
      let val (flagname,rest) = get_word(skip_white rest)
      in
	  let val value = 
	      (makestring(get_bool flagname)) handle Find => 
		  ((makestring(get_int flagname))
		   handle Find => (get_string flagname))
	  in
	      print flagname; 
	      print " = ";
	      print value;
	      print "\n";
	      do_command rest 
	  end
	  handle Find => bad ("bad flag "^flagname^"\n")
      end

  and reparse_prelude_command arg = 
	let val temp = (case (get_string "platform") of
			    "C" => C_reparse_prelude_command
			  | "alpha_osf" => native_reparse_prelude_command Linkall.ALPHA
			  | "rs_aix" => native_reparse_prelude_command Linkall.PPC
                          | unk => error ("Unknown platform: " ^ unk))
        in temp arg
        end

  and C_reparse_prelude_command (rest : char list) : unit = 
      let val (prelude,rest) = get_word(skip_white rest)
	  val (inline_prelude,rest) = get_word(skip_white rest)
      in
	  (Linkc.reparse_prelude(prelude,inline_prelude);
	   do_command rest)
      end

  and native_reparse_prelude_command plat (rest : char list) : unit = 
      (Linkall.cur_platform := plat;
       Linkall.reparse_prelude NONE;
       do_command rest)

  and help_command rest : unit = 
      (print "HELP\n";
       print "------------------------------------------------\n";
       print "help                   :  [h|?]\n";
       print "set a flag's value     :  s <flag> [true|false|<num>]\n";
       print "get a flag's value     :  g <flag>\n";
       print "list all flags         :  l\n";
       print "in C, do preludes      :  p <filename> <filename>\n";
       print "in native, do preludes :  p \n";
       print "in C, comp file        :  cf <outfile> <filename>\n";
       print "in native, comp file   :  cf <filename>\n";
       print "compile to asm         :  as <filename>\n";
       print "change directory       :  cd <directory>\n";
       print "export image           :  e <heapfilename>\n";
       print "C runtime directory    :  r <directory>\n";
       print "exit                   :  [x|q]\n";
       print "comment                :  %\n";
       print "------------------------------------------------\n\n";
       do_command rest)

  and list_command rest : unit = 
      (print "FLAGS\n";
       print "------------------------------------------------\n";
       print_bools();
       print_ints();
       print "------------------------------------------------\n\n";
       do_command rest)

  and exit_command rest : unit = 
      (print "exit? (y/n) "; flush_out(std_out);
       case explode(input_line std_in) of
	   (#"y"::_) => ()
	 | (#"Y"::_) => ()
	 | _ => do_command [])

  and toplevel _ = (print "TIL/ML compiler, version 3.0\n"; do_command[];
		    OS.Process.success)
     handle (ERROR s) => (print "ERROR: "; print s; print "\n"; OS.Process.failure)

  fun export_toplevel filename = (Linkall.reparse_prelude NONE;
				  exportFn(filename,toplevel))
end
