(*$Measure: Char Math Format Linkb Linkall Linkalpha MEASURE *)

structure Measure (* : MEASURE *) =
struct
   val system = OS.Process.system

(* XXX not OS portable *)
   val prelude_arg = (Linkalpha.default_prelude, Linkalpha.default_inlineprelude)

    exception Fail of string

   fun inline sizes =
	let val orig = !Linkb.Linkopt.inline_size
	    fun restore () = (Linkb.Linkopt.inline_size := orig;
			      Linkb.reparse_prelude prelude_arg)
	    fun step x =
	         (Linkb.Linkopt.inline_size := x;
		  Linkall.reparse_prelude NONE;
	          app (fn x => (Linkall.comp_file x; ())) 
		      Linkall.benchprogs;
		  let val cmd =
	               ("size.sh >> inline"^makestring x)
	          in system cmd; ()
	          end)
	in app step sizes
	   handle x => (restore(); raise x);
	   restore ()
	end

   fun inline_all () = inline [0,100,200,300,400,500,600,700,800,900,1000]

   fun inline_some () = inline [300,400,600,700,800,900]

   val DEBUG = true

  (* return the index of the next non-whitespace charater in s *)
    fun eatWS s = 
      let
	fun f j = if Char.isSpace(String.sub(s, j)) 
		    then f(j+1) 
		  else String.substring(s,j,size s - j)
      in (f 0) handle _ => ""
      end


    local
      fun myscanString scanFn s = let
				  val n = size s
				  fun getc i = 
				    if (i < n) then SOME(String.sub(s, i), i+1) else NONE
				in
				  case (scanFn getc 0)
				    of NONE => NONE
				  | SOME(x, j : int) => SOME (x, j)
				end;
      fun mysscanf fmt = myscanString (Format.scanf fmt)		
    in
      fun myscani (format : string) (data : string) 
	: (Format.fmt_item list * string) option = 
	(case mysscanf format data of
	   NONE => NONE
	 | SOME (allres,i) => SOME(allres,String.substring(data,i,size data - i)))
    end

   fun inline_process dir sizes tex =
       let val data = ref nil
	   fun insert (x,y) =
	     let fun loop ((h,l)::t) = 
		        if h=x then (x,y::l)::t
			else (h,l) :: loop t
                   | loop nil = [(x,[y])]
             in data := loop (!data)
	     end
	   fun dump x =
	     let fun prln (x,sizes) =
		   (print (Format.format "%-10s" [Format.STR x]);
		    app (fn x => (if tex then print " & " 
				  else ();
				  print (Format.format "%7d" [x]);
				  ()))
		    (map Format.INT sizes);
		    print "\n";
		    ())
             in prln ("program",sizes);
		app prln (map (fn (x,y) => (x,rev y)) (!data))
             end
	   fun add(x : string,y :int) = 
	      (if DEBUG then (print x; print ": ";
			      print y; print"\n")
	       else ();
	       insert(x,y))
	   fun loop nil = ()
	     | loop (h::t) =
	     let 
	         fun crunch str =
		   let val str = eatWS str
		   in if (str = "")
			then ()
		      else 
			(case (myscani "%s %s %s %s %d %s %s %d" str) of
				SOME([Format.STR name,_,_,_,_,_,_,
				     Format.INT size],rest) => (add(name,size); crunch rest)
			      | _ =>
				(case (myscani "%s" str) of
				   SOME([Format.STR name],rest) =>
				     (print ("Warning: "^h^": "^name^" missing\n");
				      add(name,~1);
				      crunch rest)
			          | _ => raise (Fail "crunch could not get name"))
		         handle _ => raise (Fail "crunch encountered unk exn"))
                   end (* crunch *)
		 val f = open_in h
	         val s = input(f,10000)
             in (crunch s; close_in f)
	       handle x => (close_in f; raise x);
		 loop t
	     end

	     val files = map (fn x => dir^"/inline"^makestring x) sizes

       in loop files;
	  dump files
       end


    local
	val errlog = ref(nil : string list)
    in
	fun adderr (s : string) = (print s; print "\n"; errlog := s :: !errlog)
	fun reseterr () = errlog := nil
        fun geterr () = rev (!errlog)
    end

    type stat = {total_time : real,
		 user_time : real,
		 sys_time : real,
		 gc_time : real,
		 gccopy_time : real,
		 gcstack_time : real,
		 majorgc_time : real,
		 alloced : real,
		 copied : real,
		 physmem : real,
		 page_reclaims : real,
		 page_faults : real,
		 gctable_size : real,
		 code_size : real}

    type avg_stat = {total_time : {avg:real,std_dev:real},
		 user_time : {avg:real,std_dev:real},
		 sys_time : {avg:real,std_dev:real},
		 gc_time : {avg:real,std_dev:real},
		 gccopy_time : {avg:real,std_dev:real},
		 gcstack_time : {avg:real,std_dev:real},
		 majorgc_time : {avg:real,std_dev:real},
		 alloced : {avg:real,std_dev:real},
		 copied : {avg:real,std_dev:real},
		 physmem : {avg:real,std_dev:real},
		 page_reclaims : {avg:real,std_dev:real},
		 page_faults : {avg:real,std_dev:real},
		 gctable_size : {avg:real,std_dev:real},
		 code_size : {avg:real,std_dev:real}}



    fun scan_stats (str : string) = 
       let fun scan name format s = 
	       (myscani format s
		handle Format.BadFormat => 
		       raise Fail (name^" input"))

(* ----- TIMES ----- *)
	   val (total_time,str) = 
	          case scan "total time " ("%s %s %s %f") str
		  of SOME([_,_,_,Format.REAL t],j) => (t,j)
		   | _ => raise Fail "total time"

            fun scan_time name s = 
		  case scan name "%s %s %f %s" s
		  of SOME([_,_,Format.REAL t,_],j) => (t,j)
		   | _ => raise Fail ("scan time: "^name)
	   val (user_time,str) = scan_time ">total-user_time" str
	   val (sys_time,str) = scan_time "total-sys_time " str
	   val (_,str) = scan_time "discard" str
	   val (_,str) = scan_time "discard" str
           val (gc_time,str) = scan_time "gc_time" str
	   val (_,str) = scan_time "discard" str
	   val (_,str) = scan_time "discard" str
           val (majorgc_time, str) = scan_time "majorgc-copy_time" str
	   val (gccopy_time ,str) = scan_time "gc-copy_time" str
	   val (_,str) = scan_time "discard" str
	   val (_,str) = scan_time "discard" str
	   val (gcstack_time, str) = scan_time "gc-stack_time" str
	   val (_,str) = scan_time "discard" str
	   val (_,str) = scan_time "discard" str

(* ----- GC and Stack ----- *)
	   fun scan_discard s str = (case scan "discard" s str of
				       SOME(_,str) => str
				     | _ => raise Fail "alloced")
	   val str = scan_discard ("%s %s %s %s %s %s %s") str 
	   val (alloced,copied,str) = 
		case scan "alloced" ("%s %s %d %s %s %d") str of
		   SOME([_,_,Format.INT a,_,_,Format.INT c],str) => (a,c,str)
		 | _ => raise Fail "alloced"
	   val str = scan_discard ("%s %s %s %s %s %s %s %s %s") str 
	   val str = scan_discard ("%s %s %s %s %s %s %s %s %s") str 


(* ----- phys mem, ignore shared mem info *)

	    val (physmem,str) = 
		case scan "phys mem" ("%s %s %s %d") str of
		  SOME([_,_,_,Format.INT mem],i) => (mem,i)
		| _ => raise Fail "phys mem"
	   val str = scan_discard ("%s %s %s %s %s %s %s %s %s") str 


(* ------ stats *)

	    val (page_reclaims,page_faults,str) = 
               case scan "paging info" 
		          ("%s %s %s %d %s %s %d %s %s %d") str of
		 SOME([_,_,_,Format.INT page_reclaims,
		       _,_,Format.INT page_faults,_,_,_],str) =>
		    (page_reclaims,page_faults,str)
                 | _ => raise (Fail ("paging info" ^ String.substring(str,0,100)))

(* ------- ignore process stats *)
				   
	   val str = scan_discard ("%s %s %s %s %s %s %s") str 


(* ----- misc stats *)
	    val (gctable_size,code_size,str) =
	        case scan "misc stats" ("%s %s %s %d %s %s %d") str of
		  SOME([_,_,_,Format.INT table,_,_,Format.INT codesize],i) =>
		    (table,codesize,i)
		 | _ => raise (Fail "misc stats")
	   val str = scan_discard ("%s %s %s %s %s %s %s %s %s") str 

(* ----- done *)
	   val str = 
	        case scan "done" ("%s") str of
		  SOME([Format.STR "DONE:"],str) => str
		| SOME([Format.STR s],str) => (print "Expect DONE: got"; print s;
					       raise (Fail "done"))
		 | _ => raise (Fail "done")

       in ({total_time = total_time,
	    user_time = user_time,
	    sys_time = sys_time,
	    gc_time =  gc_time,
	    gccopy_time = gccopy_time,
            gcstack_time =gcstack_time,
	    majorgc_time = majorgc_time,
	    alloced = real alloced,
	    copied = real copied,
	    physmem = real physmem,
	    page_reclaims = real page_reclaims,
	    page_faults = real page_faults,
	    gctable_size = real gctable_size,
	    code_size = real code_size},str)
       end handle exn as Fail x => 
	   let val msg = "scan_stats: failed on "^x
	   in adderr msg; raise exn
	   end

       (* collect a series of statistics in a file *)

       fun scan_file name =
	   let val f = open_in name
	       val str = input(f,500000)
	       fun loop s =
		   let val s = eatWS s
		   in if (s = "") then nil
		      else let val (r,s') = scan_stats s
			   in r :: loop s'
			   end
                   end
           in loop str
	   end


       fun r_arith_mean (l : real list) =
	   let fun sum (h::t,accum) = sum(t,h+accum)
	         | sum (nil,accum) = accum
           in sum(l,0.0) / real (length l)
           end

       fun r_std_dev (l : real list) =
	   let val mean = r_arith_mean l
	       fun sum (h::t,accum) = 
		       let val diff = (h-mean)
		       in sum(t,diff * diff + accum)
		       end
                 | sum (nil,accum) = accum
           in Math.sqrt (sum(l,0.0) / real (length l-1))
           end

       fun avg proj l =
	   let val l = map proj l
	   in {avg=r_arith_mean l,std_dev=r_std_dev l}
           end

       fun print_stat out stat =
	   case stat
	   of {total_time,
	       user_time,
	       sys_time,
	       gc_time,
	       gccopy_time,
	       gcstack_time,
	       majorgc_time,
	       alloced,
	       copied,
	       physmem,
	       page_reclaims,
	       page_faults,
	       gctable_size,
	       code_size} =>
        let val s =
	  [Format.format "TIME(s): total time       = %6.3f\n"
	                 [Format.REAL total_time],
	   Format.format "          user time       = %6.3f\n"
	                 [Format.REAL user_time],
           Format.format "          sys time        = %6.3f\n"
	                 [Format.REAL sys_time],
	   Format.format "          gc-all time     = %6.3f\n"
	                 [Format.REAL gc_time],
	   Format.format "          majorgc-copy    = %6.3f\n"
	                 [Format.REAL majorgc_time],
	   Format.format "          gc-copy time    = %6.3f\n"
	                 [Format.REAL gccopy_time],
	   Format.format "          gc-stack time   = %6.3f\n"
	                 [Format.REAL gcstack_time],
	   Format.format "GC(k):   bytes alloced    = %8.3f\n"
		         [Format.REAL alloced],
	   Format.format "         bytes copied     = %8.3f\n"
	                 [Format.REAL copied],
	   Format.format "MEM(K):  max phys mem     = %8.3f\n"
	                 [Format.REAL physmem],
	   Format.format "PAGING:  page reclaims    = %8.3f\n"
	                 [Format.REAL page_reclaims],
	   Format.format "          page faults     = %8.3f\n"
	                 [Format.REAL page_faults],
	   Format.format "MISC:    GCTableSize      = %8.3f\n"
	                 [Format.REAL gctable_size],
	   Format.format "         CodeSize         = %8.3f\n"
	                 [Format.REAL code_size]]
        in output(out,foldl (op ^) "" s);
	   flush_out out
        end


       fun print_avg_stat out stat =
	   case stat
	   of {total_time={avg=total_time,std_dev=total_time_dev},
	       user_time={avg=user_time,std_dev=user_time_dev},
	       sys_time={avg=sys_time,std_dev=sys_time_dev},
	       gc_time={avg=gc_time,std_dev=gc_time_dev},
	       gccopy_time={avg=gccopy_time,std_dev=gccopy_time_dev},
	       gcstack_time={avg=gcstack_time,std_dev=gcstack_time_dev},
	       majorgc_time={avg=majorgc_time,std_dev=majorgc_time_dev},
	       alloced={avg=alloced,std_dev=alloced_dev},
	       copied={avg=copied,std_dev=copied_dev},
	       physmem={avg=phys_mem,std_dev=phys_mem_dev},
	       page_reclaims = {avg=page_reclaims,std_dev = page_reclaims_dev},
	       page_faults = {avg=page_faults,std_dev = page_faults_dev},
	       gctable_size = {avg=gctable_size,std_dev = gctable_size_dev},
	       code_size = {avg=code_size,std_dev = code_size_dev}} =>
        let val s =
	  [Format.format "TIME(s): total time       = %6.2f +/- %3.2f\n"
	                 [Format.REAL total_time,
			  Format.REAL total_time_dev],
	   Format.format "          user time       = %6.2f +/- %3.2f\n"
	                 [Format.REAL user_time,
			  Format.REAL user_time_dev],
           Format.format "          sys time        = %6.2f +/- %3.2f\n"
	                 [Format.REAL sys_time,
			  Format.REAL sys_time_dev],
	   Format.format "          gc-all time     = %6.2f +/- %3.2f\n"
	                 [Format.REAL gc_time,
			  Format.REAL gc_time_dev],
	   Format.format "          majorgc-copy    = %6.2f +/- %3.2f\n"
	                 [Format.REAL majorgc_time,
			  Format.REAL majorgc_time_dev],
	   Format.format "          gc-copy time    = %6.2f +/- %3.2f\n"
	                 [Format.REAL gccopy_time,
			  Format.REAL gccopy_time_dev],
	   Format.format "          gc-stack time   = %6.2f +/- %3.2f\n"
	                 [Format.REAL gcstack_time,
			  Format.REAL gcstack_time_dev],
	   Format.format "GC(k):   bytes alloced    = %8.0f +/- %3.0f\n"
		         [Format.REAL alloced,
			  Format.REAL alloced_dev],
	   Format.format "         bytes copied     = %8.0f +/- %3.0f\n"
	                 [Format.REAL copied,
			  Format.REAL copied_dev],
	   Format.format "MEM(K):  max phys mem     = %8.0f +/- %3.0f\n"
	                 [Format.REAL phys_mem,
			  Format.REAL phys_mem_dev],
	   Format.format "PAGING:  page reclaims    = %8.0f +/- %3.0f\n"
	                 [Format.REAL page_reclaims,
			  Format.REAL page_reclaims_dev],
	   Format.format "          page faults     = %8.0f +/- %3.0f\n"
	                 [Format.REAL page_faults,
			  Format.REAL page_faults_dev],
	   Format.format "MISC:    GCTableSize      = %8.0f +/- %3.0f\n"
	                 [Format.REAL gctable_size,
			  Format.REAL gctable_size_dev],
	   Format.format "         CodeSize         = %8.0f +/- %3.0f\n"
	                 [Format.REAL code_size,
			  Format.REAL code_size_dev]]
        in output(out,foldl (op ^) "" s);
	   flush_out out
        end

       fun avg_stats (x :stat list) =
	   {total_time = avg #total_time x,
	    sys_time = avg #sys_time x,
	    user_time = avg #user_time x,
	    gc_time =avg #gc_time x,
	    gccopy_time = avg #gccopy_time x,
	    gcstack_time = avg #gcstack_time x,
	    majorgc_time = avg #majorgc_time x,
	    alloced = avg #alloced x,
	    copied = avg #copied x,
	    physmem = avg #physmem x,
	    page_reclaims = avg #page_reclaims x,
	    page_faults = avg #page_faults x,
	    gctable_size = avg #gctable_size x,
	    code_size = avg #code_size x}

       fun drop_std_dev x =
	   let fun  p {avg,std_dev} = avg
	       val {total_time,
		    sys_time,
		    user_time,
		    gc_time,
		    gccopy_time,
		    gcstack_time,
		    majorgc_time,
		    alloced,
		    copied,
		    physmem,
		    page_reclaims,
		    page_faults,
		    gctable_size,
		    code_size} = x
	    in     {total_time = p total_time,
		    sys_time = p sys_time,
		    user_time = p user_time,
		    gc_time = p gc_time,
		    gccopy_time = p gccopy_time,
		    gcstack_time = p gcstack_time,
		    majorgc_time = p majorgc_time,
		    alloced = p alloced,
		    copied = p copied,
		    physmem = p physmem,
		    page_reclaims = p page_reclaims,
		    page_faults = p page_faults,
		    gctable_size = p gctable_size,
		    code_size = p code_size}
             end

       fun ratio (x,y) =
	   let fun  p (a,b) = a / b 
	                      handle _ => ~1.0
	       val {total_time,
		    sys_time,
		    user_time,
		    gc_time,
		    gccopy_time,
		    gcstack_time,
		    majorgc_time,
		    alloced,
		    copied,
		    physmem,
		    page_reclaims,
		    page_faults,
		    gctable_size,
		    code_size} = x
	       val {total_time=total_time1,
		    sys_time=sys_time1,
		    user_time=user_time1,
		    gc_time=gc_time1,
		    gccopy_time=gccopy_time1,
		    gcstack_time=gcstack_time1,
		    majorgc_time=majorgc_time1,
		    alloced=alloced1,
		    copied=copied1,
		    physmem=physmem1,
		    page_reclaims=page_reclaims1,
		    page_faults=page_faults1,
		    gctable_size=gctable_size1,
		    code_size=code_size1} = y


	    in     {total_time = p(total_time,total_time1),
		    sys_time = p(sys_time,sys_time1),
		    user_time = p(user_time,user_time1),
		    gc_time = p(gc_time,gc_time1),
		    gccopy_time = p(gccopy_time,gccopy_time1),
		    gcstack_time = p(gcstack_time,gcstack_time1),
		    majorgc_time = p(majorgc_time,majorgc_time1),
		    alloced = p(alloced,alloced1),
		    copied = p(copied,copied1),
		    physmem = p(physmem,physmem1),
		    page_reclaims = p(page_reclaims,page_reclaims1),
		    page_faults = p(page_faults,page_faults1),
		    gctable_size = p(gctable_size,gctable_size1),
		    code_size = p(code_size,code_size1)}
             end


       fun run log iter x = 
	   let val cmd = x^" | tail -29 >> "^log
	       fun loop i =
	         if i>0
		 then (system cmd; loop (i-1))
		 else ()
           in loop iter
	   end

       fun process_log log =
	    let val stats = scan_file log
	    in avg_stats stats
	    end

       val iters = ref 100

       (* measure_opts for benchmark x, assumed to be in directory
          bench/x *)

       fun exists_file x =
	   let val f = open_in x 
	   in (close_in f; true)
	   end handle _ => false

       fun measure_opts dir log reuse opts x =
	   let val summarylog = open_out(dir^"/"^log)
	       val changed_prelude = ref false
	       fun clear () = app (fn (_,x) => x := false) opts
	       val orig = map (fn (_,r) => (r,!r)) opts
	       fun restore () = (close_out summarylog;
				 app (fn (r,orig) => r := orig) orig;
				 if !changed_prelude
				 then () (* Linkall.reparse_prelude("","") *)
				 else ())
	       fun scan nil = nil
		 | scan ((y,flag) :: t) =
		   let val logname =  (dir^"/"^y^"-"^x)
		       val benchname = ("bench/"^x)
		   in if reuse andalso exists_file logname
		      then ()
		      else  (clear();
			     flag := true;
			     (* recompile prelude modules *)
			     changed_prelude := true;
			     Linkall.reparse_prelude NONE;
			     Linkall.comp_file benchname;
			     run logname (!iters) (benchname^".exe"));
		      (y,process_log logname) :: scan t
		      handle Fail x =>
			(adderr ("failed processing log "^dir^"/"^logname ^ ": " ^ x);
			 scan t)
                   end handle exn =>
		         (adderr ("exception raised "^
				  "while measuring "^y^" for "^x);
			  adderr ("continuing processing");
			  scan t)
				  
	       val opt = scan opts

	        fun log stream = 
		   let val say = outputc stream
		   in app (fn (x,avg) =>
			    (say x;
			     say ": \n \n";
			     print_avg_stat stream avg)) opt
		   end
           in log std_out;
	      log summarylog;
	      restore();
	      (x,opt)
	   end

       val loop_opts = 
	   let open Linkb.Linkopt
	   in [("noloopopts",ref true),
	       ("cmpelim",cmpelim),
	       ("cse",cse),
	       ("hoist",hoist),
	       ("invariant",invariant),
	       ("switch",switch)]
           end

       (* print_ratios:

	  print performance ratios, given a list of results
	  and a file name to log the ratios to.   The ratios
          are relative to the first entry in the list.

	  results has the type (string * stat) list, the string
          names the statistic.*)

 
       fun print_ratios dir log (name,results) = 
	    let val f = open_append(dir^"/"^log)
	        fun say x = (output(f,x); output(std_out,x))
		val results' = map (fn (x,y) => (x,drop_std_dev y)) results
                val ratios = 
		  case results'
	          of (_,base) :: rest =>
                     let fun compare (name,y) = 
		         let val c = ratio (y,base)
		         in say ("std opts + "^name^" / std opts :\n");
		            print_stat f c;
		            print_stat std_out c;
		            (name,c)
		        end
		       val ratios = map compare rest
	             in close_out f;
		      ratios
	             end
                   | _ => nil
            in close_out f;
	       (name,ratios)
	    end

       local 
	   open Linkb.Linkopt
	   val flags = [alpha,cmpelim,cse,deadcode,flat,inline_expand,
			inline_once,invariant,hoist,reduce,alpha_after_reduce,
			minfix,sink,switch,switchfail,uncurry,transparent,
			simple_reduce,reduce_always,do_type_app,do_confn_app,
			Cse.do_exp,Invariant.do_exp,Hoist.do_exp]
       in
           fun resetflags _ = 
	       (app (fn x => x := true) flags;
		inline_size := 225)
       end
			


       fun measure_loop_opts dir log errlog reuse x = 
	    let val _ = reseterr()
	        val _ = resetflags()
                val (_,allopts) = measure_opts dir log reuse [("allopts",ref false)] x
		val (_,singleopts) = measure_opts dir log reuse loop_opts x
                val results = (x,singleopts @ allopts)
            in print_ratios dir log results;
	       case geterr()
	       of nil => ()
		| errs => (let val errlog = open_out errlog
			    val say = outputc errlog
			   in map (fn (x : string) => (say x; say "\n")) errs;
			      close_out errlog
			   end);
               results
	    end

       val cm_opts = 
	   let open Linkb.Linkopt
	   in [("cse",cse),
	       ("hoist",hoist),
	       ("invariant",invariant)]
           end

       fun measure_cm_opts dir log errlog reuse x exp = 
	    let val _ = reseterr()
	        val _ = resetflags()

                (* set whether expressions are moved *)

		val _ = (Linkb.Linkopt.Cse.do_exp := exp;
			 Linkb.Linkopt.Invariant.do_exp := exp;
			 Linkb.Linkopt.Hoist.do_exp := exp)

                val (_,allopts) = measure_opts dir log reuse [("allopts",ref false)] x
		(* turn off loop optimizations to provide base line *)

		val _ = app (fn (_,flag) => flag := false) loop_opts

	        val (_,noopts) = 
		       measure_opts dir log reuse [("noloopopts",ref false)] x

		val (_,singleopts) = measure_opts dir log reuse cm_opts x
                val results = (x,noopts @ singleopts @ allopts)
            in print_ratios dir log results;
	       case geterr()
	       of nil => ()
		| errs => (let val errlog = open_out(dir^"/"^errlog)
			    val say = outputc errlog
			   in map (fn (x : string) => (say x; say "\n")) errs;
			      close_out errlog
			   end);
               results
	    end


        (* list small bmarks, then bigger bmarks.  That way if something
	   is failing we might catch it early on.*)

       val bmarks = ["fft.sml","matmult.sml","life.sml",
		     "checksum.sml","leroy.sml","lexgen.sml","pia.sml",
		     "simple.sml"]

       fun start_bmarks dir =
	   map (fn x => measure_loop_opts dir ("summary-"^x) ("err-"^x) false x) 
	       bmarks

       fun restart_bmarks dir =
	   map (fn x => measure_loop_opts dir ("summary-"^x) ("err-"^x) true x)
	       bmarks

       fun start_cm_bmarks dir y =
	   map (fn x => measure_cm_opts dir ("summary-"^x) ("err-"^x) false x y) 
	       bmarks

       fun restart_cm_bmarks dir y =
	   map (fn x => measure_cm_opts dir ("summary-"^x) ("err-"^x) true x y)
	       bmarks

local     
       exception Notfound

       fun find (name,nil) = raise Notfound
	 | find (name,((key,data)::t)) = if key=name then data else find (name,t)

       fun fetch opt field ratios = 
	         let val num = field (find (opt,ratios))
		 in if num= ~1.0 then NONE
		    else SOME num
		 end
		 handle _ => NONE

       local
	  val xlate = [("checksum.sml","Cksum"),
		       ("fft.sml","FFT"),
		       ("leroy.sml","KB"),
		       ("lexgen.sml","Lexgen"),
		       ("life.sml","Life"),
		       ("matmult.sml","Mmult"),
		       ("pia.sml","PIA"),
		       ("simple.sml","SIMPLE")]
       in
          fun get_name x = find (x,xlate)
       end

       val header = 
"\\begin{figure}[htbp]\n\
\ \\begin{center}\n\
\ \\begin{picture}(210,120)(0,0)\n\
\ \\put(0,0){\\line(0,1){120}}  % y-axis\n\
\ \\put(0,0){\\line(1,0){210}}  % x-axis\n\
\ \\put(0,100){\\thicklines\\line(1,0){210}}                     % 1.0 line\n\
\ \\put(-2,25) {\\line(1,0){4}}                         % ticks\n\
\ \\put(-2,50) {\\line(1,0){4}}                         % ticks\n\
\ \\put(-2,75) {\\line(1,0){4}}                         % ticks\n\
\ \\put(-2,100){\\line(1,0){4}}                         % ticks\n\
\ \\put(-9,25) {\\tiny \\makebox(0,0){25\\%}}                   % tick labels\n\
\ \\put(-9,50) {\\tiny \\makebox(0,0){50\\%}}                   % tick labels\n\
\ \\put(-9,75) {\\tiny \\makebox(0,0){75\\%}}                   % tick labels\n\
\ \\put(-9,100){\\tiny \\makebox(0,0){100\\%}}                  % tick labels\n"
in
       fun print_graph opt field fieldname say all_ratios =
	  (say header;
	   let fun scan (nil,_) = ()
	         | scan ((bmark,ratios)::t,offset) =
	       let val num = fetch opt field ratios
		   val name = get_name bmark
	       in say "\\barone{";
		  say (makestring offset);
		  say "}{";
		  say (case num
		       of SOME i => (makestring (floor (i * 100.0)))
			| NONE => "0");
		  say "}{";
		  say name;
		  say "}\n";
		  scan (t,offset+25)
	       end
           in scan (all_ratios,5)
	   end;
	   say "\\end{picture}\n";
	   say "\\end{center}\n";
	   say ("\\caption{Effect of "^opt^" on "^fieldname^"}\n");
	   say ("\\label{graph:"^opt^":"^fieldname^"}\n");
	   say "\\end{figure}\n")

      local
	 val fields = [(fn {total_time,...} : stat => total_time,"total time"),
	               (fn {alloced,...} : stat => alloced,"heap allocation"),
		       (fn {copied,...} : stat => copied,"GC copying"),
		       (fn {physmem,...} : stat => physmem,"physical memory"),
		       (fn {code_size,...} : stat => code_size,"code size")]
      in
        (* print graphs for an optimization *)

        fun print_opt_graphs opt say all_ratios =
	    (app (fn (field,fieldname) =>
		  print_graph opt field fieldname say all_ratios) fields;
	     say "\\clearpage\n")
      end

        (* print all graphs *)

        fun print_all_graphs dir say =
	    let val results = restart_bmarks dir 
		val ratios = map (print_ratios "/dev/" "null") results
            in app (fn (opt,_) =>
		   print_opt_graphs opt say ratios) (tl loop_opts)
	    end

        fun print_all_graphs2 dir =
	    let val stream = open_out (dir^"/graphs.tex")
	    in print_all_graphs dir (outputc stream);
	       close_out stream
	     end
		
end
end

        
	                  
	 	
