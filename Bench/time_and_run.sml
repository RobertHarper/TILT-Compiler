structure TimeAndRun :> TIMEANDRUN =
  struct
    val stringem = String.concat
    val printem = List.app print
    val unzip = ListPair.unzip 

    type result = {name : string, 
		   cpu : {usr : Time.time, sys : Time.time, gc : Time.time},
		   real : Time.time}
      
    fun indent' n = 
      let 
	fun loop 0 acc = acc
	  | loop n acc = loop (n-1) (#" "::acc)
	val l = loop n []
      in String.implode l
      end
    
    
    fun indent 0 = ()
      | indent n = (print " ";indent (n-1))
      
    fun timerToString ind {usr : Time.time, sys : Time.time, gc : Time.time} = 
      (stringem [indent' ind,indent' ind,"usr=    ",Time.toString usr,"\n",
		 indent' ind,indent' ind,"sys=    ",Time.toString sys,"\n",
		 indent' ind,indent' ind,"gc=     ",Time.toString gc,"\n",
		 indent' ind,"total=  ",Time.toString(Time.+(usr,sys)),"\n"])  (*User time includes gc*)
      
    fun print_result ({name,cpu,real} : result) : unit = 
      let
	val ind = 10
      in
	printem ["Timings for ",name,":\n"];
	print (timerToString ind cpu);
	indent ind;printem ["real =",Time.toString real,"\n"]
      end
    
    
    fun sumsummaries [] = raise Fail "no summaries to sum"
      | sumsummaries (fst::rest) = 
      let
	fun loop([],_,acc) = rev acc
	  | loop (hd::tl,rest,acc) = 
	  let 
	    val (hds,tls) = unzip (map (fn (h::t) => (h,t)) rest)
	    val hd = foldl Real.+ hd hds
	  in loop(tl,tls, hd ::acc)
	  end
      in loop (fst,rest,[])
      end
    
    fun avgsummaries (n,l) = 
      let 
	val n = Real.fromInt n
	val sums = sumsummaries l
      in map (fn r => Real./(r,n)) sums
      end
    
    fun summarize (results : result list)  = 
      let 
	val names = map #name results
	val times = map #real results
	val treals = map Time.toReal times
	val total = (foldl Real.+ 0.0 treals)
      in
	("TOTAL"::names,total::treals)
      end
    
    fun print_summary (names,totals) = 
      let
	val tstrings = map Real.toString totals
	val _ = ListPair.app (fn (n,t) => (print (n^"="^t^"    "))) (names,tstrings)
      in print "\n"
      end
    
    fun run (f,name) : result = 
      let
	val _ = printem ["\n=============Running ",name," ======================\n"]
	val cpu_timer = Timer.startCPUTimer()
	val real_timer = Timer.startRealTimer()
	val _ = f ()
	val cpu =  Timer.checkCPUTimer cpu_timer
	val real =  Timer.checkRealTimer real_timer
      in {name = name,cpu = cpu,real=real}
      end

    fun run1 benchmarks _ = map run benchmarks;
    fun runn (n,benchmarks) = List.tabulate (n,run1 benchmarks)
    fun report (n,benchmarks) = 
      let
	val results = runn (n,benchmarks)
	val summaries = map summarize results
	val (namess,totalss) = unzip summaries
	val names = hd namess
	val totals = avgsummaries (n,totalss)
	val _ = app (fn rs => app print_result rs) results;
	val _ = print_summary (names,totals)
      in ()
      end
  end